{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# LANGUAGE LambdaCase #-}

module Analysis.SimpleActor.Semantics(
     eval
   , allPrimitives
   , allPrimitiveFunctions
   , initialSto
   ) where

import Syntax.AST
import Syntax.Span
import Data.Map (Map)
import qualified Data.Map as Map
import Domain.Scheme.Class hiding (Exp)
import qualified Domain.Scheme.Class as DomainClass
import Domain.Actor

import qualified Analysis.SimpleActor.ErlangPrimitives as Erl

import Analysis.SimpleActor.Monad
import Analysis.Actors.Monad

import Control.Monad ((>=>), foldM, when, unless)
import System.IO (hPutStrLn, stderr)
import Control.Monad.Escape
import Control.Monad.Join
import Control.Monad.Layer
import Data.Functor ( ($>) )

import Analysis.Monad.Allocation
import Analysis.Monad.Environment

import Analysis.Monad.Fix
import Domain.Core.PairDomain (cons, car ,cdr)
import Analysis.Scheme.Primitives (primitivesByName)
import Domain (Domain(inject), boolTop)
import Analysis.Environment ( Environment(..) )
import qualified Analysis.Environment as Environment
import Analysis.Monad.Context (CtxM(..))
import Data.Maybe
import Debug.Trace
import Lattice.Class (bottom)
import Analysis.Scheme.Monad
import Control.Monad.Trans.Identity
import Lattice.Equal
import Control.Monad.Choice

import Analysis.Store (Store)
import qualified Analysis.Store as Store
import Syntax.FV
import Analysis.SimpleActor.Primitives (SimpleActorPrim(Prm), SimpleActorPrimitive(..))
import Lattice (Joinable)
import Control.Monad.DomainError (DomainError)
import qualified Domain.Core.PairDomain.Class as Pair
import Analysis.Monad.Store (StoreM)
import Control.Monad.Reader
import qualified Syntax.Ide as Ide
import Data.Foldable (foldrM)
import qualified Data.List as List

------------------------------------------------------------
-- Implementation of polymorphic evaluation functions
------------------------------------------------------------

newtype ApplyT v m a = ApplyT {runApplyT' :: ReaderT (Cmp -> m v) m a}
                     deriving (Applicative, Functor, Monad, MonadJoinable, MonadChoice v, MonadEscape)

instance MonadLayer (ApplyT v) where
   lowerM f m = ApplyT $ ReaderT (f . (runReaderT . runApplyT') m)
   upperM = ApplyT . lift

instance MonadTrans (ApplyT v) where
   lift = upperM

instance (EvalM e v k km m) => MonadApply v (ApplyT v m) where
   applyFun expr fun ops = ApplyT ask >>= (\cmp -> lift $ apply cmp expr [] fun ops)


runApplyT :: (Cmp -> m v) -> ApplyT v m a -> m a
runApplyT r = flip runReaderT r . runApplyT'

------------------------------------------------------------
-- Evaluation
------------------------------------------------------------

terminate :: EvalM e v k km m => m a
terminate = mbottom

showIfBot :: EvalM e v k km m => String -> v -> m v
showIfBot s v = (if v == bottom then trace s else id) (return v)

eval :: forall v m k km e  . EvalM e v k km m => Cmp -> m v
eval = fix evalCmp
   where evalCmp recur (FuncBdy (Lam _ _ bdy _)) = eval' recur bdy
         evalCmp recur (ActorExp e) = eval' recur e
         evalCmp recur (ReceiveExp e) = evalReceive recur e
         evalCmp _ (FuncBdy e) = error $ "not a function" ++ show e

eval'' :: forall v m k km e . EvalM e v k km m => (Cmp -> m v) -> Exp -> m v
eval'' _ lam@(Lam {}) = injectClo . (lam,) . restrictEnv (fv lam) <$> getEnv
eval'' _ ex@(Literal lit _) =  injectLit lit ex
eval'' _ (App (Var (Ide "print-env" _)) [] _) = 
    (liftIO . print =<< getEnv) $> nil
eval'' rec e@(App e1 es _) = do
   v1 <- eval' rec e1
   v2 <- mapM (eval' rec) es
   apply rec e es v1 v2
eval'' rec e@(AppQual namespace funName es s) = do
   let name = Var $ Ide (Ide.name namespace ++ ":" ++ Ide.name funName) s
   v1 <- eval' rec name
   vs <- mapM (eval' rec) es
   apply rec e es v1 vs
eval'' rec (Ite e1 e2 e3 _) = do
   result <- eval' rec e1
   choice result (eval' rec e2) (eval' rec e3)
eval'' _rec (Spawn e _) =
   liftA2 (,) getEnv getCtx >>= (fmap aref . uncurry (spawn @v e))
eval'' _ (Terminate _) = terminate $> nil
eval'' _ e@(Receive _ _) = do
    ρ' <- getEnv
    dyn' <- getDynamic
    select e ρ' dyn'
eval'' rec (Match e pats _) = do
   val <- eval' rec e
   matchList (\matchedExp -> allocMapping >=> (`withEnv'` eval' rec matchedExp))
             pats val
   where withEnv' ρ = withEnv (const ρ)
eval'' rec (Letrec bds e2 _) = do
   ads <- mapM (alloc . fst) bds
   mapM_ (flip logCoverage (return ()) . fst) bds
   let bds' = zip (map (name . fst) bds) ads
   mapM_ (\(ex, adr) -> writeVar @v adr =<< withExtendedEnv bds' (eval' rec ex)) (zip (map snd bds) ads)
   withExtendedEnv bds' (eval' rec e2)
eval'' rec (Parametrize bds e2 _) = do
   ads <- mapM (alloc . fst) bds
   mapM_ (flip logCoverage (return ()) . fst) bds
   let bds' = zip (map (name . fst) bds) ads
   vs <- mapM (eval' rec . snd) bds
   mapM_ (uncurry (writeVar @v)) (zip ads vs)
   withExtendedDynamic bds' (eval' rec e2)
eval'' rec (Begin exs _) =
   if null exs
   then return nil
   else last <$> mapM (eval' rec) exs
eval'' rec e@(Pair e1 e2 _) =
   stoPai e =<< liftA2 cons (eval' rec e1) (eval' rec e2)
eval'' _ e@(Var (Ide x _)) = do 
    env' <- getEnv 
    unless (Environment.contains x env') (error $ "Variable " ++ x ++ " not found at " ++ show (spanOf e))
    lookupEnv x >>= lookupVar >>= showIfBot (show x ++ " not in store")
eval'' _ (DynVar (Ide x _)) =
   lookupDynamic x >>= lookupVar >>= showIfBot (show x ++ " dyn not in store")
eval'' _ (Self _) = aref <$> getSelf @v
eval'' rec (Blame e contract loc) = do
   liftIO (putStrLn $ "blame error for " ++ show e)
   party <- eval' rec e 
   recordBlame loc e party contract
   escape $ BlameError (show party) loc
eval'' rec (Trace e _) = do
   v <- eval' rec e
   (liftIO . putStrLn . ((("TRACE@" ++ show (spanOf e) ++ ": ")  ++) . show)) v  $> v
eval'' rec (Error e loc) =
   eval' rec e >>= escape . flip BlameError loc . show
eval'' rec (Parallel es _) = mjoins (map (eval' rec) es)
eval'' _ (Loc _ _) = return nil
eval'' _ e@(Input _) = fresh e
eval'' _ e@(Fresh _) = fresh e
-- eval'' _ e = error $  "unsupported expression: " ++ show e

logCoverage :: (SpanOf e, MonadIO m) => e -> m a -> m a
logCoverage e ma = do
       when coverageEnabled $ do
           let sp = spanOf e
               fn = filename sp
               ln = line (startPosition sp)
           liftIO $ hPutStrLn stderr $ "[coverage] (" ++ show fn ++ " " ++ show ln ++ ")"
       ma
    where coverageEnabled = False

eval' :: forall v m k km e . EvalM e v k km m => (Cmp -> m v) -> Exp -> m v
eval' rec e = logCoverage e $ eval'' rec e

evalReceive :: EvalM e v k km m => (Cmp -> m v) -> Exp -> m v
evalReceive rec expr@(Receive pats _) = logCoverage expr $ recvMsg $ matchList
         (\e -> allocMapping >=> (`withEnv'` eval' rec e))
         pats
   where withEnv' e = withEnv (const e)

evalReceive _ e = error $ "not a receive expression " ++ show e

trySend :: forall e v k km m . PrimM e v k km m => v -> v -> m ()
trySend ref msg = do
   result <- fromBL @v (isActorRef ref)
   choice result
          (mjoinMap (`send` msg) (arefs' ref))
          (escape NotAnActorReference)

-- TODO: variable number of arguments
apply :: forall e v k km m . EvalM e v k km m => (Cmp -> m v) -> Exp -> [Exp] -> v -> [v] -> m v
apply rec e es v vs = choices
   [(isClo v, mjoinMap (\env -> applyClosure e es env rec vs) (clos v)),
    (isPrim v, mjoinMap (\nam -> applyPrimitive rec nam e vs) (prims v))]
   (escape (NotAFunction (spanOf e)))
applyClosure ::forall e v k km m . EvalM e v k km m => Exp -> [Exp] -> (Exp, Env v) -> (Cmp -> m v) -> [v] -> m v
applyClosure e es (lam@(Lam prs prz _ _), env) rec vs
  | length es /= length vs = 
        error "invariant violated: number of operands in the application must equal the number of evaluated operand values"
  | length prs /= length vs && isNothing prz = error $ "invalid number of arguments at " ++ show (spanOf e) 
  | otherwise = withCtx (pushCallSite (spanOf e)) $ do
            ads <- mapM alloc prs
            -- in case the function supports a variable number of arguments
            listArgv <- allocList (drop (length prs) es) (drop (length prs) vs)
            przAdr <- maybe (return Nothing) (fmap Just . alloc) prz
            when (isJust przAdr) (writeVar @v (fromJust przAdr) listArgv)
            let varBdn = maybe [] List.singleton $ liftA2 (,) (fmap name prz) przAdr
            -- regular arguments
            let bds = zip (map name prs) ads ++ varBdn
            mapM_ (uncurry (writeVar @v)) (zip ads vs)
            -- evaluate the body of the applied function
            withEnv (const env) (withExtendedEnv bds (rec $ FuncBdy lam))
applyClosure _ _ _  _ _ = error "invalid closure"

applyPrimitive :: forall v m k km e . EvalM e v k km m => (Cmp -> m v) -> String -> Exp -> [v] -> m v
applyPrimitive rec prm expr ops =
   runApplyT rec $ (runPrimitive @_ @_ @e $ fromMaybe (error $ "could not find " ++ prm) $ lookupPrimitive prm) expr ops

type Mapping v = Map Ide v

allocMapping :: forall e v k km m . EvalM e v k km m => Map Ide v -> m (Env v)
allocMapping bds = do
   env <- getEnv
   foldM (\env' (ide@(Ide nam _), v) -> do { adr <- alloc ide ; writeVar @v adr v ; return (extend nam adr env') }) env (Map.toList bds)

-- | Allocates the list of values in the second argument as a SimpleActor list 
-- using the list in the first argument as allocation sites.
allocList :: EvalM e v k km m => [Exp] -> [v] -> m v
allocList es vs =
    foldrM (\(e, v) ptr -> stoPai e (cons v ptr)) nil (zip es vs)


-- | Matches a list of patterns (from top to bottom) 
-- against a value.
--
-- NOTE: written in CPS because @Exp@ is not @Joinable@
matchList :: EvalM e v k km m => (Exp -> Mapping v -> m v) -> [(Pat, Exp)] -> v -> m v
matchList _ [] _ = escape MatchError
matchList f ((pat, e):pats) value =
   -- TODO: don't rethrow the error, use `catchOn` for this
   (match pat value >>= f e) `catchOn` (fromBL . isMatchError, const $ matchList f pats value)

type MatchM v m = (
   -- Monadic constraints
   MonadEscape m,
   MonadJoin m,
   MonadBottom m,
   Domain (Esc m) DomainError,
   Domain (Esc m) Error,
   StoreM (VaAdr v) v m,
   StoreM (PaAdr v) (PaiDom v) m,
   StoreM (StAdr v) (StrDom v) m,
   MonadChoice v m,
   AllocM m Ide (VaAdr v),
   AllocM m Exp (PaAdr v),
   AllocM m Exp (StAdr v),
   DomainClass.Exp v ~ Exp,
   -- Value domain constraints
   Joinable v,
   EqualLattice v,
   SchemeValue v,
   ActorDomain v,
   Pair.Content (PaiDom v) ~ v,
   VarDom v ~ v
   )

-- | Match a pattern against a value
match :: forall v m . MatchM v m => Pat -> v -> m (Mapping v)
match (IdePat nam) val = return $ Map.fromList [(nam, val)]
match (ValuePat lit s) v = do
   result <- fromBL @v =<< (eql <$> injectLit lit (Literal lit s) <*> pure v)
   choice result
          (return Map.empty)
          (escape MatchError)
match (PairPat pat1 pat2) v = do
       choice (isPaiPtr v) (pptrs v >>= derefPai @v (const matchPair)) (escape MatchError)
   where matchPair vp =
            Map.unionWith (\v1' v2' -> if v1' == v2' then v1' else error "cannot map same variable to different values")
                      <$> match pat1 (car vp)
                      <*> match pat2 (cdr vp)
match (AliasPat ide pat) v =
   -- Match both the variable and the inner pattern
   Map.unionWith (\v1' v2' -> if v1' == v2' then v1' else error "cannot map same variable to different values")
             <$> match (IdePat ide) v
             <*> match pat v

injectLit :: forall v m . MatchM v m => Lit -> DomainClass.Exp v -> m v
injectLit (Boolean b)    = const $ return $ inject b
injectLit (Symbol s)     = const $ return $ symbol s
injectLit (Num n)        = const $ return $ inject n
injectLit (Character c)  = const $ return $ inject c
injectLit (Real r)       = const $ return $ inject r
injectLit Nil            = const $ return nil
injectLit (String s)     = flip (stoStr @v) (inject s)


------------------------------------------------------------
-- Primitives
------------------------------------------------------------

newtype SAPrim v = SAPrim (forall e k km m . (PrimM e v k km m) => [v] -> Exp -> m v)

instance SimpleActorPrimitive v (SAPrim v) where
   runPrimitive (SAPrim p) = flip p

-- | A nullary primitive
aprim0 :: (forall e k km m . (PrimM e v k km m) => Exp -> m v) -> SAPrim v
aprim0 f = SAPrim $ const f

-- | A primitive on one argument
aprim1 :: (forall e k km m . PrimM e v k km m => v -> Exp -> m v) -> SAPrim v
aprim1 f = SAPrim $  \case [v] -> f v
                           vs -> const $ escape $ ArityMismatch 1 (length vs)

-- | A primitive on two arguments
aprim2 :: (forall e k km m . PrimM e v k km m => v -> v -> Exp -> m v) -> SAPrim v
aprim2 f = SAPrim $ \case [v1, v2] -> f v1 v2
                          vs -> const $ escape $ ArityMismatch 2 (length vs)

-- | Primitives specific to the simple actor language
actorPrimitives :: forall v . Map String (SimpleActorPrim v)
actorPrimitives =  Prm @v <$> Map.fromList [
   ("wait-until-all-finished", aprim0 $ const $ return nil ),
   ("send^" , aprim2 $ \rcv msg _ -> trySend rcv msg >> return nil),
   ("list", aprim0 $ const $ return nil),
   ("print-env", aprim0 $ const $ (liftIO . print =<< getEnv) $> nil),
   ("string=?", aprim2 $ const $ (\_ _ -> return boolTop)),
   ("fresh-string", aprim0 $ (\e -> fresh e)),
   ("sleep", aprim1 $ const $ const $ return nil),
   -- TODO: move this primitive to somewhere else, since it belongs to the symbolic domain
   -- ("fresh", aprim1 $ \v e -> do { adr <- alloc (Ide "fresh" (spanOf e)) ;  writeVar adr (var adr v) ; return $ var adr v }),
   ("print", aprim1 $ const $ const $ return nil) ]

-- | Scheme primitives
schemePrimitives :: forall v . Map String (SimpleActorPrim v)
schemePrimitives = Prm @v <$> primitivesByName @v

erlangPrimitives :: forall v . Map String (SimpleActorPrim v)
erlangPrimitives = Prm <$> Erl.erlangPrimitives

-- | The names of all primitives
allPrimitives :: [String]
allPrimitives = Map.keys actorPrimitives ++ Map.keys schemePrimitives ++ Map.keys erlangPrimitives

allPrimitiveFunctions :: Map String (SimpleActorPrim v)
allPrimitiveFunctions = Map.unions [actorPrimitives,  schemePrimitives, erlangPrimitives]

-- | Lookup a primitive starting from the actor primitives
lookupPrimitive :: String -> Maybe (SimpleActorPrim v)
lookupPrimitive = untilJust [ (`Map.lookup` erlangPrimitives), (`Map.lookup` actorPrimitives), (`Map.lookup` schemePrimitives) ]

-- | Run the functions given as a list in the first argument on the 
-- second argument until an output is found that is @Just@.
untilJust :: [a -> Maybe b] -> a -> Maybe b
untilJust fs a = foldl (`maybe` Just) Nothing (fmap ($ a) fs)

-- | Compute a store containing the set of primitives
initialSto :: (Store s (VaAdr v) v, SchemeDomain v) => [String] -> (String -> VaAdr v) ->  s
initialSto prms allocPrm = Store.from $ fmap (\nam -> (allocPrm nam, prim nam)) prms

------------------------------------------------------------
-- MonadCall
------------------------------------------------------------

newtype SemanticsT v m a = SemanticsT (IdentityT m a)
                         deriving (Applicative, Functor, Monad, MonadLayer, MonadTrans)
