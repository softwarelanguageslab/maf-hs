{-# LANGUAGE UndecidableInstances, FlexibleInstances, ConstraintKinds #-}
{-# OPTIONS_GHC -Wall #-}
module Analysis.Scheme where

import Prelude hiding (exp, lookup)

import Debug.Trace

import Analysis.Scheme.Primitives
import qualified Analysis.Scheme.Semantics as Semantics
import Analysis.Scheme.Monad (SchemeM)
import Analysis.Monad hiding (getEnv)

import Control.SVar.ModX
import Control.Monad.Trans.Class
import Control.Monad.Join
import Control.Monad.Layer
import Control.Monad.Error (MonadError)
import Control.Monad.Cond (unlessM)

import Syntax.Scheme
import Domain (Vlu, subsumes, JoinLattice, bottom)
import Domain.Scheme hiding (Exp, Env)

import Data.DMap (DMap, fromMap, Hashable)
import Data.Map (Map)
import Data.Function ((&))
import Data.Dynamic
import Data.Functor.Identity
import Data.TypeLevel.Ghost
import Data.Functor ((<&>))

-----------------------------------------
-- Shorthands
-----------------------------------------


type Program              = Exp
type Env var v ctx dep    = Map String var          -- ^ the initial environment
type Sto var ctx v dep    = Map var v               -- ^ non-heap allocated values
type DSto ctx v           = DMap (Adrs v ctx)       -- ^ combined store with heap allocated values

-----------------------------------------
-- Store & Environment
-----------------------------------------

-- | All address types in the combined store mapped to the value
-- stored at their location.
type Adrs v ctx =  SchemeAdrs v

class (Hashable var, Ord var) => VarAdr var v ctx dep | var -> v, var -> ctx, var -> dep where
   retAdr :: Component (ModF var v ctx dep) -> var
   prmAdr :: String -> var

-- | The initial environment used by 
-- the analysis
analysisEnvironment :: VarAdr var v ctx dep => Env var v ctx dep
analysisEnvironment = initialEnv prmAdr

-- | The initial store
analysisStore :: forall v ctx dep var . (SchemeAnalysisConstraints var v ctx dep)
              => Env var v ctx dep -> DSto ctx v
analysisStore = fromMap . initialSto @v

-----------------------------------------
-- ModF
-----------------------------------------

data ModF var v ctx dep

class SchemeAlloc ctx var v dep | ctx -> var, ctx -> v, ctx -> dep where
  allocPai :: Exp -> ctx -> PAdr v
  allocVec :: Exp -> ctx -> VAdr v
  allocStr :: Exp -> ctx -> SAdr v
  allocVar :: Ide -> ctx -> var
  allocCtx :: Exp -> ctx -> ctx

instance (SchemeAnalysisConstraints var v ctx dep) => ModX (ModF var v ctx dep) where
  -- A component is a closure + a context
  type Component (ModF var v ctx dep)  = (Exp, Env var v ctx dep, ctx, GT (var, v, dep))
  -- | Global store
  type State (ModF var v ctx dep)      = DSto ctx v
  -- | Dependencies are tracked using SVar
  type Dep (ModF var v ctx dep)        = dep
  -- | The analysis of a single component runs the Scheme semantics
  -- on the body of that component
  analyze (exp, env, ctx, _) store = 
       let (((_, error), (spawns, triggers, registers)), sto) = trace (show exp) $ (Semantics.eval exp >>= writeAdr (retAdr (exp, env, ctx, Ghost)))
              & runEvalT
              & runErr
              & runCallT' @v @ctx
              & runSto store
              & runEnv env
              & runAlloc @PaAdr (allocPai @ctx)
              & runAlloc @VeAdr (allocVec @ctx)
              & runAlloc @StAdr (allocStr @ctx)
              & runAlloc @VrAdr (allocVar @ctx)
              & runCtx  ctx
              & runIdentity
       in (sto, spawns, triggers, registers)

-----------------------------------------
-- Open recursion for evaluation
-----------------------------------------

newtype BaseSchemeEvalT v m a = BaseSchemeEvalT { getInnerEvalT :: m a } deriving (Monad, Functor, Applicative)

instance (Monad m) => MonadLayer (BaseSchemeEvalT v m) where
   type Lower (BaseSchemeEvalT v m) = m
   upperM = BaseSchemeEvalT 
   lowerM f (BaseSchemeEvalT m) = BaseSchemeEvalT (f m)

instance (MonadJoin m) => MonadJoin (BaseSchemeEvalT v m) where
   mzero = BaseSchemeEvalT mzero
   mjoin (BaseSchemeEvalT ma) (BaseSchemeEvalT mb) = BaseSchemeEvalT $ mjoin ma mb

instance MonadTrans (BaseSchemeEvalT v) where
   lift = BaseSchemeEvalT

instance (MonadError m, SchemeM (BaseSchemeEvalT v m) v, SchemeAnalysisConstraints var v ctx dep) => (Analysis.Monad.EvalM (BaseSchemeEvalT v m) v Exp) where
   eval = trace "eval" Semantics.eval


runEvalT :: BaseSchemeEvalT v m a -> m a
runEvalT (BaseSchemeEvalT m) = m

-----------------------------------------
-- Polymorphic ModF dependencies 
-----------------------------------------

class (Ord dep, Hashable dep) => Dependency adr dep | adr -> dep where
   dep :: adr -> dep

-----------------------------------------
-- CallM & StoreM implementation
-----------------------------------------

newtype CallT' var v ctx dep m a = CallT' (m a) deriving (Monad, Functor, Applicative)
instance {-# OVERLAPPING #-} (Monad m, 
      SchemeAnalysisConstraints var v ctx dep,
      JoinLattice v
   ) => CallM (CallT' var v ctx dep m) (Env var v ctx dep) v where
   call = const $ return bottom

instance (Monad m) => MonadLayer (CallT' var v ctx dep m) where
   type (Lower (CallT' var v ctx dep m)) = m
   upperM = CallT'
   lowerM f (CallT' m) = CallT' (f m)

instance (MonadJoin m) => MonadJoin (CallT' var v ctx dep m) where 
   mzero = CallT' mzero
   mjoin (CallT' ma) (CallT' mb) = CallT' $ mjoin ma mb

runCallT' :: forall v ctx m a c dep var . (Monad m, c ~ ModF var v ctx dep)
         => CallT' var v ctx dep m a
         -> m (a, ([Component c], [Dep c], [Dep c]))
runCallT' (CallT' m) = m <&> (, ([], [], []))


newtype CallT var v ctx dep m a = CallT (ModxT (ModF var v ctx dep) m a) deriving (Monad, Functor, Applicative, MonadLayer)

instance (Ord (Component (ModF var v ctx dep)), Ord (Dep (ModF var v ctx dep)), MonadJoin m) => MonadJoin (CallT var v ctx dep m) where
   mzero = CallT mzero
   mjoin (CallT ma) (CallT mb) = CallT $ mjoin ma mb

needsUpdate :: forall m adr v . (JoinLattice v, Show v, Vlu adr ~ v, StoreM m adr) => adr -> v -> m Bool
needsUpdate adr vlu' = trace (show vlu') $ do
   _  <- trace "lookup before" $ return ()
   vlu <- lookupAdr adr
   if subsumes vlu' vlu then
      return False
   else
      return True

-- | When a store update occurs, registers that update as a write effect,
-- when a store read occurs, registers that read as a read effect
instance {-# OVERLAPPING #-} (
            Dependency adr dep,
            StoreM m adr,
            JoinLattice (Vlu adr),
            Show (Vlu adr),
            SchemeAnalysisConstraints var v ctx dep
   ) => StoreM (CallT var v ctx dep m) adr where

   writeAdr adr vlu = trace ("writeAdr " ++ show vlu) $ CallT @var @v @ctx $ do
      -- TODO: it is rather strange that type inference
      -- cannot figure out that `c` must equal `ModF v ctx`
      -- hence the explicit type annotations
      unlessM (lift $ needsUpdate adr vlu) $ do
         trigger @_ @(ModF var v ctx dep) (dep adr)

      lift $ writeAdr adr vlu

   updateAdr adr vlu = CallT $ do
      unlessM (lift $ needsUpdate adr vlu) $ do
         trigger @_ @(ModF var v ctx dep) (dep adr)
      lift $ updateAdr adr vlu

   lookupAdr adr = CallT $ do
      _ <- register @_ @(ModF var v ctx dep) (dep adr)
      lift $ lookupAdr adr


-- | This instances spawns the called function as a component, 
-- and reads the return value from the store.
instance {-# OVERLAPPING #-} (CtxM m ctx,
          Monad m,
          StoreM (CallT var v ctx dep m) var,
          SchemeAnalysisConstraints var v ctx dep
         ) => CallM (CallT var v ctx dep m) (Env var v ctx dep) v where
   call (e, env) = do
      -- get the current context
      ctx <- CallT $ lift getCtx
      -- create a new component from this context
      let comp = (e, env, ctx, Ghost)
      --  spawn  the new component
      _ <- CallT $ spawn comp
      -- lookup the return value of the component
      lookupAdr (retAdr comp)

-- | Run the CallT monad and peel it off the stack whilst returning 
-- its encapsulated ModX state.
runCallT :: forall v ctx m a c dep var . (Monad m, c ~ ModF var v ctx dep)
         => CallT var v ctx dep m a
         -> m (a, ([Component c], [Dep c], [Dep c]))
runCallT (CallT m) = runModxT @c m

-----------------------------------------
-- Analysis
-----------------------------------------

-- TODO: too many constraints, makes it more difficult to parse the program
-- as written, try to simplify
type SchemeAnalysisConstraints var v ctx dep = (
         Show v,
         Show (Vlu (PAdr v)),
         Show (Vlu (SAdr v)),
         Show (Vlu (VAdr v)),
         SchemeDomain v,
         SchemeConstraints v Exp var (Env var v ctx dep),
         StoreDefinedFor v,
         Dependency var dep,
         Dependency (PAdr v) dep,
         Dependency (SAdr v) dep,
         Dependency (VAdr v) dep,
         VarAdr var v ctx dep,
         Ord ctx, Ord v, Hashable ctx, Hashable v, Typeable ctx, Typeable v, SchemeAlloc ctx var v dep)

-- | The result of the analysis
newtype AnalysisResult var v ctx dep = AnalysisResult (State (ModF var v ctx dep))

-- | Analyses the given program into an analysis
-- result. It uses the default initial environment
-- as specified in `Analysis.Scheme.Primitives`
analyzeProgram :: forall v ctx wl dep var .
                  (WorkList wl (Component (ModF var v ctx dep)), SchemeAnalysisConstraints var v ctx dep)
               => Program  -- ^ the program analyse
               -> wl       -- ^ the initial contents of the worklist, can be empty. This function will add the initial component to it. 
               -> ctx -- ^ context allocation function for a given expression (usually associated with a function call)
               -> State (ModF var v ctx dep)
analyzeProgram exp initialWl initialCtx = runModX initialWl' (analysisStore @v analysisEnvironment)
  where initialWl' = add (exp, analysisEnvironment, initialCtx, Ghost) initialWl
