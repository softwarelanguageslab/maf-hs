{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fewer imports" #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Analysis.SimpleActor.Fixpoint(
    analyze,
    AnalysisState (..),
    System (..),
    BlameRecord(..),
    analyzeIO
) where


import Control.Monad.Escape (MayEscapeT)
import Data.Set (Set)
import Analysis.SimpleActor.Monad
    ( ActorError,
      Cmp(..),
      MonadMailbox(..),
      MonadSpawn(..),
      MonadDynamic(..),
      MonadFresh(..),
      MonadBlame(..), message,

      )
import Control.Monad.Reader (ReaderT, MonadReader (..))
import Analysis.SimpleActor.Fixpoint.Common
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens
import Control.Monad.State (StateT (runStateT), execStateT, mapStateT)
import Control.Monad.Except (ExceptT, throwError)
import GHC.Generics (Generic)
import Analysis.Monad.Environment (EnvM (..))
import Analysis.Monad.Cache (CacheT, MonadCache (..))
import Analysis.Monad.Map (MapM (..), MapT, runMapT, In, Out, runWithMapping')
import Analysis.Monad.Join (SetHookNonDetT, MonadNonDetHook (..))
import Analysis.Monad (WorkListT, CtxM (..), runWithComponentTracking, runIntraAnalysis, StoreM (lookupAdr, hasAdr))
import qualified Lattice.Class as Lattice
import Data.Maybe (fromMaybe, maybeToList)
import Analysis.Monad.DependencyTracking (DependencyTrackingT, runWithDependencyTracking)
import qualified Data.Set as Set
import Analysis.Monad.Fix (lfp, runAroundFixT)
import qualified Data.HashMap.Lazy as HashMap
import qualified Analysis.SimpleActor.Semantics as Semantics
import Lattice.Class (Joinable, BottomLattice, Meetable)
import Control.Monad.Layer (MonadLayer(..))
import Analysis.Monad.Partition (MonadPartition (..))
import Analysis.Actors.Monad (MonadActorLocal (..))
import qualified Domain.Actor
import Analysis.Monad.Allocation (AllocM (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Analysis.Monad.ComponentTracking (ComponentTrackingT)
import Data.Bifunctor
import qualified Analysis.Fixpoint as Fix
import Domain.Actor (Pid(EntryPid))
import Analysis.Monad.WorkList ( runWithWorkList )
import Data.Tuple.Extra
import qualified Analysis.Actors.Mailbox.Partitioned as MB
import Data.Functor
import Control.Monad ((>=>))

-- These are here for the instances of each domain for the "TopLifted" value.
import Domain.Core.PairDomain.TopLifted ()
import Domain.Core.StringDomain.TopLifted ()

import qualified Analysis.Symbolic.Monad as SCV
import qualified Domain.SimpleActor as Domain
import Symbolic.AST (emptyPC)
import qualified Solver as SCV
import Domain.Core.AbstractCount (AbstractCount)
import Analysis.Monad.AbstractCount
import Solver.Z3 (runZ3SolverWithPrelude)
import Solver (runCachedSolver)
import Analysis.Monad.IntraAnalysis (IntraAnalysisT, MonadIntraAnalysis (..))
import Analysis.Monad.Store (MonadMultiStore (..))
import qualified Analysis.Monad.Map as MapM
import qualified Analysis.ASE.SymbolicVariable as ASE
import qualified Analysis.ASE.PC as ASE
import Syntax.Span
import qualified Domain.Symbolic.Class as Symbolic
import Syntax.AST

-- These are here for the instances of each domain for the "TopLifted" value.
import Domain.Core.PairDomain.TopLifted ()
import Domain.Core.StringDomain.TopLifted ()
-- import qualified RIO as Debug
import qualified Control.Monad.State as State
import qualified Domain.Scheme.Class as Scheme
import Analysis.Scheme.Monad (SchemeStoreT, runSchemeStoreT, SchemeStoreM)
import qualified Domain.Scheme.Store as Store
import Analysis.Store (traceStore)
import Domain.Core.VectorDomain.PIVector
import Domain.Core.PairDomain.Class
import Control.Monad.Cond (ifM)
import qualified Lattice.Trace as Lattice
import Domain.Scheme.Store (SchemeStore)
import Control.Monad.Identity
import Control.Monad.Join (MonadJoinable(..), MonadBottom (mbottom))
import Data.Aeson (ToJSON)
import Control.DeepSeq
import System.Clock

------------------------------------------------------------
-- Shorthands
------------------------------------------------------------

type Beh = (Exp, ActorEnv, Map String (Store.VarAdr K))
type Err = Set ActorError

------------------------------------------------------------
-- Hacks
------------------------------------------------------------

-- This instance makes little sense on its own, but stems from the fact 
-- that Monarch requires everything that could be used in an 'mjoin' 
-- (see MonadJoinable) to be Lattice.Class.Joinable. On our monad stack
-- we use 'ExceptT' which is represented by 'Either', hence the instance.
--
-- However, we only use the list monad for non-determinism and 
-- never join values within the analysis of a single turn, hence 
-- the 'join' function does not actually get called making the error
-- below unreachable.
instance Joinable (Either e a) where
    join = error "Either is not joinable"

instance BottomLattice (Either e a) where
    bottom = error "Either does not have a bottom lattice"

------------------------------------------------------------
-- Monadic contexts
------------------------------------------------------------

data Ctx = Ctx {
        -- | Lexical environment
        _env :: ActorEnv,
        -- | Dynamic bindings 
        _dyn :: Map String (Scheme.VaAdr ActorVlu),
        -- | Self
        _self :: ActorRef,
        -- | Additional context, for instance for k-cfa
        _ctx :: AdrCtx
    } deriving (Ord, Eq, Show, Generic)

instance NFData Ctx

$(makeLenses ''Ctx)

emptyCtx :: ActorRef -> Ctx
emptyCtx ref = Ctx HashMap.empty initialDynEnvironment ref (initialContext 1)

-- | Intra-turn analysis state
data State = State {
        -- | The inbox of the actor that is being analyzed.
        _inbox :: PMB
    } deriving (Ord, Eq, Show, Generic)


instance NFData State

instance Joinable State where
    join (State inbox1) (State inbox2) =
        State (Lattice.join inbox1 inbox2)
instance Meetable State where
    meet (State _) (State _) =
        error "NYI"
        -- State (Lattice.meet inbox1 inbox2) (Lattice.meet outbox1 outbox2)
instance BottomLattice State where
    bottom = State Lattice.bottom

$(makeLenses ''State)

-- | Inter-turn analysis state
data InterTurnState = InterTurnState {
        -- | Track the outgoing mail
        _outbox :: ActorMai
    } deriving (Ord, Eq, Show, Generic)

instance NFData InterTurnState
$(makeLenses ''InterTurnState)

-- | Inter-actor actor system state
data System = System {
        -- | All the mailboxes in the system
        _mbs   :: ActorMai,
        -- | All of the reachable behaviors/turns indexed
        -- by each actor.
        _turns :: Map ActorRef (Set Turn),
        -- | The initial behaviors of an actor
        _initialBeh :: Map ActorRef Beh
    } deriving (Ord, Eq, Show, Generic)

instance NFData System

------------------------------------------------------------
-- Turns and continuations
------------------------------------------------------------

data Turn = Turn {
    -- | The active behavior of the actor in that turn
    _behavior :: Cnt,
    -- | The state of the actor (its inboxes and outboxes)
    _state    :: State
    } deriving (Ord, Eq, Show, Generic)
instance NFData Turn

data Cnt = Become Beh | Terminated deriving (Ord, Eq, Show, Generic)
instance NFData Cnt

cntEither :: Either Beh a -> Cnt
cntEither = either Become (const Terminated)

$(makeLenses ''System)
$(makeLenses ''Turn)

-- | Compute the cache key corresponding to the given turn
-- returns 'Nothing' if the turn's continuation does not 
-- have a valid cache key (i.e., when it is terminated), 
-- or 'Just ProcKey' when a ProcKey could be computed.
turnKey :: ActorRef -> Turn -> Maybe ProcKey 
turnKey selfRef (Turn (Become beh) st) = pure $ entryKey selfRef beh st
-- Turn has terminated, ignore.
turnKey _ _ = Nothing

-- | Returns the span of the expression represented by the component
spanOfCmp :: ProcKey -> Span
spanOfCmp (cmp ::*:: _ ::*:: _ ::*:: _) = spanOf cmp
spanOfCmp _ = error "unreachable pattern"

------------------------------------------------------------
-- Monads
------------------------------------------------------------

-- The analysis consists of three fixpoints: 
--
-- * A fixpoint over intra-actor procedural calls which is 
-- necessary because internal calls can be recursive. 
-- * A fixpoint over actor behaviors, which is necessary because 
-- the mailbox abstraction might introduce loops and cause the 
-- analysis of a single actor to loop as well. 
-- * A fixpoint over actor systems, which is necessary to ensure 
-- that global effects (sends and receives) between actors are 
-- propagated and taken into account.
--
-- Each of the monadic contexts defined below are contexts 
-- that are used within one iteration of each of these fixpoints.

------------------------------------------------------------

-- | Intra-procedural fixpoint: monadic context used for analyzing 
-- a single function call.
type ProcT m = (
        -- escaping control flow, primarily used for handling errors
        MayEscapeT Err (
        -- the path constraint is also part of the context and is cached
        SCV.FormulaT Domain.SymVar ActorVlu  (
        -- the evaluation context, includes a self-reference, lexical and dynamic environment.
        ReaderT Ctx m
     )))

-- | The key that is used for caching function call results
type ProcKey = Key (ProcT (IntraT Identity)) Cmp
-- | The result values of function call results in the cache
type ProcVal = Val (ProcT (IntraT Identity)) ActorVlu

initialProcKey :: Cmp -> Ctx -> State -> ProcKey
initialProcKey cmp ctx' st' = cmp <+> emptyPC <+> ctx' <+> st'

-- | Debug helper: the number of entries in each component map of a store.
storeSizes :: ActorSto -> (Int, Int, Int, Int)
storeSizes s = ( Map.size (s ^. Store.paiStore)
               , Map.size (s ^. Store.strStore)
               , Map.size (s ^. Store.vecStore)
               , Map.size (s ^. Store.varStore) )

-- | The key corresponding to the initial component of the intra-turn analysis.
entryKey :: ActorRef -> Beh -> State -> ProcKey
entryKey selfRef beh st = initialProcKey cmp' ctx' st
    where ctx' = emptyCtx selfRef
               & env .~ (beh ^._2)
               & dyn .~ (beh ^._3)
          cmp' = case beh ^._1 of
                   expr@Receive {} -> ReceiveExp expr
                   expr            -> ActorExp expr

-- | Maps a component's cache key to a flow-sensitivity index. The choice of
-- @ix@ selects which parts of the key participate in flow sensitivity; @ix ~ ()@
-- collapses every component onto a single shared store.
class Ord ix => ToIndex ix where
    toIndex :: ProcKey -> ix

instance ToIndex () where
    toIndex _ = ()

-- | An index for each actor 
newtype ActorIdx = ActorIdx ActorRef
        deriving (Ord, Eq, Show, Generic)

instance NFData ActorIdx

instance ToIndex ActorIdx where 
    toIndex (_ ::*:: _ ::*:: ctx' ::*:: _) = 
        ActorIdx $ _self ctx'
    toIndex _ = error "unreachable pattern"

------------------------------------------------------------

-- | Intra-actor fixpoint: monadic context used for analyzing a single 
-- turn of an actor.
type IntraT m = (
        -- allows returning from the analysis when ending a turn (i.e., when the 'become' statement is reached, 
        -- or when a 'receive' statement is evaluated). 
        ExceptT Beh (
        -- the state of the actor (inbox and outbox) is tracked in this monad.
        StateT State (
        -- the intra-actor analysis is path-sensitive meaning that it tracks 
        -- inbox content independently for each path through the actor. 
        SetHookNonDetT (
        CacheT (
        StateT InterTurnState m
    )))))


-- | Lift an IntraT computation to a ProcT computation
liftIntraT :: Monad m => IntraT m a -> ProcT (IntraT m) a 
liftIntraT = upperM . upperM . upperM

-- | Lift a InterTurnState computation to an IntraT computation
liftInterTurnT :: Monad m => StateT InterTurnState m a -> IntraT m a
liftInterTurnT = upperM . upperM . upperM . upperM

------------------------------------------------------------

-- | Inter-actor fixpoint: monadic context used for analyzing a single 
-- actor system. 
type SystemT m = (
        StateT System m
    )

------------------------------------------------------------

-- | Datatype for recording a blame error
data BlameRecord = BlameRecord {
        partyExpr    :: Exp, 
        partyValue   :: ActorVlu,
        contractExpr :: Exp,
        blameLoc     :: Span
    } deriving (Ord, Eq, Show, Generic)

instance NFData BlameRecord

-- | Global analysis state: the call-result cache plus bookkeeping (trace and
-- recorded blames). Dependency tracking lives in dedicated
-- 'DependencyTrackingT' layers, see 'GlobalT'.
data AnalysisState ix = AnalysisState {
        _storeIn    :: Map ix (SchemeStore Exp K ActorVlu),
        _storeOut   :: Map ix (SchemeStore Exp K ActorVlu),
        _spawnStore :: Map ActorRef ActorSto,
        _trace      :: [System],
        _blames     :: Set BlameRecord
    } deriving (Ord, Eq, Show, Generic)

instance NFData ix => NFData (AnalysisState ix)

$(makeLenses ''AnalysisState)


------------------------------------------------------------

-- | Monad global to the analysis
--
-- Dependency tracking is delegated to dedicated 'DependencyTrackingT' layers,
-- one per kind of dependency: call dependencies (keyed by 'ProcKey') and one
-- per store-address type (the multi-store keeps variables, pairs, vectors and
-- strings in separate maps). This mirrors the layering used in
-- 'Analysis.Scheme' and keeps the 'StateT AnalysisState' layer responsible
-- only for the call cache, the trace, and recorded blames.
type GlobalT ix m = (
        StateT (AnalysisState ix) (
        ComponentTrackingT ProcKey (
        MapT (In ix ActorSto) ActorSto ( 
        MapT (Out ix ActorSto) ActorSto ( 
        MapT (In ix SymCou) SymCou (
        MapT (Out ix SymCou) SymCou (
        MapT ProcKey ProcVal (
        DependencyTrackingT ProcKey ProcKey (
        DependencyTrackingT ProcKey (Store.VarAdr K) (
        DependencyTrackingT ProcKey (Store.PaiAdr Exp K) (
        DependencyTrackingT ProcKey (Store.VecAdr Exp K) (
        DependencyTrackingT ProcKey (Store.StrAdr Exp K) (
        DependencyTrackingT ProcKey (In ix ActorSto) (
        DependencyTrackingT ProcKey (Out ix ActorSto) (
        DependencyTrackingT ProcKey (In ix SymCou) (
        DependencyTrackingT ProcKey (Out ix SymCou) (
        WorkListT [ProcKey] m)))))))))))))))))

------------------------------------------------------------

-- Hooks executed at each branching point

newtype StoreHookT m a = StoreHookT { getStoreHookT :: IdentityT m a } 
    deriving (Functor, Applicative, Monad, MonadLayer)

runStoreHookT :: StoreHookT m a -> m a
runStoreHookT = runIdentityT . getStoreHookT

instance ( Monad m
         , MonadIO m
         , MonadIntraAnalysis ProcKey m
         , MonadAnalysisState ix m
         , MonadMultiStore ActorSto m
         , MapM (In ix ActorSto) ActorSto m
         , MapM (Out ix ActorSto) ActorSto m
         , MapM (In ix SymCou) SymCou m 
         , MapM (Out ix SymCou) SymCou m
         , MonadAbstractCount Domain.SymVar AbstractCount m
         , MonadIO m
         , ToIndex ix
         , Show ix
         ) => MonadNonDetHook (StoreHookT m) where
    -- Reset the working store to the input store recorded for this index.
    preBranch = StoreHookT $ upperM $ do
        -- logEvent PreBranch
        cmp <- currentCmp
        let idx = toIndex @ix cmp
        sto <- getStoreIn idx
        cou <- getCountIn idx
        putMultiStore sto
        putCounts cou
    -- Fold the resulting working store into the output store for this index.
    postBranch = StoreHookT $ upperM $ do
        -- logEvent PostBranch
        idx <- toIndex @ix <$> currentCmp
        getMultiStore >>= joinStoreOut idx
        getCounts >>= joinCountOut idx 

------------------------------------------------------------

-- The type aliases defined below are for convenience and readibility. 
-- The idea is that when defining each layer of fixpoint iterations, we peel off one 
-- of the monads in the stack and use the corresponding type alias to make it clear 
-- which leayer of fixpoint iteration we are working on.

-- The monad for the entire analysis
type AnalysisT ix m = 
    ( ProcT ( 
      IntraT (
      StoreHookT (
      AbstractCountT Domain.SymVar AbstractCount (
      SchemeStoreT Exp K ActorVlu (
      IntraAnalysisT ProcKey (
      SystemT (GlobalT ix m))))))))
-- The monad stack for the intra-actor fixpoint.
type AnalysisIntraT ix m = (IntraT (SystemT (GlobalT ix m)))
-- The monad stack for the inter-actor fixpoint. 
type AnalysisSystemT ix m = (SystemT (GlobalT ix m))
-- The monad stack for the analysis-global fixpoint.
type AnalysisGlobalT ix m = (GlobalT ix m)

------------------------------------------------------------
-- Monad instances
------------------------------------------------------------

instance (MonadAbstractCount Domain.SymVar AbstractCount m) => MonadFresh ActorVlu (ProcT m) where
    -- TODO: let the semantics decide what value to annotate with the symbolic variable
    fresh e = countIncrement var $> varValue
        where var =  flip ASE.SymbolicVariable ASE.emptyPC $ spanOf e
              varValue = flip Symbolic.var Lattice.top var


-- Intra-procedural monad instances.

-- The 'Ctx' data type captures the lexical and dynamic environment and thus implements the appropriate monad instances.
instance Monad m => EnvM (ProcT m) ActorAdr ActorEnv where
  withEnv f = lowerM (local (over env f))
  getEnv = view env
instance Monad m => MonadDynamic ActorAdr (ProcT m) where
    withDynamic f = lowerM (local (over dyn f))
    getDynamic = view dyn
-- The reader component tracks the actor local information in its context.
instance Monad m => MonadActorLocal ActorVlu (ProcT m) where
    withSelf ref = lowerM (local (set self ref))
    getSelf = view self
-- The 'Ctx' component tracks additional context information as well.
instance Monad m => CtxM (ProcT m) AdrCtx where
    withCtx f = lowerM (local (over ctx f))
    getCtx = view ctx
instance Monad m => AllocM (ProcT m) Exp (Store.PaiAdr Exp AdrCtx) where
    alloc expr = Store.PaiAdr expr <$> getCtx
instance Monad m => AllocM (ProcT m) Exp (Store.VecAdr Exp AdrCtx) where
    alloc expr = Store.VecAdr expr <$> getCtx
instance Monad m => AllocM (ProcT m) Exp (Store.StrAdr Exp AdrCtx) where
    alloc expr = Store.StrAdr expr <$> getCtx
instance Monad m => AllocM (ProcT m) Ide (Store.VarAdr AdrCtx) where
    alloc expr = Store.VrrAdr expr <$> getCtx

------------------------------------------------------------

-- Intra-actor monad instances.

-- TODO: actually implement this instance for any type of partitioning scheme.
instance Monad m => MonadPartition Partition (IntraT m) where
    integrate = const $ return ()
    get = return Lattice.bottom

-- | Adds the message context based on information in the monad
addMessageCtx :: (
      SCV.FormulaSolver Domain.SymVar m
    , MonadAbstractCount Domain.SymVar AbstractCount m
    , MonadIO m
    , SchemeStoreM Exp ActorVlu m
    ) => MsgPayload ->  (ProcT (IntraT m) Msg)
addMessageCtx payload = do 
        reachableAdrs <- traceStore (Lattice.trace payload) lookupSchemeAdr
        reachableValues <- Set.unions <$> mapM lookupSchemeAdr (Set.toList reachableAdrs)
        let reachableVariables = foldMap Symbolic.strictVariables reachableValues
        pc <- SCV.getPc
        let restrictedPC =  SCV.simplifyPC $ SCV.restrictPC reachableVariables pc
        count' <- getCounts
        let restrictedCounts = Map.restrictKeys count' reachableVariables 
        return $ message payload (restrictedPC, restrictedCounts)
    where lookupSchemeAdr :: forall m . (SchemeStoreM Exp ActorVlu m, MonadJoinable m) => Store.SchemeAdr Exp K -> m (Set ActorVlu)
          lookupSchemeAdr = \case 
            Store.SStrAdr _ -> 
                -- strings do not contain values from the variable domain
                return Set.empty
            Store.SPaiAdr adr -> 
                 ifM (hasAdr adr) (do
                    pairValue <- lookupAdr @_ @(Scheme.PaiDom ActorVlu) adr 
                    return $ Set.fromList [car pairValue, cdr pairValue])
                 (return Set.empty)
            Store.SVecAdr adr -> 
                ifM (hasAdr adr) 
                    (do 
                        (PIVector _ c) <- lookupAdr @_ @(Scheme.VecDom ActorVlu) adr 
                        return (Set.singleton c))
                    (return Set.empty)
            Store.SVarAdr adr -> 
                ifM (hasAdr adr)
                    (Set.singleton <$> lookupAdr @_ @ActorVlu adr)
                    (return Set.empty)

instance (
     MonadIO m
   , SCV.FormulaSolver Domain.SymVar m
   , MonadAbstractCount Domain.SymVar AbstractCount m
   , SchemeStoreM Exp ActorVlu m
   , MonadMultiStore ActorSto m
   , MonadAnalysisState ix m) => MonadMailbox Partition ActorRef ActorVlu MsgContext (ProcT (IntraT m)) where
  send ref v = do
      -- actually "send" the message
      v' <- addMessageCtx v
      liftIntraT $ liftInterTurnT $ do
        oldOutbox <- use outbox
        outbox . at ref . non MB.empty %= MB.enqueue Lattice.bottom Lattice.bottom v'
        outbox %= Lattice.join oldOutbox
        return ()
      -- ensure that the store at the receiving side knows the addresses
      -- and their corresponding values for addresses referenced from the message payload
      -- TODO: garbage collect here so that only values referenced from the message payload 
      -- are used.
      currentSto <- getMultiStore
      modifyAnalysisState (over spawnStore (Map.insertWith Lattice.join ref currentSto))
  select expr env' dyn' = liftIntraT $ throwError (expr, env', dyn') -- throwError is only here for its escaping mechanism, not for actually reporting an error
  recv = 
    liftIntraT $ uses inbox (MB.dequeue Lattice.bottom)
  putMailbox = liftIntraT . assign inbox
  integrateCtx (pc, counts) = SCV.putPc pc >> putCounts counts

------------------------------------------------------------

-- | Inter-actor monad instances

instance ( Monad m
         ) => MonadSpawn ActorVlu AdrCtx (SystemT m) where
    spawn behExpr environ k = do
        let newRef = Domain.Actor.Pid behExpr k
            beh = (behExpr, environ, initialDynEnvironment)
        initialBeh . at newRef ?= beh
        mbs . at newRef %= Just . fromMaybe Lattice.bottom
        return newRef

-- | Capture the store at the spawn point, at the 'SchemeStoreT' layer.
-- 'transferSystem' uses the recorded store to seed the new actor's input store
-- once its entry index is known.
instance {-# OVERLAPPING #-}
         ( Monad m
         , MonadSpawn ActorVlu AdrCtx m
         , MonadAnalysisState ix m
         ) => MonadSpawn ActorVlu AdrCtx (SchemeStoreT Exp K ActorVlu m) where
    spawn behExpr environ k = do
        newRef <- upperM (spawn behExpr environ k)
        sto    <- getMultiStore
        modifyAnalysisState (over spawnStore (Map.insertWith Lattice.join newRef sto))
        return newRef

------------------------------------------------------------

-- Analysis-global instances.


-- Tracking blame errors
instance Monad m => MonadBlame ActorVlu (StateT (AnalysisState ix) m) where
    recordBlame loc partyExpr partyValue contract = 
        blames %= Set.insert (BlameRecord partyExpr partyValue contract loc)

-- Access to the global analysis state

class Monad m => MonadAnalysisState ix m | m -> ix where
    getAnalysisState :: m (AnalysisState ix)
    putAnalysisState :: AnalysisState ix -> m ()
    modifyAnalysisState :: (AnalysisState ix -> AnalysisState ix) -> m ()
    modifyAnalysisState f = getAnalysisState >>= putAnalysisState . f
    {-# MINIMAL getAnalysisState, putAnalysisState #-}

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadLayer t, MonadAnalysisState ix m) => MonadAnalysisState ix (t m) where
    getAnalysisState = upperM getAnalysisState
    putAnalysisState = upperM . putAnalysisState

instance Monad m => MonadAnalysisState ix (StateT (AnalysisState ix) m) where
  getAnalysisState = State.get
  putAnalysisState = State.put


----------------------------------------
-- Flow-sensitive stores
----------------------------------------

-- | Read the input store recorded for an index, defaulting to bottom.
getStoreIn :: forall ix m . (Ord ix, MapM (In ix ActorSto) ActorSto m) => ix -> m ActorSto
getStoreIn =  fmap (fromMaybe Lattice.bottom) . MapM.get @(In ix ActorSto) . MapM.In

-- | Join a store into the input store recorded for an index.
joinStoreIn :: forall ix m . (Ord ix, MapM (In ix ActorSto) ActorSto m) => ix -> ActorSto -> m ()
joinStoreIn idx  = MapM.joinWith @(In ix ActorSto) (MapM.In idx)

-- | Read the output store recorded for an index, defaulting to bottom.
getStoreOut :: forall ix m . (Ord ix, MapM (Out ix ActorSto) ActorSto m) => ix -> m ActorSto
getStoreOut =  fmap (fromMaybe Lattice.bottom) . MapM.get @(Out ix ActorSto) . MapM.Out

-- | Read the output store recorded for an index, defaulting to mbottom 
getStoreOutShortciruit :: forall ix m . (Ord ix, MapM (Out ix ActorSto) ActorSto m, MonadBottom m) => ix -> m ActorSto 
getStoreOutShortciruit = MapM.get @(Out ix ActorSto) . MapM.Out >=> maybe mbottom return

-- | Join a store into the output store recorded for an index.
joinStoreOut :: forall ix m . (Ord ix, MapM (Out ix ActorSto) ActorSto m) => ix -> ActorSto -> m ()
joinStoreOut idx = MapM.joinWith @(Out ix ActorSto) (MapM.Out idx)

-- | Destructively update the store at the given index.
putStoreOut :: forall ix m . (Ord ix, MapM (Out ix ActorSto) ActorSto m) => ix -> ActorSto -> m ()
putStoreOut idx = MapM.put @(Out ix ActorSto) (MapM.Out idx)

-- | Register a system after a inter-actor turn, and returns the registered system
traceSystem :: Monad m => System -> AnalysisGlobalT ix m System
traceSystem sys = (trace %= (sys:)) $> sys

----------------------------------------
-- Flow-sensitive abstract counting for symbolic variables
----------------------------------------

joinCountIn :: forall ix m . (Ord ix, MapM (In ix SymCou) SymCou m) => ix -> SymCou -> m () 
joinCountIn idx = 
    MapM.joinWith @(In ix SymCou) (MapM.In idx) 

getCountIn :: forall ix m . (Ord ix, MapM (In ix SymCou) SymCou m) => ix -> m SymCou
getCountIn = 
    fmap (fromMaybe Lattice.bottom) . MapM.get @(In ix SymCou) . MapM.In 

joinCountOut :: forall ix m . (Ord ix, MapM (Out ix SymCou) SymCou m) => ix -> SymCou -> m () 
joinCountOut idx = 
    MapM.joinWith @(Out ix SymCou) (MapM.Out idx)

getCountOut :: forall ix m . (Ord ix, MapM (Out ix SymCou) SymCou m) => ix -> m SymCou
getCountOut = 
    fmap (fromMaybe Lattice.bottom) . MapM.get @(Out ix SymCou) . MapM.Out

putCountOut :: forall ix m . (Ord ix, MapM (Out ix SymCou) SymCou m) => ix -> SymCou -> m () 
putCountOut idx = 
    MapM.put @(Out ix SymCou) (MapM.Out idx)


------------------------------------------------------------
-- Logging & observability
------------------------------------------------------------

-- | Analysis events that can be logged in the JSON format
data LoggingEvent = 
    -- | Emitted when at the start of an intra-analysis and 
    -- reports which component is being analyzed. 
    -- Each component is identified by its source-code location (span). 
      IntraStarted Span
    -- | Emitted at the end of an intra-analysis
    | IntraEnded Span Integer
    -- | Emitted before the start of a branch, always happens after 
    -- the start, and before the end of an intra-analysis.
    | PreBranch 
    -- | Emitted after a branch has been fully evaluated, always happens 
    -- after the start, and before the end of an intra-analysis.
    | PostBranch
    deriving (Ord, Eq, Show, Generic)

instance NFData LoggingEvent
instance ToJSON LoggingEvent

logEvent :: MonadIO m => LoggingEvent -> m ()
logEvent = const $ return () -- liftIO . ByteString.putStrLn . JSON.encode

------------------------------------------------------------
-- Fixpoints
------------------------------------------------------------

-- | Constraints that need to be satisfied when executing the analysis
type AnalysisM ix m = (MonadIO m, SCV.FormulaSolver Domain.SymVar m, ToIndex ix, Show ix)

-- | Intra-turn fixpoint: analyze a single turn of an actor, and returns a set of successor turns.
--
-- This is the only place where the semantics from 'Analysis.SimpleActor.Semantics' is actually called.
intraTurn :: forall m ix . AnalysisM ix m => Beh -> ActorRef -> State -> StateT InterTurnState (AnalysisSystemT ix m) (Set Turn)
intraTurn beh selfRef st = do
        -- Debug.traceIO $ T.pack $ "transfer turn " ++ show selfRef
        -- compute a fixpoint over the function calls within this turn
        lfp intra key'
        -- The set of successor turns will have been cached at the entry component
        result <- (Set.fromList . map (uncurry Turn . first cntEither)) . maybe [] Set.toList <$> MapM.get key'
        -- We must setup the 'in' store for the next turn based on the output store of the current one.
        outputSto <- getStoreOut $ toIndex @ix key'
        mapM_ (flip joinStoreIn outputSto . toIndex @ix) (foldMap (maybeToList . turnKey selfRef) (Set.toList result))
        return result
    where key'  = entryKey selfRef beh st
          intra :: ProcKey -> StateT InterTurnState (AnalysisSystemT ix m) ()
          intra k = do 
                  logEvent (IntraStarted (spanOfCmp k))
                  start <- liftIO (getTime Monotonic)
                  result <- runAroundFixT @(AnalysisT ix m) around Semantics.eval k
                          & mapStateT (
                                runIntraAnalysis k 
                              . fmap fst 
                              . runSchemeStoreT @Exp @K @ActorVlu Store.emptySchemeStore 
                              . evalWithAbstractCountT
                              . runStoreHookT)
                  end <- result `deepseq` liftIO (getTime Monotonic)
                  let diff = toNanoSecs $ diffTimeSpec start end
                  logEvent (IntraEnded (spanOfCmp k) diff)
                  return result
          {-# SCC intra #-}
          -- Instrumentation around every recursive component call: snapshot the
          -- caller's store into the callee's input store, analyze, then adopt the
          -- callee's output store. Overwriting on the way out is sound because
          -- the analysis is monotonically increasing.
          around recur cmp = do
              idx :: ix <- toIndex @ix <$> upperM (key cmp)
              upperM . joinStoreIn idx =<< upperM getMultiStore
              upperM . joinCountIn idx =<< upperM getCounts
              v <- recur cmp
              upperM (putMultiStore =<< getStoreOutShortciruit idx)
              upperM (putCounts =<< getCountOut idx)
              return v

-- | Inter-turn fixpoint: analyze a sequence of turns of an actor 
-- until its state (i.e., inbox and outbox) no longer changes.
transferTurn :: AnalysisM ix m
             => ActorRef
             -> Turn
             -> StateT InterTurnState (AnalysisSystemT ix m) (Set Turn)
transferTurn selfRef (Turn (Become beh) turnState) = intraTurn beh selfRef turnState
-- Terminated actors do not generate successor turns, so we return the empty set.
transferTurn _ (Turn Terminated _) = return Set.empty

fixTurn :: AnalysisM ix m => ActorRef -> Turn -> AnalysisSystemT ix m (Set Turn)
fixTurn selfRef turn0 = do
    -- Run the inter-turn fixpoint, accumulating the actor's outgoing mail in the
    -- 'InterTurnState' as turns are analyzed.
    (result, interTurn) <- runStateT (Fix.lfp (Fix.lift $ transferTurn selfRef) (Set.singleton turn0))
                                      (InterTurnState Lattice.bottom)
    -- Fold the accumulated outbox into the global mailboxes.
    mbs %= Lattice.join (interTurn ^. outbox)
    return result

-- | Inter-system fixpoint, analyze a system of actors until the global state (i.e., the mailboxes) no longer changes.
transferSystem :: forall ix m . AnalysisM ix m => System -> AnalysisGlobalT ix m System
transferSystem s = do
    -- Debug.traceShowIO $ "transferSystem actors=" ++ show (Map.size (s ^. initialBeh)) ++ " total-turns=" ++ show (sum (map Set.size (Map.elems (s ^. turns))))
    -- Debug.traceShowIO $ "transferSystem mailboxes= " ++ show (s ^. mbs)
    flip execStateT s $ do
        let turnState ref = State (fromMaybe Lattice.bottom (Map.lookup ref (s ^. mbs)))
        -- Seed each actor's input store, at its entry index, with the store
        -- recorded at its spawn point. The entry index is only known here because
        -- it depends on the actor's mailbox state for this iteration.
        spawned <- _spawnStore <$> getAnalysisState
        void $ flip Map.traverseWithKey (s ^. initialBeh) $ \ref beh ->
            joinStoreIn (toIndex @ix (entryKey ref beh (turnState ref)))
                        (fromMaybe Lattice.bottom (Map.lookup ref spawned))
        let initialTurns  = Map.mapWithKey (\ref beh -> Set.singleton (Turn (Become beh) (turnState ref)))
                                             -- (Map.restrictKeys (s ^. initialBeh) changed)
                                             (s ^. initialBeh)
        newTurns <- Map.traverseWithKey (Fix.lift . fixTurn) initialTurns
        -- join the new turns with the old turns
        turns %= Lattice.join newTurns

fixSystem :: AnalysisM ix m => System -> AnalysisGlobalT ix m System
fixSystem = Fix.lfp (transferSystem >=> traceSystem)

------------------------------------------------------------
-- Running the analysis
------------------------------------------------------------

-- | The initial store contains all the primitive functions whhich are stored 
-- at a fixed address.
mainStore :: ActorSto
mainStore = Store.emptySchemeStore 
          & Store.varStore .~ withTop (Semantics.initialSto Semantics.allPrimitives Store.PrrAdr)
          & Store.paiStore .~ withTop Map.empty
          & Store.vecStore .~ Map.empty
          & Store.strStore .~ withTop Map.empty
    where  withTop :: (Ord adr, Lattice.TopLattice adr, Lattice.TopLattice vlu) => Map adr vlu -> Map adr vlu
           withTop = Map.insert Lattice.top Lattice.top

-- | The environment is populated with the initial bindings for the primitive functions.
mainEnv :: ActorEnv
mainEnv =
    HashMap.fromList $ map (second Store.PrrAdr . dupe) Semantics.allPrimitives

-- | Create an initial system from the expression of the main behavior. 
initialSystem :: Exp -> System
initialSystem mainExp = System {
        _mbs = Map.fromList [(EntryPid, Lattice.bottom)],
        _turns = Map.empty,
        _initialBeh = Map.singleton EntryPid (mainExp, mainEnv, initialDynEnvironment)
    }

-- | Create an empty analysis state
emptyAnalysisState :: AnalysisState ix
emptyAnalysisState = AnalysisState {
        _trace = [],
        _blames = Set.empty,
        _storeIn = Map.empty,
        _storeOut = Map.empty,
        -- The initial actor's spawn-point store is the primitive-seeded store.
        _spawnStore = Map.singleton EntryPid mainStore
    }

-- | Top-level function to analyze an actor system.
analyze :: forall ix m . AnalysisM ix m => Exp -> m (System, AnalysisState ix, Map ix ActorSto)
analyze mainExpr = do
    ((((system, analysisState), _), stores), _) <- 
            fixSystem initSystem
          & flip runStateT initState
          & runWithComponentTracking
          & runMapT @(In ix ActorSto) @ActorSto Map.empty
          & runMapT @(Out ix ActorSto) @ActorSto Map.empty
          & runWithMapping' @(In ix SymCou) @SymCou 
          & runWithMapping' @(Out ix SymCou) @SymCou 
          & runMapT @ProcKey @ProcVal Map.empty
          & runWithDependencyTracking @ProcKey @ProcKey
          & runWithDependencyTracking @ProcKey @(Store.VarAdr K)
          & runWithDependencyTracking @ProcKey @(Store.PaiAdr Exp K)
          & runWithDependencyTracking @ProcKey @(Store.VecAdr Exp K)
          & runWithDependencyTracking @ProcKey @(Store.StrAdr Exp K)
          & runWithDependencyTracking @ProcKey @(In ix ActorSto)
          & runWithDependencyTracking @ProcKey @(Out ix ActorSto)
          & runWithDependencyTracking @ProcKey @(In ix SymCou)
          & runWithDependencyTracking @ProcKey @(Out ix SymCou)
          & runWithWorkList
    return (system, analysisState, Map.mapKeys (\(MapM.Out ix') -> ix') stores)
    where initSystem = initialSystem mainExpr
          initState  = emptyAnalysisState

type Idx = ActorIdx

-- | Top-level function to analyze an actor system within the IO monad
analyzeIO :: Exp -> IO (System, AnalysisState Idx, Map Idx ActorSto)
analyzeIO mainExpr = analyze mainExpr
                   & runCachedSolver
                   & runZ3SolverWithPrelude
