{-# LANGUAGE FlexibleContexts, UndecidableInstances, FlexibleInstances, ConstraintKinds #-}
module Analysis.Actors.Monad(
   -- 
   ActorEvalM, 
   ActorBehaviorM(..), 
   ActorLocalM(..), 
   ActorGlobalM(..), 
   ActorM,
   runActorT, 
   module Analysis.Scheme.Monad, 
   (!), 
   runActorSystemT, 
   runActorSystemT', 
   receive, 
   runNoSpawnT,
   NoSpawnT,
   runNoSendT, 
   emptyActorSystem
) where

import Syntax.Scheme.AST
-- use the monads from the base-semantics
import Analysis.Scheme.Monad
import Analysis.Actors.Mailbox
import qualified Analysis.Actors.Mailbox as MB
import Domain.Scheme hiding (Exp)
import Lattice

import Control.Monad.Layer
import Control.Monad.Join
import Control.Monad.State.SVar (SVar)
import qualified Control.Monad.State.SVar as SVar

import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State (MonadState, StateT(..), gets, put, runStateT, modify)
import Control.Monad.Reader (MonadReader, ReaderT(..), ask, runReaderT)
import Control.Monad (void)
import Control.Monad.Identity (IdentityT, runIdentityT)
import Control.Monad.Trans
import Data.Maybe (fromMaybe)
import Analysis.Monad.IntraAnalysis
import Control.Monad.Cond (ifM)
import Analysis.Monad.DependencyTracking (DependencyTrackingM, trigger, register)
import Analysis.Monad.WorkList (WorkListM)
import Lattice.Class (BottomLattice)
import Domain.Scheme.Actors.Message

type ActorEvalM m v msg mb = 
   (SchemeM m v, 
    ActorDomain v, 
    ActorM m (ARef v) msg mb, 
    ActorBehaviorM m v,
    -- message representations
    MessageDomain msg, 
    Payload msg ~ v, 
    Tag msg ~ CP String)

class ActorBehaviorM m v | m -> v where
   -- | Spawn a new actor with the given behavior, returns an actor reference
   -- corresponding to the newly spawned actor
   spawn  :: BottomLattice v => Exp -> m v
   -- | Change the behavior of the actor to the given new behavior
   become :: Exp -> m ()

instance {-# OVERLAPPABLE #-} (Monad m, MonadLayer t, ActorBehaviorM m v) => ActorBehaviorM (t m) v where
   spawn  = upperM . spawn
   become = upperM . become

type ActorM m ref msg mb = (ActorLocalM m ref msg mb, ActorGlobalM m ref msg mb)

-- | Monadic context for actors, includes a mailbox 
-- and a reference to the current process identifier
class ActorLocalM m ref msg mb | m -> ref msg mb where
   -- | Retrieve a reference to the current actor pid
   self    :: m ref
   -- | Receive a message from the mailbox
   receiveAll :: m [(msg, mb)]
   -- | Put new mailbox
   putMailbox :: mb -> m ()

receive :: (MonadJoin m, ActorLocalM m ref msg mb, Joinable a, BottomLattice a) => (msg -> m a) -> m a
receive f = do
   msgs <- receiveAll
   mjoins (map (\(msg, mb') -> putMailbox mb' >> f msg) msgs)

class ActorGlobalM m ref msg mb | m -> ref msg mb where
   -- | Send a message to the given actor
   -- the return value indicates whether the mailbox 
   -- has been changed or not
   send         :: ref -> msg -> m Bool
   -- | Read the entire mailbox from the global mailbox state
   getMailbox   :: ref -> m mb

(!) :: (Functor m, ActorGlobalM m ref msg mb) => ref -> msg -> m ()
(!) ref = void . send ref


infixl 0 !

instance {-# OVERLAPPABLE #-} (MonadLayer t, Monad m, ActorGlobalM m ref msg mb) => ActorGlobalM (t m) ref msg mb where
   send ref = upperM . send ref
   getMailbox = upperM . getMailbox

------------------------------------------------------------
-- Instances
------------------------------------------------------------

-- | The state of an actor that consists of its mailbox and a 
-- reference to itself.
newtype ActorState mb = ActorState { mailbox :: mb }
                       deriving (Eq, Ord, Joinable, PartialOrder, BottomLattice)

-- | Basic implementation of the ActorM monad
-- It keeps track of the mailbox and a self-reference using the state monad
newtype ActorT mb ref msg m a =
      ActorT { _runActorT' :: StateT (ActorState mb) (ReaderT ref m) a }
   deriving (Functor, Applicative, Monad, MonadReader ref, MonadState (ActorState mb))


instance MonadTrans (ActorT mb ref msg) where   
   lift = ActorT . lift . lift

instance MonadLayer (ActorT mb ref msg) where
   lowerM f (ActorT m) = ActorT $ lowerM (lowerM f) m 


instance (Joinable mb, BottomLattice mb, MonadJoin m) => MonadJoin (ActorT mb ref msg m) where
   mzero = ActorT mzero
   mjoin (ActorT ma) (ActorT mb) = ActorT $ mjoin ma mb

instance {-# OVERLAPPING #-} (MonadJoin m, Mailbox mb msg) => ActorLocalM (ActorT mb ref msg m) ref msg mb where
   self    = ask
   receiveAll = gets (Set.toList . dequeue . mailbox)
   putMailbox = put . ActorState

instance (MonadLayer t, ActorLocalM m ref msg mb, Monad m) => ActorLocalM (t m) ref msg mb where
   self = upperM self
   receiveAll = upperM receiveAll
   putMailbox = upperM . putMailbox

-- | Add he ActorT monad to the stack to provide a mailbox and a reference to "self"
-- in the monadic context
runActorT :: forall mb m ref msg a . mb -> ref -> ActorT mb ref msg m a -> m (a, ActorState mb)
runActorT initialMailbox selfRef (ActorT ma) = runReaderT (runStateT ma (ActorState initialMailbox)) selfRef

------------------------------------------------------------
-- ActorSystemT
------------------------------------------------------------

-- TODO: deprecate this
type ActorSystemState ref mb = Map ref (SVar mb)

emptyActorSystem :: forall mb ref . ActorSystemState ref mb 
emptyActorSystem = Map.empty

-- | An actor system keeps track of the mailboxes
-- of each actor in the system. For this is keeps
-- a mapping from actor references to mailboxes
-- 
-- This mapping is implemented using SVars such 
-- that dependencies between components of the 
-- analysis can be automatically tracked and 
-- an effect-driven worklist algorithm can be used.
newtype ActorSystemT ref msg mb m a = ActorSystemT (StateT (ActorSystemState ref mb) m a)
                         deriving (Functor, Applicative, Monad, MonadLayer, MonadTrans, MonadState (ActorSystemState ref mb))

instance {-# OVERLAPPING #-} (Ord ref, Mailbox mb msg, SVar.MonadStateVar m) => ActorGlobalM (ActorSystemT ref msg mb m) ref msg mb where
   send ref msg = do
      mb <- gets (Map.lookup ref) >>= maybe (upperM $ SVar.new MB.empty) pure
      modify (Map.insert ref mb)
      -- TODO: we check whether the mailbox abstraction has changed in order
      -- to signal a change to the SVar layer or not. This is currently not very 
      -- efficient since it uses Eq's equality. Depending on the abstraction used a more efficient version
      -- of this could be provided.
      void $ upperM $ SVar.modify (\mb -> let mb' = enqueue msg mb in if mb == mb' then Nothing else Just mb') mb
      return True
   getMailbox = undefined 

runActorSystemT :: ActorSystemState ref mb -> ActorSystemT ref msg mb m a -> m (a, Map ref (SVar mb))
runActorSystemT initial (ActorSystemT m) = runStateT m initial

------------------------------------------------------------
-- ActorSystemT' (without SVar)
------------------------------------------------------------

type ActorSystemState' ref mb = Map ref mb

newtype ActorSystemT' ref msg mb m a = ActorSystemT' (StateT (ActorSystemState' ref mb) m a)
                                    deriving (Functor, Applicative, Monad, MonadLayer, MonadTrans, MonadState (ActorSystemState' ref mb))

instance {-# OVERLAPPING #-} (Ord ref, Monad m, Mailbox mb msg) => ActorGlobalM (ActorSystemT' ref msg mb m) ref msg mb where 
   send ref msg = do
      mb <- gets (fromMaybe MB.empty . Map.lookup ref)
      let mb' = enqueue msg mb
      modify (Map.insert ref mb')
      return (mb /= mb')
   getMailbox ref = gets (fromMaybe MB.empty . Map.lookup ref) 

runActorSystemT' :: ActorSystemState' ref mb -> ActorSystemT' ref msg mb m a -> m (a, Map ref mb)
runActorSystemT' initial (ActorSystemT' m) = runStateT m initial

------------------------------------------------------------
-- Effect registration through ActorSystemT' 
------------------------------------------------------------


instance (DependencyTrackingM m cmp ref, WorkListM m cmp, ActorGlobalM m ref msg mb) => ActorGlobalM (IntraAnalysisT cmp m) ref msg mb where
   send ref msg = 
      ifM (upperM $ send ref msg)
          (upperM $ trigger ref >> return True)
          (return False)
   getMailbox ref = 
      currentCmp >>= (upperM . register ref) >> upperM (getMailbox ref)

------------------------------------------------------------
-- No-op implementations of the monads
------------------------------------------------------------

-- | Instance of ActorBehaviorM that ignores becomes and actor spawns
newtype NoSpawnT v m a = NoSpawnT (IdentityT m a)
                       deriving (Monad, Applicative, Functor, MonadLayer, MonadTrans, MonadJoin)

instance (Monad m) => ActorBehaviorM (NoSpawnT v m) v where
   spawn  = return . const bottom
   become = const $ return ()

-- | Add the `NoSpawnT` to the monadic stack, introduces an instances of `ActorBehaviorM`
runNoSpawnT :: NoSpawnT v m a -> m a
runNoSpawnT (NoSpawnT m) = runIdentityT m

----------------------------------------

-- | Instance of ActorGlobalM that ignores "send"
newtype NoSendT ref msg mb m a = NoSendT (IdentityT m a) 
                           deriving (Monad, Applicative, Functor, MonadLayer, MonadTrans, MonadJoin)

instance (Monad m, Mailbox mb msg) => ActorGlobalM (NoSendT ref msg mb m) ref msg mb where 
   send = const . const (return False)
   getMailbox = const $ return MB.empty

runNoSendT :: NoSendT ref msg mb m a -> m a 
runNoSendT (NoSendT m) = runIdentityT m
