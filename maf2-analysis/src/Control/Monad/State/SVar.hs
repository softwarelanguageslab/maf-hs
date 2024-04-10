{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.State.SVar
  ( MonadStateVar (..),
    MonadStateVarTracking,
    VarState,
    TrackingStateVarT,
    runStateVarT,
    StateVarT,
    runTrackingStateVarT,
    wdep, rdep,
    Dep,
    RDep,
    WDep,
    reset,
    getDeps,
    SVar,
    unify,
    fromMap
  )
where

import Control.Monad.Layer
import Control.Monad.State (MonadState, MonadTrans (lift), StateT)
import qualified Control.Monad.State as ST
import Control.Monad.State.IntPool
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Unsafe.Coerce
import Prelude hiding (read)
import Data.Bifunctor (second)
import Control.Monad.Cond
import Lattice (Joinable(..), JoinLattice(..))
import Control.Monad (foldM)
import Data.Functor ((<&>))

-- Holds dynamic data
data SomeVal where
  SomeVal :: forall a. a -> SomeVal

unsafeCoerceVal :: SomeVal -> a
unsafeCoerceVal (SomeVal a) = unsafeCoerce a

newtype SVar a = SVar { getSVar :: Set Int } deriving (Eq, Ord, Show)

-- NOTE: SVars are actually NOT joinable, 
-- but they must be able to be used in MonadJoin.
-- TODO: Check whether we can encode at the type level 
-- if the JoinT monad is used at the bottom or not.
instance Joinable (SVar a) where
   join = error "cannot join svars"
instance JoinLattice (SVar a) where
   bottom = error "svar does not have bottom"

class (Monad m) => MonadStateVar m where
  -- | Create a new state variable
  -- with initial value a
  new :: a -> m (SVar a)

  -- |  Modify the given state variable,
  --  it requires a function that modifies
  --  the optionally modifies the SVar,
  --  returns true if the value was modified,
  --  false otherwise.
  modify :: (a -> Maybe a) -> SVar a -> m Bool

  -- |  Read the current value from the SVar
  read :: SVar a -> m a

  -- | Same as 'new' but also signals 
  -- a dependency on the SVar
  depend :: a -> m (SVar a)
  depend vlu = do
    var <- new vlu
    _ <- read var
    return var

instance (Monad (t m), MonadLayer t, MonadStateVar m) => MonadStateVar (t m) where
   new = upperM . new
   modify f = upperM . modify f
   read = upperM . read

------------------------------------------------------------
-- StateVarT
------------------------------------------------------------

-- | The state of the StateVarT monad.
-- It keeps track of the values of the SVar using a map
-- , mapping integers to dynamically typed values.
newtype VarState = VarState {state :: Map (Set Int) SomeVal}

-- |  Create an empty VarState
emptyVarState :: VarState
emptyVarState = VarState {state = Map.empty}

newtype StateVarT m a = StateVarT {runStateVarT' :: StateT VarState m a}
  deriving (Monad, Applicative, Functor, MonadState VarState, MonadLayer, MonadTrans)

runStateVarT :: StateVarT m a -> m (a, VarState)
runStateVarT (StateVarT ma) = ST.runStateT ma (VarState Map.empty)

instance {-# OVERLAPPING #-} (MonadIntegerPool m) => MonadStateVar (StateVarT m) where
  new vlu = do
    i <- lift fresh
    ST.modify (VarState . Map.insert (Set.singleton i) (SomeVal vlu) . state)
    return (SVar $ Set.singleton i)
  modify f (SVar i) = do
    -- SAFETY: unsafeCoerseVal is safe since each integer
    -- is only mapped once to an SVar of a specific type.
    -- Hence, the integer represents a type tag of sorts
    -- which uniquely associates the SVar with its type
    v <- ST.gets (fromJust . Map.lookup i . state)
    maybe (return False) (\v' -> ST.modify (VarState . Map.insert i (SomeVal v') . state) >> return True) (f (unsafeCoerceVal v))
  read (SVar i) =
    -- SAFETY: see above
    -- we can just apply fromJust since the SVar would not exists
    -- if it is not in the map
    ST.gets (unsafeCoerceVal . fromJust . Map.lookup i . state)

------------------------------------------------------------
-- Utilities
------------------------------------------------------------

-- | Unify a map of SVars to a map of the values stored at these SVars 
-- for a given VarState
unify :: (Ord a) => Map a (SVar b) -> VarState -> Map a b
unify m st =
   Map.fromList $ map
      (second $ unsafeCoerceVal . fromJust . flip Map.lookup (state st) . getSVar) (Map.toList m)

-- | Turn a map of values into a map of SVars that 
-- point to these values
fromMap :: (MonadStateVar m) => Ord a => Map a b -> m (Map a (SVar b))
fromMap  =
   foldM (\m (key, val) -> new val <&> flip (Map.insert key) m)
         Map.empty . Map.toList


------------------------------------------------------------
-- Tracking SVar's
------------------------------------------------------------

class Monad m => MonadStateVarTracking m where
   -- | Reset tracking
   reset   :: m ()
   -- | Get dependencies 
   getDeps :: m TrackingState

instance {-# OVERLAPPABLE #-} (MonadStateVarTracking m, MonadLayer t) => MonadStateVarTracking (t m) where
   reset    = upperM reset
   getDeps  = upperM getDeps

-- |  A dependency on an SVar
newtype Dep = Dep (Set Int) deriving (Ord, Eq, Show)

-- for convience

-- |  A read dependency
type RDep = Dep

-- | A write dependency
type WDep = Dep

-- | State for tracking of dependencies
data TrackingState = TrackingState {wdep :: Set WDep, rdep :: Set RDep} deriving (Show)

emptyTrackingState :: TrackingState
emptyTrackingState = TrackingState Set.empty Set.empty

trigger :: WDep -> TrackingState -> TrackingState
trigger dep st =
   st {wdep = Set.insert dep (wdep st)}

register :: WDep -> TrackingState -> TrackingState
register dep st =
   st {rdep = Set.insert dep (rdep st)}

-- | Monad that intercepts changes
-- to SVar and tracks their reads and writes
newtype TrackingStateVarT m a = TrackingStateVarT {runTrackingStateVarT' :: StateT TrackingState m a}
  deriving (Applicative, Monad, Functor, MonadState TrackingState, MonadLayer, MonadTrans)

instance (Monad m) => MonadStateVarTracking (TrackingStateVarT m) where
   reset   = ST.put emptyTrackingState
   getDeps = ST.get

runTrackingStateVarT :: (Monad m) => TrackingStateVarT m a -> m a
runTrackingStateVarT (TrackingStateVarT ma) = ST.evalStateT ma emptyTrackingState

instance {-# OVERLAPPING #-} (MonadStateVar m) => MonadStateVar (TrackingStateVarT m) where
  new = lift . new

  modify f var@(SVar i) =
    ifM
      (lift (modify f var))
      {- then -} (ST.modify (trigger (Dep i)) >> return True)
      {- else -} (return False)
  read var@(SVar i) =
    ST.modify (register (Dep i)) >> lift (read var)
