{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Analysis.Monad.Fix (lfp, MonadFixpoint (..), FixT (..), runFixT) where

import Analysis.Monad.Cache
import Analysis.Monad.ComponentTracking (ComponentTrackingM, call)
import Analysis.Monad.Map (MapM (..))
import Control.Monad.Escape (MonadEscape (..))
import Control.Monad.Identity (IdentityT (IdentityT))
import Control.Monad.Join (MonadJoin)
import Control.Monad.Layer (MonadLayer, MonadTrans)
import Lattice.Class
import Analysis.Monad.WorkList (WorkListM(..), iterateWL)

-- | Type of Kleisli arrows, distinct from `Kleisli` newtype from
-- the standard library.
type Kleisli m a b = a -> m b

-- | Fixpoint operator over Kleisli arrows
class MonadFixpoint m b c | m -> b c where
  fix :: (Kleisli m b c -> Kleisli m b c) -> Kleisli m b c

------------------------------------------------------------
-- ModX fixpoint
------------------------------------------------------------

-- |  ModX fixpoint monad transformer
newtype FixT b c m a = FixT {runFixT' :: IdentityT m a}
  deriving (MonadCache, MonadTrans, MonadLayer, Monad, Applicative, Functor, MonadJoin, MonadEscape)

runFixT :: forall m b c. (MonadCache m, MapM (Key m b) (Val m c) (Base m)) => (b -> FixT b c m c) -> Key m b -> Base m ()
runFixT f kb = cache @(FixT b c m) @b kb f

instance
  ( MonadJoin m,
    MonadCache m,
    ComponentTrackingM m (Key m b),
    MapM (Key m b) (Val m c) m,
    Eq c,
    BottomLattice c,
    Joinable c
  ) =>
  MonadFixpoint (FixT b c m) b c
  where
  fix f = f call

-- | Compute the least fixed point for the given (initial) component
lfp :: (WorkListM m a) => (a -> m b) -> a ->  m ()
lfp f initialCmp = add initialCmp >> iterateWL f
