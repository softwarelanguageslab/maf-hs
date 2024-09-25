{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Analysis.Monad.IntraAnalysis (
    IntraAnalysisT,
    runIntraAnalysis,
    currentCmp
) where

import Analysis.Monad.ComponentTracking
import Analysis.Monad.WorkList
import Control.Monad.Reader
import Control.Monad.Layer
import Control.Monad.Join
import Analysis.Monad.DependencyTracking
import Analysis.Monad.Store
import Control.Monad.Cond
import Analysis.Monad.Map
import Debug.Trace

---
--- IntraAnalysis monad layer 
---

newtype IntraAnalysisT cmp m a = IntraAnalysisT (ReaderT cmp m a)
    deriving (Functor, Applicative, Monad, MonadReader cmp, MonadTrans, MonadLayer, MonadTransControl, MonadJoin)

instance {-# OVERLAPPING #-} (ComponentTrackingM m cmp, WorkListM m cmp, Ord cmp) => ComponentTrackingM (IntraAnalysisT cmp m) cmp where
    spawn cmp = unlessM (upperM $ has cmp)
                        (upperM $ spawn cmp >> add cmp)
    components = upperM components

instance {-# OVERLAPPING #-} (StoreM m a v, DependencyTrackingM m cmp a, WorkListM m cmp)
        => StoreM (IntraAnalysisT cmp m) a v where
    lookupAdr a = currentCmp >>= upperM . register a >> upperM (lookupAdr a)
    writeAdr a v = whenM (upperM $ writeAdr' a v) (upperM $ trigger a)
    updateAdr a v = whenM (upperM $ updateAdr' a v) (upperM $ trigger a)
    updateWith fs fw a = whenM (upperM $ updateWith' fs fw a) (upperM $ trigger a)

instance {-# OVERLAPPING #-} (MapM k v m, Eq v, DependencyTrackingM m cmp k, WorkListM m cmp)
    => MapM k v (IntraAnalysisT cmp m) where
        get k = currentCmp >>= upperM . register k >> upperM (get k)
        put k v = whenM (upperM $ put' k v) (upperM $ trigger k)

-- | Convenience function for retrieving the component current being analyzed
currentCmp :: Monad m => IntraAnalysisT cmp m cmp
currentCmp = ask 

runIntraAnalysis :: cmp -> IntraAnalysisT cmp m a -> m a
runIntraAnalysis cmp (IntraAnalysisT f) = runReaderT f cmp
