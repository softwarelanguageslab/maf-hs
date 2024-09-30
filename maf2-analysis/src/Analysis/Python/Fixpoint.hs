{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Analysis.Python.Fixpoint where

import Lattice
import Analysis.Python.Common
import Domain.Python.Objects as PyObj  
import Analysis.Python.Semantics hiding (call)
import Analysis.Python.Monad
import Analysis.Python.Objects
import Analysis.Monad hiding (eval, call)
import Analysis.Monad.ComponentTracking hiding (has)

import Domain.Python.Syntax

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import Prelude hiding (init, read)
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Escape
import Data.Function ((&))
import Analysis.Python.Escape
import Analysis.Monad.Stack
---
--- Python analysis fixpoint algorithm
---

type IntraT m = MonadStack '[
                    MayEscapeT (Set (PyEsc PyRef)),
                    AllocT PyLoc () ObjAdr,
                    EnvT PyEnv,
                    CtxT (),
                    JoinT,
                    CacheT
                ] m 

type AnalysisM m obj = (PyDomain obj PyRef, 
                        StoreM m ObjAdr obj,
                        MapM PyCmp PyRes m,
                        ComponentTrackingM m  PyCmp,
                        DependencyTrackingM m PyCmp ObjAdr,
                        DependencyTrackingM m PyCmp PyCmp,
                        WorkListM m PyCmp)

type PyCmp = Key (IntraT Identity) PyBdy
type PyRes = Val (IntraT Identity) PyRef  

intra :: forall m obj . AnalysisM m obj => PyCmp -> m ()
intra cmp = cache @(IntraT (IntraAnalysisT PyCmp m)) cmp evalBdy
                & runAlloc (const . allocPtr)
                & runIntraAnalysis cmp 
                

inter :: forall m obj . AnalysisM m obj => PyPrg -> m () 
inter prg = do init                                 -- initialize Python infrastructure
               add ((Main prg, initialEnv), ())     -- add the main component to the worklist
               iterateWL intra                      -- start the analysis 

analyze :: forall obj . PyDomain obj PyRef => PyPrg -> (Map PyCmp PyRes, Map ObjAdr obj)
analyze prg = (rsto, osto)
    where ((_,osto),rsto) = inter prg
                                & runWithStore @(Map ObjAdr obj) @ObjAdr
                                & runWithMapping @PyCmp
                                & runWithDependencyTracking @PyCmp @ObjAdr
                                & runWithDependencyTracking @PyCmp @PyCmp
                                & runWithComponentTracking @PyCmp
                                & runWithWorkList @(Set PyCmp)
                                & runIdentity

analyzeREPL :: forall obj . PyDomain obj PyRef
    => IO PyPrg         -- a read function
    -> (obj -> IO ())   -- a display function
    -> IO ()
analyzeREPL read display = 
    void $ (init >> repl) 
            & runWithStore @(Map ObjAdr obj) @ObjAdr
            & runWithMapping @PyCmp
            & runWithDependencyTracking @PyCmp @ObjAdr
            & runWithDependencyTracking @PyCmp @PyCmp
            & runWithComponentTracking @PyCmp
            & runWithWorkList @(Set PyCmp)
    where repl = forever $ do prg <- addImplicitReturn <$> liftIO read
                              let cmp = ((Main prg, initialEnv), ())
                              add cmp 
                              iterateWL intra 
                              res <- justOrBot <$> Analysis.Monad.get cmp 
                              traverse (mapM lookupAdr . Set.toList . addrs >=> liftIO . display . joins) res

---
--- CP instantiation
---

type PyDomainCP = PyObjCP PyRef ObjAdr PyClo

analyzeCP :: PyPrg -> (Map PyCmp (MayEscape (Set (PyEsc PyRef)) PyRef), Map ObjAdr PyDomainCP)
analyzeCP = analyze @PyDomainCP
