{-# LANGUAGE UndecidableInstances, FlexibleInstances, ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Analysis.Scheme where

import Prelude hiding (iterate, exp, lookup)

import Analysis.Scheme.Primitives hiding (run)

import Syntax.Scheme
import Domain.Scheme hiding (Exp, Env)
import Domain.Scheme.Store hiding (Env)
import qualified Domain.Scheme as S

import Data.Set (Set)
import Data.Map (Map)
import Data.Functor.Identity
import Data.Function ((&))
import Control.Monad.DomainError (DomainError)
import Control.Monad.Escape
import Lattice.Equal (EqualLattice)
import Analysis.Monad.Stack (MonadStack)
import Analysis.Monad.Cache
import Analysis.Monad.IntraAnalysis
import Analysis.Scheme.Semantics (eval)
import Data.Kind (Constraint)
import Analysis.Monad.DependencyTracking
import Analysis.Monad.ComponentTracking
import Analysis.Monad.Allocation
import Syntax (Ide)
import Analysis.Monad.Fix (runFixT, lfp)
import Analysis.Monad hiding (eval)
import qualified  Analysis.Scheme.Primitives as P
import Debug.Trace (traceShow)
import Analysis.Monad hiding (eval)

-----------------------------------------
-- Shorthands
-----------------------------------------

type Program  = Exp
type Cmp      = Key (IntraT Identity) Exp
type Res   v  = Val (IntraT Identity) v
type K        = [Exp]
type Env      = Map String (EnvAdr K)
type Sto   v  = Map (EnvAdr K) v
type PaiSto v = Map (PaiAdr K) (PaiDom v)
type VecSto v = Map (VecAdr K) (VecDom v)
type StrSto v = Map (StrAdr K) (StrDom v)
type AnlRes v = (Sto v, PaiSto v, StrSto v, VecSto v, Map Cmp (Res v))


-----------------------------------------
-- Store & Environment
-----------------------------------------

-- | The initial environment used by 
-- the analysis
analysisEnv :: Env
analysisEnv = initialEnv PrmAdr

-- | The initial store
-- analysisStore :: forall v ctx . (Ord ctx, SchemeDomain v) => Env ctx -> DSto ctx v
-- analysisStore = fromValues . initialSto @v

-----------------------------------------
-- Analysis
-----------------------------------------

type IntraT m = MonadStack '[
                  MayEscapeT (Set DomainError),
                  AllocT Ide K (EnvAdr K),
                  AllocT Exp K (PaiAdr K),
                  AllocT Exp K (StrAdr K),
                  AllocT Exp K (VecAdr K),
                  EnvT Env, 
                  CtxT K,
                  JoinT,
                  CacheT
               ] m


type InterAnalysisM v m =
   (WorkListM m Cmp,
    StoreM m (PaiAdr K) (PaiDom v),
    StoreM m (VecAdr K) (VecDom v),
    StoreM m (StrAdr K) (StrDom v),
    StoreM m (EnvAdr K) v,
    ComponentTrackingM m Cmp,
    DependencyTrackingM m Cmp (PaiAdr K),
    DependencyTrackingM m Cmp (VecAdr K),
    DependencyTrackingM m Cmp (StrAdr K),
    DependencyTrackingM m Cmp (EnvAdr K),
    DependencyTrackingM m Cmp Cmp,
    MapM Cmp (Res v) m,
    SchemeDomain v) :: Constraint

----------------------------------------
-- Analysis
-----------------------------------------

intra :: forall m v var . (InstSchemeDomain var v, InterAnalysisM v m) => Cmp -> m ()
intra cmp =  runFixT @(IntraT (IntraAnalysisT Cmp m)) eval cmp
           & runAlloc EnvAdr
           & runAlloc PaiAdr
           & runAlloc StrAdr
           & runAlloc VecAdr
           & runIntraAnalysis cmp

inter :: forall v m .  (InstSchemeDomain (EnvAdr K) v, InterAnalysisM v m) => Exp -> m ()
inter exp = lfp intra ((exp, analysisEnv), [])

analyze :: forall v . (InstSchemeDomain (EnvAdr K) v) =>  Exp -> AnlRes v
analyze exp = let ((((((), varSto), paiSto), strSto), vecSto), resMap) =
                       inter @v exp
                     & runStoreT @(Sto v) @(EnvAdr K) @v (P.initialSto analysisEnv) 
                     & runWithStore @(Map (PaiAdrE Exp K) (PaiDom v))
                     & runWithStore @(Map (StrAdrE Exp K) (StrDom v))
                     & runWithStore @(Map (VecAdrE Exp K) (VecDom v))
                     & runWithMapping @Cmp @(Res v)
                     & runWithComponentTracking @Cmp
                     & runWithDependencyTracking @Cmp @Cmp
                     & runWithDependencyTracking @Cmp @(EnvAdr K)
                     & runWithDependencyTracking @Cmp @(PaiAdrE Exp K)
                     & runWithDependencyTracking @Cmp @(VecAdrE Exp K)
                     & runWithDependencyTracking @Cmp @(StrAdrE Exp K)
                     & runWithWorkList @(Set Cmp)
                     & runIdentity
              in (varSto, paiSto, strSto, vecSto, resMap)

-- | Scheme Domain constraints
type InstSchemeDomain var v = (
          Show v,
          SchemeValue v,
          Adr v ~ var,
          EnvAdr K ~ Adr v,
          PaiAdr K ~ PAdr v,
          StrAdr K ~ SAdr v,
          VecAdr K ~ VAdr v,
          SchemeConstraints v Exp var Env,
          EqualLattice v,
          S.Env v ~ Env,
          VarDom v ~ v,
          Ord K,
          Ord v)
