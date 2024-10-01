{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Analysis.Lambda where

import Control.Monad.Join
import Analysis.Monad hiding (eval)
import Syntax.Lambda
import Domain.Lambda

import qualified Control.Monad.State.SVar as SVar

import Data.Map (Map)
import Data.Function ((&))
import Data.Functor.Identity
import Prelude hiding (iterate)
import Analysis.Monad.Fix (MonadFixpoint (fix), runFixT, lfp)
import Analysis.Monad.Stack (MonadStack)
import Analysis.Monad.ComponentTracking (ComponentTrackingM, runWithComponentTracking)
import qualified Data.Map as Map
import Control.Monad.Escape (MonadEscape (..), MayEscapeT)
import Domain.Class (Domain)
import Control.Monad.DomainError (DomainError)
import Data.Set (Set)


newtype Adr = Adr String
            deriving (Eq, Show, Ord)

type LamM m = (Monad m,
               MonadJoin m,
               StoreM m Adr V,
               MonadFixpoint m Exp V,
               MonadEscape m,
               Domain (Esc m) DomainError,
               EnvM m Adr (Env Adr))

type V = LamVal Adr
type Sto = Map Adr (SVar.SVar V)

eval :: LamM m => Exp -> m V 
eval = fix eval'

eval' :: LamM m => (Exp -> m V) -> Exp -> m V
eval' _ lam@(Lam _ _) =
   clo . (,lam) <$> getEnv
eval' _ (Num n)     = return (num n)
eval' recur (Nonzero n) = do
   v <- eval' recur  n
   cond       (nonzero v)
   {- then -} (return (num 1))
   {- else -} (return (num 0))
eval' recur (Inc e)     =
   inc =<< eval' recur e
eval' recur (If e1 e2 e3) =
   cond (nonzero =<< eval' recur e1) 
        (eval' recur e2) 
        (eval' recur e3)
eval' recur (App e1 e2) = do
   v1 <- eval' recur e1
   v2 <- eval' recur e2
   clos (flip (apply recur) v2) v1
eval' _ (Var (Ident nam _)) =
   lookupEnv nam >>= lookupAdr

apply :: LamM m => (Exp -> m V) -> Clo Adr -> V -> m V
apply recur (env, Lam (Ident nam _) exp') v =
   withEnv (const env) $
      withExtendedEnv [(nam, Adr nam)] $ do
         writeAdr (Adr nam) v
         recur exp'
apply _ _ _ = error "not a valid closure"


------------------------------------------------------------
-- Analysis instantiation
------------------------------------------------------------

type Cmp = Key (IntraT Identity) Exp
type Res = Val (IntraT Identity) V
type IntraT m = MonadStack '[
      MayEscapeT (Set DomainError),
      EnvT (Map String Adr),
      JoinT,
      CacheT
   ] m

type InterM m = 
   (MapM Cmp Res m,
    WorkListM m Cmp, 
    DependencyTrackingM m Cmp Cmp, 
    DependencyTrackingM m Cmp Adr,
    ComponentTrackingM m Cmp,
    StoreM m Adr V)

intra :: forall m . InterM m => Cmp -> m ()
intra cmp = runFixT @(IntraT (IntraAnalysisT Cmp m)) eval cmp
          & runIntraAnalysis cmp

inter :: forall m . InterM m => Exp -> m ()
inter e = lfp intra (e, Map.empty)

analyze :: Exp -> (Map Adr V, Map Cmp Res)
analyze prg = let (((), sto), res) = inter prg
                     & runWithStore @(Map Adr V)
                     & runWithMapping @Cmp @Res
                     & runWithComponentTracking
                     & runWithDependencyTracking @Cmp @Cmp
                     & runWithDependencyTracking @Cmp @Adr
                     & runWithWorkList @[_]
                     & runIdentity
              in (sto, res)
