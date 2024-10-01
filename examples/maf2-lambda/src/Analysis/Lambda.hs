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

------------------------------------------------------------
-- Semantics
------------------------------------------------------------

-- | Addresses are represented by their name
newtype Adr = Adr String
            deriving (Eq, Show, Ord)

-- | The set of constraints on the monadic 
-- context used in the analysis. 
--
-- This makes a number of functions available
-- for expressing the program semantics.
type LamM m = (Monad m,
               MonadJoin m,           -- approximating program branches
               StoreM m Adr V,        -- storing values of variables
               MonadFixpoint m Exp V, -- fixpoints
               MonadEscape m,         -- escape/exceptions
               Domain (Esc m) DomainError,
               EnvM m Adr (Env Adr) -- lexical environment
            )

-- | We fix the type of values here to `V` from @Domain.Lambda$.
--
-- In practice, however, you would make the abstract domain 
-- polymorphic and have type class constraints expressing
-- what operations the domain should support.
type V = LamVal Adr
type Sto = Map Adr (SVar.SVar V)

-- | The evaluation function
eval :: LamM m => Exp -> m V 
eval = fix eval' -- we use `fix` here so that 
                 -- the open recursion in `eval'` 
                 -- can use `recur` to schedule
                 -- the recursive call in the 
                 -- fixpoint algorithm
                 
-- | The real evaluation function,
-- it is mostly straightforward as 
-- it is implemented as a recursive 
-- interpreter.
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

-- | Function application semantics
apply :: LamM m => (Exp -> m V) -> Clo Adr -> V -> m V
apply recur (env, Lam (Ident nam _) exp') v =
   withEnv (const env) $
      withExtendedEnv [(nam, Adr nam)] $ do
         writeAdr (Adr nam) v
         -- the "magic" for having a terminating
         -- analysis occurs here. Instead of calling 
         -- `eval' recur` we call `recur` signaling
         -- to the framework that the expression `exp'`
         -- should be evaluated in a new fixpoint
         -- computation.
         recur exp'
apply _ _ _ = error "not a valid closure"


------------------------------------------------------------
-- Analysis instantiation
------------------------------------------------------------

-- | Shorthand for defining components in the analysis
type Cmp = Key (IntraT Identity) Exp
-- | Shorthand for defining results of the analysis
type Res = Val (IntraT Identity) V
-- | Monadic stack used in the intra analysis
type IntraT m = MonadStack '[
      MayEscapeT (Set DomainError),
      EnvT (Map String Adr),
      JoinT,
      CacheT
   ] m

-- | Constraints remaining for the inter-analysis
-- and which are global to the fixpoint computation
-- of the intra analysis.
type InterM m = 
   (MapM Cmp Res m,
    WorkListM m Cmp, 
    DependencyTrackingM m Cmp Cmp, 
    DependencyTrackingM m Cmp Adr,
    ComponentTrackingM m Cmp,
    StoreM m Adr V)

-- | The intra analysis uses `runFixT` which automatically
-- inserts the monad transformers defined in `IntraT` and 
-- instantiates the open recursion with an appropriate 
-- `recur` function.
--
-- `runIntraAnalysis` adds some additional bookkeeping 
-- for scheduling the intra-analysis of components 
-- in the fixed point computation.
intra :: forall m . InterM m => Cmp -> m ()
intra cmp = runFixT @(IntraT (IntraAnalysisT Cmp m)) eval cmp
          & runIntraAnalysis cmp

-- | The `inter` analysis simply computes the 
-- `intra` analysis for each component discovered
-- during the analysis until a fixpoint is reached.
inter :: forall m . InterM m => Exp -> m ()
inter e = lfp intra (e, Map.empty)

-- | This function calls the `inter` analysis 
-- and instantiates the remaining monad type class
-- constraints using additional monad transformers.
analyze :: Exp -> (Map Adr V, Map Cmp Res)
analyze prg = let (((), sto), res) = inter prg
                     & runWithStore @(Map Adr V)
                     & runWithMapping @Cmp @Res
                     -- | The tracking transformers
                     -- introduced here are needed 
                     -- for the effect-driven fixed 
                     -- point computation, if another 
                     -- fixed point computation would 
                     -- have been used, another set 
                     -- of bookeeping transforms 
                     -- would be here.
                     & runWithComponentTracking
                     & runWithDependencyTracking @Cmp @Cmp
                     & runWithDependencyTracking @Cmp @Adr
                     & runWithWorkList @[_]
                     & runIdentity
              in (sto, res)
