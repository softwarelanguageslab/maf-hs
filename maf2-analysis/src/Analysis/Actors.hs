{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
module Analysis.Actors where


import Analysis.Scheme.Primitives
import Analysis.Actors.Mailbox
import Analysis.Actors.Monad
import Analysis.Scheme.Actors.Components
import qualified Analysis.Actors.Semantics as Actors
import Analysis.Monad
import Analysis.Scheme.Store
import Analysis.Scheme hiding (runCallT, Component(..), Env, CallT, EvalT(..), State, runEvalT, RetAdr)
import qualified Control.Fixpoint.EffectDriven as EF
import Control.Monad (void, foldM)
import Control.Monad.DomainError
import Control.Monad.Join
import Control.Monad.Layer
import qualified Control.Monad.State.SVar as SVar
import Data.Function ((&))
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Domain.Scheme.Actors.Class
import Domain.Scheme.Store
import Syntax.Scheme.AST
import Data.Functor ((<&>))
import qualified Domain.Scheme.Actors.CP as CP
import Control.Monad.Trans.Class
import Domain (Address)

------------------------------------------------------------
-- Domain
------------------------------------------------------------

-- | Mailbox type
type MB = Set Msg

-- | Type of messages
type Msg = ()

------------------------------------------------------------
-- Shorthands
------------------------------------------------------------

type V = CP.CPActorValue (EnvAdr Ctx) (PaiAdr Ctx) (VecAdr Ctx) (StrAdr Ctx) Ctx
type Pid = CP.Pid Ctx

type Ctx = ()

------------------------------------------------------------
-- Semantics
------------------------------------------------------------

newtype EvalT m a = EvalT (m a) deriving (Applicative, Monad, Functor, MonadJoin)

instance MonadTrans EvalT where
   lift = EvalT

instance MonadLayer EvalT where
  upperM = EvalT
  lowerM f (EvalT m) = EvalT $ f m

instance (ActorEvalM (EvalT m) V Msg MB) => EvalM (EvalT m) V Exp where
  eval = Actors.eval

instance (Monad m, MonadEscape m, Esc m ~ Set DomainError) => MonadEscape (EvalT m) where
  type Esc (EvalT m) = Set DomainError
  throw = upperM . throw
  catch (EvalT m) hdl = EvalT $ catch @_ m (getInnerEvalT . hdl)
    where
      getInnerEvalT (EvalT m') = m'

runEvalT :: EvalT m a -> m a
runEvalT (EvalT m) = m

------------------------------------------------------------
-- ModX
------------------------------------------------------------

type State = (SSto Ctx V, Map (Component Ctx) (SVar.SVar V), Map Pid (SVar.SVar (Set Msg)))

------------------------------------------------------------
-- Analysis
------------------------------------------------------------

analyze :: Exp -> (DSto Ctx V, Map (Component Ctx) V)
analyze e = undefined -- let ((sto, retSto, _), state) = (EF.setup initialState >>= EF.iterate intra)
            --         & EF.runEffectT @[_] (Main e)
            --         & runIdentity
            -- in (unifyStore sto state, SVar.unify retSto state)
  -- where runIntra :: (EF.EffectSVarM m (Component Ctx), SVar.MonadStateVar m) => Component Ctx -> Pid -> Exp -> Env Ctx -> State -> m State
  --       runIntra cmp pid exp' env (sto, retSto, mailboxes) = do
  --            (m, mailboxes') <- mailbox pid mailboxes
  --            r <-   (Analysis.Monad.eval exp' >>= writeAdr cmp)
  --                   & runEvalT
  --                   & runMayEscape @(Set DomainError)
  --                   & runCallT @V @Ctx
  --                   & runSpawnT
  --                   & runEnv env
  --                   & runAlloc @CExp @_ @Ctx (const . const ())
  --                   & runAlloc @_ @Ctx PaiAdr
  --                   & runAlloc @_ @Ctx VecAdr
  --                   & runAlloc @_ @Ctx StrAdr
  --                   & runAlloc @_ @Ctx EnvAdr
  --                   & runActorT m pid
  --                   & runCtx ()
  --                   & runJoinT
  --                   & runSchemeStoreT sto
  --                   & runStoreT' retSto
  --                   & runActorSystemT mailboxes'
  --            let (((_, sto'), retSto'), mailboxes'') = r
  --            return (sto', retSto', mailboxes'')
  --       mailbox pid mailboxes = do
  --           var <- maybe (SVar.depend Set.empty) pure (Map.lookup pid mailboxes)
  --           mb  <- SVar.read var
  --           return (mb, Map.insert pid var mailboxes)
  --       intra cmp@(Main expr) = runIntra cmp CP.EntryPid expr analysisEnv
  --       intra cmp@(Actor pid expr ctx env) = runIntra cmp pid expr env
  --       initialState = do
  --           values <- foldM (\m (adr, v) -> SVar.new v <&> flip (Map.insert adr) m) Map.empty (Map.toList (initialSto @V analysisEnv))
  --           return (fromValues @SVar @V values, Map.empty, Map.empty)
  --       analysisEnv  = initialEnv PrmAdr
