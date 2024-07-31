{-# LANGUAGE UndecidableInstances, AllowAmbiguousTypes #-}
module Analysis.Symbolic where

import Analysis.Scheme.Prelude

-- Syntax
import qualified Symbolic.AST as Symbolic

-- Monads
import Analysis.Symbolic.Monad
import Analysis.Actors.Monad
import Analysis.Contracts.Monad

-- Semantics
import qualified Analysis.Contracts.Semantics as Contracts
import qualified Analysis.Symbolic.Semantics as Symbolic

-- Stores
import Analysis.Contracts.Store
import Analysis.Actors.Mailbox

-- ModX
import Analysis.Scheme.Actors.Components

-- Domains
import qualified Domain.Contract.CP as CCP
import qualified Domain.Scheme.Actors.CP as CP
import Domain.Contract.Symbolic
import Domain.Symbolic.Paired
import Domain.Contract.Store
import Lattice.Class (bottom)

-- Control monads
import qualified Control.Monad.State.SVar as SVar
import Control.Monad.State.SVar (mergeMap)
import qualified Control.Fixpoint.EffectDriven as EF

import Control.Monad.Trans.Class
import Control.Monad.Identity
import Control.Monad.Extra

-- Solving
import Solver (FormulaSolver, runCachedSolver)
import qualified Solver
import Symbolic.SMT (setupSMT)
import Solver.Z3 (runZ3Solver)

-- Builtin data
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Extra as Map.Extra
import Data.Function ((&))
import Data.Functor.Identity
import Data.Maybe
import Text.Printf
import Prelude hiding (exp, iterate)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra

-- Propagation of path conditions and symbolic stores 
-- accross component boundaries
import Analysis.Symbolic.Monad.Propagation

import Debug.Trace
import Analysis.Symbolic.Monad.Propagation (PropagationStrategy(initialSSto))
import Analysis.Context (emptyMcfaContext)

------------------------------------------------------------
-- Evaluation function
------------------------------------------------------------

newtype SymbolicEvalT v m a = SymbolicEvalT { getSymbolicEvalT :: m a } deriving (Applicative, Functor, Monad, MonadJoin)

instance MonadTrans (SymbolicEvalT v) where
   lift = SymbolicEvalT
instance MonadLayer (SymbolicEvalT v) where
   upperM = SymbolicEvalT
   lowerM f (SymbolicEvalT m) = SymbolicEvalT (f m)

instance (ContractM (SymbolicEvalT v m) v msg mb, SymbolicM (SymbolicEvalT v m) v) => EvalM (SymbolicEvalT v m) v Exp where
   eval = Symbolic.eval

-- TODO: this is rather ugly right now but needed
-- since we cannot derive MonadEscape yet if it 
-- is not on top of the layers (see Control.Monad.Layer)
instance (Monad m, MonadEscape m, Esc m ~ Set Error) => MonadEscape (SymbolicEvalT v m) where
   type Esc (SymbolicEvalT v m) = Set Error
   throw = upperM . throw
   catch (SymbolicEvalT m) hdl = SymbolicEvalT $ catch @_ m (getSymbolicEvalT . hdl)

runSymbolicEvalT :: SymbolicEvalT v m a -> m a
runSymbolicEvalT (SymbolicEvalT m) = m

------------------------------------------------------------
-- Symbolic Pid representation
------------------------------------------------------------

instance SymbolicARef (Pid ctx) where
   identity EntryPid   = Symbolic.Literal $ Symbolic.Actor Nothing
   identity (Pid e _)  = Symbolic.Literal $ Symbolic.Actor (Just $ spanOf e)


------------------------------------------------------------
-- Aliases for convenience
------------------------------------------------------------

type K = MCFA Exp
type Vlu k = V k
type Sto k = SSto k (Vlu k)
type RetSto k = Map (Component k) (SVar.SVar (Vlu k))

-- | Alias for messages
type Msg k = ()

-- | Alias for the mailbox
type MB k = Set (Msg k)

-- | State of all mailboxes
type Mailboxes k = Map (Pid k) (SVar.SVar (MB k))

------------------------------------------------------------
-- Analysis
------------------------------------------------------------

data Stores k = Stores { vsto :: Sto k, rsto ::  RetSto k, csto :: ContractStore' SVar k (Vlu k) }

type State k = (Stores k, Mailboxes k)
stores :: State k -> Stores k
stores = fst


-- | Inter analysis monad
type InterM m k = (
         FormulaSolver m,
         EffectSVarM m (Component k),
         SVar.MonadStateVar m)

-- | Convenience constraints for working with propagation strategies
type Propagation s k  = (PropagationStrategy s (EnvAdr k) (Vlu k) k)

-- | Result of intra-analysis
type IntraResult k = ([MayEscape (Set Error) (Vlu k)], State k)

-- | Simple intra-analysis
intra :: forall m s k . (InterM m k, Propagation s k)
      => Exp
      -> Component k
      -> k
      -> Pid k
      -> Env k
      -> State k
      -> m (IntraResult k)
intra e cmp ctx pid env (Stores store retStore contractStore, mbs) = undefined
         --(mailbox, mbs') <- firstM SVar.read =<< Map.Extra.lookupInsertDefaultF (SVar.depend empty) pid mbs
         --fmap result $ setupSMT >> Symbolic.eval e >>= (\v -> writeAdr cmp v >> return v)
         --                                     & runSymbolicEvalT
         --                                     & runMayEscape @(Set Error)
         --                                     & runCallT @(Vlu k) @k
         --                                     & runPropagationHookT @s @k @(EnvAdr k) @_ @(Component k) ctx
         --                                     & runWithFormulaT (initialPc @s ctx)
         --                                     & runSchemeAllocT (EnvAdr @k) (VecAdr @k) (PaiAdr @k) (StrAdr @k)
         --                                     --
         --                                     & runContractAllocT @k
         --                                     -- 
         --                                     & runSpawnT
         --                                     & runCtx ctx
         --                                     & runEnv env
         --                                     & runActorT @(MB k) mailbox pid
         --                                     & runSymbolicStoreT @(EnvAdr k) @(Vlu k) (initialSSto @s ctx)
         --                                     -- & runSymbolicStoreT @(Component K) @(Vlu (Context s K)) Map.empty
         --                                     & runNonDetT
         --                                     & runSchemeStoreT store                  -- scheme store
         --                                     & runContractStoreT contractStore        -- contract store
         --                                     & runStoreT' retStore                    -- return values
         --                                     & runPropagationStoreT @(Vlu k) @s @(EnvAdr k) @(Component k)
         --                                     & runActorSystemT mbs'
         --                                     & runIntegerPoolT


   -- where result ((((as, ssto'), csto'), rsto'), mbs') =
   --         (fmap localResult as, (Stores ssto' rsto' csto', mbs'))
   --       localResult ((a, _pc), _) = a

-- | Run the intra analysis based on the state of a component
-- runIntra :: forall m s k . (InterM m k, Propagation s k) => k -> Component k -> State k -> m (State k)
-- runIntra initialCtx c@(Main e) = trace "main" $ intra @_ @s e c initialCtx EntryPid analysisEnv >=>  return . snd
-- runIntra _ c@(Actor pid e k env) = trace "actor" $ intra @_ @s e c k pid env >=> return . snd

-- | Compute the initial state of the analysis
--initialState :: (SVar.MonadStateVar m, Ord k, Show k) => m (State k)
--initialState = do
--   analysisSto <- SVar.fromMap $ initialSto analysisEnv
--   return (Stores (fromValues analysisSto) Map.empty emptyContractStore, emptyActorSystem)

-- | Run the inter analysis
--inter :: forall s k .  Propagation s k => k -> Exp -> IO (State k, SVar.VarState)
--inter k e = runZ3Solver
--          $ runCachedSolver
--          $ runEffectT @[_] (Main e)
--          $ setupSMT >> setup initialState >>= iterate (runIntra @_ @s k)

-- 
simpleAnalysis :: Exp -> IO (Vlu K, DSto K (Vlu K))
simpleAnalysis e = undefined -- ret . uncurry unifyStores . first stores <$> inter @(NoPropagation _ _ K) (emptyMcfaContext 1) e
--   where unifyStores stores state = (SVar.unify (rsto stores) state, unifyStore (vsto stores) state)
--         ret (rsto, vsto) = (fromMaybe bottom $ Map.lookup (Main e) rsto, vsto)
