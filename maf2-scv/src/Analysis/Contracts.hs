{-# LANGUAGE UndecidableInstances #-}
module Analysis.Contracts where

import Prelude hiding (exp)
import Domain hiding (Exp, Env)
import qualified Domain.Scheme.Actors.CP as CP
import Data.Map (Map)
import Data.TypeLevel.HMap ((::->))
import Analysis.Contracts.Behavior
import Analysis.Monad
import qualified Analysis.Contracts.Semantics as Sem
import Lattice
import Control.Monad.Identity
import Control.Monad.DomainError
import Control.Monad.Trans.Class
import Control.Monad.Layer
import Domain.Contract.Store
import Data.Set (Set)
import qualified Data.Set as Set
import Analysis.Contracts.Monad
import Control.Monad.Join
import Analysis.Actors ()
import Analysis.Actors.Monad
import Analysis.Scheme.Prelude hiding (spawn, Env)

------------------------------------------------------------
-- Environment
------------------------------------------------------------

type Env = Map String (EnvAdr K)

------------------------------------------------------------
-- Domain
------------------------------------------------------------

type M = '[
       RealConf ::-> CP Double,
       IntConf  ::-> CP Integer,
       CharConf ::-> CP Char,
       BoolConf ::-> CP Bool,
       EnvConf ::-> Env,
       ExpConf ::-> Exp,
       StrConf ::-> StrAdr K,
       PaiConf ::-> PaiAdr K,
       VecConf ::-> VecAdr K,
       VarConf ::-> EnvAdr K,
       PidConf ::-> CP.Pid K,
       BeCConf ::-> UnorderedBehaviorContract (MsCAdr K),
       MoαConf ::-> MoαAdr K,
       FlaConf ::-> FlaAdr K,
       PMeConf ::-> MsCAdr K]

type V = SchemeVal M

type instance VarDom V = V
type instance PaiDom V = SimplePair V
type instance VecDom V = PIVector V V
type instance StrDom V = SchemeString (CP String) V

------------------------------------------------------------
-- Actors
------------------------------------------------------------

type Msg = ()
type MB  = Set Msg

------------------------------------------------------------
-- EvalT
------------------------------------------------------------

newtype EvalT m a = EvalT { getEvalT :: IdentityT m a }
                  deriving (Monad, Applicative, Functor, MonadTrans, MonadJoin, MonadLayer)


instance (Esc m ~ Set Error, ContractM (EvalT m) V Msg MB) => EvalM (EvalT m) V Exp where
   eval e = Sem.eval e `catch` (mjoins . map handle . Set.toList)
      where handle err@(WithLoc _ _) = escape err
            handle err = escape (WithLoc err $ spanOf e)

runEvalT :: EvalT m a -> m a
runEvalT (EvalT m) = runIdentityT m


instance (Monad m, MonadEscape m, Esc m ~ Set Error) => MonadEscape (EvalT m) where
   type Esc (EvalT m) = Set Error
   throw = upperM . throw
   catch (EvalT (IdentityT m)) hdl = EvalT $ IdentityT $ catch @_ m (runIdentityT . getEvalT . hdl)

------------------------------------------------------------
-- SpawnT
------------------------------------------------------------

newtype SpawnT m a = SpawnT (IdentityT m a) deriving (Applicative, Monad, Functor, MonadTrans, MonadLayer, MonadJoin)

-- TODO: this is just a test to see whether the implementation
-- works without actually using the fixpoint
instance {-# OVERLAPPING #-} (EnvM m (EnvAdr K) Env) => ActorBehaviorM (SpawnT m) V where
  spawn e = do
    --env' <- getEnv
    --upperM (EF.spawn (Actor (CP.Pid e ()) e env'))
    return (aref (CP.Pid e ()))
  become = void . spawn

runSpawnT :: SpawnT m a -> m a
runSpawnT (SpawnT m) = runIdentityT m

------------------------------------------------------------
-- Store
------------------------------------------------------------

type K = ()
type Sto = Map (EnvAdr K) V

runAnalysis :: Exp -> (MayEscape (Set Error) V, Sto)
runAnalysis = undefined
-- runAnalysis exp = let ((((((((v, _), _), _), _), _), _), sto), _) =  Sem.eval @V exp
--                          & runEvalT
--                          & runMayEscape
--                          & runCallBottomT @V
--                          & runSpawnT
--                          & runEnv (analysisEnv @K)
--                          & runStoreT @(PaiAdr K) Map.empty
--                          & runStoreT @(VecAdr K) Map.empty
--                          & runStoreT @(StrAdr K) Map.empty
--                          & runStoreT @(MsCAdr K) Map.empty
--                          & runStoreT @(FlaAdr K) Map.empty
--                          & runStoreT @(MoαAdr K) Map.empty
--                          & runStoreT @(EnvAdr K) @V (initialSto (analysisEnv @K))
--                          & runAlloc @_ @K PaiAdr
--                          & runAlloc @_ @K VecAdr
--                          & runAlloc @_ @K StrAdr
--                          & runAlloc @_ @K EnvAdr
--                          & runAlloc @_ @K MsCAdr
--                          & runAlloc @_ @K MoαAdr
--                          & runAlloc @_ @K FlaAdr
--                          & runActorT @MB Set.empty EntryPid
--                          & runCtx ()
--                          & runNoSendT
--                          & runIdentity
--                   in (v, sto)
