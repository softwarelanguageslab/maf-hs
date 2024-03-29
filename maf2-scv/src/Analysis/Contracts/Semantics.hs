{-# LANGUAGE AllowAmbiguousTypes #-}
module Analysis.Contracts.Semantics(eval, ContractM) where

import Prelude hiding (exp)
import Lattice
import Syntax.Scheme
import Analysis.Monad(store, deref)
import qualified Analysis.Monad as Monad
import Analysis.Contracts.Monad (ContractM, Error (BlameError, AssertionError), AssertionMessage (..))
import Domain.Contract ( ContractDomain(..), Flat(..), Moα (..) )
import Domain.Scheme.Actors.Contract (MessageContract(MessageContract))
import qualified Analysis.Actors.Semantics as Actors
import qualified Analysis.Scheme.Semantics as Scheme
import Control.Monad ((>=>))
import Analysis.Contracts.Behavior (behaviorContract, BehaviorContract (..))
import qualified Data.Set as Set
import Control.Monad.Join
import Control.Monad.DomainError (escape, DomainError (WrongType))
import Domain (ActorDomain(..))
import Analysis.Actors.Mailbox (Message(message))
import Analysis.Actors.Monad ((!))
import Domain.Scheme (nil)

-- | Flip the negative and positive parties of the blame 
-- label
flipLabel :: Labels -> Labels
flipLabel (Labels pos neg) = Labels neg pos

-- | Monitors a flat contract against a value
monFlat :: ContractM m v msg mb => Exp -> Labels -> v -> v -> m v
monFlat e lbl contract value =
      cond (deref (const $ flip (Scheme.applyFun e) [value] . flatProc) (flats contract))
           (return value)
           (escape $ BlameError (positive lbl))

-- | Monitors on actor references result in monitored
-- actor references. These actor references keep track
-- of their original behavior contract and some additional
-- (flipped) blame labels.
monAct :: ContractM m v msg mb => Exp -> Labels -> v -> v -> m v
monAct e lbl contract value = 
      αmon <$> store e (Moα (Set.singleton (flipLabel lbl)) contract value)
   
-- | Contract monitoring function, monitors a contract on value,
-- blaming the positive part of `Labels` if a contract violation is found.
mon :: forall m v msg mb . ContractM m v msg mb => Exp -> Labels -> v -> v -> m v
mon e lbl contract value =
   conds @(CP Bool)
      [-- [MonFlat]
       (pure (isFlat contract), monFlat e lbl contract value),
       -- [MonAct]
       (pure (isBehaviorContract @_ @v contract), monAct e lbl contract value)]
      {- else -}
      (escape WrongType)

-- | Monitored message send (sender-side contracts)
monSend :: forall m v msg mb . ContractM m v msg mb => v -> String -> [v] -> m v
monSend contract tag values = 
   conds @(CP Bool)
      [-- Actor reference ==> regular send
       (pure $ isActorRef contract, mjoins $ map (! message tag values) (Set.toList (arefs' contract))),
       -- Monitored value 
       (pure $ isαmon contract, undefined)]
      {- else -}
      (escape WrongType) >> return nil

-- | TODO: use AssertM (assert)
assert :: forall v m msg mb . (ContractM m v msg mb) => (v -> CP Bool) -> AssertionMessage -> v -> m v
assert b e v = cond (pure (b v)) (return v) (escape (AssertionError e))

-- | This evaluation function extends 'Analysis.Symbolic.Semantics.eval' 
-- with support for contracts on actors.
eval :: forall v m msg mb . ContractM m v msg mb => Exp -> m v
eval exp@(MsgC tag rcv payload comm _) =
   messageContract <$> 
      (store exp =<< (MessageContract <$> Monad.eval tag <*> Monad.eval rcv <*> Monad.eval payload <*> Monad.eval comm))
eval (BehC exs _) =  do
   vlus <- mapM Monad.eval exs
   adrs <- joins <$> mapM (assert isMessageContract ExpectedMessageContract >=> pure . messageContracts) vlus
   return (behaviorContract @_ @v (Set.toList adrs))
eval exp@(Syntax.Scheme.Flat e _) = do
   flat <$> (store exp . Domain.Contract.Flat =<< Monad.eval e)
eval exp@(Mon labels contract value _) = do  
   contract' <- Monad.eval contract
   value'    <- Monad.eval value
   mon exp labels contract' value'
eval (Sen rcpt tag values _) = do
   rcpt'    <- Monad.eval rcpt 
   values'  <- mapM Monad.eval values
   monSend rcpt' tag values'

eval e = Actors.eval e
