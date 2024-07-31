{-# LANGUAGE AllowAmbiguousTypes #-}
module Analysis.Contracts.Semantics(eval, ContractM) where

import Prelude hiding (exp)
import Lattice hiding (Bottom)
import Syntax.Scheme
import Analysis.Monad(store, deref)
import qualified Analysis.Monad as Monad
import Analysis.Contracts.Monad (ContractM, Error (NotAContract, BlameError, AssertionError), AssertionMessage (..))
import Domain.Contract ( ContractDomain(..), Flat(..), Moα (..) )
import Domain.Scheme.Actors.Contract (MessageContract(MessageContract))
import qualified Domain.Scheme.Actors.Contract as Contract
import qualified Analysis.Actors.Semantics as Actors
import qualified Analysis.Scheme.Semantics as Scheme
import Control.Monad ((>=>), void, zipWithM)
import Analysis.Contracts.Behavior (behaviorContract, BehaviorContract (..))
import qualified Data.Set as Set
import Control.Monad.Join
import Control.Monad.DomainError (escape, DomainError (WrongType))
import Domain ( ActorDomain(..), SchemeDomain(symbol) )
import Analysis.Actors.Monad ((!), send)
import Domain.Scheme.Actors.Message
import qualified Domain.Core.SeqDomain.BoundedList as BoundedList
import Domain.Scheme (nil)
import Data.Set (Set)
import Data.Bifunctor
import Analysis.Symbolic.Monad (choices, choice)

-- | Flip the negative and positive parties of the blame 
-- label
flipLabel :: Labels -> Labels
flipLabel (Labels pos neg) = Labels neg pos

-- | Monitors a flat contract against a value, or a recipient contract
-- against an actor reference
monFlat :: forall m v msg mb . ContractM m v msg mb => Exp -> Set Labels -> v -> v -> m v
monFlat e lbl contract value =
      cond (deref (const $ flip (Scheme.applyFun e) [value] . flatProc) (flats contract))
           (return value)
           (escape $ BlameError (Set.map positive lbl))

-- | Same as `monFlat` but first checks whether the contrat is indeed a flat contract
ensureMonFlat :: ContractM m v msg mb => Exp -> Set Labels -> v -> v -> m v
ensureMonFlat exp lbls contract value =
      cond @(CP Bool) (pure $ isFlat contract)
           (monFlat exp lbls contract value)
           (escape NotAContract)

-- | Monitors on actor references result in monitored
-- actor references. These actor references keep track
-- of their original behavior contract and some additional
-- (flipped) blame labels.
monAct :: ContractM m v msg mb => Exp -> Set Labels -> v -> v -> m v
monAct e lbl contract value =
      αmon <$> store e (Moα lbl contract value)

-- | Contract monitoring function, monitors a contract on value,
-- blaming the positive part of `Labels` if a contract violation is found.
mon :: forall m v msg mb . ContractM m v msg mb => Exp -> Set Labels -> v -> v -> m v
mon e lbl contract value =
   choices
      [-- [MonFlat]
       (pure (isFlat contract), monFlat e lbl contract value),
       -- [MonAct]
       (pure (isBehaviorContract @_ contract), monAct e lbl contract value)]
      {- else -}
      (escape NotAContract)

-- | Checks whether the given send is valid according to the contract.
-- It applies `f` on the resulting actor references and passes the monitored payload 
-- along.
checkSend :: forall v a m msg mb . (Lattice a, ContractM m v msg mb) => Exp -> (String -> [v] -> ARef v -> m a) -> String -> [v] -> Moα v -> m a
checkSend e f tag payload (Moα lbl contract value) =
      choices
         [ (pure $ isActorRef value, do
               { (rcptc, payload') <- check ; mjoins $ map (checkRcpt rcptc tag payload') (Set.toList (arefs' value)) }),
           (pure $ isαmon value, do
               { (rcptc, payload') <- check ; deref (const $ checkSend e (checkRcpt rcptc) tag payload') (αmons value) }) ]
         (escape WrongType)
   where check :: m (v, [v])
         check =
             second BoundedList.elements <$> choice (pure $ isBehaviorContract @_ contract)
               (do
                  -- TODO: actually this is not quite sound, we also need 
                  -- to consider the case where we are not sure whether
                  -- a tag matches the tag of the message, and if so 
                  -- generate a blame error!
                  contracts <- matchingContracts (symbol tag) contract
                  payload' <- mjoinMap checkPayload contracts
                  rcptc <- Scheme.applyFun e (joinMap Contract.rcpt contracts) (BoundedList.elements payload')
                  return (rcptc, payload')
               )
               (escape WrongType)
         checkRcpt rcptc tag' payload' rcpt =
            mjoinMap (f tag' payload') . Set.toList . arefs' =<< ensureMonFlat e (Set.map flipLabel lbl) rcptc (aref rcpt)
         checkPayload contract' =
            BoundedList.fromList <$> zipWithM (mon e (Set.map flipLabel lbl))
                                              (BoundedList.elements (Contract.payload contract'))
                                              payload
checkSend _ _ _ _ Bottom = mzero


-- | Monitored message send (sender-side contracts)
monSend :: forall m v msg mb . ContractM m v msg mb => Exp -> v -> String -> [v] -> m v
monSend e contract tag values =
   choices
      [-- Actor reference ==> regular send
       (pure $ isActorRef contract, mjoins $ map (! message tag values) (Set.toList (arefs' contract))),
       -- Monitored value 
       (pure $ isαmon contract, void $ deref (const $ checkSend e (\t p r -> void $ send r (message t p)) tag values) (αmons contract) )]
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
      (store exp =<< (MessageContract <$> Monad.eval tag <*> Monad.eval rcv <*> (BoundedList.fromList <$> mapM Monad.eval payload) <*> Monad.eval comm))
eval (BehC exs _) =  do
   vlus <- mapM Monad.eval exs
   adrs <- joins <$> mapM (assert isMessageContract ExpectedMessageContract >=> pure . messageContracts) vlus
   return (behaviorContract @_ (Set.toList adrs))
eval exp@(Syntax.Scheme.Flat e _) = do
   flat <$> (store exp . Domain.Contract.Flat =<< Monad.eval e)
eval exp@(Mon labels contract value _) = do
   contract' <- Monad.eval contract
   value'    <- Monad.eval value
   mon exp (Set.singleton labels) contract' value'
eval e@(Sen rcpt tag values _) = do
   rcpt'    <- Monad.eval rcpt
   values'  <- mapM Monad.eval values
   monSend e rcpt' tag values'


eval e = Actors.eval e
