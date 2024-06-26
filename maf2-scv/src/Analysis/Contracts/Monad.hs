module Analysis.Contracts.Monad where

import Domain.Class (Domain(..))
import Syntax.Scheme
import Analysis.Actors.Monad 
import Analysis.Monad (StoreM, AllocM)
import Domain.Contract (ContractDomain(..), Flat(..), Moα)
import Domain.Scheme.Actors.Contract (MessageContract)
import Analysis.Contracts.Behavior (MAdr)
import Control.Monad.DomainError
import qualified Data.Set as Set
import Data.Set (Set)
import Domain (ActorDomain)
import Lattice (EqualLattice)
import Analysis.Symbolic.Monad (SymbolicM)

data AssertionMessage = ExpectedMessageContract
   deriving (Eq, Ord, Show)

-- | Error types that could occur while evaluating 
-- the program with contracts
data Error = BlameError (Set String) -- ^ blame error, consisting of the party being blamed for the contract violation  
           | AssertionError AssertionMessage 
           | NotAContract
           | WithLoc Error Span
           | DomainWrap DomainError  -- ^ errors originating from implementations of the domain
           deriving (Eq, Ord, Show)

-- | For a `Set` representation of errors we already have `Domain (Set Error) Error` (by the 'SetLattice').
-- This instance translates a `DomainError` (as defined in 'Control.Monad.DomainError') into `Set Error`.
instance Domain (Set Error) DomainError where   
   inject = Set.singleton . DomainWrap

type ContractM m v msg mb = 
   (  -- Specialised stores
      StoreM m (MAdr v) (MessageContract v),
      StoreM m (FAdr v) (Flat v),
      StoreM m (OAdr v) (Moα v),
      -- Specialized allocations
      AllocM m Exp (MAdr v),
      AllocM m Exp (FAdr v),
      AllocM m Exp (OAdr v),
      -- Symbolic execution
      SymbolicM m v,
      -- Domains
      Domain (Esc m) Error,
      ContractDomain v, 
      ActorDomain v,
      EqualLattice v,
      -- Semantics monads
      ActorEvalM m v msg mb,
      SchemeM m v, Ord v)
