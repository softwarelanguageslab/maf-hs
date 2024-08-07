-- | Specification of the symbolic language
-- used for encoding constraints generated by the
-- program under analysis.
module Symbolic.AST
  ( SolverResult (..),
    Formula (..),
    Proposition (..),
    Literal (..),
    SelectVariable (..),
    isSat,
    isUnsat,
    isUnknown,
    PC,
  )
where

import Syntax.Scheme (Span)
import Data.Set (Set)

-- | A literal as they appear in a source program
data Literal
  = Num !Integer
  | Rea !Double
  | Str !String
  | Boo !Bool
  | Cha !Char
  | Sym !String
  | -- | a behavior of an actor
    Beh
  | -- | a contract monitor
    Mon
  | Nil
  | Unsp
  | Actor !(Maybe Span)
  deriving (Eq, Ord, Show)

-- | A proposition consists of an
-- application of a primitive predicate,
-- or of a single variable holding a particular truth value.
--
-- All variables for our propositions are universivelly
-- quantified.
data Proposition
  = Variable !String
  | Literal !Literal
  | -- | assertion that the proposition's truth value is "true"
    IsTrue !Proposition
  | -- | assertion that the proposition's trught value is "false
    IsFalse !Proposition
  | -- | an atomic predicate
    Predicate !String ![Proposition]
  | Application !Proposition ![Proposition]
  -- | A statement that is always true
  | Tautology
   -- | Generate an unquantified fresh variable
  | Fresh
   -- | Representation of the bottom value, nothing can be derived from this and a
   -- all assertions fail
  | Bottom
  deriving (Eq, Ord, Show)

-- | Inductively defined formulae, these include
-- conjunction, disjunction negation and atomic formulas.
data Formula
  = Conjunction !Formula !Formula
  | Disjunction !Formula !Formula
  | Implies !Formula !Formula
  | Negation !Formula
  | Atomic !Proposition
  | Empty
  deriving (Eq, Ord, Show)

-- | The path condition is an unordered conjunction of formulas
type PC = Set Formula

-- | Select all variables in the formula
class SelectVariable v where
  variables :: v -> [String]

-- | Variables can be selected from formulas
instance SelectVariable Formula where
  variables (Conjunction f1 f2) = variables f1 ++ variables f2
  variables (Disjunction f1 f2) = variables f1 ++ variables f2
  variables (Negation f) = variables f
  variables (Implies f1 f2) = variables f1 ++ variables f2
  variables (Atomic prop) = variables prop
  variables Empty = []

-- | Variables can be selected from propositions
instance SelectVariable Proposition where
  variables (Variable nam) = pure nam
  variables (IsTrue prop) = variables prop
  variables (IsFalse prop) = variables prop
  variables (Predicate _ props) = mconcat (map variables props)
  variables (Literal _) = []
  variables Tautology = []
  variables Fresh = []
  variables Bottom = []
  variables (Application p1 p2) = variables p1 ++ mconcat (map variables p2)

-- |  The result of solving an SMT formula.
data SolverResult
  = Sat
  | Unsat
  | Unknown
  deriving (Show)

isSat :: SolverResult -> Bool
isSat Sat = True
isSat _ = False

isUnsat :: SolverResult -> Bool
isUnsat Unsat = True
isUnsat _ = False

isUnknown :: SolverResult -> Bool
isUnknown Unknown = True
isUnknown _ = False
