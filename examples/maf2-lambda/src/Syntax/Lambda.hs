-- | This module defines the syntax of the programming 
-- language.
module Syntax.Lambda(Ident(..), Exp(..)) where

-- | Identifiers, based on a string and their 
-- position in the source code. 
--
-- The position information is used for 
-- accurate allocation-site addresses
-- which are needed for a precise analysis.
data Ident = Ident String Int
           deriving (Eq, Ord, Show)

-- | The expression abstract syntax tree.
data Exp = Lam Ident Exp   
         | Num Int 
         | If Exp Exp Exp 
         | Nonzero Exp 
         | Inc Exp 
         | App Exp Exp 
         | Var Ident
         deriving (Eq, Ord, Show)
   
