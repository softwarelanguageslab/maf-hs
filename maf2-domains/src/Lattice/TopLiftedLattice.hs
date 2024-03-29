-- | Lifts an infinite domain such that a widening 
-- operator is implemented that widens to `Top.
module Lattice.TopLiftedLattice(TopLifted) where

import Lattice.Class

import qualified Data.Set as Set

data TopLifted a = Value a 
                 | Top
   deriving (Eq, Ord) 

instance (Joinable v) => Joinable (TopLifted v) where 
   join Top _   = Top 
   join _   Top = Top 
   join (Value v1) (Value v2) = Value (join v1 v2)
 
instance (JoinLattice v) => JoinLattice (TopLifted v) where 
   bottom = Value bottom
   subsumes Top _ = True 
   subsumes _ Top = False
   subsumes (Value v1) (Value v2) = subsumes v1 v2

instance (JoinLattice v) => TopLattice (TopLifted v) where 
   top = Top

instance (SplitLattice v, Ord v) => SplitLattice (TopLifted v) where
   split (Value v) = Set.map Value (split v) 
   split Top = Set.singleton Top 