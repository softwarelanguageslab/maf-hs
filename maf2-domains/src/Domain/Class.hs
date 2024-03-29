module Domain.Class (Domain(..), Domain') where

import Data.Kind
import Data.Singletons
import Lattice.Class

-- | A domain maps a concrete value `c`
-- into an abstract value `v` using the `inject` 
-- function.
class JoinLattice v => Domain v c where
   inject :: c -> v
   injects :: Foldable t => t c -> v 
   injects = joinMap inject 

-- | A curried and flipped version of the `Domain` constraint
data Domain' (c :: Type) :: Type ~> Constraint
type instance Apply (Domain' c) v = Domain v c
