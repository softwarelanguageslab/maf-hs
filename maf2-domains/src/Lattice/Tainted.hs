module Lattice.Tainted where 

import Lattice.Class

data Tainted t a = Tainted a t
    deriving (Eq, Ord, Show)

instance (BottomLattice a, BottomLattice t) => BottomLattice (Tainted t a) where
    bottom = Tainted bottom bottom
instance (Joinable a, Joinable t) => Joinable (Tainted t a) where
    join (Tainted a1 t1) (Tainted a2 t2) = Tainted (a1 `join` a2) (t1 `join` t2)
instance (PartialOrder a, PartialOrder t) => PartialOrder (Tainted t a) where
    leq (Tainted a1 t1) (Tainted a2 t2) = a1 `leq` a2 && t1 `leq` t2 