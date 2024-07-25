module Domain.Core.VectorDomain.Class (VectorDomain(..)) where 

import Lattice 
import Control.Monad.AbstractM

import Data.Kind 

class (Lattice v) => VectorDomain v where
   type VContent v :: Type
   type VIndex   v :: Type

   makeVector :: VIndex v -> VContent v -> v
   vectorRef :: AbstractM m => v -> VIndex v -> m (VContent v)
   vectorSet :: AbstractM m => v -> VIndex v -> VContent v -> m v
   vectorLength :: v -> VIndex v
