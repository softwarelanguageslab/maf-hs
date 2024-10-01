{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
-- | The domain of our programming language defined as a 
-- sparse labeled product over integers and closures.
--
-- Operations to extract and inject these values from/into
-- the abstract domain are also provided.
--
-- Note that you would usually define the operations 
-- specific the programming language's abstract domain 
-- as a type class so that multiple instances of this 
-- abstract domain can be implemented. 
--
-- For example, another representation than the 
-- sparse labeled product may be used to represent 
-- abstract values in this our lambda programming language.
module Domain.Lambda(Env, Clo, M, LamVal(..), clo, num, nonzero, inc, clos, SLamKey(..)) where

import Data.Singletons
import Domain (inject)
import Lattice (Joinable, Lattice, PartialOrder, BottomLattice)
import Lattice.ConstantPropagationLattice
import Data.TypeLevel.HMap
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Syntax.Lambda

import Control.Monad.Join
import Analysis.Scheme.Prelude (escape)
import Control.Monad.DomainError (DomainError(..))
import Control.Monad.Escape (MonadEscape (..))
import Domain.Class (Domain)

-- | Shorthand for environments
type Env adr = Map String adr
-- | Shorthand for closures
type Clo adr = (Env adr, Exp)

-- | The labels for the sparse labeled product
data LamKey = IntKey | CloKey deriving (Eq, Ord)
-- this is necessary for the infrastructure promoting
-- data constructors to types and the other way around.
$(genHKeys ''LamKey)

-- | The definition of the contents of the sparse 
-- labeled product
type M adr = '[ IntKey ::-> CP Int, 
                CloKey ::-> Set (Clo adr) ]

-- | Abstract values in our programming language 
-- are constructed from sparse labeled products with 
-- the contents defined by the type alias @M@ (cf. above)
newtype LamVal adr = LamVal (HMap (M adr))
   deriving (Eq, Joinable, BottomLattice, PartialOrder)

-- | 
clos :: forall a adr m . Lattice a => MonadJoin m => (Clo adr -> m a) -> LamVal adr -> m a
clos f (LamVal v) = mjoins (mapList select v)
   where select :: forall k . Sing (k :: LamKey) -> Assoc k (M adr) -> m a
         select SCloKey v' = mjoins $ Prelude.map f (Set.toList v')
         select _ _ = mzero

-- | Inject a closure in the domain
clo :: Clo adr -> LamVal adr
clo = LamVal . singleton @CloKey . Set.singleton

-- | Inject a number in the domain
num :: Int -> LamVal adr
num = LamVal . singleton @IntKey . inject

-- | Checks if the given value is nonzero
-- This operation is more interesting than the previous ones
-- since it has to select a particular part of the 
-- labeled product to perform its operations on and 
-- produce an error for all other incompatible constituents.
--
-- To do so, the operation returns an @m (CP Bool)$ instead
-- of a @CP Bool@. The context @m@ is constrained by 
-- two monads: the @MonadEscape@ and the @MonadJoin@.
--
-- The @MonadJoin@ type class enables using the @mjoins@
-- function which joins multiple computations together 
-- into a single one. The @MonadEscape@ type class 
-- enables expressing escaping control flow, which
-- is used in this case for expressing an exception.
--
-- Finally, the @Domain@ constraint ensures that there 
-- is a mapping from a concrete @DomainError@ to 
-- an abstract representation of that error.
nonzero :: forall adr m . (MonadJoin m, MonadEscape m, Domain (Esc m) DomainError) => LamVal adr -> m (CP Bool)
nonzero (LamVal v) = mjoins (mapList select v)
   -- @select@ receives a "Sing" which corresponds to a demote 
   -- datatype constructed from the labels of the sparse 
   -- labeled product. Based on this @Sing@ datatype 
   -- the possible values passed as the second argument 
   -- to @select@ can be limited to the correct constituent 
   -- of the sparse labeled product.
   --
   -- For example for @SIntKey@ the associated constituent type 
   -- is @CP Int@ meaning that the static type of @v'@ is 
   -- @CP Int@. This renders reasoning over the constituents
   -- in the sparse labeled product type safe. 
   where select :: forall k . Sing (k :: LamKey) -> Assoc k (M adr) -> m (CP Bool)
         -- Calling @nonzero@ with a closure is a type error, 
         -- meaning that the analysis should fail or "escape"
         -- with a @WrongType@.
         select SCloKey _ = escape WrongType
         select SIntKey v' = return $ fmap (==0) v'

-- | Abstract operation to increment a value defined over lambda-language values.
--
-- The error handling and selection logic is identical to the 
-- @nonzero@ function.
inc :: forall adr m . (Ord adr, MonadJoin m, MonadEscape m, Domain (Esc m) DomainError) => LamVal adr -> m (LamVal adr)
inc (LamVal v) = mjoins (mapList select v)
   where select :: forall k . Sing (k :: LamKey) -> Assoc k (M adr) -> m (LamVal adr)
         select SCloKey _ = escape WrongType
         select SIntKey v' = return $ LamVal $ singleton @IntKey (fmap (+1) v')
