{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- | This module provides a typelevel association list,
-- useful for defining mappings between types.
--
-- Additionally, it provides auxilary type level function 
-- for manipulating the type the association list.
--
-- For example, function `MapWith` maps a function `f` over 
-- all keys in the mapping. 
module Data.TypeLevel.AssocList(
   AtKey,
   LookupIn,
   Const,
   And,
   AtKey1,
   InstanceOf,
   KeyIs,
   KeyIs1,
   (::->),
   (:->),
   Assoc,
   Keys,
   MapWith,
   MapWithAt,
   AssocValue(..),
   HAssocList(..),
   nil,
   HList(..),
   Sigma(..),
) where

import Data.Kind
import Data.Void
import Data.Singletons
import Data.Singletons.Sigma

data a :-> b = a :-> b
type a ::-> b = a ':-> b -- nicer than the ':-> syntax ':-) 

type family Assoc (kt :: k) (m :: [k :-> Type]) :: Type where
  Assoc kt (kt ::-> t : r)  = t
  Assoc kt (_ : r)          = Assoc kt r
  Assoc kt '[]              = Void -- Should be this? TypeError (Text "The type " :<>: ShowType kt :<>: Text " was not found in the mapping")
type family Keys (m :: [k :-> Type]) :: [k] where
  Keys '[]              = '[]
  Keys (kt ::-> _ ': r) = (kt ': Keys r)

-- | Maps function `f` over `ks` and returns a mapping where each key is mapped to 
-- the result of applying `f` to that key
type family MapWith (f :: k ~> Type) (ks :: [k]) :: [k :-> Type] where
  MapWith f '[]       = '[]
  MapWith f (kt ': r) = kt ::-> (f @@ kt) : MapWith f r

-- | Looks up the value of key `kt` in the map resulting from 
-- mapping f over association list `m`.
type MapWithAt f kt m = Assoc kt (MapWith f (Keys m))

-- | Enforces the constraint generated by `c` to hold on the value associated with key `kt`
data AtKey (c :: Type ~> Constraint) (m :: [k :-> Type]) :: k ~> Constraint
type instance Apply (AtKey c m) kt = c @@ Assoc kt m

-- | Lookup the value of the given key in the given map
data LookupIn (m :: [k:->Type]) :: k ~> Type
type instance Apply (LookupIn m) k = Assoc k m

-- | Generates a defunctionalized typelevel function that ignores 
-- its argument and returns `t`
data Const (t :: k) :: a ~> k
type instance Apply (Const t) _ = t

-- | Combine two defunctionalized constraints into a single constraint
data And (c1 :: t ~> Constraint) (c2 :: t ~> Constraint) :: t ~> Constraint
type instance Apply (And c1 c2) kt = (c1 @@ kt, c2 @@ kt)

-- | Same as `AtKey` but does not require a defunctionalised `c`. Instead
-- a regular function can be passed.
type AtKey1 (c :: Type -> Constraint) (m :: [k :-> Type]) = AtKey (InstanceOf c) m :: k ~> Constraint

-- | Defunctionalized the given function `c`
data InstanceOf (c :: a -> b) :: a ~> b
type instance Apply (InstanceOf a) b = a b

-- | Generates a constraint `c` on the value of associated with key `k` of map `m`
type KeyIs (c :: Type ~> Constraint) (m :: [k :-> Type]) (kt :: k) = AtKey c m @@ kt
-- | Same as `KeyIs` but takes a normal function as its input, 
-- equivalent to KeyIs (InstanceOf c) m kt,  for some c m and kt
type KeyIs1 (c :: Type -> Constraint) (m :: [k :-> Type]) (kt :: k) = AtKey1 c m @@ kt

data HList :: [Type] -> Type where
   HNil  :: HList '[]
   (:+:) :: a -> HList as -> HList (a ': as)

infixr 6 :+:

nil  :: HList '[] 
nil  = HNil

type family Transform (m :: [k :-> Type]) where
   Transform '[] = '[]
   Transform (a ::-> b ': r) = b ': Transform r

newtype HAssocList m = HAssocList (HList (Transform m))

class AssocValue (m :: [k :-> Type]) (t :: k) where
   assoc :: HAssocList m -> Assoc t m
instance {-# OVERLAPPING #-} AssocValue '[] k where   
   assoc (HAssocList HNil) = error "key not found"
instance {-# OVERLAPPING #-} AssocValue (k ::-> v ': r) k where   
   assoc (HAssocList (a :+: _)) = a
instance (AssocValue r k1, Assoc k1 r ~ Assoc k1 (k ::-> v ': r)) => AssocValue (k ::-> v ': r) k1 where 
   assoc (HAssocList (_ :+: b)) = assoc @_ @r @k1 (HAssocList b)
