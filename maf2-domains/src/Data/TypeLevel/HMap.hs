{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE StandaloneKindSignatures   #-}

module Data.TypeLevel.HMap (
    HMap,
    (:->),
    (::->),
    Dict(..),
    KeyType,
    Keys,
    KeyKind,
    HMapKey,
    withDict,
    withFacts,
    withKey,
    empty,
    singleton,
    singletonWithSing,
    get,
    getWithSing,
    set,
    setWithSing,
    member,
    memberWithSing,
    null,
    keys,
    size,
    map,
    map',
    mapList,
    filter,
    foldr,
    fromList,
    toList,
    unionWith,
    withC,
    withC_,
    ForAll(..),
    ForAllOf,
    All,
    Assoc,
    LookupIn,
    InstanceOf,
    AtKey,
    AtKey1,
    AllAtKey1,
    KeyIs,
    KeyIs1,
    Const,
    BindingFrom,
    genHKeys, 
    MapWith,
    MapWithAt,
) where

import Prelude hiding (map, filter, foldr, null)
import qualified Prelude
import Data.Kind
import Data.Singletons
import Data.Singletons.TH

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Unsafe.Coerce (unsafeCoerce)

import Data.TypeLevel.HMap.TH
import Data.Singletons.Sigma
import Data.Singletons.Decide

import qualified Data.List as List

--
-- SomeVal (TODO: move this to other utility module?)
--

data SomeVal = forall v . SomeVal v

-- only use this when absolutely sure about the type 
unsafeCoerceVal :: SomeVal -> v
unsafeCoerceVal (SomeVal v) = unsafeCoerce v

--
-- Dict (TODO: move this to other utility module?)
--

data Dict (c :: Constraint) where
  Dict :: c => Dict c

withDict :: Dict c -> (c => v) -> v
withDict Dict = id

---
--- Some useful constraints (TODO: move this to other utility module?)
---

-- | Enforces the constraint generated by `c` to hold on the value of at given key `kt`
data AtKey (c :: Type ~> Constraint) (m :: [k :-> Type]) :: k ~> Constraint
type instance Apply (AtKey c m) kt = c @@ Assoc kt m

data LookupIn (m :: [k:->Type]) :: k ~> Type
type instance Apply (LookupIn m) k = Assoc k m

data Const (t :: k) :: a ~> k
type instance Apply (Const t) _ = t

data And (c1 :: t ~> Constraint) (c2 :: t ~> Constraint) :: t ~> Constraint
type instance Apply (And c1 c2) kt = (c1 @@ kt, c2 @@ kt)

-- | Same as `AtKey` but does not require a defunctionalised `c`. Instead
-- a regular function can be passed.
type AtKey1 (c :: Type -> Constraint) (m :: [k :-> Type]) = AtKey (InstanceOf c) m :: k ~> Constraint 
type AllAtKey1 c m = ForAll (KeyKind m) (AtKey1 c m) :: Constraint 

-- | Defunctionalized the given function `c`
data InstanceOf (c :: a -> b) :: a ~> b
type instance Apply (InstanceOf a) b = a b

-- | Generates a constraint `c` on the value of key `k` of map `m`
type KeyIs (c :: Type ~> Constraint) (m :: [k :-> Type]) (kt :: k) = AtKey c m @@ kt

type KeyIs1 (c :: Type -> Constraint) (m :: [k :-> Type]) (kt :: k) = AtKey1 c m @@ kt

---
--- El HMap
---

data a :-> b = a :-> b
type a ::-> b = a ':-> b -- nicer than the ':-> syntax ':-) 

type family Assoc (kt :: k) (m :: [k :-> Type]) :: Type where
  Assoc kt (kt ::-> t : r)  = t
  Assoc kt (_ : r)          = Assoc kt r
  Assoc kt '[]              = Void
type family Keys (m :: [k :-> Type]) :: [k] where
  Keys '[]              = '[]
  Keys (kt ::-> _ ': r) = (kt ': Keys r)

-- | Maps function `f` over `ks` and returns a mapping where each key is mapped to 
-- the result of applying `f` to that key
type family MapWith (f :: k ~> Type) (ks :: [k]) :: [k :-> Type] where
  MapWith f '[]       = '[]
  MapWith f (kt ': r) = kt ::-> (f @@ kt) : MapWith f r


type MapWithAt f kt m = Assoc kt (MapWith f (Keys m))

-- | Returns the kind of the keys in the mapping
type KeyKind (m :: [k :-> Type]) = k
type KeyType m = Demote (KeyKind m)

newtype HMap (m :: [k :-> Type]) = HMap (Map (KeyType m) SomeVal)

-- shorthand for mappings with a "valid"/"usable" key
type HMapKey m = (SingKind (KeyKind m), SDecide (KeyKind m), Ord (KeyType m))
-- shorthand for dependent key/value pairs
type BindingFrom m = (Sigma (KeyKind m) (LookupIn m))

empty :: HMap m
empty = HMap Map.empty

singleton :: forall kt m . (HMapKey m, SingI kt) => Assoc kt m -> HMap m
singleton = HMap . Map.singleton (demote @kt) . SomeVal

singletonWithSing :: forall kt m . (HMapKey m) => Sing kt -> Assoc kt m -> HMap m
singletonWithSing Sing = singleton @kt

get :: forall kt m . (HMapKey m, SingI kt) => HMap m -> Maybe (Assoc kt m)
get (HMap m) = unsafeCoerceVal <$> Map.lookup (demote @kt) m

getWithSing :: forall kt m . (HMapKey m) => Sing kt -> HMap m -> Maybe (Assoc kt m)
getWithSing Sing = get @kt

set :: forall kt m . (HMapKey m, SingI kt) => Assoc kt m -> HMap m -> HMap m
set v (HMap m) = HMap $ Map.insert (demote @kt) (SomeVal v) m

setWithSing :: forall kt m . (HMapKey m) => Sing kt -> Assoc kt m -> HMap m -> HMap m
setWithSing Sing = set @kt

member :: forall {k} (kt :: k)  (m :: [k :-> Type]) . (HMapKey m, SingI kt) => HMap m -> Bool
member (HMap m) = Map.member (demote @kt) m

memberWithSing :: forall {k} (kt :: k) (m :: [k :-> Type]) . (HMapKey m) => Sing kt -> HMap m -> Bool
memberWithSing Sing = member @kt


-- | Map the given function over each key/Value in the hmap.
--
-- The result is a new Hmap.
map :: forall f m . (HMapKey m) => (forall (kt :: KeyKind m) . Sing kt -> Assoc kt m -> f @@ kt) -> HMap m -> HMap (MapWith f (Keys m))
map f (HMap m) = HMap $ Map.mapWithKey (\k v -> withSomeSing k (g v)) m
  where g :: forall (kt :: KeyKind m) . SomeVal -> Sing kt -> SomeVal
        g v s = SomeVal $ f s (unsafeCoerceVal @(Assoc kt m) v)

map' :: forall m a . (HMapKey m) => (forall (kt :: KeyKind m) . Sing kt -> Assoc kt m -> a) -> HMap m -> HMap (MapWith (Const a) (Keys m))
map' = map @(Const a)

-- | Map over the key/value pairs in the HMap and return a list of their results.
-- This function assumes that each key in the HMap maps to the same value (which is true for all 'Const' functions) such that 
-- the results can be collected into a single Haskell list.
mapList :: forall a m . HMapKey m => (forall (kt :: KeyKind m) . Sing kt -> Assoc kt m -> a) -> HMap m -> [a]
mapList f m1 = 
   let (HMap m) = map' f m1
   in -- safety: from applying `f` on all key-value pairs in m we know that 
      -- each key is mapped to `a`, therefore `SomeVal`s are actually a's 
      -- and can be safely coerced.
      List.map (unsafeCoerceVal . snd) $ Map.toList m

filter :: forall m . (HMapKey m) => (forall (kt :: KeyKind m) . Sing kt -> Assoc kt m -> Bool) -> HMap m -> HMap m
filter p (HMap m) = HMap $ Map.filterWithKey (\k v -> withSomeSing k (g v)) m
  where g :: forall (kt :: KeyKind m) . SomeVal -> Sing kt -> Bool
        g v s = p s (unsafeCoerceVal @(Assoc kt m) v)

foldr :: forall m b . (HMapKey m) => (forall (kt :: KeyKind m) . Sing kt -> Assoc kt m -> b -> b) -> b -> HMap m -> b
foldr f b (HMap m) = Map.foldrWithKey (\k v r -> withSomeSing k (g v r)) b m
  where g :: forall (kt :: KeyKind m) . SomeVal -> b -> Sing kt -> b
        g v r s = f s (unsafeCoerceVal @(Assoc kt m) v) r

-- TODO?: can be generalised for different mappings m1 and m2 (but might be confusing for a "union")
unionWith :: forall m . (HMapKey m) => (forall (kt :: KeyKind m) .  Sing kt -> Assoc kt m -> Assoc kt m -> Assoc kt m) -> HMap m -> HMap m -> HMap m
unionWith f (HMap m1) (HMap m2) = HMap $ Map.unionWithKey (\k v1 v2 -> withSomeSing k (g v1 v2)) m1 m2
  where g :: forall (kt :: KeyKind m) . SomeVal -> SomeVal -> Sing kt -> SomeVal
        g v1 v2 s = SomeVal $ f s (unsafeCoerceVal @(Assoc kt m) v1) (unsafeCoerceVal @(Assoc kt m) v2)

keys :: HMap m -> Set (KeyType m)
keys (HMap m) = Map.keysSet m

withKey :: HMapKey m => (forall (kt :: KeyKind m) . Sing kt -> r) -> KeyType m -> r
withKey f k = withSomeSing k f

size :: HMap m -> Int
size (HMap m) = Map.size m

null :: HMap m -> Bool
null (HMap m) = Map.null m

fromList :: forall m . HMapKey m => [BindingFrom m] -> HMap m
fromList = HMap . Map.fromList . Prelude.map sigmaToPair      -- equivalent to: Prelude.foldr (\(s :&: v) -> setWithSing s v) empty 
  where sigmaToPair :: BindingFrom m -> (KeyType m, SomeVal)
        sigmaToPair (s :&: v) = (fromSing s, SomeVal v)

toList :: forall m . HMapKey m => HMap m -> [BindingFrom m]
toList (HMap m) = Prelude.map (\(k, v) -> withKey (pairToSigma v) k) (Map.toList m)
  where pairToSigma :: forall (kt :: KeyKind m) . SomeVal -> Sing kt -> BindingFrom m
        pairToSigma v s = s :&: unsafeCoerceVal @(Assoc kt m) v

-- optionally, supporting these can be used for generic Eq/Semigroup/... instances
type family All k :: [k]
type ForAllOf k c = ForAllIn (All k) c
type family ForAllIn (t :: [k]) (c :: k ~> Constraint) :: Constraint where
  ForAllIn '[] c      = ()
  ForAllIn (k ': r) c = (c @@ k, ForAllIn r c)

-- | Provides evidence that constraint `c` is satisfied for a type `t` of kind `k`.
--
-- Useful for stating that some constraint should hold for all keys in a mapping of an HMap
-- (which are always of the same key kind). Mostly used in polymorphic contexts when `m` 
-- isn't known. Otherwise the constraint does not need to be stated explicitly as the instance
-- can be resolved from the concrete type.
-- 
-- Example: 
--
-- @
-- data MyKey = IntKey | StringKey deriving (Ord, Eq)
-- genHKeys ''MyKey
-- 
-- showMap :: forall (m :: [MyKey :-> Type]) . ForAll MyKey (AtKey1 Show m) => HMap m -> HMap (MapWith (Const String) (Keys m)) 
-- showMap = map  @(Const String) @m  showIt
--    where showIt :: forall (k :: MyKey) . Sing k -> Assoc k m -> String
--          showIt sing v = withDict (for @MyKey @(AtKey1 Show m) sing) (show v)
-- @
class ForAll k c where
  for :: forall (t :: k) . Sing t -> Dict (c @@ t)

-- convenience functions combining withDict and for
withC :: forall {k} c t r . (ForAll k c) => (c @@ t => Sing t -> r) -> Sing t -> r
withC f s = withDict (for @k @c s) (f s)

withC_ :: forall {k} c t r . (ForAll k c) => (c @@ t => r) -> Sing t -> r
withC_ f = withC @c (const f)

-- Eq instance
instance (HMapKey m, AllAtKey1 Eq m) => Eq (BindingFrom m) where 
  (k1 :&: v1) == (k2 :&: v2) = 
    case decideEquality k1 k2 of
      Just Refl -> withC_ @(AtKey1 Eq m) (v1 == v2) k1
      Nothing   -> False 
instance (HMapKey m, AllAtKey1 Eq m) => Eq (HMap m) where
  m1 == m2 = size m1 == size m2 && toList m1 == toList m2 

-- Ord instance
instance (HMapKey m, AllAtKey1 Eq m, AllAtKey1 Ord m) => Ord (BindingFrom m) where 
  (k1 :&: v1) `compare` (k2 :&: v2) = 
    case decideEquality k1 k2 of
      Just Refl -> withC_ @(AtKey1 Ord m) (compare v1 v2) k1
      Nothing   -> compare (fromSing k1) (fromSing k2)
instance (HMapKey m, AllAtKey1 Eq m, AllAtKey1 Ord m) => Ord (HMap m) where
  compare m1 m2 = compare (toList m1) (toList m2)


type ValueIsSemigroup m = AtKey1 Semigroup m
instance (HMapKey m, ForAll (KeyKind m) (ValueIsSemigroup m)) => Semigroup (HMap m) where
  (<>) = unionWith (withC_ @(ValueIsSemigroup m) (<>))

-- | Introduces trivial facts about the operations on the HMap which Haskell cannot figure out on its own.
type Facts f kt m = Assoc kt (MapWith f (Keys m)) ~ f @@ kt
withFacts :: forall f kt m r . (Facts f kt m => r) -> r
withFacts = withDict forced
   where forced :: Dict (Facts f kt m)
         forced = unsafeCoerce (Dict @())

-- example 

-- data MyKey = IntKey | BoolKey | StringKey
--   deriving (Eq, Ord)
-- 
-- genHKeys ''MyKey
-- 
-- type M = [IntKey ::-> Int, BoolKey ::-> Bool]
-- 
-- myHMap :: HMap [IntKey ::-> Int, BoolKey ::-> Bool]
-- myHMap = set @BoolKey True $ set @IntKey 42 empty
-- 
-- myHMap' :: HMap [IntKey ::-> Int, BoolKey ::-> Bool]
-- myHMap' = set @IntKey 42 empty
-- 
-- test = myHMap == myHMap'
-- 
-- myHMap'' = map @(Const String) (withC_ @(AtKey1 Show M) show) myHMap


data MyKey = IntKey | StringKey deriving (Ord, Eq)
type M = '[ IntKey ::-> Int, StringKey ::-> String ]
genHKeys ''MyKey

showMap :: HMap M -> HMap (MapWith (Const String) (Keys M)) 
showMap = map @(Const String) (withC_ @(AtKey1 Show M) show)
