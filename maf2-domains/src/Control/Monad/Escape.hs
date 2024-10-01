{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies, AllowAmbiguousTypes, DeriveFunctor #-}

module Control.Monad.Escape (
   MonadEscape(..), 
   MayEscape(Escape, Value, MayBoth), 
   MayEscapeT(..),
   escape, 
   orElse, 
   try,
   catchOn,
   fromValue
) where

import Lattice.Class hiding (Bottom)
import Domain.Class 

import Control.Monad.Join
import Data.Kind (Type)
import Control.Monad (ap)
import Control.Monad.Trans
import Control.Monad.Lift.Class (MonadTransControl(..))
import Data.Functor ((<&>))
import Control.Monad.Identity (IdentityT (..))
import Lattice.Split (SplitLattice)
import Lattice.ConstantPropagationLattice (CP)

-- | Monad to handle errors in the abstract domain
class MonadEscape m where
   type Esc m
   throw :: (BottomLattice a) => Esc m -> m a 
   catch :: (BottomLattice a, Joinable a) => m a -> (Esc m -> m a) -> m a

catchOn :: (MonadEscape m, MonadJoin m, SplitLattice (Esc m), Lattice (Esc m), Lattice a) => m a -> (Esc m -> CP Bool, Esc m -> m a) -> m a
catchOn bdy (prd, hdl) = bdy `catch` msplitOn (return . prd) hdl throw 

escape :: (MonadEscape m, Domain (Esc m) e, BottomLattice a) => e -> m a 
escape = throw . inject 

orElse :: (MonadEscape m, BottomLattice a, Joinable a) => m a -> m a -> m a
orElse a = catch a . const  

try :: (MonadEscape m, BottomLattice a, Joinable a) => [m a] -> m a -> m a
try = flip $ foldr orElse 

------------------------------------------------------------
--- MayEscape
------------------------------------------------------------

data MayEscape e v = Bottom 
                   | Escape e 
                   | Value v
                   | MayBoth v e
   deriving (Eq, Ord, Functor, Show)

-- | Retrieve the value from `MayEscape`, or 
-- return the default if there is only an error 
-- and no value.
fromValue :: a -> MayEscape e a -> a 
fromValue _ (Value v) = v 
fromValue _ (MayBoth v _) = v 
fromValue v _ = v 

instance Foldable (MayEscape e) where 
   foldMap _ Bottom = mempty
   foldMap _ (Escape _) = mempty
   foldMap f (Value v) = f v
   foldMap f (MayBoth v _) = f v 

instance Traversable (MayEscape e) where
   traverse _ Bottom        = pure Bottom
   traverse _ (Escape e)    = pure (Escape e)  
   traverse f (Value v)     = Value <$> f v
   traverse f (MayBoth v e) = flip MayBoth e <$> f v   

addError :: Joinable e => e -> MayEscape e a -> MayEscape e a
addError e Bottom          = Escape e 
addError e1 (Escape e2)    = Escape (e1 `join` e2)
addError e (Value v)       = MayBoth v e
addError e1 (MayBoth v e2) = MayBoth v (e1 `join` e2) 

instance (Joinable e, Joinable a) => Joinable (MayEscape e a) where
   join Bottom mf                         = mf
   join mf Bottom                         = mf
   join (Escape e1) (Escape e2)           = Escape (e1 `join` e2)
   join (Escape e) (Value v)              = MayBoth v e
   join (Escape e1) (MayBoth v e2)        = MayBoth v (e1 `join` e2)
   join (Value v) (Escape e)              = MayBoth v e
   join (Value v1) (Value v2)             = Value (v1 `join` v2)
   join (Value v1) (MayBoth v2 e)         = MayBoth (v1 `join` v2) e 
   join (MayBoth v e1) (Escape e2)        = MayBoth v (e1 `join` e2)
   join (MayBoth v1 e) (Value v2)         = MayBoth (v1 `join` v2) e
   join (MayBoth v1 e1) (MayBoth v2 e2)   = MayBoth (v1 `join` v2) (e1 `join` e2)

instance BottomLattice (MayEscape e a) where
   bottom = Bottom

------------------------------------------------------------
--- MayEscapeT
------------------------------------------------------------

newtype MayEscapeT e (m :: Type -> Type) a = MayEscapeT { runMayEscape :: m (MayEscape e a) }

instance (Monad m) => Functor (MayEscapeT e m) where
   fmap f (MayEscapeT ma) = MayEscapeT $ fmap (fmap f) ma  

instance (Monad m, Joinable e) => Applicative (MayEscapeT e m) where
   pure = MayEscapeT . return . Value
   (<*>) = ap

instance (Monad m, Joinable e) => Monad (MayEscapeT e m) where
   return = pure
   MayEscapeT m >>= f = MayEscapeT $ m >>= \case 
                                             Bottom      -> return Bottom
                                             Escape e    -> return $ Escape e
                                             Value v     -> runMayEscape (f v) 
                                             MayBoth v e -> runMayEscape (f v) <&> addError e

instance (MonadJoin m, Joinable e) => MonadEscape (MayEscapeT e m) where
   type Esc (MayEscapeT e m) = e 
   throw = MayEscapeT . return . Escape
   catch (MayEscapeT m) hdl = MayEscapeT $ m >>= handle
      where handle Bottom        = return Bottom
            handle (Escape e)    = runMayEscape (hdl e)
            handle suc@(Value _) = return suc
            handle (MayBoth v e) = handle (Value v) `mjoin` handle (Escape e)

instance (Eq e, Joinable e, MonadJoin m) => MonadJoin (MayEscapeT e m) where
    mzero = MayEscapeT mzero
    mjoin (MayEscapeT ma) (MayEscapeT mb) = MayEscapeT (mjoin ma mb)

instance MonadTrans (MayEscapeT e) where  
   lift = MayEscapeT . fmap Value

instance (Joinable e) => MonadTransControl (MayEscapeT e) where  
   type LayerState (MayEscapeT e)  = () 
   type LayerResult (MayEscapeT e) = MayEscape e
   
   suspend (MayEscapeT m) () = fmap (,()) m
   resume (a, _) = MayEscapeT $ return a 
   capture = return ()

   extract = error "unsupported operation: extract"
   -- TODO: this does not seem quite right in the `MayBoth`
   -- so we use  `error` as the default implementation
   -- to check whether it is actually used.
   -- extract _ (Escape e) = Left (Escape e)
   -- extract _ (Value a) = Right a
   -- extract _ (MayBoth a e) = Right a
   -- extract _ Bottom = Left Bottom


------------------------------------------------------------
-- Trivial instances
------------------------------------------------------------

instance (MonadEscape m) => MonadEscape (IdentityT m) where  
   type Esc (IdentityT m) = Esc m 
   throw = IdentityT . throw 
   catch ma f = IdentityT $ runIdentityT ma `catch` (runIdentityT . f)

instance MonadEscape Maybe where 
   type Esc Maybe = ()
   throw = const Nothing
   -- NOTE: this is an overapproximation,
   -- we assume that an error can always occur in the `ma` 
   -- value because `mjoin Nothing (Just a)` = Just a
   -- and the error information is lost. Therefore
   -- we always have to pretend that a `Just a` 
   -- value could also have been an error
   catch ma f = mjoin ma (f ())

