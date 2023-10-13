{-# LANGUAGE RankNTypes, FlexibleContexts #-}
-- | This module contains OO-specific semantics
--
-- Conceptually, the semantics for attribute lookup
-- are given by the __getattribute__ metamethod of the `object` 
-- class (which every class inherits from). However
-- since meta-methods also rely on attribute-lookup, attribute-lookup
-- is implemented as a primitive instead.
--
--
module Analysis.Python.OO where

-- import Domain
-- import Domain.Python as PyDomain hiding (void)
-- import Domain.Python.ClassDomain
-- import Analysis.Python.Monad
-- import Data.Functor ((<&>))
-- 
-- import Control.Monad.Error hiding (raiseError)
-- import Control.Monad (void)
-- import Control.Monad.Join
-- import Control.Monad.Cond
-- 
-- import Syntax.Python
-- 
-- todo = error . ("todo: "++)
-- 
-- -- | An alias for convience
-- type PyObj m = Vlu (PyObjAdr m)
-- 
-- -------------------------------------------------------------------------------------------------
-- -- Attribute lookup/modification primitives
-- -------------------------------------------------------------------------------------------------
-- 
-- boundedMethod :: PyDomain v o a => Vlu o -> v -> v
-- boundedMethod = undefined
-- 
-- -- | Lookup an attribute, starting from the given 
-- -- instance. If the attribute is not found in the given 
-- -- instance then it is looked up in its class (i.e. getClass obj) 
-- -- where a lookup is performed in its linearized superclass chain.
-- --
-- -- Functions found in the class of an object are converted to 
-- -- bounded methods. A bounded method is a partially applied function
-- -- where the first argument is bound to the current receiver.
-- --
-- -- Hence, the analysis becomes `receiver` sensitive!
-- lookupAttribute :: forall a v m . (OVlu (PyObj m) ~ v, PyM m v a)
--                 => PyObj m             -- ^ Lookup the attribute in the given object
--                 -> OKey (PyObj m)      -- ^ The key that decribes the attribute
--                 -> m v                 -- ^ Returns a value from the Python domain
-- lookupAttribute obj attr =
--         pure (lookupLocal attr obj)                             -- try lookup in the object itself
--    <|> (deref (const $ lookupInClass attr) (getClass obj) <&>   -- if it is not there, look it up in the class
--               boundedMethod obj)
-- 
-- -- | Lookup the attribute in the given class or its superclasses
-- lookupInClass :: (OVlu (PyObj m) ~ v, PyM m v a)
--               => OKey (PyObj m)
--               -> PyObj m
--               -> m v
-- lookupInClass attr obj =
--    -- for every element on the path in the linearisation order 
--    -- we look for the requested attribute in its local map
--    --
--    -- note: foldl1 is safe since every class has at least one class on its path 
--    -- to the superclass: itself.
--    let path = map (deref (const $ return . lookupLocal attr)) (mro obj)
--    in reraiseError (const $ DomainError "AttributeError") $ foldr1 (<|>) path
-- 
-- -- | Checks whether the given object contains the given attribute
-- hasAttribute :: forall m v a . (OVlu (PyObj m) ~ v, PyM m v a)
--              => OKey (PyObj m)
--              -> PyObj m
--              -> m Bool
-- hasAttribute k obj = do
--    notContains <- hasError (void (lookupAttribute @a obj k))
--    return $ Prelude.not notContains
-- 
-- -------------------------------------------------------------------------------------------------
-- -- Utility functions
-- ------------------------------------------------------------------------------------------------
-- 
-- -- | Apply the given method (can be either a primitive or a user-defined function) 
-- -- use the given object (the receiver) as the first argument of that function
-- applyMethod :: PyM m v a => v -> Either String (Fun a (Env v)) -> m v
-- applyMethod = undefined
-- 
-- -------------------------------------------------------------------------------------------------
-- -- Python built-in meta-functions
-- ------------------------------------------------------------------------------------------------
-- 
-- -- | Implementation of `getattr`
-- pyGetAttr :: (OVlu (PyObj m) ~ v, PyM m v a)
--           => PyObj m             -- ^ the object to find the attribute in
--           -> OKey (PyObj m)      -- ^ the key attribute to look for
--           -> m v                 -- ^ the result is a PyVlu
-- pyGetAttr o k =
--    deref (\adr cls ->
--       condM
--          [(hasAttribute (inject "__getattr__") cls, do
--              -- lookup the attribute 
--              vlu <- lookupInClass (inject "__getattr__") cls
--              -- apply the value of the attribute if it is a function
--              withProc (applyMethod (PyDomain.object adr)) vlu),
--           (otherwiseM, lookupAttribute o k <|> attributeNotFound cls adr)]) (getClass o)
--    where attributeNotFound cls adrₒ =
--             -- if the attribute is not found, then call __getattribute__ if available
--             condM
--                [(hasAttribute (inject "__getattribute__") cls,
--                   lookupInClass (inject "__getattribute__") cls >>= withProc (applyMethod (PyDomain.object adrₒ))),
--                 (otherwiseM, raiseError (DomainError "AttributeError"))]
-- 
-- 
-- 
-- -- | Implementation of `setattr` 
-- pySetAttr :: (OVlu (PyObj m) ~ v, PyM m v a)
--           => OKey (PyObj m)
--           -> v
--           -> PyObj m
--           -> m ()
-- pySetAttr = undefined
-- 
-- -- | Implementation of `hasattr` 
-- pyHasAttr :: PyM m v a
--           => OKey (PyObj m)
--           -> PyObj m
--           -> m v
-- pyHasAttr = undefined
-- 
-- -- | A method has a name and an associated function to execute 
-- -- upon it.
-- data Method v a = Method {
--    methodName :: String,
--    -- | Runs the method with the given call-site expression (first-argument)
--    -- and a lisf of values (second argument).
--    --
--    -- The method can be run in any monadic context that supports the `PyM` operations.
--    runMethod :: forall m . PyM m v a => Exp a Micro -> [v] -> m v
-- }
-- 
-- -------------------------------------------------------------------------------------------------
-- -- Python built-in meta-objects
-- -------------------------------------------------------------------------------------------------
-- 
-- -- | Construct a new object, to be injected in the 
-- -- abstract object domain
-- newtype Object v a = Object [Method v a]
-- 
-- -- | Creates a method
-- method :: String -> (forall m . PyM m v a => Exp a Micro -> [v] -> m v) -> Method v a
-- method = Method
-- 
-- -- | The `type` object
-- objType = Object [
--    method "__new__"
--       -- note: the fourth argument is used to initialize the class
--       -- with a set of attributes, however we do not support this at the moment
--       (\callsite [self, nam, supercls, _] -> todo "__new__")]
