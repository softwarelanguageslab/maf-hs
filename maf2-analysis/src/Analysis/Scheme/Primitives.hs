{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Analysis.Scheme.Primitives(Prim(..), allPrimitives, primitive, primitivesByName, initialEnv, initialSto) where

import Data.Functor
import Analysis.Environment
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Analysis.Monad
import Analysis.Scheme.Monad
import Domain hiding (Exp, empty)
import Control.Monad.Join
import Control.Monad ((>=>))
import Syntax.Scheme.AST
import Control.Monad.DomainError

data Prim v = Prim { primName :: String, run :: forall m . PrimM m v => Exp -> [v] -> m v }

type PrimM m v = (MonadEscape m, Domain (Esc m) DomainError, MonadJoin m, SchemeDomain v, SchemeM m v)

-- | No arguments
fix0 :: String -> (forall m . PrimM m v => m v) -> Prim v
fix0 nam f = Prim nam (\_ [] -> f)

-- | Unary primitives 
efix1 :: String -> (forall m . PrimM m v => Exp -> v -> m v) -> Prim v
efix1 nam f = Prim nam (\ex [x1] -> f ex x1)
-- | Binary primitives
efix2 :: String -> (forall m . PrimM m v => Exp -> v -> v -> m v) -> Prim v
efix2 nam f = Prim nam (\ex [x1, x2] -> f ex x1 x2)
-- | Ternary primitives
efix3 ::  String -> (forall m . PrimM m v => Exp -> v -> v -> v -> m v) -> Prim v
efix3 nam f = Prim nam (\ex [x1, x2, x3] -> f ex x1 x2 x3)

-- | Heap unrelated operations 
fix1 :: String -> (forall m . PrimM m v => v -> m v) -> Prim v
fix1 nam f = efix1 nam (\_ v-> f v)
fix2 :: String -> (forall m . PrimM m v => v -> v -> m v) -> Prim v
fix2 nam f = efix2 nam (\_ v1 v2 -> f v1 v2)
fix3 :: String -> (forall m . PrimM m v => v -> v -> v -> m v) -> Prim v
fix3 nam f = efix3 nam (\_ v1 v2 v3 -> f v1 v2 v3)

allPrimitives :: [Prim v]
allPrimitives = [
   fix2 "modulo" modulo,
   fix2 "*" times, -- todo: vararg 
   fix2 "+" plus, -- todo: vararg
   fix2 "-" minus, -- todo: vararg
   fix2 "/" Domain.div, -- todo: vararg
   fix1 "acos" Domain.acos,
   fix1 "atan" Domain.atan,
   -- fix1 "boolean?" (return . isBool),
   fix1 "true?" (return . inject . isTrue),
   fix1 "false?" (return . inject . isFalse),
   fix1 "car" (pptrs >=> deref (const (return . car))),
   fix1 "cdr" (pptrs >=> deref (const (return . cdr))),
   fix1 "ceiling" Domain.ceiling,
   -- fix1 "char->integer" todo,
   fix2 "char-ci<?" charLtCI,
   fix2 "char-ci=?" charEqCI,
   fix1 "char-downcase" Domain.downcase,
   fix1 "char-upcase" Domain.upcase,
   fix1 "char-lower-case?" Domain.isLower,
   fix1 "char-upper-case?" Domain.isUpper,
   fix2 "char<?" charLt,
   fix2 "char=?" charEq,
   efix2 "cons" (\ex a b -> stoPai ex (cons a b)),
   -- fix2 "eq?" todo 
   fix2 "expt" expt,
   fix1 "floor" Domain.floor,
   -- fix1 "integer->char" todo, 
   fix1 "log" Domain.log,
   fix1 "null?" (return . inject . isNil),
   -- fix1 "number->string" todo, 
   -- fix2 "make-string" todo, 
   fix1 "number?" (\v -> return $ inject (isReal v || isInteger v)),
   fix1 "pair?" (return . inject . isPaiPtr),
   -- fix1 "procedure?" todo, 
   fix2 "quotient" quotient,
   fix1 "real?" (return . inject . isReal),
   fix2 "remainder" remainder,
   fix1 "round" Domain.round,
   fix2 "set-car!" (\adr v -> pptrs adr >>= deref (\adr pai ->
      updateAdr adr (cons v (cdr pai)) $> unsp)),
   fix2 "set-cdr!" (\adr v -> pptrs adr >>= deref (\adr pai ->
      updateAdr adr (cons (car pai) v) $> unsp)),
   fix1 "sin" Domain.sin,
   fix1 "sqrt" Domain.sqrt,
   -- fix1 "string->number" todo, 
   -- fix1 "string->symbol" todo, 
   efix2 "string-append"
      (\ex s1 s2 -> do
         adrs1 <- sptrs s1
         adrs2 <- sptrs s2
         result <- deref (\_ str1 -> deref (const (append str1)) adrs2) adrs1
         stoStr ex result),
   fix1 "string-length"
      (sptrs >=> deref (const Domain.length)),
   fix2 "string-ref"
      (\s i -> sptrs s >>= deref (const (`ref` i))),
   fix3 "string-set!"
      (\s i c -> sptrs s >>= deref (\adr str -> updateAdr adr =<< set str i c) >> return unsp),
   -- fix2 "string<?" todo,
   fix1 "string?" (return . inject . isStrPtr),
   -- fix2 "substring" todo, 
   -- fix1 "symbol->string" todo, 
   -- fix1 "symbol?" todo, 
   fix1 "tan" Domain.tan,
   -- fix2 "make-vector" todo,
   -- varg "vector" todo, 
   fix1 "vector-length" (vptrs >=> deref (const (return . vectorLength))),
   fix2 "vector-ref" (\adr i -> vptrs adr >>= deref (const (`vectorRef` i))),
   fix3 "vector-set!" (\adr i v -> vptrs adr >>= deref (\adr vec -> updateAdr adr =<< vectorSet vec i v) >> return unsp),
   fix1 "vector?" $ return . inject . isVecPtr,
   fix2 "<" lt,
   fix2 "=" eq,
   fix1 "random" Domain.random,
   fix0 "bool-top" $ return Domain.boolTop,
   fix1 "not" $ return . Domain.not
   -- fix1 "error" todo
   ]

primitivesByName :: Map String (Prim v)
primitivesByName = Map.fromList $ map (\p -> (primName p, p)) allPrimitives

primitive :: String -> Prim v
primitive = fromJust . flip Map.lookup primitivesByName

primitiveNames :: [String]
primitiveNames = Map.keys primitivesByName

-- | Returs an initial environment for the primitives
initialEnv :: forall e adr . (Environment e adr) => (String -> adr) -> e
initialEnv alloc =
   let names = primitiveNames
   in extends (zip names (map alloc names)) empty

-- | Return an initial store for all the primitives
initialSto :: forall v e adr . (SchemeDomain v, Environment e adr, Ord adr) => e -> Map adr v
initialSto e =
   foldr (\nam -> Map.insert (Analysis.Environment.lookup nam e) (prim nam)) Map.empty primitiveNames
