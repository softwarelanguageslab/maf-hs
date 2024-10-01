{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Main (main) where

import Lib
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Text.Printf
import qualified Data.TypeLevel.HMap as HMap
import Domain.Lambda
import Data.Singletons.Sigma
import Analysis.Scheme.Prelude (MayEscape)
import Control.Monad.DomainError (DomainError)
import Data.Set (Set)
import Control.Monad.Escape (MayEscape(..), fromValue)
import Data.Functor.Identity

printSto :: (Show adr, Functor f) => (forall a . a -> f a -> a) -> Map adr (f V) -> String
printSto f m =
   intercalate "\n" (map (\(k,v) -> printf "%*s | %s" indent (show k) (f "⊥" $ fmap showIt v)) adrs) ++ "\n----\n"
   where adrs   = Map.toList m
         indent = maximum (map (length . show . fst) adrs) + 5

showIt :: V -> String
showIt (LamVal hm) = intercalate "," (map select (HMap.toList hm))
   where select :: HMap.BindingFrom (M Adr) -> String
         select (SIntKey :&: i) = "int ↦ " ++ show i
         select (SCloKey :&: s) = "clo ↦ " ++ "λ"

program =
   App (Lam (Ident "x" 1) (Inc (Var (Ident "x" 2))))
       (Num 0)

main :: IO ()
main = do
   putStrLn $ printSto fromValue res
   putStrLn "=========="
   putStrLn $ printSto (const $ runIdentity) (fmap Identity sto)
   where (sto, res) = analyze program
