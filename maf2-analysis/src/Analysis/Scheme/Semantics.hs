{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
module Analysis.Scheme.Semantics(eval, applyFun, applyClo, applyPrim) where

import Analysis.Monad hiding (eval, run, call)
import Analysis.Scheme.Monad
import Syntax.Scheme.AST
import Syntax.Ide
import Domain hiding (Exp)
import Control.Applicative (Applicative(liftA2))
import Analysis.Scheme.Primitives
import Control.Monad.Join
import Control.Monad (zipWithM_)
import Prelude hiding (exp, lex)
import Analysis.Monad.Fix
import Debug.Trace

eval :: forall m v . (SchemeDomain v, SchemeM m v) => Exp -> m v
eval = fix eval'

-- | Base level evaluation function
eval' ::  forall m v . (SchemeDomain v, SchemeM m v) => (Exp -> m v) -> Exp -> m v
eval' _ (Num n _)            = return (inject n)
eval' _ (Bln b _)            = return (inject b)
eval' _ (Nll _)              = return nil
eval' _ (Sym s _)            = return (symbol s)
eval' _ e@(Str s _)          = stoStr e (inject s)
eval' _ (Var (Ide nam _))    = lookupEnv nam >>= lookupAdr
eval' recur (Iff prd csq alt _)  = 
   cond (eval' recur prd) (eval' recur csq) (eval' recur alt)
eval' recur (Bgn sqq _)          = last <$> mapM (eval' recur) sqq
eval' _ e@(Lam {})           = curry injectClo e <$> getEnv -- TODO: restrict env based on fv
eval' recur (Let bds bdy _)      = evalLet recur bds bdy
eval' recur (Ltt bds bdy _)      = evalLetStar recur bds bdy
eval' recur (Ltr bds bdy _)      = evalLetRec recur bds bdy
eval' recur (Lrr bds bdy _)      = evalLetrecStar recur bds bdy
eval' recur e@(App op opr  _)    = evalApp recur e op opr
eval' _ (Debug msg)          = trace msg $ return nil
eval' _ e                    = error $ "Unrecognized expression" ++ show e

evalLet :: (SchemeDomain v, SchemeM m v) => (Exp -> m v) -> [(Ide, Exp)] -> Exp -> m v
evalLet recur bds bdy = do
   vlus <- mapM (eval' recur) eps
   adrs <- mapM allocVar vrs
   mapM_ (uncurry writeAdr) (zip adrs vlus)
   withExtendedEnv (zip (map name vrs) adrs) (eval' recur bdy)
   where (vrs, eps) = unzip bds

evalLetStar :: (SchemeDomain v, SchemeM m v) => (Exp -> m v) -> [(Ide, Exp)] -> Exp -> m v
evalLetStar recur [] bdy = eval' recur bdy
evalLetStar recur ((nam, ex) : rst) bdy = do
   vlu <- eval' recur ex
   adr <- allocVar nam
   _   <- writeAdr adr vlu
   withExtendedEnv [(name nam, adr)] (evalLetStar recur rst bdy)

evalLetRec :: forall v m . (SchemeDomain v, SchemeM m v) => (Exp -> m v) -> [(Ide, Exp)] -> Exp -> m v
evalLetRec recur bds bdy = do
   adrs  <- mapM allocVar vrs
   -- mapM_ (uncurry writeAdr . (, unsp)) adrs
   withExtendedEnv (zip (map name vrs) adrs) $ do
      vlus <- mapM (eval' recur) eps
      mapM_ (uncurry writeAdr) (zip adrs vlus)
      eval' recur bdy
   where (vrs, eps) = unzip bds

evalLetrecStar :: forall v m . (SchemeDomain v, SchemeM m v) => (Exp -> m v) -> [(Ide, Exp)] -> Exp -> m v
evalLetrecStar recur bds bdy = do
   adrs  <- mapM (allocVar . fst) bds
   -- mapM_ (uncurry writeAdr . (, unsp)) adrs
   withExtendedEnv (zip (map (name . fst) bds) adrs) $ do
      -- difference with `letrec` is that the store is updated after each binding evaluation
      mapM_ (uncurry evalBinding) (zip adrs bds)
      eval' recur bdy
   where evalBinding adr (_, exp) = do
            vlu <- eval' recur exp
            writeAdr @_ @_ @v adr vlu
            return vlu

evalApp :: (SchemeDomain v, SchemeM m v) => (Exp -> m v) -> Exp -> Exp -> [Exp] -> m v
evalApp recur app op opr =
   uncurry (applyFun recur app) =<< liftA2 (,) (eval' recur op) (mapM (eval' recur) opr)

applyFun :: (SchemeDomain v, SchemeM m v) => (Exp -> m v) -> Exp -> v -> [v] -> m v
applyFun recur app op ags = withProc (either (applyPrim app ags) (\clo -> applyClo recur clo ags)) op

applyClo :: (SchemeDomain v, SchemeM m v) => (Exp -> m v) -> (Exp, Env v) -> [v] -> m v
applyClo recur (Lam prs bdy _, lex) ags = do
   withEnv (const lex) $ do
      adrs <- mapM allocVar prs
      zipWithM_ writeAdr adrs ags
      withExtendedEnv (zip (map name prs) adrs) $ recur bdy
applyClo _ _ _ = error "invalid closure"

applyPrim :: (SchemeDomain v, SchemeM m v) => Exp -> [v] -> String -> m v
applyPrim f ags nam = run (primitive nam) f ags
