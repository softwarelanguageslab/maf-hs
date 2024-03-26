module Analysis.Symbolic.Semantics(eval) where

import Syntax.Scheme
import qualified Analysis.Actors.Semantics as Actors
import Domain.Symbolic
import qualified Analysis.Monad as Monad
import qualified Analysis.Scheme.Semantics as Scheme
import Analysis.Symbolic.Monad (SymbolicM, choice)
import Analysis.Actors.Monad
import Control.Monad.State.IntPool
import Control.Applicative (liftA2)


eval :: (ActorEvalM m v msg mb, SymbolicM m v) => Exp -> m v
eval (App (Var (Ide "fresh" _)) [e] _) = 
   var <$> fresh <*> eval e
eval (App (Var (Ide "fresh" _)) _ _) =
   error "invalid call to fresh"
eval e@(App op opr  _)    = evalApp e op opr
eval (Iff cnd csq alt _)  = 
   choice (eval cnd) (eval csq) (eval alt)
eval e = Actors.eval e

evalApp :: SymbolicM m v => Exp -> Exp -> [Exp] -> m v
evalApp app op opr =
   uncurry (applyFun app) =<< liftA2 (,) (Monad.eval op) (mapM Monad.eval opr)

applyFun :: SymbolicM m v => Exp -> v -> [v] -> m v
applyFun app op ags = ap op ags <$> Scheme.applyFun app op ags
