module Analysis.Monad.Eval (
    EvalM(..)
) where

class (Monad m) => EvalM m v e  | m -> v where
   eval :: e -> m v

