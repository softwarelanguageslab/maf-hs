{-# LANGUAGE FlexibleInstances #-}
-- | Simple ModF based Scheme analyses
-- based on the constant propagation domain
module Analysis.Scheme.Simple(VariableAdr, V, runAnalysis) where

import Syntax.Scheme
import Analysis.Scheme
import Domain.Scheme.Store
import Domain.Scheme hiding (Exp)
import Data.Maybe
import Prelude hiding (exp)

-- | Alias for values
type V = (CPValue (PaiAdr K) (VecAdr K) (StrAdr K) (EnvAdr K) Exp)

-- | Alias for address to variables
type VariableAdr = EnvAdr K

-- | Expose a function to run the analysis
runAnalysis :: String -> AnlRes V
runAnalysis program = analyze exp
   where exp = fromJust $ parseString program
