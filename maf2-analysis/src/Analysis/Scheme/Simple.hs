{-# LANGUAGE FlexibleInstances #-}
-- | Simple ModF based Scheme analyses
-- based on the constant propagation domain
module Analysis.Scheme.Simple where

import Syntax.Scheme
import Analysis.Scheme
import Domain.Scheme hiding (Exp)
import GHC.Generics
import Data.Maybe
import Text.Printf
import Data.Print
import Prelude hiding (exp)

-- | Type of pointer address
data PointerAdr  = PointerAdr Exp [Exp]
                 deriving (Eq, Ord, Generic, Show)
data VariableAdr = Adr Ide [Exp]
                 | Prm String
                 | Ret (Component VariableAdr V K)
                 deriving (Eq, Ord, Generic)

instance Show VariableAdr where
   show (Adr ide ctx) = printf "Adr(%s, %s)" (show ide) (show ctx)
   show (Prm nam)     = printf "Prm(%s)" nam
   show (Ret cmp)     = printf "Ret(%s)" (printShort cmp) 

instance VarAdr VariableAdr V K where
   retAdr = Ret
   prmAdr = Prm

-- | Alias for k-sensitivity context
type K = [Exp]

-- | Alias for values
type V = (CPValue PointerAdr VariableAdr Exp)

-- | Type of dependency 
data AdrDep = VarAdrDep VariableAdr
            | PtrDep PointerAdr
            deriving (Eq, Ord, Generic)

-- | AdrDep is indeed a `Dependency` (i.e. it satisfies
-- the `Dependency` typeclass)
instance Dependency PointerAdr AdrDep where
   dep = PtrDep
instance Dependency VariableAdr AdrDep where
   dep = VarAdrDep

instance Address VariableAdr
instance Address PointerAdr

-- | The allocator
instance SchemeAlloc K VariableAdr PointerAdr PointerAdr PointerAdr where
   allocVar = Adr
   allocCtx = const id
   allocPai = PointerAdr
   allocVec = PointerAdr
   allocStr = PointerAdr

-- | Expose a function to run the analysis
runAnalysis :: String -> DSto K V
runAnalysis program = analyzeProgram @V exp [] []
   where exp = fromJust $ parseString program
