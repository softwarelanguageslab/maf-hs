{-# LANGUAGE FlexibleContexts, UndecidableInstances, FlexibleInstances, ConstraintKinds #-}
module Analysis.Scheme.Monad(SchemeM, SchemeM',SchemeDomainM,  allocPai, allocVec, allocStr, allocVar, stoPai, stoStr) where

import Data.Functor
import Syntax.Scheme.AST
import Domain hiding (Exp)
import qualified Domain.Scheme as S
import Analysis.Monad
import Control.Monad.Join
import Control.Monad.DomainError
import Control.Monad.Escape
import Syntax.Ide
import Lattice.Equal
import Analysis.Monad.Fix (MonadFixpoint)

stoPai :: SchemeDomainM e v m => e -> PaiDom v -> m v
stoPai ex v = allocPai ex >>= (\adr -> writeAdr adr v $> pptr adr)
stoStr :: SchemeDomainM e v m => e -> StrDom v -> m v
stoStr ex v = allocStr ex >>= (\adr -> writeAdr adr v $> sptr adr)

-- | Minimal Scheme monad for expressing primitive operations.
-- It is more general than SchemeM since it does not impose any
-- constraints on calling semantics, evaluation semantics and expression representations.
type SchemeDomainM e v m = (
   -- Domain
   SchemeValue v,
   MonadJoin m,
   MonadEscape m,
   Domain (Esc m) DomainError,
   VarDom v ~ v,
   Boo v    ~ v,
   EqualLattice v,
   -- Store interactions
   StoreM m (PAdr v) (PaiDom v),
   StoreM m (Adr v)  (VarDom v),
   StoreM m (VAdr v) (VecDom v),
   StoreM m (SAdr v) (StrDom v),
   -- Allocation
   AllocM m Ide (Adr v),   -- variable allocation
   AllocM m e (PAdr v),    -- pair allocation
   AllocM m e (VAdr v),    -- vector allocation
   AllocM m e (SAdr v),    -- string allocation
   S.Exp v ~ e
   )

-- | Scheme analysis monad (without open recursion on @eval@) 
type SchemeM' m v = (
   SchemeDomainM Exp v m,
   -- Environment
   EnvM m (Adr v) (Env v))

-- | Full Scheme analysis monad
type SchemeM m v = (SchemeM' m v, MonadFixpoint m Exp v)

allocPai :: SchemeDomainM e v m => e -> m (PAdr v)
allocPai = alloc
allocVec :: SchemeDomainM e v m => e -> m (VAdr v)
allocVec = alloc
allocStr :: SchemeDomainM e v m => e -> m (SAdr v)
allocStr = alloc
allocVar :: SchemeDomainM e v m => Ide -> m (Adr v)
allocVar = alloc
