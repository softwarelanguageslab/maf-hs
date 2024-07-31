{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- | Serves as an extension (i.e., it requires) for the semantics in 'Analysis.Scheme.Semantics'.
-- It adds support for the analysis of programs contain actor-based concurrency. 
module Analysis.Actors.Semantics(Analysis.Actors.Semantics.eval) where

import Domain.Scheme hiding (Exp)
import Analysis.Monad
import Data.Functor ((<&>), ($>))
import Syntax.Scheme.AST
import qualified Analysis.Scheme.Semantics as Base
import Analysis.Actors.Monad
import qualified Analysis.Monad as Monad
import Control.Monad.Join
import Lattice.Class (Lattice, bottom)
import Domain.Scheme.Actors.Message
import Domain (BoolDomain(isTrue))

eval :: ActorEvalM m v msg mb => Exp -> m v
eval (Spw beh args _) = initBehavior beh args spawn
eval (Bec beh args _) = initBehavior beh args become $> unsp
eval (Sen rcpt tag args _) = do
   message  <- message tag <$> mapM Monad.eval args
   Monad.eval rcpt >>= arefs (! message)
   return unsp
eval (Rcv hdls _) = do
   receive (`selectHandler` hdls)

eval e@(Beh {}) = getEnv <&> curry beh e
eval e@(Mir {}) = getEnv <&> curry beh e
eval e@(Sel _)  = aref <$> self
eval (Ter _)    = return bottom -- no behavior in the abstract
eval e = Base.eval e

-- | Initialize the behavior in the first argument with the arguments in the second 
-- then run the function in the third argument on the expression of the behavior
initBehavior :: (Lattice a, ActorEvalM m v msg mb) => Exp -> [Exp] -> (Exp -> m a) -> m a
initBehavior beh args run =
   Monad.eval beh >>= withBehs (\(Beh prs bdy _, env) -> do
      vlus <- mapM Monad.eval args
      adrs <- mapM allocVar prs
      mapM_ (uncurry writeAdr) (zip adrs vlus)
      withEnv (const env) $ withExtendedEnv (zip (map name prs) adrs) $ run bdy)


-- | Select a handler from the given list of handlers that matches the given message
selectHandler :: ActorEvalM m v msg mb => msg -> [Hdl] -> m v
selectHandler msg = mjoins . map runHandler . filter (isTrue . flip matchesTagCP msg . nameOf)
   where nameOf (Hdl (Ide nam _) _ _) = nam
         runHandler (Hdl _ prs bdy) = do
            adrs <- mapM allocVar prs
            mapM_ (uncurry writeAdr) (zip adrs (payload msg))
            withExtendedEnv (zip (map name prs) adrs) $ Monad.eval bdy
