{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- | The Z3 solver
module Solver.Z3(
    Z3Solver, 
    Z3SolverState, 
    runZ3Solver, 
    runZ3SolverWithSetup, 
    runZ3SolverBackground, 
    runZ3SolverWithPrelude,
    execInZ3State
    )  where

import System.Process
import System.IO
import Text.Printf
import Control.Monad.State
import Control.Monad ((>=>), when)
import Data.Maybe

import Solver
import Symbolic.SMT
import Symbolic.AST
import qualified Syntax.Scheme.Parser as SExp
import Data.Functor
import qualified Data.Map as Map
import qualified Data.Set as Set

data Z3SolverState = Z3SolverState {
   -- | Buffer of output from Z3
   buffer :: [String],
   -- | Cached setup-code
   setupCode :: String,
   -- | The (stdin, write) input handle 
   inputHandle   :: Handle,
   -- | The (std out, read) output handle 
   outputHandle  :: Handle,
   -- | Handle to the spawned process of Z3
   processHandle :: ProcessHandle
}

-- | Spawns the Z3 process assumes the Z3 binary is on the path.
-- 
-- The result is the Z3SolverState that can be injected into 
-- the state monad.
spawnZ3 :: String -- ^ setup SMTLib code
        -> IO Z3SolverState
spawnZ3 setupCode' = do
   (Just hin, Just hout, _, handle) <- createProcess (proc "z3" ["-in"]) { std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit }
   return Z3SolverState {
      buffer = [],
      setupCode = setupCode',
      inputHandle = hin,
      outputHandle = hout,
      processHandle = handle
   }


-- | A monad in which the Z3 solver can be executed.
--
-- To this end, the monad keeps track of an input and output 
-- stream handle that is connected to a running Z3 process.
--
-- Where the handle to become inactive (due to the process being killed)
-- the Z3 process is spawned again and its setup code reevaluted.
newtype Z3Solver i a = Z3Solver  { getZ3Solver :: StateT (Maybe Z3SolverState) IO a }
                        deriving (Applicative, Functor, Monad, MonadState (Maybe Z3SolverState), MonadIO)

-- | Terminate the current instance of the solver
terminateZ3 :: Z3Solver i ()
terminateZ3 =
   gets (processHandle . fromJust) >>= Z3Solver . lift . terminateProcess

-- | Read until a valid datum (from the S-expressions) is found 
-- in the input handle. Reads line-by-line.
readDatum :: Z3Solver i SExp.SExp
readDatum = do
   hout <- gets (outputHandle . fromJust)
   line <- liftIO $ hGetLine hout
   currentBuffer <- modify (\(Just st) -> Just $ st { buffer = line : buffer st }) >> gets (buffer . fromJust)
   case SExp.parseDatum "z3-in" (concat $ reverse currentBuffer) of 
      -- continue reading lines until a valid datum is found
      Left _ -> readDatum
      -- otherwise return the result
      Right (result, rest) -> do
         modify (\(Just st) -> Just $ st { buffer = [rest] }) 
         return result

-- | Ensure that the given S-expression is a symbol with the given contents
ensureAtom :: String -> SExp.SExp -> Z3Solver i ()
ensureAtom expected (SExp.Atom nam _) 
   | nam == expected = return () 
   | otherwise = error $ "expected " ++ expected ++ " but got " ++ nam
ensureAtom expected expr = 
   error $ "expected atom with contents " ++ expected ++ " but got " ++ show expr ++ " instead"

-- | Assumes that the given S-expression is an atom and returns its name 
-- otherwise results in an error.
fromAtom :: SExp.SExp -> Z3Solver i String
fromAtom (SExp.Atom name _) = return name 
fromAtom expr = error $ "not an artom " ++ show expr

-- | Evaluate the given script 
-- and read the answer from the output of the Z3 process.
eval :: String -> Z3Solver i SExp.SExp
eval query = do
   -- write the query to stdin of the attached process
   hin <- gets (inputHandle . fromJust)
   Z3Solver $ lift $ hPutStrLn hin query
   Z3Solver $ lift $ hFlush hin
   readDatum

-- | Runs the command and ensures that it is successfully executed, 
-- but requires that there is no other output. (assumes the :print-success option)
command :: String -> Z3Solver i () 
command = eval >=> ensureAtom "success"

-- | Exits the solver
exit :: Z3Solver i ()
exit = do
   hin <- gets (inputHandle . fromJust)
   hout <- gets (outputHandle . fromJust)
   pid <- gets (processHandle . fromJust)
   Z3Solver $ lift $ hPutStrLn hin "(exit)"
   Z3Solver $ lift $ hFlush hin
   Z3Solver $ lift $ hClose hin
   Z3Solver $ lift $ hClose hout
   Z3Solver $ lift $ terminateProcess pid
   void $ Z3Solver $ lift $ waitForProcess pid


-- | Creates a checkpoint at the current point in evaluation
checkpoint :: Z3Solver i ()
checkpoint = command "(push 1)"

-- | Restores the solver the last checkpoint
restoreCheckpoint :: Z3Solver i ()
restoreCheckpoint =
   command "(pop 1)" >> checkpoint

-- | Run the Z3Solver
runZ3Solver :: Z3Solver i a -> IO a
runZ3Solver m  = evalStateT (getZ3Solver $ m <* exit) Nothing

runZ3SolverWithSetup :: Show i => Ord i => String -> Z3Solver i a -> IO a
runZ3SolverWithSetup setupCode' m = evalStateT (getZ3Solver $ setup setupCode' >> m <* exit) Nothing

-- | Run the Z3 solver with the default setup code from `Symbolic.SMT.prelude`.
runZ3SolverWithPrelude :: (Show i, Ord i) => Z3Solver i a -> IO a 
runZ3SolverWithPrelude = runZ3SolverWithSetup prelude

-- | Same as @runZ3SolverWithSetup@ but the Z3 process is not terminated
runZ3SolverBackground :: forall i . (Ord i, Show i) => String -> IO Z3SolverState
runZ3SolverBackground setupCode' = fromMaybe (error "failed to start the Z3 solver") <$> execStateT (getZ3Solver $ setup @i setupCode') Nothing

-- | Execute the given Z3 computation in the given Z3 state
execInZ3State :: forall i a . Z3SolverState -> Z3Solver i a -> IO a
execInZ3State s = flip evalStateT (Just s) . getZ3Solver 

-- TODO: ShowableVariable is no longer used remove
instance {-# OVERLAPPING #-} (Show i, Ord i) => FormulaSolver i (Z3Solver i) where
   setup setupCode' = do
      spawned <- gets isJust
      -- if there is already an active instance of Z3 
      -- terminate it and spawn a new one.
      when spawned terminateZ3
      -- spawn the new instance
      Z3Solver (lift $ spawnZ3 setupCode') >>= (put . Just)
      -- evaluate the setup script
      _ <- command setupCode'
      -- store this as a checkpoint to return to in the 
      -- beginning of a solve script
      checkpoint

   getModel count script = eval "(get-model)" <&> (either error id  . parseModel assgn')   
      where (_, _, assgn) = translate count script
            assgn' = Map.fromList $ concatMap (\(key, values) -> map (,key) (Set.toList values)) $ Map.toList assgn

   solve _ (Atomic Fail) = return Unsat
   solve count script   = do
      -- liftIO (print count)
      restoreCheckpoint
      -- Z3Solver $ liftIO $ putStrLn $ "trying to translate script " ++ show script
      -- Declare all variables as constants
      let (translatedScript, names, _) = translate count script
      -- liftIO (putStrLn $ "Solver:original>>>" ++ show script)
      -- liftIO (putStrLn $ "Solver:translated>>>" ++ show translatedScript)
      -- evaluate the mall in the solver
      -- mapM_ (command . printf "(declare-const %s V)" ) names
      mapM_ (command . printf "(declare-const %s V)" ) names
      -- Evaluate all the assertions, and ignore any errors
      _ <- command (printf "(assert %s)" translatedScript)
      -- Check whether the model is satisfiable
      result <- parseResult <$> (fromAtom =<< eval "(check-sat)")
      return result
