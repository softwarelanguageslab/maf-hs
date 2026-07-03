module Runnables.ContractBenchmarks(entrypoint) where

import CommandLine.Options
import qualified Analysis.SimpleActor.Fixpoint as Fixpoint
import Syntax.AST
import System.Clock
import Data.Aeson
import GHC.Generics
import Data.Traversable
import Control.DeepSeq
import qualified Data.Set as Set
import System.IO

------------------------------------------------------------
-- Constants
------------------------------------------------------------

maxRuns :: Integer
maxRuns = 30

------------------------------------------------------------
-- Output data
------------------------------------------------------------

data BlameRecord = BlameRecord {
        partyExpr    :: Exp, 
        contractExpr :: Exp,
        blameLoc     :: Span
    } deriving (Ord, Eq, Show, Generic)

instance ToJSON BlameRecord

fromFixpointBlameRecord :: Fixpoint.BlameRecord -> BlameRecord
fromFixpointBlameRecord Fixpoint.BlameRecord { .. } = 
    BlameRecord partyExpr contractExpr blameLoc
    
data BenchmarkOutput = BenchmarkOutput {
        -- false positives if the program is safe, measure for precision
        blames :: [BlameRecord],
        -- number of runs of the benchmark program
        runs :: Integer, 
        -- the actual execution times for each run in nanoseconds
        executionTimes :: [ Integer ]
    } deriving (Ord, Eq, Show, Generic)

instance ToJSON BenchmarkOutput

------------------------------------------------------------
-- Time
------------------------------------------------------------

-- | Get the time now, according to a monotonically increasing
-- system clock.
now :: IO TimeSpec
now = getTime Monotonic

------------------------------------------------------------
-- Entrypoint
------------------------------------------------------------

entrypoint :: InputOptions -> OutputOptions -> IO ()
entrypoint InputOptions { .. } OutputOptions { .. } = do    
    ast <- loadFile' {- doTranslate -} False filename
    analyze ast outputPath

-- | Analyze the given AST, and write the output in JSON to the given output path
analyze :: Exp -> FilePath -> IO ()
analyze expr outputPath = do
    -- Run once for precision benchmarks
    (_, state, _) <- Fixpoint.analyzeIO expr
    let blames = Fixpoint._blames state
    hPutStrLn stderr ("Finished initial run for precision got "  ++ show (Set.size blames))
    -- Run multiples times (3à) to measure performance
    times <- for [0..maxRuns] $ \i -> do 
        start <- now
        output <- Fixpoint.analyzeIO expr
        end <- output `deepseq` now
        hPutStrLn stderr ("Iteration "  ++ show i ++ " of the " ++ show maxRuns)
        return (toNanoSecs $ diffTimeSpec start end)

    -- Create the output, and write it in a JSON format to the output file
    let output = BenchmarkOutput { blames = map fromFixpointBlameRecord $ Set.toList blames, runs = maxRuns, executionTimes = times }
    encodeFile outputPath output
