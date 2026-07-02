module Runnables.SimpleActorAnalyze(
        analyzeAst, 
        entrypoint
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Analysis.Store as Store
import qualified Analysis.SimpleActor.Fixpoint as Fixpoint
import qualified Analysis.SimpleActor.Fixpoint.Common as Common
import qualified Analysis.Actors.Mailbox.Graph.Dot as Dot
import Criterion.Main
import qualified Criterion.Main.Options as Criterion
import Criterion.Types (Config(..))
import CommandLine.Options
import Text.Pretty.Simple
import Syntax.AST
import qualified Domain.Scheme.Store as Store

------------------------------------------------------------
-- Utilities
------------------------------------------------------------

escapePath :: String -> String
escapePath ('/':cs) = '_' : escapePath cs 
escapePath (c:cs) = c : escapePath cs 
escapePath [] = []

------------------------------------------------------------
-- Dot mailbox rendering
------------------------------------------------------------

-- | Renders the mailbox abstraction of each actor in the system to a DOT file for visualization with Graphviz.
renderMailboxesToDot :: String -- ^ the output directory
                     -> String -- ^ string to prefix the output file with 
                     -> Map Common.ActorRef Common.PMB
                     -> IO ()
renderMailboxesToDot outDir prefix = mapM_ (uncurry renderMailbox) . Map.toList
    where
        renderMailbox actorRef mb = do
            let mailboxName = "mailbox_" <> show actorRef
                dotContent = Dot.render mb
             in writeFile (outDir ++ "/" ++ prefix ++ escapePath mailboxName ++ ".dot") dotContent


-- | Renders the mailbox abstraction for each step in the analysis trace to a DOT file for visualization with Graphviz.
-- The files are numbered sequentially to reflect the order of the trace.
renderTraceMailboxesToDot :: String -> [Map Common.ActorRef Common.PMB] -> IO ()
renderTraceMailboxesToDot outDir mailboxTrace = mapM_ renderStep (zip [0..] mailboxTrace)
    where
        renderStep :: (Int, Map Common.ActorRef Common.PMB) -> IO ()
        renderStep (stepNum, mbs) = do
            renderMailboxesToDot outDir ("trace_step_" ++ show stepNum ++ "_") mbs

------------------------------------------------------------
-- Entrypoints
------------------------------------------------------------

entrypoint :: InputOptions -> OutputDirOptions -> Bool -> IO ()
entrypoint InputOptions { .. } OutputDirOptions { .. } doBenchmark = do
    ast <- loadFile' doTranslate filename
    if doBenchmark
    then runMode (Criterion.Run (defaultConfig { reportFile = Just $ outputDirPath ++ "report.html", csvFile = Just $ outputDirPath ++ "report.csv" }) Criterion.Glob ["*"]) [ bench (escapePath filename) $ nfIO (Fixpoint.analyzeIO ast) ]
    else do
        pPrint ast
        analyzeAst ast outputDirPath

analyzeAst :: Exp -> String -> IO ()
analyzeAst expr outDir = do
        (_, state, sto) <- Fixpoint.analyzeIO expr
        -- let sto    = Fixpoint._store $ snd output
        let blames = Fixpoint._blames $ state

        let trace = reverse $ map Fixpoint._mbs $ Fixpoint._trace $ state
        putStrLn $ show blames
        renderTraceMailboxesToDot outDir trace
        mapM_ (uncurry printSto) (Map.toList sto)
    where printSto k sto = 
                do
                    putStrLn ""
                    putStrLn (show k) 
                    putStrLn $ Store.printMap show (const True) (Store._paiStore sto)
                    putStrLn $ Store.printMap show (const True) (Store._varStore sto)

