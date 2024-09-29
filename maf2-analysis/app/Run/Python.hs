{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}

module Run.Python(main, Options, options) where

import Syntax.Python
import Options.Applicative
import Data.Maybe
import Analysis.Python.Fixpoint 
import Data.Map (Map)
import qualified Data.Map as Map 
import Analysis.Python.Common
import Data.List
import Text.Printf (printf)
import Control.Monad.DomainError
import Analysis.Python.Escape (PyEsc)
import Data.Set (Set)
import Analysis.Python.Monad 
import Language.Python.Common (annot)
import Domain.Python.Syntax 
import Data.IORef 
import System.IO 
import Data.Function ((&))
import Control.Monad.Escape (MayEscape(..))
import "maf2-analysis" Data.Graph (edges)
import Lattice 

newtype Options = Options String 
   deriving Show

options :: Parser Options
options = Options <$> 
   strOption (long "filename" <> short 'f' <> help "Name of the file to parse")

printOSto :: Show obj => Map ObjAdr obj -> String
printOSto m = intercalate "\n" $ map (\(k,v) -> printf "%*s | %s" indent (show k) (show v)) adrs
   where adrs   = Map.toList m
                    & filter (\case (PrmAdr _, _) -> False ; _ -> True)
         indent = maximum (map (length . show . fst) adrs) + 5

printRSto :: Map PyCmp (MayEscape PyRet PyRef) -> String
printRSto m = intercalate "\n" $ map (\(k,v) -> printf "%*s | %s" indent (showCmp k) (showRes v)) cmps
   where cmps = Map.toList m
         showRes (Escape e) = "[!!: "++show e++"]"
         showRes (Value v) = show v
         showRes (MayBoth v e) = "[!!: "++show e++"]" ++ show v 
         showRes _ = "⊥"
         showCmp ((Main _, _), _) = "<main>"
         showCmp ((LoopBdy loc _ _, _), ctx) = "<loop " ++ show loc ++ " with context " ++ show ctx ++ ">"
         showCmp ((FuncBdy loc _, _), ctx) = "<func " ++ show loc ++ " with context " ++ show ctx ++ ">"
         indent = maximum (map (length . showCmp . fst) cmps) + 5

runREPL :: IO ()
runREPL = do count <- newIORef 0 
             analyzeREPL @PyDomainCP (read count) print 
      where read count = do cur <- readIORef count
                            writeIORef count (cur + 1)
                            prompt cur
            prompt cur = do putStr ">>> "
                            hFlush stdout
                            txt <- getLine 
                            case parse ("REPL:" ++ show cur) txt of  
                              Just parsed -> return parsed
                              Nothing     -> putStrLn "Invalid program" >> prompt cur 


runFile :: String -> IO () 
runFile fileName = 
   do program <- readFile fileName
      let Just parsed = parse "testje" program
      let (rsto, osto, graph) = analyzeCP parsed 
      putStrLn "\nPROGRAM:\n"
      putStrLn (prettyString parsed)
      putStrLn "\nRESULTS PER COMPONENT:\n"
      putStrLn (printRSto rsto)
      putStrLn "\nOBJECT STORE RESULTS:\n"
      putStrLn (printOSto osto)
      putStrLn "\nDEPENDENCY GRAPH:\n"
      putStrLn "\n"

generateGraph :: [String] -> IO () 
generateGraph files =
   do putStrLn "digraph ECOPIPE {"
      mapM_ generateGraphForFile files 
      putStrLn "}"
   where generateGraphForFile file = do program <- readFile file
                                        let Just parsed = parse "testje" program
                                        let (_, _, graph) = analyzeCP parsed 
                                        printGraph file graph
         printGraph file graph = mapM_ (putStrLn . showEdge file) (edges graph)
         showEdge file (from, to, ()) = "\t" ++ showNode from ++ " -> " ++ showNode to ++ " [label = " ++ show (shortFileName file) ++ " ];"
         showNode (Constant name) = name
         showNode Top = "⊤"
         showNode _ = "⊥"
         shortFileName = reverse . takeWhile (/='/') . reverse 

main :: Options -> IO ()
main (Options fileName) = generateGraph ["programs/python/zensor/add_regime_status_tag.py"]


