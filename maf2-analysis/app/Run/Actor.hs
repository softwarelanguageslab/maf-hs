-- | Runs a configurable analysis on the given file
module Run.Actor(Options, options, main) where

import Options.Applicative

newtype Options = Options String deriving Show

options :: Parser Options
options =
   Options <$>
      strOption (long "filename" <> short 'f' <> help "the file to analyse")

main :: Options -> IO ()
main (Options _) = error "actor analysis is not defined in this package anymore, see `simplecc` package." 
