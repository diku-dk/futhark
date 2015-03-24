module Main where

import Data.Version
import Control.Monad
import System.Environment

import Language.Futhark.Parser
import Futhark.Version
import Futhark.Pipeline
import Futhark.Passes
import Futhark.Compiler

banner :: String
banner = unlines [
  "|// |\\    |   |\\  |\\   /",
  "|/  | \\   |\\  |\\  |/  /",
  "|   |  \\  |/  |   |\\  \\",
  "|   |   \\ |   |   | \\  \\"
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    []     -> repl
    [prog] -> interpret prog
    _      -> usage

repl :: IO ()
repl = do
  putStr banner
  putStrLn $ "Version " ++ showVersion version
  putStrLn "(C) HIPERFIT research centre"
  putStrLn "Department of Computer Science, University of Copenhagen (DIKU)"
  putStrLn ""
  forever $ print =<< parseExpIncrIO "input" ""

usage :: IO ()
usage = do prog <- getProgName
           putStrLn $ "Usage: " ++ prog ++ " [file]"

interpret :: FilePath -> IO ()
interpret = runCompilerOnProgram interpreterConfig

interpreterConfig :: FutharkConfig
interpreterConfig = FutharkConfig { futharkpipeline = interpreterPipeline
                                  , futharkaction = interpretAction'
                                  , futharkcheckAliases = True
                                  , futharkverbose = Nothing
                                  , futharkboundsCheck = True
                                  }

interpreterPipeline :: [Pass]
interpreterPipeline =
  [ uttransform
  , eotransform
  , inlinetransform
  , commonSubexpressionElimination
  , eotransform
  , hotransform
  , commonSubexpressionElimination
  , eotransform
  , removeDeadFunctions
  ]
