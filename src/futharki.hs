module Main where

import Data.Version
import Control.Monad
import Data.Monoid

import Prelude

import Language.Futhark.Parser
import Futhark.Version
import Futhark.Passes
import Futhark.Compiler
import Futhark.Util.Options

banner :: String
banner = unlines [
  "|// |\\    |   |\\  |\\   /",
  "|/  | \\   |\\  |\\  |/  /",
  "|   |  \\  |/  |   |\\  \\",
  "|   |   \\ |   |   | \\  \\"
  ]

main :: IO ()
main = mainWithOptions interpreterConfig [] run
  where run [prog] config = Just $ interpret config prog
        run []     _      = Just repl
        run _      _      = Nothing

repl :: IO ()
repl = do
  putStr banner
  putStrLn $ "Version " ++ showVersion version
  putStrLn "(C) HIPERFIT research centre"
  putStrLn "Department of Computer Science, University of Copenhagen (DIKU)"
  putStrLn ""
  forever $ print =<< parseExpIncrIO "input" mempty

interpret :: FutharkConfig -> FilePath -> IO ()
interpret config =
  runCompilerOnProgram config (standardPipeline Executable) interpretAction'

interpreterConfig :: FutharkConfig
interpreterConfig = newFutharkConfig
