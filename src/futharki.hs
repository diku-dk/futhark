module Main where

import Data.Version
import Control.Monad

import Language.Futhark.Parser
import Futhark.Version
import Futhark.Pipeline
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
  forever $ print =<< parseExpIncrIO RealAsFloat64 "input" ""

interpret :: FutharkConfig -> FilePath -> IO ()
interpret = runCompilerOnProgram

interpreterConfig :: FutharkConfig
interpreterConfig = FutharkConfig { futharkpipeline = interpreterPipeline
                                  , futharkaction = interpretAction' RealAsFloat64
                                  , futharkcheckAliases = True
                                  , futharkverbose = Nothing
                                  , futharkboundsCheck = True
                                  , futharkRealConfiguration = RealAsFloat64
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
