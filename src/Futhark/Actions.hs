module Futhark.Actions
  ( printAction
  , interpretAction
  , impCodeGenAction
  , kernelImpCodeGenAction
  , seqCodeGenAction
  , rangeAction
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import System.Exit (exitWith, ExitCode(..))
import System.IO

import Prelude

import Futhark.Pipeline
import Futhark.Analysis.Alias
import Futhark.Analysis.Range
import Futhark.Representation.AST
import Futhark.Representation.Basic (Basic)
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import Futhark.Interpreter
import qualified Futhark.CodeGen.ImpGen.Sequential as ImpGenSequential
import qualified Futhark.CodeGen.ImpGen.Kernels as ImpGenKernels
import qualified Futhark.CodeGen.Backends.SequentialC as SequentialC

printAction :: PrettyLore lore => Action lore
printAction =
  Action { actionName = "Prettyprint"
         , actionDescription = "Prettyprint the resulting internal representation on standard output."
         , actionProcedure = liftIO . putStrLn . pretty . aliasAnalysis
         }

interpretAction :: Show error => (FilePath -> String -> Either error [Value])
                -> Action Basic
interpretAction parser =
  Action { actionName = "Interpret"
         , actionDescription = "Run the program via an interpreter."
         , actionProcedure = liftIO . interpret parser
         }

rangeAction :: PrettyLore lore => Action lore
rangeAction =
    Action { actionName = "Range analysis"
           , actionDescription = "Print the program with range annotations added."
           , actionProcedure = liftIO . putStrLn . pretty . rangeAnalysis
           }

seqCodeGenAction :: Action ExplicitMemory
seqCodeGenAction =
  Action { actionName = "Compile sequentially"
         , actionDescription = "Translate program into sequential C and write it on standard output."
         , actionProcedure = liftIO . either error putStrLn . SequentialC.compileProg
         }


impCodeGenAction :: Action ExplicitMemory
impCodeGenAction =
  Action { actionName = "Compile imperative"
         , actionDescription = "Translate program into imperative IL and write it on standard output."
         , actionProcedure = liftIO . either error (putStrLn . pretty) . ImpGenSequential.compileProg
         }

kernelImpCodeGenAction :: Action ExplicitMemory
kernelImpCodeGenAction =
  Action { actionName = "Compile imperative kernels"
         , actionDescription = "Translate program into imperative IL with kernels and write it on standard output."
         , actionProcedure = liftIO . either error (putStrLn . pretty) . ImpGenKernels.compileProg
         }

interpret :: (Show error, PrettyLore lore) =>
             (FilePath -> String -> Either error [Value])
          -> Prog lore -> IO ()
interpret parseValues prog =
  case funDecByName defaultEntryPoint prog of
    Nothing -> do hPutStrLn stderr "Interpreter error: no main function."
                  exitWith $ ExitFailure 2
    Just _ -> do
      parseres <- liftM (parseValues "<stdin>") getContents
      args <- case parseres of Left e -> do hPutStrLn stderr $ "Read error: " ++ show e
                                            exitWith $ ExitFailure 2
                               Right vs -> return vs
      let (res, trace) = runFunWithShapes defaultEntryPoint args prog
      mapM_ (hPutStrLn stderr) trace
      case res of
        Left err -> do hPutStrLn stderr $ "Interpreter error:\n" ++ show err
                       exitWith $ ExitFailure 2
        Right val  -> putStrLn $ ppOutput val
  where ppOutput vs = intercalate "\n" $ map pretty vs
