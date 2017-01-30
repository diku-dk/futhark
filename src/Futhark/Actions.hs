{-# LANGUAGE FlexibleContexts #-}
module Futhark.Actions
  ( printAction
  , interpretAction
  , impCodeGenAction
  , kernelImpCodeGenAction
  , seqCodeGenAction
  , rangeAction
  , memoryAction
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Exit (exitWith, ExitCode(..))
import System.IO

import Prelude

import Futhark.Pipeline
import Futhark.Analysis.Alias
import Futhark.Analysis.Range
import Futhark.Representation.AST
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.SOACS (SOACS)
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import Futhark.Interpreter
import Futhark.MemoryBlockMerging --C.O.
import qualified Futhark.CodeGen.ImpGen.Sequential as ImpGenSequential
import qualified Futhark.CodeGen.ImpGen.Kernels as ImpGenKernels
import qualified Futhark.CodeGen.Backends.SequentialC as SequentialC
import Futhark.Representation.AST.Attributes.Ranges (CanBeRanged)

printAction :: (Attributes lore, CanBeAliased (Op lore)) => Action lore
printAction =
  Action { actionName = "Prettyprint"
         , actionDescription = "Prettyprint the resulting internal representation on standard output."
         , actionProcedure = liftIO . putStrLn . pretty . aliasAnalysis
         }

interpretAction :: Show error => (FilePath -> T.Text -> Either error [Value])
                -> Action SOACS
interpretAction parser =
  Action { actionName = "Interpret"
         , actionDescription = "Run the program via an interpreter."
         , actionProcedure = liftIO . interpret parser
         }

rangeAction :: (Attributes lore, CanBeRanged (Op lore)) => Action lore
rangeAction =
    Action { actionName = "Range analysis"
           , actionDescription = "Print the program with range annotations added."
           , actionProcedure = liftIO . putStrLn . pretty . rangeAnalysis
           }

seqCodeGenAction :: Action ExplicitMemory
seqCodeGenAction =
  Action { actionName = "Compile sequentially"
         , actionDescription = "Translate program into sequential C and write it on standard output."
         , actionProcedure = either compileFail (liftIO . putStrLn) <=<
                             SequentialC.compileProg
         }


impCodeGenAction :: Action ExplicitMemory
impCodeGenAction =
  Action { actionName = "Compile imperative"
         , actionDescription = "Translate program into imperative IL and write it on standard output."
         , actionProcedure = either compileFail (liftIO . putStrLn . pretty) <=<
                             ImpGenSequential.compileProg
         }

kernelImpCodeGenAction :: Action ExplicitMemory
kernelImpCodeGenAction =
  Action { actionName = "Compile imperative kernels"
         , actionDescription = "Translate program into imperative IL with kernels and write it on standard output."
         , actionProcedure = either compileFail (liftIO . putStrLn . pretty) <=<
                             ImpGenKernels.compileProg
         }

interpret :: Show error =>
             (FilePath -> T.Text -> Either error [Value])
          -> Prog SOACS -> IO ()
interpret parseValues prog =
  case funDefByName defaultEntryPoint prog of
    Nothing -> do hPutStrLn stderr "Interpreter error: no main function."
                  exitWith $ ExitFailure 2
    Just _ -> do
      parseres <- fmap (parseValues "<stdin>") T.getContents
      args <- case parseres of Left e -> do hPutStrLn stderr $ "Read error: " ++ show e
                                            exitWith $ ExitFailure 2
                               Right vs -> return vs
      case runFunWithShapes defaultEntryPoint args prog of
        Left err -> do hPutStrLn stderr $ "Interpreter error:\n" ++ show err
                       exitWith $ ExitFailure 2
        Right val  -> putStrLn $ ppOutput val
  where ppOutput vs = intercalate "\n" $ map pretty vs

memoryAction :: Action ExplicitMemory --C.O.
memoryAction =
  Action { actionName = "Memory playground"
         , actionDescription = "Memory block merging playground"
         , actionProcedure = liftIO . memoryBlockMerging
         }
