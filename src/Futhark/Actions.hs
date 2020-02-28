{-# LANGUAGE FlexibleContexts #-}
module Futhark.Actions
  ( printAction
  , impCodeGenAction
  , kernelImpCodeGenAction
  , multicoreImpCodeGenAction
  , rangeAction
  , metricsAction
  )
where

import Control.Monad.IO.Class

import Futhark.Pipeline
import Futhark.Analysis.Alias
import Futhark.Analysis.Range
import Futhark.Representation.AST
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import qualified Futhark.CodeGen.ImpGen.Sequential as ImpGenSequential
import qualified Futhark.CodeGen.ImpGen.Kernels as ImpGenKernels
import qualified Futhark.CodeGen.ImpGen.Multicore as ImpGenMulticore
import Futhark.Representation.AST.Attributes.Ranges (CanBeRanged)
import Futhark.Analysis.Metrics
import Futhark.Util.Pretty (prettyText)

printAction :: (Attributes lore, CanBeAliased (Op lore)) => Action lore
printAction =
  Action { actionName = "Prettyprint"
         , actionDescription = "Prettyprint the resulting internal representation on standard output."
         , actionProcedure = liftIO . putStrLn . pretty . aliasAnalysis
         }

rangeAction :: (Attributes lore, CanBeRanged (Op lore)) => Action lore
rangeAction =
    Action { actionName = "Range analysis"
           , actionDescription = "Print the program with range annotations added."
           , actionProcedure = liftIO . putStrLn . pretty . rangeAnalysis
           }

metricsAction :: OpMetrics (Op lore) => Action lore
metricsAction =
  Action { actionName = "Compute metrics"
         , actionDescription = "Print metrics on the final AST."
         , actionProcedure = liftIO . putStr . show . progMetrics
         }

impCodeGenAction :: Action ExplicitMemory
impCodeGenAction =
  Action { actionName = "Compile imperative"
         , actionDescription = "Translate program into imperative IL and write it on standard output."
         , actionProcedure = \prog ->
                               either (`internalError` prettyText prog) (liftIO . putStrLn . pretty) =<<
                               ImpGenSequential.compileProg prog
         }

kernelImpCodeGenAction :: Action ExplicitMemory
kernelImpCodeGenAction =
  Action { actionName = "Compile imperative kernels"
         , actionDescription = "Translate program into imperative IL with kernels and write it on standard output."
         , actionProcedure = \prog ->
                               either (`internalError` prettyText prog) (liftIO . putStrLn . pretty) =<<
                               ImpGenKernels.compileProg prog
         }

multicoreImpCodeGenAction :: Action ExplicitMemory
multicoreImpCodeGenAction =
  Action { actionName = "Compile to imperative multicore"
         , actionDescription = "Translate program into imperative multicore IL and write it on standard output."
         , actionProcedure = \prog ->
                               either (`internalError` prettyText prog) (liftIO . putStrLn . pretty) =<<
                               ImpGenMulticore.compileProg prog
         }
