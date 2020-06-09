{-# LANGUAGE FlexibleContexts #-}
-- | All (almost) compiler pipelines end with an 'Action', which does
-- something with the result of the pipeline.
module Futhark.Actions
  ( printAction
  , impCodeGenAction
  , kernelImpCodeGenAction
  , multicoreImpCodeGenAction
  , metricsAction
  )
where

import Control.Monad
import Control.Monad.IO.Class

import Futhark.Pipeline
import Futhark.Analysis.Alias
import Futhark.IR
import Futhark.IR.Prop.Aliases
import Futhark.IR.KernelsMem (KernelsMem)
import Futhark.IR.SeqMem (SeqMem)
import Futhark.IR.MCMem (MCMem)
import qualified Futhark.CodeGen.ImpGen.Sequential as ImpGenSequential
import qualified Futhark.CodeGen.ImpGen.Kernels as ImpGenKernels
import qualified Futhark.CodeGen.ImpGen.Multicore as ImpGenMulticore
import Futhark.Analysis.Metrics

-- | Print the result to stdout, with alias annotations.
printAction :: (ASTLore lore, CanBeAliased (Op lore)) => Action lore
printAction =
  Action { actionName = "Prettyprint"
         , actionDescription = "Prettyprint the resulting internal representation on standard output."
         , actionProcedure = liftIO . putStrLn . pretty . aliasAnalysis
         }

-- | Print metrics about AST node counts to stdout.
metricsAction :: OpMetrics (Op lore) => Action lore
metricsAction =
  Action { actionName = "Compute metrics"
         , actionDescription = "Print metrics on the final AST."
         , actionProcedure = liftIO . putStr . show . progMetrics
         }

-- | Convert the program to sequential ImpCode and print it to stdout.
impCodeGenAction :: Action SeqMem
impCodeGenAction =
  Action { actionName = "Compile imperative"
         , actionDescription = "Translate program into imperative IL and write it on standard output."
         , actionProcedure = liftIO . putStrLn . pretty <=< ImpGenSequential.compileProg
         }

-- | Convert the program to GPU ImpCode and print it to stdout.
kernelImpCodeGenAction :: Action KernelsMem
kernelImpCodeGenAction =
  Action { actionName = "Compile imperative kernels"
         , actionDescription = "Translate program into imperative IL with kernels and write it on standard output."
         , actionProcedure = liftIO . putStrLn . pretty <=< ImpGenKernels.compileProgOpenCL
         }

multicoreImpCodeGenAction :: Action MCMem
multicoreImpCodeGenAction =
  Action { actionName = "Compile to imperative multicore"
         , actionDescription = "Translate program into imperative multicore IL and write it on standard output."
         , actionProcedure = liftIO . putStrLn . pretty <=< ImpGenMulticore.compileProg
         }
