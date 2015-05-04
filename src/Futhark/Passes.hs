module Futhark.Passes
  ( CompileError(..)
  , FutharkM
  , compileError
  , Pass(..)
  , canFail
  , fotransform
  , uttransform
  , eotransform
  , hotransform
  , inlinetransform
  , removeDeadFunctions
  , optimisePredicates
  , optimiseShapes
  , explicitMemory
  , inPlaceLowering
  , commonSubexpressionElimination
  , flattening
  , doubleBuffer
  , sequentialiseKernels
  )
where

import Control.Applicative

import Prelude

import Futhark.Optimise.SimpleOpts
import Futhark.Optimise.Fusion
import qualified Futhark.FirstOrderTransform as FOT
import Futhark.Untrace
import Futhark.Pipeline
import Futhark.Optimise.InliningDeadFun
import qualified Futhark.Optimise.SuffCond
import qualified Futhark.Optimise.SplitShapes
import qualified Futhark.ExplicitAllocations
import qualified Futhark.Optimise.InPlaceLowering
import qualified Futhark.Optimise.CSE
import qualified Futhark.Flattening
import qualified Futhark.Optimise.DoubleBuffer
import qualified Futhark.KernelSequentialisation

fotransform :: Pass
fotransform = unfailableBasicPass "first-order transform"
              FOT.transformProg

uttransform :: Pass
uttransform = unfailableBasicPass "debugging annotation removal"
              untraceProg

eotransform :: Pass
eotransform = polyPass "enabling optimations" op
  where op (Basic prog)          =
          canFail "" (Just $ Basic prog) $
          Basic <$>
          simpleOptProg bindableSimpleOps basicRules prog
        op (ExplicitMemory prog) =
          canFail "" (Just $ ExplicitMemory prog) $
          ExplicitMemory <$>
          simpleOptProg Futhark.ExplicitAllocations.simplifiable standardRules prog

hotransform :: Pass
hotransform = basicPass "higher-order optimisations"
              fuseProg

inlinetransform :: Pass
inlinetransform = basicPass "inline functions"
                  aggInlineDriver

removeDeadFunctions :: Pass
removeDeadFunctions = basicPass "Remove dead functions"
                      deadFunElim

optimisePredicates :: Pass
optimisePredicates = unfailableBasicPass "optimise predicates"
                     Futhark.Optimise.SuffCond.optimiseProg

optimiseShapes :: Pass
optimiseShapes = unfailableBasicPass "optimise shape analysis"
                 Futhark.Optimise.SplitShapes.splitShapes

explicitMemory :: Pass
explicitMemory = polyPass "insert explicit allocations" op
  where op s = do prog <- basicProg s
                  return $ ExplicitMemory $
                    Futhark.ExplicitAllocations.explicitAllocations prog

inPlaceLowering :: Pass
inPlaceLowering = unfailableBasicPass "lower in-place updates into loops"
                  Futhark.Optimise.InPlaceLowering.optimiseProgram

commonSubexpressionElimination :: Pass
commonSubexpressionElimination =
  polyPass "common subexpression elimination" op
  where op (Basic prog)          =
          return $ Basic $ Futhark.Optimise.CSE.performCSE prog
        op (ExplicitMemory prog) =
          return $ ExplicitMemory $ Futhark.Optimise.CSE.performCSE prog

flattening :: Pass
flattening = basicPass "Flattening"
             Futhark.Flattening.flattenProg

doubleBuffer :: Pass
doubleBuffer = unfailableExplicitMemoryPass "double buffering"
               Futhark.Optimise.DoubleBuffer.optimiseProg

sequentialiseKernels :: Pass
sequentialiseKernels = unfailableBasicPass "sequentialise kernels"
                       Futhark.KernelSequentialisation.sequentialiseKernels
