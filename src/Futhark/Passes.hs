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
  , babysitKernels
  , expandAllocations
  , expandArrays
  , extractKernels
  , standardPipeline
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
import qualified Futhark.KernelBabysitting
import qualified Futhark.ExpandAllocations
import qualified Futhark.ExpandArrays
import qualified Futhark.ExtractKernels

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

babysitKernels :: Pass
babysitKernels = unfailableBasicPass "babysit kernels"
                 Futhark.KernelBabysitting.babysitKernels

expandAllocations :: Pass
expandAllocations = unfailableExplicitMemoryPass "expand allocations"
                    Futhark.ExpandAllocations.expandAllocations

expandArrays :: Pass
expandArrays = unfailableBasicPass "expand arrays"
               Futhark.ExpandArrays.expandArrays

extractKernels :: Pass
extractKernels = unfailableBasicPass "distribute kernels"
                 Futhark.ExtractKernels.transformProg

standardPipeline :: [Pass]
standardPipeline =
  [ uttransform
  , eotransform
  , inlinetransform
  , commonSubexpressionElimination
  , eotransform
    -- We run fusion twice.
  , hotransform
  , commonSubexpressionElimination
  , eotransform
  , hotransform
  , commonSubexpressionElimination
  , eotransform
  , removeDeadFunctions
  ]
