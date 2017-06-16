{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Optimisation pipelines.
module Futhark.Passes
  ( standardPipeline
  , sequentialPipeline
  , kernelsPipeline
  , sequentialCpuPipeline
  , gpuPipeline
  , CompilationMode (..)
  )
where

import Control.Category ((>>>))

import Futhark.Optimise.CSE
import Futhark.Optimise.Fusion
import Futhark.Optimise.InPlaceLowering
import Futhark.Optimise.InliningDeadFun
import Futhark.Optimise.TileLoops
import Futhark.Optimise.DoubleBuffer
import Futhark.Optimise.Unstream
import Futhark.Pass.MemoryBlockMerging
import Futhark.Pass.RegisterAllocation
import Futhark.Pass.ExpandAllocations
import Futhark.Pass.ExplicitAllocations
import Futhark.Pass.ExtractKernels
import Futhark.Pass.FirstOrderTransform
import Futhark.Pass.KernelBabysitting
import Futhark.Pass.Simplify
import Futhark.Pipeline
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import Futhark.Representation.Kernels (Kernels)
import Futhark.Representation.SOACS (SOACS)
import Futhark.Util

-- | Are we compiling the Futhark program as an executable or a
-- library?  This affects which functions are considered as roots for
-- dead code elimination and ultimately exist in generated code.
data CompilationMode = Executable
                     -- ^ Only the top-level function named @main@ is
                       -- alive.
                     | Library
                       -- ^ Only top-level functions marked @entry@
                       -- are alive.

standardPipeline :: Pipeline SOACS SOACS
standardPipeline =
  passes [ simplifySOACS
         , inlineAndRemoveDeadFunctions
         , performCSE True
         , simplifySOACS
           -- We run fusion twice
         , fuseSOACs
         , performCSE True
         , simplifySOACS
         , fuseSOACs
         , performCSE True
         , simplifySOACS
         , removeDeadFunctions
         ]

-- Experimental!  Enable by setting the environment variable
-- MEMORY_BLOCK_MERGING to 1.
usesExperimentalMemoryBlockMerging :: Bool
usesExperimentalMemoryBlockMerging =
  maybe False (=="1") $ lookup "MEMORY_BLOCK_MERGING" unixEnvironment

withExperimentalMemoryBlockMerging :: Pipeline SOACS ExplicitMemory
                                   -> Pipeline SOACS ExplicitMemory
withExperimentalMemoryBlockMerging =
  (>>> passes [ mergeMemoryBlocks
              , simplifyExplicitMemory
              ])

-- Experimental!  Enable by setting the environment variable
-- REGISTER_ALLOCATION to 1.
usesExperimentalRegisterAllocation :: Bool
usesExperimentalRegisterAllocation =
  maybe False (=="1") $ lookup "REGISTER_ALLOCATION" unixEnvironment

withExperimentalRegisterAllocation :: Pipeline SOACS ExplicitMemory
                                   -> Pipeline SOACS ExplicitMemory
withExperimentalRegisterAllocation =
  (>>> passes [ runThroughAllocations
              , simplifyExplicitMemory
              ])

withExperimentalCPUPasses :: Pipeline SOACS ExplicitMemory
                          -> Pipeline SOACS ExplicitMemory
withExperimentalCPUPasses pipeline =
  let pipeline1 = if usesExperimentalMemoryBlockMerging
                  then withExperimentalMemoryBlockMerging pipeline
                  else pipeline
      pipeline2 = if usesExperimentalRegisterAllocation
                  then withExperimentalRegisterAllocation pipeline1
                  else pipeline1
  in pipeline2

withExperimentalGPUPasses :: Pipeline SOACS ExplicitMemory
                          -> Pipeline SOACS ExplicitMemory
withExperimentalGPUPasses pipeline =
  let pipeline1 = if usesExperimentalMemoryBlockMerging
                  then withExperimentalMemoryBlockMerging pipeline
                  else pipeline
      pipeline2 = if usesExperimentalRegisterAllocation
                  then withExperimentalRegisterAllocation pipeline1
                  else pipeline1
  in pipeline2

-- Old!  Does not work.  See the module for more details.  Enable by setting the
-- environment variable IN_PLACE_LOWERING to 1.
usesOldInPlaceLowering :: Bool
usesOldInPlaceLowering =
  maybe False (== "1") $ lookup "IN_PLACE_LOWERING" unixEnvironment

inPlaceLoweringMaybe :: Pipeline Kernels Kernels
inPlaceLoweringMaybe = if usesOldInPlaceLowering
                       then onePass inPlaceLowering
                       else passes []

kernelsPipeline :: Pipeline SOACS Kernels
kernelsPipeline =
  standardPipeline >>>
  onePass extractKernels >>>
  passes [ simplifyKernels
         , babysitKernels
         , simplifyKernels
         , tileLoops
         , unstream
         , simplifyKernels
         , performCSE True
         , simplifyKernels
         ] >>>
  inPlaceLoweringMaybe

sequentialPipeline :: Pipeline SOACS Kernels
sequentialPipeline =
  standardPipeline >>>
  onePass firstOrderTransform >>>
  passes [ simplifyKernels
         ] >>>
  inPlaceLoweringMaybe

sequentialCpuPipeline :: Pipeline SOACS ExplicitMemory
sequentialCpuPipeline =
  withExperimentalCPUPasses $
  sequentialPipeline >>>
  onePass explicitAllocations >>>
  passes [ simplifyExplicitMemory
         , performCSE False
         , simplifyExplicitMemory
         , doubleBuffer
         , simplifyExplicitMemory
         ]

gpuPipeline :: Pipeline SOACS ExplicitMemory
gpuPipeline =
  withExperimentalGPUPasses $
  kernelsPipeline >>>
  onePass explicitAllocations >>>
  passes [ simplifyExplicitMemory
         , performCSE False
         , simplifyExplicitMemory
         , doubleBuffer
         , simplifyExplicitMemory
         , expandAllocations
         , simplifyExplicitMemory
         ]
