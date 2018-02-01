{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Optimisation pipelines.
module Futhark.Passes
  ( standardPipeline
  , sequentialPipeline
  , kernelsPipeline
  , sequentialCpuPipeline
  , gpuPipeline
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
import Futhark.Optimise.MemoryBlockMerging
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

-- Do we use in-place lowering?  Currently enabled by default.  Disable by
-- setting the environment variable IN_PLACE_LOWERING=0.
usesInPlaceLowering :: Bool
usesInPlaceLowering =
  isEnvVarSet "IN_PLACE_LOWERING" True

inPlaceLoweringMaybe :: Pipeline Kernels Kernels
inPlaceLoweringMaybe =
  if usesInPlaceLowering
  then onePass inPlaceLowering
  else passes []

-- Do we use the coalescing part of memory block merging?  Currently disabled by
-- default.  Enable by setting the environment variable
-- MEMORY_BLOCK_MERGING_COALESCING=1.
usesMemoryBlockMergingCoalescing :: Bool
usesMemoryBlockMergingCoalescing =
  isEnvVarSet "MEMORY_BLOCK_MERGING_COALESCING" False

memoryBlockMergingCoalescingMaybe :: Pipeline ExplicitMemory ExplicitMemory
memoryBlockMergingCoalescingMaybe =
  passes $ if usesMemoryBlockMergingCoalescing
           then [ memoryBlockMergingCoalescing
                , simplifyExplicitMemory
                ]
           else []

memoryBlockMergingCoalescingMaybeCPU :: Pipeline ExplicitMemory ExplicitMemory
memoryBlockMergingCoalescingMaybeCPU = memoryBlockMergingCoalescingMaybe

memoryBlockMergingCoalescingMaybeGPU :: Pipeline ExplicitMemory ExplicitMemory
memoryBlockMergingCoalescingMaybeGPU = memoryBlockMergingCoalescingMaybe

-- Do we use the reuse part of memory block merging?  Currently disabled by
-- default.  Enable by setting the environment variable
-- MEMORY_BLOCK_MERGING_REUSE=1.
usesMemoryBlockMergingReuse :: Bool
usesMemoryBlockMergingReuse =
  isEnvVarSet "MEMORY_BLOCK_MERGING_REUSE" False

memoryBlockMergingReuseMaybe :: Pipeline ExplicitMemory ExplicitMemory
memoryBlockMergingReuseMaybe =
  passes $ if usesMemoryBlockMergingReuse
           then [ memoryBlockMergingReuse
--                , simplifyExplicitMemory -- FIXME: When this is enabled,
--                something goes haywire related to the existential memory block
--                sizes produced when the max trick of the memory block reuse is
--                enabled.  In any case the simplifier should not be able to do
--                much after the previous pass.
                ]
           else []

memoryBlockMergingReuseMaybeCPU :: Pipeline ExplicitMemory ExplicitMemory
memoryBlockMergingReuseMaybeCPU = memoryBlockMergingReuseMaybe

memoryBlockMergingReuseMaybeGPU :: Pipeline ExplicitMemory ExplicitMemory
memoryBlockMergingReuseMaybeGPU = memoryBlockMergingReuseMaybe


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
  sequentialPipeline >>>
  onePass explicitAllocations >>>
  passes [ simplifyExplicitMemory
         , performCSE False
         , simplifyExplicitMemory
         , doubleBuffer
         , simplifyExplicitMemory
         ] >>>
  memoryBlockMergingCoalescingMaybeCPU >>>
  memoryBlockMergingReuseMaybeCPU

gpuPipeline :: Pipeline SOACS ExplicitMemory
gpuPipeline =
  kernelsPipeline >>>
  onePass explicitAllocations >>>
  passes [ simplifyExplicitMemory
         , performCSE False
         , simplifyExplicitMemory
         , doubleBuffer
         , simplifyExplicitMemory
         , expandAllocations
         , simplifyExplicitMemory
         ] >>>
  memoryBlockMergingCoalescingMaybeGPU >>>
  memoryBlockMergingReuseMaybeGPU
