{-# LANGUAGE FlexibleContexts #-}
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
import Futhark.Optimise.Sink
import Futhark.Optimise.TileLoops
import Futhark.Optimise.DoubleBuffer
import Futhark.Optimise.Unstream
import Futhark.Pass.ExpandAllocations
import qualified Futhark.Pass.ExplicitAllocations.Kernels as Kernels
import qualified Futhark.Pass.ExplicitAllocations.Seq as Seq
import Futhark.Pass.ExtractKernels
import Futhark.Pass.FirstOrderTransform
import Futhark.Pass.KernelBabysitting
import Futhark.Pass.ResolveAssertions
import Futhark.Pass.Simplify
import Futhark.Pipeline
import Futhark.Representation.KernelsMem (KernelsMem)
import Futhark.Representation.SeqMem (SeqMem)
import Futhark.Representation.Kernels (Kernels)
import Futhark.Representation.Seq (Seq)
import Futhark.Representation.SOACS (SOACS)

standardPipeline :: Pipeline SOACS SOACS
standardPipeline =
  passes [ simplifySOACS
         , inlineFunctions
         , simplifySOACS
         , performCSE True
         , simplifySOACS
           -- We run fusion twice
         , fuseSOACs
         , performCSE True
         , simplifySOACS
         , fuseSOACs
         , performCSE True
         , simplifySOACS
         , resolveAssertions
         , removeDeadFunctions
         ]

kernelsPipeline :: Pipeline SOACS Kernels
kernelsPipeline =
  standardPipeline >>>
  onePass extractKernels >>>
  passes [ simplifyKernels
         , babysitKernels
         , tileLoops
         , unstream
         , performCSE True
         , simplifyKernels
         , sink
         , inPlaceLoweringKernels
         ]

sequentialPipeline :: Pipeline SOACS Seq
sequentialPipeline =
  standardPipeline >>>
  onePass firstOrderTransform >>>
  passes [ simplifySeq
         , inPlaceLoweringSeq
         ]

sequentialCpuPipeline :: Pipeline SOACS SeqMem
sequentialCpuPipeline =
  sequentialPipeline >>>
  onePass Seq.explicitAllocations >>>
  passes [ performCSE False
         , simplifySeqMem
         , simplifySeqMem
         ]

gpuPipeline :: Pipeline SOACS KernelsMem
gpuPipeline =
  kernelsPipeline >>>
  onePass Kernels.explicitAllocations >>>
  passes [ simplifyKernelsMem
         , performCSE False
         , simplifyKernelsMem
         , doubleBuffer
         , simplifyKernelsMem
         , expandAllocations
         , simplifyKernelsMem
         ]
