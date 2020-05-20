{-# LANGUAGE FlexibleContexts #-}
-- | Optimisation pipelines.
module Futhark.Passes
  ( standardPipeline
  , sequentialPipeline
  , kernelsPipeline
  , sequentialCpuPipeline
  , gpuPipeline
  , mcPipeline
  , multicorePipeline
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
import qualified Futhark.Pass.ExplicitAllocations.MC as MC
import Futhark.Pass.ExtractKernels
import Futhark.Pass.ExtractMulticore
import Futhark.Pass.FirstOrderTransform
import Futhark.Pass.KernelBabysitting
import Futhark.Pass.ResolveAssertions
import Futhark.Pass.Simplify
import Futhark.Pipeline
import Futhark.IR.KernelsMem (KernelsMem)
import Futhark.IR.SeqMem (SeqMem)
import Futhark.IR.MCMem (MCMem)
import Futhark.IR.Kernels (Kernels)
import Futhark.IR.Seq (Seq)
import Futhark.IR.MC (MC)
import Futhark.IR.SOACS (SOACS)

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
         , unstreamKernels
         , performCSE True
         , simplifyKernels
         , sinkKernels
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

mcPipeline :: Pipeline SOACS MC
mcPipeline =
  standardPipeline >>>
  onePass extractMulticore >>>
  passes [ simplifyMC
         , unstreamMC
         , performCSE True
         , simplifyMC
         , sinkMC
         , inPlaceLoweringMC
         ]

multicorePipeline :: Pipeline SOACS MCMem
multicorePipeline =
  mcPipeline >>>
  onePass MC.explicitAllocations >>>
  passes [ simplifyMCMem
         , performCSE False
         , simplifyMCMem
         ]
