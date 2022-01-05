{-# LANGUAGE FlexibleContexts #-}

-- | Optimisation pipelines.
module Futhark.Passes
  ( standardPipeline,
    sequentialPipeline,
    kernelsPipeline,
    sequentialCpuPipeline,
    gpuPipeline,
    mcPipeline,
    multicorePipeline,
  )
where

import Control.Category ((>>>))
import Futhark.IR.GPU (GPU)
import Futhark.IR.GPUMem (GPUMem)
import Futhark.IR.MC (MC)
import Futhark.IR.MCMem (MCMem)
import Futhark.IR.SOACS (SOACS)
import Futhark.IR.Seq (Seq)
import Futhark.IR.SeqMem (SeqMem)
import Futhark.Optimise.CSE
import Futhark.Optimise.DoubleBuffer
import Futhark.Optimise.Fusion
import Futhark.Optimise.GenRedOpt
import Futhark.Optimise.HistAccs
import Futhark.Optimise.InPlaceLowering
import Futhark.Optimise.InliningDeadFun
import qualified Futhark.Optimise.MemoryBlockMerging as MemoryBlockMerging
import Futhark.Optimise.Sink
import Futhark.Optimise.TileLoops
import Futhark.Optimise.Unstream
import Futhark.Pass.AD
import Futhark.Pass.ExpandAllocations
import qualified Futhark.Pass.ExplicitAllocations.GPU as GPU
import qualified Futhark.Pass.ExplicitAllocations.MC as MC
import qualified Futhark.Pass.ExplicitAllocations.Seq as Seq
import Futhark.Pass.ExtractKernels
import Futhark.Pass.ExtractMulticore
import Futhark.Pass.FirstOrderTransform
import Futhark.Pass.KernelBabysitting
import Futhark.Pass.Simplify
import Futhark.Pipeline

standardPipeline :: Pipeline SOACS SOACS
standardPipeline =
  passes
    [ simplifySOACS,
      inlineConservatively,
      simplifySOACS,
      inlineAggressively,
      simplifySOACS,
      performCSE True,
      simplifySOACS,
      -- We run fusion twice
      fuseSOACs,
      performCSE True,
      simplifySOACS,
      fuseSOACs,
      performCSE True,
      simplifySOACS,
      removeDeadFunctions,
      -- TODO: we sould skip the AD pass and its subsequent extra
      -- optimisation passes if the program contains no AD constructs.
      algebraicDifferentiation,
      simplifySOACS,
      performCSE True,
      fuseSOACs,
      performCSE True,
      simplifySOACS,
      fuseSOACs,
      simplifySOACS
    ]

kernelsPipeline :: Pipeline SOACS GPU
kernelsPipeline =
  standardPipeline
    >>> onePass extractKernels
    >>> passes
      [ simplifyGPU,
        optimiseGenRed,
        simplifyGPU,
        histAccsGPU,
        babysitKernels,
        tileLoops,
        simplifyGPU,
        babysitKernels,
        simplifyGPU,
        unstreamGPU,
        performCSE True,
        simplifyGPU,
        sinkGPU,
        inPlaceLoweringGPU
      ]

sequentialPipeline :: Pipeline SOACS Seq
sequentialPipeline =
  standardPipeline
    >>> onePass firstOrderTransform
    >>> passes
      [ simplifySeq,
        inPlaceLoweringSeq
      ]

sequentialCpuPipeline :: Pipeline SOACS SeqMem
sequentialCpuPipeline =
  sequentialPipeline
    >>> onePass Seq.explicitAllocations
    >>> passes
      [ performCSE False,
        simplifySeqMem
      ]

gpuPipeline :: Pipeline SOACS GPUMem
gpuPipeline =
  kernelsPipeline
    >>> onePass GPU.explicitAllocations
    >>> passes
      [ simplifyGPUMem,
        performCSE False,
        simplifyGPUMem,
        doubleBufferGPU,
        simplifyGPUMem,
        MemoryBlockMerging.optimise,
        simplifyGPUMem,
        expandAllocations,
        simplifyGPUMem
      ]

mcPipeline :: Pipeline SOACS MC
mcPipeline =
  standardPipeline
    >>> onePass extractMulticore
    >>> passes
      [ simplifyMC,
        unstreamMC,
        performCSE True,
        simplifyMC,
        sinkMC,
        inPlaceLoweringMC
      ]

multicorePipeline :: Pipeline SOACS MCMem
multicorePipeline =
  mcPipeline
    >>> onePass MC.explicitAllocations
    >>> passes
      [ simplifyMCMem,
        performCSE False,
        simplifyMCMem,
        doubleBufferMC,
        simplifyMCMem
      ]
