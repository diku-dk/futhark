-- | Code generation for ImpCode with CUDA kernels.
module Futhark.CodeGen.ImpGen.CUDA
  ( compileProg,
    Warnings,
  )
where

import Data.Bifunctor (second)
import Futhark.CodeGen.ImpCode.OpenCL
import Futhark.CodeGen.ImpGen.GPU
import Futhark.CodeGen.ImpGen.GPU.ToOpenCL
import Futhark.IR.GPUMem
import Futhark.MonadFreshNames

-- | Compile the program to ImpCode with CUDA kernels.
compileProg :: (MonadFreshNames m) => Prog GPUMem -> m (Warnings, Program)
compileProg prog = second kernelsToCUDA <$> compileProgCUDA prog
