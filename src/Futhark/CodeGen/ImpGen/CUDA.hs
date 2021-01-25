-- | Code generation for ImpCode with CUDA kernels.
module Futhark.CodeGen.ImpGen.CUDA
  ( compileProg,
    Warnings,
  )
where

import Data.Bifunctor (second)
import Futhark.CodeGen.ImpCode.OpenCL
import Futhark.CodeGen.ImpGen.Kernels
import Futhark.CodeGen.ImpGen.Kernels.ToOpenCL
import Futhark.IR.KernelsMem
import Futhark.MonadFreshNames

-- | Compile the program to ImpCode with CUDA kernels.
compileProg :: MonadFreshNames m => Prog KernelsMem -> m (Warnings, Program)
compileProg prog = second kernelsToCUDA <$> compileProgCUDA prog
