-- | Code generation for ImpCode with HIP kernels.
module Futhark.CodeGen.ImpGen.HIP
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

-- | Compile the program to ImpCode with HIP kernels.
compileProg :: (MonadFreshNames m) => Prog GPUMem -> m (Warnings, Program)
compileProg prog = second kernelsToHIP <$> compileProgHIP prog
