-- | Code generation for ImpCode with OpenCL kernels.
module Futhark.CodeGen.ImpGen.OpenCL
  ( compileProg,
    Warnings,
  )
where

import Data.Bifunctor (second)
import Futhark.CodeGen.ImpCode.OpenCL qualified as OpenCL
import Futhark.CodeGen.ImpGen.GPU
import Futhark.CodeGen.ImpGen.GPU.ToOpenCL
import Futhark.IR.GPUMem
import Futhark.MonadFreshNames

-- | Compile the program to ImpCode with OpenCL kernels.
compileProg :: (MonadFreshNames m) => Prog GPUMem -> m (Warnings, OpenCL.Program)
compileProg prog = second kernelsToOpenCL <$> compileProgOpenCL prog
