module Futhark.CodeGen.ImpGen.CUDA
  ( compileProg
  ) where

import Futhark.Representation.KernelsMem
import qualified Futhark.CodeGen.ImpCode.OpenCL as OpenCL
import qualified Futhark.CodeGen.ImpGen.Kernels as ImpGenKernels
import Futhark.CodeGen.ImpGen.Kernels.ToOpenCL
import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m => Prog KernelsMem -> m OpenCL.Program
compileProg prog = kernelsToCUDA <$> ImpGenKernels.compileProgCUDA prog
