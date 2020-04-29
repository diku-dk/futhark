module Futhark.CodeGen.ImpGen.OpenCL
  ( compileProg
  ) where

import Futhark.Representation.ExplicitMemory
import qualified Futhark.CodeGen.ImpCode.OpenCL as OpenCL
import qualified Futhark.CodeGen.ImpGen.Kernels as ImpGenKernels
import Futhark.CodeGen.ImpGen.Kernels.ToOpenCL
import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m => Prog ExplicitMemory -> m OpenCL.Program
compileProg prog = kernelsToOpenCL <$> ImpGenKernels.compileProgOpenCL prog
