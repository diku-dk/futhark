module Futhark.CodeGen.ImpGen.OpenCL
  ( compileProg,
    Warnings,
  )
where

import Data.Bifunctor (second)
import qualified Futhark.CodeGen.ImpCode.OpenCL as OpenCL
import Futhark.CodeGen.ImpGen.Kernels
import Futhark.CodeGen.ImpGen.Kernels.ToOpenCL
import Futhark.IR.KernelsMem
import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m => Prog KernelsMem -> m (Warnings, OpenCL.Program)
compileProg prog = second kernelsToOpenCL <$> compileProgOpenCL prog
