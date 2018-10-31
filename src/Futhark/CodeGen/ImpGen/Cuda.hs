{-# LANGUAGE QuasiQuotes #-}
module Futhark.CodeGen.ImpGen.Cuda
  ( compileProg
  ) where

import Futhark.Error
import Futhark.Representation.ExplicitMemory
import qualified Futhark.CodeGen.ImpCode.OpenCL as OpenCL
import qualified Futhark.CodeGen.ImpGen.Kernels as ImpGenKernels
import Futhark.CodeGen.ImpGen.Kernels.ToOpenCL
import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m => Prog ExplicitMemory
            -> m (Either InternalError OpenCL.Program)
compileProg prog = either Left kernelsToCuda <$> ImpGenKernels.compileProg prog

