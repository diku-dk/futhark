module Futhark.CodeGen.ImpGen.OpenGL
  ( compileProg
  ) where

import Futhark.Error
import Futhark.Representation.ExplicitMemory
import qualified Futhark.CodeGen.ImpCode.OpenGL as OpenGL
import qualified Futhark.CodeGen.ImpGen.Kernels as ImpGenKernels
import Futhark.CodeGen.ImpGen.Kernels.ToOpenGL
import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m => Prog ExplicitMemory -> m (Either InternalError OpenGL.Program)
compileProg prog = either Left kernelsToOpenGL <$> ImpGenKernels.compileProg prog
