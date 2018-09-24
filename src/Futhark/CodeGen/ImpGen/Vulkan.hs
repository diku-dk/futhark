module Futhark.CodeGen.ImpGen.Vulkan
  ( compileProg
  ) where

import Futhark.Error
import Futhark.Representation.ExplicitMemory
import qualified Futhark.CodeGen.ImpCode.Vulkan as Vulkan
import qualified Futhark.CodeGen.ImpGen.Kernels as ImpGenKernels
import Futhark.CodeGen.ImpGen.Kernels.ToVulkan
import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m => Prog ExplicitMemory -> m (Either InternalError Vulkan.Program)
compileProg prog = either Left kernelsToVulkan <$> ImpGenKernels.compileProg prog