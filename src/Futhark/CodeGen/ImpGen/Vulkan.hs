module Futhark.CodeGen.ImpGen.Vulkan
  ( compileProg,
    Warnings,
  )
where

import Data.Bifunctor (second)
import qualified Futhark.CodeGen.ImpCode.Vulkan as Vulkan
import Futhark.CodeGen.ImpGen.Kernels
import Futhark.CodeGen.ImpGen.Kernels.ToVulkan
import Futhark.IR.KernelsMem
import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m => Prog KernelsMem -> m (Warnings, Vulkan.Program)
compileProg prog = second kernelsToVulkan <$> compileProgOpenCL prog
