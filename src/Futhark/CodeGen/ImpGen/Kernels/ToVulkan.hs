-- | This module defines a translation from imperative code with
-- kernels to imperative code with Vulkan calls.
module Futhark.CodeGen.ImpGen.Kernels.ToVulkan
  ( kernelsToVulkan
  )
  where
    
import Futhark.Error
import qualified Futhark.CodeGen.OpenCL.Kernels as Kernels
import qualified Futhark.CodeGen.Backends.GenericC as GenericC
import qualified Futhark.CodeGen.Backends.SPIRV as SPIRV
import qualified Futhark.CodeGen.ImpCode.Kernels as ImpKernels
import qualified Futhark.CodeGen.ImpCode.Vulkan as ImpVulkan
import qualified Futhark.CodeGen.ImpCode as ImpCode

import Data.Word

-- | Translate a kernels-program to a Vulkan/SPIR-V program.
kernelsToVulkan :: ImpKernels.Program -> Either InternalError ImpVulkan.Program
kernelsToVulkan prog =
  let ((spirv_code, entry_points), _) = SPIRV.runCompilerM SPIRV.newCompilerState $ kernelsToSPIRV prog
  in return $ ImpVulkan.Program spirv_code entry_points $ ImpVulkan.Functions []

kernelsToSPIRV :: ImpKernels.Program -> SPIRV.CompilerM ([Word32], [ImpVulkan.EntryPointName])
kernelsToSPIRV prog = do 
  mapM_ SPIRV.compileKernel $ ImpKernels.getKernels prog
  code <- SPIRV.finalizedProgram
  entry_points <- SPIRV.getEntryPoints
  let (entry_names, _) = unzip entry_points
  return (code, entry_names)