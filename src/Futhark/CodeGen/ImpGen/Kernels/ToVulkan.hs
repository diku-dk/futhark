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
kernelsToVulkan (ImpKernels.Functions funs) =
  let (spirv_code, _) = SPIRV.runCompilerM SPIRV.newCompilerState $ kernelsToSPIRV funs
  in fail "Not implemented"

kernelsToSPIRV :: [(ImpVulkan.Name, ImpCode.Function ImpKernels.HostOp)] -> SPIRV.CompilerM [Word32]
kernelsToSPIRV funs = do 
  mapM_ (\(name, fun) -> traverse kernelToSPIRV fun) funs
  SPIRV.makePrelude
  SPIRV.getResult

kernelToSPIRV :: ImpKernels.HostOp -> SPIRV.CompilerM ()
kernelToSPIRV fun = return ()