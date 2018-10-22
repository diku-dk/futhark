-- | This module defines a translation from imperative code with
-- kernels to imperative code with Vulkan calls.
module Futhark.CodeGen.ImpGen.Kernels.ToVulkan
  ( kernelsToVulkan
  )
  where
    
import Futhark.Error
import qualified Futhark.CodeGen.Backends.SPIRV as SPIRV
import qualified Futhark.CodeGen.ImpCode.Kernels as ImpKernels
import Futhark.CodeGen.ImpCode.Kernels hiding (Program)
import qualified Futhark.CodeGen.ImpCode.Vulkan as ImpVulkan
import Futhark.MonadFreshNames

import Data.Word
import qualified Data.Map.Strict as M
import qualified Data.Semigroup as Sem
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.Reader

newtype ToVulkan = ToVulkan { vkSizes :: M.Map VName (SizeClass, Name) }

instance Sem.Semigroup ToVulkan where
  ToVulkan sz1 <> ToVulkan sz2 =
    ToVulkan (sz1<>sz2)

instance Monoid ToVulkan where
  mempty = ToVulkan mempty
  mappend = (Sem.<>)

type OnKernelM = Writer ToVulkan

-- | Translate a kernels-program to a Vulkan/SPIR-V program.
kernelsToVulkan :: ImpKernels.Program -> Either InternalError ImpVulkan.Program
kernelsToVulkan prog =
  return $ SPIRV.runCompilerM SPIRV.newCompilerState $ kernelsToSPIRV prog

kernelsToSPIRV :: ImpKernels.Program -> SPIRV.CompilerM ImpVulkan.Program
kernelsToSPIRV prog@(ImpKernels.Functions funs) = do
  mapM_ SPIRV.compileKernel $ ImpKernels.getKernels prog
  code         <- SPIRV.finalizedProgram
  entry_points <- SPIRV.getEntryPoints
  desc_sets    <- SPIRV.getDescriptorSets
  prog'        <- traverse hostOpToFunction prog
  let (_, ToVulkan sizes) = runWriter $ forM funs $
                              \(fname, fun) -> traverse (vulkanOnHostOp fname) fun
  return $ ImpVulkan.Program code entry_points desc_sets sizes prog'

hostOpToFunction :: HostOp -> SPIRV.CompilerM ImpVulkan.Vulkan
hostOpToFunction (GetSize v key size_class)     = return $ ImpVulkan.GetSize v key
hostOpToFunction (CmpSizeLe v key size_class x) = return $ ImpVulkan.CmpSizeLe v key x
hostOpToFunction (GetSizeMax v size_class)      = return $ ImpVulkan.GetSizeMax v size_class
hostOpToFunction (CallKernel k)                 = do
  desc_set_map <- SPIRV.getDescriptorSets
  let name = getKernelName k
      desc_set_binding = M.findIndex name desc_set_map
      (kernel_size, workgroup_size) = kernelAndWorkgroupSize k
  return $ ImpVulkan.LaunchEntryPoint name desc_set_binding [] kernel_size workgroup_size -- Set params

getKernelName :: CallKernel -> VName
getKernelName (Map kernel)                       = mapKernelThreadNum kernel
getKernelName (AnyKernel kernel)                 = kernelName kernel
getKernelName (MapTranspose _ n _ _ _ _ _ _ _ _) = n -- TODO: fix

kernelAndWorkgroupSize :: CallKernel -> ([Exp], [Exp])
kernelAndWorkgroupSize (Map kernel) =
  ([sizeToExp (mapKernelNumGroups kernel) *
    sizeToExp (mapKernelGroupSize kernel)],
   [sizeToExp $ mapKernelGroupSize kernel])
kernelAndWorkgroupSize (AnyKernel kernel) =
  ([sizeToExp (kernelNumGroups kernel) *
    sizeToExp (kernelGroupSize kernel)],
   [sizeToExp $ kernelGroupSize kernel])
kernelAndWorkgroupSize MapTranspose{} = 
  ([sizeToExp (ConstSize 1)], 
   [sizeToExp (ConstSize 1)]) -- TODO: fix

vulkanOnHostOp :: Name -> HostOp -> OnKernelM ()
vulkanOnHostOp fname (GetSize v key size_class)     =
  void $ tell mempty { vkSizes = M.singleton key (size_class, fname) }
vulkanOnHostOp fname (CmpSizeLe v key size_class x) =
  void $ tell mempty { vkSizes = M.singleton key (size_class, fname) }
vulkanOnHostOp _ _                                  = return ()
