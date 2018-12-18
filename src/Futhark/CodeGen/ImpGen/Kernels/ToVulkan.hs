{-# LANGUAGE TupleSections #-}
-- | This module defines a translation from imperative code with
-- kernels to imperative code with Vulkan calls.
module Futhark.CodeGen.ImpGen.Kernels.ToVulkan
  ( kernelsToVulkan
  )
  where
    
import Futhark.Error
import Futhark.CodeGen.ImpCode.Kernels hiding (Program)
import qualified Futhark.CodeGen.Backends.SPIRV as SPIRV
import qualified Futhark.CodeGen.ImpCode.Kernels as ImpKernels
import qualified Futhark.CodeGen.ImpCode.Vulkan as ImpVulkan

import qualified Data.Map.Strict as M
import qualified Data.Semigroup as Sem
import Data.Maybe
import Control.Monad.Identity
import Control.Monad.Writer

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
kernelsToVulkan prog@(ImpKernels.Functions funs) = return $ ImpVulkan.Program shaders sizes prog'
  where shaders = map SPIRV.kernelToShader $ ImpKernels.getKernels prog
        (prog', ToVulkan sizes) = runWriter $ fmap Functions $ forM funs $
            \(fname, fun) -> (fname,) <$> traverse (vulkanOnHostOp fname) fun

getKernelName :: CallKernel -> ImpVulkan.EntryPointName
getKernelName (Map kernel)       = pretty $ mapKernelThreadNum kernel
getKernelName (AnyKernel kernel) = pretty $ kernelName kernel

numGroups :: CallKernel -> ImpVulkan.WorkGroups
numGroups (Map kernel)       = (sizeToExp $ mapKernelNumGroups kernel,
                                sizeToExp $ ConstSize 1,
                                sizeToExp $ ConstSize 1)
numGroups (AnyKernel kernel) =
  case kernelNumGroups kernel of
    [x, y, z] -> (x, y, z)
    [x, y]    -> (x, y, sizeToExp $ ConstSize 1)
    [x]       -> (x, sizeToExp $ ConstSize 1, sizeToExp $ ConstSize 1)
    _         -> (sizeToExp $ ConstSize 1, sizeToExp $ ConstSize 1, sizeToExp $ ConstSize 1)

getArgs :: CallKernel -> [ImpVulkan.EntryPointArg]
getArgs (Map kernel)       = mapMaybe getArg $ mapKernelUses kernel
getArgs (AnyKernel kernel) = mapMaybe getArg $ kernelUses kernel

getArg :: KernelUse -> Maybe ImpVulkan.EntryPointArg
getArg (MemoryUse v)    = Just $ ImpVulkan.MemKArg v
getArg (ScalarUse v bt) = Just $ ImpVulkan.ValueKArg (LeafExp (ScalarVar v) bt) bt
getArg _                = Nothing

getKernelWorkgroupSizeExp :: CallKernel -> Int -> ImpVulkan.SpecConstExp
getKernelWorkgroupSizeExp (Map k) 0        = ImpVulkan.SpecConstSizeExp $ mapKernelGroupSize k
getKernelWorkgroupSizeExp (Map _) _        = ImpVulkan.SpecConstSizeExp $ ConstSize 1
getKernelWorkgroupSizeExp (AnyKernel k) i  = 
  let gs = kernelGroupSize k
  in if i < length gs then ImpVulkan.SpecConstExp $ gs!!i
                      else ImpVulkan.SpecConstSizeExp $ ConstSize 1

getReservedSpecExpr :: CallKernel -> SPIRV.ReservedSpec -> ImpVulkan.SpecConstExp
getReservedSpecExpr kernel SPIRV.WorkgroupSizeXSpec = getKernelWorkgroupSizeExp kernel 0
getReservedSpecExpr kernel SPIRV.WorkgroupSizeYSpec = getKernelWorkgroupSizeExp kernel 1
getReservedSpecExpr kernel SPIRV.WorkgroupSizeZSpec = getKernelWorkgroupSizeExp kernel 2
getReservedSpecExpr _ SPIRV.LockstepWidthSpec       = ImpVulkan.SpecConstLockstepWidth

getConstExp :: KernelUse -> Maybe (VName, ImpVulkan.KernelConstExp)
getConstExp (ConstUse vn e) = Just (vn, e)
getConstExp _               = Nothing

getKernelConsts :: CallKernel -> [(VName, ImpVulkan.KernelConstExp)]
getKernelConsts (Map k)        = mapMaybe getConstExp $ mapKernelUses k
getKernelConsts (AnyKernel k)  = mapMaybe getConstExp $ kernelUses k

getLocalMemConsts :: CallKernel -> [ImpVulkan.SpecConstExp]
getLocalMemConsts (AnyKernel k) =
  map (ImpVulkan.SpecConstLocalMemExp . snd) $ kernelLocalMemory k
getLocalMemConsts _             = []

getSpecConstExps :: CallKernel -> [ImpVulkan.SpecConstExp]
getSpecConstExps kernel = 
  let res_exprs   = map (getReservedSpecExpr kernel) SPIRV.reservedSpecList
      const_exprs = map (uncurry ImpVulkan.SpecConstKernelExp) $ getKernelConsts kernel
      mem_exprs   = getLocalMemConsts kernel
  in res_exprs ++ const_exprs ++ mem_exprs

vulkanOnHostOp :: Name -> HostOp -> OnKernelM ImpVulkan.Vulkan
vulkanOnHostOp fname (GetSize v key size_class)     = do
  void $ tell mempty { vkSizes = M.singleton key (size_class, fname) }
  return $ ImpVulkan.GetSize v key
vulkanOnHostOp fname (CmpSizeLe v key size_class x) = do
  tell mempty { vkSizes = M.singleton key (size_class, fname) }
  return $ ImpVulkan.CmpSizeLe v key x
vulkanOnHostOp _ (GetSizeMax v size_class)          =
  return $ ImpVulkan.GetSizeMax v size_class
vulkanOnHostOp _ (CallKernel k)                     =
  let name = getKernelName k
      args = getArgs k
      spec_exps = getSpecConstExps k
      num_groups = numGroups k
  in return $ ImpVulkan.LaunchEntryPoint name args spec_exps num_groups
