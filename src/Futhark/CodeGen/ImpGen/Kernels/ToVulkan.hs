{-# LANGUAGE TupleSections #-}
-- | This module defines a translation from imperative code with
-- kernels to imperative code with Vulkan calls.
module Futhark.CodeGen.ImpGen.Kernels.ToVulkan
  ( kernelsToVulkan
  )
  where
    
import Futhark.Error
import Futhark.CodeGen.ImpCode.Kernels hiding (Program)
import Futhark.Representation.AST.Attributes.Types
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
getKernelName (Map kernel)                        = pretty $ mapKernelThreadNum kernel
getKernelName (AnyKernel kernel)                  = pretty $ kernelName kernel
getKernelName (MapTranspose bt _ _ _ _ _ _ _ _ _) = ImpVulkan.transposeEntryPointName bt

numGroups :: CallKernel -> Exp
numGroups (Map kernel)       = sizeToExp (mapKernelNumGroups kernel)
numGroups (AnyKernel kernel) = sizeToExp (kernelNumGroups kernel)
numGroups (MapTranspose _ _ _ _ _ _ _ _ in_elems _) =
  let impDiv  = BinOpExp $ SDiv Int32
  in ((in_elems - 1) `impDiv` ImpVulkan.transposeGroupSize) + 1

getArgs :: CallKernel -> [ImpVulkan.EntryPointArg]
getArgs (Map kernel)       = mapMaybe getArg $ mapKernelUses kernel
getArgs (AnyKernel kernel) = mapMaybe getArg $ kernelUses kernel
getArgs (MapTranspose _ destmem destoffset srcmem srcoffset _ x_elems y_elems in_elems _) = 
  [ ImpVulkan.MemKArg destmem
  , ImpVulkan.ValueKArg destoffset int32
  , ImpVulkan.MemKArg srcmem
  , ImpVulkan.ValueKArg srcoffset int32
  , ImpVulkan.ValueKArg x_elems int32
  , ImpVulkan.ValueKArg y_elems int32
  , ImpVulkan.ValueKArg in_elems int32
  ]

getArg :: KernelUse -> Maybe ImpVulkan.EntryPointArg
getArg (MemoryUse v _)  = Just $ ImpVulkan.MemKArg v
getArg (ScalarUse v bt) = Just $ ImpVulkan.ValueKArg (LeafExp (ScalarVar v) bt) bt
getArg _                = Nothing

getKernelWorkgroupSizeExp :: CallKernel -> ImpVulkan.SpecConstExp
getKernelWorkgroupSizeExp (Map k)        = ImpVulkan.SpecConstSizeExp $ mapKernelGroupSize k
getKernelWorkgroupSizeExp (AnyKernel k)  = ImpVulkan.SpecConstSizeExp $ kernelGroupSize k
getKernelWorkgroupSizeExp MapTranspose{} =
  ImpVulkan.SpecConstSizeExp $ ConstSize ImpVulkan.transposeGroupSize

getReservedSpecExpr :: CallKernel -> SPIRV.ReservedSpec -> ImpVulkan.SpecConstExp
getReservedSpecExpr kernel SPIRV.WorkgroupSizeXSpec = getKernelWorkgroupSizeExp kernel
getReservedSpecExpr _ SPIRV.LockstepWidthSpec       = ImpVulkan.SpecConstLockstepWidth

getConstExp :: KernelUse -> Maybe (VName, ImpVulkan.KernelConstExp)
getConstExp (ConstUse vn e) = Just (vn, e)
getConstExp _               = Nothing

getKernelConsts :: CallKernel -> [(VName, ImpVulkan.KernelConstExp)]
getKernelConsts (Map k)        = mapMaybe getConstExp $ mapKernelUses k
getKernelConsts (AnyKernel k)  = mapMaybe getConstExp $ kernelUses k
getKernelConsts MapTranspose{} = []

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
