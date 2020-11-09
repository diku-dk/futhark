{-# LANGUAGE TupleSections #-}

-- | This module defines a translation from imperative code with
-- kernels to imperative code with Vulkan calls.
module Futhark.CodeGen.ImpGen.Kernels.ToVulkan
  ( kernelsToVulkan,
  )
where

import Control.Monad.Identity
import Control.Monad.Writer
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Semigroup as Sem
import qualified Futhark.CodeGen.Backends.SPIRV as SPIRV
import Futhark.CodeGen.ImpCode.Kernels hiding (Program)
import qualified Futhark.CodeGen.ImpCode.Kernels as ImpKernels
import qualified Futhark.CodeGen.ImpCode.Vulkan as ImpVulkan
import Futhark.IR.Prop.Constants

newtype ToVulkan = ToVulkan {vkSizes :: M.Map Name (SizeClass, Name)}

instance Sem.Semigroup ToVulkan where
  ToVulkan sz1 <> ToVulkan sz2 =
    ToVulkan (sz1 <> sz2)

instance Monoid ToVulkan where
  mempty = ToVulkan mempty
  mappend = (Sem.<>)

type OnKernelM = Writer ToVulkan

-- | Translate a kernels-program to a Vulkan/SPIR-V program.
kernelsToVulkan :: ImpKernels.Program -> ImpVulkan.Program
kernelsToVulkan
  prog@( ImpKernels.Definitions
           (ImpKernels.Constants ps consts)
           (ImpKernels.Functions funs)
         ) =
    ImpVulkan.Program shaders sizes $
      -- FIXME: throwing away top-level constants here
      ImpVulkan.Definitions
        (ImpKernels.Constants mempty mempty)
        prog'
    where
      shaders = map SPIRV.kernelToShader $ getKernels prog
      (prog', ToVulkan sizes) = runWriter $
        fmap Functions $
          forM funs $
            \(fname, fun) -> (fname,) <$> traverse (vulkanOnHostOp fname) fun

getKernels :: Definitions HostOp -> [Kernel]
getKernels
  ( ImpKernels.Definitions
      (ImpKernels.Constants _ps consts)
      (ImpKernels.Functions funs)
    ) =
    foldMap getKernel consts
      <> foldMap (foldMap getKernel . functionBody . snd) funs
    where
      getKernel (ImpKernels.CallKernel k) = [k]
      getKernel _ = []

numGroups :: Kernel -> ImpVulkan.WorkGroups
numGroups kernel =
  case kernelNumGroups kernel of
    [x, y, z] ->
      (x, y, z)
    [x, y] ->
      (x, y, untyped (1 :: TExp Int32))
    [x] ->
      ( x,
        untyped (1 :: TExp Int32),
        untyped (1 :: TExp Int32)
      )
    _ ->
      ( untyped (1 :: TExp Int32),
        untyped (1 :: TExp Int32),
        untyped (1 :: TExp Int32)
      )

getArgs :: Kernel -> [ImpVulkan.EntryPointArg]
getArgs kernel = mapMaybe getArg $ kernelUses kernel
  where
    getArg (MemoryUse v) = Just $ ImpVulkan.MemKArg v
    getArg (ScalarUse v bt) = Just $ ImpVulkan.ValueKArg (LeafExp (ScalarVar v) bt) bt
    getArg _ = Nothing

getKernelWorkgroupSizeExp :: Kernel -> Int -> ImpVulkan.SpecConstExp
getKernelWorkgroupSizeExp k i =
  let gs = kernelGroupSize k
   in if i < length gs
        then ImpVulkan.SpecConstExp $ gs !! i
        else ImpVulkan.SpecConstSizeExp $ constant (1 :: Int32)

getReservedSpecExpr :: Kernel -> SPIRV.ReservedSpec -> ImpVulkan.SpecConstExp
getReservedSpecExpr kernel SPIRV.WorkgroupSizeXSpec = getKernelWorkgroupSizeExp kernel 0
getReservedSpecExpr kernel SPIRV.WorkgroupSizeYSpec = getKernelWorkgroupSizeExp kernel 1
getReservedSpecExpr kernel SPIRV.WorkgroupSizeZSpec = getKernelWorkgroupSizeExp kernel 2
getReservedSpecExpr _ SPIRV.LockstepWidthSpec = ImpVulkan.SpecConstLockstepWidth

getConstExp :: KernelUse -> Maybe (VName, ImpVulkan.KernelConstExp)
getConstExp (ConstUse vn e) = Just (vn, e)
getConstExp _ = Nothing

getKernelConsts :: Kernel -> [(VName, ImpVulkan.KernelConstExp)]
getKernelConsts k = mapMaybe getConstExp $ kernelUses k

type LocalMemoryUse = (VName, KernelConstExp)

kernelLocalMemory :: Kernel -> [LocalMemoryUse]
kernelLocalMemory =
  error $
    unlines
      [ "Vulkan kernelLocalMemory: not implemented ATM",
        "start with making this return an empty list"
      ]

getLocalMemConsts :: Kernel -> [ImpVulkan.SpecConstExp]
getLocalMemConsts k =
  map (ImpVulkan.SpecConstLocalMemExp . snd) $ kernelLocalMemory k

getSpecConstExps :: Kernel -> [ImpVulkan.SpecConstExp]
getSpecConstExps kernel =
  let res_exprs = map (getReservedSpecExpr kernel) SPIRV.reservedSpecList
      const_exprs = map (uncurry ImpVulkan.SpecConstKernelExp) $ getKernelConsts kernel
      mem_exprs = getLocalMemConsts kernel
   in res_exprs ++ const_exprs ++ mem_exprs

vulkanOnHostOp :: Name -> HostOp -> OnKernelM ImpVulkan.Vulkan
vulkanOnHostOp fname (GetSize v key size_class) = do
  void $ tell mempty {vkSizes = M.singleton key (size_class, fname)}
  return $ ImpVulkan.GetSize v key
vulkanOnHostOp fname (CmpSizeLe v key size_class x) = do
  tell mempty {vkSizes = M.singleton key (size_class, fname)}
  return $ ImpVulkan.CmpSizeLe v key x
vulkanOnHostOp _ (GetSizeMax v size_class) =
  return $ ImpVulkan.GetSizeMax v size_class
vulkanOnHostOp _ (CallKernel k) =
  let name = pretty $ kernelName k
      args = getArgs k
      spec_exps = getSpecConstExps k
      num_groups = numGroups k
   in return $ ImpVulkan.LaunchEntryPoint name args spec_exps num_groups
