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
import qualified Data.Semigroup as Sem
import Futhark.CodeGen.Backends.SPIRV (kernelToShader)
import Futhark.CodeGen.ImpCode.Kernels hiding (Program)
import qualified Futhark.CodeGen.ImpCode.Kernels as ImpKernels
import qualified Futhark.CodeGen.ImpCode.Vulkan as ImpVulkan

data ToVulkan = ToVulkan
  { vkSizes :: M.Map Name SizeClass,
    vkShaders :: [ImpVulkan.SingleEntryShader]
  }

instance Sem.Semigroup ToVulkan where
  ToVulkan sz1 shaders1 <> ToVulkan sz2 shaders2 =
    ToVulkan (sz1 <> sz2) (shaders1 <> shaders2)

instance Monoid ToVulkan where
  mempty = ToVulkan mempty []
  mappend = (Sem.<>)

type OnKernelM = Writer ToVulkan

-- | Translate a kernels-program to a Vulkan/SPIR-V program.
kernelsToVulkan :: ImpKernels.Program -> ImpVulkan.Program
kernelsToVulkan (ImpKernels.Definitions
                  (ImpKernels.Constants ps consts)
                  (ImpKernels.Functions funs))
  = let ( prog',
          ToVulkan sizes shaders
          ) =
            runWriter $ do
              consts' <- traverse onHostOp consts
              funs' <- forM funs $ \(fname, fun) ->
                          (fname,) <$> traverse onHostOp fun
              return $
                ImpVulkan.Definitions
                  (ImpKernels.Constants ps consts')
                  (ImpKernels.Functions funs')
    in ImpVulkan.Program shaders sizes prog'

getNumGroups :: Kernel -> (Exp, Exp, Exp)
getNumGroups kernel =
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
    _ -> error "More than three dimensions of workgroups"

-- getArgs :: Kernel -> [ImpVulkan.EntryPointArg]
-- getArgs kernel = mapMaybe getArg $ kernelUses kernel
--   where
--     getArg (MemoryUse v) = Just $ ImpVulkan.MemKArg v
--     getArg (ScalarUse v bt) = Just $ ImpVulkan.ValueKArg (LeafExp (ScalarVar v) bt) bt
--     getArg _ = Nothing
-- 
-- getKernelWorkgroupSizeExp :: Kernel -> Int -> ImpVulkan.SpecConstExp
-- getKernelWorkgroupSizeExp k i =
--   let gs = kernelGroupSize k
--    in if i < length gs
--         then ImpVulkan.SpecConstExp $ gs !! i
--         else ImpVulkan.SpecConstSizeExp $ constant (1 :: Int32) -- constant is from IR.Prop.Constant(s?)
-- 
-- getReservedSpecExpr :: Kernel -> SPIRV.ReservedSpec -> ImpVulkan.SpecConstExp
-- getReservedSpecExpr kernel SPIRV.WorkgroupSizeXSpec = getKernelWorkgroupSizeExp kernel 0
-- getReservedSpecExpr kernel SPIRV.WorkgroupSizeYSpec = getKernelWorkgroupSizeExp kernel 1
-- getReservedSpecExpr kernel SPIRV.WorkgroupSizeZSpec = getKernelWorkgroupSizeExp kernel 2
-- getReservedSpecExpr _ SPIRV.LockstepWidthSpec = ImpVulkan.SpecConstLockstepWidth
-- 
-- getConstExp :: KernelUse -> Maybe (VName, ImpVulkan.KernelConstExp)
-- getConstExp (ConstUse vn e) = Just (vn, e)
-- getConstExp _ = Nothing
-- 
-- getKernelConsts :: Kernel -> [(VName, ImpVulkan.KernelConstExp)]
-- getKernelConsts k = mapMaybe getConstExp $ kernelUses k
-- 
-- type LocalMemoryUse = (VName, Count Bytes Exp)
-- 
-- kernelLocalMemory :: KernelCode -> [LocalMemoryUse]
-- kernelLocalMemory (lc :>>: rc) = kernelLocalMemory lc ++ kernelLocalMemory rc
-- kernelLocalMemory (For _ _ body) = kernelLocalMemory body
-- kernelLocalMemory (While _ body) = kernelLocalMemory body
-- kernelLocalMemory (If _ tbranch fbranch) = kernelLocalMemory tbranch ++ kernelLocalMemory fbranch
-- kernelLocalMemory (Comment _ c) = kernelLocalMemory c
-- kernelLocalMemory (Op (LocalAlloc v (Count c))) = [(v, Count $ untyped c)]
-- kernelLocalMemory _ = []
-- 
-- getLocalMemConsts :: Kernel -> [ImpVulkan.SpecConstExp]
-- getLocalMemConsts k =
--   map (ImpVulkan.SpecConstLocalMemExp . snd) $ kernelLocalMemory (kernelBody k)
-- 
-- getSpecConstExps :: Kernel -> [ImpVulkan.SpecConstExp]
-- getSpecConstExps kernel =
--   let res_exprs = map (getReservedSpecExpr kernel) SPIRV.reservedSpecList
--       const_exprs = map (uncurry ImpVulkan.SpecConstKernelExp) $ getKernelConsts kernel
--       mem_exprs = getLocalMemConsts kernel
--    in res_exprs ++ const_exprs ++ mem_exprs

----------------------------------------

onHostOp :: HostOp -> OnKernelM ImpVulkan.Vulkan
onHostOp (GetSize v key size_class) = do
  tell mempty {vkSizes = M.singleton key size_class}
  return $ ImpVulkan.GetSize v key
onHostOp (CmpSizeLe v key size_class x) = do
  tell mempty {vkSizes = M.singleton key size_class}
  return $ ImpVulkan.CmpSizeLe v key x
onHostOp (GetSizeMax v size_class) =
  return $ ImpVulkan.GetSizeMax v size_class
onHostOp (CallKernel k) = do
  let (seShader, args, specExps) = kernelToShader k
  tell mempty {vkShaders = [seShader]}
  return $ ImpVulkan.LaunchEntryPoint name args specExps numGroups
  where
    name = pretty $ kernelName k
    numGroups = getNumGroups k

