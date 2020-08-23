{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Compile a 'KernelsMem' program to imperative code with kernels.
-- This is mostly (but not entirely) the same process no matter if we
-- are targeting OpenCL or CUDA.  The important distinctions (the host
-- level code) are introduced later.
module Futhark.CodeGen.ImpGen.Kernels
  ( compileProgOpenCL
  , compileProgCUDA
  , Warnings
  )
  where

import Control.Monad.Except
import Data.Bifunctor (second)
import qualified Data.Map as M
import Data.Maybe
import Data.List (foldl')

import Prelude hiding (quot)

import Futhark.Error
import Futhark.MonadFreshNames
import Futhark.IR.KernelsMem
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import Futhark.CodeGen.ImpCode.Kernels (bytes)
import Futhark.CodeGen.ImpGen hiding (compileProg)
import qualified Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Kernels.Base
import Futhark.CodeGen.ImpGen.Kernels.SegMap
import Futhark.CodeGen.ImpGen.Kernels.SegRed
import Futhark.CodeGen.ImpGen.Kernels.SegScan
import Futhark.CodeGen.ImpGen.Kernels.SegHist
import Futhark.CodeGen.ImpGen.Kernels.Transpose
import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.CodeGen.SetDefaultSpace
import Futhark.Util.IntegralExp (quot, divUp, IntegralExp)

callKernelOperations :: Operations KernelsMem HostEnv Imp.HostOp
callKernelOperations =
  Operations { opsExpCompiler = expCompiler
             , opsCopyCompiler = callKernelCopy
             , opsOpCompiler = opCompiler
             , opsStmsCompiler = defCompileStms
             , opsAllocCompilers = mempty
             }

openclAtomics, cudaAtomics :: AtomicBinOp
(openclAtomics, cudaAtomics) = (flip lookup opencl, flip lookup cuda)
  where opencl = [ (Add Int32 OverflowUndef, Imp.AtomicAdd Int32)
                 , (SMax Int32, Imp.AtomicSMax Int32)
                 , (SMin Int32, Imp.AtomicSMin Int32)
                 , (UMax Int32, Imp.AtomicUMax Int32)
                 , (UMin Int32, Imp.AtomicUMin Int32)
                 , (And Int32, Imp.AtomicAnd Int32)
                 , (Or Int32, Imp.AtomicOr Int32)
                 , (Xor Int32, Imp.AtomicXor Int32)
                 ]
        cuda = opencl ++ [(FAdd Float32, Imp.AtomicFAdd Float32)]

compileProg :: MonadFreshNames m => HostEnv -> Prog KernelsMem
            -> m (Warnings, Imp.Program)
compileProg env prog =
  second (setDefaultSpace (Imp.Space "device")) <$>
  Futhark.CodeGen.ImpGen.compileProg env callKernelOperations (Imp.Space "device") prog

-- | Compile a 'KernelsMem' program to low-level parallel code, with
-- either CUDA or OpenCL characteristics.
compileProgOpenCL, compileProgCUDA
  :: MonadFreshNames m => Prog KernelsMem -> m (Warnings, Imp.Program)
compileProgOpenCL = compileProg $ HostEnv openclAtomics
compileProgCUDA = compileProg $ HostEnv cudaAtomics

opCompiler :: Pattern KernelsMem -> Op KernelsMem
           -> CallKernelGen ()

opCompiler dest (Alloc e space) =
  compileAlloc dest e space

opCompiler (Pattern _ [pe]) (Inner (SizeOp (GetSize key size_class))) = do
  fname <- askFunction
  sOp $ Imp.GetSize (patElemName pe) (keyWithEntryPoint fname key) $
    sizeClassWithEntryPoint fname size_class

opCompiler (Pattern _ [pe]) (Inner (SizeOp (CmpSizeLe key size_class x))) = do
  fname <- askFunction
  let size_class' = sizeClassWithEntryPoint fname size_class
  sOp . Imp.CmpSizeLe (patElemName pe) (keyWithEntryPoint fname key) size_class'
    =<< toExp x

opCompiler (Pattern _ [pe]) (Inner (SizeOp (GetSizeMax size_class))) =
  sOp $ Imp.GetSizeMax (patElemName pe) size_class

opCompiler (Pattern _ [pe]) (Inner (SizeOp (CalcNumGroups w64 max_num_groups_key group_size))) = do
  fname <- askFunction
  max_num_groups <- dPrim "max_num_groups" int32
  sOp $ Imp.GetSize max_num_groups (keyWithEntryPoint fname max_num_groups_key) $
    sizeClassWithEntryPoint fname SizeNumGroups

  -- If 'w' is small, we launch fewer groups than we normally would.
  -- We don't want any idle groups.
  --
  -- The calculations are done with 64-bit integers to avoid overflow
  -- issues.
  let num_groups_maybe_zero = BinOpExp (SMin Int64)
                              (toExp' int64 w64 `divUp`
                               sExt Int64 (toExp' int32 group_size)) $
                              sExt Int64 (Imp.vi32 max_num_groups)
  -- We also don't want zero groups.
  let num_groups = BinOpExp (SMax Int64) 1 num_groups_maybe_zero
  patElemName pe <-- sExt Int32 num_groups

opCompiler dest (Inner (SegOp op)) =
  segOpCompiler dest op

opCompiler pat e =
  compilerBugS $ "opCompiler: Invalid pattern\n  " ++
  pretty pat ++ "\nfor expression\n  " ++ pretty e

sizeClassWithEntryPoint :: Maybe Name -> Imp.SizeClass -> Imp.SizeClass
sizeClassWithEntryPoint fname (Imp.SizeThreshold path def) =
  Imp.SizeThreshold (map f path) def
  where f (name, x) = (keyWithEntryPoint fname name, x)
sizeClassWithEntryPoint _ size_class = size_class

segOpCompiler :: Pattern KernelsMem -> SegOp SegLevel KernelsMem
              -> CallKernelGen ()
segOpCompiler pat (SegMap lvl space _ kbody) =
  compileSegMap pat lvl space kbody
segOpCompiler pat (SegRed lvl@SegThread{} space reds _ kbody) =
  compileSegRed pat lvl space reds kbody
segOpCompiler pat (SegScan lvl@SegThread{} space scans _ kbody) =
  compileSegScan pat lvl space scans kbody
segOpCompiler pat (SegHist (SegThread num_groups group_size _) space ops _ kbody) =
  compileSegHist pat num_groups group_size space ops kbody
segOpCompiler pat segop =
  compilerBugS $ "segOpCompiler: unexpected " ++ pretty (segLevel segop) ++ " for rhs of pattern " ++ pretty pat

-- Create boolean expression that checks whether all kernels in the
-- enclosed code do not use more local memory than we have available.
-- We look at *all* the kernels here, even those that might be
-- otherwise protected by their own multi-versioning branches deeper
-- down.  Currently the compiler will not generate multi-versioning
-- that makes this a problem, but it might in the future.
checkLocalMemoryReqs :: Imp.Code -> CallKernelGen (Maybe Imp.Exp)
checkLocalMemoryReqs code = do
  scope <- askScope
  let alloc_sizes = map (sum . localAllocSizes . Imp.kernelBody) $ getKernels code

  -- If any of the sizes involve a variable that is not known at this
  -- point, then we cannot check the requirements.
  if any (`M.notMember` scope) (namesToList $ freeIn alloc_sizes)
    then return Nothing
    else do
    local_memory_capacity <- dPrim "local_memory_capacity" int32
    sOp $ Imp.GetSizeMax local_memory_capacity SizeLocalMemory

    let local_memory_capacity_64 =
          sExt Int64 $ Imp.vi32 local_memory_capacity
        fits size =
          unCount size .<=. local_memory_capacity_64
    return $ Just $ foldl' (.&&.) true (map fits alloc_sizes)

  where getKernels = foldMap getKernel
        getKernel (Imp.CallKernel k) = [k]
        getKernel _ = []

        localAllocSizes = foldMap localAllocSize
        localAllocSize (Imp.LocalAlloc _ size) = [size]
        localAllocSize _ = []

expCompiler :: ExpCompiler KernelsMem HostEnv Imp.HostOp

-- We generate a simple kernel for itoa and replicate.
expCompiler (Pattern _ [pe]) (BasicOp (Iota n x s et)) = do
  n' <- toExp n
  x' <- toExp x
  s' <- toExp s

  sIota (patElemName pe) n' x' s' et

expCompiler (Pattern _ [pe]) (BasicOp (Replicate _ se)) =
  sReplicate (patElemName pe) se

-- Allocation in the "local" space is just a placeholder.
expCompiler _ (Op (Alloc _ (Space "local"))) =
  return ()

-- This is a multi-versioning If created by incremental flattening.
-- We need to augment the conditional with a check that any local
-- memory requirements in tbranch are compatible with the hardware.
-- We do not check anything for fbranch, as we assume that it will
-- always be safe (and what would we do if none of the branches would
-- work?).
expCompiler dest (If cond tbranch fbranch (IfDec _ IfEquiv)) = do
  tcode <- collect $ compileBody dest tbranch
  fcode <- collect $ compileBody dest fbranch
  check <- checkLocalMemoryReqs tcode
  emit $ case check of
           Nothing -> fcode
           Just ok -> Imp.If (ok .&&. toExp' Bool cond) tcode fcode

expCompiler dest e =
  defCompileExp dest e

callKernelCopy :: CopyCompiler KernelsMem HostEnv Imp.HostOp
callKernelCopy bt
  destloc@(MemLocation destmem _ destIxFun) destslice
  srcloc@(MemLocation srcmem srcshape srcIxFun) srcslice
  | Just (destoffset, srcoffset,
          num_arrays, size_x, size_y) <-
      isMapTransposeKernel bt destloc destslice srcloc srcslice = do

      fname <- mapTransposeForType bt
      emit $ Imp.Call [] fname
        [Imp.MemArg destmem, Imp.ExpArg destoffset,
         Imp.MemArg srcmem, Imp.ExpArg srcoffset,
         Imp.ExpArg num_arrays, Imp.ExpArg size_x, Imp.ExpArg size_y]

  | bt_size <- primByteSize bt,
    Just destoffset <-
      IxFun.linearWithOffset (IxFun.slice destIxFun destslice) bt_size,
    Just srcoffset  <-
      IxFun.linearWithOffset (IxFun.slice srcIxFun srcslice) bt_size = do
        let num_elems = Imp.elements $ product $ map (toExp' int32) srcshape
        srcspace <- entryMemSpace <$> lookupMemory srcmem
        destspace <- entryMemSpace <$> lookupMemory destmem
        emit $ Imp.Copy
          destmem (bytes destoffset) destspace
          srcmem (bytes srcoffset) srcspace $
          num_elems `Imp.withElemType` bt

  | otherwise = sCopy bt destloc destslice srcloc srcslice

mapTransposeForType :: PrimType -> CallKernelGen Name
mapTransposeForType bt = do
  let fname = nameFromString $ "builtin#" <> mapTransposeName bt

  exists <- hasFunction fname
  unless exists $ emitFunction fname $ mapTransposeFunction bt

  return fname

mapTransposeName :: PrimType -> String
mapTransposeName bt = "map_transpose_" ++ pretty bt

mapTransposeFunction :: PrimType -> Imp.Function
mapTransposeFunction bt =
  Imp.Function False [] params transpose_code [] []

  where params = [memparam destmem, intparam destoffset,
                  memparam srcmem, intparam srcoffset,
                  intparam num_arrays, intparam x, intparam y]

        space = Space "device"
        memparam v = Imp.MemParam v space
        intparam v = Imp.ScalarParam v $ IntType Int32

        [destmem, destoffset, srcmem, srcoffset,
         num_arrays, x, y,
         mulx, muly, block] =
           zipWith (VName . nameFromString)
           ["destmem",
             "destoffset",
             "srcmem",
             "srcoffset",
             "num_arrays",
             "x_elems",
             "y_elems",
             -- The following is only used for low width/height
             -- transpose kernels
             "mulx",
             "muly",
             "block"
            ]
           [0..]

        v32 v = Imp.var v int32

        block_dim_int = 16

        block_dim :: IntegralExp a => a
        block_dim = 16

        -- When an input array has either width==1 or height==1, performing a
        -- transpose will be the same as performing a copy.
        can_use_copy =
          let onearr = CmpOpExp (CmpEq $ IntType Int32) (v32 num_arrays) 1
              height_is_one = CmpOpExp (CmpEq $ IntType Int32) (v32 y) 1
              width_is_one = CmpOpExp (CmpEq $ IntType Int32) (v32 x) 1
          in onearr .&&. (width_is_one .||. height_is_one)

        transpose_code =
          Imp.If input_is_empty mempty $ mconcat
          [ Imp.DeclareScalar muly Imp.Nonvolatile (IntType Int32)
          , Imp.SetScalar muly $ block_dim `quot` v32 x
          , Imp.DeclareScalar mulx Imp.Nonvolatile (IntType Int32)
          , Imp.SetScalar mulx $ block_dim `quot` v32 y
          , Imp.If can_use_copy copy_code $
            Imp.If should_use_lowwidth (callTransposeKernel TransposeLowWidth) $
            Imp.If should_use_lowheight (callTransposeKernel TransposeLowHeight) $
            Imp.If should_use_small (callTransposeKernel TransposeSmall) $
            callTransposeKernel TransposeNormal]

        input_is_empty =
          v32 num_arrays .==. 0 .||. v32 x .==. 0 .||. v32 y .==. 0

        should_use_small = BinOpExp LogAnd
          (CmpOpExp (CmpSle Int32) (v32 x) (block_dim `quot` 2))
          (CmpOpExp (CmpSle Int32) (v32 y) (block_dim `quot` 2))

        should_use_lowwidth = BinOpExp LogAnd
          (CmpOpExp (CmpSle Int32) (v32 x) (block_dim `quot` 2))
          (CmpOpExp (CmpSlt Int32) block_dim (v32 y))

        should_use_lowheight = BinOpExp LogAnd
          (CmpOpExp (CmpSle Int32) (v32 y) (block_dim `quot` 2))
          (CmpOpExp (CmpSlt Int32) block_dim (v32 x))

        copy_code =
          let num_bytes =
                v32 x * v32 y * Imp.LeafExp (Imp.SizeOf bt) (IntType Int32)
          in Imp.Copy
               destmem (Imp.Count $ v32 destoffset) space
               srcmem (Imp.Count $ v32 srcoffset) space
               (Imp.Count num_bytes)

        callTransposeKernel =
          Imp.Op . Imp.CallKernel .
          mapTransposeKernel (mapTransposeName bt) block_dim_int
          (destmem, v32 destoffset, srcmem, v32 srcoffset,
            v32 x, v32 y,
            v32 mulx, v32 muly, v32 num_arrays,
            block) bt

isMapTransposeKernel :: PrimType
                     -> MemLocation -> Slice Imp.Exp
                     -> MemLocation -> Slice Imp.Exp
                     -> Maybe (Imp.Exp, Imp.Exp,
                               Imp.Exp, Imp.Exp, Imp.Exp)
isMapTransposeKernel bt
  (MemLocation _ _ destIxFun) destslice
  (MemLocation _ _ srcIxFun) srcslice
  | Just (dest_offset, perm_and_destshape) <- IxFun.rearrangeWithOffset destIxFun' bt_size,
    (perm, destshape) <- unzip perm_and_destshape,
    Just src_offset <- IxFun.linearWithOffset srcIxFun' bt_size,
    Just (r1, r2, _) <- isMapTranspose perm =
      isOk destshape swap r1 r2 dest_offset src_offset
  | Just dest_offset <- IxFun.linearWithOffset destIxFun' bt_size,
    Just (src_offset, perm_and_srcshape) <- IxFun.rearrangeWithOffset srcIxFun' bt_size,
    (perm, srcshape) <- unzip perm_and_srcshape,
    Just (r1, r2, _) <- isMapTranspose perm =
      isOk srcshape id r1 r2 dest_offset src_offset
  | otherwise =
      Nothing
  where bt_size = primByteSize bt
        swap (x,y) = (y,x)

        destIxFun' = IxFun.slice destIxFun destslice
        srcIxFun' = IxFun.slice srcIxFun srcslice

        isOk shape f r1 r2 dest_offset src_offset = do
          let (num_arrays, size_x, size_y) = getSizes shape f r1 r2
          return (dest_offset, src_offset,
                  num_arrays, size_x, size_y)

        getSizes shape f r1 r2 =
          let (mapped, notmapped) = splitAt r1 shape
              (pretrans, posttrans) = f $ splitAt r2 notmapped
          in (product mapped, product pretrans, product posttrans)
