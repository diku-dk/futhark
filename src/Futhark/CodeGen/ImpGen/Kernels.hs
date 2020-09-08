{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Compile a 'KernelsMem' program to imperative code with kernels.
-- This is mostly (but not entirely) the same process no matter if we
-- are targeting OpenCL or CUDA.  The important distinctions (the host
-- level code) are introduced later.
module Futhark.CodeGen.ImpGen.Kernels
  ( compileProgOpenCL,
    compileProgCUDA,
    Warnings,
  )
where

import Control.Monad.Except
import Data.Bifunctor (second)
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe
import Futhark.CodeGen.ImpCode.Kernels (bytes)
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import Futhark.CodeGen.ImpGen hiding (compileProg)
import qualified Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Kernels.Base
import Futhark.CodeGen.ImpGen.Kernels.SegHist
import Futhark.CodeGen.ImpGen.Kernels.SegMap
import Futhark.CodeGen.ImpGen.Kernels.SegRed
import Futhark.CodeGen.ImpGen.Kernels.SegScan
import Futhark.CodeGen.ImpGen.Kernels.Transpose
import Futhark.CodeGen.SetDefaultSpace
import Futhark.Error
import Futhark.IR.KernelsMem
import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.MonadFreshNames
import Futhark.Util.IntegralExp (IntegralExp, divUp, quot)
import Prelude hiding (quot)

callKernelOperations :: Operations KernelsMem HostEnv Imp.HostOp
callKernelOperations =
  Operations
    { opsExpCompiler = expCompiler,
      opsCopyCompiler = callKernelCopy,
      opsOpCompiler = opCompiler,
      opsStmsCompiler = defCompileStms,
      opsAllocCompilers = mempty
    }

openclAtomics, cudaAtomics :: AtomicBinOp
(openclAtomics, cudaAtomics) = (flip lookup opencl, flip lookup cuda)
  where
    opencl =
      [ (Add Int32 OverflowUndef, Imp.AtomicAdd Int32),
        (SMax Int32, Imp.AtomicSMax Int32),
        (SMin Int32, Imp.AtomicSMin Int32),
        (UMax Int32, Imp.AtomicUMax Int32),
        (UMin Int32, Imp.AtomicUMin Int32),
        (And Int32, Imp.AtomicAnd Int32),
        (Or Int32, Imp.AtomicOr Int32),
        (Xor Int32, Imp.AtomicXor Int32)
      ]
    cuda = opencl ++ [(FAdd Float32, Imp.AtomicFAdd Float32)]

compileProg ::
  MonadFreshNames m =>
  HostEnv ->
  Prog KernelsMem ->
  m (Warnings, Imp.Program)
compileProg env prog =
  second (setDefaultSpace (Imp.Space "device"))
    <$> Futhark.CodeGen.ImpGen.compileProg env callKernelOperations (Imp.Space "device") prog

-- | Compile a 'KernelsMem' program to low-level parallel code, with
-- either CUDA or OpenCL characteristics.
compileProgOpenCL,
  compileProgCUDA ::
    MonadFreshNames m => Prog KernelsMem -> m (Warnings, Imp.Program)
compileProgOpenCL = compileProg $ HostEnv openclAtomics
compileProgCUDA = compileProg $ HostEnv cudaAtomics

opCompiler ::
  Pattern KernelsMem ->
  Op KernelsMem ->
  CallKernelGen ()
opCompiler dest (Alloc e space) =
  compileAlloc dest e space
opCompiler (Pattern _ [pe]) (Inner (SizeOp (GetSize key size_class))) = do
  fname <- askFunction
  sOp $
    Imp.GetSize (patElemName pe) (keyWithEntryPoint fname key) $
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
  max_num_groups :: TV Int32 <- dPrim "max_num_groups" int32
  sOp $
    Imp.GetSize (tvVar max_num_groups) (keyWithEntryPoint fname max_num_groups_key) $
      sizeClassWithEntryPoint fname SizeNumGroups

  -- If 'w' is small, we launch fewer groups than we normally would.
  -- We don't want any idle groups.
  --
  -- The calculations are done with 64-bit integers to avoid overflow
  -- issues.
  let num_groups_maybe_zero =
        sMin64
          ( toInt64Exp w64
              `divUp` sExt64 (toInt32Exp group_size)
          )
          $ sExt64 (tvExp max_num_groups)
  -- We also don't want zero groups.
  let num_groups = sMax64 1 num_groups_maybe_zero
  mkTV (patElemName pe) int32 <-- sExt32 num_groups
opCompiler dest (Inner (SegOp op)) =
  segOpCompiler dest op
opCompiler pat e =
  compilerBugS $
    "opCompiler: Invalid pattern\n  "
      ++ pretty pat
      ++ "\nfor expression\n  "
      ++ pretty e

sizeClassWithEntryPoint :: Maybe Name -> Imp.SizeClass -> Imp.SizeClass
sizeClassWithEntryPoint fname (Imp.SizeThreshold path def) =
  Imp.SizeThreshold (map f path) def
  where
    f (name, x) = (keyWithEntryPoint fname name, x)
sizeClassWithEntryPoint _ size_class = size_class

segOpCompiler ::
  Pattern KernelsMem ->
  SegOp SegLevel KernelsMem ->
  CallKernelGen ()
segOpCompiler pat (SegMap lvl space _ kbody) =
  compileSegMap pat lvl space kbody
segOpCompiler pat (SegRed lvl@SegThread {} space reds _ kbody) =
  compileSegRed pat lvl space reds kbody
segOpCompiler pat (SegScan lvl@SegThread {} space scans _ kbody) =
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
checkLocalMemoryReqs :: Imp.Code -> CallKernelGen (Maybe (Imp.TExp Bool))
checkLocalMemoryReqs code = do
  scope <- askScope
  let alloc_sizes = map (sum . localAllocSizes . Imp.kernelBody) $ getKernels code

  -- If any of the sizes involve a variable that is not known at this
  -- point, then we cannot check the requirements.
  if any (`M.notMember` scope) (namesToList $ freeIn alloc_sizes)
    then return Nothing
    else do
      local_memory_capacity :: TV Int32 <- dPrim "local_memory_capacity" int32
      sOp $ Imp.GetSizeMax (tvVar local_memory_capacity) SizeLocalMemory

      let local_memory_capacity_64 =
            sExt64 $ tvExp local_memory_capacity
          fits size =
            unCount size .<=. local_memory_capacity_64
      return $ Just $ foldl' (.&&.) true (map fits alloc_sizes)
  where
    getKernels = foldMap getKernel
    getKernel (Imp.CallKernel k) = [k]
    getKernel _ = []

    localAllocSizes = foldMap localAllocSize
    localAllocSize (Imp.LocalAlloc _ size) = [size]
    localAllocSize _ = []

expCompiler :: ExpCompiler KernelsMem HostEnv Imp.HostOp
-- We generate a simple kernel for itoa and replicate.
expCompiler (Pattern _ [pe]) (BasicOp (Iota n x s et)) = do
  x' <- toExp x
  s' <- toExp s

  sIota (patElemName pe) (toInt32Exp n) x' s' et
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
    Just ok -> Imp.If (ok .&&. toBoolExp cond) tcode fcode
expCompiler dest e =
  defCompileExp dest e

callKernelCopy :: CopyCompiler KernelsMem HostEnv Imp.HostOp
callKernelCopy
  bt
  destloc@(MemLocation destmem _ destIxFun)
  destslice
  srcloc@(MemLocation srcmem srcshape srcIxFun)
  srcslice
    | Just
        ( destoffset,
          srcoffset,
          num_arrays,
          size_x,
          size_y
          ) <-
        isMapTransposeCopy bt destloc destslice srcloc srcslice = do
      fname <- mapTransposeForType bt
      emit $
        Imp.Call
          []
          fname
          [ Imp.MemArg destmem,
            Imp.ExpArg $ untyped destoffset,
            Imp.MemArg srcmem,
            Imp.ExpArg $ untyped srcoffset,
            Imp.ExpArg $ untyped num_arrays,
            Imp.ExpArg $ untyped size_x,
            Imp.ExpArg $ untyped size_y
          ]
    | bt_size <- primByteSize bt,
      Just destoffset <-
        IxFun.linearWithOffset (IxFun.slice destIxFun destslice) bt_size,
      Just srcoffset <-
        IxFun.linearWithOffset (IxFun.slice srcIxFun srcslice) bt_size = do
      let num_elems = Imp.elements $ product $ map toInt32Exp srcshape
      srcspace <- entryMemSpace <$> lookupMemory srcmem
      destspace <- entryMemSpace <$> lookupMemory destmem
      emit $
        Imp.Copy
          destmem
          (bytes $ sExt64 destoffset)
          destspace
          srcmem
          (bytes $ sExt64 srcoffset)
          srcspace
          $ num_elems `Imp.withElemType` bt
    | otherwise = sCopy bt destloc destslice srcloc srcslice

mapTransposeForType :: PrimType -> CallKernelGen Name
mapTransposeForType bt = do
  let fname = nameFromString $ "builtin#" <> mapTransposeName bt

  exists <- hasFunction fname
  unless exists $ emitFunction fname $ mapTransposeFunction bt

  return fname

mapTransposeName :: PrimType -> String
mapTransposeName bt = "gpu_map_transpose_" ++ pretty bt

mapTransposeFunction :: PrimType -> Imp.Function
mapTransposeFunction bt =
  Imp.Function False [] params transpose_code [] []
  where
    params =
      [ memparam destmem,
        intparam destoffset,
        memparam srcmem,
        intparam srcoffset,
        intparam num_arrays,
        intparam x,
        intparam y
      ]

    space = Space "device"
    memparam v = Imp.MemParam v space
    intparam v = Imp.ScalarParam v $ IntType Int32

    [ destmem,
      destoffset,
      srcmem,
      srcoffset,
      num_arrays,
      x,
      y,
      mulx,
      muly,
      block
      ] =
        zipWith
          (VName . nameFromString)
          [ "destmem",
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
          [0 ..]

    block_dim_int = 16

    block_dim :: IntegralExp a => a
    block_dim = 16

    -- When an input array has either width==1 or height==1, performing a
    -- transpose will be the same as performing a copy.
    can_use_copy =
      let onearr = Imp.vi32 num_arrays .==. 1
          height_is_one = Imp.vi32 y .==. 1
          width_is_one = Imp.vi32 x .==. 1
       in onearr .&&. (width_is_one .||. height_is_one)

    transpose_code =
      Imp.If input_is_empty mempty $
        mconcat
          [ Imp.DeclareScalar muly Imp.Nonvolatile (IntType Int32),
            Imp.SetScalar muly $ untyped $ block_dim `quot` Imp.vi32 x,
            Imp.DeclareScalar mulx Imp.Nonvolatile (IntType Int32),
            Imp.SetScalar mulx $ untyped $ block_dim `quot` Imp.vi32 y,
            Imp.If can_use_copy copy_code $
              Imp.If should_use_lowwidth (callTransposeKernel TransposeLowWidth) $
                Imp.If should_use_lowheight (callTransposeKernel TransposeLowHeight) $
                  Imp.If should_use_small (callTransposeKernel TransposeSmall) $
                    callTransposeKernel TransposeNormal
          ]

    input_is_empty =
      Imp.vi32 num_arrays .==. 0 .||. Imp.vi32 x .==. 0 .||. Imp.vi32 y .==. 0

    should_use_small =
      Imp.vi32 x .<=. (block_dim `quot` 2)
        .&&. Imp.vi32 y .<=. (block_dim `quot` 2)

    should_use_lowwidth =
      Imp.vi32 x .<=. (block_dim `quot` 2)
        .&&. block_dim .<. Imp.vi32 y

    should_use_lowheight =
      Imp.vi32 y .<=. (block_dim `quot` 2)
        .&&. block_dim .<. Imp.vi32 x

    copy_code =
      let num_bytes =
            sExt64 $
              Imp.vi32 x * Imp.vi32 y * isInt32 (Imp.LeafExp (Imp.SizeOf bt) (IntType Int32))
       in Imp.Copy
            destmem
            (Imp.Count $ sExt64 $ Imp.vi32 destoffset)
            space
            srcmem
            (Imp.Count $ sExt64 $ Imp.vi32 srcoffset)
            space
            (Imp.Count num_bytes)

    callTransposeKernel =
      Imp.Op . Imp.CallKernel
        . mapTransposeKernel
          (mapTransposeName bt)
          block_dim_int
          ( destmem,
            Imp.vi32 destoffset,
            srcmem,
            Imp.vi32 srcoffset,
            Imp.vi32 x,
            Imp.vi32 y,
            Imp.vi32 mulx,
            Imp.vi32 muly,
            Imp.vi32 num_arrays,
            block
          )
          bt
