{-# LANGUAGE TypeFamilies #-}

-- | Compile a 'GPUMem' program to imperative code with kernels.
-- This is mostly (but not entirely) the same process no matter if we
-- are targeting OpenCL or CUDA.  The important distinctions (the host
-- level code) are introduced later.
module Futhark.CodeGen.ImpGen.GPU
  ( compileProgOpenCL,
    compileProgCUDA,
    Warnings,
  )
where

import Control.Monad
import Control.Monad.State
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Map qualified as M
import Data.Maybe
import Futhark.CodeGen.ImpCode.GPU qualified as Imp
import Futhark.CodeGen.ImpGen hiding (compileProg)
import Futhark.CodeGen.ImpGen qualified
import Futhark.CodeGen.ImpGen.GPU.Base
import Futhark.CodeGen.ImpGen.GPU.Copy
import Futhark.CodeGen.ImpGen.GPU.SegHist
import Futhark.CodeGen.ImpGen.GPU.SegMap
import Futhark.CodeGen.ImpGen.GPU.SegRed
import Futhark.CodeGen.ImpGen.GPU.SegScan
import Futhark.CodeGen.ImpGen.GPU.Transpose
import Futhark.Error
import Futhark.IR.GPUMem
import Futhark.IR.Mem.IxFun qualified as IxFun
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.MonadFreshNames
import Futhark.Util.IntegralExp (IntegralExp, divUp, quot, rem)
import Prelude hiding (quot, rem)

callKernelOperations :: Operations GPUMem HostEnv Imp.HostOp
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
    opencl64 =
      [ (Add Int64 OverflowUndef, Imp.AtomicAdd Int64),
        (SMax Int64, Imp.AtomicSMax Int64),
        (SMin Int64, Imp.AtomicSMin Int64),
        (UMax Int64, Imp.AtomicUMax Int64),
        (UMin Int64, Imp.AtomicUMin Int64),
        (And Int64, Imp.AtomicAnd Int64),
        (Or Int64, Imp.AtomicOr Int64),
        (Xor Int64, Imp.AtomicXor Int64)
      ]
    opencl32 =
      [ (Add Int32 OverflowUndef, Imp.AtomicAdd Int32),
        (SMax Int32, Imp.AtomicSMax Int32),
        (SMin Int32, Imp.AtomicSMin Int32),
        (UMax Int32, Imp.AtomicUMax Int32),
        (UMin Int32, Imp.AtomicUMin Int32),
        (And Int32, Imp.AtomicAnd Int32),
        (Or Int32, Imp.AtomicOr Int32),
        (Xor Int32, Imp.AtomicXor Int32)
      ]
    opencl = opencl32 ++ opencl64
    cuda =
      opencl
        ++ [ (FAdd Float32, Imp.AtomicFAdd Float32),
             (FAdd Float64, Imp.AtomicFAdd Float64)
           ]

compileProg ::
  MonadFreshNames m =>
  HostEnv ->
  Prog GPUMem ->
  m (Warnings, Imp.Program)
compileProg env =
  Futhark.CodeGen.ImpGen.compileProg env callKernelOperations device_space
  where
    device_space = Imp.Space "device"

-- | Compile a 'GPUMem' program to low-level parallel code, with
-- either CUDA or OpenCL characteristics.
compileProgOpenCL,
  compileProgCUDA ::
    MonadFreshNames m => Prog GPUMem -> m (Warnings, Imp.Program)
compileProgOpenCL = compileProg $ HostEnv openclAtomics OpenCL mempty
compileProgCUDA = compileProg $ HostEnv cudaAtomics CUDA mempty

opCompiler ::
  Pat LetDecMem ->
  Op GPUMem ->
  CallKernelGen ()
opCompiler dest (Alloc e space) =
  compileAlloc dest e space
opCompiler (Pat [pe]) (Inner (SizeOp (GetSize key size_class))) = do
  fname <- askFunction
  sOp $
    Imp.GetSize (patElemName pe) (keyWithEntryPoint fname key) $
      sizeClassWithEntryPoint fname size_class
opCompiler (Pat [pe]) (Inner (SizeOp (CmpSizeLe key size_class x))) = do
  fname <- askFunction
  let size_class' = sizeClassWithEntryPoint fname size_class
  sOp . Imp.CmpSizeLe (patElemName pe) (keyWithEntryPoint fname key) size_class'
    =<< toExp x
opCompiler (Pat [pe]) (Inner (SizeOp (GetSizeMax size_class))) =
  sOp $ Imp.GetSizeMax (patElemName pe) size_class
opCompiler (Pat [pe]) (Inner (SizeOp (CalcNumGroups w64 max_num_groups_key group_size))) = do
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
        sMin64 (pe64 w64 `divUp` pe64 group_size) $
          sExt64 (tvExp max_num_groups)
  -- We also don't want zero groups.
  let num_groups = sMax64 1 num_groups_maybe_zero
  mkTV (patElemName pe) int32 <-- sExt32 num_groups
opCompiler dest (Inner (SegOp op)) =
  segOpCompiler dest op
opCompiler (Pat pes) (Inner (GPUBody _ (Body _ stms res))) = do
  tid <- newVName "tid"
  let one = Count (intConst Int64 1)
  sKernelThread "gpuseq" tid (defKernelAttrs one one) $
    compileStms (freeIn res) stms $
      forM_ (zip pes res) $ \(pe, SubExpRes _ se) ->
        copyDWIM (patElemName pe) [DimFix 0] se []
opCompiler pat e =
  compilerBugS $
    "opCompiler: Invalid pattern\n  "
      ++ prettyString pat
      ++ "\nfor expression\n  "
      ++ prettyString e

sizeClassWithEntryPoint :: Maybe Name -> Imp.SizeClass -> Imp.SizeClass
sizeClassWithEntryPoint fname (Imp.SizeThreshold path def) =
  Imp.SizeThreshold (map f path) def
  where
    f (name, x) = (keyWithEntryPoint fname name, x)
sizeClassWithEntryPoint _ size_class = size_class

segOpCompiler ::
  Pat LetDecMem ->
  SegOp SegLevel GPUMem ->
  CallKernelGen ()
segOpCompiler pat (SegMap lvl space _ kbody) =
  compileSegMap pat lvl space kbody
segOpCompiler pat (SegRed lvl@(SegThread _ _) space reds _ kbody) =
  compileSegRed pat lvl space reds kbody
segOpCompiler pat (SegScan lvl@(SegThread _ _) space scans _ kbody) =
  compileSegScan pat lvl space scans kbody
segOpCompiler pat (SegHist lvl@(SegThread _ _) space ops _ kbody) =
  compileSegHist pat lvl space ops kbody
segOpCompiler pat segop =
  compilerBugS $ "segOpCompiler: unexpected " ++ prettyString (segLevel segop) ++ " for rhs of pattern " ++ prettyString pat

-- Create boolean expression that checks whether all kernels in the
-- enclosed code do not use more local memory than we have available.
-- We look at *all* the kernels here, even those that might be
-- otherwise protected by their own multi-versioning branches deeper
-- down.  Currently the compiler will not generate multi-versioning
-- that makes this a problem, but it might in the future.
checkLocalMemoryReqs :: Imp.HostCode -> CallKernelGen (Maybe (Imp.TExp Bool))
checkLocalMemoryReqs code = do
  scope <- askScope
  let alloc_sizes = map (sum . map alignedSize . localAllocSizes . Imp.kernelBody) $ getGPU code

  -- If any of the sizes involve a variable that is not known at this
  -- point, then we cannot check the requirements.
  if any (`M.notMember` scope) (namesToList $ freeIn alloc_sizes)
    then pure Nothing
    else do
      local_memory_capacity :: TV Int32 <- dPrim "local_memory_capacity" int32
      sOp $ Imp.GetSizeMax (tvVar local_memory_capacity) SizeLocalMemory

      let local_memory_capacity_64 =
            sExt64 $ tvExp local_memory_capacity
          fits size =
            unCount size .<=. local_memory_capacity_64
      pure $ Just $ foldl' (.&&.) true (map fits alloc_sizes)
  where
    getGPU = foldMap getKernel
    getKernel (Imp.CallKernel k) | Imp.kernelCheckLocalMemory k = [k]
    getKernel _ = []

    localAllocSizes = foldMap localAllocSize
    localAllocSize (Imp.LocalAlloc _ size) = [size]
    localAllocSize _ = []

    -- These allocations will actually be padded to an 8-byte aligned
    -- size, so we should take that into account when checking whether
    -- they fit.
    alignedSize x = x + ((8 - (x `rem` 8)) `rem` 8)

withAcc ::
  Pat LetDecMem ->
  [(Shape, [VName], Maybe (Lambda GPUMem, [SubExp]))] ->
  Lambda GPUMem ->
  CallKernelGen ()
withAcc pat inputs lam = do
  atomics <- hostAtomics <$> askEnv
  locksForInputs atomics $ zip accs inputs
  where
    accs = map paramName $ lambdaParams lam
    locksForInputs _ [] =
      defCompileExp pat $ WithAcc inputs lam
    locksForInputs atomics ((c, (_, _, op)) : inputs')
      | Just (op_lam, _) <- op,
        AtomicLocking _ <- atomicUpdateLocking atomics op_lam = do
          let num_locks = 100151
          locks_arr <- genZeroes "withacc_locks" num_locks
          let locks = Locks locks_arr num_locks
              extend env = env {hostLocks = M.insert c locks $ hostLocks env}
          localEnv extend $ locksForInputs atomics inputs'
      | otherwise =
          locksForInputs atomics inputs'

expCompiler :: ExpCompiler GPUMem HostEnv Imp.HostOp
-- We generate a simple kernel for itoa and replicate.
expCompiler (Pat [pe]) (BasicOp (Iota n x s et)) = do
  x' <- toExp x
  s' <- toExp s

  sIota (patElemName pe) (pe64 n) x' s' et
expCompiler (Pat [pe]) (BasicOp (Replicate shape se))
  | Acc {} <- patElemType pe = pure ()
  | otherwise =
      if shapeRank shape == 0
        then copyDWIM (patElemName pe) [] se []
        else sReplicate (patElemName pe) se
-- Allocation in the "local" space is just a placeholder.
expCompiler _ (Op (Alloc _ (Space "local"))) =
  pure ()
expCompiler pat (WithAcc inputs lam) =
  withAcc pat inputs lam
-- This is a multi-versioning Match created by incremental flattening.
-- We need to augment the conditional with a check that any local
-- memory requirements in tbranch are compatible with the hardware.
-- We do not check anything for defbody, as we assume that it will
-- always be safe (and what would we do if none of the branches would
-- work?).
expCompiler dest (Match cond (first_case : cases) defbranch sort@(MatchDec _ MatchEquiv)) = do
  tcode <- collect $ compileBody dest $ caseBody first_case
  fcode <- collect $ expCompiler dest $ Match cond cases defbranch sort
  check <- checkLocalMemoryReqs tcode
  let matches = caseMatch cond (casePat first_case)
  emit $ case check of
    Nothing -> fcode
    Just ok -> Imp.If (matches .&&. ok) tcode fcode
expCompiler dest e =
  defCompileExp dest e

gpuCopyForType :: Rank -> PrimType -> CallKernelGen Name
gpuCopyForType r bt = do
  let fname = nameFromString $ "builtin#" <> gpuCopyName r bt

  exists <- hasFunction fname
  unless exists $ emitFunction fname $ gpuCopyFunction r bt

  pure fname

gpuCopyName :: Rank -> PrimType -> String
gpuCopyName (Rank r) bt = "gpu_copy_" <> show r <> "d_" <> prettyString bt

gpuCopyFunction :: Rank -> PrimType -> Imp.Function Imp.HostOp
gpuCopyFunction (Rank r) pt = do
  let tdesc = mconcat (replicate r "[]") <> prettyString pt
  Imp.Function Nothing [] params $
    Imp.DebugPrint ("\n# Copy " <> tdesc) Nothing
      <> copy_code
      <> Imp.DebugPrint "" Nothing
  where
    space = Space "device"
    memparam v = Imp.MemParam v space
    intparam v = Imp.ScalarParam v $ IntType Int64

    mkIxFun desc = do
      let new x = newVName $ desc <> "_" <> x
          newDim i = LMAD.LMADDim <$> new "stride" <*> new "shape" <*> pure i
      LMAD.LMAD <$> new "offset" <*> mapM newDim [0 .. r - 1]

    (params, copy_code) = do
      flip evalState blankNameSource $ do
        dest_mem <- newVName "destmem"
        dest_lmad <- mkIxFun "dest"

        src_mem <- newVName "srcmem"
        src_lmad <- mkIxFun "src"

        group_size <- newVName "group_size"
        num_groups <- newVName "num_groups"

        let kernel =
              copyKernel
                pt
                (le64 num_groups, Left $ untyped $ le64 group_size)
                (dest_mem, le64 <$> dest_lmad)
                (src_mem, le64 <$> src_lmad)

            dest_offset =
              Imp.elements (le64 (LMAD.offset dest_lmad)) `Imp.withElemType` pt

            src_offset =
              Imp.elements (le64 (LMAD.offset src_lmad)) `Imp.withElemType` pt

            num_bytes =
              Imp.elements (product (le64 <$> LMAD.shape src_lmad)) `Imp.withElemType` pt

            do_copy =
              Imp.Copy
                pt
                dest_mem
                dest_offset
                (Space "device")
                src_mem
                src_offset
                (Space "device")
                num_bytes

        pure
          ( [memparam dest_mem]
              ++ map intparam (toList dest_lmad)
              ++ [memparam src_mem]
              ++ map intparam (toList src_lmad),
            Imp.DeclareScalar group_size Imp.Nonvolatile int64
              <> Imp.DeclareScalar num_groups Imp.Nonvolatile int64
              <> Imp.Op (Imp.GetSize group_size "copy_group_size" Imp.SizeGroup)
              <> Imp.Op (Imp.GetSize num_groups "copy_num_groups" Imp.SizeNumGroups)
              <> Imp.If
                (LMAD.memcpyable (le64 <$> dest_lmad) (le64 <$> src_lmad))
                ( Imp.DebugPrint "## Simple copy" Nothing
                    <> do_copy
                )
                ( Imp.DebugPrint "## Kernel copy" Nothing
                    <> Imp.Op (Imp.CallKernel kernel)
                )
          )

mapTransposeForType :: PrimType -> CallKernelGen Name
mapTransposeForType bt = do
  let fname = nameFromString $ "builtin#" <> mapTransposeName bt

  exists <- hasFunction fname
  unless exists $ emitFunction fname $ mapTransposeFunction bt

  pure fname

mapTransposeName :: PrimType -> String
mapTransposeName bt = "gpu_map_transpose_" ++ prettyString bt

mapTransposeFunction :: PrimType -> Imp.Function Imp.HostOp
mapTransposeFunction bt =
  Imp.Function Nothing [] params $
    Imp.DebugPrint ("\n# Transpose " <> prettyString bt) Nothing
      <> Imp.DebugPrint "Number of arrays  " (Just $ untyped $ Imp.le64 num_arrays)
      <> Imp.DebugPrint "X elements        " (Just $ untyped $ Imp.le64 x)
      <> Imp.DebugPrint "Y elements        " (Just $ untyped $ Imp.le64 y)
      <> Imp.DebugPrint "Source      offset" (Just $ untyped $ Imp.le64 srcoffset)
      <> Imp.DebugPrint "Destination offset" (Just $ untyped $ Imp.le64 destoffset)
      <> transpose_code
      <> Imp.DebugPrint "" Nothing
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
    intparam v = Imp.ScalarParam v $ IntType Int64

    [ destmem,
      destoffset,
      srcmem,
      srcoffset,
      num_arrays,
      x,
      y,
      mulx,
      muly,
      block,
      use_32b
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
            "block",
            "use_32b"
          ]
          [0 ..]

    block_dim_int = 16

    block_dim :: IntegralExp a => a
    block_dim = 16

    -- When an input array has either width==1 or height==1, performing a
    -- transpose will be the same as performing a copy.
    can_use_copy =
      let onearr = Imp.le64 num_arrays .==. 1
          height_is_one = Imp.le64 y .==. 1
          width_is_one = Imp.le64 x .==. 1
       in onearr .&&. (width_is_one .||. height_is_one)

    transpose_code =
      Imp.If input_is_empty mempty $
        mconcat
          [ Imp.DeclareScalar muly Imp.Nonvolatile (IntType Int64),
            Imp.SetScalar muly $ untyped $ block_dim `quot` Imp.le64 x,
            Imp.DeclareScalar mulx Imp.Nonvolatile (IntType Int64),
            Imp.SetScalar mulx $ untyped $ block_dim `quot` Imp.le64 y,
            Imp.DeclareScalar use_32b Imp.Nonvolatile Bool,
            Imp.SetScalar use_32b $
              untyped $
                (le64 destoffset + le64 num_arrays * le64 x * le64 y) .<=. 2 ^ (31 :: Int) - 1
                  .&&. (le64 srcoffset + le64 num_arrays * le64 x * le64 y) .<=. 2 ^ (31 :: Int) - 1,
            Imp.If can_use_copy copy_code $
              Imp.If should_use_lowwidth (callTransposeKernel TransposeLowWidth) $
                Imp.If should_use_lowheight (callTransposeKernel TransposeLowHeight) $
                  Imp.If should_use_small (callTransposeKernel TransposeSmall) $
                    callTransposeKernel TransposeNormal
          ]

    input_is_empty =
      Imp.le64 num_arrays .==. 0 .||. Imp.le64 x .==. 0 .||. Imp.le64 y .==. 0

    should_use_small =
      Imp.le64 x .<=. (block_dim `quot` 2)
        .&&. Imp.le64 y .<=. (block_dim `quot` 2)

    should_use_lowwidth =
      Imp.le64 x .<=. (block_dim `quot` 2)
        .&&. block_dim .<. Imp.le64 y

    should_use_lowheight =
      Imp.le64 y .<=. (block_dim `quot` 2)
        .&&. block_dim .<. Imp.le64 x

    copy_code =
      let num_bytes = sExt64 $ Imp.le64 x * Imp.le64 y * primByteSize bt
       in Imp.Copy
            bt
            destmem
            (Imp.Count $ Imp.le64 destoffset)
            space
            srcmem
            (Imp.Count $ Imp.le64 srcoffset)
            space
            (Imp.Count num_bytes)

    callTransposeKernel which =
      Imp.If
        (isBool (LeafExp use_32b Bool))
        ( Imp.DebugPrint "Using 32-bit indexing" Nothing
            <> callTransposeKernel32 which
        )
        ( Imp.DebugPrint "Using 64-bit indexing" Nothing
            <> callTransposeKernel64 which
        )

    callTransposeKernel64 =
      Imp.Op
        . Imp.CallKernel
        . mapTransposeKernel
          (int64, le64)
          (mapTransposeName bt)
          block_dim_int
          ( destmem,
            le64 destoffset,
            srcmem,
            le64 srcoffset,
            le64 x,
            le64 y,
            le64 mulx,
            le64 muly,
            le64 num_arrays,
            block
          )
          bt

    callTransposeKernel32 =
      Imp.Op
        . Imp.CallKernel
        . mapTransposeKernel
          (int32, le32)
          (mapTransposeName bt)
          block_dim_int
          ( destmem,
            sExt32 (le64 destoffset),
            srcmem,
            sExt32 (le64 srcoffset),
            sExt32 (le64 x),
            sExt32 (le64 y),
            sExt32 (le64 mulx),
            sExt32 (le64 muly),
            sExt32 (le64 num_arrays),
            block
          )
          bt

-- Note [32-bit transpositions]
--
-- Transposition kernels are much slower when they have to use 64-bit
-- arithmetic.  I observed about 0.67x slowdown on an A100 GPU when
-- transposing four-byte elements (much less when transposing 8-byte
-- elements).  Unfortunately, 64-bit arithmetic is a requirement for
-- large arrays (see #1953 for what happens otherwise).  We generate
-- both 32- and 64-bit index arithmetic versions of transpositions,
-- and dynamically pick between them at runtime.  This is an
-- unfortunate code bloat, and it would be preferable if we could
-- simply optimise the 64-bit version to make this distinction
-- unnecessary.  Fortunately these kernels are quite small.

callKernelCopy :: CopyCompiler GPUMem HostEnv Imp.HostOp
callKernelCopy pt destloc@(MemLoc destmem _ dest_ixfun) srcloc@(MemLoc srcmem _ src_ixfun)
  | Just (is_transpose, (destoffset, srcoffset, num_arrays, size_x, size_y)) <-
      isMapTransposeCopy pt destloc srcloc = do
      fname <- mapTransposeForType pt
      sIf
        is_transpose
        ( emit . Imp.Call [] fname $
            [ Imp.MemArg destmem,
              Imp.ExpArg $ untyped destoffset,
              Imp.MemArg srcmem,
              Imp.ExpArg $ untyped srcoffset,
              Imp.ExpArg $ untyped num_arrays,
              Imp.ExpArg $ untyped size_x,
              Imp.ExpArg $ untyped size_y
            ]
        )
        nontranspose
  | otherwise = nontranspose
  where
    nontranspose = do
      fname <- gpuCopyForType (Rank (IxFun.rank dest_ixfun)) pt
      dest_space <- entryMemSpace <$> lookupMemory destmem
      src_space <- entryMemSpace <$> lookupMemory srcmem
      let dest_lmad = LMAD.noPermutation $ IxFun.ixfunLMAD dest_ixfun
          src_lmad = LMAD.noPermutation $ IxFun.ixfunLMAD src_ixfun
          num_elems = Imp.elements $ product $ LMAD.shape dest_lmad
      if dest_space == Space "device" && src_space == Space "device"
        then
          emit . Imp.Call [] fname $
            [Imp.MemArg destmem]
              ++ map (Imp.ExpArg . untyped) (toList dest_lmad)
              ++ [Imp.MemArg srcmem]
              ++ map (Imp.ExpArg . untyped) (toList src_lmad)
        else -- FIXME: this assumes a linear representation!
        -- Currently we never generate code where this is not the
        -- case, but we might in the future.

          sCopy
            destmem
            (Imp.elements (LMAD.offset dest_lmad) `Imp.withElemType` pt)
            dest_space
            srcmem
            (Imp.elements (LMAD.offset src_lmad) `Imp.withElemType` pt)
            src_space
            num_elems
            pt
