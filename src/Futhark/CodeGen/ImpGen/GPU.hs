{-# LANGUAGE TypeFamilies #-}

-- | Compile a 'GPUMem' program to imperative code with kernels.
-- This is mostly (but not entirely) the same process no matter if we
-- are targeting OpenCL or CUDA.  The important distinctions (the host
-- level code) are introduced later.
module Futhark.CodeGen.ImpGen.GPU
  ( compileProgOpenCL,
    compileProgCUDA,
    compileProgHIP,
    Warnings,
  )
where

import Control.Monad
import Data.List (foldl')
import Data.Map qualified as M
import Data.Maybe
import Futhark.CodeGen.ImpCode.GPU qualified as Imp
import Futhark.CodeGen.ImpGen hiding (compileProg)
import Futhark.CodeGen.ImpGen qualified
import Futhark.CodeGen.ImpGen.GPU.Base
import Futhark.CodeGen.ImpGen.GPU.SegHist
import Futhark.CodeGen.ImpGen.GPU.SegMap
import Futhark.CodeGen.ImpGen.GPU.SegRed
import Futhark.CodeGen.ImpGen.GPU.SegScan
import Futhark.Error
import Futhark.IR.GPUMem
import Futhark.MonadFreshNames
import Futhark.Util.IntegralExp (divUp, nextMul)
import Prelude hiding (quot, rem)

callKernelOperations :: Operations GPUMem HostEnv Imp.HostOp
callKernelOperations =
  Operations
    { opsExpCompiler = expCompiler,
      opsCopyCompiler = lmadCopy,
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
  (MonadFreshNames m) =>
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
  compileProgCUDA,
  compileProgHIP ::
    (MonadFreshNames m) => Prog GPUMem -> m (Warnings, Imp.Program)
compileProgOpenCL = compileProg $ HostEnv openclAtomics OpenCL mempty
compileProgCUDA = compileProg $ HostEnv cudaAtomics CUDA mempty
compileProgHIP = compileProg $ HostEnv cudaAtomics HIP mempty

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
opCompiler (Pat [pe]) (Inner (SizeOp (CalcNumBlocks w64 max_num_tblocks_key tblock_size))) = do
  fname <- askFunction
  max_num_tblocks :: TV Int32 <- dPrim "max_num_tblocks" int32
  sOp $
    Imp.GetSize (tvVar max_num_tblocks) (keyWithEntryPoint fname max_num_tblocks_key) $
      sizeClassWithEntryPoint fname SizeGrid

  -- If 'w' is small, we launch fewer blocks than we normally would.
  -- We don't want any idle blocks.
  --
  -- The calculations are done with 64-bit integers to avoid overflow
  -- issues.
  let num_tblocks_maybe_zero =
        sMin64 (pe64 w64 `divUp` pe64 tblock_size) $
          sExt64 (tvExp max_num_tblocks)
  -- We also don't want zero blocks.
  let num_tblocks = sMax64 1 num_tblocks_maybe_zero
  mkTV (patElemName pe) int32 <-- sExt32 num_tblocks
opCompiler dest (Inner (SegOp op)) =
  segOpCompiler dest op
opCompiler (Pat pes) (Inner (GPUBody _ (Body _ stms res))) = do
  tid <- newVName "tid"
  let one = Count (intConst Int64 1)
  sKernelThread "gpuseq" tid (defKernelAttrs one one) $
    compileStms (freeIn res) stms $
      forM_ (zip pes res) $ \(pe, SubExpRes _ se) ->
        copyDWIMFix (patElemName pe) [0] se []
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
-- enclosed code do not use more shared memory than we have available.
-- We look at *all* the kernels here, even those that might be
-- otherwise protected by their own multi-versioning branches deeper
-- down.  Currently the compiler will not generate multi-versioning
-- that makes this a problem, but it might in the future.
checkSharedMemoryReqs :: (VName -> Bool) -> Imp.HostCode -> CallKernelGen (Maybe (Imp.TExp Bool))
checkSharedMemoryReqs in_scope code = do
  let alloc_sizes = map (sum . map alignedSize . localAllocSizes . Imp.kernelBody) $ getGPU code

  -- If any of the sizes involve a variable that is not known at this
  -- point, then we cannot check the requirements.
  if not $ all in_scope $ namesToList $ freeIn alloc_sizes
    then pure Nothing
    else do
      shared_memory_capacity :: TV Int32 <- dPrim "shared_memory_capacity" int32
      sOp $ Imp.GetSizeMax (tvVar shared_memory_capacity) SizeSharedMemory

      let shared_memory_capacity_64 =
            sExt64 $ tvExp shared_memory_capacity
          fits size =
            unCount size .<=. shared_memory_capacity_64
      pure $ Just $ foldl' (.&&.) true (map fits alloc_sizes)
  where
    getGPU = foldMap getKernel
    getKernel (Imp.CallKernel k) | Imp.kernelCheckSharedMemory k = [k]
    getKernel _ = []

    localAllocSizes = foldMap localAllocSize
    localAllocSize (Imp.SharedAlloc _ size) = [size]
    localAllocSize _ = []

    -- These allocations will actually be padded to an 8-byte aligned
    -- size, so we should take that into account when checking whether
    -- they fit.
    alignedSize x = nextMul x 8

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
-- Allocation in the "shared" space is just a placeholder.
expCompiler _ (Op (Alloc _ (Space "shared"))) =
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
  scope <- askScope
  tcode <- collect $ compileBody dest $ caseBody first_case
  fcode <- collect $ expCompiler dest $ Match cond cases defbranch sort
  check <- checkSharedMemoryReqs (`M.member` scope) tcode
  let matches = caseMatch cond (casePat first_case)
  emit $ case check of
    Nothing -> fcode
    Just ok -> Imp.If (matches .&&. ok) tcode fcode
expCompiler dest e =
  defCompileExp dest e
