{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.CodeGen.ImpGen.GPU.Base
  ( KernelConstants (..),
    threadOperations,
    keyWithEntryPoint,
    CallKernelGen,
    InKernelGen,
    Locks (..),
    HostEnv (..),
    Target (..),
    KernelEnv (..),
    groupReduce,
    groupScan,
    groupLoop,
    isActive,
    sKernel,
    sKernelThread,
    KernelAttrs (..),
    defKernelAttrs,
    lvlKernelAttrs,
    allocLocal,
    kernelAlloc,
    compileThreadResult,
    virtualiseGroups,
    kernelLoop,
    groupCoverSpace,
    fenceForArrays,
    updateAcc,
    genZeroes,

    -- * Host-level bulk operations
    sReplicate,
    sIota,

    -- * Atomics
    AtomicBinOp,
    atomicUpdateLocking,
    Locking (..),
    AtomicUpdate (..),
    DoAtomicUpdate,
  )
where

import Control.Monad
import Data.List (foldl')
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.CodeGen.ImpCode.GPU qualified as Imp
import Futhark.CodeGen.ImpGen
import Futhark.Error
import Futhark.IR.GPUMem
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.MonadFreshNames
import Futhark.Transform.Rename
import Futhark.Util (dropLast, nubOrd, splitFromEnd)
import Futhark.Util.IntegralExp (divUp, quot, rem)
import Prelude hiding (quot, rem)

-- | Which target are we ultimately generating code for?  While most
-- of the kernels code is the same, there are some cases where we
-- generate special code based on the ultimate low-level API we are
-- targeting.
data Target = CUDA | OpenCL | HIP

-- | Information about the locks available for accumulators.
data Locks = Locks
  { locksArray :: VName,
    locksCount :: Int
  }

data HostEnv = HostEnv
  { hostAtomics :: AtomicBinOp,
    hostTarget :: Target,
    hostLocks :: M.Map VName Locks
  }

data KernelEnv = KernelEnv
  { kernelAtomics :: AtomicBinOp,
    kernelConstants :: KernelConstants,
    kernelLocks :: M.Map VName Locks
  }

type CallKernelGen = ImpM GPUMem HostEnv Imp.HostOp

type InKernelGen = ImpM GPUMem KernelEnv Imp.KernelOp

data KernelConstants = KernelConstants
  { kernelGlobalThreadId :: Imp.TExp Int32,
    kernelLocalThreadId :: Imp.TExp Int32,
    kernelGroupId :: Imp.TExp Int32,
    kernelGlobalThreadIdVar :: VName,
    kernelLocalThreadIdVar :: VName,
    kernelGroupIdVar :: VName,
    kernelNumGroupsCount :: Count NumGroups SubExp,
    kernelGroupSizeCount :: Count GroupSize SubExp,
    kernelNumGroups :: Imp.TExp Int64,
    kernelGroupSize :: Imp.TExp Int64,
    kernelNumThreads :: Imp.TExp Int32,
    kernelWaveSize :: Imp.TExp Int32,
    -- | A mapping from dimensions of nested SegOps to already
    -- computed local thread IDs.  Only valid in non-virtualised case.
    kernelLocalIdMap :: M.Map [SubExp] [Imp.TExp Int32],
    -- | Mapping from dimensions of nested SegOps to how many
    -- iterations the virtualisation loop needs.
    kernelChunkItersMap :: M.Map [SubExp] (Imp.TExp Int32)
  }

keyWithEntryPoint :: Maybe Name -> Name -> Name
keyWithEntryPoint fname key =
  nameFromString $ maybe "" ((++ ".") . nameToString) fname ++ nameToString key

allocLocal :: AllocCompiler GPUMem r Imp.KernelOp
allocLocal mem size =
  sOp $ Imp.LocalAlloc mem size

kernelAlloc ::
  Pat LetDecMem ->
  SubExp ->
  Space ->
  InKernelGen ()
kernelAlloc (Pat [_]) _ ScalarSpace {} =
  -- Handled by the declaration of the memory block, which is then
  -- translated to an actual scalar variable during C code generation.
  pure ()
kernelAlloc (Pat [mem]) size (Space "local") =
  allocLocal (patElemName mem) $ Imp.bytes $ pe64 size
kernelAlloc (Pat [mem]) _ _ =
  compilerLimitationS $ "Cannot allocate memory block " ++ prettyString mem ++ " in kernel."
kernelAlloc dest _ _ =
  error $ "Invalid target for in-kernel allocation: " ++ show dest

updateAcc :: VName -> [SubExp] -> [SubExp] -> InKernelGen ()
updateAcc acc is vs = sComment "UpdateAcc" $ do
  -- See the ImpGen implementation of UpdateAcc for general notes.
  let is' = map pe64 is
  (c, space, arrs, dims, op) <- lookupAcc acc is'
  sWhen (inBounds (Slice (map DimFix is')) dims) $
    case op of
      Nothing ->
        forM_ (zip arrs vs) $ \(arr, v) -> copyDWIMFix arr is' v []
      Just lam -> do
        dLParams $ lambdaParams lam
        let (_x_params, y_params) =
              splitAt (length vs) $ map paramName $ lambdaParams lam
        forM_ (zip y_params vs) $ \(yp, v) -> copyDWIM yp [] v []
        atomics <- kernelAtomics <$> askEnv
        case atomicUpdateLocking atomics lam of
          AtomicPrim f -> f space arrs is'
          AtomicCAS f -> f space arrs is'
          AtomicLocking f -> do
            c_locks <- M.lookup c . kernelLocks <$> askEnv
            case c_locks of
              Just (Locks locks num_locks) -> do
                let locking =
                      Locking locks 0 1 0 $
                        pure . (`rem` fromIntegral num_locks) . flattenIndex dims
                f locking space arrs is'
              Nothing ->
                error $ "Missing locks for " ++ prettyString acc

-- | Generate a constant device array of 32-bit integer zeroes with
-- the given number of elements.  Initialised with a replicate.
genZeroes :: String -> Int -> CallKernelGen VName
genZeroes desc n = genConstants $ do
  counters_mem <- sAlloc (desc <> "_mem") (4 * fromIntegral n) (Space "device")
  let shape = Shape [intConst Int64 (fromIntegral n)]
  counters <- sArrayInMem desc int32 shape counters_mem
  sReplicate counters $ intConst Int32 0
  pure (namesFromList [counters_mem], counters)

compileThreadExp :: ExpCompiler GPUMem KernelEnv Imp.KernelOp
compileThreadExp (Pat [pe]) (BasicOp (Opaque _ se)) =
  -- Cannot print in GPU code.
  copyDWIM (patElemName pe) [] se []
compileThreadExp (Pat [dest]) (BasicOp (ArrayLit es _)) =
  forM_ (zip [0 ..] es) $ \(i, e) ->
    copyDWIMFix (patElemName dest) [fromIntegral (i :: Int64)] e []
compileThreadExp _ (BasicOp (UpdateAcc acc is vs)) =
  updateAcc acc is vs
compileThreadExp dest e =
  defCompileExp dest e

-- | Assign iterations of a for-loop to all threads in the kernel.
-- The passed-in function is invoked with the (symbolic) iteration.
-- The body must contain thread-level code.  For multidimensional
-- loops, use 'groupCoverSpace'.
kernelLoop ::
  (IntExp t) =>
  Imp.TExp t ->
  Imp.TExp t ->
  Imp.TExp t ->
  (Imp.TExp t -> InKernelGen ()) ->
  InKernelGen ()
kernelLoop tid num_threads n f =
  localOps threadOperations $
    if n == num_threads
      then f tid
      else do
        num_chunks <- dPrimVE "num_chunks" $ n `divUp` num_threads
        sFor "chunk_i" num_chunks $ \chunk_i -> do
          i <- dPrimVE "i" $ chunk_i * num_threads + tid
          sWhen (i .<. n) $ f i

-- | Assign iterations of a for-loop to threads in the workgroup.  The
-- passed-in function is invoked with the (symbolic) iteration.  For
-- multidimensional loops, use 'groupCoverSpace'.
groupLoop ::
  (IntExp t) =>
  Imp.TExp t ->
  (Imp.TExp t -> InKernelGen ()) ->
  InKernelGen ()
groupLoop n f = do
  constants <- kernelConstants <$> askEnv
  kernelLoop
    (kernelLocalThreadId constants `sExtAs` n)
    (kernelGroupSize constants `sExtAs` n)
    n
    f

-- | Iterate collectively though a multidimensional space, such that
-- all threads in the group participate.  The passed-in function is
-- invoked with a (symbolic) point in the index space.
groupCoverSpace ::
  (IntExp t) =>
  [Imp.TExp t] ->
  ([Imp.TExp t] -> InKernelGen ()) ->
  InKernelGen ()
groupCoverSpace ds f = do
  constants <- kernelConstants <$> askEnv
  let group_size = kernelGroupSize constants
  case splitFromEnd 1 ds of
    -- Optimise the case where the inner dimension of the space is
    -- equal to the group size.
    (ds', [last_d])
      | last_d == (group_size `sExtAs` last_d) -> do
          let ltid = kernelLocalThreadId constants `sExtAs` last_d
          sLoopSpace ds' $ \ds_is ->
            f $ ds_is ++ [ltid]
    _ ->
      groupLoop (product ds) $ f . unflattenIndex ds

-- Which fence do we need to protect shared access to this memory space?
fenceForSpace :: Space -> Imp.Fence
fenceForSpace (Space "local") = Imp.FenceLocal
fenceForSpace _ = Imp.FenceGlobal

-- | If we are touching these arrays, which kind of fence do we need?
fenceForArrays :: [VName] -> InKernelGen Imp.Fence
fenceForArrays = fmap (foldl' max Imp.FenceLocal) . mapM need
  where
    need arr =
      fmap (fenceForSpace . entryMemSpace)
        . lookupMemory
        . memLocName
        . entryArrayLoc
        =<< lookupArray arr

inBlockScan ::
  KernelConstants ->
  Maybe (Imp.TExp Int32 -> Imp.TExp Int32 -> Imp.TExp Bool) ->
  Imp.TExp Int64 ->
  Imp.TExp Int32 ->
  Imp.TExp Int32 ->
  Imp.TExp Bool ->
  [VName] ->
  InKernelGen () ->
  Lambda GPUMem ->
  InKernelGen ()
inBlockScan constants seg_flag arrs_full_size lockstep_width block_size active arrs barrier scan_lam = everythingVolatile $ do
  skip_threads <- dPrim "skip_threads" int32
  let actual_params = lambdaParams scan_lam
      (x_params, y_params) =
        splitAt (length actual_params `div` 2) actual_params
      y_to_x =
        forM_ (zip x_params y_params) $ \(x, y) ->
          when (primType (paramType x)) $
            copyDWIM (paramName x) [] (Var (paramName y)) []

  -- Set initial y values
  sComment "read input for in-block scan" $
    sWhen active $ do
      zipWithM_ readInitial y_params arrs
      -- Since the final result is expected to be in x_params, we may
      -- need to copy it there for the first thread in the block.
      sWhen (in_block_id .==. 0) y_to_x

  when array_scan barrier

  let op_to_x in_block_thread_active
        | Nothing <- seg_flag =
            localOps threadOperations
              . sWhen in_block_thread_active
              $ compileBody' x_params
              $ lambdaBody scan_lam
        | Just flag_true <- seg_flag = do
            inactive <-
              dPrimVE "inactive" $ flag_true (ltid32 - tvExp skip_threads) ltid32
            sWhen (in_block_thread_active .&&. inactive) $
              forM_ (zip x_params y_params) $ \(x, y) ->
                copyDWIM (paramName x) [] (Var (paramName y)) []
            -- The convoluted control flow is to ensure all threads
            -- hit this barrier (if applicable).
            when array_scan barrier
            localOps threadOperations
              . sWhen in_block_thread_active
              . sUnless inactive
              $ compileBody' x_params
              $ lambdaBody scan_lam

      maybeBarrier =
        sWhen
          (lockstep_width .<=. tvExp skip_threads)
          barrier

  sComment "in-block scan (hopefully no barriers needed)" $ do
    skip_threads <-- 1
    sWhile (tvExp skip_threads .<. block_size) $ do
      thread_active <-
        dPrimVE "thread_active" $ tvExp skip_threads .<=. in_block_id .&&. active

      sWhen thread_active . sComment "read operands" $
        zipWithM_ (readParam (sExt64 $ tvExp skip_threads)) x_params arrs
      sComment "perform operation" $ op_to_x thread_active

      maybeBarrier

      sWhen thread_active . sComment "write result" $
        sequence_ $
          zipWith3 writeResult x_params y_params arrs

      maybeBarrier

      skip_threads <-- tvExp skip_threads * 2
  where
    block_id = ltid32 `quot` block_size
    in_block_id = ltid32 - block_id * block_size
    ltid32 = kernelLocalThreadId constants
    ltid = sExt64 ltid32
    gtid = sExt64 $ kernelGlobalThreadId constants
    array_scan = not $ all primType $ lambdaReturnType scan_lam

    readInitial p arr
      | primType $ paramType p =
          copyDWIM (paramName p) [] (Var arr) [DimFix ltid]
      | otherwise =
          copyDWIM (paramName p) [] (Var arr) [DimFix gtid]

    readParam behind p arr
      | primType $ paramType p =
          copyDWIM (paramName p) [] (Var arr) [DimFix $ ltid - behind]
      | otherwise =
          copyDWIM (paramName p) [] (Var arr) [DimFix $ gtid - behind + arrs_full_size]

    writeResult x y arr
      | primType $ paramType x = do
          copyDWIM arr [DimFix ltid] (Var $ paramName x) []
          copyDWIM (paramName y) [] (Var $ paramName x) []
      | otherwise =
          copyDWIM (paramName y) [] (Var $ paramName x) []

groupScan ::
  Maybe (Imp.TExp Int32 -> Imp.TExp Int32 -> Imp.TExp Bool) ->
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  Lambda GPUMem ->
  [VName] ->
  InKernelGen ()
groupScan seg_flag arrs_full_size w lam arrs = do
  constants <- kernelConstants <$> askEnv
  renamed_lam <- renameLambda lam

  let ltid32 = kernelLocalThreadId constants
      ltid = sExt64 ltid32
      (x_params, y_params) = splitAt (length arrs) $ lambdaParams lam

  dLParams (lambdaParams lam ++ lambdaParams renamed_lam)

  ltid_in_bounds <- dPrimVE "ltid_in_bounds" $ ltid .<. w

  fence <- fenceForArrays arrs

  -- The scan works by splitting the group into blocks, which are
  -- scanned separately.  Typically, these blocks are smaller than
  -- the lockstep width, which enables barrier-free execution inside
  -- them.
  --
  -- We hardcode the block size here.  The only requirement is that
  -- it should not be less than the square root of the group size.
  -- With 32, we will work on groups of size 1024 or smaller, which
  -- fits every device Troels has seen.  Still, it would be nicer if
  -- it were a runtime parameter.  Some day.
  let block_size = 32
      simd_width = kernelWaveSize constants
      block_id = ltid32 `quot` block_size
      in_block_id = ltid32 - block_id * block_size
      doInBlockScan seg_flag' active =
        inBlockScan
          constants
          seg_flag'
          arrs_full_size
          simd_width
          block_size
          active
          arrs
          barrier
      array_scan = not $ all primType $ lambdaReturnType lam
      barrier
        | array_scan =
            sOp $ Imp.Barrier Imp.FenceGlobal
        | otherwise =
            sOp $ Imp.Barrier fence

      errorsync
        | array_scan =
            sOp $ Imp.ErrorSync Imp.FenceGlobal
        | otherwise =
            sOp $ Imp.ErrorSync Imp.FenceLocal

      group_offset = sExt64 (kernelGroupId constants) * kernelGroupSize constants

      writeBlockResult p arr
        | primType $ paramType p =
            copyDWIM arr [DimFix $ sExt64 block_id] (Var $ paramName p) []
        | otherwise =
            copyDWIM arr [DimFix $ group_offset + sExt64 block_id] (Var $ paramName p) []

      readPrevBlockResult p arr
        | primType $ paramType p =
            copyDWIM (paramName p) [] (Var arr) [DimFix $ sExt64 block_id - 1]
        | otherwise =
            copyDWIM (paramName p) [] (Var arr) [DimFix $ group_offset + sExt64 block_id - 1]

  doInBlockScan seg_flag ltid_in_bounds lam
  barrier

  let is_first_block = block_id .==. 0
  when array_scan $ do
    sComment "save correct values for first block" $
      sWhen is_first_block $
        forM_ (zip x_params arrs) $ \(x, arr) ->
          unless (primType $ paramType x) $
            copyDWIM arr [DimFix $ arrs_full_size + group_offset + sExt64 block_size + ltid] (Var $ paramName x) []

    barrier

  let last_in_block = in_block_id .==. block_size - 1
  sComment "last thread of block 'i' writes its result to offset 'i'" $
    sWhen (last_in_block .&&. ltid_in_bounds) $
      everythingVolatile $
        zipWithM_ writeBlockResult x_params arrs

  barrier

  let first_block_seg_flag = do
        flag_true <- seg_flag
        Just $ \from to ->
          flag_true (from * block_size + block_size - 1) (to * block_size + block_size - 1)
  comment
    "scan the first block, after which offset 'i' contains carry-in for block 'i+1'"
    $ doInBlockScan first_block_seg_flag (is_first_block .&&. ltid_in_bounds) renamed_lam

  errorsync

  when array_scan $ do
    sComment "move correct values for first block back a block" $
      sWhen is_first_block $
        forM_ (zip x_params arrs) $ \(x, arr) ->
          unless (primType $ paramType x) $
            copyDWIM
              arr
              [DimFix $ arrs_full_size + group_offset + ltid]
              (Var arr)
              [DimFix $ arrs_full_size + group_offset + sExt64 block_size + ltid]

    barrier

  no_carry_in <- dPrimVE "no_carry_in" $ is_first_block .||. bNot ltid_in_bounds

  let read_carry_in = sUnless no_carry_in $ do
        forM_ (zip x_params y_params) $ \(x, y) ->
          copyDWIM (paramName y) [] (Var (paramName x)) []
        zipWithM_ readPrevBlockResult x_params arrs

      op_to_x
        | Nothing <- seg_flag =
            sUnless no_carry_in $ compileBody' x_params $ lambdaBody lam
        | Just flag_true <- seg_flag = do
            inactive <-
              dPrimVE "inactive" $ flag_true (block_id * block_size - 1) ltid32
            sUnless no_carry_in . sWhen inactive . forM_ (zip x_params y_params) $ \(x, y) ->
              copyDWIM (paramName x) [] (Var (paramName y)) []
            -- The convoluted control flow is to ensure all threads
            -- hit this barrier (if applicable).
            when array_scan barrier
            sUnless no_carry_in $ sUnless inactive $ compileBody' x_params $ lambdaBody lam

      write_final_result =
        forM_ (zip x_params arrs) $ \(p, arr) ->
          when (primType $ paramType p) $
            copyDWIM arr [DimFix ltid] (Var $ paramName p) []

  sComment "carry-in for every block except the first" $
    localOps threadOperations $ do
      sComment "read operands" read_carry_in
      sComment "perform operation" op_to_x
      sComment "write final result" $ sUnless no_carry_in write_final_result

  barrier

  sComment "restore correct values for first block" $
    sWhen (is_first_block .&&. ltid_in_bounds) $
      forM_ (zip3 x_params y_params arrs) $ \(x, y, arr) ->
        if primType (paramType y)
          then copyDWIM arr [DimFix ltid] (Var $ paramName y) []
          else copyDWIM (paramName x) [] (Var arr) [DimFix $ arrs_full_size + group_offset + ltid]

  barrier

groupReduce ::
  Imp.TExp Int32 ->
  Lambda GPUMem ->
  [VName] ->
  InKernelGen ()
groupReduce w lam arrs = do
  offset <- dPrim "offset" int32
  groupReduceWithOffset offset w lam arrs

groupReduceWithOffset ::
  TV Int32 ->
  Imp.TExp Int32 ->
  Lambda GPUMem ->
  [VName] ->
  InKernelGen ()
groupReduceWithOffset offset w lam arrs = do
  constants <- kernelConstants <$> askEnv

  let local_tid = kernelLocalThreadId constants
      global_tid = kernelGlobalThreadId constants

      barrier
        | all primType $ lambdaReturnType lam = sOp $ Imp.Barrier Imp.FenceLocal
        | otherwise = sOp $ Imp.Barrier Imp.FenceGlobal

      errorsync
        | all primType $ lambdaReturnType lam = sOp $ Imp.ErrorSync Imp.FenceLocal
        | otherwise = sOp $ Imp.ErrorSync Imp.FenceGlobal

      readReduceArgument param arr
        | Prim _ <- paramType param = do
            let i = local_tid + tvExp offset
            copyDWIMFix (paramName param) [] (Var arr) [sExt64 i]
        | otherwise = do
            let i = global_tid + tvExp offset
            copyDWIMFix (paramName param) [] (Var arr) [sExt64 i]

      writeReduceOpResult param arr
        | Prim _ <- paramType param =
            copyDWIMFix arr [sExt64 local_tid] (Var $ paramName param) []
        | otherwise =
            pure ()

      writeArrayOpResult param arr
        | Prim _ <- paramType param =
            pure ()
        | otherwise =
            copyDWIMFix arr [sExt64 global_tid] (Var $ paramName param) []

  let (reduce_acc_params, reduce_arr_params) =
        splitAt (length arrs) $ lambdaParams lam

  skip_waves <- dPrimV "skip_waves" (1 :: Imp.TExp Int32)
  dLParams $ lambdaParams lam

  offset <-- (0 :: Imp.TExp Int32)

  comment "participating threads read initial accumulator" $
    localOps threadOperations . sWhen (local_tid .<. w) $
      zipWithM_ readReduceArgument reduce_acc_params arrs

  let do_reduce = localOps threadOperations $ do
        comment "read array element" $
          zipWithM_ readReduceArgument reduce_arr_params arrs
        comment "apply reduction operation" $
          compileBody' reduce_acc_params $
            lambdaBody lam
        comment "write result of operation" $
          zipWithM_ writeReduceOpResult reduce_acc_params arrs
      in_wave_reduce = everythingVolatile do_reduce

      wave_size = kernelWaveSize constants
      group_size = kernelGroupSize constants
      wave_id = local_tid `quot` wave_size
      in_wave_id = local_tid - wave_id * wave_size
      num_waves = (sExt32 group_size + wave_size - 1) `quot` wave_size
      arg_in_bounds = local_tid + tvExp offset .<. w

      doing_in_wave_reductions =
        tvExp offset .<. wave_size
      apply_in_in_wave_iteration =
        (in_wave_id .&. (2 * tvExp offset - 1)) .==. 0
      in_wave_reductions = do
        offset <-- (1 :: Imp.TExp Int32)
        sWhile doing_in_wave_reductions $ do
          sWhen
            (arg_in_bounds .&&. apply_in_in_wave_iteration)
            in_wave_reduce
          offset <-- tvExp offset * 2

      doing_cross_wave_reductions =
        tvExp skip_waves .<. num_waves
      is_first_thread_in_wave =
        in_wave_id .==. 0
      wave_not_skipped =
        (wave_id .&. (2 * tvExp skip_waves - 1)) .==. 0
      apply_in_cross_wave_iteration =
        arg_in_bounds .&&. is_first_thread_in_wave .&&. wave_not_skipped
      cross_wave_reductions =
        sWhile doing_cross_wave_reductions $ do
          barrier
          offset <-- tvExp skip_waves * wave_size
          sWhen apply_in_cross_wave_iteration do_reduce
          skip_waves <-- tvExp skip_waves * 2

  in_wave_reductions
  cross_wave_reductions
  errorsync

  sComment "Copy array-typed operands to result array" $ do
    sWhen (local_tid .==. 0) $
      localOps threadOperations $
        zipWithM_ writeArrayOpResult reduce_acc_params arrs

compileThreadOp :: OpCompiler GPUMem KernelEnv Imp.KernelOp
compileThreadOp pat (Alloc size space) =
  kernelAlloc pat size space
compileThreadOp pat _ =
  compilerBugS $ "compileThreadOp: cannot compile rhs of binding " ++ prettyString pat

-- | Locking strategy used for an atomic update.
data Locking = Locking
  { -- | Array containing the lock.
    lockingArray :: VName,
    -- | Value for us to consider the lock free.
    lockingIsUnlocked :: Imp.TExp Int32,
    -- | What to write when we lock it.
    lockingToLock :: Imp.TExp Int32,
    -- | What to write when we unlock it.
    lockingToUnlock :: Imp.TExp Int32,
    -- | A transformation from the logical lock index to the
    -- physical position in the array.  This can also be used
    -- to make the lock array smaller.
    lockingMapping :: [Imp.TExp Int64] -> [Imp.TExp Int64]
  }

-- | A function for generating code for an atomic update.  Assumes
-- that the bucket is in-bounds.
type DoAtomicUpdate rep r =
  Space -> [VName] -> [Imp.TExp Int64] -> ImpM rep r Imp.KernelOp ()

-- | The mechanism that will be used for performing the atomic update.
-- Approximates how efficient it will be.  Ordered from most to least
-- efficient.
data AtomicUpdate rep r
  = -- | Supported directly by primitive.
    AtomicPrim (DoAtomicUpdate rep r)
  | -- | Can be done by efficient swaps.
    AtomicCAS (DoAtomicUpdate rep r)
  | -- | Requires explicit locking.
    AtomicLocking (Locking -> DoAtomicUpdate rep r)

-- | Is there an atomic t'BinOp' corresponding to this t'BinOp'?
type AtomicBinOp =
  BinOp ->
  Maybe (VName -> VName -> Count Imp.Elements (Imp.TExp Int64) -> Imp.Exp -> Imp.AtomicOp)

-- | Do an atomic update corresponding to a binary operator lambda.
atomicUpdateLocking ::
  AtomicBinOp ->
  Lambda GPUMem ->
  AtomicUpdate GPUMem KernelEnv
atomicUpdateLocking atomicBinOp lam
  | Just ops_and_ts <- lamIsBinOp lam,
    all (\(_, t, _, _) -> primBitSize t `elem` [32, 64]) ops_and_ts =
      primOrCas ops_and_ts $ \space arrs bucket ->
        -- If the operator is a vectorised binary operator on 32/64-bit
        -- values, we can use a particularly efficient
        -- implementation. If the operator has an atomic implementation
        -- we use that, otherwise it is still a binary operator which
        -- can be implemented by atomic compare-and-swap if 32/64 bits.
        forM_ (zip arrs ops_and_ts) $ \(a, (op, t, x, y)) -> do
          -- Common variables.
          old <- dPrim "old" t

          (arr', _a_space, bucket_offset) <- fullyIndexArray a bucket

          case opHasAtomicSupport space (tvVar old) arr' bucket_offset op of
            Just f -> sOp $ f $ Imp.var y t
            Nothing ->
              atomicUpdateCAS space t a (tvVar old) bucket x $
                x <~~ Imp.BinOpExp op (Imp.var x t) (Imp.var y t)
  where
    opHasAtomicSupport space old arr' bucket' bop = do
      let atomic f = Imp.Atomic space . f old arr' bucket'
      atomic <$> atomicBinOp bop

    primOrCas ops
      | all isPrim ops = AtomicPrim
      | otherwise = AtomicCAS

    isPrim (op, _, _, _) = isJust $ atomicBinOp op

-- If the operator functions purely on single 32/64-bit values, we can
-- use an implementation based on CAS, no matter what the operator
-- does.
atomicUpdateLocking _ op
  | [Prim t] <- lambdaReturnType op,
    [xp, _] <- lambdaParams op,
    primBitSize t `elem` [32, 64] = AtomicCAS $ \space [arr] bucket -> do
      old <- dPrim "old" t
      atomicUpdateCAS space t arr (tvVar old) bucket (paramName xp) $
        compileBody' [xp] $
          lambdaBody op
atomicUpdateLocking _ op = AtomicLocking $ \locking space arrs bucket -> do
  old <- dPrim "old" int32
  continue <- dPrimVol "continue" Bool true

  -- Correctly index into locks.
  (locks', _locks_space, locks_offset) <-
    fullyIndexArray (lockingArray locking) $ lockingMapping locking bucket

  -- Critical section
  let try_acquire_lock =
        sOp $
          Imp.Atomic space $
            Imp.AtomicCmpXchg
              int32
              (tvVar old)
              locks'
              locks_offset
              (untyped $ lockingIsUnlocked locking)
              (untyped $ lockingToLock locking)
      lock_acquired = tvExp old .==. lockingIsUnlocked locking
      -- Even the releasing is done with an atomic rather than a
      -- simple write, for memory coherency reasons.
      release_lock =
        sOp $
          Imp.Atomic space $
            Imp.AtomicCmpXchg
              int32
              (tvVar old)
              locks'
              locks_offset
              (untyped $ lockingToLock locking)
              (untyped $ lockingToUnlock locking)
      break_loop = continue <-- false

  -- Preparing parameters. It is assumed that the caller has already
  -- filled the arr_params. We copy the current value to the
  -- accumulator parameters.
  --
  -- Note the use of 'everythingVolatile' when reading and writing the
  -- buckets.  This was necessary to ensure correct execution on a
  -- newer NVIDIA GPU (RTX 2080).  The 'volatile' modifiers likely
  -- make the writes pass through the (SM-local) L1 cache, which is
  -- necessary here, because we are really doing device-wide
  -- synchronisation without atomics (naughty!).
  let (acc_params, _arr_params) = splitAt (length arrs) $ lambdaParams op
      bind_acc_params =
        everythingVolatile $
          sComment "bind lhs" $
            forM_ (zip acc_params arrs) $ \(acc_p, arr) ->
              copyDWIMFix (paramName acc_p) [] (Var arr) bucket

  let op_body =
        sComment "execute operation" $
          compileBody' acc_params $
            lambdaBody op

      do_hist =
        everythingVolatile $
          sComment "update global result" $
            zipWithM_ (writeArray bucket) arrs $
              map (Var . paramName) acc_params

      fence = sOp $ Imp.MemFence $ fenceForSpace space

  -- While-loop: Try to insert your value
  sWhile (tvExp continue) $ do
    try_acquire_lock
    sWhen lock_acquired $ do
      dLParams acc_params
      bind_acc_params
      op_body
      do_hist
      fence
      release_lock
      break_loop
    fence
  where
    writeArray bucket arr val = copyDWIMFix arr bucket val []

atomicUpdateCAS ::
  Space ->
  PrimType ->
  VName ->
  VName ->
  [Imp.TExp Int64] ->
  VName ->
  InKernelGen () ->
  InKernelGen ()
atomicUpdateCAS space t arr old bucket x do_op = do
  -- Code generation target:
  --
  -- old = d_his[idx];
  -- do {
  --   assumed = old;
  --   x = do_op(assumed, y);
  --   old = atomicCAS(&d_his[idx], assumed, tmp);
  -- } while(assumed != old);
  assumed <- tvVar <$> dPrim "assumed" t
  run_loop <- dPrimV "run_loop" true

  -- XXX: CUDA may generate really bad code if this is not a volatile
  -- read.  Unclear why.  The later reads are volatile, so maybe
  -- that's it.
  everythingVolatile $ copyDWIMFix old [] (Var arr) bucket

  (arr', _a_space, bucket_offset) <- fullyIndexArray arr bucket

  -- While-loop: Try to insert your value
  let (toBits, fromBits) =
        case t of
          FloatType Float16 ->
            ( \v -> Imp.FunExp "to_bits16" [v] int16,
              \v -> Imp.FunExp "from_bits16" [v] t
            )
          FloatType Float32 ->
            ( \v -> Imp.FunExp "to_bits32" [v] int32,
              \v -> Imp.FunExp "from_bits32" [v] t
            )
          FloatType Float64 ->
            ( \v -> Imp.FunExp "to_bits64" [v] int64,
              \v -> Imp.FunExp "from_bits64" [v] t
            )
          _ -> (id, id)

      int
        | primBitSize t == 16 = int16
        | primBitSize t == 32 = int32
        | otherwise = int64

  sWhile (tvExp run_loop) $ do
    assumed <~~ Imp.var old t
    x <~~ Imp.var assumed t
    do_op
    old_bits_v <- newVName "old_bits"
    dPrim_ old_bits_v int
    let old_bits = Imp.var old_bits_v int
    sOp . Imp.Atomic space $
      Imp.AtomicCmpXchg
        int
        old_bits_v
        arr'
        bucket_offset
        (toBits (Imp.var assumed t))
        (toBits (Imp.var x t))
    old <~~ fromBits old_bits
    let won = CmpOpExp (CmpEq int) (toBits (Imp.var assumed t)) old_bits
    sWhen (isBool won) (run_loop <-- false)

computeKernelUses ::
  (FreeIn a) =>
  a ->
  [VName] ->
  CallKernelGen [Imp.KernelUse]
computeKernelUses kernel_body bound_in_kernel = do
  let actually_free = freeIn kernel_body `namesSubtract` namesFromList bound_in_kernel
  -- Compute the variables that we need to pass to the kernel.
  nubOrd <$> readsFromSet actually_free

readsFromSet :: Names -> CallKernelGen [Imp.KernelUse]
readsFromSet = fmap catMaybes . mapM f . namesToList
  where
    f var = do
      t <- lookupType var
      vtable <- getVTable
      case t of
        Array {} -> pure Nothing
        Acc {} -> pure Nothing
        Mem (Space "local") -> pure Nothing
        Mem {} -> pure $ Just $ Imp.MemoryUse var
        Prim bt ->
          isConstExp vtable (Imp.var var bt) >>= \case
            Just ce -> pure $ Just $ Imp.ConstUse var ce
            Nothing -> pure $ Just $ Imp.ScalarUse var bt

isConstExp ::
  VTable GPUMem ->
  Imp.Exp ->
  ImpM rep r op (Maybe Imp.KernelConstExp)
isConstExp vtable size = do
  fname <- askFunction
  let onLeaf name _ = lookupConstExp name
      lookupConstExp name =
        constExp =<< hasExp =<< M.lookup name vtable
      constExp (Op (Inner (SizeOp (GetSize key _)))) =
        Just $ LeafExp (Imp.SizeConst $ keyWithEntryPoint fname key) int32
      constExp (Op (Inner (SizeOp (GetSizeMax size_class)))) =
        Just $ LeafExp (Imp.SizeMaxConst size_class) int32
      constExp e = primExpFromExp lookupConstExp e
  pure $ replaceInPrimExpM onLeaf size
  where
    hasExp (ArrayVar e _) = e
    hasExp (AccVar e _) = e
    hasExp (ScalarVar e _) = e
    hasExp (MemVar e _) = e

kernelInitialisationSimple ::
  Count NumGroups SubExp ->
  Count GroupSize SubExp ->
  CallKernelGen (KernelConstants, InKernelGen ())
kernelInitialisationSimple num_groups group_size = do
  global_tid <- newVName "global_tid"
  local_tid <- newVName "local_tid"
  group_id <- newVName "group_tid"
  wave_size <- newVName "wave_size"
  inner_group_size <- newVName "group_size"
  let num_groups' = Imp.pe64 (unCount num_groups)
      group_size' = Imp.pe64 (unCount group_size)
      constants =
        KernelConstants
          { kernelGlobalThreadId = Imp.le32 global_tid,
            kernelLocalThreadId = Imp.le32 local_tid,
            kernelGroupId = Imp.le32 group_id,
            kernelGlobalThreadIdVar = global_tid,
            kernelLocalThreadIdVar = local_tid,
            kernelNumGroupsCount = num_groups,
            kernelGroupSizeCount = group_size,
            kernelGroupIdVar = group_id,
            kernelNumGroups = num_groups',
            kernelGroupSize = group_size',
            kernelNumThreads = sExt32 (group_size' * num_groups'),
            kernelWaveSize = Imp.le32 wave_size,
            kernelLocalIdMap = mempty,
            kernelChunkItersMap = mempty
          }

  let set_constants = do
        dPrim_ local_tid int32
        dPrim_ inner_group_size int64
        dPrim_ wave_size int32
        dPrim_ group_id int32

        sOp (Imp.GetLocalId local_tid 0)
        sOp (Imp.GetLocalSize inner_group_size 0)
        sOp (Imp.GetLockstepWidth wave_size)
        sOp (Imp.GetGroupId group_id 0)
        dPrimV_ global_tid $ le32 group_id * le32 inner_group_size + le32 local_tid

  pure (constants, set_constants)

isActive :: [(VName, SubExp)] -> Imp.TExp Bool
isActive limit = case actives of
  [] -> true
  x : xs -> foldl (.&&.) x xs
  where
    (is, ws) = unzip limit
    actives = zipWith active is $ map pe64 ws
    active i = (Imp.le64 i .<.)

-- | Change every memory block to be in the global address space,
-- except those who are in the local memory space.  This only affects
-- generated code - we still need to make sure that the memory is
-- actually present on the device (and declared as variables in the
-- kernel).
makeAllMemoryGlobal :: CallKernelGen a -> CallKernelGen a
makeAllMemoryGlobal =
  localDefaultSpace (Imp.Space "global") . localVTable (M.map globalMemory)
  where
    globalMemory (MemVar _ entry)
      | entryMemSpace entry /= Space "local" =
          MemVar Nothing entry {entryMemSpace = Imp.Space "global"}
    globalMemory entry =
      entry

simpleKernelGroups ::
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  CallKernelGen (Imp.TExp Int32, Count NumGroups SubExp, Count GroupSize SubExp)
simpleKernelGroups max_num_groups kernel_size = do
  group_size <- dPrim "group_size" int64
  fname <- askFunction
  let group_size_key = keyWithEntryPoint fname $ nameFromString $ prettyString $ tvVar group_size
  sOp $ Imp.GetSize (tvVar group_size) group_size_key Imp.SizeGroup
  virt_num_groups <- dPrimVE "virt_num_groups" $ kernel_size `divUp` tvExp group_size
  num_groups <- dPrimV "num_groups" $ virt_num_groups `sMin64` max_num_groups
  pure (sExt32 virt_num_groups, Count $ tvSize num_groups, Count $ tvSize group_size)

simpleKernelConstants ::
  Imp.TExp Int64 ->
  String ->
  CallKernelGen
    ( (Imp.TExp Int64 -> InKernelGen ()) -> InKernelGen (),
      KernelConstants
    )
simpleKernelConstants kernel_size desc = do
  -- For performance reasons, codegen assumes that the thread count is
  -- never more than will fit in an i32.  This means we need to cap
  -- the number of groups here.  The cap is set much higher than any
  -- GPU will possibly need.  Feel free to come back and laugh at me
  -- in the future.
  let max_num_groups = 1024 * 1024
  thread_gtid <- newVName $ desc ++ "_gtid"
  thread_ltid <- newVName $ desc ++ "_ltid"
  group_id <- newVName $ desc ++ "_gid"
  inner_group_size <- newVName "group_size"
  (virt_num_groups, num_groups, group_size) <-
    simpleKernelGroups max_num_groups kernel_size
  let group_size' = Imp.pe64 $ unCount group_size
      num_groups' = Imp.pe64 $ unCount num_groups

      constants =
        KernelConstants
          { kernelGlobalThreadId = Imp.le32 thread_gtid,
            kernelLocalThreadId = Imp.le32 thread_ltid,
            kernelGroupId = Imp.le32 group_id,
            kernelGlobalThreadIdVar = thread_gtid,
            kernelLocalThreadIdVar = thread_ltid,
            kernelGroupIdVar = group_id,
            kernelNumGroupsCount = num_groups,
            kernelGroupSizeCount = group_size,
            kernelNumGroups = num_groups',
            kernelGroupSize = group_size',
            kernelNumThreads = sExt32 (group_size' * num_groups'),
            kernelWaveSize = 0,
            kernelLocalIdMap = mempty,
            kernelChunkItersMap = mempty
          }

      wrapKernel m = do
        dPrim_ thread_ltid int32
        dPrim_ inner_group_size int64
        dPrim_ group_id int32
        sOp (Imp.GetLocalId thread_ltid 0)
        sOp (Imp.GetLocalSize inner_group_size 0)
        sOp (Imp.GetGroupId group_id 0)
        dPrimV_ thread_gtid $ le32 group_id * le32 inner_group_size + le32 thread_ltid
        virtualiseGroups SegVirt virt_num_groups $ \virt_group_id -> do
          global_tid <-
            dPrimVE "global_tid" $
              sExt64 virt_group_id * sExt64 (le32 inner_group_size)
                + sExt64 (kernelLocalThreadId constants)
          m global_tid

  pure (wrapKernel, constants)

-- | For many kernels, we may not have enough physical groups to cover
-- the logical iteration space.  Some groups thus have to perform
-- double duty; we put an outer loop to accomplish this.  The
-- advantage over just launching a bazillion threads is that the cost
-- of memory expansion should be proportional to the number of
-- *physical* threads (hardware parallelism), not the amount of
-- application parallelism.
virtualiseGroups ::
  SegVirt ->
  Imp.TExp Int32 ->
  (Imp.TExp Int32 -> InKernelGen ()) ->
  InKernelGen ()
virtualiseGroups SegVirt required_groups m = do
  constants <- kernelConstants <$> askEnv
  phys_group_id <- dPrim "phys_group_id" int32
  sOp $ Imp.GetGroupId (tvVar phys_group_id) 0
  iterations <-
    dPrimVE "iterations" $
      (required_groups - tvExp phys_group_id) `divUp` sExt32 (kernelNumGroups constants)

  sFor "i" iterations $ \i -> do
    m . tvExp
      =<< dPrimV
        "virt_group_id"
        (tvExp phys_group_id + i * sExt32 (kernelNumGroups constants))
    -- Make sure the virtual group is actually done before we let
    -- another virtual group have its way with it.
    sOp $ Imp.ErrorSync Imp.FenceGlobal
virtualiseGroups _ _ m = do
  gid <- kernelGroupIdVar . kernelConstants <$> askEnv
  m $ Imp.le32 gid

-- | Various extra configuration of the kernel being generated.
data KernelAttrs = KernelAttrs
  { -- | Can this kernel execute correctly even if previous kernels failed?
    kAttrFailureTolerant :: Bool,
    -- | Does whatever launch this kernel check for local memory capacity itself?
    kAttrCheckLocalMemory :: Bool,
    -- | Number of groups.
    kAttrNumGroups :: Count NumGroups SubExp,
    -- | Group size.
    kAttrGroupSize :: Count GroupSize SubExp
  }

-- | The default kernel attributes.
defKernelAttrs ::
  Count NumGroups SubExp ->
  Count GroupSize SubExp ->
  KernelAttrs
defKernelAttrs num_groups group_size =
  KernelAttrs
    { kAttrFailureTolerant = False,
      kAttrCheckLocalMemory = True,
      kAttrNumGroups = num_groups,
      kAttrGroupSize = group_size
    }

getSize :: String -> SizeClass -> CallKernelGen (TV Int64)
getSize desc size_class = do
  v <- dPrim desc int64
  fname <- askFunction
  let v_key = keyWithEntryPoint fname $ nameFromString $ prettyString $ tvVar v
  sOp $ Imp.GetSize (tvVar v) v_key size_class
  pure v

-- | Compute kernel attributes from 'SegLevel'; including synthesising
-- group-size and thread count if no grid is provided.
lvlKernelAttrs :: SegLevel -> CallKernelGen KernelAttrs
lvlKernelAttrs lvl =
  case lvl of
    SegThread _ Nothing -> mkGrid
    SegThread _ (Just (KernelGrid num_groups group_size)) ->
      pure $ defKernelAttrs num_groups group_size
    SegGroup _ Nothing -> mkGrid
    SegGroup _ (Just (KernelGrid num_groups group_size)) ->
      pure $ defKernelAttrs num_groups group_size
    SegThreadInGroup {} ->
      error "lvlKernelAttrs: SegThreadInGroup"
  where
    mkGrid = do
      group_size <- getSize "group_size" Imp.SizeGroup
      num_groups <- getSize "num_groups" Imp.SizeNumGroups
      pure $ defKernelAttrs (Count $ tvSize num_groups) (Count $ tvSize group_size)

sKernel ::
  Operations GPUMem KernelEnv Imp.KernelOp ->
  (KernelConstants -> Imp.TExp Int32) ->
  String ->
  VName ->
  KernelAttrs ->
  InKernelGen () ->
  CallKernelGen ()
sKernel ops flatf name v attrs f = do
  (constants, set_constants) <-
    kernelInitialisationSimple (kAttrNumGroups attrs) (kAttrGroupSize attrs)
  name' <- nameForFun $ name ++ "_" ++ show (baseTag v)
  sKernelOp attrs constants ops name' $ do
    set_constants
    dPrimV_ v $ flatf constants
    f

sKernelThread ::
  String ->
  VName ->
  KernelAttrs ->
  InKernelGen () ->
  CallKernelGen ()
sKernelThread = sKernel threadOperations kernelGlobalThreadId

sKernelOp ::
  KernelAttrs ->
  KernelConstants ->
  Operations GPUMem KernelEnv Imp.KernelOp ->
  Name ->
  InKernelGen () ->
  CallKernelGen ()
sKernelOp attrs constants ops name m = do
  HostEnv atomics _ locks <- askEnv
  body <- makeAllMemoryGlobal $ subImpM_ (KernelEnv atomics constants locks) ops m
  uses <- computeKernelUses body mempty
  group_size <- onGroupSize $ kernelGroupSize constants
  emit . Imp.Op . Imp.CallKernel $
    Imp.Kernel
      { Imp.kernelBody = body,
        Imp.kernelUses = uses,
        Imp.kernelNumGroups = [untyped $ kernelNumGroups constants],
        Imp.kernelGroupSize = [group_size],
        Imp.kernelName = name,
        Imp.kernelFailureTolerant = kAttrFailureTolerant attrs,
        Imp.kernelCheckLocalMemory = kAttrCheckLocalMemory attrs
      }
  where
    -- Figure out if this expression actually corresponds to a
    -- KernelConst.
    onGroupSize e = do
      vtable <- getVTable
      x <- isConstExp vtable $ untyped e
      pure $
        case x of
          Just (LeafExp kc _) -> Right kc
          _ -> Left $ untyped e

sKernelFailureTolerant ::
  Bool ->
  Operations GPUMem KernelEnv Imp.KernelOp ->
  KernelConstants ->
  Name ->
  InKernelGen () ->
  CallKernelGen ()
sKernelFailureTolerant tol ops constants name m = do
  sKernelOp attrs constants ops name m
  where
    attrs =
      ( defKernelAttrs
          (kernelNumGroupsCount constants)
          (kernelGroupSizeCount constants)
      )
        { kAttrFailureTolerant = tol
        }

threadOperations :: Operations GPUMem KernelEnv Imp.KernelOp
threadOperations =
  (defaultOperations compileThreadOp)
    { opsCopyCompiler = lmadCopy,
      opsExpCompiler = compileThreadExp,
      opsStmsCompiler = \_ -> defCompileStms mempty,
      opsAllocCompilers =
        M.fromList [(Space "local", allocLocal)]
    }

-- | Perform a Replicate with a kernel.
sReplicateKernel :: VName -> SubExp -> CallKernelGen ()
sReplicateKernel arr se = do
  t <- subExpType se
  ds <- dropLast (arrayRank t) . arrayDims <$> lookupType arr

  let dims = map pe64 $ ds ++ arrayDims t
  n <- dPrimVE "replicate_n" $ product $ map sExt64 dims
  (virtualise, constants) <- simpleKernelConstants n "replicate"

  fname <- askFunction
  let name =
        keyWithEntryPoint fname $
          nameFromString $
            "replicate_" ++ show (baseTag $ kernelGlobalThreadIdVar constants)

  sKernelFailureTolerant True threadOperations constants name $
    virtualise $ \gtid -> do
      is' <- dIndexSpace' "rep_i" dims gtid
      sWhen (gtid .<. n) $
        copyDWIMFix arr is' se $
          drop (length ds) is'

replicateName :: PrimType -> String
replicateName bt = "replicate_" ++ prettyString bt

replicateForType :: PrimType -> CallKernelGen Name
replicateForType bt = do
  let fname = nameFromString $ "builtin#" <> replicateName bt

  exists <- hasFunction fname
  unless exists $ do
    mem <- newVName "mem"
    num_elems <- newVName "num_elems"
    val <- newVName "val"

    let params =
          [ Imp.MemParam mem (Space "device"),
            Imp.ScalarParam num_elems int64,
            Imp.ScalarParam val bt
          ]
        shape = Shape [Var num_elems]
    function fname [] params $ do
      arr <-
        sArray "arr" bt shape mem $ LMAD.iota 0 $ map pe64 $ shapeDims shape
      sReplicateKernel arr $ Var val

  pure fname

replicateIsFill :: VName -> SubExp -> CallKernelGen (Maybe (CallKernelGen ()))
replicateIsFill arr v = do
  ArrayEntry (MemLoc arr_mem arr_shape arr_ixfun) _ <- lookupArray arr
  v_t <- subExpType v
  case v_t of
    Prim v_t'
      | LMAD.isDirect arr_ixfun -> pure $
          Just $ do
            fname <- replicateForType v_t'
            emit $
              Imp.Call
                []
                fname
                [ Imp.MemArg arr_mem,
                  Imp.ExpArg $ untyped $ product $ map pe64 arr_shape,
                  Imp.ExpArg $ toExp' v_t' v
                ]
    _ -> pure Nothing

-- | Perform a Replicate with a kernel.
sReplicate :: VName -> SubExp -> CallKernelGen ()
sReplicate arr se = do
  -- If the replicate is of a particularly common and simple form
  -- (morally a memset()/fill), then we use a common function.
  is_fill <- replicateIsFill arr se

  case is_fill of
    Just m -> m
    Nothing -> sReplicateKernel arr se

-- | Perform an Iota with a kernel.
sIotaKernel ::
  VName ->
  Imp.TExp Int64 ->
  Imp.Exp ->
  Imp.Exp ->
  IntType ->
  CallKernelGen ()
sIotaKernel arr n x s et = do
  destloc <- entryArrayLoc <$> lookupArray arr
  (virtualise, constants) <- simpleKernelConstants n "iota"

  fname <- askFunction
  let name =
        keyWithEntryPoint fname $
          nameFromString $
            "iota_"
              ++ prettyString et
              ++ "_"
              ++ show (baseTag $ kernelGlobalThreadIdVar constants)

  sKernelFailureTolerant True threadOperations constants name $
    virtualise $ \gtid ->
      sWhen (gtid .<. n) $ do
        (destmem, destspace, destidx) <- fullyIndexArray' destloc [gtid]

        emit $
          Imp.Write destmem destidx (IntType et) destspace Imp.Nonvolatile $
            BinOpExp
              (Add et OverflowWrap)
              (BinOpExp (Mul et OverflowWrap) (Imp.sExt et $ untyped gtid) s)
              x

iotaName :: IntType -> String
iotaName bt = "iota_" ++ prettyString bt

iotaForType :: IntType -> CallKernelGen Name
iotaForType bt = do
  let fname = nameFromString $ "builtin#" <> iotaName bt

  exists <- hasFunction fname
  unless exists $ do
    mem <- newVName "mem"
    n <- newVName "n"
    x <- newVName "x"
    s <- newVName "s"

    let params =
          [ Imp.MemParam mem (Space "device"),
            Imp.ScalarParam n int64,
            Imp.ScalarParam x $ IntType bt,
            Imp.ScalarParam s $ IntType bt
          ]
        shape = Shape [Var n]
        n' = Imp.le64 n
        x' = Imp.var x $ IntType bt
        s' = Imp.var s $ IntType bt

    function fname [] params $ do
      arr <-
        sArray "arr" (IntType bt) shape mem $
          LMAD.iota 0 (map pe64 (shapeDims shape))
      sIotaKernel arr n' x' s' bt

  pure fname

-- | Perform an Iota with a kernel.
sIota ::
  VName ->
  Imp.TExp Int64 ->
  Imp.Exp ->
  Imp.Exp ->
  IntType ->
  CallKernelGen ()
sIota arr n x s et = do
  ArrayEntry (MemLoc arr_mem _ arr_ixfun) _ <- lookupArray arr
  if LMAD.isDirect arr_ixfun
    then do
      fname <- iotaForType et
      emit $
        Imp.Call
          []
          fname
          [Imp.MemArg arr_mem, Imp.ExpArg $ untyped n, Imp.ExpArg x, Imp.ExpArg s]
    else sIotaKernel arr n x s et

compileThreadResult ::
  SegSpace ->
  PatElem LetDecMem ->
  KernelResult ->
  InKernelGen ()
compileThreadResult _ _ RegTileReturns {} =
  compilerLimitationS "compileThreadResult: RegTileReturns not yet handled."
compileThreadResult space pe (Returns _ _ what) = do
  let is = map (Imp.le64 . fst) $ unSegSpace space
  copyDWIMFix (patElemName pe) is what []
compileThreadResult _ pe (WriteReturns _ arr dests) = do
  arr_t <- lookupType arr
  let rws' = map pe64 $ arrayDims arr_t
  forM_ dests $ \(slice, e) -> do
    let slice' = fmap pe64 slice
        write = inBounds slice' rws'
    sWhen write $ copyDWIM (patElemName pe) (unSlice slice') e []
compileThreadResult _ _ TileReturns {} =
  compilerBugS "compileThreadResult: TileReturns unhandled."
