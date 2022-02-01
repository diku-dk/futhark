{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.CodeGen.ImpGen.GPU.Base
  ( KernelConstants (..),
    keyWithEntryPoint,
    CallKernelGen,
    InKernelGen,
    Locks (..),
    HostEnv (..),
    Target (..),
    KernelEnv (..),
    computeThreadChunkSize,
    groupReduce,
    groupScan,
    isActive,
    sKernelThread,
    sKernelGroup,
    KernelAttrs (..),
    defKernelAttrs,
    sReplicate,
    sIota,
    sCopy,
    compileThreadResult,
    compileGroupResult,
    virtualiseGroups,
    kernelLoop,
    groupCoverSpace,
    Precomputed,
    precomputeConstants,
    precomputedConstants,
    atomicUpdateLocking,
    AtomicBinOp,
    Locking (..),
    AtomicUpdate (..),
    DoAtomicUpdate,
  )
where

import Control.Monad.Except
import Data.Bifunctor
import Data.List (foldl', partition, zip4)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Futhark.CodeGen.ImpCode.GPU as Imp
import Futhark.CodeGen.ImpGen
import Futhark.Construct (fullSliceNum)
import Futhark.Error
import Futhark.IR.GPUMem
import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.MonadFreshNames
import Futhark.Transform.Rename
import Futhark.Util (chunks, dropLast, mapAccumLM, nubOrd, takeLast)
import Futhark.Util.IntegralExp (divUp, quot, rem)
import Prelude hiding (quot, rem)

-- | Which target are we ultimately generating code for?  While most
-- of the kernels code is the same, there are some cases where we
-- generate special code based on the ultimate low-level API we are
-- targeting.
data Target = CUDA | OpenCL

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
    kernelNumGroups :: Imp.TExp Int64,
    kernelGroupSize :: Imp.TExp Int64,
    kernelNumThreads :: Imp.TExp Int32,
    kernelWaveSize :: Imp.TExp Int32,
    kernelThreadActive :: Imp.TExp Bool,
    -- | A mapping from dimensions of nested SegOps to already
    -- computed local thread IDs.  Only valid in non-virtualised case.
    kernelLocalIdMap :: M.Map [SubExp] [Imp.TExp Int32],
    -- | Mapping from dimensions of nested SegOps to how many
    -- iterations the virtualisation loop needs.
    kernelChunkItersMap :: M.Map [SubExp] (Imp.TExp Int32)
  }

-- | The sizes of nested iteration spaces in the kernel.
type SegOpSizes = S.Set [SubExp]

-- | Find the sizes of nested parallelism in a 'SegOp' body.
segOpSizes :: Stms GPUMem -> SegOpSizes
segOpSizes = onStms
  where
    onStms = foldMap (onExp . stmExp)
    onExp (Op (Inner (SegOp op))) =
      case segVirt $ segLevel op of
        SegNoVirtFull seq_dims ->
          S.singleton $ map snd $ snd $ partitionSeqDims seq_dims $ segSpace op
        _ -> S.singleton $ map snd $ unSegSpace $ segSpace op
    onExp (BasicOp (Replicate shape _)) =
      S.singleton $ shapeDims shape
    onExp (If _ tbranch fbranch _) =
      onStms (bodyStms tbranch) <> onStms (bodyStms fbranch)
    onExp (DoLoop _ _ body) =
      onStms (bodyStms body)
    onExp _ = mempty

-- | Various useful precomputed information.
data Precomputed = Precomputed
  { pcSegOpSizes :: SegOpSizes,
    pcChunkItersMap :: M.Map [SubExp] (Imp.TExp Int32)
  }

-- | Precompute various constants and useful information.
precomputeConstants :: Count GroupSize (Imp.TExp Int64) -> Stms GPUMem -> CallKernelGen Precomputed
precomputeConstants group_size stms = do
  let sizes = segOpSizes stms
  iters_map <- M.fromList <$> mapM mkMap (S.toList sizes)
  pure $ Precomputed sizes iters_map
  where
    mkMap dims = do
      let n = product $ map Imp.pe64 dims
      num_chunks <- dPrimVE "num_chunks" $ sExt32 $ n `divUp` unCount group_size
      pure (dims, num_chunks)

-- | Make use of various precomputed constants.
precomputedConstants :: Precomputed -> InKernelGen a -> InKernelGen a
precomputedConstants pre m = do
  ltid <- kernelLocalThreadId . kernelConstants <$> askEnv
  new_ids <- M.fromList <$> mapM (mkMap ltid) (S.toList (pcSegOpSizes pre))
  let f env =
        env
          { kernelConstants =
              (kernelConstants env)
                { kernelLocalIdMap = new_ids,
                  kernelChunkItersMap = pcChunkItersMap pre
                }
          }
  localEnv f m
  where
    mkMap ltid dims = do
      let dims' = map toInt64Exp dims
      ids' <- dIndexSpace' "ltid_pre" dims' (sExt64 ltid)
      return (dims, map sExt32 ids')

keyWithEntryPoint :: Maybe Name -> Name -> Name
keyWithEntryPoint fname key =
  nameFromString $ maybe "" ((++ ".") . nameToString) fname ++ nameToString key

allocLocal :: AllocCompiler GPUMem r Imp.KernelOp
allocLocal mem size =
  sOp $ Imp.LocalAlloc mem size

kernelAlloc ::
  Pat GPUMem ->
  SubExp ->
  Space ->
  InKernelGen ()
kernelAlloc (Pat [_]) _ ScalarSpace {} =
  -- Handled by the declaration of the memory block, which is then
  -- translated to an actual scalar variable during C code generation.
  return ()
kernelAlloc (Pat [mem]) size (Space "local") =
  allocLocal (patElemName mem) $ Imp.bytes $ toInt64Exp size
kernelAlloc (Pat [mem]) _ _ =
  compilerLimitationS $ "Cannot allocate memory block " ++ pretty mem ++ " in kernel."
kernelAlloc dest _ _ =
  error $ "Invalid target for in-kernel allocation: " ++ show dest

splitSpace ::
  (ToExp w, ToExp i, ToExp elems_per_thread) =>
  Pat GPUMem ->
  SplitOrdering ->
  w ->
  i ->
  elems_per_thread ->
  ImpM rep r op ()
splitSpace (Pat [size]) o w i elems_per_thread = do
  num_elements <- Imp.elements . TPrimExp <$> toExp w
  let i' = toInt64Exp i
  elems_per_thread' <- Imp.elements . TPrimExp <$> toExp elems_per_thread
  computeThreadChunkSize o i' elems_per_thread' num_elements (mkTV (patElemName size) int64)
splitSpace pat _ _ _ _ =
  error $ "Invalid target for splitSpace: " ++ pretty pat

updateAcc :: VName -> [SubExp] -> [SubExp] -> InKernelGen ()
updateAcc acc is vs = sComment "UpdateAcc" $ do
  -- See the ImpGen implementation of UpdateAcc for general notes.
  let is' = map toInt64Exp is
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
                error $ "Missing locks for " ++ pretty acc

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
-- 'threadOperations' will be in effect in the body.  For
-- multidimensional loops, use 'groupCoverSpace'.
kernelLoop ::
  IntExp t =>
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
  IntExp t =>
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
  IntExp t =>
  [Imp.TExp t] ->
  ([Imp.TExp t] -> InKernelGen ()) ->
  InKernelGen ()
groupCoverSpace ds f =
  groupLoop (product ds) $ f . unflattenIndex ds

localThreadIDs :: [SubExp] -> InKernelGen [Imp.TExp Int64]
localThreadIDs dims = do
  ltid <- sExt64 . kernelLocalThreadId . kernelConstants <$> askEnv
  let dims' = map toInt64Exp dims
  maybe (dIndexSpace' "ltid" dims' ltid) (pure . map sExt64)
    . M.lookup dims
    . kernelLocalIdMap
    . kernelConstants
    =<< askEnv

partitionSeqDims :: SegSeqDims -> SegSpace -> ([(VName, SubExp)], [(VName, SubExp)])
partitionSeqDims (SegSeqDims seq_is) space =
  bimap (map fst) (map fst) $
    partition ((`elem` seq_is) . snd) (zip (unSegSpace space) [0 ..])

groupCoverSegSpace :: SegVirt -> SegSpace -> InKernelGen () -> InKernelGen ()
groupCoverSegSpace virt space m = do
  let (ltids, dims) = unzip $ unSegSpace space
      dims' = map pe64 dims

  constants <- kernelConstants <$> askEnv
  let group_size = kernelGroupSize constants
  -- Maybe we can statically detect that this is actually a
  -- SegNoVirtFull and generate ever-so-slightly simpler code.
  let virt' = if dims' == [group_size] then SegNoVirtFull (SegSeqDims []) else virt
  case virt' of
    SegVirt -> do
      iters <- M.lookup dims . kernelChunkItersMap . kernelConstants <$> askEnv
      case iters of
        Nothing -> do
          iterations <- dPrimVE "iterations" $ product $ map sExt32 dims'
          groupLoop iterations $ \i -> do
            dIndexSpace (zip ltids dims') $ sExt64 i
            m
        Just num_chunks -> do
          let ltid = kernelLocalThreadId constants
          sFor "chunk_i" num_chunks $ \chunk_i -> do
            i <- dPrimVE "i" $ chunk_i * sExt32 group_size + ltid
            dIndexSpace (zip ltids dims') $ sExt64 i
            sWhen (inBounds (Slice (map (DimFix . le64) ltids)) dims') m
    SegNoVirt -> localOps threadOperations $ do
      zipWithM_ dPrimV_ ltids =<< localThreadIDs dims
      sWhen (isActive $ zip ltids dims) m
    SegNoVirtFull seq_dims -> do
      let ((ltids_seq, dims_seq), (ltids_par, dims_par)) =
            bimap unzip unzip $ partitionSeqDims seq_dims space
      sLoopNest (Shape dims_seq) $ \is_seq -> do
        zipWithM_ dPrimV_ ltids_seq is_seq
        localOps threadOperations $ do
          zipWithM_ dPrimV_ ltids_par =<< localThreadIDs dims_par
          m

compileGroupExp :: ExpCompiler GPUMem KernelEnv Imp.KernelOp
compileGroupExp (Pat [pe]) (BasicOp (Opaque _ se)) =
  -- Cannot print in GPU code.
  copyDWIM (patElemName pe) [] se []
-- The static arrays stuff does not work inside kernels.
compileGroupExp (Pat [dest]) (BasicOp (ArrayLit es _)) =
  forM_ (zip [0 ..] es) $ \(i, e) ->
    copyDWIMFix (patElemName dest) [fromIntegral (i :: Int64)] e []
compileGroupExp _ (BasicOp (UpdateAcc acc is vs)) =
  updateAcc acc is vs
compileGroupExp (Pat [dest]) (BasicOp (Replicate ds se)) = do
  flat <- newVName "rep_flat"
  is <- replicateM (shapeRank ds) (newVName "rep_i")
  let is' = map le64 is
  groupCoverSegSpace SegVirt (SegSpace flat $ zip is $ shapeDims ds) $
    copyDWIMFix (patElemName dest) is' se []
  sOp $ Imp.Barrier Imp.FenceLocal
compileGroupExp (Pat [dest]) (BasicOp (Iota n e s it)) = do
  n' <- toExp n
  e' <- toExp e
  s' <- toExp s
  groupLoop (TPrimExp n') $ \i' -> do
    x <-
      dPrimV "x" $
        TPrimExp $
          BinOpExp (Add it OverflowUndef) e' $
            BinOpExp (Mul it OverflowUndef) (untyped i') s'
    copyDWIMFix (patElemName dest) [i'] (Var (tvVar x)) []
  sOp $ Imp.Barrier Imp.FenceLocal

-- When generating code for a scalar in-place update, we must make
-- sure that only one thread performs the write.  When writing an
-- array, the group-level copy code will take care of doing the right
-- thing.
compileGroupExp (Pat [pe]) (BasicOp (Update safety _ slice se))
  | null $ sliceDims slice = do
    sOp $ Imp.Barrier Imp.FenceLocal
    ltid <- kernelLocalThreadId . kernelConstants <$> askEnv
    sWhen (ltid .==. 0) $
      case safety of
        Unsafe -> write
        Safe -> sWhen (inBounds slice' dims) write
    sOp $ Imp.Barrier Imp.FenceLocal
  where
    slice' = fmap toInt64Exp slice
    dims = map toInt64Exp $ arrayDims $ patElemType pe
    write = copyDWIM (patElemName pe) (unSlice slice') se []
compileGroupExp dest e =
  defCompileExp dest e

sanityCheckLevel :: SegLevel -> InKernelGen ()
sanityCheckLevel SegThread {} = return ()
sanityCheckLevel SegGroup {} =
  error "compileGroupOp: unexpected group-level SegOp."

compileFlatId :: SegLevel -> SegSpace -> InKernelGen ()
compileFlatId lvl space = do
  sanityCheckLevel lvl
  ltid <- kernelLocalThreadId . kernelConstants <$> askEnv
  dPrimV_ (segFlat space) ltid

-- Construct the necessary lock arrays for an intra-group histogram.
prepareIntraGroupSegHist ::
  Count GroupSize SubExp ->
  [HistOp GPUMem] ->
  InKernelGen [[Imp.TExp Int64] -> InKernelGen ()]
prepareIntraGroupSegHist group_size =
  fmap snd . mapAccumLM onOp Nothing
  where
    onOp l op = do
      constants <- kernelConstants <$> askEnv
      atomicBinOp <- kernelAtomics <$> askEnv

      let local_subhistos = histDest op

      case (l, atomicUpdateLocking atomicBinOp $ histOp op) of
        (_, AtomicPrim f) -> return (l, f (Space "local") local_subhistos)
        (_, AtomicCAS f) -> return (l, f (Space "local") local_subhistos)
        (Just l', AtomicLocking f) -> return (l, f l' (Space "local") local_subhistos)
        (Nothing, AtomicLocking f) -> do
          locks <- newVName "locks"

          let num_locks = toInt64Exp $ unCount group_size
              dims = map toInt64Exp $ shapeDims (histOpShape op <> histShape op)
              l' = Locking locks 0 1 0 (pure . (`rem` num_locks) . flattenIndex dims)
              locks_t = Array int32 (Shape [unCount group_size]) NoUniqueness

          locks_mem <- sAlloc "locks_mem" (typeSize locks_t) $ Space "local"
          dArray locks int32 (arrayShape locks_t) locks_mem $
            IxFun.iota $ map pe64 $ arrayDims locks_t

          sComment "All locks start out unlocked" $
            groupCoverSpace [kernelGroupSize constants] $ \is ->
              copyDWIMFix locks is (intConst Int32 0) []

          return (Just l', f l' (Space "local") local_subhistos)

-- Which fence do we need to protect shared access to this memory space?
fenceForSpace :: Space -> Imp.Fence
fenceForSpace (Space "local") = Imp.FenceLocal
fenceForSpace _ = Imp.FenceGlobal

-- If we are touching these arrays, which kind of fence do we need?
fenceForArrays :: [VName] -> InKernelGen Imp.Fence
fenceForArrays = fmap (foldl' max Imp.FenceLocal) . mapM need
  where
    need arr =
      fmap (fenceForSpace . entryMemSpace) . lookupMemory
        . memLocName
        . entryArrayLoc
        =<< lookupArray arr

groupChunkLoop ::
  Imp.TExp Int32 ->
  (Imp.TExp Int32 -> TV Int64 -> InKernelGen ()) ->
  InKernelGen ()
groupChunkLoop w m = do
  constants <- kernelConstants <$> askEnv
  let max_chunk_size = sExt32 $ kernelGroupSize constants
  num_chunks <- dPrimVE "num_chunks" $ w `divUp` max_chunk_size
  sFor "chunk_i" num_chunks $ \chunk_i -> do
    chunk_start <-
      dPrimVE "chunk_start" $ chunk_i * max_chunk_size
    chunk_end <-
      dPrimVE "chunk_end" $ sMin32 w (chunk_start + max_chunk_size)
    chunk_size <-
      dPrimV "chunk_size" $ sExt64 $ chunk_end - chunk_start
    m chunk_start chunk_size

sliceArray :: Imp.TExp Int64 -> TV Int64 -> VName -> ImpM rep r op VName
sliceArray start size arr = do
  MemLoc mem _ ixfun <- entryArrayLoc <$> lookupArray arr
  arr_t <- lookupType arr
  let slice =
        fullSliceNum
          (map Imp.pe64 (arrayDims arr_t))
          [DimSlice start (tvExp size) 1]
  sArray
    (baseString arr ++ "_chunk")
    (elemType arr_t)
    (arrayShape arr_t `setOuterDim` Var (tvVar size))
    mem
    $ IxFun.slice ixfun slice

-- | @flattenArray k flat arr@ flattens the outer @k@ dimensions of
-- @arr@ to @flat@.  (Make sure @flat@ is the sum of those dimensions
-- or you'll have a bad time.)
flattenArray :: Int -> TV Int64 -> VName -> ImpM rep r op VName
flattenArray k flat arr = do
  ArrayEntry arr_loc pt <- lookupArray arr
  let flat_shape = Shape $ Var (tvVar flat) : drop k (memLocShape arr_loc)
  sArray (baseString arr ++ "_flat") pt flat_shape (memLocName arr_loc) $
    IxFun.reshape (memLocIxFun arr_loc) $ map (DimNew . pe64) $ shapeDims flat_shape

-- | @applyLambda lam dests args@ emits code that:
--
-- 1. Binds each parameter of @lam@ to the corresponding element of
--    @args@, interpreted as a (name,slice) pair (as in 'copyDWIM').
--    Use an empty list for a scalar.
--
-- 2. Executes the body of @lam@.
--
-- 3. Binds the 'SubExp's that are the 'Result' of @lam@ to the
-- provided @dest@s, again interpreted as the destination for a
-- 'copyDWIM'.
applyLambda ::
  Mem rep inner =>
  Lambda rep ->
  [(VName, [DimIndex (Imp.TExp Int64)])] ->
  [(SubExp, [DimIndex (Imp.TExp Int64)])] ->
  ImpM rep r op ()
applyLambda lam dests args = do
  dLParams $ lambdaParams lam
  forM_ (zip (lambdaParams lam) args) $ \(p, (arg, arg_slice)) ->
    copyDWIM (paramName p) [] arg arg_slice
  compileStms mempty (bodyStms $ lambdaBody lam) $ do
    let res = map resSubExp $ bodyResult $ lambdaBody lam
    forM_ (zip dests res) $ \((dest, dest_slice), se) ->
      copyDWIM dest dest_slice se []

-- | As 'applyLambda', but first rename the names in the lambda.  This
-- makes it safe to apply it in multiple places.  (It might be safe
-- anyway, but you have to be more careful - use this if you are in
-- doubt.)
applyRenamedLambda ::
  Mem rep inner =>
  Lambda rep ->
  [(VName, [DimIndex (Imp.TExp Int64)])] ->
  [(SubExp, [DimIndex (Imp.TExp Int64)])] ->
  ImpM rep r op ()
applyRenamedLambda lam dests args = do
  lam_renamed <- renameLambda lam
  applyLambda lam_renamed dests args

virtualisedGroupScan ::
  Maybe (Imp.TExp Int32 -> Imp.TExp Int32 -> Imp.TExp Bool) ->
  Imp.TExp Int32 ->
  Lambda GPUMem ->
  [VName] ->
  InKernelGen ()
virtualisedGroupScan seg_flag w lam arrs = do
  groupChunkLoop w $ \chunk_start chunk_size -> do
    constants <- kernelConstants <$> askEnv
    let ltid = kernelLocalThreadId constants
        crosses_segment =
          case seg_flag of
            Nothing -> false
            Just flag_true ->
              flag_true (sExt32 (chunk_start -1)) (sExt32 chunk_start)
    sComment "possibly incorporate carry" $
      sWhen (chunk_start .>. 0 .&&. ltid .==. 0 .&&. bNot crosses_segment) $ do
        carry_idx <- dPrimVE "carry_idx" $ sExt64 chunk_start - 1
        applyRenamedLambda
          lam
          (zip arrs $ repeat [DimFix $ sExt64 chunk_start])
          ( zip (map Var arrs) (repeat [DimFix carry_idx])
              ++ zip (map Var arrs) (repeat [DimFix $ sExt64 chunk_start])
          )

    arrs_chunks <- mapM (sliceArray (sExt64 chunk_start) chunk_size) arrs

    sOp $ Imp.ErrorSync Imp.FenceLocal

    groupScan
      seg_flag
      (sExt64 w)
      (tvExp chunk_size)
      lam
      arrs_chunks

compileGroupOp :: OpCompiler GPUMem KernelEnv Imp.KernelOp
compileGroupOp pat (Alloc size space) =
  kernelAlloc pat size space
compileGroupOp pat (Inner (SizeOp (SplitSpace o w i elems_per_thread))) =
  splitSpace pat o w i elems_per_thread
compileGroupOp pat (Inner (SegOp (SegMap lvl space _ body))) = do
  compileFlatId lvl space

  groupCoverSegSpace (segVirt lvl) space $
    compileStms mempty (kernelBodyStms body) $
      zipWithM_ (compileThreadResult space) (patElems pat) $
        kernelBodyResult body
  sOp $ Imp.ErrorSync Imp.FenceLocal
compileGroupOp pat (Inner (SegOp (SegScan lvl space scans _ body))) = do
  compileFlatId lvl space

  let (ltids, dims) = unzip $ unSegSpace space
      dims' = map toInt64Exp dims

  groupCoverSegSpace (segVirt lvl) space $
    compileStms mempty (kernelBodyStms body) $
      forM_ (zip (patNames pat) $ kernelBodyResult body) $ \(dest, res) ->
        copyDWIMFix
          dest
          (map Imp.le64 ltids)
          (kernelResultSubExp res)
          []

  fence <- fenceForArrays $ patNames pat
  sOp $ Imp.ErrorSync fence

  let segment_size = last dims'
      crossesSegment from to =
        (sExt64 to - sExt64 from) .>. (sExt64 to `rem` segment_size)

  -- groupScan needs to treat the scan output as a one-dimensional
  -- array of scan elements, so we invent some new flattened arrays
  -- here.
  dims_flat <- dPrimV "dims_flat" $ product dims'
  let scan = head scans
      num_scan_results = length $ segBinOpNeutral scan
  arrs_flat <-
    mapM (flattenArray (length dims') dims_flat) $
      take num_scan_results $ patNames pat

  case segVirt lvl of
    SegVirt ->
      virtualisedGroupScan
        (Just crossesSegment)
        (sExt32 $ tvExp dims_flat)
        (segBinOpLambda scan)
        arrs_flat
    _ ->
      groupScan
        (Just crossesSegment)
        (product dims')
        (product dims')
        (segBinOpLambda scan)
        arrs_flat
compileGroupOp pat (Inner (SegOp (SegRed lvl space ops _ body))) = do
  compileFlatId lvl space

  let dims' = map toInt64Exp dims
      mkTempArr t =
        sAllocArray "red_arr" (elemType t) (Shape dims <> arrayShape t) $ Space "local"

  tmp_arrs <- mapM mkTempArr $ concatMap (lambdaReturnType . segBinOpLambda) ops
  groupCoverSegSpace (segVirt lvl) space $
    compileStms mempty (kernelBodyStms body) $ do
      let (red_res, map_res) =
            splitAt (segBinOpResults ops) $ kernelBodyResult body
      forM_ (zip tmp_arrs red_res) $ \(dest, res) ->
        copyDWIMFix dest (map Imp.le64 ltids) (kernelResultSubExp res) []
      zipWithM_ (compileThreadResult space) map_pes map_res

  sOp $ Imp.ErrorSync Imp.FenceLocal

  let tmps_for_ops = chunks (map (length . segBinOpNeutral) ops) tmp_arrs
  case segVirt lvl of
    SegVirt -> virtCase dims' tmps_for_ops
    _ -> nonvirtCase dims' tmps_for_ops
  where
    (ltids, dims) = unzip $ unSegSpace space
    (red_pes, map_pes) = splitAt (segBinOpResults ops) $ patElems pat

    virtCase [dim'] tmps_for_ops = do
      ltid <- kernelLocalThreadId . kernelConstants <$> askEnv
      groupChunkLoop (sExt32 dim') $ \chunk_start chunk_size -> do
        sComment "possibly incorporate carry" $
          sWhen (chunk_start .>. 0 .&&. ltid .==. 0) $
            forM_ (zip ops tmps_for_ops) $ \(op, tmps) ->
              applyRenamedLambda
                (segBinOpLambda op)
                (zip tmps $ repeat [DimFix $ sExt64 chunk_start])
                ( zip (map (Var . patElemName) red_pes) (repeat [])
                    ++ zip (map Var tmps) (repeat [DimFix $ sExt64 chunk_start])
                )

        sOp $ Imp.ErrorSync Imp.FenceLocal

        forM_ (zip ops tmps_for_ops) $ \(op, tmps) -> do
          tmps_chunks <- mapM (sliceArray (sExt64 chunk_start) chunk_size) tmps
          groupReduce (sExt32 (tvExp chunk_size)) (segBinOpLambda op) tmps_chunks

        sOp $ Imp.ErrorSync Imp.FenceLocal

        forM_ (zip red_pes $ concat tmps_for_ops) $ \(pe, arr) ->
          copyDWIMFix (patElemName pe) [] (Var arr) [sExt64 chunk_start]
    virtCase dims' tmps_for_ops = do
      dims_flat <- dPrimV "dims_flat" $ product dims'
      let segment_size = last dims'
          crossesSegment from to =
            (sExt64 to - sExt64 from) .>. (sExt64 to `rem` sExt64 segment_size)

      forM_ (zip ops tmps_for_ops) $ \(op, tmps) -> do
        tmps_flat <- mapM (flattenArray (length dims') dims_flat) tmps
        virtualisedGroupScan
          (Just crossesSegment)
          (sExt32 $ tvExp dims_flat)
          (segBinOpLambda op)
          tmps_flat

      sOp $ Imp.ErrorSync Imp.FenceLocal

      forM_ (zip red_pes $ concat tmps_for_ops) $ \(pe, arr) ->
        copyDWIM
          (patElemName pe)
          []
          (Var arr)
          (map (unitSlice 0) (init dims') ++ [DimFix $ last dims' -1])

      sOp $ Imp.Barrier Imp.FenceLocal

    nonvirtCase [dim'] tmps_for_ops = do
      -- Nonsegmented case (or rather, a single segment) - this we can
      -- handle directly with a group-level reduction.
      forM_ (zip ops tmps_for_ops) $ \(op, tmps) ->
        groupReduce (sExt32 dim') (segBinOpLambda op) tmps
      sOp $ Imp.ErrorSync Imp.FenceLocal
      forM_ (zip red_pes $ concat tmps_for_ops) $ \(pe, arr) ->
        copyDWIMFix (patElemName pe) [] (Var arr) [0]
    --
    nonvirtCase dims' tmps_for_ops = do
      -- Segmented intra-group reductions are turned into (regular)
      -- segmented scans.  It is possible that this can be done
      -- better, but at least this approach is simple.

      -- groupScan operates on flattened arrays.  This does not
      -- involve copying anything; merely playing with the index
      -- function.
      dims_flat <- dPrimV "dims_flat" $ product dims'
      let segment_size = last dims'
          crossesSegment from to =
            (sExt64 to - sExt64 from) .>. (sExt64 to `rem` sExt64 segment_size)

      forM_ (zip ops tmps_for_ops) $ \(op, tmps) -> do
        tmps_flat <- mapM (flattenArray (length dims') dims_flat) tmps
        groupScan
          (Just crossesSegment)
          (product dims')
          (product dims')
          (segBinOpLambda op)
          tmps_flat

      sOp $ Imp.ErrorSync Imp.FenceLocal

      forM_ (zip red_pes $ concat tmps_for_ops) $ \(pe, arr) ->
        copyDWIM
          (patElemName pe)
          []
          (Var arr)
          (map (unitSlice 0) (init dims') ++ [DimFix $ last dims' -1])

      sOp $ Imp.Barrier Imp.FenceLocal
compileGroupOp pat (Inner (SegOp (SegHist lvl space ops _ kbody))) = do
  compileFlatId lvl space
  let (ltids, _dims) = unzip $ unSegSpace space

  -- We don't need the red_pes, because it is guaranteed by our type
  -- rules that they occupy the same memory as the destinations for
  -- the ops.
  let num_red_res = length ops + sum (map (length . histNeutral) ops)
      (_red_pes, map_pes) =
        splitAt num_red_res $ patElems pat

  ops' <- prepareIntraGroupSegHist (segGroupSize lvl) ops

  -- Ensure that all locks have been initialised.
  sOp $ Imp.Barrier Imp.FenceLocal

  groupCoverSegSpace (segVirt lvl) space $
    compileStms mempty (kernelBodyStms kbody) $ do
      let (red_res, map_res) = splitAt num_red_res $ kernelBodyResult kbody
          (red_is, red_vs) = splitAt (length ops) $ map kernelResultSubExp red_res
      zipWithM_ (compileThreadResult space) map_pes map_res

      let vs_per_op = chunks (map (length . histDest) ops) red_vs

      forM_ (zip4 red_is vs_per_op ops' ops) $
        \(bin, op_vs, do_op, HistOp dest_shape _ _ _ shape lam) -> do
          let bin' = toInt64Exp bin
              dest_shape' = map toInt64Exp $ shapeDims dest_shape
              bin_in_bounds = inBounds (Slice (map DimFix [bin'])) dest_shape'
              bin_is = map Imp.le64 (init ltids) ++ [bin']
              vs_params = takeLast (length op_vs) $ lambdaParams lam

          sComment "perform atomic updates" $
            sWhen bin_in_bounds $ do
              dLParams $ lambdaParams lam
              sLoopNest shape $ \is -> do
                forM_ (zip vs_params op_vs) $ \(p, v) ->
                  copyDWIMFix (paramName p) [] v is
                do_op (bin_is ++ is)

  sOp $ Imp.ErrorSync Imp.FenceLocal
compileGroupOp pat _ =
  compilerBugS $ "compileGroupOp: cannot compile rhs of binding " ++ pretty pat

compileThreadOp :: OpCompiler GPUMem KernelEnv Imp.KernelOp
compileThreadOp pat (Alloc size space) =
  kernelAlloc pat size space
compileThreadOp pat (Inner (SizeOp (SplitSpace o w i elems_per_thread))) =
  splitSpace pat o w i elems_per_thread
compileThreadOp pat _ =
  compilerBugS $ "compileThreadOp: cannot compile rhs of binding " ++ pretty pat

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
      compileBody' [xp] $ lambdaBody op
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
          compileBody' acc_params $ lambdaBody op

      do_hist =
        everythingVolatile $
          sComment "update global result" $
            zipWithM_ (writeArray bucket) arrs $ map (Var . paramName) acc_params

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
  FreeIn a =>
  a ->
  [VName] ->
  CallKernelGen [Imp.KernelUse]
computeKernelUses kernel_body bound_in_kernel = do
  let actually_free = freeIn kernel_body `namesSubtract` namesFromList bound_in_kernel
  -- Compute the variables that we need to pass to the kernel.
  nubOrd <$> readsFromSet actually_free

readsFromSet :: Names -> CallKernelGen [Imp.KernelUse]
readsFromSet free =
  fmap catMaybes $
    forM (namesToList free) $ \var -> do
      t <- lookupType var
      vtable <- getVTable
      case t of
        Array {} -> return Nothing
        Acc {} -> return Nothing
        Mem (Space "local") -> return Nothing
        Mem {} -> return $ Just $ Imp.MemoryUse var
        Prim bt ->
          isConstExp vtable (Imp.var var bt) >>= \case
            Just ce -> return $ Just $ Imp.ConstUse var ce
            Nothing -> return $ Just $ Imp.ScalarUse var bt

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
      constExp e = primExpFromExp lookupConstExp e
  return $ replaceInPrimExpM onLeaf size
  where
    hasExp (ArrayVar e _) = e
    hasExp (AccVar e _) = e
    hasExp (ScalarVar e _) = e
    hasExp (MemVar e _) = e

computeThreadChunkSize ::
  SplitOrdering ->
  Imp.TExp Int64 ->
  Imp.Count Imp.Elements (Imp.TExp Int64) ->
  Imp.Count Imp.Elements (Imp.TExp Int64) ->
  TV Int64 ->
  ImpM rep r op ()
computeThreadChunkSize (SplitStrided stride) thread_index elements_per_thread num_elements chunk_var =
  chunk_var
    <-- sMin64
      (Imp.unCount elements_per_thread)
      ((Imp.unCount num_elements - thread_index) `divUp` toInt64Exp stride)
computeThreadChunkSize SplitContiguous thread_index elements_per_thread num_elements chunk_var = do
  starting_point <-
    dPrimV "starting_point" $
      thread_index * Imp.unCount elements_per_thread
  remaining_elements <-
    dPrimV "remaining_elements" $
      Imp.unCount num_elements - tvExp starting_point

  let no_remaining_elements = tvExp remaining_elements .<=. 0
      beyond_bounds = Imp.unCount num_elements .<=. tvExp starting_point

  sIf
    (no_remaining_elements .||. beyond_bounds)
    (chunk_var <-- 0)
    ( sIf
        is_last_thread
        (chunk_var <-- Imp.unCount last_thread_elements)
        (chunk_var <-- Imp.unCount elements_per_thread)
    )
  where
    last_thread_elements =
      num_elements - Imp.elements thread_index * elements_per_thread
    is_last_thread =
      Imp.unCount num_elements
        .<. (thread_index + 1) * Imp.unCount elements_per_thread

kernelInitialisationSimple ::
  Count NumGroups (Imp.TExp Int64) ->
  Count GroupSize (Imp.TExp Int64) ->
  CallKernelGen (KernelConstants, InKernelGen ())
kernelInitialisationSimple (Count num_groups) (Count group_size) = do
  global_tid <- newVName "global_tid"
  local_tid <- newVName "local_tid"
  group_id <- newVName "group_tid"
  wave_size <- newVName "wave_size"
  inner_group_size <- newVName "group_size"
  let constants =
        KernelConstants
          (Imp.le32 global_tid)
          (Imp.le32 local_tid)
          (Imp.le32 group_id)
          global_tid
          local_tid
          group_id
          num_groups
          group_size
          (sExt32 (group_size * num_groups))
          (Imp.le32 wave_size)
          true
          mempty
          mempty

  let set_constants = do
        dPrim_ global_tid int32
        dPrim_ local_tid int32
        dPrim_ inner_group_size int64
        dPrim_ wave_size int32
        dPrim_ group_id int32

        sOp (Imp.GetGlobalId global_tid 0)
        sOp (Imp.GetLocalId local_tid 0)
        sOp (Imp.GetLocalSize inner_group_size 0)
        sOp (Imp.GetLockstepWidth wave_size)
        sOp (Imp.GetGroupId group_id 0)

  return (constants, set_constants)

isActive :: [(VName, SubExp)] -> Imp.TExp Bool
isActive limit = case actives of
  [] -> true
  x : xs -> foldl (.&&.) x xs
  where
    (is, ws) = unzip limit
    actives = zipWith active is $ map toInt64Exp ws
    active i = (Imp.le64 i .<.)

-- | Change every memory block to be in the global address space,
-- except those who are in the local memory space.  This only affects
-- generated code - we still need to make sure that the memory is
-- actually present on the device (and dared as variables in the
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
          return ()

  let (reduce_acc_params, reduce_arr_params) = splitAt (length arrs) $ lambdaParams lam

  skip_waves <- dPrimV "skip_waves" (1 :: Imp.TExp Int32)
  dLParams $ lambdaParams lam

  offset <-- (0 :: Imp.TExp Int32)

  comment "participating threads read initial accumulator" $
    sWhen (local_tid .<. w) $
      zipWithM_ readReduceArgument reduce_acc_params arrs

  let do_reduce = do
        comment "read array element" $
          zipWithM_ readReduceArgument reduce_arr_params arrs
        comment "apply reduction operation" $
          compileBody' reduce_acc_params $ lambdaBody lam
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
          sWhen
            apply_in_cross_wave_iteration
            do_reduce
          skip_waves <-- tvExp skip_waves * 2

  in_wave_reductions
  cross_wave_reductions

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
          flag_true (from * block_size + block_size -1) (to * block_size + block_size -1)
  comment
    "scan the first block, after which offset 'i' contains carry-in for block 'i+1'"
    $ doInBlockScan first_block_seg_flag (is_first_block .&&. ltid_in_bounds) renamed_lam

  barrier

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
            dPrimVE "inactive" $ flag_true (block_id * block_size -1) ltid32
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

  sComment "carry-in for every block except the first" $ do
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
          sWhen in_block_thread_active $
            compileBody' x_params $ lambdaBody scan_lam
        | Just flag_true <- seg_flag = do
          inactive <-
            dPrimVE "inactive" $ flag_true (ltid32 - tvExp skip_threads) ltid32
          sWhen (in_block_thread_active .&&. inactive) $
            forM_ (zip x_params y_params) $ \(x, y) ->
              copyDWIM (paramName x) [] (Var (paramName y)) []
          -- The convoluted control flow is to ensure all threads
          -- hit this barrier (if applicable).
          when array_scan barrier
          sWhen in_block_thread_active . sUnless inactive $
            compileBody' x_params $ lambdaBody scan_lam

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
        sequence_ $ zipWith3 writeResult x_params y_params arrs

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

computeMapKernelGroups :: Imp.TExp Int64 -> CallKernelGen (Imp.TExp Int64, Imp.TExp Int64)
computeMapKernelGroups kernel_size = do
  group_size <- dPrim "group_size" int64
  fname <- askFunction
  let group_size_key = keyWithEntryPoint fname $ nameFromString $ pretty $ tvVar group_size
  sOp $ Imp.GetSize (tvVar group_size) group_size_key Imp.SizeGroup
  num_groups <- dPrimV "num_groups" $ kernel_size `divUp` tvExp group_size
  return (tvExp num_groups, tvExp group_size)

simpleKernelConstants ::
  Imp.TExp Int64 ->
  String ->
  CallKernelGen (KernelConstants, InKernelGen ())
simpleKernelConstants kernel_size desc = do
  thread_gtid <- newVName $ desc ++ "_gtid"
  thread_ltid <- newVName $ desc ++ "_ltid"
  group_id <- newVName $ desc ++ "_gid"
  (num_groups, group_size) <- computeMapKernelGroups kernel_size
  let set_constants = do
        dPrim_ thread_gtid int32
        dPrim_ thread_ltid int32
        dPrim_ group_id int32
        sOp (Imp.GetGlobalId thread_gtid 0)
        sOp (Imp.GetLocalId thread_ltid 0)
        sOp (Imp.GetGroupId group_id 0)

  return
    ( KernelConstants
        (Imp.le32 thread_gtid)
        (Imp.le32 thread_ltid)
        (Imp.le32 group_id)
        thread_gtid
        thread_ltid
        group_id
        num_groups
        group_size
        (sExt32 (group_size * num_groups))
        0
        (Imp.le64 thread_gtid .<. kernel_size)
        mempty
        mempty,
      set_constants
    )

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
  let iterations =
        (required_groups - tvExp phys_group_id)
          `divUp` sExt32 (kernelNumGroups constants)

  sFor "i" iterations $ \i -> do
    m . tvExp
      =<< dPrimV
        "virt_group_id"
        (tvExp phys_group_id + i * sExt32 (kernelNumGroups constants))
    -- Make sure the virtual group is actually done before we let
    -- another virtual group have its way with it.
    sOp $ Imp.Barrier Imp.FenceGlobal
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
    kAttrNumGroups :: Count NumGroups (Imp.TExp Int64),
    -- | Group size.
    kAttrGroupSize :: Count GroupSize (Imp.TExp Int64)
  }

-- | The default kernel attributes.
defKernelAttrs ::
  Count NumGroups (Imp.TExp Int64) ->
  Count GroupSize (Imp.TExp Int64) ->
  KernelAttrs
defKernelAttrs num_groups group_size =
  KernelAttrs
    { kAttrFailureTolerant = False,
      kAttrCheckLocalMemory = True,
      kAttrNumGroups = num_groups,
      kAttrGroupSize = group_size
    }

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

sKernelGroup ::
  String ->
  VName ->
  KernelAttrs ->
  InKernelGen () ->
  CallKernelGen ()
sKernelGroup = sKernel groupOperations kernelGroupId

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
  emit . Imp.Op . Imp.CallKernel $
    Imp.Kernel
      { Imp.kernelBody = body,
        Imp.kernelUses = uses,
        Imp.kernelNumGroups = [untyped $ kernelNumGroups constants],
        Imp.kernelGroupSize = [untyped $ kernelGroupSize constants],
        Imp.kernelName = name,
        Imp.kernelFailureTolerant = kAttrFailureTolerant attrs,
        Imp.kernelCheckLocalMemory = kAttrCheckLocalMemory attrs
      }

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
          (Count (kernelNumGroups constants))
          (Count (kernelGroupSize constants))
      )
        { kAttrFailureTolerant = tol
        }

copyInGroup :: CopyCompiler GPUMem KernelEnv Imp.KernelOp
copyInGroup pt destloc srcloc = do
  dest_space <- entryMemSpace <$> lookupMemory (memLocName destloc)
  src_space <- entryMemSpace <$> lookupMemory (memLocName srcloc)

  let src_ixfun = memLocIxFun srcloc
      dims = IxFun.shape src_ixfun
      rank = length dims

  case (dest_space, src_space) of
    (ScalarSpace destds _, ScalarSpace srcds _) -> do
      let fullDim d = DimSlice 0 d 1
          destslice' =
            Slice $
              replicate (rank - length destds) (DimFix 0)
                ++ takeLast (length destds) (map fullDim dims)
          srcslice' =
            Slice $
              replicate (rank - length srcds) (DimFix 0)
                ++ takeLast (length srcds) (map fullDim dims)
      copyElementWise
        pt
        (sliceMemLoc destloc destslice')
        (sliceMemLoc srcloc srcslice')
    _ -> do
      groupCoverSpace (map sExt32 dims) $ \is ->
        copyElementWise
          pt
          (sliceMemLoc destloc (Slice $ map (DimFix . sExt64) is))
          (sliceMemLoc srcloc (Slice $ map (DimFix . sExt64) is))
      sOp $ Imp.Barrier Imp.FenceLocal

threadOperations, groupOperations :: Operations GPUMem KernelEnv Imp.KernelOp
threadOperations =
  (defaultOperations compileThreadOp)
    { opsCopyCompiler = copyElementWise,
      opsExpCompiler = compileThreadExp,
      opsStmsCompiler = \_ -> defCompileStms mempty,
      opsAllocCompilers =
        M.fromList [(Space "local", allocLocal)]
    }
groupOperations =
  (defaultOperations compileGroupOp)
    { opsCopyCompiler = copyInGroup,
      opsExpCompiler = compileGroupExp,
      opsStmsCompiler = \_ -> defCompileStms mempty,
      opsAllocCompilers =
        M.fromList [(Space "local", allocLocal)]
    }

-- | Perform a Replicate with a kernel.
sReplicateKernel :: VName -> SubExp -> CallKernelGen ()
sReplicateKernel arr se = do
  t <- subExpType se
  ds <- dropLast (arrayRank t) . arrayDims <$> lookupType arr

  let dims = map toInt64Exp $ ds ++ arrayDims t
  (constants, set_constants) <-
    simpleKernelConstants (product $ map sExt64 dims) "replicate"

  fname <- askFunction
  let name =
        keyWithEntryPoint fname $
          nameFromString $
            "replicate_" ++ show (baseTag $ kernelGlobalThreadIdVar constants)

  sKernelFailureTolerant True threadOperations constants name $ do
    set_constants
    is' <- dIndexSpace' "rep_i" dims $ sExt64 $ kernelGlobalThreadId constants
    sWhen (kernelThreadActive constants) $
      copyDWIMFix arr is' se $ drop (length ds) is'

replicateName :: PrimType -> String
replicateName bt = "replicate_" ++ pretty bt

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
            Imp.ScalarParam num_elems int32,
            Imp.ScalarParam val bt
          ]
        shape = Shape [Var num_elems]
    function fname [] params $ do
      arr <-
        sArray "arr" bt shape mem $
          IxFun.iota $ map pe64 $ shapeDims shape
      sReplicateKernel arr $ Var val

  return fname

replicateIsFill :: VName -> SubExp -> CallKernelGen (Maybe (CallKernelGen ()))
replicateIsFill arr v = do
  ArrayEntry (MemLoc arr_mem arr_shape arr_ixfun) _ <- lookupArray arr
  v_t <- subExpType v
  case v_t of
    Prim v_t'
      | IxFun.isLinear arr_ixfun -> return $
        Just $ do
          fname <- replicateForType v_t'
          emit $
            Imp.Call
              []
              fname
              [ Imp.MemArg arr_mem,
                Imp.ExpArg $ untyped $ product $ map toInt64Exp arr_shape,
                Imp.ExpArg $ toExp' v_t' v
              ]
    _ -> return Nothing

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
  (constants, set_constants) <- simpleKernelConstants n "iota"

  fname <- askFunction
  let name =
        keyWithEntryPoint fname $
          nameFromString $
            "iota_" ++ pretty et ++ "_"
              ++ show (baseTag $ kernelGlobalThreadIdVar constants)

  sKernelFailureTolerant True threadOperations constants name $ do
    set_constants
    let gtid = sExt64 $ kernelGlobalThreadId constants
    sWhen (kernelThreadActive constants) $ do
      (destmem, destspace, destidx) <- fullyIndexArray' destloc [gtid]

      emit $
        Imp.Write destmem destidx (IntType et) destspace Imp.Nonvolatile $
          BinOpExp
            (Add et OverflowWrap)
            (BinOpExp (Mul et OverflowWrap) (Imp.sExt et $ untyped gtid) s)
            x

iotaName :: IntType -> String
iotaName bt = "iota_" ++ pretty bt

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
            Imp.ScalarParam n int32,
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
          IxFun.iota $ map pe64 $ shapeDims shape
      sIotaKernel arr (sExt64 n') x' s' bt

  return fname

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
  if IxFun.isLinear arr_ixfun
    then do
      fname <- iotaForType et
      emit $
        Imp.Call
          []
          fname
          [Imp.MemArg arr_mem, Imp.ExpArg $ untyped n, Imp.ExpArg x, Imp.ExpArg s]
    else sIotaKernel arr n x s et

sCopy :: CopyCompiler GPUMem HostEnv Imp.HostOp
sCopy pt destloc@(MemLoc destmem _ _) srcloc@(MemLoc srcmem srcdims _) = do
  -- Note that the shape of the destination and the source are
  -- necessarily the same.
  let shape = map toInt64Exp srcdims
      kernel_size = product shape

  (constants, set_constants) <- simpleKernelConstants kernel_size "copy"

  fname <- askFunction
  let name =
        keyWithEntryPoint fname $
          nameFromString $
            "copy_" ++ show (baseTag $ kernelGlobalThreadIdVar constants)

  sKernelFailureTolerant True threadOperations constants name $ do
    set_constants

    let gtid = sExt64 $ kernelGlobalThreadId constants
    is <- dIndexSpace' "copy_i" shape gtid

    (_, destspace, destidx) <- fullyIndexArray' destloc is
    (_, srcspace, srcidx) <- fullyIndexArray' srcloc is

    sWhen (gtid .<. kernel_size) $ do
      tmp <- tvVar <$> dPrim "tmp" pt
      emit $ Imp.Read tmp srcmem srcidx pt srcspace Imp.Nonvolatile
      emit $ Imp.Write destmem destidx pt destspace Imp.Nonvolatile $ Imp.var tmp pt

compileGroupResult ::
  SegSpace ->
  PatElem GPUMem ->
  KernelResult ->
  InKernelGen ()
compileGroupResult _ pe (TileReturns _ [(w, per_group_elems)] what) = do
  n <- toInt64Exp . arraySize 0 <$> lookupType what

  constants <- kernelConstants <$> askEnv
  let ltid = sExt64 $ kernelLocalThreadId constants
      offset =
        toInt64Exp per_group_elems
          * sExt64 (kernelGroupId constants)

  -- Avoid loop for the common case where each thread is statically
  -- known to write at most one element.
  localOps threadOperations $
    if toInt64Exp per_group_elems == kernelGroupSize constants
      then
        sWhen (ltid + offset .<. toInt64Exp w) $
          copyDWIMFix (patElemName pe) [ltid + offset] (Var what) [ltid]
      else sFor "i" (n `divUp` kernelGroupSize constants) $ \i -> do
        j <- dPrimVE "j" $ kernelGroupSize constants * i + ltid
        sWhen (j + offset .<. toInt64Exp w) $
          copyDWIMFix (patElemName pe) [j + offset] (Var what) [j]
compileGroupResult space pe (TileReturns _ dims what) = do
  let gids = map fst $ unSegSpace space
      out_tile_sizes = map (toInt64Exp . snd) dims
      group_is = zipWith (*) (map Imp.le64 gids) out_tile_sizes
  local_is <- localThreadIDs $ map snd dims
  is_for_thread <-
    mapM (dPrimV "thread_out_index") $
      zipWith (+) group_is local_is

  localOps threadOperations $
    sWhen (isActive $ zip (map tvVar is_for_thread) $ map fst dims) $
      copyDWIMFix (patElemName pe) (map tvExp is_for_thread) (Var what) local_is
compileGroupResult space pe (RegTileReturns _ dims_n_tiles what) = do
  constants <- kernelConstants <$> askEnv

  let gids = map fst $ unSegSpace space
      (dims, group_tiles, reg_tiles) = unzip3 dims_n_tiles
      group_tiles' = map toInt64Exp group_tiles
      reg_tiles' = map toInt64Exp reg_tiles

  -- Which group tile is this group responsible for?
  let group_tile_is = map Imp.le64 gids

  -- Within the group tile, which register tile is this thread
  -- responsible for?
  reg_tile_is <-
    dIndexSpace' "reg_tile_i" group_tiles' $ sExt64 $ kernelLocalThreadId constants

  -- Compute output array slice for the register tile belonging to
  -- this thread.
  let regTileSliceDim (group_tile, group_tile_i) (reg_tile, reg_tile_i) = do
        tile_dim_start <-
          dPrimVE "tile_dim_start" $
            reg_tile * (group_tile * group_tile_i + reg_tile_i)
        return $ DimSlice tile_dim_start reg_tile 1
  reg_tile_slices <-
    Slice
      <$> zipWithM
        regTileSliceDim
        (zip group_tiles' group_tile_is)
        (zip reg_tiles' reg_tile_is)

  localOps threadOperations $
    sLoopNest (Shape reg_tiles) $ \is_in_reg_tile -> do
      let dest_is = fixSlice reg_tile_slices is_in_reg_tile
          src_is = reg_tile_is ++ is_in_reg_tile
      sWhen (foldl1 (.&&.) $ zipWith (.<.) dest_is $ map toInt64Exp dims) $
        copyDWIMFix (patElemName pe) dest_is (Var what) src_is
compileGroupResult space pe (Returns _ _ what) = do
  constants <- kernelConstants <$> askEnv
  in_local_memory <- arrayInLocalMemory what
  let gids = map (Imp.le64 . fst) $ unSegSpace space

  if not in_local_memory
    then
      localOps threadOperations $
        sWhen (kernelLocalThreadId constants .==. 0) $
          copyDWIMFix (patElemName pe) gids what []
    else -- If the result of the group is an array in local memory, we
    -- store it by collective copying among all the threads of the
    -- group.  TODO: also do this if the array is in global memory
    -- (but this is a bit more tricky, synchronisation-wise).
      copyDWIMFix (patElemName pe) gids what []
compileGroupResult _ _ WriteReturns {} =
  compilerLimitationS "compileGroupResult: WriteReturns not handled yet."
compileGroupResult _ _ ConcatReturns {} =
  compilerLimitationS "compileGroupResult: ConcatReturns not handled yet."

compileThreadResult ::
  SegSpace ->
  PatElem GPUMem ->
  KernelResult ->
  InKernelGen ()
compileThreadResult _ _ RegTileReturns {} =
  compilerLimitationS "compileThreadResult: RegTileReturns not yet handled."
compileThreadResult space pe (Returns _ _ what) = do
  let is = map (Imp.le64 . fst) $ unSegSpace space
  copyDWIMFix (patElemName pe) is what []
compileThreadResult _ pe (ConcatReturns _ SplitContiguous _ per_thread_elems what) = do
  constants <- kernelConstants <$> askEnv
  let offset =
        toInt64Exp per_thread_elems
          * sExt64 (kernelGlobalThreadId constants)
  n <- toInt64Exp . arraySize 0 <$> lookupType what
  copyDWIM (patElemName pe) [DimSlice offset n 1] (Var what) []
compileThreadResult _ pe (ConcatReturns _ (SplitStrided stride) _ _ what) = do
  offset <- sExt64 . kernelGlobalThreadId . kernelConstants <$> askEnv
  n <- toInt64Exp . arraySize 0 <$> lookupType what
  copyDWIM (patElemName pe) [DimSlice offset n $ toInt64Exp stride] (Var what) []
compileThreadResult _ pe (WriteReturns _ (Shape rws) _arr dests) = do
  constants <- kernelConstants <$> askEnv
  let rws' = map toInt64Exp rws
  forM_ dests $ \(slice, e) -> do
    let slice' = fmap toInt64Exp slice
        write = kernelThreadActive constants .&&. inBounds slice' rws'
    sWhen write $ copyDWIM (patElemName pe) (unSlice slice') e []
compileThreadResult _ _ TileReturns {} =
  compilerBugS "compileThreadResult: TileReturns unhandled."

arrayInLocalMemory :: SubExp -> InKernelGen Bool
arrayInLocalMemory (Var name) = do
  res <- lookupVar name
  case res of
    ArrayVar _ entry ->
      (Space "local" ==) . entryMemSpace
        <$> lookupMemory (memLocName (entryArrayLoc entry))
    _ -> return False
arrayInLocalMemory Constant {} = return False
