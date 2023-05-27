{-# LANGUAGE TypeFamilies #-}

-- | Generation of kernels with group-level bodies.
module Futhark.CodeGen.ImpGen.GPU.Group
  ( sKernelGroup,
    compileGroupResult,
    groupOperations,

    -- * Precomputation
    Precomputed,
    precomputeConstants,
    precomputedConstants,
    atomicUpdateLocking,
  )
where

import Control.Monad.Except
import Data.Bifunctor
import Data.List (partition, zip4)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.CodeGen.ImpCode.GPU qualified as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.GPU.Base
import Futhark.Construct (fullSliceNum)
import Futhark.Error
import Futhark.IR.GPUMem
import Futhark.IR.Mem.IxFun qualified as IxFun
import Futhark.MonadFreshNames
import Futhark.Transform.Rename
import Futhark.Util (chunks, mapAccumLM, takeLast)
import Futhark.Util.IntegralExp (divUp, rem)
import Prelude hiding (quot, rem)

-- | @flattenArray k flat arr@ flattens the outer @k@ dimensions of
-- @arr@ to @flat@.  (Make sure @flat@ is the sum of those dimensions
-- or you'll have a bad time.)
flattenArray :: Int -> TV Int64 -> VName -> ImpM rep r op VName
flattenArray k flat arr = do
  ArrayEntry arr_loc pt <- lookupArray arr
  let flat_shape = Shape $ Var (tvVar flat) : drop k (memLocShape arr_loc)
  sArray (baseString arr ++ "_flat") pt flat_shape (memLocName arr_loc) $
    IxFun.reshape (memLocIxFun arr_loc) $
      map pe64 $
        shapeDims flat_shape

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

-- | @applyLambda lam dests args@ emits code that:
--
-- 1. Binds each parameter of @lam@ to the corresponding element of
--    @args@, interpreted as a (name,slice) pair (as in 'copyDWIM').
--    Use an empty list for a scalar.
--
-- 2. Executes the body of @lam@.
--
-- 3. Binds the t'SubExp's that are the 'Result' of @lam@ to the
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

-- | As applyLambda, but first rename the names in the lambda.  This
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
              flag_true (sExt32 (chunk_start - 1)) (sExt32 chunk_start)
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

    groupScan seg_flag (sExt64 w) (tvExp chunk_size) lam arrs_chunks

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

localThreadIDs :: [SubExp] -> InKernelGen [Imp.TExp Int64]
localThreadIDs dims = do
  ltid <- sExt64 . kernelLocalThreadId . kernelConstants <$> askEnv
  let dims' = map pe64 dims
  maybe (dIndexSpace' "ltid" dims' ltid) (pure . map sExt64)
    . M.lookup dims
    . kernelLocalIdMap
    . kernelConstants
    =<< askEnv

partitionSeqDims :: SegSeqDims -> SegSpace -> ([(VName, SubExp)], [(VName, SubExp)])
partitionSeqDims (SegSeqDims seq_is) space =
  bimap (map fst) (map fst) $
    partition ((`elem` seq_is) . snd) (zip (unSegSpace space) [0 ..])

compileFlatId :: SegSpace -> InKernelGen ()
compileFlatId space = do
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
        (_, AtomicPrim f) -> pure (l, f (Space "local") local_subhistos)
        (_, AtomicCAS f) -> pure (l, f (Space "local") local_subhistos)
        (Just l', AtomicLocking f) -> pure (l, f l' (Space "local") local_subhistos)
        (Nothing, AtomicLocking f) -> do
          locks <- newVName "locks"

          let num_locks = pe64 $ unCount group_size
              dims = map pe64 $ shapeDims (histOpShape op <> histShape op)
              l' = Locking locks 0 1 0 (pure . (`rem` num_locks) . flattenIndex dims)
              locks_t = Array int32 (Shape [unCount group_size]) NoUniqueness

          locks_mem <- sAlloc "locks_mem" (typeSize locks_t) $ Space "local"
          dArray locks int32 (arrayShape locks_t) locks_mem $
            IxFun.iota . map pe64 . arrayDims $
              locks_t

          sComment "All locks start out unlocked" $
            groupCoverSpace [kernelGroupSize constants] $ \is ->
              copyDWIMFix locks is (intConst Int32 0) []

          pure (Just l', f l' (Space "local") local_subhistos)

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
        Just num_chunks -> localOps threadOperations $ do
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
  is <- replicateM (arrayRank dest_t) (newVName "rep_i")
  let is' = map le64 is
  groupCoverSegSpace SegVirt (SegSpace flat $ zip is $ arrayDims dest_t) $
    copyDWIMFix (patElemName dest) is' se (drop (shapeRank ds) is')
  sOp $ Imp.Barrier Imp.FenceLocal
  where
    dest_t = patElemType dest
compileGroupExp (Pat [dest]) (BasicOp (Rotate rs arr)) = do
  ds <- map pe64 . arrayDims <$> lookupType arr
  groupCoverSpace ds $ \is -> do
    is' <- sequence $ zipWith3 rotate ds rs is
    copyDWIMFix (patElemName dest) is (Var arr) is'
  sOp $ Imp.Barrier Imp.FenceLocal
  where
    rotate d r i = dPrimVE "rot_i" $ rotateIndex d (pe64 r) i
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
    slice' = fmap pe64 slice
    dims = map pe64 $ arrayDims $ patElemType pe
    write = copyDWIM (patElemName pe) (unSlice slice') se []
compileGroupExp dest e =
  defCompileExp dest e

compileGroupOp :: OpCompiler GPUMem KernelEnv Imp.KernelOp
compileGroupOp pat (Alloc size space) =
  kernelAlloc pat size space
compileGroupOp pat (Inner (SegOp (SegMap lvl space _ body))) = do
  compileFlatId space

  groupCoverSegSpace (segVirt lvl) space $
    compileStms mempty (kernelBodyStms body) $
      zipWithM_ (compileThreadResult space) (patElems pat) $
        kernelBodyResult body
  sOp $ Imp.ErrorSync Imp.FenceLocal
compileGroupOp pat (Inner (SegOp (SegScan lvl space scans _ body))) = do
  compileFlatId space

  let (ltids, dims) = unzip $ unSegSpace space
      dims' = map pe64 dims

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
      take num_scan_results $
        patNames pat

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
  compileFlatId space

  let dims' = map pe64 dims
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
          (map (unitSlice 0) (init dims') ++ [DimFix $ last dims' - 1])

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
          (map (unitSlice 0) (init dims') ++ [DimFix $ last dims' - 1])

      sOp $ Imp.Barrier Imp.FenceLocal
compileGroupOp pat (Inner (SegOp (SegHist lvl space ops _ kbody))) = do
  compileFlatId space
  let (ltids, _dims) = unzip $ unSegSpace space

  -- We don't need the red_pes, because it is guaranteed by our type
  -- rules that they occupy the same memory as the destinations for
  -- the ops.
  let num_red_res = length ops + sum (map (length . histNeutral) ops)
      (_red_pes, map_pes) =
        splitAt num_red_res $ patElems pat

  group_size <- kernelGroupSizeCount . kernelConstants <$> askEnv
  ops' <- prepareIntraGroupSegHist group_size ops

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
          let bin' = pe64 bin
              dest_shape' = map pe64 $ shapeDims dest_shape
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
  compilerBugS $ "compileGroupOp: cannot compile rhs of binding " ++ prettyString pat

groupOperations :: Operations GPUMem KernelEnv Imp.KernelOp
groupOperations =
  (defaultOperations compileGroupOp)
    { opsCopyCompiler = copyInGroup,
      opsExpCompiler = compileGroupExp,
      opsStmsCompiler = \_ -> defCompileStms mempty,
      opsAllocCompilers =
        M.fromList [(Space "local", allocLocal)]
    }

arrayInLocalMemory :: SubExp -> InKernelGen Bool
arrayInLocalMemory (Var name) = do
  res <- lookupVar name
  case res of
    ArrayVar _ entry ->
      (Space "local" ==) . entryMemSpace
        <$> lookupMemory (memLocName (entryArrayLoc entry))
    _ -> pure False
arrayInLocalMemory Constant {} = pure False

sKernelGroup ::
  String ->
  VName ->
  KernelAttrs ->
  InKernelGen () ->
  CallKernelGen ()
sKernelGroup = sKernel groupOperations kernelGroupId

compileGroupResult ::
  SegSpace ->
  PatElem LetDecMem ->
  KernelResult ->
  InKernelGen ()
compileGroupResult _ pe (TileReturns _ [(w, per_group_elems)] what) = do
  n <- pe64 . arraySize 0 <$> lookupType what

  constants <- kernelConstants <$> askEnv
  let ltid = sExt64 $ kernelLocalThreadId constants
      offset =
        pe64 per_group_elems
          * sExt64 (kernelGroupId constants)

  -- Avoid loop for the common case where each thread is statically
  -- known to write at most one element.
  localOps threadOperations $
    if pe64 per_group_elems == kernelGroupSize constants
      then
        sWhen (ltid + offset .<. pe64 w) $
          copyDWIMFix (patElemName pe) [ltid + offset] (Var what) [ltid]
      else sFor "i" (n `divUp` kernelGroupSize constants) $ \i -> do
        j <- dPrimVE "j" $ kernelGroupSize constants * i + ltid
        sWhen (j + offset .<. pe64 w) $
          copyDWIMFix (patElemName pe) [j + offset] (Var what) [j]
compileGroupResult space pe (TileReturns _ dims what) = do
  let gids = map fst $ unSegSpace space
      out_tile_sizes = map (pe64 . snd) dims
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
      group_tiles' = map pe64 group_tiles
      reg_tiles' = map pe64 reg_tiles

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
        pure $ DimSlice tile_dim_start reg_tile 1
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
      sWhen (foldl1 (.&&.) $ zipWith (.<.) dest_is $ map pe64 dims) $
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

-- | The sizes of nested iteration spaces in the kernel.
type SegOpSizes = S.Set [SubExp]

-- | Various useful precomputed information for group-level SegOps.
data Precomputed = Precomputed
  { pcSegOpSizes :: SegOpSizes,
    pcChunkItersMap :: M.Map [SubExp] (Imp.TExp Int32)
  }

-- | Find the sizes of nested parallelism in a t'SegOp' body.
segOpSizes :: Stms GPUMem -> SegOpSizes
segOpSizes = onStms
  where
    onStms = foldMap onStm
    onStm (Let _ _ (Op (Inner (SegOp op)))) =
      case segVirt $ segLevel op of
        SegNoVirtFull seq_dims ->
          S.singleton $ map snd $ snd $ partitionSeqDims seq_dims $ segSpace op
        _ -> S.singleton $ map snd $ unSegSpace $ segSpace op
    onStm (Let (Pat [pe]) _ (BasicOp (Replicate {}))) =
      S.singleton $ arrayDims $ patElemType pe
    onStm (Let (Pat [pe]) _ (BasicOp (Iota {}))) =
      S.singleton $ arrayDims $ patElemType pe
    onStm (Let (Pat [pe]) _ (BasicOp (Copy {}))) =
      S.singleton $ arrayDims $ patElemType pe
    onStm (Let (Pat [pe]) _ (BasicOp (Manifest {}))) =
      S.singleton $ arrayDims $ patElemType pe
    onStm (Let _ _ (Match _ cases defbody _)) =
      foldMap (onStms . bodyStms . caseBody) cases <> onStms (bodyStms defbody)
    onStm (Let _ _ (DoLoop _ _ body)) =
      onStms (bodyStms body)
    onStm _ = mempty

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
      let dims' = map pe64 dims
      ids' <- dIndexSpace' "ltid_pre" dims' (sExt64 ltid)
      pure (dims, map sExt32 ids')
