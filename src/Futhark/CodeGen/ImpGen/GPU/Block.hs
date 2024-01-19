{-# LANGUAGE TypeFamilies #-}

-- | Generation of kernels with block-level bodies.
module Futhark.CodeGen.ImpGen.GPU.Block
  ( sKernelBlock,
    compileBlockResult,
    blockOperations,

    -- * Precomputation
    Precomputed,
    precomputeConstants,
    precomputedConstants,
    atomicUpdateLocking,
  )
where

import Control.Monad
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
import Futhark.IR.Mem.LMAD qualified as LMAD
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
    fromMaybe (error "flattenArray") $
      LMAD.reshape (memLocLMAD arr_loc) (map pe64 $ shapeDims flat_shape)

sliceArray :: Imp.TExp Int64 -> TV Int64 -> VName -> ImpM rep r op VName
sliceArray start size arr = do
  MemLoc mem _ lmad <- entryArrayLoc <$> lookupArray arr
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
    $ LMAD.slice lmad slice

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
  (Mem rep inner) =>
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
  (Mem rep inner) =>
  Lambda rep ->
  [(VName, [DimIndex (Imp.TExp Int64)])] ->
  [(SubExp, [DimIndex (Imp.TExp Int64)])] ->
  ImpM rep r op ()
applyRenamedLambda lam dests args = do
  lam_renamed <- renameLambda lam
  applyLambda lam_renamed dests args

blockChunkLoop ::
  Imp.TExp Int32 ->
  (Imp.TExp Int32 -> TV Int64 -> InKernelGen ()) ->
  InKernelGen ()
blockChunkLoop w m = do
  constants <- kernelConstants <$> askEnv
  let max_chunk_size = sExt32 $ kernelBlockSize constants
  num_chunks <- dPrimVE "num_chunks" $ w `divUp` max_chunk_size
  sFor "chunk_i" num_chunks $ \chunk_i -> do
    chunk_start <-
      dPrimVE "chunk_start" $ chunk_i * max_chunk_size
    chunk_end <-
      dPrimVE "chunk_end" $ sMin32 w (chunk_start + max_chunk_size)
    chunk_size <-
      dPrimV "chunk_size" $ sExt64 $ chunk_end - chunk_start
    m chunk_start chunk_size

virtualisedBlockScan ::
  Maybe (Imp.TExp Int32 -> Imp.TExp Int32 -> Imp.TExp Bool) ->
  Imp.TExp Int32 ->
  Lambda GPUMem ->
  [VName] ->
  InKernelGen ()
virtualisedBlockScan seg_flag w lam arrs = do
  blockChunkLoop w $ \chunk_start chunk_size -> do
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
          (map (,[DimFix $ sExt64 chunk_start]) arrs)
          ( map ((,[DimFix carry_idx]) . Var) arrs
              ++ map ((,[DimFix $ sExt64 chunk_start]) . Var) arrs
          )

    arrs_chunks <- mapM (sliceArray (sExt64 chunk_start) chunk_size) arrs

    sOp $ Imp.ErrorSync Imp.FenceLocal

    blockScan seg_flag (sExt64 w) (tvExp chunk_size) lam arrs_chunks

copyInBlock :: CopyCompiler GPUMem KernelEnv Imp.KernelOp
copyInBlock pt destloc srcloc = do
  dest_space <- entryMemSpace <$> lookupMemory (memLocName destloc)
  src_space <- entryMemSpace <$> lookupMemory (memLocName srcloc)

  let src_lmad = memLocLMAD srcloc
      dims = LMAD.shape src_lmad
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
      lmadCopy
        pt
        (sliceMemLoc destloc destslice')
        (sliceMemLoc srcloc srcslice')
    _ -> do
      blockCoverSpace (map sExt32 dims) $ \is ->
        lmadCopy
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

-- Construct the necessary lock arrays for an intra-block histogram.
prepareIntraBlockSegHist ::
  Shape ->
  Count BlockSize SubExp ->
  [HistOp GPUMem] ->
  InKernelGen [[Imp.TExp Int64] -> InKernelGen ()]
prepareIntraBlockSegHist segments tblock_size =
  fmap snd . mapAccumLM onOp Nothing
  where
    onOp l op = do
      constants <- kernelConstants <$> askEnv
      atomicBinOp <- kernelAtomics <$> askEnv

      let local_subhistos = histDest op

      case (l, atomicUpdateLocking atomicBinOp $ histOp op) of
        (_, AtomicPrim f) -> pure (l, f (Space "shared") local_subhistos)
        (_, AtomicCAS f) -> pure (l, f (Space "shared") local_subhistos)
        (Just l', AtomicLocking f) -> pure (l, f l' (Space "shared") local_subhistos)
        (Nothing, AtomicLocking f) -> do
          locks <- newVName "locks"

          let num_locks = pe64 $ unCount tblock_size
              dims = map pe64 $ shapeDims (segments <> histOpShape op <> histShape op)
              l' = Locking locks 0 1 0 (pure . (`rem` num_locks) . flattenIndex dims)
              locks_t = Array int32 (Shape [unCount tblock_size]) NoUniqueness

          locks_mem <- sAlloc "locks_mem" (typeSize locks_t) $ Space "shared"
          dArray locks int32 (arrayShape locks_t) locks_mem $
            LMAD.iota 0 . map pe64 . arrayDims $
              locks_t

          sComment "All locks start out unlocked" $
            blockCoverSpace [kernelBlockSize constants] $ \is ->
              copyDWIMFix locks is (intConst Int32 0) []

          pure (Just l', f l' (Space "shared") local_subhistos)

blockCoverSegSpace :: SegVirt -> SegSpace -> InKernelGen () -> InKernelGen ()
blockCoverSegSpace virt space m = do
  let (ltids, dims) = unzip $ unSegSpace space
      dims' = map pe64 dims

  constants <- kernelConstants <$> askEnv
  let tblock_size = kernelBlockSize constants
  -- Maybe we can statically detect that this is actually a
  -- SegNoVirtFull and generate ever-so-slightly simpler code.
  let virt' = if dims' == [tblock_size] then SegNoVirtFull (SegSeqDims []) else virt
  case virt' of
    SegVirt -> do
      iters <- M.lookup dims . kernelChunkItersMap . kernelConstants <$> askEnv
      case iters of
        Nothing -> do
          iterations <- dPrimVE "iterations" $ product $ map sExt32 dims'
          blockLoop iterations $ \i -> do
            dIndexSpace (zip ltids dims') $ sExt64 i
            m
        Just num_chunks -> localOps threadOperations $ do
          let ltid = kernelLocalThreadId constants
          sFor "chunk_i" num_chunks $ \chunk_i -> do
            i <- dPrimVE "i" $ chunk_i * sExt32 tblock_size + ltid
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

compileBlockExp :: ExpCompiler GPUMem KernelEnv Imp.KernelOp
compileBlockExp (Pat [pe]) (BasicOp (Opaque _ se)) =
  -- Cannot print in GPU code.
  copyDWIM (patElemName pe) [] se []
-- The static arrays stuff does not work inside kernels.
compileBlockExp (Pat [dest]) (BasicOp (ArrayLit es _)) =
  forM_ (zip [0 ..] es) $ \(i, e) ->
    copyDWIMFix (patElemName dest) [fromIntegral (i :: Int64)] e []
compileBlockExp _ (BasicOp (UpdateAcc acc is vs)) = do
  ltid <- kernelLocalThreadId . kernelConstants <$> askEnv
  sWhen (ltid .==. 0) $ updateAcc acc is vs
  sOp $ Imp.Barrier Imp.FenceLocal
compileBlockExp (Pat [dest]) (BasicOp (Replicate ds se)) | ds /= mempty = do
  flat <- newVName "rep_flat"
  is <- replicateM (arrayRank dest_t) (newVName "rep_i")
  let is' = map le64 is
  blockCoverSegSpace SegVirt (SegSpace flat $ zip is $ arrayDims dest_t) $
    copyDWIMFix (patElemName dest) is' se (drop (shapeRank ds) is')
  sOp $ Imp.Barrier Imp.FenceLocal
  where
    dest_t = patElemType dest
compileBlockExp (Pat [dest]) (BasicOp (Iota n e s it)) = do
  n' <- toExp n
  e' <- toExp e
  s' <- toExp s
  blockLoop (TPrimExp n') $ \i' -> do
    x <-
      dPrimV "x" $
        TPrimExp $
          BinOpExp (Add it OverflowUndef) e' $
            BinOpExp (Mul it OverflowUndef) (untyped i') s'
    copyDWIMFix (patElemName dest) [i'] (Var (tvVar x)) []
  sOp $ Imp.Barrier Imp.FenceLocal

-- When generating code for a scalar in-place update, we must make
-- sure that only one thread performs the write.  When writing an
-- array, the block-level copy code will take care of doing the right
-- thing.
compileBlockExp (Pat [pe]) (BasicOp (Update safety _ slice se))
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
compileBlockExp dest e = do
  -- It is a messy to jump into control flow for error handling.
  -- Avoid that by always doing an error sync here.  Potential
  -- improvement: only do this if any errors are pending (this could
  -- also be handled in later codegen).
  when (doSync e) $ sOp $ Imp.ErrorSync Imp.FenceLocal
  defCompileExp dest e
  where
    doSync Loop {} = True
    doSync Match {} = True
    doSync _ = False

blockAlloc ::
  Pat LetDecMem ->
  SubExp ->
  Space ->
  InKernelGen ()
blockAlloc (Pat [_]) _ ScalarSpace {} =
  -- Handled by the declaration of the memory block, which is then
  -- translated to an actual scalar variable during C code generation.
  pure ()
blockAlloc (Pat [mem]) size (Space "shared") =
  allocLocal (patElemName mem) $ Imp.bytes $ pe64 size
blockAlloc (Pat [mem]) _ _ =
  compilerLimitationS $ "Cannot allocate memory block " ++ prettyString mem ++ " in kernel block."
blockAlloc dest _ _ =
  error $ "Invalid target for in-kernel allocation: " ++ show dest

compileBlockOp :: OpCompiler GPUMem KernelEnv Imp.KernelOp
compileBlockOp pat (Alloc size space) =
  blockAlloc pat size space
compileBlockOp pat (Inner (SegOp (SegMap lvl space _ body))) = do
  compileFlatId space

  blockCoverSegSpace (segVirt lvl) space $
    compileStms mempty (kernelBodyStms body) $
      zipWithM_ (compileThreadResult space) (patElems pat) $
        kernelBodyResult body
  sOp $ Imp.ErrorSync Imp.FenceLocal
compileBlockOp pat (Inner (SegOp (SegScan lvl space scans _ body))) = do
  compileFlatId space

  let (ltids, dims) = unzip $ unSegSpace space
      dims' = map pe64 dims

  blockCoverSegSpace (segVirt lvl) space $
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

  -- blockScan needs to treat the scan output as a one-dimensional
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
      virtualisedBlockScan
        (Just crossesSegment)
        (sExt32 $ tvExp dims_flat)
        (segBinOpLambda scan)
        arrs_flat
    _ ->
      blockScan
        (Just crossesSegment)
        (product dims')
        (product dims')
        (segBinOpLambda scan)
        arrs_flat
compileBlockOp pat (Inner (SegOp (SegRed lvl space ops _ body))) = do
  compileFlatId space

  let dims' = map pe64 dims
      mkTempArr t =
        sAllocArray "red_arr" (elemType t) (Shape dims <> arrayShape t) $ Space "shared"

  tmp_arrs <- mapM mkTempArr $ concatMap (lambdaReturnType . segBinOpLambda) ops
  blockCoverSegSpace (segVirt lvl) space $
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
      blockChunkLoop (sExt32 dim') $ \chunk_start chunk_size -> do
        sComment "possibly incorporate carry" $
          sWhen (chunk_start .>. 0 .&&. ltid .==. 0) $
            forM_ (zip ops tmps_for_ops) $ \(op, tmps) ->
              applyRenamedLambda
                (segBinOpLambda op)
                (map (,[DimFix $ sExt64 chunk_start]) tmps)
                ( map ((,[]) . Var . patElemName) red_pes
                    ++ map ((,[DimFix $ sExt64 chunk_start]) . Var) tmps
                )

        sOp $ Imp.ErrorSync Imp.FenceLocal

        forM_ (zip ops tmps_for_ops) $ \(op, tmps) -> do
          tmps_chunks <- mapM (sliceArray (sExt64 chunk_start) chunk_size) tmps
          blockReduce (sExt32 (tvExp chunk_size)) (segBinOpLambda op) tmps_chunks

        sOp $ Imp.ErrorSync Imp.FenceLocal

        sComment "Save result of reduction." $
          forM_ (zip red_pes $ concat tmps_for_ops) $ \(pe, arr) ->
            copyDWIMFix (patElemName pe) [] (Var arr) [sExt64 chunk_start]

    --
    virtCase dims' tmps_for_ops = do
      dims_flat <- dPrimV "dims_flat" $ product dims'
      let segment_size = last dims'
          crossesSegment from to =
            (sExt64 to - sExt64 from) .>. (sExt64 to `rem` sExt64 segment_size)

      forM_ (zip ops tmps_for_ops) $ \(op, tmps) -> do
        tmps_flat <- mapM (flattenArray (length dims') dims_flat) tmps
        virtualisedBlockScan
          (Just crossesSegment)
          (sExt32 $ tvExp dims_flat)
          (segBinOpLambda op)
          tmps_flat

      sOp $ Imp.ErrorSync Imp.FenceLocal

      sComment "Save result of reduction." $
        forM_ (zip red_pes $ concat tmps_for_ops) $ \(pe, arr) ->
          copyDWIM
            (patElemName pe)
            []
            (Var arr)
            (map (unitSlice 0) (init dims') ++ [DimFix $ last dims' - 1])

      sOp $ Imp.Barrier Imp.FenceLocal

    -- Nonsegmented case (or rather, a single segment) - this we can
    -- handle directly with a block-level reduction.
    nonvirtCase [dim'] tmps_for_ops = do
      forM_ (zip ops tmps_for_ops) $ \(op, tmps) ->
        blockReduce (sExt32 dim') (segBinOpLambda op) tmps
      sOp $ Imp.ErrorSync Imp.FenceLocal
      sComment "Save result of reduction." $
        forM_ (zip red_pes $ concat tmps_for_ops) $ \(pe, arr) ->
          copyDWIMFix (patElemName pe) [] (Var arr) [0]
      sOp $ Imp.ErrorSync Imp.FenceLocal

    -- Segmented intra-block reductions are turned into (regular)
    -- segmented scans.  It is possible that this can be done
    -- better, but at least this approach is simple.
    nonvirtCase dims' tmps_for_ops = do
      -- blockScan operates on flattened arrays.  This does not
      -- involve copying anything; merely playing with the index
      -- function.
      dims_flat <- dPrimV "dims_flat" $ product dims'
      let segment_size = last dims'
          crossesSegment from to =
            (sExt64 to - sExt64 from) .>. (sExt64 to `rem` sExt64 segment_size)

      forM_ (zip ops tmps_for_ops) $ \(op, tmps) -> do
        tmps_flat <- mapM (flattenArray (length dims') dims_flat) tmps
        blockScan
          (Just crossesSegment)
          (product dims')
          (product dims')
          (segBinOpLambda op)
          tmps_flat

      sOp $ Imp.ErrorSync Imp.FenceLocal

      sComment "Save result of reduction." $
        forM_ (zip red_pes $ concat tmps_for_ops) $ \(pe, arr) ->
          copyDWIM
            (patElemName pe)
            []
            (Var arr)
            (map (unitSlice 0) (init dims') ++ [DimFix $ last dims' - 1])

      sOp $ Imp.Barrier Imp.FenceLocal
compileBlockOp pat (Inner (SegOp (SegHist lvl space ops _ kbody))) = do
  compileFlatId space
  let (ltids, dims) = unzip $ unSegSpace space

  -- We don't need the red_pes, because it is guaranteed by our type
  -- rules that they occupy the same memory as the destinations for
  -- the ops.
  let num_red_res = length ops + sum (map (length . histNeutral) ops)
      (_red_pes, map_pes) =
        splitAt num_red_res $ patElems pat

  tblock_size <- kernelBlockSizeCount . kernelConstants <$> askEnv
  ops' <- prepareIntraBlockSegHist (Shape $ init dims) tblock_size ops

  -- Ensure that all locks have been initialised.
  sOp $ Imp.Barrier Imp.FenceLocal

  blockCoverSegSpace (segVirt lvl) space $
    compileStms mempty (kernelBodyStms kbody) $ do
      let (red_res, map_res) = splitAt num_red_res $ kernelBodyResult kbody
          (red_is, red_vs) = splitAt (length ops) $ map kernelResultSubExp red_res
      zipWithM_ (compileThreadResult space) map_pes map_res

      let vs_per_op = chunks (map (length . histDest) ops) red_vs

      forM_ (zip4 red_is vs_per_op ops' ops) $
        \(bin, op_vs, do_op, HistOp dest_shape _ _ _ shape lam) -> do
          let bin' = pe64 bin
              dest_shape' = map pe64 $ shapeDims dest_shape
              bin_in_bounds = inBounds (Slice [DimFix bin']) dest_shape'
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
compileBlockOp pat _ =
  compilerBugS $ "compileBlockOp: cannot compile rhs of binding " ++ prettyString pat

blockOperations :: Operations GPUMem KernelEnv Imp.KernelOp
blockOperations =
  (defaultOperations compileBlockOp)
    { opsCopyCompiler = copyInBlock,
      opsExpCompiler = compileBlockExp,
      opsStmsCompiler = \_ -> defCompileStms mempty,
      opsAllocCompilers =
        M.fromList [(Space "shared", allocLocal)]
    }

arrayInSharedMemory :: SubExp -> InKernelGen Bool
arrayInSharedMemory (Var name) = do
  res <- lookupVar name
  case res of
    ArrayVar _ entry ->
      (Space "shared" ==) . entryMemSpace
        <$> lookupMemory (memLocName (entryArrayLoc entry))
    _ -> pure False
arrayInSharedMemory Constant {} = pure False

sKernelBlock ::
  String ->
  VName ->
  KernelAttrs ->
  InKernelGen () ->
  CallKernelGen ()
sKernelBlock = sKernel blockOperations kernelBlockId

compileBlockResult ::
  SegSpace ->
  PatElem LetDecMem ->
  KernelResult ->
  InKernelGen ()
compileBlockResult _ pe (TileReturns _ [(w, per_block_elems)] what) = do
  n <- pe64 . arraySize 0 <$> lookupType what

  constants <- kernelConstants <$> askEnv
  let ltid = sExt64 $ kernelLocalThreadId constants
      offset =
        pe64 per_block_elems
          * sExt64 (kernelBlockId constants)

  -- Avoid loop for the common case where each thread is statically
  -- known to write at most one element.
  localOps threadOperations $
    if pe64 per_block_elems == kernelBlockSize constants
      then
        sWhen (ltid + offset .<. pe64 w) $
          copyDWIMFix (patElemName pe) [ltid + offset] (Var what) [ltid]
      else sFor "i" (n `divUp` kernelBlockSize constants) $ \i -> do
        j <- dPrimVE "j" $ kernelBlockSize constants * i + ltid
        sWhen (j + offset .<. pe64 w) $
          copyDWIMFix (patElemName pe) [j + offset] (Var what) [j]
compileBlockResult space pe (TileReturns _ dims what) = do
  let gids = map fst $ unSegSpace space
      out_tile_sizes = map (pe64 . snd) dims
      block_is = zipWith (*) (map Imp.le64 gids) out_tile_sizes
  local_is <- localThreadIDs $ map snd dims
  is_for_thread <-
    mapM (dPrimV "thread_out_index") $
      zipWith (+) block_is local_is

  localOps threadOperations $
    sWhen (isActive $ zip (map tvVar is_for_thread) $ map fst dims) $
      copyDWIMFix (patElemName pe) (map tvExp is_for_thread) (Var what) local_is
compileBlockResult space pe (RegTileReturns _ dims_n_tiles what) = do
  constants <- kernelConstants <$> askEnv

  let gids = map fst $ unSegSpace space
      (dims, block_tiles, reg_tiles) = unzip3 dims_n_tiles
      block_tiles' = map pe64 block_tiles
      reg_tiles' = map pe64 reg_tiles

  -- Which block tile is this block responsible for?
  let block_tile_is = map Imp.le64 gids

  -- Within the block tile, which register tile is this thread
  -- responsible for?
  reg_tile_is <-
    dIndexSpace' "reg_tile_i" block_tiles' $ sExt64 $ kernelLocalThreadId constants

  -- Compute output array slice for the register tile belonging to
  -- this thread.
  let regTileSliceDim (block_tile, block_tile_i) (reg_tile, reg_tile_i) = do
        tile_dim_start <-
          dPrimVE "tile_dim_start" $
            reg_tile * (block_tile * block_tile_i + reg_tile_i)
        pure $ DimSlice tile_dim_start reg_tile 1
  reg_tile_slices <-
    Slice
      <$> zipWithM
        regTileSliceDim
        (zip block_tiles' block_tile_is)
        (zip reg_tiles' reg_tile_is)

  localOps threadOperations $
    sLoopNest (Shape reg_tiles) $ \is_in_reg_tile -> do
      let dest_is = fixSlice reg_tile_slices is_in_reg_tile
          src_is = reg_tile_is ++ is_in_reg_tile
      sWhen (foldl1 (.&&.) $ zipWith (.<.) dest_is $ map pe64 dims) $
        copyDWIMFix (patElemName pe) dest_is (Var what) src_is
compileBlockResult space pe (Returns _ _ what) = do
  constants <- kernelConstants <$> askEnv
  in_shared_memory <- arrayInSharedMemory what
  let gids = map (Imp.le64 . fst) $ unSegSpace space

  if not in_shared_memory
    then
      localOps threadOperations $
        sWhen (kernelLocalThreadId constants .==. 0) $
          copyDWIMFix (patElemName pe) gids what []
    else -- If the result of the block is an array in shared memory, we
    -- store it by collective copying among all the threads of the
    -- block.  TODO: also do this if the array is in global memory
    -- (but this is a bit more tricky, synchronisation-wise).
      copyDWIMFix (patElemName pe) gids what []
compileBlockResult _ _ WriteReturns {} =
  compilerLimitationS "compileBlockResult: WriteReturns not handled yet."

-- | The sizes of nested iteration spaces in the kernel.
type SegOpSizes = S.Set [SubExp]

-- | Various useful precomputed information for block-level SegOps.
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
    onStm (Let (Pat [pe]) _ (BasicOp (Manifest {}))) =
      S.singleton $ arrayDims $ patElemType pe
    onStm (Let _ _ (Match _ cases defbody _)) =
      foldMap (onStms . bodyStms . caseBody) cases <> onStms (bodyStms defbody)
    onStm (Let _ _ (Loop _ _ body)) =
      onStms (bodyStms body)
    onStm _ = mempty

-- | Precompute various constants and useful information.
precomputeConstants :: Count BlockSize (Imp.TExp Int64) -> Stms GPUMem -> CallKernelGen Precomputed
precomputeConstants tblock_size stms = do
  let sizes = segOpSizes stms
  iters_map <- M.fromList <$> mapM mkMap (S.toList sizes)
  pure $ Precomputed sizes iters_map
  where
    mkMap dims = do
      let n = product $ map Imp.pe64 dims
      num_chunks <- dPrimVE "num_chunks" $ sExt32 $ n `divUp` unCount tblock_size
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
