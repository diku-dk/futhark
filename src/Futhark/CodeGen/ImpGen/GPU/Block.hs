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
import Futhark.Util.IntegralExp (ceilDiv, rem)
import Prelude hiding (quot, rem)

-- | @flattenArray k flat arr@ flattens the outer @k@ dimensions of
-- @arr@ to @flat@.  (Make sure @flat@ is the sum of those dimensions
-- or you'll have a bad time.)
flattenArray :: Int -> TV Int64 -> VName -> ImpM rep r op VName
flattenArray k flat arr = do
  ArrayEntry arr_loc pt <- lookupArray arr
  let flat_shape = Shape $ Var (tvVar flat) : drop k (memLocShape arr_loc)
  sArray (baseName arr <> "_flat") pt flat_shape (memLocName arr_loc) $
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
    (baseName arr <> "_chunk")
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
  -- The loop body contains error syncs, and failures must not jump into it.
  sOp $ Imp.ErrorSync Imp.FenceLocal
  constants <- kernelConstants <$> askEnv
  let max_chunk_size = sExt32 $ kernelBlockSize constants
  num_chunks <- dPrimVE "num_chunks" $ w `ceilDiv` max_chunk_size
  sFor "chunk_i" num_chunks $ \chunk_i -> do
    chunk_start <-
      dPrimVE "chunk_start" $ chunk_i * max_chunk_size
    chunk_end <-
      dPrimVE "chunk_end" $ sMin32 w (chunk_start + max_chunk_size)
    chunk_size <-
      dPrimV "chunk_size" $ sExt64 $ chunk_end - chunk_start
    m chunk_start chunk_size

-- | Whether two indices into a flattened in-block space, segmented along its
-- innermost dimension of the given size, are in different segments. We know
-- these must fit in 32-bit integers, which means we can use much cheaper 32-bit
-- divisions.
crossesSegment :: Imp.TExp Int64 -> Imp.TExp Int32 -> Imp.TExp Int32 -> Imp.TExp Bool
crossesSegment segment_size from to =
  (to - from) .>. (to `rem` sExt32 segment_size)

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
    sComment "possibly incorporate carry" . localOps threadOperations $
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

-- | Semantically, a block-level statement finishes before the next one. This
-- means that any statement that writes memory must be enclosed in barriers, as
-- sibling statements may reuse the same memory. Our current approach to this is
-- a bit naive and may result in duplicate barriers. If this ever turns out to
-- be a performance problem, we'll do something more sophisticated.
beforeBlockWrite :: InKernelGen ()
beforeBlockWrite = sOp $ Imp.Barrier Imp.FenceLocal

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
      beforeBlockWrite
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
  dPrimV_ (segFlat space) $ sExt64 ltid

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
              locks_t = Array int32 (Shape [unCount tblock_size]) NoMode

          locks_mem <- sAlloc "locks_mem" (typeSize locks_t) $ Space "shared"
          dArray locks int32 (arrayShape locks_t) locks_mem $
            LMAD.iota 0 . map pe64 . arrayDims $
              locks_t

          sComment "All locks start out unlocked" $
            blockCoverSpace [kernelBlockSize constants] $ \is ->
              copyDWIMFix locks is (intConst Int32 0) []

          pure (Just l', f l' (Space "shared") local_subhistos)

-- | The virtualisation that 'blockCoverSegSpace' will actually use for this
-- index space.  Maybe we can statically detect that the requested one is really
-- a 'SegNoVirtFull', and generate ever-so-slightly simpler code.  Covering an
-- index space twice has to know this, as it decides whether the cover loops -
-- and so introduces a scope of its own for the indexes.
blockCoverVirt :: SegVirt -> SegSpace -> InKernelGen SegVirt
blockCoverVirt virt space = do
  tblock_size <- kernelBlockSize . kernelConstants <$> askEnv
  pure $
    if map pe64 (segSpaceDims space) == [tblock_size]
      then SegNoVirtFull (SegSeqDims [])
      else virt

blockCoverSegSpace :: SegVirt -> SegSpace -> InKernelGen () -> InKernelGen ()
blockCoverSegSpace virt space m = do
  let (ltids, dims) = unzip $ unSegSpace space
      dims' = map pe64 dims

  constants <- kernelConstants <$> askEnv
  let tblock_size = kernelBlockSize constants
  virt' <- blockCoverVirt virt space
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
compileBlockExp (Pat [dest]) (BasicOp (ArrayVal vs t)) =
  compileBlockExp (Pat [dest]) (BasicOp (ArrayLit (map Constant vs) (Prim t)))
compileBlockExp (Pat [dest]) (BasicOp (ArrayLit es _)) = do
  beforeBlockWrite
  forM_ (zip [0 ..] es) $ \(i, e) ->
    copyDWIMFix (patElemName dest) [fromIntegral (i :: Int64)] e []
compileBlockExp _ (BasicOp (UpdateAcc safety acc is vs)) = do
  ltid <- kernelLocalThreadId . kernelConstants <$> askEnv
  sWhen (ltid .==. 0) $ updateAcc safety acc is vs
  sOp $ Imp.Barrier Imp.FenceLocal
compileBlockExp (Pat [dest]) (BasicOp (Replicate ds se)) | ds /= mempty = do
  beforeBlockWrite
  flat <- newVName "rep_flat"
  is <- replicateM (arrayRank dest_t) (newVName "rep_i")
  let is' = map le64 is
  blockCoverSegSpace SegVirt (SegSpace flat $ zip is $ arrayDims dest_t) $
    copyDWIMFix (patElemName dest) is' se (drop (shapeRank ds) is')
  sOp $ Imp.Barrier Imp.FenceLocal
  where
    dest_t = patElemType dest
compileBlockExp (Pat [dest]) (BasicOp (Iota n e s it)) = do
  beforeBlockWrite
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

-- | How many elements an operand array for 'blockScan' must be backed by, given
-- the number of elements actually scanned and the return types of the operator.
-- Array-typed elements are kept twice over, with some scratch space past that;
-- see 'blockScan'.
blockScanElems :: Imp.TExp Int64 -> [Type] -> Imp.TExp Int64
blockScanElems n ts
  | all primType ts = n
  | otherwise = 2 * n + 2 * blockScanChunkSize

-- | A shared memory array of @elems@ values of type @elem_t@, staging the
-- operands of a 'blockScan' or 'blockReduce'. Note that 'blockScan' needs
-- memory past the index space it scans, and that excess is deliberately left
-- outside the shape (but is given by @elems@), as nothing but 'blockScan' may
-- touch it.
sharedStagingArray ::
  Name -> Type -> Imp.TExp Int64 -> Shape -> InKernelGen VName
sharedStagingArray desc elem_t elems shape = do
  mem <-
    sAlloc (desc <> "_mem") (Imp.bytes $ elems * unCount (typeSize elem_t)) $
      Space "shared"
  sArrayInMem desc (elemType elem_t) shape mem

-- | Will a single thread be responsible for more than one element of this index
-- space? If so, a value computed for one element in one phase of a multi-phase
-- operation cannot be kept in a scalar variable until a later phase, as the
-- thread will by then have overwritten it with the value belonging to some
-- other element.
severalElemsPerThread :: SegVirt -> Bool
severalElemsPerThread SegVirt = True
severalElemsPerThread SegNoVirt = False
severalElemsPerThread (SegNoVirtFull (SegSeqDims seq_dims)) = not $ null seq_dims

-- | Compile a block-level 'SegScan'. This proceeds in three phases, separated
-- by barriers:
--
-- 1. The kernel body is run to produce the scan inputs as distinct arrays.
--
-- 2. The scan itself is performed.
--
-- 3. Finally the post operator is applied to the scan results.
--
-- Everything that has to survive a barrier lives in shared memory, and because
-- the index space may be virtualised (such that a thread handles more than one
-- element), that includes the map results of the kernel body.
compileBlockScan ::
  Pat LetDecMem ->
  SegLevel ->
  SegSpace ->
  KernelBody GPUMem ->
  [SegBinOp GPUMem] ->
  SegPostOp GPUMem ->
  InKernelGen ()
compileBlockScan pat lvl space body scans post_op = do
  compileFlatId space

  -- Whether a thread ends up handling several elements of the index space
  -- decides both where the map results have to live, and how the post operator
  -- gets at the indexes again.
  virt <- blockCoverVirt (segVirt lvl) space

  scan_arrs <- mapM scanArray scan_ts
  map_dests <- mapM (mapResDest virt) map_ts

  dScope Nothing $ scopeOfLParams $ lambdaParams post_lam

  computeScanInputs virt scan_arrs map_dests
  sOp $ Imp.ErrorSync Imp.FenceLocal
  doScan virt scan_arrs
  applyPostOp virt scan_arrs map_dests

  sOp $ Imp.Barrier Imp.FenceGlobal
  where
    (ltids, dims) = unzip $ unSegSpace space
    dims' = map pe64 dims
    [scan] = scans
    post_lam = segPostOpLambda post_op
    num_scan_res = length $ segBinOpNeutral scan
    (scan_pars, map_pars) = splitAt num_scan_res $ lambdaParams post_lam
    (body_scan_res, body_map_res) = splitAt num_scan_res $ bodyResult body
    scan_ts = concatMap shpT scans
    shpT op = (segBinOpShape op,) <$> lambdaReturnType (segBinOpLambda op)
    map_ts = map ((mempty,) . paramType) map_pars

    shapeFor s t =
      arrayShape $ foldr (flip arrayOfRow) (arrayOfShape t s) dims

    -- One value per element of the index space, plus whatever 'blockScan'
    -- needs past that.
    scanArray (s, t) =
      sharedStagingArray
        "scan_out"
        (t `arrayOfShape` s)
        (blockScanElems (product dims') (lambdaReturnType (segBinOpLambda scan)))
        (shapeFor s t)

    -- Map results are not operands of the scan, so one value per element of
    -- the index space is all they need.
    mapArray (s, t) =
      sharedStagingArray
        "map_out"
        (t `arrayOfShape` s)
        (product dims')
        (shapeFor s t)

    -- Where does a map result produced by the kernel body live until the post
    -- operator needs it? A shared memory array when a thread handles several
    -- elements, as a scalar would by then have been overwritten; otherwise the
    -- post operator parameter itself. Accumulators are phantoms in ImpGen, so
    -- they need no storage at all.
    mapResDest virt (s, t)
      | isAcc t = pure Nothing
      | severalElemsPerThread virt = Just <$> mapArray (s, t)
      | otherwise = pure Nothing

    seg_flag = case dims' of
      [_] -> Nothing -- Nonsegmented.
      _ -> Just $ crossesSegment $ last dims'

    -- Run the kernel body for every element of the index space, stashing the
    -- scan inputs - and the map results, if they cannot be kept in scalars -
    -- in shared memory.
    computeScanInputs virt scan_arrs map_dests =
      blockCoverSegSpace virt space $
        compileStms mempty (bodyStms body) $ do
          let ltids' = map Imp.le64 ltids
          forM_ (zip scan_arrs body_scan_res) $ \(arr, res) ->
            copyDWIMFix arr ltids' (kernelResultSubExp res) []

          sComment "stash map results for the post operator" $
            forM_ (zip3 map_pars map_dests body_map_res) $ \(par, dest, res) ->
              case dest of
                Just arr -> copyDWIMFix arr ltids' (kernelResultSubExp res) []
                Nothing -> copyDWIMFix (paramName par) [] (kernelResultSubExp res) []

    -- blockScan needs to treat the scan input as a one-dimensional array of
    -- scan elements, so we invent some new flattened arrays here.
    doScan virt scan_arrs = do
      dims_flat <- dPrimV "dims_flat" $ product dims'
      arrs_flat <- mapM (flattenArray (length dims') dims_flat) scan_arrs
      case virt of
        SegVirt ->
          virtualisedBlockScan
            seg_flag
            (sExt32 $ tvExp dims_flat)
            (segBinOpLambda scan)
            arrs_flat
        _ ->
          blockScan
            seg_flag
            (product dims')
            (product dims')
            (segBinOpLambda scan)
            arrs_flat

    applyPostOp virt scan_arrs map_dests
      | severalElemsPerThread virt =
          blockCoverSegSpace virt space postOp
      | otherwise =
          -- In this case the ltids from computeScanInputs are still in scope.
          sWhen (isActive $ zip ltids dims) $ localOps threadOperations postOp
      where
        ltids' = map Imp.le64 ltids
        res = fmap resSubExp $ bodyResult $ lambdaBody post_lam
        postOp = do
          sComment "bind scan results to post lambda params" $
            forM_ (zip scan_pars scan_arrs) $ \(par, arr) ->
              copyDWIMFix (paramName par) [] (Var arr) ltids'

          sComment "bind map results to post lambda params" $
            forM_ (zip map_pars map_dests) $ \(par, dest) ->
              forM_ dest $ \arr ->
                copyDWIMFix (paramName par) [] (Var arr) ltids'

          sComment "compute post op." $
            compileStms mempty (bodyStms $ lambdaBody post_lam) $
              sComment "write values" $
                forM_ (zip (patElems pat) res) $ \(pe, subexp) ->
                  copyDWIMFix (patElemName pe) ltids' subexp []

isEmptySpace :: SegSpace -> Imp.TExp Bool
isEmptySpace = (.==. 0) . product . map pe64 . segSpaceDims

-- | Compile a block-level 'SegRed'. Unless the index space is virtualised, the
-- kernel body is first run for the entire index space, with the results stashed
-- in shared memory, which is then reduced. A virtualised index space is instead
-- handled one block-sized chunk at a time, so that we need no more shared
-- memory than there are threads in the block.
compileBlockRed ::
  Pat LetDecMem ->
  SegLevel ->
  SegSpace ->
  KernelBody GPUMem ->
  [SegBinOp GPUMem] ->
  InKernelGen ()
compileBlockRed pat lvl space body ops = do
  compileFlatId space

  sIf (isEmptySpace space) emptyCase $ do
    virt <- blockCoverVirt (segVirt lvl) space
    case (virt, dims') of
      (SegVirt, [dim']) -> chunkedCase dim'
      (_, [dim']) -> stageWholeSpace (product dims') $ singleSegmentCase dim'
      _ -> stageWholeSpace (blockScanElems (product dims') red_ts) $ segmentedCase virt
  where
    (ltids, dims) = unzip $ unSegSpace space
    dims' = map pe64 dims
    (red_pes, map_pes) = splitAt (segBinOpResults ops) $ patElems pat
    (body_red_res, body_map_res) =
      splitAt (segBinOpResults ops) $ bodyResult body
    red_ts = concatMap (lambdaReturnType . segBinOpLambda) ops
    forOps = chunks $ map (length . segBinOpNeutral) ops

    -- Shared memory for @elems@ values per reduction operand, declared with
    -- the given shape.
    sharedArrays elems shape =
      forM red_ts $ \t ->
        sharedStagingArray "red_arr" t elems (shape <> arrayShape t)

    saveResults tmps_for_ops is =
      sComment "Save result of reduction." $
        forM_ (zip red_pes $ concat tmps_for_ops) $ \(pe, arr) ->
          copyDWIM (patElemName pe) [] (Var arr) is

    emptyCase = do
      -- This is the result if there are no chunks at all.
      sComment "set result to neutral element" $
        blockCoverSpace (map pe64 $ init dims) $ \is ->
          forM_ (zip red_pes $ concatMap segBinOpNeutral ops) $ \(pe, ne) ->
            copyDWIMFix (patElemName pe) is ne []

    -- Run the kernel body for the entire index space, staging the results in
    -- shared memory, then reduce them with the passed-in continuation.
    stageWholeSpace elems k = do
      tmp_arrs <- sharedArrays elems $ Shape dims
      blockCoverSegSpace (segVirt lvl) space $
        compileStms mempty (bodyStms body) $ do
          forM_ (zip tmp_arrs body_red_res) $ \(dest, res) ->
            copyDWIMFix dest (map Imp.le64 ltids) (kernelResultSubExp res) []
          zipWithM_ (compileThreadResult space) map_pes body_map_res
      sOp $ Imp.ErrorSync Imp.FenceLocal
      k $ forOps tmp_arrs

    -- A single segment, virtualised. We run the kernel body for one block-sized
    -- chunk at a time, reduce that chunk, and carry the result into the next
    -- chunk.
    chunkedCase dim' = do
      tblock_size <- kernelBlockSizeCount . kernelConstants <$> askEnv
      ltid <- kernelLocalThreadId . kernelConstants <$> askEnv
      -- A chunk is never larger than the block, nor than the index space
      -- itself, so that is all the shared memory we need.
      tmp_arrs <-
        sharedArrays (sMin64 dim' (pe64 (unCount tblock_size))) $
          Shape [unCount tblock_size]
      let tmps_for_ops = forOps tmp_arrs
          [ltid_name] = ltids

      blockChunkLoop (sExt32 dim') $ \chunk_start chunk_size -> do
        sComment "run the kernel body for this chunk" . localOps threadOperations $ do
          dPrimV_ ltid_name $ sExt64 chunk_start + sExt64 ltid
          sWhen (Imp.le64 ltid_name .<. dim') $
            compileStms mempty (bodyStms body) $ do
              forM_ (zip tmp_arrs body_red_res) $ \(dest, res) ->
                copyDWIMFix dest [sExt64 ltid] (kernelResultSubExp res) []
              zipWithM_ (compileThreadResult space) map_pes body_map_res

        sOp $ Imp.ErrorSync Imp.FenceLocal

        -- As in 'virtualisedBlockScan', the operator runs in a single thread
        -- and so must be compiled as thread-level code.
        sComment "possibly incorporate carry" . localOps threadOperations $
          sWhen (chunk_start .>. 0 .&&. ltid .==. 0) $
            forM_ (zip ops tmps_for_ops) $ \(op, tmps) ->
              applyRenamedLambda
                (segBinOpLambda op)
                (map (,[DimFix 0]) tmps)
                ( map ((,[]) . Var . patElemName) red_pes
                    ++ map ((,[DimFix 0]) . Var) tmps
                )

        sOp $ Imp.ErrorSync Imp.FenceLocal

        forM_ (zip ops tmps_for_ops) $ \(op, tmps) ->
          blockReduce (sExt32 (tvExp chunk_size)) (segBinOpLambda op) tmps

        sOp $ Imp.ErrorSync Imp.FenceLocal

        saveResults tmps_for_ops [DimFix 0]

        -- Everyone must be done reading the result before the next chunk
        -- overwrites it.
        sOp $ Imp.Barrier Imp.FenceLocal

    -- A single segment - this we can handle directly with a block-level
    -- reduction.
    singleSegmentCase dim' tmps_for_ops = do
      forM_ (zip ops tmps_for_ops) $ \(op, tmps) ->
        blockReduce (sExt32 dim') (segBinOpLambda op) tmps
      sOp $ Imp.ErrorSync Imp.FenceLocal
      saveResults tmps_for_ops [DimFix 0]
      sOp $ Imp.ErrorSync Imp.FenceLocal

    -- Multiple segments. These are turned into (regular) segmented scans, of
    -- which we then pick the last element of each segment. It is possible that
    -- this can be done better, but at least this approach is simple.
    segmentedCase virt tmps_for_ops = do
      -- The scans operate on flattened arrays. This does not involve copying
      -- anything; merely playing with the index function.
      dims_flat <- dPrimV "dims_flat" $ product dims'

      forM_ (zip ops tmps_for_ops) $ \(op, tmps) -> do
        tmps_flat <- mapM (flattenArray (length dims') dims_flat) tmps
        let seg_flag = Just $ crossesSegment $ last dims'
        case virt of
          SegVirt ->
            virtualisedBlockScan
              seg_flag
              (sExt32 $ tvExp dims_flat)
              (segBinOpLambda op)
              tmps_flat
          _ ->
            blockScan
              seg_flag
              (product dims')
              (product dims')
              (segBinOpLambda op)
              tmps_flat

      sOp $ Imp.ErrorSync Imp.FenceLocal

      saveResults tmps_for_ops lastInSegment

      sOp $ Imp.Barrier Imp.FenceLocal

    lastInSegment =
      map (unitSlice 0) (init dims') ++ [DimFix $ last dims' - 1]

compileBlockOp :: OpCompiler GPUMem KernelEnv Imp.KernelOp
compileBlockOp pat (Alloc size space) =
  blockAlloc pat size space
compileBlockOp pat (Inner (SegOp op)) = do
  beforeBlockWrite
  compileBlockSegOp pat op
compileBlockOp pat _ =
  compilerBugS $ "compileBlockOp: cannot compile rhs of binding " ++ prettyString pat

compileBlockSegOp :: Pat LetDecMem -> SegOp SegLevel GPUMem -> InKernelGen ()
compileBlockSegOp pat (SegMap lvl space _ body) = do
  compileFlatId space

  blockCoverSegSpace (segVirt lvl) space $
    compileStms mempty (bodyStms body) $
      zipWithM_ (compileThreadResult space) (patElems pat) $
        bodyResult body
  sOp $ Imp.ErrorSync Imp.FenceLocal
compileBlockSegOp pat (SegScan lvl space _ body scans post_op) =
  compileBlockScan pat lvl space body scans post_op
compileBlockSegOp pat (SegRed lvl space _ body ops) =
  compileBlockRed pat lvl space body ops
compileBlockSegOp pat (SegHist lvl space _ kbody ops) = do
  compileFlatId space
  let (ltids, dims) = unzip $ unSegSpace space

  -- We don't need the red_pes, because it is guaranteed by our type
  -- rules that they occupy the same memory as the destinations for
  -- the ops.
  let num_is_res = sum $ map (shapeRank . histShape) ops
      num_red_res = num_is_res + sum (map (length . histNeutral) ops)
      (_red_pes, map_pes) =
        splitAt num_red_res $ patElems pat

  tblock_size <- kernelBlockSizeCount . kernelConstants <$> askEnv
  ops' <- prepareIntraBlockSegHist (Shape $ init dims) tblock_size ops

  -- Ensure that all locks have been initialised.
  sOp $ Imp.Barrier Imp.FenceLocal

  blockCoverSegSpace (segVirt lvl) space $
    compileStms mempty (bodyStms kbody) $ do
      let (red_res, map_res) = splitAt num_red_res $ bodyResult kbody
          (red_is, red_vs) = splitAt num_is_res $ map kernelResultSubExp red_res
      zipWithM_ (compileThreadResult space) map_pes map_res

      let is_per_op = chunks (map (shapeRank . histShape) ops) red_is
          vs_per_op = chunks (map (length . histDest) ops) red_vs

      forM_ (zip4 is_per_op vs_per_op ops' ops) $
        \(bin, op_vs, do_op, HistOp dest_shape _ _ _ shape lam) -> do
          let bin' = map pe64 bin
              dest_shape' = map pe64 $ shapeDims dest_shape
              bin_in_bounds = inBounds (Slice $ map DimFix bin') dest_shape'
              bin_is = map Imp.le64 (init ltids) ++ bin'
              vs_params = takeLast (length op_vs) $ lambdaParams lam

          sComment "perform atomic updates" $
            sWhen bin_in_bounds $ do
              dLParams $ lambdaParams lam
              sLoopNest shape $ \is -> do
                forM_ (zip vs_params op_vs) $ \(p, v) ->
                  copyDWIMFix (paramName p) [] v is
                do_op (bin_is ++ is)

  sOp $ Imp.ErrorSync Imp.FenceLocal

-- | The operations for block-level kernels.
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

-- | Create a kernel with GPU operations at the block level.
sKernelBlock ::
  Name ->
  VName ->
  KernelAttrs ->
  InKernelGen () ->
  CallKernelGen ()
sKernelBlock = sKernel blockOperations $ sExt64 . kernelBlockId

-- | Generate code for writing this result of a block-level kernel. The
-- 'PatElem' and 'KernelResult' must be matched as in the original segop.
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
      else sFor "i" (n `ceilDiv` kernelBlockSize constants) $ \i -> do
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
      num_chunks <- dPrimVE "num_chunks" $ sExt32 $ n `ceilDiv` unCount tblock_size
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
