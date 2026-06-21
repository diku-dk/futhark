{-# LANGUAGE TypeFamilies #-}

-- | Code generation for segmented and non-segmented scans.  Uses a
-- fairly inefficient two-pass algorithm, but can handle anything.
module Futhark.CodeGen.ImpGen.GPU.SegScan.TwoPass (compileSegScan) where

import Control.Monad
import Control.Monad.State
import Data.List (zip4)
import Data.List qualified as L
import Data.Maybe
import Futhark.CodeGen.ImpCode.GPU qualified as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.GPU.Base
import Futhark.IR.GPUMem
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.Transform.Rename
import Futhark.Util.IntegralExp (divUp, quot, rem)
import Prelude hiding (quot, rem)

-- Aggressively try to reuse memory for different SegBinOps, because
-- we will run them sequentially after another.
makeLocalArrays ::
  Imp.TExp Int64 ->
  Count BlockSize SubExp ->
  [SegScanOp GPUMem] ->
  InKernelGen [[VName]]
makeLocalArrays tblock_id (Count tblock_size) scans = do
  (arrs, mems_and_sizes) <- runStateT (mapM onScan scans) mempty
  let maxSize sizes = Imp.bytes $ L.foldl' sMax64 1 $ map Imp.unCount sizes
  forM_ mems_and_sizes $ \(sizes, mem) ->
    sAlloc_ mem (maxSize sizes) (Space "shared")
  pure arrs
  where
    onScan (SegScanOp scan_op _) = do
      let (scan_x_params, _scan_y_params) =
            splitAt (length (lambdaReturnType scan_op)) $ lambdaParams scan_op
      (arrs, used_mems) <- fmap unzip . forM scan_x_params $ \p ->
        case paramDec p of
          MemArray pt shape _ (ArrayIn mem _) -> do
            let shape' = Shape [tblock_size] <> shape
            let shape_E = map pe64 $ shapeDims shape'
            arr <-
              lift . sArray "scan_arr" pt shape' mem $
                LMAD.iota (tblock_id * product shape_E) shape_E
            pure (arr, [])
          _ -> do
            let pt = elemType $ paramType p
                shape = Shape [tblock_size]
            (sizes, mem') <- getMem pt shape
            arr <- lift $ sArrayInMem "scan_arr" pt shape mem'
            pure (arr, [(sizes, mem')])
      modify (<> concat used_mems)
      pure arrs

    getMem pt shape = do
      let size = typeSize $ Array pt shape NoUniqueness
      mems <- get
      case (L.find ((size `elem`) . fst) mems, mems) of
        (Just mem, _) -> do
          modify $ L.delete mem
          pure mem
        (Nothing, (size', mem) : mems') -> do
          put mems'
          pure (size : size', mem)
        (Nothing, []) -> do
          mem <- lift $ sDeclareMem "scan_arr_mem" $ Space "shared"
          pure ([size], mem)

type CrossesSegment = Maybe (Imp.TExp Int64 -> Imp.TExp Int64 -> Imp.TExp Bool)

localArrayIndex :: KernelConstants -> Imp.TExp Int64
localArrayIndex constants = sExt64 (kernelLocalThreadId constants)

barrierFor :: Lambda GPUMem -> (Bool, Imp.Fence, InKernelGen ())
barrierFor scan_op = (array_scan, fence, sOp $ Imp.Barrier fence)
  where
    array_scan = not $ all primType $ lambdaReturnType scan_op
    fence
      | array_scan = Imp.FenceGlobal
      | otherwise = Imp.FenceLocal

xParams, yParams :: SegScanOp GPUMem -> [LParam GPUMem]
xParams scan =
  take (length (lambdaReturnType (segScanOpLambda scan))) (lambdaParams (segScanOpLambda scan))
yParams scan =
  drop (length (lambdaReturnType (segScanOpLambda scan))) (lambdaParams (segScanOpLambda scan))

writeToScanValues ::
  [VName] ->
  ([VName], SegScanOp GPUMem, [KernelResult]) ->
  InKernelGen ()
writeToScanValues gtids (pes, scan, scan_res)
  | shapeRank (segScanOpShape scan) > 0 =
      forM_ (zip pes scan_res) $ \(pe, res) ->
        copyDWIMFix
          pe
          (map Imp.le64 gtids)
          (kernelResultSubExp res)
          []
  | otherwise =
      forM_ (zip (yParams scan) scan_res) $ \(p, res) ->
        copyDWIMFix (paramName p) [] (kernelResultSubExp res) []

readToScanValues ::
  [Imp.TExp Int64] ->
  [VName] ->
  SegScanOp GPUMem ->
  InKernelGen ()
readToScanValues is pes scan
  | shapeRank (segScanOpShape scan) > 0 =
      forM_ (zip (yParams scan) pes) $ \(p, pe) ->
        copyDWIMFix (paramName p) [] (Var pe) is
  | otherwise =
      pure ()

readCarries ::
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  [Imp.TExp Int64] ->
  [Imp.TExp Int64] ->
  [VName] ->
  SegScanOp GPUMem ->
  TV Bool ->
  InKernelGen ()
readCarries chunk_id chunk_offset dims' vec_is pes scan has_carry
  | shapeRank (segScanOpShape scan) > 0 = do
      ltid <- kernelLocalThreadId . kernelConstants <$> askEnv
      -- For each vec-loop iteration, thread 0 either reloads carry from
      -- the global output of the previous chunk (when j > 0) or resets
      -- has_carry to False.  This prevents the k=0 carry from bleeding
      -- into k=1 via the per-j carry-update.
      sWhen (ltid .==. 0) $
        sIf
          (chunk_id .>. 0)
          ( do
              let is = unflattenIndex dims' $ chunk_offset - 1
              forM_ (zip (xParams scan) pes) $ \(p, pe) ->
                copyDWIMFix (paramName p) [] (Var pe) (is ++ vec_is)
              has_carry <-- fromBool True
          )
          (has_carry <-- fromBool False)
  | otherwise =
      pure ()

-- | Produce partially scanned intervals; one per threadblock.
--
-- The @Maybe VName@ for the result is to cater to accumulators, which become
-- @Nothing@.
scanStage1 ::
  [VName] ->
  [Maybe VName] ->
  Count NumBlocks SubExp ->
  Count BlockSize SubExp ->
  SegSpace ->
  [SegScanOp GPUMem] ->
  KernelBody GPUMem ->
  CallKernelGen (TV Int32, Imp.TExp Int64, CrossesSegment)
scanStage1 scan_out map_out num_tblocks tblock_size space scans kbody = do
  let num_tblocks' = fmap pe64 num_tblocks
      tblock_size' = fmap pe64 tblock_size
  num_threads <- dPrimV "num_threads" $ sExt32 $ unCount num_tblocks' * unCount tblock_size'

  let (gtids, dims) = unzip $ unSegSpace space
      dims' = map pe64 dims
  let num_elements = product dims'
      elems_per_thread = num_elements `divUp` sExt64 (tvExp num_threads)
      elems_per_group = unCount tblock_size' * elems_per_thread

  let crossesSegment =
        case reverse dims' of
          segment_size : _ : _ -> Just $ \from to ->
            (to - from) .>. (to `rem` segment_size)
          _ -> Nothing

  sKernelThread "scan_stage1" (segFlat space) (defKernelAttrs num_tblocks tblock_size) $ do
    constants <- kernelConstants <$> askEnv
    let tblock_id = kernelBlockId constants
    all_local_arrs <- makeLocalArrays (sExt64 tblock_id) tblock_size scans

    -- Declare lambda params; has_carry tracks whether thread 0 holds a
    -- valid carry-in (avoids using neutral elements).
    all_has_carry <- forM scans $ \scan -> do
      dScope Nothing $ scopeOfLParams $ lambdaParams $ segScanOpLambda scan
      dPrimV "has_carry" $ fromBool False

    sFor "j" elems_per_thread $ \j -> do
      chunk_offset <-
        dPrimV "chunk_offset" $
          sExt64 (kernelBlockSize constants) * j
            + sExt64 (kernelBlockId constants) * elems_per_group
      flat_idx <-
        dPrimV "flat_idx" $
          tvExp chunk_offset + sExt64 (kernelLocalThreadId constants)
      -- Construct segment indices.
      zipWithM_ dPrimV_ gtids $ unflattenIndex dims' $ tvExp flat_idx

      let per_scan_pes = segScanOpChunks scans scan_out

          in_bounds =
            foldl1 (.&&.) $ zipWith (.<.) (map Imp.le64 gtids) dims'

          when_in_bounds =
            compileStms mempty (bodyStms kbody) $ do
              let (all_scan_res, map_res) =
                    splitAt (segScanOpResults scans) $ bodyResult kbody
                  per_scan_res =
                    segScanOpChunks scans all_scan_res

              sComment "write to-scan values to parameters" $
                mapM_ (writeToScanValues gtids) $
                  zip3 per_scan_pes scans per_scan_res

              sComment "write mapped values results to global memory" $
                forM_ (zip map_out map_res) $ \(pe, se) ->
                  forM pe $ \p ->
                    copyDWIMFix
                      p
                      (map Imp.le64 gtids)
                      (kernelResultSubExp se)
                      []

      sComment "threads in bounds read input" $
        sWhen in_bounds when_in_bounds

      unless (all (null . segScanOpShape) scans) $
        sOp $
          Imp.Barrier Imp.FenceGlobal

      -- Number of in-bounds threads for this chunk: at most blockSize,
      -- but clamped to the remaining elements.
      active_w <-
        dPrimVE "active_w" $
          sMin64
            (sExt64 $ kernelBlockSize constants)
            (sMax64 0 $ num_elements - tvExp chunk_offset)

      forM_ (zip4 per_scan_pes scans all_local_arrs all_has_carry) $
        \(pes, scan@(SegScanOp scan_op vec_shape), local_arrs, has_carry) ->
          sComment "do one intra-group scan operation" $ do
            let scan_x_params = xParams scan
                (array_scan, fence, barrier) = barrierFor scan_op

            when array_scan barrier

            sLoopNest vec_shape $ \vec_is -> do
              -- For in-bounds threads: load the to-scan value into yParams and
              -- reload carry (xParams) for array scans.  For thread 0 with a
              -- valid carry, run scan_op(carry, y); for all other in-bounds
              -- threads (and thread 0 without a carry) write yParams directly.
              -- Out-of-bounds threads do not write to shared memory at all.
              sWhen in_bounds $ do
                readToScanValues (map Imp.le64 gtids ++ vec_is) pes scan
                readCarries j (tvExp chunk_offset) dims' vec_is pes scan has_carry
                sIf
                  (kernelLocalThreadId constants .==. 0 .&&. tvExp has_carry)
                  ( sComment "combine carry with value and write to shared memory" $
                      compileStms mempty (bodyStms $ lambdaBody scan_op) $
                        forM_ (zip local_arrs $ map resSubExp $ bodyResult $ lambdaBody scan_op) $
                          \(arr, se) ->
                            copyDWIMFix arr [localArrayIndex constants] se []
                  )
                  ( sComment "write value directly to shared memory (no carry)" $
                      forM_ (zip local_arrs $ yParams scan) $ \(arr, p) ->
                        copyDWIMFix arr [localArrayIndex constants] (Var $ paramName p) []
                  )

              let crossesSegment' = do
                    f <- crossesSegment
                    Just $ \from to ->
                      let from' = sExt64 from + tvExp chunk_offset
                          to' = sExt64 to + tvExp chunk_offset
                       in f from' to'

              sOp $ Imp.ErrorSync fence

              -- We need to avoid parameter name clashes.
              scan_op_renamed <- renameLambda scan_op
              blockScan
                crossesSegment'
                (sExt64 $ tvExp num_threads)
                active_w
                scan_op_renamed
                local_arrs

              sComment "threads in bounds write partial scan result" $
                sWhen in_bounds $
                  forM_ (zip pes local_arrs) $ \(pe, arr) ->
                    copyDWIMFix
                      pe
                      (map Imp.le64 gtids ++ vec_is)
                      (Var arr)
                      [localArrayIndex constants]

              barrier

              -- Thread 0 reads the last active element as carry-in for the
              -- next iteration, unless a segment boundary falls at the end of
              -- this chunk (in which case it has no carry for the next chunk).
              sComment "first thread reads last element as carry-in for next iteration" $ do
                crosses_segment <- dPrimVE "crosses_segment" $
                  case crossesSegment of
                    Nothing -> false
                    Just f ->
                      f
                        ( tvExp chunk_offset
                            + active_w
                            - 1
                        )
                        ( tvExp chunk_offset
                            + active_w
                        )
                sWhen (kernelLocalThreadId constants .==. 0 .&&. active_w .>. 0) $ do
                  sIf
                    crosses_segment
                    (has_carry <-- fromBool False)
                    ( do
                        forM_ (zip local_arrs scan_x_params) $ \(arr, p) ->
                          copyDWIMFix
                            (paramName p)
                            []
                            (Var arr)
                            [active_w - 1]
                        has_carry <-- fromBool True
                    )
                when array_scan barrier

              barrier

  pure (num_threads, elems_per_group, crossesSegment)

scanStage2 ::
  [VName] ->
  TV Int32 ->
  Imp.TExp Int64 ->
  Count NumBlocks SubExp ->
  Count BlockSize SubExp ->
  CrossesSegment ->
  SegSpace ->
  [SegScanOp GPUMem] ->
  CallKernelGen ()
scanStage2 scan_out stage1_num_threads elems_per_group stage1_num_tblocks stage2_tblock_size crossesSegment space scans = do
  let (gtids, dims) = unzip $ unSegSpace space
      dims' = map pe64 dims
      stage1_num_tblocks_e = pe64 $ unCount stage1_num_tblocks
      stage2_tblock_size_e = pe64 $ unCount stage2_tblock_size

  -- Number of chunks needed to cover all stage-1 blocks.
  num_chunks <-
    dPrimVE "stage2_num_chunks" $
      stage1_num_tblocks_e `divUp` stage2_tblock_size_e

  sKernelThread "scan_stage2" (segFlat space) (defKernelAttrs (Count (intConst Int64 1)) stage2_tblock_size) $ do
    constants <- kernelConstants <$> askEnv
    let tblock_id = kernelBlockId constants
    per_scan_local_arrs <- makeLocalArrays (sExt64 tblock_id) stage2_tblock_size scans
    let per_scan_pes = segScanOpChunks scans scan_out

    -- Declare lambda params; has_carry tracks whether thread 0 holds a
    -- valid carry-in (avoids using neutral elements).
    all_has_carry <- forM scans $ \scan -> do
      dScope Nothing $ scopeOfLParams $ lambdaParams $ segScanOpLambda scan
      dPrimV "has_carry" $ fromBool False

    sFor "chunk_id" num_chunks $ \chunk_id -> do
      let chunk_offset = chunk_id * stage2_tblock_size_e

      flat_idx <-
        dPrimV "flat_idx" $
          (chunk_offset + sExt64 (kernelLocalThreadId constants) + 1) * elems_per_group - 1
      -- Construct segment indices.
      zipWithM_ dPrimV_ gtids $ unflattenIndex dims' $ tvExp flat_idx

      -- Number of in-bounds threads: thread t is in-bounds iff
      -- (chunk_offset + t + 1) * elems_per_group - 1 < num_elements,
      -- i.e. t < floor(num_elements / elems_per_group) - chunk_offset.
      -- The partial last stage-1 block, if any, has its last element
      -- outside num_elements and is therefore excluded from active_w;
      -- its carry is not needed by stage 3.
      let num_complete_groups = product dims' `quot` elems_per_group
      active_w <-
        dPrimVE "active_w" $
          sMin64
            (sExt64 $ kernelBlockSize constants)
            (sMax64 0 $ num_complete_groups - chunk_offset)

      forM_ (zip4 scans per_scan_local_arrs per_scan_pes all_has_carry) $
        \(scan@(SegScanOp scan_op vec_shape), local_arrs, pes, has_carry) ->
          sComment "do one stage-2 scan chunk" $ do
            let (array_scan, fence, barrier) = barrierFor scan_op
                scan_x_params = xParams scan
                scan_y_params = yParams scan
                -- Scalar scans with a non-trivial vec_shape need per-vec-element
                -- carries reloaded from global memory, just like array scans.
                -- The scalar carry register cannot hold independent carries for
                -- each vector element across chunk iterations.
                use_global_carry = array_scan || not (null (shapeDims vec_shape))

            when use_global_carry (sOp $ Imp.Barrier Imp.FenceGlobal)

            sLoopNest vec_shape $ \vec_is -> do
              let glob_is = map Imp.le64 gtids ++ vec_is
                  in_bounds =
                    foldl1 (.&&.) $ zipWith (.<.) (map Imp.le64 gtids) dims'
                  -- Element index of the last element of the previous chunk,
                  -- used to load the inter-chunk carry from global memory.
                  prev_chunk_last = chunk_offset * elems_per_group - 1

              -- For array scans and scalar scans with non-trivial vec_shape,
              -- reload carry (xParams) from the scan output written by the
              -- previous chunk.  Thread 0 reads the last element of the
              -- previous chunk if chunk_id > 0 and no segment boundary falls
              -- between chunks; otherwise thread 0 has no carry.
              when use_global_carry $ do
                crosses_seg <-
                  dPrimVE "crosses_seg" $
                    case crossesSegment of
                      Nothing -> false
                      Just f -> f prev_chunk_last (prev_chunk_last + 1)
                sWhen (kernelLocalThreadId constants .==. 0) $
                  sIf
                    (chunk_id .>. 0 .&&. bNot crosses_seg)
                    ( do
                        let carry_is = unflattenIndex dims' prev_chunk_last
                        forM_ (zip scan_x_params pes) $ \(p, pe) ->
                          copyDWIMFix (paramName p) [] (Var pe) (carry_is ++ vec_is)
                        has_carry <-- fromBool True
                    )
                    (has_carry <-- fromBool False)

              -- Load the stage-1 partial-scan result for this thread's block
              -- into yParams.  Out-of-bounds threads do not write to shared memory.
              sWhen in_bounds $
                forM_ (zip scan_y_params pes) $ \(p, pe) ->
                  copyDWIMFix (paramName p) [] (Var pe) glob_is

              -- Combine carry (xParams) with value (yParams) for thread 0
              -- when it has a valid carry; all other in-bounds threads write
              -- yParams directly.  Out-of-bounds threads do not write.
              sWhen in_bounds $
                sIf
                  (kernelLocalThreadId constants .==. 0 .&&. tvExp has_carry)
                  ( compileStms mempty (bodyStms $ lambdaBody scan_op) $
                      forM_ (zip local_arrs $ map resSubExp $ bodyResult $ lambdaBody scan_op) $
                        \(arr, se) ->
                          copyDWIMFix arr [localArrayIndex constants] se []
                  )
                  ( forM_ (zip local_arrs scan_y_params) $ \(arr, p) ->
                      copyDWIMFix arr [localArrayIndex constants] (Var $ paramName p) []
                  )

              sOp $ Imp.ErrorSync fence

              -- crossesSegment' maps block-local thread IDs to element
              -- indices, adjusting for the current chunk offset.
              let crossesSegment' =
                    crossesSegment >>= \f ->
                      Just $ \from to ->
                        f
                          ((chunk_offset + sExt64 from + 1) * elems_per_group - 1)
                          ((chunk_offset + sExt64 to + 1) * elems_per_group - 1)

              scan_op_renamed <- renameLambda scan_op
              blockScan
                crossesSegment'
                (sExt64 $ tvExp stage1_num_threads)
                active_w
                scan_op_renamed
                local_arrs

              sComment "threads in bounds write scanned carries" $
                sWhen in_bounds $
                  forM_ (zip pes local_arrs) $ \(pe, arr) ->
                    copyDWIMFix
                      pe
                      glob_is
                      (Var arr)
                      [localArrayIndex constants]

              barrier

              -- For scalar scans with trivial vec_shape (no vec loop), update
              -- the carry register (xParams) so the next chunk can use it.
              -- For array scans and scalar scans with non-trivial vec_shape,
              -- the carry is reloaded from global memory at the start of each
              -- chunk (above), so no register update is needed here.
              unless use_global_carry $ do
                let next_chunk_start = chunk_offset + stage2_tblock_size_e
                crosses_seg2 <-
                  dPrimVE "crosses_seg" $
                    case crossesSegment of
                      Nothing -> false
                      Just f ->
                        f
                          (next_chunk_start * elems_per_group - 1)
                          ((next_chunk_start + 1) * elems_per_group - 1)
                sWhen (kernelLocalThreadId constants .==. 0) $
                  sIf
                    (bNot crosses_seg2 .&&. active_w .>. 0)
                    ( do
                        forM_ (zip local_arrs scan_x_params) $ \(arr, p) ->
                          copyDWIMFix
                            (paramName p)
                            []
                            (Var arr)
                            [active_w - 1]
                        has_carry <-- fromBool True
                    )
                    (has_carry <-- fromBool False)

              barrier

scanStage3 ::
  Pat LetDecMem ->
  [VName] ->
  [Maybe VName] ->
  Count NumBlocks SubExp ->
  Count BlockSize SubExp ->
  Imp.TExp Int64 ->
  CrossesSegment ->
  SegSpace ->
  [SegScanOp GPUMem] ->
  SegPostOp GPUMem ->
  CallKernelGen ()
scanStage3 pat scan_out map_out num_tblocks tblock_size elems_per_group crossesSegment space scans post_op = do
  let tblock_size' = fmap pe64 tblock_size
      (gtids, dims) = unzip $ unSegSpace space
      dims' = map pe64 dims
  required_groups <-
    dPrimVE "required_groups" $
      sExt32 $
        product dims' `divUp` sExt64 (unCount tblock_size')

  sKernelThread "scan_stage3" (segFlat space) (defKernelAttrs num_tblocks tblock_size) $
    virtualiseBlocks SegVirt required_groups $ \virt_tblock_id -> do
      constants <- kernelConstants <$> askEnv

      -- Compute our logical index.
      flat_idx <-
        dPrimVE "flat_idx" $
          sExt64 virt_tblock_id * sExt64 (unCount tblock_size')
            + sExt64 (kernelLocalThreadId constants)
      zipWithM_ dPrimV_ gtids $ unflattenIndex dims' flat_idx

      -- Figure out which group this element was originally in.
      orig_group <- dPrimV "orig_group" $ flat_idx `quot` elems_per_group
      -- Then the index of the carry-in of the preceding group.
      carry_in_flat_idx <-
        dPrimV "carry_in_flat_idx" $
          tvExp orig_group * elems_per_group - 1
      -- Figure out the logical index of the carry-in.
      let carry_in_idx = unflattenIndex dims' $ tvExp carry_in_flat_idx

      -- Apply the carry if we are not in the scan results for the first
      -- group, and are not the last element in such a group (because
      -- then the carry was updated in stage 2), and we are not crossing
      -- a segment boundary.
      let in_bounds =
            foldl1 (.&&.) $ zipWith (.<.) (map Imp.le64 gtids) dims'
          crosses_segment =
            fromMaybe false $
              crossesSegment
                <*> pure (tvExp carry_in_flat_idx)
                <*> pure flat_idx
          is_a_carry = flat_idx .==. (tvExp orig_group + 1) * elems_per_group - 1
          no_carry_in = tvExp orig_group .==. 0 .||. is_a_carry .||. crosses_segment

      let per_scan_pes = segScanOpChunks scans scan_out
      sWhen in_bounds $ do
        sUnless no_carry_in $
          forM_ (zip per_scan_pes scans) $
            \(pes, SegScanOp scan_op vec_shape) -> do
              dScope Nothing $ scopeOfLParams $ lambdaParams scan_op
              let (scan_x_params, scan_y_params) =
                    splitAt (length (lambdaReturnType scan_op)) $ lambdaParams scan_op

              sLoopNest vec_shape $ \vec_is -> do
                forM_ (zip scan_x_params pes) $ \(p, pe) ->
                  copyDWIMFix
                    (paramName p)
                    []
                    (Var pe)
                    (carry_in_idx ++ vec_is)

                forM_ (zip scan_y_params pes) $ \(p, pe) ->
                  copyDWIMFix
                    (paramName p)
                    []
                    (Var pe)
                    (map Imp.le64 gtids ++ vec_is)

                compileBody' scan_x_params $ lambdaBody scan_op

                forM_ (zip scan_x_params pes) $ \(p, pe) ->
                  copyDWIMFix
                    pe
                    (map Imp.le64 gtids ++ vec_is)
                    (Var $ paramName p)
                    []

      sOp $ Imp.Barrier Imp.FenceLocal

      if isIdentityPostOp post_op
        then pure ()
        else do
          sWhen in_bounds $ do
            let (scan_pars, map_pars) = splitAt (segScanOpResults scans) $ lambdaParams $ segPostOpLambda post_op
            dScope Nothing $
              scopeOfLParams $
                lambdaParams $
                  segPostOpLambda post_op

            sComment "bind scan results to post lambda params" $ do
              forM_ (zip scan_pars scan_out) $ \(par, acc) ->
                copyDWIMFix (paramName par) [] (Var acc) (map Imp.le64 gtids)

            sComment "bind map results to post lamda params" $
              forM_ (zip map_pars map_out) $ \(par, out) -> do
                maybe
                  (pure ())
                  ( \o ->
                      copyDWIMFix (paramName par) [] (Var o) (map Imp.le64 gtids)
                  )
                  out

            let res = fmap resSubExp $ bodyResult $ lambdaBody $ segPostOpLambda post_op
            sComment "compute post op." $
              compileStms mempty (bodyStms $ lambdaBody $ segPostOpLambda post_op) $
                sComment "write values" $
                  forM_ (zip (patElems pat) res) $ \(pe, subexp) ->
                    copyDWIMFix (patElemName pe) (map Imp.le64 gtids) subexp []

      sOp $ Imp.Barrier Imp.FenceGlobal

isIdentityPostOp :: SegPostOp rep -> Bool
isIdentityPostOp post_op =
  map resSubExp (bodyResult (lambdaBody lam))
    == map (Var . paramName) (lambdaParams lam)
  where
    lam = segPostOpLambda post_op

-- | Compile 'SegScan' instance to host-level code with calls to
-- various kernels.
compileSegScan ::
  Pat LetDecMem ->
  SegLevel ->
  SegSpace ->
  [Type] ->
  [SegScanOp GPUMem] ->
  KernelBody GPUMem ->
  SegPostOp GPUMem ->
  CallKernelGen ()
compileSegScan pat lvl space ts scans kbody post_op = do
  attrs <- lvlKernelAttrs lvl

  -- Stage 2 uses loop virtualization, so stage1_num_tblocks is no
  -- longer capped by the maximum thread block size.
  let stage1_num_tblocks = kAttrNumBlocks attrs

  -- The stage-2 block size is a tunable/user-settable parameter.  It
  -- is independent of stage1_num_tblocks so the autotuner can treat
  -- it as a fixed knob.
  stage2_tblock_size_param <- getSize "segscan_stage2_tblock_size" SizeThreadBlock
  stage2_tblock_size <-
    fmap (Imp.Count . tvSize) $
      dPrimV "stage2_tblock_size" $
        tvExp stage2_tblock_size_param

  let shpT op = (segScanOpShape op,) <$> lambdaReturnType (segScanOpLambda op)
      scan_ts = concatMap shpT scans
      shpOfT t s =
        arrayShape $
          foldr (flip arrayOfRow) (arrayOfShape t s) $
            segSpaceDims space

  (scan_out, map_out) <-
    if isIdentityPostOp post_op
      then
        let (scan_out, map_out) = splitAt (segScanOpResults scans) $ patElemName <$> patElems pat
            map_out' =
              zipWith
                ( \t n ->
                    if isAcc t then Nothing else Just n
                )
                (drop (segScanOpResults scans) ts)
                map_out
         in pure (scan_out, map_out')
      else do
        scan_out <- forM scan_ts $ \(s, t) ->
          sAllocArray "scan_out" (elemType t) (shpOfT t s) (Space "device")

        map_out <- forM (drop (segScanOpResults scans) ts) $ \t ->
          if isAcc t
            then pure Nothing
            else Just <$> sAllocArray "map_out" (elemType t) (shpOfT t mempty) (Space "device")

        pure (scan_out, map_out)

  (stage1_num_threads, elems_per_group, crossesSegment) <-
    scanStage1 scan_out map_out stage1_num_tblocks (kAttrBlockSize attrs) space scans kbody

  emit $ Imp.DebugPrint "elems_per_group" $ Just $ untyped elems_per_group

  scanStage2 scan_out stage1_num_threads elems_per_group stage1_num_tblocks stage2_tblock_size crossesSegment space scans
  scanStage3 pat scan_out map_out (kAttrNumBlocks attrs) (kAttrBlockSize attrs) elems_per_group crossesSegment space scans post_op
