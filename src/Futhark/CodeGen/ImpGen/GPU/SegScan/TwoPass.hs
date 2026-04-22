{-# LANGUAGE TypeFamilies #-}

-- | Code generation for segmented and non-segmented scans.  Uses a
-- fairly inefficient two-pass algorithm, but can handle anything.
module Futhark.CodeGen.ImpGen.GPU.SegScan.TwoPass (compileSegScan) where

import Control.Monad
import Control.Monad.State
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
  Count BlockSize SubExp ->
  SubExp ->
  [SegBinOp GPUMem] ->
  InKernelGen [[VName]]
makeLocalArrays (Count tblock_size) num_threads scans = do
  (arrs, mems_and_sizes) <- runStateT (mapM onScan scans) mempty
  let maxSize sizes = Imp.bytes $ L.foldl' sMax64 1 $ map Imp.unCount sizes
  forM_ mems_and_sizes $ \(sizes, mem) ->
    sAlloc_ mem (maxSize sizes) (Space "shared")
  pure arrs
  where
    onScan (SegBinOp _ scan_op nes _) = do
      let (scan_x_params, _scan_y_params) =
            splitAt (length nes) $ lambdaParams scan_op
      (arrs, used_mems) <- fmap unzip $
        forM scan_x_params $ \p ->
          case paramDec p of
            MemArray pt shape _ (ArrayIn mem _) -> do
              let shape' = Shape [num_threads] <> shape
              arr <-
                lift . sArray "scan_arr" pt shape' mem $
                  LMAD.iota 0 (map pe64 $ shapeDims shape')
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

localArrayIndex :: KernelConstants -> Type -> Imp.TExp Int64
localArrayIndex constants t =
  if primType t
    then sExt64 (kernelLocalThreadId constants)
    else sExt64 (kernelGlobalThreadId constants)

barrierFor :: Lambda GPUMem -> (Bool, Imp.Fence, InKernelGen ())
barrierFor scan_op = (array_scan, fence, sOp $ Imp.Barrier fence)
  where
    array_scan = not $ all primType $ lambdaReturnType scan_op
    fence
      | array_scan = Imp.FenceGlobal
      | otherwise = Imp.FenceLocal

xParams, yParams :: SegBinOp GPUMem -> [LParam GPUMem]
xParams scan =
  take (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))
yParams scan =
  drop (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))

writeToScanValues ::
  [VName] ->
  ([VName], SegBinOp GPUMem, [KernelResult]) ->
  InKernelGen ()
writeToScanValues gtids (pes, scan, scan_res)
  | shapeRank (segBinOpShape scan) > 0 =
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
  SegBinOp GPUMem ->
  InKernelGen ()
readToScanValues is pes scan
  | shapeRank (segBinOpShape scan) > 0 =
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
  SegBinOp GPUMem ->
  InKernelGen ()
readCarries chunk_id chunk_offset dims' vec_is pes scan
  | shapeRank (segBinOpShape scan) > 0 = do
      ltid <- kernelLocalThreadId . kernelConstants <$> askEnv
      -- We may have to reload the carries from the output of the
      -- previous chunk.
      sIf
        (chunk_id .>. 0 .&&. ltid .==. 0)
        ( do
            let is = unflattenIndex dims' $ chunk_offset - 1
            forM_ (zip (xParams scan) pes) $ \(p, pe) ->
              copyDWIMFix (paramName p) [] (Var pe) (is ++ vec_is)
        )
        ( forM_ (zip (xParams scan) (segBinOpNeutral scan)) $ \(p, ne) ->
            copyDWIMFix (paramName p) [] ne []
        )
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
  [SegBinOp GPUMem] ->
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
    all_local_arrs <- makeLocalArrays tblock_size (tvSize num_threads) scans

    -- The variables from scan_op will be used for the carry and such
    -- in the big chunking loop.
    forM_ scans $ \scan -> do
      dScope Nothing $ scopeOfLParams $ lambdaParams $ segBinOpLambda scan
      forM_ (zip (xParams scan) (segBinOpNeutral scan)) $ \(p, ne) ->
        copyDWIMFix (paramName p) [] ne []

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

      let per_scan_pes = segBinOpChunks scans scan_out

          in_bounds =
            foldl1 (.&&.) $ zipWith (.<.) (map Imp.le64 gtids) dims'

          when_in_bounds =
            compileStms mempty (bodyStms kbody) $ do
              let (all_scan_res, map_res) =
                    splitAt (segBinOpResults scans) $ bodyResult kbody
                  per_scan_res =
                    segBinOpChunks scans all_scan_res

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

      unless (all (null . segBinOpShape) scans) $
        sOp $
          Imp.Barrier Imp.FenceGlobal

      forM_ (zip3 per_scan_pes scans all_local_arrs) $
        \(pes, scan@(SegBinOp _ scan_op nes vec_shape), local_arrs) ->
          sComment "do one intra-group scan operation" $ do
            let rets = lambdaReturnType scan_op
                scan_x_params = xParams scan
                (array_scan, fence, barrier) = barrierFor scan_op

            when array_scan barrier

            sLoopNest vec_shape $ \vec_is -> do
              sComment "maybe restore some to-scan values to parameters, or read neutral" $
                sIf
                  in_bounds
                  ( do
                      readToScanValues (map Imp.le64 gtids ++ vec_is) pes scan
                      readCarries j (tvExp chunk_offset) dims' vec_is pes scan
                  )
                  ( forM_ (zip (yParams scan) (segBinOpNeutral scan)) $ \(p, ne) ->
                      copyDWIMFix (paramName p) [] ne []
                  )

              sComment "combine with carry and write to shared memory" $
                compileStms mempty (bodyStms $ lambdaBody scan_op) $
                  forM_ (zip3 rets local_arrs $ map resSubExp $ bodyResult $ lambdaBody scan_op) $
                    \(t, arr, se) ->
                      copyDWIMFix arr [localArrayIndex constants t] se []

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
                (sExt64 $ kernelBlockSize constants)
                scan_op_renamed
                local_arrs

              sComment "threads in bounds write partial scan result" $
                sWhen in_bounds $
                  forM_ (zip3 rets pes local_arrs) $ \(t, pe, arr) ->
                    copyDWIMFix
                      pe
                      (map Imp.le64 gtids ++ vec_is)
                      (Var arr)
                      [localArrayIndex constants t]

              barrier

              let load_carry =
                    forM_ (zip local_arrs scan_x_params) $ \(arr, p) ->
                      copyDWIMFix
                        (paramName p)
                        []
                        (Var arr)
                        [ if primType $ paramType p
                            then sExt64 (kernelBlockSize constants) - 1
                            else
                              (sExt64 (kernelBlockId constants) + 1)
                                * sExt64 (kernelBlockSize constants)
                                - 1
                        ]
                  load_neutral =
                    forM_ (zip nes scan_x_params) $ \(ne, p) ->
                      copyDWIMFix (paramName p) [] ne []

              sComment "first thread reads last element as carry-in for next iteration" $ do
                crosses_segment <- dPrimVE "crosses_segment" $
                  case crossesSegment of
                    Nothing -> false
                    Just f ->
                      f
                        ( tvExp chunk_offset
                            + sExt64 (kernelBlockSize constants)
                            - 1
                        )
                        ( tvExp chunk_offset
                            + sExt64 (kernelBlockSize constants)
                        )
                should_load_carry <-
                  dPrimVE "should_load_carry" $
                    kernelLocalThreadId constants .==. 0 .&&. bNot crosses_segment
                sWhen should_load_carry load_carry
                when array_scan barrier
                sUnless should_load_carry load_neutral

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
  [SegBinOp GPUMem] ->
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
    per_scan_local_arrs <- makeLocalArrays stage2_tblock_size (tvSize stage1_num_threads) scans
    let per_scan_rets = map (lambdaReturnType . segBinOpLambda) scans
        per_scan_pes = segBinOpChunks scans scan_out

    -- Declare lambda params and initialise carries (xParams) to the
    -- neutral element.  For scalar scans these persist across chunk
    -- iterations as registers; for array scans they are reloaded from
    -- global memory at the start of each chunk.
    forM_ scans $ \scan -> do
      dScope Nothing $ scopeOfLParams $ lambdaParams $ segBinOpLambda scan
      forM_ (zip (xParams scan) (segBinOpNeutral scan)) $ \(p, ne) ->
        copyDWIMFix (paramName p) [] ne []

    sFor "chunk_id" num_chunks $ \chunk_id -> do
      let chunk_offset = chunk_id * stage2_tblock_size_e

      flat_idx <-
        dPrimV "flat_idx" $
          (chunk_offset + sExt64 (kernelLocalThreadId constants) + 1) * elems_per_group - 1
      -- Construct segment indices.
      zipWithM_ dPrimV_ gtids $ unflattenIndex dims' $ tvExp flat_idx

      forM_ (L.zip4 scans per_scan_local_arrs per_scan_rets per_scan_pes) $
        \(scan@(SegBinOp _ scan_op nes vec_shape), local_arrs, rets, pes) ->
          sComment "do one stage-2 scan chunk" $ do
            let (array_scan, fence, barrier) = barrierFor scan_op
                scan_x_params = xParams scan
                scan_y_params = yParams scan

            when array_scan barrier

            sLoopNest vec_shape $ \vec_is -> do
              let glob_is = map Imp.le64 gtids ++ vec_is
                  in_bounds =
                    foldl1 (.&&.) $ zipWith (.<.) (map Imp.le64 gtids) dims'
                  -- Element index of the last element of the previous chunk,
                  -- used to load the inter-chunk carry for array scans.
                  prev_chunk_last = chunk_offset * elems_per_group - 1

              -- For array scans, reload carry (xParams) from the scan
              -- output written by the previous chunk.  Thread 0 reads
              -- the last element of the previous chunk, unless a segment
              -- boundary falls between the chunks.
              when array_scan $ do
                crosses_seg <-
                  dPrimVE "crosses_seg" $
                    case crossesSegment of
                      Nothing -> false
                      Just f -> f prev_chunk_last (prev_chunk_last + 1)
                sIf
                  (chunk_id .>. 0 .&&. kernelLocalThreadId constants .==. 0 .&&. bNot crosses_seg)
                  ( do
                      let carry_is = unflattenIndex dims' prev_chunk_last
                      forM_ (zip scan_x_params pes) $ \(p, pe) ->
                        copyDWIMFix (paramName p) [] (Var pe) (carry_is ++ vec_is)
                  )
                  ( forM_ (zip scan_x_params nes) $ \(p, ne) ->
                      copyDWIMFix (paramName p) [] ne []
                  )

              -- Load the stage-1 partial-scan result for this thread's
              -- block into yParams (or the neutral element when out of
              -- bounds).
              sIf
                in_bounds
                ( forM_ (zip scan_y_params pes) $ \(p, pe) ->
                    copyDWIMFix (paramName p) [] (Var pe) glob_is
                )
                ( forM_ (zip scan_y_params nes) $ \(p, ne) ->
                    copyDWIMFix (paramName p) [] ne []
                )

              -- Combine carry (xParams) with new value (yParams) and
              -- write the result to shared/local memory.  Thread 0
              -- incorporates the inter-chunk carry; other threads have
              -- neutral in xParams, so the op is a no-op for them.
              compileStms mempty (bodyStms $ lambdaBody scan_op) $
                forM_ (zip3 rets local_arrs $ map resSubExp $ bodyResult $ lambdaBody scan_op) $
                  \(t, arr, se) ->
                    copyDWIMFix arr [localArrayIndex constants t] se []

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
                (sExt64 $ kernelBlockSize constants)
                scan_op_renamed
                local_arrs

              sComment "threads in bounds write scanned carries" $
                sWhen in_bounds $
                  forM_ (zip3 rets pes local_arrs) $ \(t, pe, arr) ->
                    copyDWIMFix
                      pe
                      glob_is
                      (Var arr)
                      [localArrayIndex constants t]

              barrier

              -- For scalar scans, update the carry register (xParams)
              -- so the next chunk can use it.  For array scans the carry
              -- is reloaded from global memory at the start of each chunk.
              unless array_scan $ do
                let next_chunk_start = chunk_offset + stage2_tblock_size_e
                crosses_seg2 <-
                  dPrimVE "crosses_seg" $
                    case crossesSegment of
                      Nothing -> false
                      Just f ->
                        f
                          (next_chunk_start * elems_per_group - 1)
                          (next_chunk_start * elems_per_group)
                let should_load_carry =
                      kernelLocalThreadId constants .==. 0 .&&. bNot crosses_seg2
                    load_carry =
                      forM_ (zip local_arrs scan_x_params) $ \(arr, p) ->
                        copyDWIMFix
                          (paramName p)
                          []
                          (Var arr)
                          [sExt64 (kernelBlockSize constants) - 1]
                    load_neutral =
                      forM_ (zip nes scan_x_params) $ \(ne, p) ->
                        copyDWIMFix (paramName p) [] ne []
                sWhen should_load_carry load_carry
                sUnless should_load_carry load_neutral

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
  [SegBinOp GPUMem] ->
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

      let per_scan_pes = segBinOpChunks scans scan_out
      sWhen in_bounds $ do
        sUnless no_carry_in $
          forM_ (zip per_scan_pes scans) $
            \(pes, SegBinOp _ scan_op nes vec_shape) -> do
              dScope Nothing $ scopeOfLParams $ lambdaParams scan_op
              let (scan_x_params, scan_y_params) =
                    splitAt (length nes) $ lambdaParams scan_op

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
            let (scan_pars, map_pars) = splitAt (segBinOpResults scans) $ lambdaParams $ segPostOpLambda post_op
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
  [SegBinOp GPUMem] ->
  KernelBody GPUMem ->
  SegPostOp GPUMem ->
  CallKernelGen ()
compileSegScan pat lvl space ts scans kbody post_op = do
  attrs <- lvlKernelAttrs lvl

  -- Stage 2 uses loop virtualization, so stage1_num_tblocks is no
  -- longer capped by the maximum thread block size.  Instead we query
  -- the maximum to use as the stage-2 block size.
  stage2_max_tblock_size <- dPrim "stage2_max_tblock_size"
  sOp $ Imp.GetSizeMax (tvVar stage2_max_tblock_size) SizeThreadBlock

  let stage1_num_tblocks = kAttrNumBlocks attrs

  stage2_tblock_size <-
    fmap (Imp.Count . tvSize) $
      dPrimV "stage2_tblock_size" $
        sMin64 (tvExp stage2_max_tblock_size) $
          pe64 . Imp.unCount $
            stage1_num_tblocks

  let shpT op = (segBinOpShape op,) <$> lambdaReturnType (segBinOpLambda op)
      scan_ts = concatMap shpT scans
      shpOfT t s =
        arrayShape $
          foldr (flip arrayOfRow) (arrayOfShape t s) $
            segSpaceDims space

  (scan_out, map_out) <-
    if isIdentityPostOp post_op
      then
        let (scan_out, map_out) = splitAt (segBinOpResults scans) $ patElemName <$> patElems pat
            map_out' =
              zipWith
                ( \t n ->
                    if isAcc t then Nothing else Just n
                )
                (drop (segBinOpResults scans) ts)
                map_out
         in pure (scan_out, map_out')
      else do
        scan_out <- forM scan_ts $ \(s, t) ->
          sAllocArray "scan_out" (elemType t) (shpOfT t s) (Space "device")

        map_out <- forM (drop (segBinOpResults scans) ts) $ \t ->
          if isAcc t
            then pure Nothing
            else Just <$> sAllocArray "map_out" (elemType t) (shpOfT t mempty) (Space "device")

        pure (scan_out, map_out)

  (stage1_num_threads, elems_per_group, crossesSegment) <-
    scanStage1 scan_out map_out stage1_num_tblocks (kAttrBlockSize attrs) space scans kbody

  emit $ Imp.DebugPrint "elems_per_group" $ Just $ untyped elems_per_group

  scanStage2 scan_out stage1_num_threads elems_per_group stage1_num_tblocks stage2_tblock_size crossesSegment space scans
  scanStage3 pat scan_out map_out (kAttrNumBlocks attrs) (kAttrBlockSize attrs) elems_per_group crossesSegment space scans post_op
