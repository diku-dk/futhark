{-# LANGUAGE TypeFamilies #-}

-- | Code generation for segmented and non-segmented scans.  Uses a
-- fairly inefficient two-pass algorithm, but can handle anything.
module Futhark.CodeGen.ImpGen.GPU.SegScan.TwoPass (compileSegScan) where

import Control.Monad
import Control.Monad.State
import Data.List (delete, find, foldl', zip4)
import Data.Maybe
import Futhark.CodeGen.ImpCode.GPU qualified as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.GPU.Base
import Futhark.IR.GPUMem
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.Transform.Rename
import Futhark.Util (takeLast)
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
  let maxSize sizes = Imp.bytes $ foldl' sMax64 1 $ map Imp.unCount sizes
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
      case (find ((size `elem`) . fst) mems, mems) of
        (Just mem, _) -> do
          modify $ delete mem
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
  ([PatElem LetDecMem], SegBinOp GPUMem, [KernelResult]) ->
  InKernelGen ()
writeToScanValues gtids (pes, scan, scan_res)
  | shapeRank (segBinOpShape scan) > 0 =
      forM_ (zip pes scan_res) $ \(pe, res) ->
        copyDWIMFix
          (patElemName pe)
          (map Imp.le64 gtids)
          (kernelResultSubExp res)
          []
  | otherwise =
      forM_ (zip (yParams scan) scan_res) $ \(p, res) ->
        copyDWIMFix (paramName p) [] (kernelResultSubExp res) []

readToScanValues ::
  [Imp.TExp Int64] ->
  [PatElem LetDecMem] ->
  SegBinOp GPUMem ->
  InKernelGen ()
readToScanValues is pes scan
  | shapeRank (segBinOpShape scan) > 0 =
      forM_ (zip (yParams scan) pes) $ \(p, pe) ->
        copyDWIMFix (paramName p) [] (Var (patElemName pe)) is
  | otherwise =
      pure ()

readCarries ::
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  [Imp.TExp Int64] ->
  [Imp.TExp Int64] ->
  [PatElem LetDecMem] ->
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
              copyDWIMFix (paramName p) [] (Var (patElemName pe)) (is ++ vec_is)
        )
        ( forM_ (zip (xParams scan) (segBinOpNeutral scan)) $ \(p, ne) ->
            copyDWIMFix (paramName p) [] ne []
        )
  | otherwise =
      pure ()

-- | Produce partially scanned intervals; one per threadblock.
scanStage1 ::
  Pat LetDecMem ->
  Count NumBlocks SubExp ->
  Count BlockSize SubExp ->
  SegSpace ->
  [SegBinOp GPUMem] ->
  KernelBody GPUMem ->
  CallKernelGen (TV Int32, Imp.TExp Int64, CrossesSegment)
scanStage1 (Pat all_pes) num_tblocks tblock_size space scans kbody = do
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

      let per_scan_pes = segBinOpChunks scans all_pes

          in_bounds =
            foldl1 (.&&.) $ zipWith (.<.) (map Imp.le64 gtids) dims'

          when_in_bounds = compileStms mempty (kernelBodyStms kbody) $ do
            let (all_scan_res, map_res) =
                  splitAt (segBinOpResults scans) $ kernelBodyResult kbody
                per_scan_res =
                  segBinOpChunks scans all_scan_res

            sComment "write to-scan values to parameters" $
              mapM_ (writeToScanValues gtids) $
                zip3 per_scan_pes scans per_scan_res

            sComment "write mapped values results to global memory" $
              forM_ (zip (takeLast (length map_res) all_pes) map_res) $ \(pe, se) ->
                copyDWIMFix
                  (patElemName pe)
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
                      (patElemName pe)
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
  Pat LetDecMem ->
  TV Int32 ->
  Imp.TExp Int64 ->
  Count NumBlocks SubExp ->
  CrossesSegment ->
  SegSpace ->
  [SegBinOp GPUMem] ->
  CallKernelGen ()
scanStage2 (Pat all_pes) stage1_num_threads elems_per_group num_tblocks crossesSegment space scans = do
  let (gtids, dims) = unzip $ unSegSpace space
      dims' = map pe64 dims

  -- Our group size is the number of groups for the stage 1 kernel.
  let tblock_size = Count $ unCount num_tblocks

  let crossesSegment' = do
        f <- crossesSegment
        Just $ \from to ->
          f
            ((sExt64 from + 1) * elems_per_group - 1)
            ((sExt64 to + 1) * elems_per_group - 1)

  sKernelThread "scan_stage2" (segFlat space) (defKernelAttrs (Count (intConst Int64 1)) tblock_size) $ do
    constants <- kernelConstants <$> askEnv
    per_scan_local_arrs <- makeLocalArrays tblock_size (tvSize stage1_num_threads) scans
    let per_scan_rets = map (lambdaReturnType . segBinOpLambda) scans
        per_scan_pes = segBinOpChunks scans all_pes

    flat_idx <-
      dPrimV "flat_idx" $
        (sExt64 (kernelLocalThreadId constants) + 1) * elems_per_group - 1
    -- Construct segment indices.
    zipWithM_ dPrimV_ gtids $ unflattenIndex dims' $ tvExp flat_idx

    forM_ (zip4 scans per_scan_local_arrs per_scan_rets per_scan_pes) $
      \(SegBinOp _ scan_op nes vec_shape, local_arrs, rets, pes) ->
        sLoopNest vec_shape $ \vec_is -> do
          let glob_is = map Imp.le64 gtids ++ vec_is

              in_bounds =
                foldl1 (.&&.) $ zipWith (.<.) (map Imp.le64 gtids) dims'

              when_in_bounds = forM_ (zip3 rets local_arrs pes) $ \(t, arr, pe) ->
                copyDWIMFix
                  arr
                  [localArrayIndex constants t]
                  (Var $ patElemName pe)
                  glob_is

              when_out_of_bounds = forM_ (zip3 rets local_arrs nes) $ \(t, arr, ne) ->
                copyDWIMFix arr [localArrayIndex constants t] ne []
              (_, _, barrier) =
                barrierFor scan_op

          sComment "threads in bound read carries; others get neutral element" $
            sIf in_bounds when_in_bounds when_out_of_bounds

          barrier

          blockScan
            crossesSegment'
            (sExt64 $ tvExp stage1_num_threads)
            (sExt64 $ kernelBlockSize constants)
            scan_op
            local_arrs

          sComment "threads in bounds write scanned carries" $
            sWhen in_bounds $
              forM_ (zip3 rets pes local_arrs) $ \(t, pe, arr) ->
                copyDWIMFix
                  (patElemName pe)
                  glob_is
                  (Var arr)
                  [localArrayIndex constants t]

scanStage3 ::
  Pat LetDecMem ->
  Count NumBlocks SubExp ->
  Count BlockSize SubExp ->
  Imp.TExp Int64 ->
  CrossesSegment ->
  SegSpace ->
  [SegBinOp GPUMem] ->
  CallKernelGen ()
scanStage3 (Pat all_pes) num_tblocks tblock_size elems_per_group crossesSegment space scans = do
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

      let per_scan_pes = segBinOpChunks scans all_pes
      sWhen in_bounds $
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
                    (Var $ patElemName pe)
                    (carry_in_idx ++ vec_is)

                forM_ (zip scan_y_params pes) $ \(p, pe) ->
                  copyDWIMFix
                    (paramName p)
                    []
                    (Var $ patElemName pe)
                    (map Imp.le64 gtids ++ vec_is)

                compileBody' scan_x_params $ lambdaBody scan_op

                forM_ (zip scan_x_params pes) $ \(p, pe) ->
                  copyDWIMFix
                    (patElemName pe)
                    (map Imp.le64 gtids ++ vec_is)
                    (Var $ paramName p)
                    []

-- | Compile 'SegScan' instance to host-level code with calls to
-- various kernels.
compileSegScan ::
  Pat LetDecMem ->
  SegLevel ->
  SegSpace ->
  [SegBinOp GPUMem] ->
  KernelBody GPUMem ->
  CallKernelGen ()
compileSegScan pat lvl space scans kbody = do
  attrs <- lvlKernelAttrs lvl

  -- Since stage 2 involves a group size equal to the number of groups
  -- used for stage 1, we have to cap this number to the maximum group
  -- size.
  stage1_max_num_tblocks <- dPrim "stage1_max_num_tblocks" int64
  sOp $ Imp.GetSizeMax (tvVar stage1_max_num_tblocks) SizeThreadBlock

  stage1_num_tblocks <-
    fmap (Imp.Count . tvSize) $
      dPrimV "stage1_num_tblocks" $
        sMin64 (tvExp stage1_max_num_tblocks) $
          pe64 . Imp.unCount . kAttrNumBlocks $
            attrs

  (stage1_num_threads, elems_per_group, crossesSegment) <-
    scanStage1 pat stage1_num_tblocks (kAttrBlockSize attrs) space scans kbody

  emit $ Imp.DebugPrint "elems_per_group" $ Just $ untyped elems_per_group

  scanStage2 pat stage1_num_threads elems_per_group stage1_num_tblocks crossesSegment space scans
  scanStage3 pat (kAttrNumBlocks attrs) (kAttrBlockSize attrs) elems_per_group crossesSegment space scans
