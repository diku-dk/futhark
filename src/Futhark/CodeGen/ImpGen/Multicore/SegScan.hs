module Futhark.CodeGen.ImpGen.Multicore.SegScan
  ( compileSegScan,
  )
where

import Control.Monad
import Data.List (zip4)
import qualified Futhark.CodeGen.ImpCode.Multicore as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.IR.MCMem
import Futhark.Util.IntegralExp (quot, rem)
import Prelude hiding (quot, rem)

-- Compile a SegScan construct
compileSegScan ::
  Pat MCMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  TV Int32 ->
  MulticoreGen Imp.Code
compileSegScan pat space reds kbody nsubtasks
  | [_] <- unSegSpace space =
    nonsegmentedScan pat space reds kbody nsubtasks
  | otherwise =
    segmentedScan pat space reds kbody

xParams, yParams :: SegBinOp MCMem -> [LParam MCMem]
xParams scan =
  take (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))
yParams scan =
  drop (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))

lamBody :: SegBinOp MCMem -> Body MCMem
lamBody = lambdaBody . segBinOpLambda

-- Arrays for storing worker results.
resultArrays :: String -> [SegBinOp MCMem] -> MulticoreGen [[VName]]
resultArrays s segops =
  forM segops $ \(SegBinOp _ lam _ shape) ->
    forM (lambdaReturnType lam) $ \t -> do
      let pt = elemType t
          full_shape = shape <> arrayShape t
      sAllocArray s pt full_shape DefaultSpace

nonsegmentedScan ::
  Pat MCMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  TV Int32 ->
  MulticoreGen Imp.Code
nonsegmentedScan pat space scan_ops kbody nsubtasks = do
  emit $ Imp.DebugPrint "nonsegmented segScan" Nothing
  collect $ do
    scanStage1 pat space scan_ops kbody

    let nsubtasks' = tvExp nsubtasks
    sWhen (nsubtasks' .>. 1) $ do
      scan_ops2 <- renameSegBinOp scan_ops
      scanStage2 pat nsubtasks space scan_ops2 kbody
      scan_ops3 <- renameSegBinOp scan_ops
      scanStage3 pat space scan_ops3 kbody

scanStage1 ::
  Pat MCMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  MulticoreGen ()
scanStage1 pat space scan_ops kbody = do
  let (all_scan_res, map_res) = splitAt (segBinOpResults scan_ops) $ kernelBodyResult kbody
      per_scan_res = segBinOpChunks scan_ops all_scan_res
      per_scan_pes = segBinOpChunks scan_ops $ patElems pat
  let (is, ns) = unzip $ unSegSpace space
      ns' = map toInt64Exp ns

  -- Stage 1 : each thread partially scans a chunk of the input
  -- Writes directly to the resulting array

  body <- collect $ do
    dScope Nothing $ scopeOfLParams $ concatMap (lambdaParams . segBinOpLambda) scan_ops
    local_accs <- forM scan_ops $ \scan_op -> do
      let shape = segBinOpShape scan_op
          ts = lambdaReturnType $ segBinOpLambda scan_op
      forM (zip3 (xParams scan_op) (segBinOpNeutral scan_op) ts) $ \(p, ne, t) -> do
        acc <-
          case shapeDims shape of
            [] -> pure $ paramName p
            _ -> do
              let pt = elemType t
              sAllocArray "local_acc" pt (shape <> arrayShape t) DefaultSpace

        -- Now neutral-initialise the accumulator.
        sLoopNest (segBinOpShape scan_op) $ \vec_is ->
          copyDWIMFix acc vec_is ne []

        pure acc

    generateChunkLoop "SegScan" $ \i -> do
      zipWithM_ dPrimV_ is $ unflattenIndex ns' i
      compileStms mempty (kernelBodyStms kbody) $ do
        sComment "write mapped values results to memory" $ do
          let map_arrs = drop (segBinOpResults scan_ops) $ patElems pat
          zipWithM_ (compileThreadResult space) map_arrs map_res

        forM_ (zip4 per_scan_pes scan_ops per_scan_res local_accs) $ \(pes, scan_op, scan_res, acc) ->
          sLoopNest (segBinOpShape scan_op) $ \vec_is -> do
            -- Read accum value
            forM_ (zip (xParams scan_op) acc) $ \(p, acc') ->
              copyDWIMFix (paramName p) [] (Var acc') vec_is

            -- Read next value
            sComment "Read next values" $
              forM_ (zip (yParams scan_op) scan_res) $ \(p, se) ->
                copyDWIMFix (paramName p) [] (kernelResultSubExp se) vec_is

            compileStms mempty (bodyStms $ lamBody scan_op) $
              forM_ (zip3 acc pes $ map resSubExp $ bodyResult $ lamBody scan_op) $
                \(acc', pe, se) -> do
                  copyDWIMFix (patElemName pe) (map Imp.le64 is ++ vec_is) se []
                  copyDWIMFix acc' vec_is se []

  free_params <- freeParams body [segFlat space]
  emit $ Imp.Op $ Imp.ParLoop "scan_stage_1" body free_params $ segFlat space

scanStage2 ::
  Pat MCMem ->
  TV Int32 ->
  SegSpace ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  MulticoreGen ()
scanStage2 pat nsubtasks space scan_ops kbody = do
  emit $ Imp.DebugPrint "nonsegmentedScan stage 2" Nothing
  let (is, ns) = unzip $ unSegSpace space
      ns_64 = map toInt64Exp ns
      per_scan_pes = segBinOpChunks scan_ops $ patElems pat
      nsubtasks' = tvExp nsubtasks

  dScope Nothing $ scopeOfLParams $ concatMap (lambdaParams . segBinOpLambda) scan_ops
  offset <- dPrimV "offset" (0 :: Imp.TExp Int64)
  let offset' = tvExp offset
  offset_index <- dPrimV "offset_index" (0 :: Imp.TExp Int64)
  let offset_index' = tvExp offset_index

  -- Parameters used to find the chunk sizes
  -- Perhaps get this information from ``scheduling information``
  -- instead of computing it manually here.
  let iter_pr_subtask = product ns_64 `quot` sExt64 nsubtasks'
      remainder = product ns_64 `rem` sExt64 nsubtasks'

  accs <- resultArrays "scan_stage_2_accum" scan_ops
  forM_ (zip scan_ops accs) $ \(scan_op, acc) ->
    sLoopNest (segBinOpShape scan_op) $ \vec_is ->
      forM_ (zip acc $ segBinOpNeutral scan_op) $ \(acc', ne) ->
        copyDWIMFix acc' vec_is ne []

  -- Perform sequential scan over the last element of each chunk
  sFor "i" (nsubtasks' - 1) $ \i -> do
    offset <-- iter_pr_subtask
    sWhen (sExt64 i .<. remainder) (offset <-- offset' + 1)
    offset_index <-- offset_index' + offset'
    zipWithM_ dPrimV_ is $ unflattenIndex ns_64 $ sExt64 offset_index'

    compileStms mempty (kernelBodyStms kbody) $
      forM_ (zip3 per_scan_pes scan_ops accs) $ \(pes, scan_op, acc) ->
        sLoopNest (segBinOpShape scan_op) $ \vec_is -> do
          sComment "Read carry in" $
            forM_ (zip (xParams scan_op) acc) $ \(p, acc') ->
              copyDWIMFix (paramName p) [] (Var acc') vec_is

          sComment "Read next values" $
            forM_ (zip (yParams scan_op) pes) $ \(p, pe) ->
              copyDWIMFix (paramName p) [] (Var $ patElemName pe) ((offset_index' - 1) : vec_is)

          compileStms mempty (bodyStms $ lamBody scan_op) $
            forM_ (zip3 acc pes $ map resSubExp $ bodyResult $ lamBody scan_op) $
              \(acc', pe, se) -> do
                copyDWIMFix (patElemName pe) ((offset_index' - 1) : vec_is) se []
                copyDWIMFix acc' vec_is se []

-- Stage 3 : Finally each thread partially scans a chunk of the input
--           reading its corresponding carry-in
scanStage3 ::
  Pat MCMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  MulticoreGen ()
scanStage3 pat space scan_ops kbody = do
  let (is, ns) = unzip $ unSegSpace space
      all_scan_res = take (segBinOpResults scan_ops) $ kernelBodyResult kbody
      per_scan_res = segBinOpChunks scan_ops all_scan_res
      per_scan_pes = segBinOpChunks scan_ops $ patElems pat
      ns' = map toInt64Exp ns

  body <- collect $ do
    dScope Nothing $ scopeOfLParams $ concatMap (lambdaParams . segBinOpLambda) scan_ops
    local_accs <- forM (zip scan_ops per_scan_pes) $ \(scan_op, pes) -> do
      let shape = segBinOpShape scan_op
          ts = lambdaReturnType $ segBinOpLambda scan_op
      forM (zip4 (xParams scan_op) pes ts $ segBinOpNeutral scan_op) $ \(p, pe, t, ne) -> do
        acc <-
          case shapeDims shape of
            [] -> pure $ paramName p
            _ -> do
              let pt = elemType t
              sAllocArray "local_acc" pt (shape <> arrayShape t) DefaultSpace

        -- Initialise the accumulator with neutral from previous chunk.
        -- or read neutral if first ``iter``
        (start, _end) <- getLoopBounds
        sLoopNest (segBinOpShape scan_op) $ \vec_is -> do
          let read_carry_in =
                copyDWIMFix acc vec_is (Var $ patElemName pe) (start - 1 : vec_is)
              read_neutral =
                copyDWIMFix acc vec_is ne []
          sIf (start .==. 0) read_neutral read_carry_in
        pure acc

    generateChunkLoop "SegScan" $ \i -> do
      zipWithM_ dPrimV_ is $ unflattenIndex ns' i
      sComment "stage 3 scan body" $
        compileStms mempty (kernelBodyStms kbody) $
          forM_ (zip4 per_scan_pes scan_ops per_scan_res local_accs) $ \(pes, scan_op, scan_res, acc) ->
            sLoopNest (segBinOpShape scan_op) $ \vec_is -> do
              forM_ (zip (xParams scan_op) acc) $ \(p, acc') ->
                copyDWIMFix (paramName p) [] (Var acc') vec_is

              -- Read next value
              forM_ (zip (yParams scan_op) scan_res) $ \(p, se) ->
                copyDWIMFix (paramName p) [] (kernelResultSubExp se) vec_is

              compileStms mempty (bodyStms $ lamBody scan_op) $
                forM_ (zip3 pes (map resSubExp $ bodyResult $ lamBody scan_op) acc) $
                  \(pe, se, acc') -> do
                    copyDWIMFix (patElemName pe) (map Imp.le64 is ++ vec_is) se []
                    copyDWIMFix acc' vec_is se []

  free_params' <- freeParams body [segFlat space]
  emit $ Imp.Op $ Imp.ParLoop "scan_stage_3" body free_params' $ segFlat space

-- This implementation for a Segmented scan only
-- parallelize over the segments and each segment is
-- scanned sequentially.
segmentedScan ::
  Pat MCMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  MulticoreGen Imp.Code
segmentedScan pat space scan_ops kbody = do
  emit $ Imp.DebugPrint "segmented segScan" Nothing
  collect $ do
    body <- compileSegScanBody pat space scan_ops kbody
    free_params <- freeParams body [segFlat space]
    emit $ Imp.Op $ Imp.ParLoop "seg_scan" body free_params $ segFlat space

compileSegScanBody ::
  Pat MCMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  MulticoreGen Imp.Code
compileSegScanBody pat space scan_ops kbody = do
  let (is, ns) = unzip $ unSegSpace space
      ns_64 = map toInt64Exp ns

  let per_scan_pes = segBinOpChunks scan_ops $ patElems pat
  collect . generateChunkLoop "SegScan" $ \segment_i -> do
    forM_ (zip scan_ops per_scan_pes) $ \(scan_op, scan_pes) -> do
      dScope Nothing $ scopeOfLParams $ lambdaParams $ segBinOpLambda scan_op
      let (scan_x_params, scan_y_params) = splitAt (length $ segBinOpNeutral scan_op) $ (lambdaParams . segBinOpLambda) scan_op

      forM_ (zip scan_x_params $ segBinOpNeutral scan_op) $ \(p, ne) ->
        copyDWIMFix (paramName p) [] ne []

      let inner_bound = last ns_64
      -- Perform a sequential scan over the segment ``segment_i``
      sFor "i" inner_bound $ \i -> do
        zipWithM_ dPrimV_ (init is) $ unflattenIndex (init ns_64) segment_i
        dPrimV_ (last is) i
        compileStms mempty (kernelBodyStms kbody) $ do
          let (scan_res, map_res) = splitAt (length $ segBinOpNeutral scan_op) $ kernelBodyResult kbody
          sComment "write to-scan values to parameters" $
            forM_ (zip scan_y_params scan_res) $ \(p, se) ->
              copyDWIMFix (paramName p) [] (kernelResultSubExp se) []

          sComment "write mapped values results to memory" $
            forM_ (zip (drop (length $ segBinOpNeutral scan_op) $ patElems pat) map_res) $ \(pe, se) ->
              copyDWIMFix (patElemName pe) (map Imp.le64 is) (kernelResultSubExp se) []

          sComment "combine with carry and write to memory" $
            compileStms mempty (bodyStms $ lambdaBody $ segBinOpLambda scan_op) $
              forM_ (zip3 scan_x_params scan_pes $ map resSubExp $ bodyResult $ lambdaBody $ segBinOpLambda scan_op) $ \(p, pe, se) -> do
                copyDWIMFix (patElemName pe) (map Imp.le64 is) se []
                copyDWIMFix (paramName p) [] se []
