module Futhark.CodeGen.ImpGen.Multicore.SegScan
  (compileSegScan
  )
  where

import Control.Monad
import Data.List
import Prelude hiding (quot, rem)

import qualified Futhark.CodeGen.ImpCode.Multicore as Imp

import Futhark.CodeGen.ImpGen
import Futhark.IR.MCMem
import Futhark.CodeGen.ImpGen.Multicore.Base

import Futhark.Util.IntegralExp (quot, rem)

-- Compile a SegScan construct
compileSegScan :: Pattern MCMem
               -> SegSpace
               -> [SegBinOp MCMem]
               -> KernelBody MCMem
               -> VName
               -> MulticoreGen Imp.Code
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


nonsegmentedScan :: Pattern MCMem
                 -> SegSpace
                 -> [SegBinOp MCMem]
                 -> KernelBody MCMem
                 -> VName
                 -> MulticoreGen Imp.Code
nonsegmentedScan pat space scan_ops kbody nsubtasks = do
  emit $ Imp.DebugPrint "nonsegmented segScan" Nothing

  -- sequentialScan pat space scan_ops kbody
  collect $ do
    scanStage1 pat space nsubtasks scan_ops kbody

    nsubtasks' <- toExp $ Var nsubtasks
    sWhen (nsubtasks' .>. 1) $ do
      scan_ops2 <- renameSegBinOp scan_ops
      scanStage2 pat nsubtasks space scan_ops2 kbody
      scan_ops3 <- renameSegBinOp scan_ops
      scanStage3 pat nsubtasks space scan_ops3 kbody

scanStage1 :: Pattern MCMem
           -> SegSpace
           -> VName
           -> [SegBinOp MCMem]
           -> KernelBody MCMem
           -> MulticoreGen ()
scanStage1 pat space nsubtasks scan_ops kbody = do
  let (all_scan_res, map_res) = splitAt (segBinOpResults scan_ops) $ kernelBodyResult kbody
      per_scan_res           = segBinOpChunks scan_ops all_scan_res
      per_scan_pes           = segBinOpChunks scan_ops $ patternValueElements pat
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns
  iter <- dPrim "iter" $ IntType Int32
  tid' <- toExp $ Var $ segFlat space

  -- Stage 1 : each thread partially scans a chunk of the input
  -- Writes directly to the resulting array

  -- Accumulator array for each thread to use
  accs <- groupResultArrays "scan_stage_1_accum_arr" (Var nsubtasks) scan_ops
  prebody <- collect $ do
    dScope Nothing $ scopeOfLParams $ concatMap (lambdaParams . segBinOpLambda) scan_ops
    forM_ (zip scan_ops accs) $ \(scan_op, acc) ->
      sLoopNest (segBinOpShape scan_op) $ \vec_is ->
        case vec_is of
        [] -> forM_ (zip (xParams scan_op) $ segBinOpNeutral scan_op) $ \(p, ne) ->
              copyDWIMFix (paramName p) [] ne []
        _ -> forM_ (zip acc $ segBinOpNeutral scan_op) $ \(acc', ne) ->
               copyDWIMFix acc' (tid' : vec_is) ne []

  body <- collect $ do
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 iter
    sComment "stage 1 scan body" $
      compileStms mempty (kernelBodyStms kbody) $ do
        sComment "write mapped values results to memory" $ do
          let map_arrs = drop (segBinOpResults scan_ops) $ patternElements pat
          zipWithM_ (compileThreadResult space) map_arrs map_res

        forM_ (zip4 per_scan_pes scan_ops per_scan_res accs) $ \(pes, scan_op, scan_res, acc) ->
          sLoopNest (segBinOpShape scan_op) $ \vec_is -> do

          case vec_is of
            [] -> return mempty
            _ ->
              -- Read accum value
              forM_ (zip (xParams scan_op) acc) $ \(p, acc') ->
                copyDWIMFix (paramName p) [] (Var acc') (tid' : vec_is)

          -- Read next value
          sComment "Read next values" $
            forM_ (zip (yParams scan_op) scan_res) $ \(p, se) ->
              copyDWIMFix (paramName p) [] (kernelResultSubExp se) vec_is

          compileStms mempty (bodyStms $ lamBody scan_op) $
            forM_ (zip4 acc (xParams scan_op) pes (bodyResult $ lamBody scan_op)) $
              \(acc', p, pe, se) -> do
                copyDWIMFix (patElemName pe) (map Imp.vi32 is ++ vec_is) se []
                case vec_is of
                  [] -> copyDWIMFix (paramName p)  [] se []
                  _ -> copyDWIMFix acc' (tid' : vec_is) se []

  free_params <- freeParams (prebody <> body) (segFlat space : [iter])
  let (body_allocs, body') = extractAllocations body
  emit $ Imp.Op $ Imp.MCFunc "scan_stage_1" iter (body_allocs <> prebody) body' free_params $ segFlat space


scanStage2 :: Pattern MCMem
           -> VName
           -> SegSpace
           -> [SegBinOp MCMem]
           -> KernelBody MCMem
           -> MulticoreGen ()
scanStage2 pat nsubtasks space scan_ops kbody = do
  emit $ Imp.DebugPrint "nonsegmentedScan stage 2" Nothing
  let (is, ns) = unzip $ unSegSpace space
      ns_64 = map (sExt Int64 . toExp' int32) ns
      per_scan_pes = segBinOpChunks scan_ops $ patternValueElements pat
  nsubtasks' <- toExp $ Var nsubtasks

  -- |
  -- Begin stage two of scan
  dScope Nothing $ scopeOfLParams $ concatMap (lambdaParams . segBinOpLambda) scan_ops

  offset <- dPrim "offset" $ IntType Int32
  offset <-- 0
  offset' <- toExp $ Var offset

  offset_index <- dPrim "offset_index" $ IntType Int32
  offset_index <-- 0
  offset_index' <- toExp $ Var offset_index

  let iter_pr_subtask = sExt Int32 $ product ns_64 `quot` sExt Int64 nsubtasks'
      remainder       = sExt Int32 $ product ns_64 `rem` sExt Int64 nsubtasks'

  accs <- resultArrays "scan_stage_2_accum" scan_ops
  forM_ (zip scan_ops accs) $ \(scan_op, acc) ->
    sLoopNest (segBinOpShape scan_op) $ \vec_is ->
    forM_ (zip acc $ segBinOpNeutral scan_op) $ \(acc', ne) ->
      copyDWIMFix acc' vec_is ne []

  sFor "i" (nsubtasks'-1) $ \i -> do
     offset <-- iter_pr_subtask
     sWhen (i .<. remainder) (offset <-- offset' + 1)
     offset_index <-- offset_index' + offset'
     zipWithM_ dPrimV_ is $ map (sExt Int32) $ unflattenIndex ns_64 offset_index'

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
            forM_ (zip3 acc pes (bodyResult $ lamBody scan_op)) $
              \(acc', pe, se) -> do copyDWIMFix (patElemName pe) ((offset_index' - 1) : vec_is) se []
                                    copyDWIMFix acc' vec_is se []



-- Stage 3 : Finally each thread partially scans a chunk of the input
--           reading it's corresponding carry-in
scanStage3 :: Pattern MCMem
           -> VName
           -> SegSpace
           -> [SegBinOp MCMem]
           -> KernelBody MCMem
           -> MulticoreGen ()
scanStage3 pat nsubtasks space scan_ops kbody = do
  emit $ Imp.DebugPrint "nonsegmentedScan stage 3" Nothing

  let (is, ns) = unzip $ unSegSpace space
      all_scan_res = take (segBinOpResults scan_ops) $ kernelBodyResult kbody
      per_scan_res = segBinOpChunks scan_ops all_scan_res
      per_scan_pes = segBinOpChunks scan_ops $ patternValueElements pat

  ns' <- mapM toExp ns

  iter <- dPrim "iter" $ IntType Int32
  iter' <- toExp $ Var iter
  tid' <- toExp $ Var $ segFlat space

  accs <- groupResultArrays "scan_stage_3_accum_arr" (Var nsubtasks) scan_ops
  prebody <- collect $ do
    dScope Nothing $ scopeOfLParams $ concatMap (lambdaParams . segBinOpLambda) scan_ops
    forM_ (zip3 scan_ops accs per_scan_pes) $ \(scan_op, acc, pes) ->
      sLoopNest (segBinOpShape scan_op) $ \vec_is ->
        -- Read carry in or neutral element
        case vec_is of
          [] -> do
            let read_carry_in = forM_ (zip (xParams scan_op) pes) $ \(p, pe) ->
                                  copyDWIMFix (paramName p) [] (Var $ patElemName pe) (iter' - 1 : vec_is)
                read_neutral = forM_ (zip (xParams scan_op) $ segBinOpNeutral scan_op) $ \(p, ne) ->
                                  copyDWIMFix (paramName p) [] ne []
            sIf (iter' .==. 0) read_neutral read_carry_in

          _ -> do
            let read_carry_in =  forM_ (zip acc pes) $ \(acc', pe) ->
                     copyDWIMFix acc' (tid' : vec_is) (Var $ patElemName pe) (iter' - 1 : vec_is)

                read_neutral = forM_ (zip acc $ segBinOpNeutral scan_op) $ \(acc', ne) ->
                     copyDWIMFix acc' (tid' : vec_is) ne []
            sIf (iter' .==. 0) read_neutral read_carry_in

  body <- collect $ do
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 iter
    sComment "stage 3 scan body" $
      compileStms mempty (kernelBodyStms kbody) $
        forM_ (zip4 per_scan_pes scan_ops per_scan_res accs) $ \(pes, scan_op, scan_res, acc) ->
          sLoopNest (segBinOpShape scan_op) $ \vec_is -> do

          case vec_is of
            [] -> return ()
            _ -> forM_ (zip (xParams scan_op) acc) $ \(p, acc') ->
                   copyDWIMFix (paramName p) [] (Var acc') (tid' : vec_is)

          -- Read next value
          forM_ (zip (yParams scan_op) scan_res) $ \(p, se) ->
            copyDWIMFix (paramName p) [] (kernelResultSubExp se) vec_is

          compileStms mempty (bodyStms $ lamBody scan_op) $
            forM_ (zip4 pes (bodyResult $ lamBody scan_op) (xParams scan_op) acc) $
              \(pe, se, p, acc')-> do
                copyDWIMFix (patElemName pe) (map Imp.vi32 is ++ vec_is) se []
                case vec_is of
                  [] -> copyDWIMFix (paramName p) [] se []
                  _ -> copyDWIMFix acc' (tid' : vec_is) se []

  free_params' <- freeParams (prebody <> body)  (segFlat space : [iter])
  let (body_allocs, body') = extractAllocations body
  emit $ Imp.Op $ Imp.MCFunc "scan_stage_3" iter (body_allocs <> prebody) body' free_params' $ segFlat space

segmentedScan :: Pattern MCMem
              -> SegSpace
              -> [SegBinOp MCMem]
              -> KernelBody MCMem
              -> MulticoreGen Imp.Code
segmentedScan pat space scan_ops kbody = do
  emit $ Imp.DebugPrint "segmented segScan" Nothing
  collect $ do
    n_par_segments <- dPrim "segment_iter" $ IntType Int64
    -- iteration variable
    body <- compileSegScanBody n_par_segments pat space scan_ops kbody
    free_params <- freeParams body (segFlat space : [n_par_segments])
    let (body_allocs, body') = extractAllocations body
    emit $ Imp.Op $ Imp.MCFunc "seg_scan" n_par_segments body_allocs body' free_params $ segFlat space


compileSegScanBody :: VName
                   -> Pattern MCMem
                   -> SegSpace
                   -> [SegBinOp MCMem]
                   -> KernelBody MCMem
                   -> MulticoreGen Imp.Code
compileSegScanBody idx pat space scan_ops kbody = do
  let (is, ns) = unzip $ unSegSpace space
      ns_64 = map (sExt Int64 . toExp' int32) ns

  idx' <- toExp $ Var idx

  let per_scan_pes = segBinOpChunks scan_ops $ patternValueElements pat
  collect $ do
    emit $ Imp.DebugPrint "segmented segScan stage 1" Nothing
    forM_ (zip scan_ops per_scan_pes) $ \(scan_op, scan_pes) -> do
      dScope Nothing $ scopeOfLParams $ lambdaParams $ segBinOpLambda scan_op
      let (scan_x_params, scan_y_params) = splitAt (length $ segBinOpNeutral scan_op) $ (lambdaParams . segBinOpLambda) scan_op

      forM_ (zip scan_x_params $ segBinOpNeutral scan_op) $ \(p, ne) ->
        copyDWIMFix (paramName p) [] ne []

      let inner_bound = last ns_64
      sFor "i" inner_bound $ \i -> do
        zipWithM_ dPrimV_ (init is) $ map (sExt Int32) $ unflattenIndex (init ns_64) idx'
        dPrimV_ (last is) i
        compileStms mempty (kernelBodyStms kbody) $ do
          let (scan_res, map_res) = splitAt (length $ segBinOpNeutral scan_op) $ kernelBodyResult kbody
          sComment "write to-scan values to parameters" $
            forM_ (zip scan_y_params scan_res) $ \(p, se) ->
              copyDWIMFix (paramName p) [] (kernelResultSubExp se) []

          sComment "write mapped values results to memory" $
            forM_ (zip (drop (length $ segBinOpNeutral scan_op) $ patternElements pat) map_res) $ \(pe, se) ->
              copyDWIMFix (patElemName pe) (map Imp.vi32 is) (kernelResultSubExp se) []

          sComment "combine with carry and write to memory" $
            compileStms mempty (bodyStms $ lambdaBody $ segBinOpLambda scan_op) $
            forM_ (zip3 scan_x_params scan_pes (bodyResult $ lambdaBody $ segBinOpLambda scan_op)) $ \(p, pe, se) -> do
              copyDWIMFix (patElemName pe) (map Imp.vi32 is)  se []
              copyDWIMFix (paramName p) [] se []


sequentialScan pat space scan_ops kbody = do
  let ns = map snd $ unSegSpace space
  ns' <- mapM toExp ns

  dScope Nothing $ scopeOfLParams $ concatMap (lambdaParams . segBinOpLambda) scan_ops
  forM_ scan_ops $ \scan_op ->
    forM_ (zip (xParams scan_op) $ segBinOpNeutral scan_op) $ \(p, ne) ->
      copyDWIMFix (paramName p) [] ne []

  collect $ do
    flat_seq_idx <- dPrimV "seq_iter" 0
    seq_code_body <- sequentialScanBody flat_seq_idx pat space scan_ops kbody
    sFor "i" (product ns') $ \i -> do
      flat_seq_idx <-- i
      emit seq_code_body

sequentialScanBody :: VName
                   -> Pattern MCMem
                   -> SegSpace
                   -> [SegBinOp MCMem]
                   -> KernelBody MCMem
                   -> MulticoreGen Imp.Code
sequentialScanBody iter pat space scan_ops kbody = do
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  let (all_scan_res, map_res) = splitAt (segBinOpResults scan_ops) $ kernelBodyResult kbody
      per_scan_res            = segBinOpChunks scan_ops all_scan_res
      per_scan_pes            = segBinOpChunks scan_ops $ patternValueElements pat

  collect $ do
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 iter
    compileStms mempty (kernelBodyStms kbody) $ do
      let map_arrs = drop (segBinOpResults scan_ops) $ patternElements pat
      zipWithM_ (compileThreadResult space) map_arrs map_res

      forM_ (zip3 per_scan_pes scan_ops per_scan_res) $ \(pes, scan_op, scan_res) ->
        sLoopNest (segBinOpShape scan_op) $ \vec_is -> do

        -- Read next value
        forM_ (zip (yParams scan_op) scan_res) $ \(p, se) ->
          copyDWIMFix (paramName p) [] (kernelResultSubExp se) vec_is

        compileStms mempty (bodyStms $ lamBody scan_op) $
          forM_ (zip3 (xParams scan_op) pes (bodyResult $ lamBody scan_op)) $
            \(p, pe, se) -> do copyDWIMFix (patElemName pe) (map Imp.vi32 is ++ vec_is) se []
                               copyDWIMFix (paramName p) [] se []
