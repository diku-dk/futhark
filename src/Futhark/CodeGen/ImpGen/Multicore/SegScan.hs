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
                -> Mode
                -> MulticoreGen Imp.Code
compileSegScan pat space reds kbody mode
  | [_] <- unSegSpace space =
      nonsegmentedScan pat space reds kbody mode
  | otherwise =
      segmentedScan pat space reds kbody mode


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
                 -> Mode
                 -> MulticoreGen Imp.Code

nonsegmentedScan pat space scan_ops kbody ModeSequential = do
  let ns = map snd $ unSegSpace space
  ns' <- mapM toExp ns

  collect $ localMode ModeSequential $ do
    flat_seq_idx <- dPrimV "seq_iter" 0
    seq_code_body <- sequentialScan flat_seq_idx pat space scan_ops kbody
    sFor "i" (product ns') $ \i -> do
      flat_seq_idx <-- i
      emit seq_code_body



nonsegmentedScan pat space scan_ops kbody ModeParallel = do
  emit $ Imp.DebugPrint "nonsegmented segScan" Nothing

  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  tid' <- toExp $ Var $ segFlat space

  let (all_scan_res, map_res) = splitAt (segBinOpResults scan_ops) $ kernelBodyResult kbody
      per_scan_res            = segBinOpChunks scan_ops all_scan_res
      per_scan_pes            = segBinOpChunks scan_ops $ patternValueElements pat

  collect $ do
    flat_par_idx <- dPrimV "iter" 0
    flat_par_idx' <- toExp $ Var flat_par_idx

    -- Stage 1 : each thread partially scans a chunk of the input
    -- Writes directly to the resulting array
    stage_1_prebody <- collect $ do
      zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_par_idx
      dScope Nothing $ scopeOfLParams $ concatMap (lambdaParams . segBinOpLambda) scan_ops
      compileStms mempty (kernelBodyStms kbody) $
        sComment "write mapped values results to memory" $ do
          let map_arrs = drop (segBinOpResults scan_ops) $ patternElements pat
          zipWithM_ (compileThreadResult space) map_arrs map_res

          forM_ (zip3 per_scan_pes scan_ops per_scan_res) $ \(pes, scan_op, scan_res) ->
            sLoopNest (segBinOpShape scan_op) $ \vec_is -> do

            -- Read neutral element
            forM_ (zip (xParams scan_op) (segBinOpNeutral scan_op)) $ \(p, ne) ->
                copyDWIMFix (paramName p) [] ne []

            -- Read next value
            forM_ (zip (yParams scan_op) scan_res) $ \(p, se) ->
                copyDWIMFix (paramName p) [] (kernelResultSubExp se) vec_is

            compileStms mempty (bodyStms $ lamBody scan_op) $
              forM_ (zip pes (bodyResult $ lamBody scan_op)) $
                \(pe, se) -> copyDWIMFix (patElemName pe) (map Imp.vi32 is ++ vec_is) se []

      flat_par_idx <-- flat_par_idx' + 1

    stage_1_body <- collect $ do
      forM_  (zip is $ unflattenIndex ns' $ Imp.vi32 flat_par_idx) $ uncurry (<--)
      sComment "stage 1 scan body" $
        compileStms mempty (kernelBodyStms kbody) $ do
          sComment "write mapped values results to memory" $ do
            let map_arrs = drop (segBinOpResults scan_ops) $ patternElements pat
            zipWithM_ (compileThreadResult space) map_arrs map_res

          forM_ (zip3 per_scan_pes scan_ops per_scan_res) $ \(pes, scan_op, scan_res) ->
            sLoopNest (segBinOpShape scan_op) $ \vec_is -> do

            -- Read accum value
            sComment "Read accum value" $
              forM_ (zip (xParams scan_op) pes) $ \(p, pe) ->
                copyDWIMFix (paramName p) [] (Var $ patElemName pe) (flat_par_idx' - 1 : vec_is)

            -- Read next value
            sComment "Read next values" $
              forM_ (zip (yParams scan_op) scan_res) $ \(p, se) ->
                copyDWIMFix (paramName p) [] (kernelResultSubExp se) vec_is

            compileStms mempty (bodyStms $ lamBody scan_op) $
              forM_ (zip pes (bodyResult $ lamBody scan_op)) $
                \(pe, se) -> copyDWIMFix (patElemName pe) (map Imp.vi32 is ++ vec_is) se []


    free_params <- freeParams (stage_1_prebody <> stage_1_body) (segFlat space : [flat_par_idx])
    ntasks <- dPrim "ntasks" $ IntType Int32
    ntasks' <- toExp $ Var ntasks
    emit $ Imp.Op $ Imp.MCFunc flat_par_idx stage_1_prebody stage_1_body free_params $
      Imp.MulticoreInfo ntasks Imp.Static (segFlat space)



    emit $ Imp.DebugPrint "nonsegmentedScan stage 2" Nothing
    scan_ops2 <- renameSegBinOp scan_ops
    -- |
    -- Begin stage two of scan
    dScope Nothing $ scopeOfLParams $ concatMap (lambdaParams . segBinOpLambda) scan_ops2

    offset <- dPrimV "offset" 0
    offset' <- toExp $ Var offset

    offset_index <- dPrimV "offset_index" 0
    offset_index' <- toExp $ Var offset_index

    sWhen (ntasks' .>. 1) $ do
      let iter_pr_subtask = product ns' `quot` ntasks'
          remainder       = product ns' `rem` ntasks'

      accs <- resultArrays scan_ops2
      forM_ (zip scan_ops2 accs) $ \(scan_op, acc) ->
        sLoopNest (segBinOpShape scan_op) $ \vec_is ->
        forM_ (zip acc $ segBinOpNeutral scan_op) $ \(acc', ne) ->
          copyDWIMFix acc' vec_is ne []

      sFor "i" (ntasks'-1) $ \i -> do
         offset <-- iter_pr_subtask
         sWhen (i .<. remainder) (offset <-- offset' + 1)
         offset_index <-- offset_index' + offset'
         zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 offset_index

         compileStms mempty (kernelBodyStms kbody) $
           forM_ (zip3 per_scan_pes scan_ops2 accs) $ \(pes, scan_op, acc) ->
             sLoopNest (segBinOpShape scan_op) $ \vec_is -> do

             sComment "Read carry in" $
               forM_ (zip (xParams scan_op) acc) $ \(p, acc') ->
                 copyDWIMFix (paramName p) [] (Var acc') vec_is

             sComment "Read next values" $
               forM_ (zip (yParams scan_op) pes) $ \(p, pe) ->
                 copyDWIMFix (paramName p) [] (Var $ patElemName pe) ((offset_index'-1) : vec_is)

             compileStms mempty (bodyStms $ lamBody scan_op) $
                forM_ (zip3 acc pes (bodyResult $ lamBody scan_op)) $
                  \(acc', pe, se) -> do copyDWIMFix (patElemName pe) ((offset_index'-1) : vec_is) se []
                                        copyDWIMFix acc' vec_is se []

      -- Stage 3 : Finally each thread partially scans a chunk of the input
      --           reading it's corresponding carry-in
      scan_ops3 <- renameSegBinOp scan_ops
      accs' <- groupResultArrays (Var ntasks) scan_ops3
      stage_3_prebody <- collect $ do
        zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_par_idx
        dScope Nothing $ scopeOfLParams $ concatMap (lambdaParams . segBinOpLambda) scan_ops3
        -- Read carry in
        let read_carry_in = forM_ (zip3 scan_ops3 accs' per_scan_pes) $ \(scan_op, acc, pes) ->
                              sLoopNest (segBinOpShape scan_op) $ \vec_is ->
                                forM_ (zip acc pes) $ \(acc', pe) ->
                                  copyDWIMFix acc' (tid' : vec_is) (Var $ patElemName pe) (flat_par_idx' - 1 : vec_is)

            read_neutral = forM_ (zip scan_ops3 accs') $ \(scan_op, acc) ->
                             sLoopNest (segBinOpShape scan_op) $ \vec_is ->
                               forM_ (zip acc $ segBinOpNeutral scan_op) $ \(acc', ne) ->
                                 copyDWIMFix acc' (tid' : vec_is) ne []

        sIf (flat_par_idx' .==. 0) read_neutral read_carry_in

      stage_3_body <- collect $ do
        forM_  (zip is $ unflattenIndex ns' $ Imp.vi32 flat_par_idx) $ uncurry (<--)
        sComment "stage 3 scan body" $
          compileStms mempty (kernelBodyStms kbody) $
            forM_ (zip4 per_scan_pes scan_ops3 per_scan_res accs') $ \(pes, scan_op, scan_res, acc) ->
              sLoopNest (segBinOpShape scan_op) $ \vec_is -> do

              -- Read accum value
              forM_ (zip (xParams scan_op) acc) $ \(p, acc') ->
                copyDWIMFix (paramName p) [] (Var acc') (tid' : vec_is)

              -- Read next value
              forM_ (zip (yParams scan_op) scan_res) $ \(p, se) ->
                copyDWIMFix (paramName p) [] (kernelResultSubExp se) vec_is

              compileStms mempty (bodyStms $ lamBody scan_op) $
                forM_ (zip3 pes (bodyResult $ lamBody scan_op) acc) $
                  \(pe, se, acc') -> do
                    copyDWIMFix (patElemName pe) (map Imp.vi32 is ++ vec_is) se []
                    copyDWIMFix acc' (tid' : vec_is) se []


      free_params' <- freeParams  (stage_3_prebody <> stage_3_body)  (segFlat space : [flat_par_idx])
      emit $ Imp.Op $ Imp.MCFunc flat_par_idx stage_3_prebody stage_3_body free_params' $
        Imp.MulticoreInfo ntasks Imp.Static (segFlat space)

sequentialScan :: VName
               -> Pattern MCMem
               -> SegSpace
               -> [SegBinOp MCMem]
               -> KernelBody MCMem
               -> MulticoreGen Imp.Code
sequentialScan flat_par_idx pat space scan_ops kbody = do
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  let (all_scan_res, map_res) = splitAt (segBinOpResults scan_ops) $ kernelBodyResult kbody
      per_scan_res            = segBinOpChunks scan_ops all_scan_res
      per_scan_pes            = segBinOpChunks scan_ops $ patternValueElements pat

  collect $ do
    dScope Nothing $ scopeOfLParams $ concatMap (lambdaParams . segBinOpLambda) scan_ops
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_par_idx
    compileStms mempty (kernelBodyStms kbody) $ do
      sComment "write mapped values results to memory" $ do
        let map_arrs = drop (segBinOpResults scan_ops) $ patternElements pat
        zipWithM_ (compileThreadResult space) map_arrs map_res

      forM_ (zip3 per_scan_pes scan_ops per_scan_res) $ \(pes, scan_op, scan_res) ->
        sLoopNest (segBinOpShape scan_op) $ \vec_is -> do

        -- Read accum value
        let last_is = last is
        last_is' <- toExp $ Var last_is

        sComment "Read accum value" $
          sIf (last_is' .==. 0)
              (forM_ (zip (xParams scan_op) $ segBinOpNeutral scan_op) $ \(p, ne) ->
                copyDWIMFix (paramName p) [] ne [])
              (forM_ (zip (xParams scan_op) pes) $ \(p, pe) ->
                copyDWIMFix (paramName p) [] (Var $ patElemName pe) (map Imp.vi32 (init is) ++ [last_is'-1] ++ vec_is))
        -- Read next value
        sComment "Read next values" $
          forM_ (zip (yParams scan_op) scan_res) $ \(p, se) ->
            copyDWIMFix (paramName p) [] (kernelResultSubExp se) vec_is

        compileStms mempty (bodyStms $ lamBody scan_op) $
          forM_ (zip pes (bodyResult $ lamBody scan_op)) $
            \(pe, se) -> copyDWIMFix (patElemName pe) (map Imp.vi32 is ++ vec_is) se []


compileSegScanBody :: VName
                   -> Pattern MCMem
                   -> SegSpace
                   -> [SegBinOp MCMem]
                   -> KernelBody MCMem
                   -> MulticoreGen Imp.Code
compileSegScanBody idx pat space scan_ops kbody = do
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  flat_par_idx <- dPrim "iter" int32
  idx' <- toExp $ Var idx
  collect $ do
    emit $ Imp.DebugPrint "segmented segScan stage 1" Nothing
    forM_ scan_ops $ \scan_op -> do
      dScope Nothing $ scopeOfLParams $ lambdaParams $ segBinOpLambda scan_op
      let (scan_x_params, scan_y_params) = splitAt (length $ segBinOpNeutral scan_op) $ (lambdaParams . segBinOpLambda) scan_op

      forM_ (zip scan_x_params $ segBinOpNeutral scan_op) $ \(p, ne) ->
        copyDWIMFix (paramName p) [] ne []

      let inner_bound = last ns'
      sFor "i" inner_bound $ \i -> do
        dPrimV_ flat_par_idx (idx' * inner_bound + i)
        zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_par_idx
        compileStms mempty (kernelBodyStms kbody) $ do
          let (scan_res, map_res) = splitAt (length $ segBinOpNeutral scan_op) $ kernelBodyResult kbody
          sComment "write to-scan values to parameters" $
            forM_ (zip scan_y_params scan_res) $ \(p, se) ->
              copyDWIMFix (paramName p) [] (kernelResultSubExp se) []
          sComment "write mapped values results to global memory" $
            forM_ (zip (drop (length $ segBinOpNeutral scan_op) $ patternElements pat) map_res) $ \(pe, se) ->
              copyDWIMFix (patElemName pe) (map Imp.vi32 is) (kernelResultSubExp se) []

          sComment "combine with carry and write to local memory" $
            compileStms mempty (bodyStms $ lambdaBody $ segBinOpLambda scan_op) $
            forM_ (zip3 scan_x_params (patternElements pat) (bodyResult $ lambdaBody $ segBinOpLambda scan_op)) $ \(p, pe, se) -> do
              copyDWIMFix (patElemName pe) (map Imp.vi32 is)  se []
              copyDWIMFix (paramName p) [] se []




segmentedScan :: Pattern MCMem
              -> SegSpace
              -> [SegBinOp MCMem]
              -> KernelBody MCMem
              -> Mode
              -> MulticoreGen Imp.Code
segmentedScan pat space scan_ops kbody ModeParallel = do
  emit $ Imp.DebugPrint "segmented segScan" Nothing
  collect $ do
    n_par_segments <- dPrim "segment_iter" $ IntType Int32
    -- iteration variable
    fbody <- compileSegScanBody n_par_segments pat space scan_ops kbody
    ntasks <- dPrim "num_tasks" $ IntType Int32
    free_params <- freeParams fbody  (segFlat space : [n_par_segments])
    let sched = decideScheduling fbody
    emit $ Imp.Op $ Imp.MCFunc n_par_segments mempty fbody free_params $
      Imp.MulticoreInfo ntasks sched (segFlat space)

segmentedScan pat space scan_ops kbody ModeSequential = do
  let ns = map snd $ unSegSpace space
  ns' <- mapM toExp ns
  collect $ localMode ModeSequential $ do
    n_seq_segments <- dPrim "segment_iter" $ IntType Int32
    fbody <- compileSegScanBody n_seq_segments pat space scan_ops kbody
    sFor "i" (product $ init ns') $ \i -> do
      n_seq_segments <-- i
      emit fbody
