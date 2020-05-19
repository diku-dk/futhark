module Futhark.CodeGen.ImpGen.Multicore.SegScan
  (compileSegScan
  )
  where

import Control.Monad
import Data.List
import Prelude hiding (quot, rem)

import qualified Futhark.CodeGen.ImpCode.Multicore as Imp

import Futhark.CodeGen.ImpGen
import Futhark.Representation.MCMem
import Futhark.CodeGen.ImpGen.Multicore.Base

import Futhark.Util.IntegralExp (quot, rem)

-- Compile a SegScan construct
compileSegScan :: Pattern MCMem
                -> SegSpace
                -> [SegBinOp MCMem]
                -> KernelBody MCMem
                -> MulticoreGen ()
compileSegScan pat space reds kbody
  | [_] <- unSegSpace space =
      nonsegmentedScan pat space reds kbody
  | otherwise =
      segmentedScan pat space reds kbody


accumulatorArray :: [SegBinOp MCMem]
                 -> MulticoreGen [[VName]]
accumulatorArray reds =
  forM reds $ \(SegBinOp _ lam _ shape) ->
    forM (lambdaReturnType lam) $ \t -> do
    let pt = elemType t
        full_shape = shape <> arrayShape t
    sAllocArray "accum_arr" pt full_shape DefaultSpace

xParams, yParams :: SegBinOp MCMem -> [LParam MCMem]
xParams scan =
  take (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))
yParams scan =
  drop (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))

lamBody :: SegBinOp MCMem -> Body MCMem
lamBody = lambdaBody . segBinOpLambda

sequentialScan :: Pattern MCMem
               -> SegSpace
               -> [SegBinOp MCMem]
               -> KernelBody MCMem
               -> MulticoreGen ()
sequentialScan pat space scan_ops kbody = do
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns
  dPrimV_ (segFlat space) 0
  flat_idx <- dPrim "iter" int32

  let (all_scan_res, map_res) = splitAt (segBinOpResults scan_ops) $ kernelBodyResult kbody
      per_scan_res            = segBinOpChunks scan_ops all_scan_res
      per_scan_pes            = segBinOpChunks scan_ops $ patternValueElements pat

  dScope Nothing $ scopeOfLParams $ concatMap (lambdaParams . segBinOpLambda) scan_ops

  sFor "i" (product ns') $ \i -> do
    flat_idx <-- i
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_idx
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





nonsegmentedScan :: Pattern MCMem
                 -> SegSpace
                 -> [SegBinOp MCMem]
                 -> KernelBody MCMem
                 -> MulticoreGen ()
nonsegmentedScan pat space scan_ops kbody = do
  emit $ Imp.DebugPrint "nonsegmented segScan" Nothing
  sUnpauseProfiling

  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  tid <- dPrim "tid" $ IntType Int32
  tid' <- toExp $ Var tid

  flat_idx <- dPrimV "iter" 0
  flat_idx' <- toExp $ Var flat_idx

  num_threads <- getNumThreads
  num_threads' <- toExp $ Var num_threads

  let (all_scan_res, map_res) = splitAt (segBinOpResults scan_ops) $ kernelBodyResult kbody
      per_scan_res            = segBinOpChunks scan_ops all_scan_res
      per_scan_pes            = segBinOpChunks scan_ops $ patternValueElements pat

  -- Stage 1 : each thread partially scans a chunk of the input
  -- Writes directly to the resulting array
  stage_1_prebody <- collect $ do
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_idx
    dPrimV_ (segFlat space) tid'
    dScope Nothing $ scopeOfLParams $ concatMap (lambdaParams . segBinOpLambda) scan_ops
    compileStms mempty (kernelBodyStms kbody) $
      sComment "write mapped values results to memory" $ do
        let map_arrs = drop (segBinOpResults scan_ops) $ patternElements pat
        zipWithM_ (compileThreadResult space) map_arrs map_res

        forM_ (zip3 per_scan_pes scan_ops per_scan_res) $ \(pes, scan_op, scan_res) ->
          sLoopNest (segBinOpShape scan_op) $ \vec_is -> do

          forM_ (zip (xParams scan_op) (segBinOpNeutral scan_op)) $ \(p, ne) ->
              copyDWIMFix (paramName p) [] ne []

          -- Read next value
          forM_ (zip (yParams scan_op) scan_res) $ \(p, se) ->
              copyDWIMFix (paramName p) [] (kernelResultSubExp se) vec_is

          compileStms mempty (bodyStms $ lamBody scan_op) $
            forM_ (zip pes (bodyResult $ lamBody scan_op)) $
              \(pe, se) -> copyDWIMFix (patElemName pe) (map Imp.vi32 is ++ vec_is) se []

    flat_idx <-- flat_idx' + 1

  stage_1_body <- collect $ do
    forM_  (zip is $ unflattenIndex ns' $ Imp.vi32 flat_idx) $ uncurry (<--)
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
              copyDWIMFix (paramName p) [] (Var $ patElemName pe) (flat_idx' - 1 : vec_is)

          -- Read next value
          sComment "Read next values" $
            forM_ (zip (yParams scan_op) scan_res) $ \(p, se) ->
              copyDWIMFix (paramName p) [] (kernelResultSubExp se) vec_is

          compileStms mempty (bodyStms $ lamBody scan_op) $
            forM_ (zip pes (bodyResult $ lamBody scan_op)) $
              \(pe, se) -> copyDWIMFix (patElemName pe) (map Imp.vi32 is ++ vec_is) se []

  let freeVariables = namesToList $ freeIn (stage_1_prebody <> stage_1_body) `namesSubtract`
                      namesFromList (tid : [flat_idx])

  ts <- mapM lookupType freeVariables
  let freeParams = zipWith toParam freeVariables ts

  ntasks <- dPrim "ntasks" $ IntType Int32
  ntasks' <- toExp $ Var ntasks

  emit $ Imp.Op $ Imp.ParLoop Imp.Static ntasks flat_idx (product ns')
                              (Imp.MulticoreFunc freeParams stage_1_prebody stage_1_body tid)


  emit $ Imp.DebugPrint "nonsegmentedScan stage 2" Nothing
  scan_ops2 <- renameSegBinOp scan_ops
  -- |
  -- Begin stage two of scan
  dPrimV_ (segFlat space) 0
  let iter_pr_subtask = product ns' `quot` num_threads'
      remainder       = product ns' `rem` num_threads'

  dScope Nothing $ scopeOfLParams $ concatMap (lambdaParams . segBinOpLambda) scan_ops2
  forM_ scan_ops2 $ \scan_op ->
    forM_ (zip (yParams scan_op) $ segBinOpNeutral scan_op) $ \(p, ne) ->
      copyDWIMFix (paramName p) [] ne []

  offset <- dPrimV "offset" 0
  offset' <- toExp $ Var offset

  accs <- accumulatorArray scan_ops2
  forM_ (zip scan_ops2 accs) $ \(scan_op, acc) ->
    sLoopNest (segBinOpShape scan_op) $ \vec_is ->
    forM_ (zip acc $ segBinOpNeutral scan_op) $ \(acc', ne) ->
      copyDWIMFix acc' vec_is ne []

  sFor "i" (ntasks'-1) $ \i -> do
     offset <-- iter_pr_subtask
     sWhen (i .<. remainder) (offset <-- offset' + 1)
     flat_idx <-- flat_idx' + offset'
     zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_idx

     compileStms mempty (kernelBodyStms kbody) $
       forM_ (zip3 per_scan_pes scan_ops2 accs) $ \(pes, scan_op, acc) ->
         sLoopNest (segBinOpShape scan_op) $ \vec_is -> do

         sComment "Read carry in" $
           forM_ (zip (xParams scan_op) acc) $ \(p, acc') ->
             copyDWIMFix (paramName p) [] (Var acc') vec_is

         sComment "Read next values" $
           forM_ (zip (yParams scan_op) pes) $ \(p, pe) ->
             copyDWIMFix (paramName p) [] (Var $ patElemName pe) ((flat_idx'-1) : vec_is)

         compileStms mempty (bodyStms $ lamBody scan_op) $
            forM_ (zip3 acc pes (bodyResult $ lamBody scan_op)) $
              \(acc', pe, se) -> do copyDWIMFix (patElemName pe) ((flat_idx'-1) : vec_is) se []
                                    copyDWIMFix acc' vec_is se []

  -- Stage 3 : Finally each thread partially scans a chunk of the input
  --           reading it's corresponding carry-in
  scan_ops3 <- renameSegBinOp scan_ops
  accs' <- groupResultArrays (Var num_threads) scan_ops3
  stage_3_prebody <- collect $ do
    num_threads <-- num_threads'
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_idx
    dPrimV_ (segFlat space) tid'
    dScope Nothing $ scopeOfLParams $ concatMap (lambdaParams . segBinOpLambda) scan_ops3
    -- Read carry in
    let read_carry_in = forM_ (zip3 scan_ops3 accs' per_scan_pes) $ \(scan_op, acc, pes) ->
                          sLoopNest (segBinOpShape scan_op) $ \vec_is ->
                            forM_ (zip acc pes) $ \(acc', pe) ->
                              copyDWIMFix acc' (tid' : vec_is) (Var $ patElemName pe) (flat_idx' - 1 : vec_is)

        read_neutral = forM_ (zip scan_ops3 accs') $ \(scan_op, acc) ->
                         sLoopNest (segBinOpShape scan_op) $ \vec_is ->
                           forM_ (zip acc $ segBinOpNeutral scan_op) $ \(acc', ne) ->
                             copyDWIMFix acc' (tid' : vec_is) ne []

    sIf (flat_idx' .==. 0) read_neutral read_carry_in

  stage_3_body <- collect $ do
    forM_  (zip is $ unflattenIndex ns' $ Imp.vi32 flat_idx) $ uncurry (<--)
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

  let freeVariables' = namesToList $ freeIn (stage_3_prebody <> stage_3_body) `namesSubtract`
                       namesFromList (tid : [flat_idx])

  ts' <- mapM lookupType freeVariables'
  let freeParams' = zipWith toParam freeVariables' ts'

  emit $ Imp.Op $ Imp.ParLoop Imp.Static ntasks flat_idx (product ns')
                              (Imp.MulticoreFunc freeParams' stage_3_prebody stage_3_body tid)

  emit $ Imp.DebugPrint "nonsegmentedScan End" Nothing

segmentedScan :: Pattern MCMem
              -> SegSpace
              -> [SegBinOp MCMem]
              -> KernelBody MCMem
              -> MulticoreGen ()
segmentedScan pat space scan_ops kbody = do
  emit $ Imp.DebugPrint "segmented segScan" Nothing
  sUnpauseProfiling

  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  tid <- dPrim "tid" $ IntType Int32

  flat_idx <- dPrim "iter" int32

  -- iteration variable
  n_segments <- dPrim "segment_iter" $ IntType Int32
  n_segments' <- toExp $ Var n_segments

  fbody <- collect $ do
    emit $ Imp.DebugPrint "segmented segScan stage 1" Nothing
    forM_ scan_ops $ \scan_op -> do
      dScope Nothing $ scopeOfLParams $ lambdaParams $ segBinOpLambda scan_op
      let (scan_x_params, scan_y_params) = splitAt (length $ segBinOpNeutral scan_op) $ (lambdaParams . segBinOpLambda) scan_op

      forM_ (zip scan_x_params $ segBinOpNeutral scan_op) $ \(p, ne) ->
        copyDWIMFix (paramName p) [] ne []

      let inner_bound = last ns'
      sFor "i" inner_bound $ \i -> do
        dPrimV_ flat_idx (n_segments' * inner_bound + i)
        zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_idx
        compileStms mempty (kernelBodyStms kbody) $ do
          let (scan_res, map_res) = splitAt (length $ segBinOpNeutral scan_op) $ kernelBodyResult kbody
          sComment "write to-scan values to parameters" $
            forM_ (zip scan_y_params scan_res) $ \(p, se) ->
              copyDWIMFix (paramName p) [] (kernelResultSubExp se) []
          sComment "write mapped values results to global memory" $
            forM_ (zip (drop (length $ segBinOpNeutral scan_op) $ patternElements pat) map_res) $ \(pe, se) ->
              copyDWIMFix (patElemName pe) (map (`Imp.var` int32) is) (kernelResultSubExp se) []

          sComment "combine with carry and write to local memory" $
            compileStms mempty (bodyStms $ lambdaBody $ segBinOpLambda scan_op) $
            forM_ (zip3 scan_x_params (patternElements pat) (bodyResult $ lambdaBody $ segBinOpLambda scan_op)) $ \(p, pe, se) -> do
              copyDWIMFix (patElemName pe) (map (`Imp.var` int32) is)  se []
              copyDWIMFix (paramName p) [] se []


  let freeVariables = namesToList $ freeIn fbody `namesSubtract`
                                  namesFromList (tid : [n_segments])

  ts <- mapM lookupType freeVariables
  let params = zipWith toParam freeVariables ts
  let sched = decideScheduling fbody

  ntask <- dPrim "num_tasks" $ IntType Int32

  emit $ Imp.Op $ Imp.ParLoop sched ntask n_segments (product $ init ns')
                              (Imp.MulticoreFunc params mempty fbody tid)
