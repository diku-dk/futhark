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



-- | A SegScanOp with auxiliary information.
data SegScanOpSlug =
  SegScanOpSlug
  { slugOp :: SegBinOp MCMem
  , slugAccs :: [(VName, [Imp.Exp])]
    -- ^ Places to store accumulator in stage 1 reduction.
  }

slugBody :: SegScanOpSlug -> Body MCMem
slugBody = lambdaBody . segBinOpLambda . slugOp

slugParams :: SegScanOpSlug -> [LParam MCMem]
slugParams = lambdaParams . segBinOpLambda . slugOp

slugNeutral :: SegScanOpSlug -> [SubExp]
slugNeutral = segBinOpNeutral . slugOp

slugShape :: SegScanOpSlug -> Shape
slugShape = segBinOpShape . slugOp

accParams, nextParams :: SegScanOpSlug -> [LParam MCMem]
accParams slug = take (length (slugNeutral slug)) $ slugParams slug
nextParams slug = drop (length (slugNeutral slug)) $ slugParams slug

slugsComm :: [SegScanOpSlug] -> Commutativity
slugsComm = mconcat . map (segBinOpComm . slugOp)

segScanOpSlug :: Imp.Exp
             -> (SegBinOp MCMem, [VName])
             -> MulticoreGen SegScanOpSlug
segScanOpSlug local_tid (op, param_arrs) =
  SegScanOpSlug op <$> mapM (\param_arr -> return (param_arr, [local_tid])) param_arrs

-- TODO: this does not seem to work when the neutral element is not the actual neutral element
-- e.g. tests/soacs/scan-with-map.fut fails
nonsegmentedScan :: Pattern MCMem
                 -> SegSpace
                 -> [SegBinOp MCMem]
                 -> KernelBody MCMem
                 -> MulticoreGen ()
nonsegmentedScan pat space scan_ops kbody = do
  emit $ Imp.DebugPrint "nonsegmented segScan " Nothing
  sUnpauseProfiling

  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  tid <- dPrim "tid" $ IntType Int32
  tid' <- toExp $ Var tid

  num_threads <- getNumThreads
  num_threads' <- toExp $ Var num_threads

  stage_one_red_arrs <- groupResultArrays (Var num_threads) scan_ops

  dPrimV_ (segFlat space) 0

  slugs_stage_one <- mapM (segScanOpSlug tid') $ zip scan_ops stage_one_red_arrs

  sFor "i" num_threads' $ \i -> do
    tid <-- i
    sComment "SegScan - neutral-initialise the acc used by tid" $
      forM_ slugs_stage_one $ \slug ->
        forM_ (zip (slugAccs slug) (slugNeutral slug)) $ \((acc, acc_is), ne) ->
          sLoopNest (slugShape slug) $ \vec_is ->
            copyDWIMFix acc (acc_is++vec_is) ne []
  let (all_scan_res, map_res) = splitAt (segBinOpResults scan_ops) $ kernelBodyResult kbody
      per_scan_res            = segBinOpChunks scan_ops all_scan_res

  reduce_body <- collect $
    sComment "reduce (scan) body" $ do
      zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
      dScope Nothing $ scopeOfLParams $ concatMap slugParams slugs_stage_one

      compileStms mempty (kernelBodyStms kbody) $ do
        sComment "write mapped values results to memory" $ do
          let map_arrs = drop (segBinOpResults scan_ops) $ patternElements pat
          zipWithM_ (compileThreadResult space) map_arrs map_res

        forM_ (zip slugs_stage_one per_scan_res) $ \(slug, scan_res) ->
          sLoopNest (slugShape slug) $ \vec_is -> do

          forM_ (zip (accParams slug) (slugAccs slug) ) $ \(p, (acc, acc_is)) ->
            copyDWIMFix (paramName p) [] (Var acc) (acc_is++vec_is)

          forM_ (zip (nextParams slug) scan_res) $ \(p, se) ->
            copyDWIMFix (paramName p) [] (kernelResultSubExp se) vec_is

          compileStms mempty (bodyStms $ slugBody slug) $
            forM_ (zip (slugAccs slug) (bodyResult $ slugBody slug)) $
              \((acc, acc_is), se) -> copyDWIMFix acc (acc_is++vec_is) se []


  let freeVariables = namesToList $ freeIn reduce_body `namesSubtract`
                                  namesFromList (tid : [segFlat space])
  ts <- mapM lookupType freeVariables
  let freeParams = zipWith toParam freeVariables ts

  ntasks <- dPrim "ntasks" $ IntType Int32
  ntasks' <- toExp $ Var ntasks

  emit $ Imp.Op $ Imp.ParLoop Imp.Static ntasks (segFlat space) (product ns')
                              (Imp.MulticoreFunc freeParams mempty reduce_body tid)

  -- |
  -- Begin stage two of scan

  stage_two_red_arrs <- groupResultArrays (Var num_threads) scan_ops
  slugs_stage_two <- mapM (segScanOpSlug tid') $ zip scan_ops stage_two_red_arrs

  emit $ Imp.DebugPrint "nonsegmented segScan stage 2" Nothing
  -- Set neutral element value
  forM_ slugs_stage_two $ \slug ->
    forM_ (zip (slugAccs slug) $ slugNeutral slug) $ \((acc, _), ne) ->
      sLoopNest (slugShape slug) $ \vec_is ->
         copyDWIMFix acc (0 : vec_is) ne []

  -- Let master thread accumulate "neutral elements" for each segment
  -- This is essentially a exclusive scan
  sWhen (ntasks' .>. 0) $ do
    dScope Nothing $ scopeOfLParams $ concatMap slugParams slugs_stage_two
    sFor "i" (ntasks'-1) $ \i -> do
      tid <-- i
      forM_ (zip slugs_stage_two slugs_stage_one) $ \(slug2, slug1) ->
        sLoopNest (slugShape slug1) $ \vec_is -> do
        -- Load next params
        sComment "load next params" $
          forM_ (zip (nextParams slug2) (slugAccs slug1)) $ \(p, (acc, acc_is)) ->
            copyDWIMFix (paramName p) [] (Var acc) (acc_is++vec_is)
        sComment "load acc params" $
          forM_ (zip (accParams slug2) (slugAccs slug2)) $ \(p, (acc, acc_is)) ->
          copyDWIMFix (paramName p) [] (Var acc) (acc_is++vec_is)

        compileStms mempty (bodyStms $ slugBody slug1) $
          forM_ (zip (slugAccs slug2) (bodyResult $ slugBody slug1)) $ \((acc, acc_is), se) ->
            copyDWIMFix acc (i+1 : tail acc_is ++ vec_is) se []

  let per_scan_pes = segBinOpChunks scan_ops $ patternValueElements pat

  -- Do first iteration here
  scan_prebody <- collect $ do
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space

    dScope Nothing $ scopeOfLParams $ concatMap slugParams slugs_stage_two

    compileStms mempty (kernelBodyStms kbody) $ do
      forM_ (zip3 per_scan_res per_scan_pes slugs_stage_two) $ \(scan_res, pes, slug) -> do
        let (scan_x_params, scan_y_params) = splitAt (length $ slugNeutral slug) $ slugParams slug
        sLoopNest (slugShape slug) $ \vec_is -> do
          sComment "Load x params" $  -- next value
            forM_ (zip scan_x_params scan_res) $ \(p, se) ->
              copyDWIMFix (paramName p) [] (kernelResultSubExp se) vec_is
          sComment "Load y params" $
            forM_ (zip scan_y_params (slugAccs slug)) $ \(p, (acc, acc_is)) ->
                copyDWIMFix (paramName p) [] (Var acc) (acc_is++vec_is)

          compileStms mempty (bodyStms $ slugBody slug) $
            forM_ (zip pes $ bodyResult $ slugBody slug) $ \(pe, se) ->
              copyDWIMFix (patElemName pe) (map Imp.vi32 is++vec_is) se []

      iter <- toExp $ Var $ segFlat space
      segFlat space <-- iter + 1

  scan_body <- collect $ do
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
    sComment "scan body" $
      compileStms mempty (kernelBodyStms kbody) $
        forM_ (zip3 per_scan_res per_scan_pes slugs_stage_two) $ \(scan_res, pes, slug) -> do
          let (scan_x_params, scan_y_params) = splitAt (length $ slugNeutral slug) $ slugParams slug
          sLoopNest (slugShape slug) $ \vec_is -> do
            sComment "Load x params" $  -- next value
              forM_ (zip scan_x_params scan_res) $ \(p, se) ->
                copyDWIMFix (paramName p) [] (kernelResultSubExp se) vec_is

            test_idx  <- dPrimV "carry_in_flat_idx" $ product (map Imp.vi32 is) - 1
            test_idx' <- toExp $ Var test_idx

            sComment "Load y params" $
              forM_ (zip scan_y_params pes) $ \(p, pe) ->
                copyDWIMFix (paramName p) [] (Var $ patElemName pe) (test_idx' : vec_is)

            compileStms mempty (bodyStms $ slugBody slug) $
              forM_ (zip pes $ bodyResult $ slugBody slug) $ \(pe, se) ->
                copyDWIMFix (patElemName pe) (map Imp.vi32 is++vec_is) se []


  let freeVariables' = namesToList (freeIn (scan_prebody <> scan_body) `namesSubtract`
                     namesFromList [tid, segFlat space])

  ts' <- mapM lookupType freeVariables'
  let freeParams' = zipWith toParam freeVariables' ts'

  emit $ Imp.DebugPrint "nonsegmented segScan stage 3" Nothing
  emit $ Imp.Op $ Imp.ParLoop Imp.Static ntasks (segFlat space) (product ns')
                             (Imp.MulticoreFunc freeParams' scan_prebody scan_body tid)

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
        dPrimV_ (segFlat space) (n_segments' * inner_bound + i)
        zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
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

  ntask <- dPrim "num_tasks" $ IntType Int32

  emit $ Imp.Op $ Imp.ParLoop Imp.Static ntask n_segments (product $ init ns')
                              (Imp.MulticoreFunc params mempty fbody tid)
