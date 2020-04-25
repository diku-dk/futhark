module Futhark.CodeGen.ImpGen.Multicore.SegScan
  (compileSegScan
  )
  where

import Control.Monad
import Data.List
import Prelude hiding (quot, rem)

import qualified Futhark.CodeGen.ImpCode.Multicore as Imp
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun

import Futhark.CodeGen.ImpGen
import Futhark.Representation.ExplicitMemory
import Futhark.CodeGen.ImpGen.Multicore.Base

import Futhark.Transform.Rename


createTemporaryArrays :: SubExp -> [SubExp] -> Lambda ExplicitMemory
                -> MulticoreGen [VName]
createTemporaryArrays num_threads nes scan_op = do
  let (scan_x_params, _scan_y_params) =
        splitAt (length nes) $ lambdaParams scan_op
  forM scan_x_params $ \p ->
    case paramAttr p of
      MemArray pt shape _ (ArrayIn mem _) -> do
        let shape' = Shape [num_threads] <> shape
        sArray "scan_arr_mem" pt shape' $
          ArrayIn mem $ IxFun.iota $ map (primExpFromSubExp int32) $ shapeDims shape'
      _ -> do
        let pt = elemType $ paramType p
            shape = Shape [num_threads]
        sAllocArray "scan_arr_p" pt shape DefaultSpace

compileSegScan :: Pattern ExplicitMemory
                -> SegSpace
                -> Lambda ExplicitMemory
                -> [SubExp]
                -> KernelBody ExplicitMemory
                -> MulticoreGen ()
compileSegScan pat space scan_op nes kbody
  | segment_depth <- unSegSpace space,
    length segment_depth == 1 =
      nonsegmentedScan pat space scan_op nes kbody
  | otherwise =
      segmentedScan pat space scan_op nes kbody


segmentedScan :: Pattern ExplicitMemory
                -> SegSpace
                -> Lambda ExplicitMemory
                -> [SubExp]
                -> KernelBody ExplicitMemory
                -> MulticoreGen ()
segmentedScan pat space scan_op nes kbody = do
  emit $ Imp.DebugPrint "segmented segScan" Nothing
  sUnpauseProfiling

  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  tid <- dPrim "tid" $ IntType Int32

  let (scan_x_params, scan_y_params) =
        splitAt (length nes) $ lambdaParams scan_op


  -- iteration variable
  n_segments <- dPrim "segment_iter" $ IntType Int32
  n_segments' <- toExp $ Var n_segments


  fbody <- collect $ do
    emit $ Imp.DebugPrint "segmented segScan stage 1" Nothing
    dScope Nothing $ scopeOfLParams $ lambdaParams scan_op
    forM_ (zip scan_x_params nes) $ \(p, ne) ->
      copyDWIMFix (paramName p) [] ne []

    let inner_bound = last ns'
    sFor "i" inner_bound $ \i -> do
      dPrimV_ (segFlat space) (n_segments' * inner_bound + i)
      zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
      compileStms mempty (kernelBodyStms kbody) $ do
        let (scan_res, map_res) = splitAt (length nes) $ kernelBodyResult kbody
        sComment "write to-scan values to parameters" $
          forM_ (zip scan_y_params scan_res) $ \(p, se) ->
          copyDWIMFix (paramName p) [] (kernelResultSubExp se) []
        sComment "write mapped values results to global memory" $
          forM_ (zip (drop (length nes) $ patternElements pat) map_res) $ \(pe, se) ->
          copyDWIMFix (patElemName pe) (map (`Imp.var` int32) is)
          (kernelResultSubExp se) []

      sComment "combine with carry and write to local memory" $
        compileStms mempty (bodyStms $ lambdaBody scan_op) $
        forM_ (zip3 scan_x_params (patternElements pat) (bodyResult $ lambdaBody scan_op)) $ \(p, pe, se) -> do
          copyDWIMFix (patElemName pe) (map (`Imp.var` int32) is)  se []
          copyDWIMFix (paramName p) [] se []


  let freeVariables = namesToList $ freeIn fbody `namesSubtract`
                                  namesFromList (tid : [n_segments])

  ts <- mapM lookupType freeVariables
  let params = zipWith toParam freeVariables ts

  ntask <- dPrim "num_tasks" $ IntType Int32

  emit $ Imp.Op $ Imp.ParLoop Imp.Static ntask n_segments (product $ init ns')
                              (Imp.MulticoreFunc params mempty fbody tid)



-- | A SegScanOp with auxiliary information.
data SegScanOpSlug =
  SegScanOpSlug
  { slugOp :: Lambda ExplicitMemory
  , slugAccs :: [(VName, [Imp.Exp])]
    -- ^ Places to store accumulator in stage 1 reduction.
  }

slugParams :: SegScanOpSlug -> [LParam ExplicitMemory]
slugParams = lambdaParams . slugOp

segScanOpSlug :: Imp.Exp
             -> Lambda ExplicitMemory
             -> [VName]
             -> MulticoreGen SegScanOpSlug
segScanOpSlug local_tid op param_arrs =
  SegScanOpSlug op <$>
  mapM mkAcc param_arrs
  where mkAcc param_arr = return (param_arr, [local_tid])


nonsegmentedScan :: Pattern ExplicitMemory
                 -> SegSpace
                 -> Lambda ExplicitMemory
                 -> [SubExp]
                 -> KernelBody ExplicitMemory
                 -> MulticoreGen ()
nonsegmentedScan pat space scan_op nes kbody = do
  emit $ Imp.DebugPrint "nonsegmented segScan " Nothing
  sUnpauseProfiling

  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  tid <- dPrim "tid" $ IntType Int32
  tid' <- toExp $ Var tid

  num_threads <- getNumThreads

  stage_one_red_res <- createTemporaryArrays (Var num_threads) nes scan_op

  let (scan_x_params, scan_y_params) =
        splitAt (length nes) $ lambdaParams scan_op

  dPrimV_ (segFlat space) 0

  slug <- segScanOpSlug tid' scan_op stage_one_red_res

  prebody <- collect $ do
    emit $ Imp.DebugPrint "nonsegmented segScan stage 1" Nothing
    sComment "neutral-initialise the acc used by this thread" $
      forM_ (zip (slugAccs slug) nes) $ \((acc, acc_is), ne) ->
        copyDWIMFix acc acc_is ne []


  reduce_body <- collect $
    sComment "reduce body" $ do
      zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
      dScope Nothing $ scopeOfLParams $ slugParams slug

      compileStms mempty (kernelBodyStms kbody) $ do
        let (scan_res, map_res) = splitAt (length nes) $ kernelBodyResult kbody
        sComment "write mapped values results to global memory" $
         forM_ (zip (drop (length nes) $ patternElements pat) map_res) $ \(pe, se) ->
           copyDWIMFix (patElemName pe) (map (`Imp.var` int32) is) (kernelResultSubExp se) []

        -- Load new value
        forM_ (zip scan_y_params scan_res) $ \(p, se) ->
          copyDWIMFix (paramName p) [] (kernelResultSubExp se) []
        -- Load reduce accumulator
        forM_ (zip scan_x_params $ slugAccs slug) $ \(p, (acc, acc_is)) ->
          copyDWIMFix (paramName p) [] (Var acc) acc_is
        compileStms mempty (bodyStms $ lambdaBody scan_op) $
          forM_ (zip (slugAccs slug) $ bodyResult $ lambdaBody scan_op) $ \((acc, acc_is), se) ->
            copyDWIMFix acc acc_is se []

  let freeVariables = namesToList $ freeIn (prebody <> reduce_body) `namesSubtract`
                                  namesFromList (tid : [segFlat space])
  ts <- mapM lookupType freeVariables
  let freeParams = zipWith toParam freeVariables ts

  ntasks <- dPrim "ntasks" $ IntType Int32
  ntasks' <- toExp $ Var ntasks

  emit $ Imp.Op $ Imp.ParLoop Imp.Static ntasks (segFlat space) (product ns')
                              (Imp.MulticoreFunc freeParams prebody reduce_body tid)

  -- |
  -- Begin stage two of scan
  scan_op' <- renameLambda scan_op
  let (scan_x_params', scan_y_params') =
        splitAt (length nes) $ lambdaParams scan_op'

  stage_two_red_res <- createTemporaryArrays (Var ntasks) nes scan_op'

  -- Set neutral element value
  forM_ (zip stage_two_red_res nes) $ \(stage_two_res, ne) ->
       copyDWIMFix stage_two_res [0] ne []

  -- Let master thread accumulate "neutral elements" for each segment
  -- This is essentially a exclusive scan
  emit $ Imp.DebugPrint "nonsegmented segScan stage 2" Nothing
  sWhen (ntasks' .>. 0) $
    sFor "i" (ntasks'-1) $ \i -> do
      dScope Nothing $ scopeOfLParams $ lambdaParams scan_op'
      forM_ (zip scan_x_params' stage_two_red_res) $ \(p, se) ->
        copyDWIMFix (paramName p) [] (Var se) [i]
      forM_ (zip scan_y_params' stage_one_red_res) $ \(p, se) ->
        copyDWIMFix (paramName p) [] (Var se) [i]
      compileStms mempty (bodyStms $ lambdaBody scan_op') $
        forM_ (zip stage_two_red_res $ bodyResult $ lambdaBody scan_op') $ \(arr, se) ->
          copyDWIMFix arr [i+1] se []

  prebody' <- collect $ do
    emit $ Imp.DebugPrint "nonsegmented segScan stage 3" Nothing
    dScope Nothing $ scopeOfLParams $ lambdaParams scan_op'
    forM_ (zip scan_x_params' stage_two_red_res) $ \(p, ne) ->
       copyDWIMFix (paramName p) [] (Var ne) [tid']

  scan_body <- collect $ do
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
    compileStms mempty (kernelBodyStms kbody) $ do
      let scan_res = take (length nes) $ kernelBodyResult kbody
      forM_ (zip scan_y_params' scan_res) $ \(p, se) ->
         copyDWIMFix (paramName p) [] (kernelResultSubExp se) []
      compileStms mempty (bodyStms $ lambdaBody scan_op') $
       forM_ (zip3 scan_x_params' (patternElements pat) $ bodyResult $ lambdaBody scan_op')
       $ \(p, pe, se) -> do
         copyDWIMFix (paramName p) [] se []
         copyDWIMFix (patElemName pe) (map Imp.vi32 is) se []

  let freeVariables' = namesToList (freeIn (prebody' <> scan_body) `namesSubtract`
                     namesFromList [tid, segFlat space])

  ts' <- mapM lookupType freeVariables'
  let freeParams' = zipWith toParam freeVariables' ts'

  emit $ Imp.Op $ Imp.ParLoop Imp.Static ntasks (segFlat space) (product ns')
                             (Imp.MulticoreFunc freeParams' prebody' scan_body tid)
