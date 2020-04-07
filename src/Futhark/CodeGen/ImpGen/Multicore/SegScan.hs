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


makeLocalArrays :: SubExp -> [SubExp] -> Lambda ExplicitMemory
                -> MulticoreGen [VName]
makeLocalArrays num_threads nes scan_op = do
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


  let paramsNames = namesToList $ freeIn fbody `namesSubtract`
                                  namesFromList (tid : [n_segments])

  ts <- mapM lookupType paramsNames
  let params = zipWith toParam paramsNames ts

  ntask <- dPrim "num_tasks" $ IntType Int32

  emit $ Imp.Op $ Imp.ParLoop ntask n_segments (product $ init ns')
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
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  tid <- dPrim "tid" $ IntType Int32
  tid' <- toExp $ Var tid

  num_threads <- getNumThreads

  stage_one_red_res <- makeLocalArrays (Var num_threads) nes scan_op

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

  let paramsNames = namesToList $ freeIn (reduce_body <> prebody) `namesSubtract`
                                  namesFromList (tid : [segFlat space])
  ts <- mapM lookupType paramsNames
  let params = zipWith toParam paramsNames ts


  ntasks <- dPrim "ntasks" $ IntType Int32
  ntasks' <- toExp $ Var ntasks

  emit $ Imp.Op $ Imp.ParLoop ntasks (segFlat space) (product ns')
                              (Imp.MulticoreFunc params prebody reduce_body tid)

  -- |
  -- Begin stage two of scan
  scan_op' <- renameLambda scan_op
  let (scan_x_params', scan_y_params') =
        splitAt (length nes) $ lambdaParams scan_op'

  stage_two_red_res <- makeLocalArrays (Var ntasks) nes scan_op'

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
    dScope Nothing $ scopeOfLParams $ lambdaParams scan_op
    forM_ (zip scan_x_params stage_two_red_res) $ \(p, ne) ->
       copyDWIMFix (paramName p) [] (Var ne) [tid']

  scan_body <- collect $ do
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
    compileStms mempty (kernelBodyStms kbody) $ do
      let scan_res = take (length nes) $ kernelBodyResult kbody
      forM_ (zip scan_y_params scan_res) $ \(p, se) ->
         copyDWIMFix (paramName p) [] (kernelResultSubExp se) []
      compileStms mempty (bodyStms $ lambdaBody scan_op) $
       forM_ (zip3 scan_x_params (patternElements pat) $ bodyResult $ lambdaBody scan_op)
       $ \(p, pe, se) -> do
         copyDWIMFix (paramName p) [] se []
         copyDWIMFix (patElemName pe) (map Imp.vi32 is) se []

  let paramsNames' = namesToList (freeIn (scan_body <> prebody') `namesSubtract`
                     namesFromList (tid : segFlat space : map paramName (lambdaParams scan_op)))

  ts' <- mapM lookupType paramsNames'
  let params' = zipWith toParam paramsNames' ts'

  emit $ Imp.Op $ Imp.ParLoop ntasks (segFlat space) (product ns')
                             (Imp.MulticoreFunc params' prebody' scan_body tid)



  -- -- Dummy value for now
  -- -- Need to postpone allocation of intermediate res for scheduler
  -- let num_threads = Constant $ IntValue $ Int32Value 12
  --     num_elements = product ns'

  -- -- This assumes
  -- -- 1) all inner dims are Same
  -- -- 2) inner dims > 0
  -- let newSegment =
  --       case reverse ns' of
  --         segment_size : _ : _ -> Just $ \to ->
  --           (segment_size-1) .==. (to `rem` segment_size)
  --         _ -> Nothing


  -- local_arrs <- makeLocalArrays num_threads nes scan_op

  -- let (scan_x_params, scan_y_params) =
  --       splitAt (length nes) $ lambdaParams scan_op

  -- dScope Nothing $ scopeOfLParams $ lambdaParams scan_op
  -- forM_ (zip scan_x_params nes) $ \(p, ne) ->
  --   copyDWIMFix (paramName p) [] ne []


  -- sFor "i" (product ns') $ \i -> do
  --   dPrimV_ (segFlat space) i

  --   zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
  --   compileStms mempty (kernelBodyStms kbody) $ do
  --     let (scan_res, map_res) = splitAt (length nes) $ kernelBodyResult kbody
  --     sComment "write to-scan values to parameters" $
  --       forM_ (zip scan_y_params scan_res) $ \(p, se) ->
  --       copyDWIMFix (paramName p) [] (kernelResultSubExp se) []
  --     sComment "write mapped values results to global memory" $
  --       forM_ (zip (drop (length nes) $ patternElements pat) map_res) $ \(pe, se) ->
  --       copyDWIMFix (patElemName pe) (map (`Imp.var` int32) is)
  --       (kernelResultSubExp se) []
  --   sComment "combine with carry and write to local memory" $
  --     compileStms mempty (bodyStms $ lambdaBody scan_op) $
  --     forM_ (zip local_arrs (bodyResult $ lambdaBody scan_op)) $ \(arr, se) ->
  --       copyDWIMFix arr [tid'] se []


  --   sComment "threads write partial scan result" $
  --     forM_ (zip (patternElements pat) local_arrs) $ \(pe, arr) ->
  --       copyDWIMFix (patElemName pe) (map (`Imp.var` int32) is) (Var arr) [tid']


  --   let load_carry =
  --         forM_ (zip local_arrs scan_x_params) $ \(arr, p) ->
  --         copyDWIMFix (paramName p) [] (Var arr) [tid']
  --       load_neutral =
  --         forM_ (zip nes scan_x_params) $ \(ne, p) ->
  --         copyDWIMFix (paramName p) [] ne []

  --   sComment "first thread reads last element as carry-in for next iteration" $ do
  --     new_segment <- dPrimVE "new_segment" $
  --       case newSegment of
  --         Nothing -> false
  --         Just f -> f (Imp.var (segFlat space) int32)
  --     should_load_carry <- dPrimVE "should_load_carry" $ UnOpExp Not new_segment
  --     sIf should_load_carry load_carry load_neutral
