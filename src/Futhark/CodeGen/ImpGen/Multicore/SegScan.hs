module Futhark.CodeGen.ImpGen.Multicore.SegScan
  (compileSegScan
  )
  where

import Control.Monad
import Data.List
import Prelude hiding (quot, rem)

import qualified Futhark.CodeGen.ImpCode.Multicore as Imp
import Futhark.CodeGen.ImpGen
import Futhark.Representation.ExplicitMemory
import Futhark.CodeGen.ImpGen.Multicore.Base


allocIntermediateArrays :: Space
                     -> Count u SubExp
                     -> [Type]
                     -> ImpM lore2 op [VName]
allocIntermediateArrays space (Count size) types =
  forM types $ \t -> do
    let pt = elemType t
        full_shape = Shape [size] <> arrayShape t
        -- Move the groupsize dimension last to ensure coalesced
        -- memory access.
        perm = [1..shapeRank full_shape-1] ++ [0]
    sAllocArrayPerm "group_res_arr" pt full_shape space perm


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




compileSegScan :: Pattern ExplicitMemory
                -> SegSpace
                -> Lambda ExplicitMemory
                -> [SubExp]
                -> KernelBody ExplicitMemory
                -> MulticoreGen ()
compileSegScan pat space lore nes kbody = do
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  tid <- dPrim "tid" $ IntType Int32
  tid' <- toExp $ Var tid

  -- Dummy value for now
  -- Need to postpone allocation of intermediate res for scheduler
  let num_threads = Count $ Constant $ IntValue $ Int32Value 10
  stage_one_red_res <- allocIntermediateArrays DefaultSpace num_threads $ lambdaReturnType lore

  let (scan_x_params, scan_y_params) =
        splitAt (length nes) $ lambdaParams lore

  dPrim_ (segFlat space) (IntType Int32)

  slug <- segScanOpSlug tid' lore stage_one_red_res

  prebody <- collect $ do
    dScope Nothing $ scopeOfLParams $ slugParams slug
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
    compileKBody kbody $ \kbody_res ->
      forM_ (zip  (slugAccs slug) kbody_res) $ \((acc, acc_is), (res, res_is)) ->
        copyDWIMFix acc acc_is res res_is

    emit $ Imp.SetScalar (segFlat space) (Imp.vi32 (segFlat space) + 1)


  reduce_body <- collect $ do
    forM_ is $ \is' ->
      copyDWIMFix is' [] (Var $ segFlat space) []

    compileStms mempty (kernelBodyStms kbody) $ do
      let scan_res = take (length nes) $ kernelBodyResult kbody
      -- Load new value
      forM_ (zip scan_y_params scan_res) $ \(p, se) ->
        copyDWIMFix (paramName p) [] (kernelResultSubExp se) []
      -- Load reduce accumulator
      forM_ (zip scan_x_params $ slugAccs slug) $ \(p, (acc, acc_is)) ->
        copyDWIMFix (paramName p) [] (Var acc) acc_is
      compileStms mempty (bodyStms $ lambdaBody lore) $
        forM_ (zip (slugAccs slug) $ bodyResult $ lambdaBody lore) $ \((acc, acc_is), se) ->
          copyDWIMFix acc acc_is se []

  let paramsNames = namesToList $ (freeIn reduce_body `namesIntersection` freeIn prebody) `namesSubtract`
                                  namesFromList (tid : [segFlat space])
  ts <- mapM lookupType paramsNames
  let params = zipWith toParam paramsNames ts


  ntasks <- dPrim "ntasks" $ IntType Int32
  ntasks' <- toExp $ Var ntasks

  emit $ Imp.Op $ Imp.ParLoop ntasks (segFlat space) (product ns')
                              (Imp.MulticoreFunc params prebody reduce_body tid)


  -- |
  -- Begin stage two of scan
  stage_two_red_res <- allocIntermediateArrays DefaultSpace (Count $ Var tid) $ lambdaReturnType lore

  -- Set neutral element value
  dScope Nothing $ scopeOfLParams $ lambdaParams lore
  forM_ (zip stage_two_red_res nes) $ \(stage_two_res, ne) ->
       copyDWIMFix  stage_two_res [] ne []

  -- Let master thread accumulate "neutral elements" for each segment
  -- This is essentially a exclusive scan
  sFor "i" (ntasks'-1) $ \i -> do
    forM_ (zip scan_y_params stage_one_red_res) $ \(p, se) ->
      copyDWIMFix (paramName p) [] (Var se) [i]
    forM_ (zip scan_x_params stage_two_red_res) $ \(p, se) ->
      copyDWIMFix (paramName p) [] (Var se) [i]
    compileStms mempty (bodyStms $ lambdaBody lore) $
      forM_ (zip stage_two_red_res $ bodyResult $ lambdaBody lore) $ \(arr, se) ->
        copyDWIMFix arr [i+1] se []


  -- Prepare function body for second scan iteration
  prebody' <- collect $ do
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
    dScope Nothing $ scopeOfLParams $ lambdaParams lore
    -- Set neutral element
    forM_ (zip3  (patternElements pat) scan_x_params stage_two_red_res) $ \(pe, p, ne) -> do
       copyDWIMFix  (paramName p) [] (Var ne) [tid']
       copyDWIMFix (patElemName pe) (map Imp.vi32 is) (Var $ paramName p) []

    -- Read initial value
    compileStms mempty (kernelBodyStms kbody) $ do
      let scan_res = take (length nes) $ kernelBodyResult kbody
      forM_ (zip scan_y_params scan_res) $ \(p, se) ->
        copyDWIMFix (paramName p) [] (kernelResultSubExp se) []
    -- sComment "combine with carry and write back to res and accum" $
    compileStms mempty (bodyStms $ lambdaBody lore) $
      forM_ (zip3  scan_x_params (patternElements pat) $ bodyResult $ lambdaBody lore) $ \(p, pe, se) -> do
       copyDWIMFix (patElemName pe) (map Imp.vi32 is) se []
       copyDWIMFix (paramName p) [] se []

    emit $ Imp.SetScalar (segFlat space) (Imp.vi32 (segFlat space) + 1)

  scan_body <- collect $ do
     forM_ is $ \is' ->
       copyDWIMFix is' [] (Var $ segFlat space) []

     compileStms mempty (kernelBodyStms kbody) $ do
       let (scan_res, _map_res) = splitAt (length nes) $ kernelBodyResult kbody
       forM_ (zip scan_y_params scan_res) $ \(p, se) ->
         copyDWIMFix (paramName p) [] (kernelResultSubExp se) []

     compileStms mempty (bodyStms $ lambdaBody lore) $
       forM_ (zip3  scan_x_params (patternElements pat) $ bodyResult $ lambdaBody lore) $ \(p, pe, se) -> do
         copyDWIMFix (patElemName pe) (map Imp.vi32 is) se []
         copyDWIMFix (paramName p) [] se []

  stage_two_red_arr <- mapM lookupArray stage_two_red_res
  let stage_two_red_arr_names  = map (memLocationName . entryArrayLocation) stage_two_red_arr

  let paramsNames' = stage_two_red_arr_names ++
                     namesToList ((freeIn scan_body `namesIntersection` freeIn prebody') `namesSubtract`
                     namesFromList (tid: segFlat space : map paramName (lambdaParams lore) ++ is))

  ts' <- mapM lookupType paramsNames'
  let params' = zipWith toParam paramsNames' ts'

  emit $ Imp.Op $ Imp.ParLoop ntasks (segFlat space) (product ns')
                             (Imp.MulticoreFunc params' prebody' scan_body tid)
