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


compileSegScan :: Pattern ExplicitMemory
                -> SegSpace
                -> Lambda ExplicitMemory
                -> [SubExp]
                -> KernelBody ExplicitMemory
                -> ImpM ExplicitMemory Imp.Multicore ()
compileSegScan pat space lore subexps kbody = do
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  num_tasks <- dPrim "num_tasks" $ IntType Int32
  num_tasks' <- toExp $ Var num_tasks

  -- Dummy value for now
  -- Need to postpone allocation of intermediate res for scheduler
  let num_threads = Count $ Constant $ IntValue $ Int32Value 10
  stage_one_red_res <- allocIntermediateArrays DefaultSpace num_threads $ lambdaReturnType lore

  let (scan_x_params, scan_y_params) =
        splitAt (length subexps) $ lambdaParams lore

  dPrim_ (segFlat space) (IntType Int32)

  prebody <- collect $ do
    dScope Nothing $ scopeOfLParams $ lambdaParams lore
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
    compileKBody kbody $ \all_red_res ->
      forM_ (zip stage_one_red_res all_red_res) $ \(slug_arr, (res, res_is)) ->
        copyDWIMFix slug_arr [num_tasks'] res res_is

    emit $ Imp.SetScalar (segFlat space) (Imp.vi32 (segFlat space) + 1)


  reduce_body <- collect $ do
    forM_ is $ \is' ->
      copyDWIMFix is' [] (Var $ segFlat space) []

    compileStms mempty (kernelBodyStms kbody) $ do
      let scan_res = take (length subexps) $ kernelBodyResult kbody
      -- Load new value
      forM_ (zip scan_y_params scan_res) $ \(p, se) ->
        copyDWIMFix (paramName p) [] (kernelResultSubExp se) []
      -- Load reduce accumulator
      forM_ (zip scan_x_params stage_one_red_res) $ \(p, slug_arr) ->
        copyDWIMFix (paramName p) [] (Var slug_arr) [num_tasks']
      compileStms mempty (bodyStms $ lambdaBody lore) $
        forM_ (zip stage_one_red_res $ bodyResult $ lambdaBody lore) $ \(res_arr, se) ->
          copyDWIMFix res_arr [num_tasks'] se []

  let paramsNames = namesToList $ (freeIn reduce_body `namesIntersection` freeIn prebody) `namesSubtract`
                                  namesFromList (num_tasks : [segFlat space])
  ts <- mapM lookupType paramsNames
  let params = zipWith toParam paramsNames ts

  emit $ Imp.Op $ Imp.ParLoop (segFlat space) (product ns')
                              (Imp.MulticoreFunc params prebody reduce_body num_tasks)


  -- |
  -- Begin stage two of scan
  stage_two_red_res <- allocIntermediateArrays DefaultSpace (Count $ Var num_tasks) $ lambdaReturnType lore

  -- Set neutral element value
  dScope Nothing $ scopeOfLParams $ lambdaParams lore
  forM_ (zip stage_two_red_res subexps) $ \(two_res, ne) ->
       copyDWIMFix  two_res [] ne []

  -- Let master thread accumulate "neutral elements" for each segment
  -- This is essentially a exclusive scan
  sFor "i" (num_tasks'-1) $ \i -> do
    forM_ (zip scan_y_params stage_one_red_res) $ \(p, se) ->
      copyDWIMFix (paramName p) [] (Var se) [i]
    forM_ (zip scan_x_params stage_two_red_res) $ \(p, se) ->
      copyDWIMFix (paramName p) [] (Var se) [i]
    compileStms mempty (bodyStms $ lambdaBody lore) $
      forM_ (zip stage_two_red_res $ bodyResult $ lambdaBody lore) $ \(scan_arr, se) ->
        copyDWIMFix scan_arr [i+1] se []


  -- Prepare function body for second scan iteration
  prebody' <- collect $ do
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
    dScope Nothing $ scopeOfLParams $ lambdaParams lore
    -- Set neutral element
    forM_ (zip3  (patternElements pat) scan_x_params stage_two_red_res) $ \(pe, p, ne) -> do
       copyDWIMFix  (paramName p) [] (Var ne) [num_tasks']
       copyDWIMFix (patElemName pe) (map Imp.vi32 is) (Var $ paramName p) []

    -- Read initial value
    compileStms mempty (kernelBodyStms kbody) $ do
      let scan_res = take (length subexps) $ kernelBodyResult kbody
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
       let (scan_res, _map_res) = splitAt (length subexps) $ kernelBodyResult kbody
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
                     namesFromList (num_tasks: segFlat space : map paramName (lambdaParams lore) ++ is))

  ts' <- mapM lookupType paramsNames'
  let params' = zipWith toParam paramsNames' ts'

  emit $ Imp.Op $ Imp.ParLoop (segFlat space) (product ns')
                              (Imp.MulticoreFunc params' prebody' scan_body num_tasks)
