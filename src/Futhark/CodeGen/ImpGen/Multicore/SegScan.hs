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



makeLocalArrays :: SubExp -> [SubExp] -> Lambda ExplicitMemory
                -> MulticoreGen [VName]
makeLocalArrays num_threads nes scan_op = do
  let (scan_x_params, _scan_y_params) =
        splitAt (length nes) $ lambdaParams scan_op
  forM scan_x_params $ \p ->
    case paramAttr p of
      MemArray pt shape _ (ArrayIn mem _) -> do
        let shape' = Shape [num_threads] <> shape
        sArray "scan_arr" pt shape' $
          ArrayIn mem $ IxFun.iota $ map (primExpFromSubExp int32) $ shapeDims shape'
      _ -> do
        let pt = elemType $ paramType p
            shape = Shape [num_threads]
        sAllocArray "scan_arr" pt shape DefaultSpace


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
compileSegScan pat space scan_op nes kbody = do
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  tid <- dPrim "tid" $ IntType Int32
  tid' <- toExp $ Var tid

  -- Dummy value for now
  -- Need to postpone allocation of intermediate res for scheduler
  let num_threads = Constant $ IntValue $ Int32Value 12

  stage_one_red_res <- makeLocalArrays num_threads nes scan_op

  let (scan_x_params, scan_y_params) =
        splitAt (length nes) $ lambdaParams scan_op

  dPrimV_ (segFlat space) 0

  slug <- segScanOpSlug tid' scan_op stage_one_red_res

  prebody <- collect $
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
  stage_two_red_res <- makeLocalArrays (Var tid) nes scan_op

  -- Set neutral element value
  forM_ (zip stage_two_red_res nes) $ \(stage_two_res, ne) ->
       copyDWIMFix stage_two_res [0] ne []

  -- Let master thread accumulate "neutral elements" for each segment
  -- This is essentially a exclusive scan
  sFor "i" (ntasks'-1) $ \i -> do
    dScope Nothing $ scopeOfLParams $ lambdaParams scan_op
    forM_ (zip scan_y_params stage_one_red_res) $ \(p, se) ->
      copyDWIMFix (paramName p) [] (Var se) [i]
    forM_ (zip scan_x_params stage_two_red_res) $ \(p, se) ->
      copyDWIMFix (paramName p) [] (Var se) [i]
    compileStms mempty (bodyStms $ lambdaBody scan_op) $
      forM_ (zip stage_two_red_res $ bodyResult $ lambdaBody scan_op) $ \(arr, se) ->
        copyDWIMFix arr [i+1] se []

  prebody' <- collect $ do
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
