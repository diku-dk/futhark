module Futhark.CodeGen.ImpGen.Multicore.SegRed
  (compileSegRed
  )
  where

import Control.Monad
import Data.List
import Prelude hiding (quot, rem)

import qualified Futhark.CodeGen.ImpCode.Multicore as Imp
import Futhark.CodeGen.ImpGen
import Futhark.Representation.ExplicitMemory
import Futhark.Util (chunks)
import Futhark.CodeGen.ImpGen.Multicore.Base


-- Compile SegReduce
-- 1. This can't handle multidimensional reductions (e.g. tests/soacs/reduce4.fut)
compileSegRed :: Pattern ExplicitMemory
                 -> SegLevel -> SegSpace
                 -> [SegRedOp ExplicitMemory]
                 -> KernelBody ExplicitMemory
                 -> MulticoreGen ()
compileSegRed pat lvl space reds kbody
  | [(_, Constant (IntValue (Int32Value 1))), _] <- unSegSpace space =
      nonsegmentedReduction pat lvl space reds kbody
  | otherwise =
      segmentedReduction pat space reds kbody

compileKBodyRed :: Pattern ExplicitMemory
             -> SegSpace
             -> (KernelBody ExplicitMemory)
             -> [SegRedOp ExplicitMemory]
             -> ([(SubExp, [Imp.Exp])] -> ImpM ExplicitMemory Imp.Multicore ())
             -> ImpM ExplicitMemory Imp.Multicore ()
compileKBodyRed pat space kbody reds red_cont =
  compileStms mempty (kernelBodyStms kbody) $ do
    let (red_res, map_res) = splitAt (segRedResults reds) $ kernelBodyResult kbody

    sComment "save map-out results" $ do
      let map_arrs = drop (segRedResults reds) $ patternElements pat
      zipWithM_ (compileThreadResult space ) map_arrs map_res

    red_cont $ zip (map kernelResultSubExp red_res) $ repeat []


-- | Arrays for storing group results.
--
-- The group-result arrays have an extra dimension (of size groupsize)
-- because they are also used for keeping vectorised accumulators for
-- first-stage reduction, if necessary.  When actually storing group
-- results, the first index is set to 0.
groupResultArrays :: Count NumGroups SubExp
                  -> [SegRedOp ExplicitMemory]
                  -> MulticoreGen [[VName]]
groupResultArrays (Count num_threads) reds =
  forM reds $ \(SegRedOp _ lam _ shape) ->
    forM (lambdaReturnType lam) $ \t -> do
    let pt = elemType t
        full_shape = Shape [num_threads] <> shape <> arrayShape t
        perm = [1..shapeRank full_shape-1] ++ [0]
    sAllocArrayPerm "group_res_arr" pt full_shape DefaultSpace perm


-- | A SegRedOp with auxiliary information.
data SegRedOpSlug =
  SegRedOpSlug
  { slugOp :: SegRedOp ExplicitMemory
  , slugAccs :: [(VName, [Imp.Exp])]
    -- ^ Places to store accumulator in stage 1 reduction.
  }


slugBody :: SegRedOpSlug -> Body ExplicitMemory
slugBody = lambdaBody . segRedLambda . slugOp

slugParams :: SegRedOpSlug -> [LParam ExplicitMemory]
slugParams = lambdaParams . segRedLambda . slugOp

slugNeutral :: SegRedOpSlug -> [SubExp]
slugNeutral = segRedNeutral . slugOp

slugShape :: SegRedOpSlug -> Shape
slugShape = segRedShape . slugOp

accParams, nextParams :: SegRedOpSlug -> [LParam ExplicitMemory]
accParams slug = take (length (slugNeutral slug)) $ slugParams slug
nextParams slug = drop (length (slugNeutral slug)) $ slugParams slug


segRedOpSlug :: Imp.Exp
             -> (SegRedOp ExplicitMemory, [VName])
             -> MulticoreGen SegRedOpSlug
segRedOpSlug local_tid (op, param_arrs) =
  SegRedOpSlug op <$> mapM (\param_arr -> return (param_arr, [local_tid])) param_arrs


nonsegmentedReduction :: Pattern ExplicitMemory
                      -> SegLevel
                      -> SegSpace
                      -> [SegRedOp ExplicitMemory]
                      -> KernelBody ExplicitMemory
                      -> MulticoreGen ()
nonsegmentedReduction pat _ space reds kbody = do
  let (is, ns) = unzip $ unSegSpace space

  -- Dummy value for now
  -- Need to postpone allocation of intermediate res for scheduler
  let num_threads = Count $ Constant $ IntValue $ Int32Value 12
  ns' <- mapM toExp ns
  stage_one_red_arrs <- groupResultArrays num_threads reds

  -- Thread id for indexing into each threads accumulator element(s)
  tid <- dPrim "tid" $ IntType Int32
  tid' <- toExp $ Var tid

  slugs <- mapM (segRedOpSlug tid') $ zip reds stage_one_red_arrs

  -- TODO: Need to declare this for reduce6.fut
  -- reduce6.fut still doesn't work though
  dPrimV_ (segFlat space) 0

  prebody <- collect $
    sComment "neutral-initialise the acc used by this thread" $
      forM_ slugs $ \slug ->
        forM_ (zip (slugAccs slug) (slugNeutral slug)) $ \((acc, acc_is), ne) ->
          sLoopNest (slugShape slug) $ \vec_is ->
            copyDWIMFix acc (acc_is++vec_is) ne []

  fbody <- collect $ do
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
    dScope Nothing $ scopeOfLParams $ concatMap slugParams slugs
    compileKBodyRed pat space kbody reds $ \all_red_res -> do
      let all_red_res' = chunks (map (length . slugNeutral) slugs) all_red_res
      forM_ (zip all_red_res' slugs) $ \(red_res, slug) ->
        sLoopNest (slugShape slug) $ \vec_is -> do
          sComment "load acc params" $
            forM_ (zip (accParams slug) (slugAccs slug)) $ \(p, (acc, acc_is)) ->
              copyDWIMFix (paramName p) [] (Var acc) (acc_is++vec_is)
          sComment "load next params" $
            forM_ (zip (nextParams slug) red_res) $ \(p, (res, res_is)) ->
              copyDWIMFix (paramName p) [] res (res_is ++ vec_is)
          forM_ (zip (slugParams slug) (slugAccs slug)) $ \(p, (acc, acc_is)) ->
              copyDWIMFix (paramName p) [] (Var acc) (acc_is++vec_is)
          sComment "red body" $
            compileStms mempty (bodyStms $ slugBody slug) $
                forM_ (zip (slugAccs slug) (bodyResult $ slugBody slug)) $
                  \((acc, acc_is), se) -> copyDWIMFix acc (acc_is++vec_is) se []

  let paramsNames = namesToList $ (freeIn fbody <> freeIn prebody) `namesSubtract`
                                  namesFromList (tid : [segFlat space])
  ts <- mapM lookupType paramsNames
  let params = zipWith toParam paramsNames ts

  ntasks <- dPrim "num_tasks" $ IntType Int32
  ntasks' <- toExp $ Var ntasks
  emit $ Imp.Op $ Imp.ParLoop ntasks (segFlat space) (product ns')
                              (Imp.MulticoreFunc params prebody fbody tid)

  sComment "neutral-initialise the output" $
    forM_ slugs $ \slug ->
      forM_ (zip (patternElements pat) (slugNeutral slug)) $ \(pe, ne) ->
        sLoopNest (slugShape slug) $ \vec_is ->
          copyDWIMFix (patElemName pe) (0 : vec_is) ne []

  -- Reduce over intermediate results
  dScope Nothing $ scopeOfLParams $ concatMap slugParams slugs
  sFor "i" ntasks' $ \i' -> do
    emit $ Imp.SetScalar tid i'
    sComment "Apply main thread reduction" $
      forM_ slugs $ \slug ->
        sLoopNest (slugShape slug) $ \vec_is -> do
        sComment "load acc params" $
          forM_ (zip (accParams slug) (slugAccs slug)) $ \(p, (acc, acc_is)) ->
          copyDWIMFix (paramName p) [] (Var acc) (acc_is++vec_is)
        sComment "load next params" $
          forM_ (zip (nextParams slug) $ patternElements pat) $ \(p, pe) ->
          copyDWIMFix (paramName p) [] (Var $ patElemName pe) (0 : vec_is)
        sComment "red body" $
          compileStms mempty (bodyStms $ slugBody slug) $
            forM_ (zip (patternElements pat) (bodyResult $ slugBody slug)) $
              \(pe, se') -> copyDWIMFix (patElemName pe) (0 : vec_is) se' []


-- Each thread reduces over the number of segments
-- each of which is done sequentially
-- Maybe we should select the work of the inner loop
-- based on n_segments and dimensions etc.
segmentedReduction :: Pattern ExplicitMemory
                      -> SegSpace
                      -> [SegRedOp ExplicitMemory]
                      -> KernelBody ExplicitMemory
                      -> MulticoreGen ()
segmentedReduction pat space reds kbody = do
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  tid <- dPrim "tid" $ IntType Int32

  n_segments <- dPrim "segment_iter" $ IntType Int32
  n_segments' <- toExp $ Var n_segments

  -- Perform sequential reduce on inner most dimension
  fbody <- collect $ do
    sComment "neutral-initialise the accumulators" $
      forM_ reds $ \red->
        forM_ (zip (patternElements pat) (segRedNeutral red)) $
        \(pe, ne) ->
          sLoopNest (segRedShape red) $ \vec_is ->
            copyDWIMFix (patElemName pe) (n_segments' : vec_is) ne []


    sComment "function main body" $ do
      let inner_bound = last ns'
      dScope Nothing $ scopeOfLParams $ concatMap (lambdaParams . segRedLambda) reds
      sFor "i" inner_bound $ \i -> do
        dPrimV_ (segFlat space) (n_segments' * inner_bound + i)
        zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
        compileKBody kbody $ \all_red_res -> do
          let red_res' = chunks (map (length . segRedNeutral) reds) all_red_res
          forM_ (zip reds red_res') $ \(red, res') ->
            sLoopNest (segRedShape red) $ \vec_is -> do

            sComment "load acuum " $ do
              let acc_params = take (length (segRedNeutral red)) $ (lambdaParams . segRedLambda) red
              forM_ (zip acc_params (patternElements pat)) $ \(p, pe) ->
                copyDWIMFix (paramName p) [] (Var $ patElemName pe) [n_segments']

            sComment "load new val" $ do
              let next_params = drop (length (segRedNeutral red)) $ (lambdaParams . segRedLambda) red
              forM_ (zip next_params res') $ \(p, (res, res_is)) ->
                copyDWIMFix (paramName p) [] res (res_is ++ vec_is)

            sComment "apply reduction" $ do
              let lbody = (lambdaBody . segRedLambda) red
              compileStms mempty (bodyStms lbody) $
                sComment "write back to res" $
                forM_ (zip (patternElements pat) (bodyResult lbody)) $
                  \(pe, se') -> copyDWIMFix (patElemName pe) [n_segments'] se' []


  let paramsNames = namesToList $ freeIn fbody `namesSubtract`
                                  namesFromList (tid : [n_segments])

  ts <- mapM lookupType paramsNames
  let params = zipWith toParam paramsNames ts

  ntask <- dPrim "num_tasks" $ IntType Int32

  emit $ Imp.Op $ Imp.ParLoop ntask n_segments (product $ init ns') (Imp.MulticoreFunc params mempty fbody tid)
