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
compileSegRed pat _lvl space reds kbody
  | [(_, Constant (IntValue (Int32Value 1))), _] <- unSegSpace space =
      nonsegmentedReduction pat space reds kbody
  | otherwise =
      segmentedReduction pat space reds kbody


-- | Arrays for storing group results.
--
-- The group-result arrays have an extra dimension (of size groupsize)
-- because they are also used for keeping vectorised accumulators for
-- first-stage reduction, if necessary.  When actually storing group
-- results, the first index is set to 0.
groupResultArrays :: Count NumGroups SubExp
                  -> [SubExp]
                  -> [SegRedOp ExplicitMemory]
                  -> MulticoreGen [[VName]]
groupResultArrays (Count num_threads) dims reds =
  forM (zip reds dims) $ \(SegRedOp _ lam _ shape, dim) ->
    forM (lambdaReturnType lam) $ \t -> do
    let pt = elemType t
        full_shape = Shape [dim, num_threads] <> shape <> arrayShape t
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
             -> Imp.Exp
             -> (SegRedOp ExplicitMemory, [VName])
             -> MulticoreGen SegRedOpSlug
segRedOpSlug local_tid dims (op, param_arrs) =
  SegRedOpSlug op <$>
  mapM mkAcc param_arrs
  where mkAcc param_arr
          | shapeRank (segRedShape op) == 0 =
              return (param_arr, [local_tid])
          | otherwise =
              return (param_arr, [local_tid, dims])


nonsegmentedReduction :: Pattern ExplicitMemory
                      -> SegSpace
                      -> [SegRedOp ExplicitMemory]
                      -> KernelBody ExplicitMemory
                      -> MulticoreGen ()
nonsegmentedReduction pat space reds kbody = do
  let (is, ns) = unzip $ unSegSpace space

  -- Dummy value for now
  -- Need to postpone allocation of intermediate res for scheduler
  let num_threads = Count $ Constant $ IntValue $ Int32Value 10
  num_threads' <- toExp $ unCount num_threads

  ns' <- mapM toExp ns
  stage_one_red_arrs <- groupResultArrays num_threads ns reds

  tid <- dPrim "tid" $ IntType Int32
  tid' <- toExp $ Var tid

  slugs <- mapM (segRedOpSlug tid' num_threads') $ zip reds stage_one_red_arrs

  prebody <- collect $ do
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
    dScope Nothing $ scopeOfLParams $ concatMap slugParams slugs
    compileKBody kbody $ \all_red_res -> do
      let slugs_res = chunks (map (length . slugNeutral) slugs) all_red_res
      forM_ (zip slugs slugs_res) $ \(slug, red_res) ->
        forM_ (zip (slugAccs slug) red_res) $ \((acc, acc_is), (res, res_is)) ->
          copyDWIMFix acc acc_is res res_is
      emit $ Imp.SetScalar (segFlat space) (Imp.vi32 (segFlat space) + 1)

  fbody <- collect $
    sComment "function main body" $ do
      zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
      compileKBody kbody $ \all_red_res -> do
        let slugs_res' = chunks (map (length . slugNeutral) slugs) all_red_res
        forM_ (zip slugs slugs_res') $ \(slug, red_res') ->
          sLoopNest (slugShape slug) $ \vec_is -> do
          forM_ (zip (accParams slug) (slugAccs slug)) $ \(p, (acc, acc_is)) ->
            copyDWIMFix (paramName p) [] (Var acc) acc_is
          forM_ (zip (nextParams slug) red_res') $ \(p, (res, res_is)) ->
            copyDWIMFix (paramName p) [] res (res_is ++ vec_is)
          compileStms mempty (bodyStms $ slugBody slug) $
            forM_ (zip (slugAccs slug) (bodyResult $ slugBody slug)) $
              \((acc, acc_is), se') -> copyDWIMFix acc acc_is se' []


  let paramsNames = namesToList $ (freeIn fbody `namesIntersection` freeIn prebody) `namesSubtract`
                                  namesFromList (tid : [segFlat space])
  ts <- mapM lookupType paramsNames
  let params = zipWith toParam paramsNames ts

  ntasks <- dPrim "num_tasks" $ IntType Int32
  ntasks' <- toExp $ Var ntasks

  emit $ Imp.Op $ Imp.ParLoop ntasks (segFlat space) (product ns')
                              (Imp.MulticoreFunc params prebody fbody tid)

  forM_ slugs $ \slug ->
    forM_ (zip (patternElements pat) (slugNeutral slug)) $ \(pe, ne) ->
      copyDWIMFix (patElemName pe) [] ne []

  -- Reduce over intermediate results
  dScope Nothing $ scopeOfLParams $ concatMap slugParams slugs
  sFor "i" ntasks' $ \i' ->
    sComment "apply map function" $
      forM_ slugs $ \slug ->
        sLoopNest (slugShape slug) $ \_vec_is -> do
        -- sComment "load accumulator" $
        forM_ (zip (accParams slug) (patternElements pat)) $ \(p, pe) ->
          copyDWIMFix (paramName p) [] (Var $ patElemName pe) []
        -- sComment "set new values to func_param" $
        forM_ (zip (nextParams slug) (slugAccs slug)) $ \(p, (acc, _)) ->
          copyDWIMFix (paramName p) [] (Var acc) [i']
        -- sComment "apply reduction operator" $
        compileStms mempty (bodyStms $ slugBody slug) $
            forM_ (zip (patternElements pat) (bodyResult $ slugBody slug)) $
            \(pe, se') -> copyDWIMFix (patElemName pe) [] se' []





-- Each thread reduces over the number of segments
-- each of which is done sequentially
segmentedReduction :: Pattern ExplicitMemory
                      -> SegSpace
                      -> [SegRedOp ExplicitMemory]
                      -> KernelBody ExplicitMemory
                      -> MulticoreGen ()
segmentedReduction pat space reds kbody = do
  let (is, ns) = unzip $ unSegSpace space
      -- Dummy value for now
      -- Need to postpone allocation of intermediate res for scheduler
      num_threads = Count $ Constant $ IntValue $ Int32Value 1
  ns' <- mapM toExp ns

  stage_one_red_arrs <- groupResultArrays num_threads ns reds


  tid <- dPrim "tid" $ IntType Int32
  tid' <- toExp $ Var tid

  slugs <- mapM (segRedOpSlug tid' (product ns')) $ zip reds stage_one_red_arrs

  dPrim_ (segFlat space) $ IntType Int32

  n_segments <- dPrim "iter" $ IntType Int32
  n_segments' <- toExp $ Var n_segments

  -- Perform sequential reduce on inner most dimension
  fbody <- collect $ do
    -- fix reading in the neutral element
    forM_ slugs $ \slug ->
      forM_ (zip (patternElements pat) (slugNeutral slug)) $ \(pe, ne) ->
        copyDWIMFix (patElemName pe) [n_segments'] ne []

    sComment "function main body" $ do
      let inner_bound = last ns'
      dScope Nothing $ scopeOfLParams $ concatMap slugParams slugs
      sFor "i" inner_bound $ \i -> do
        dPrimV_ (segFlat space) (n_segments' * inner_bound + i)
        zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
        compileKBody kbody $ \all_red_res -> do
          let slugs_res' = chunks (map (length . slugNeutral) slugs) all_red_res
          forM_ (zip slugs slugs_res') $ \(slug, red_res') ->
            sLoopNest (slugShape slug) $ \vec_is -> do

            sComment "load acuum " $ do
              forM_ (zip (accParams slug) (patternElements pat)) $ \(p, pe) ->
                copyDWIMFix (paramName p) [] (Var $ patElemName pe) [n_segments']

            sComment "load new val" $ do
              forM_ (zip (nextParams slug) red_res') $ \(p, (res, res_is)) ->
                copyDWIMFix (paramName p) [] res (res_is ++ vec_is)

            sComment "apply reduction" $ do
              compileStms mempty (bodyStms $ slugBody slug) $
                sComment "write back to res" $ do
                forM_ (zip (patternElements pat) (bodyResult $ slugBody slug)) $
                  \(pe, se') -> copyDWIMFix (patElemName pe) [n_segments'] se' []


  let paramsNames = namesToList $ freeIn fbody `namesSubtract`
                                  namesFromList (tid : [n_segments])

  ts <- mapM lookupType paramsNames
  let params = zipWith toParam paramsNames ts

  ntask <- dPrim "num_tasks" $ IntType Int32

  emit $ Imp.Op $ Imp.ParLoop ntask n_segments (product $ init ns') (Imp.MulticoreFunc params mempty fbody tid)
