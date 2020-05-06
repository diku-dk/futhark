module Futhark.CodeGen.ImpGen.Multicore.SegRed
  ( compileSegRed
  , compileSegRed'
  )
  where

import Control.Monad
import Data.List
import Prelude hiding (quot, rem)

import qualified Futhark.CodeGen.ImpCode.Multicore as Imp
import Futhark.CodeGen.ImpGen
import Futhark.Representation.MCMem
import Futhark.Util (chunks)
import Futhark.CodeGen.ImpGen.Multicore.Base

type DoSegBody = (([(SubExp, [Imp.Exp])] -> MulticoreGen ()) -> MulticoreGen ())

-- Compile a SegRed construct
compileSegRed :: Pattern MCMem
                 -> SegSpace
                 -> [SegRedOp MCMem]
                 -> KernelBody MCMem
                 -> MulticoreGen ()
compileSegRed pat space reds kbody =
  compileSegRed' pat space reds $ \red_cont ->
    compileStms mempty (kernelBodyStms kbody) $ do
    let (red_res, map_res) = splitAt (segRedResults reds) $ kernelBodyResult kbody

    sComment "save map-out results" $ do
      let map_arrs = drop (segRedResults reds) $ patternElements pat
      zipWithM_ (compileThreadResult space) map_arrs map_res

    red_cont $ zip (map kernelResultSubExp red_res) $ repeat []

-- | Like 'compileSegRed', but where the body is a monadic action.
compileSegRed' :: Pattern MCMem
               -> SegSpace
               -> [SegRedOp MCMem]
               -> DoSegBody
               -> MulticoreGen ()
compileSegRed' pat space reds kbody
  | [_] <- unSegSpace space =
      nonsegmentedReduction pat space reds kbody
  | otherwise =
      segmentedReduction pat space reds kbody


-- | Arrays for storing group results.
--
-- The group-result arrays have an extra dimension (of size groupsize)
-- because they are also used for keeping vectorised accumulators for
-- first-stage reduction, if necessary.  When actually storing group
-- results, the first index is set to 0.
groupResultArrays :: SubExp
                  -> [SegRedOp MCMem]
                  -> MulticoreGen [[VName]]
groupResultArrays num_threads reds =
  forM reds $ \(SegRedOp _ lam _ shape) ->
    forM (lambdaReturnType lam) $ \t -> do
    let pt = elemType t
        full_shape = Shape [num_threads] <> shape <> arrayShape t
        perm = [1..shapeRank full_shape-1] ++ [0]
    sAllocArrayPerm "group_res_arr" pt full_shape DefaultSpace perm


-- | A SegRedOp with auxiliary information.
data SegRedOpSlug =
  SegRedOpSlug
  { slugOp :: SegRedOp MCMem
  , slugAccs :: [(VName, [Imp.Exp])]
    -- ^ Places to store accumulator in stage 1 reduction.
  }


slugBody :: SegRedOpSlug -> Body MCMem
slugBody = lambdaBody . segRedLambda . slugOp

slugParams :: SegRedOpSlug -> [LParam MCMem]
slugParams = lambdaParams . segRedLambda . slugOp

slugNeutral :: SegRedOpSlug -> [SubExp]
slugNeutral = segRedNeutral . slugOp

slugShape :: SegRedOpSlug -> Shape
slugShape = segRedShape . slugOp

accParams, nextParams :: SegRedOpSlug -> [LParam MCMem]
accParams slug = take (length (slugNeutral slug)) $ slugParams slug
nextParams slug = drop (length (slugNeutral slug)) $ slugParams slug

slugsComm :: [SegRedOpSlug] -> Commutativity
slugsComm = mconcat . map (segRedComm . slugOp)


segRedOpSlug :: Imp.Exp
             -> (SegRedOp MCMem, [VName])
             -> MulticoreGen SegRedOpSlug
segRedOpSlug local_tid (op, param_arrs) =
  SegRedOpSlug op <$> mapM (\param_arr -> return (param_arr, [local_tid])) param_arrs


nonsegmentedReduction :: Pattern MCMem
                      -> SegSpace
                      -> [SegRedOp MCMem]
                      -> DoSegBody
                      -> MulticoreGen ()
nonsegmentedReduction pat space reds kbody = do
  emit $ Imp.DebugPrint "nonsegmented segRed " Nothing
  sUnpauseProfiling

  let (is, ns) = unzip $ unSegSpace space
  num_threads <- getNumThreads
  num_threads' <- toExp $ Var num_threads

  ns' <- mapM toExp ns
  stage_one_red_arrs <- groupResultArrays (Var num_threads) reds

  -- Thread id for indexing into each threads accumulator element(s)
  tid <- dPrim "tid" $ IntType Int32
  tid' <- toExp $ Var tid

  slugs <- mapM (segRedOpSlug tid') $ zip reds stage_one_red_arrs

  -- TODO: Need to declare this for reduce6.fut
  -- reduce6.fut still doesn't work though
  dPrimV_ (segFlat space) 0

  sFor "i" num_threads' $ \i -> do
    tid <-- i
    sComment "neutral-initialise the acc used by tid" $
      forM_ slugs $ \slug ->
        forM_ (zip (slugAccs slug) (slugNeutral slug)) $ \((acc, acc_is), ne) ->
          sLoopNest (slugShape slug) $ \vec_is ->
            copyDWIMFix acc (acc_is++vec_is) ne []

  fbody <- collect $ do
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
    dScope Nothing $ scopeOfLParams $ concatMap slugParams slugs
    kbody $ \all_red_res -> do
      let all_red_res' = chunks (map (length . slugNeutral) slugs) all_red_res
      forM_ (zip all_red_res' slugs) $ \(red_res, slug) ->
        sLoopNest (slugShape slug) $ \vec_is -> do
          sComment "load acc params" $
            forM_ (zip (accParams slug) (slugAccs slug)) $ \(p, (acc, acc_is)) ->
              copyDWIMFix (paramName p) [] (Var acc) (acc_is++vec_is)
          sComment "load next params" $
            forM_ (zip (nextParams slug) red_res) $ \(p, (res, res_is)) ->
              copyDWIMFix (paramName p) [] res (res_is ++ vec_is)
          sComment "red body" $
            compileStms mempty (bodyStms $ slugBody slug) $
                forM_ (zip (slugAccs slug) (bodyResult $ slugBody slug)) $
                  \((acc, acc_is), se) -> copyDWIMFix acc (acc_is++vec_is) se []

  let freeVariables = namesToList $ freeIn fbody `namesSubtract`
                                  namesFromList (tid : [segFlat space])
  ts <- mapM lookupType freeVariables
  let freeParams = zipWith toParam freeVariables ts
      scheduling = case slugsComm slugs of
                     Commutative -> decideScheduling fbody
                     Noncommutative -> Imp.Static

  ntasks <- dPrim "num_tasks" $ IntType Int32
  ntasks' <- toExp $ Var ntasks
  emit $ Imp.Op $ Imp.ParLoop scheduling ntasks (segFlat space) (product ns')
                              (Imp.MulticoreFunc freeParams mempty fbody tid)

  sComment "neutral-initialise the output" $
    forM_ slugs $ \slug ->
      forM_ (zip (patternElements pat) (slugNeutral slug)) $ \(pe, ne) ->
        sLoopNest (slugShape slug) $ \vec_is ->
          copyDWIMFix (patElemName pe) vec_is ne []

  -- Reduce over intermediate results
  dScope Nothing $ scopeOfLParams $ concatMap slugParams slugs
  sFor "i" ntasks' $ \i' -> do
    emit $ Imp.DebugPrint "nonsegmented segRed stage 2" Nothing
    tid <-- i'
    sComment "Apply main thread reduction" $
      forM_ slugs $ \slug ->
        sLoopNest (slugShape slug) $ \vec_is -> do
        sComment "load acc params" $
          forM_ (zip (accParams slug) (slugAccs slug)) $ \(p, (acc, acc_is)) ->
          copyDWIMFix (paramName p) [] (Var acc) (acc_is++vec_is)
        sComment "load next params" $
          forM_ (zip (nextParams slug) $ patternElements pat) $ \(p, pe) ->
          copyDWIMFix (paramName p) [] (Var $ patElemName pe) vec_is
        sComment "red body" $
          compileStms mempty (bodyStms $ slugBody slug) $
            forM_ (zip (patternElements pat) (bodyResult $ slugBody slug)) $
              \(pe, se') -> copyDWIMFix (patElemName pe) vec_is se' []


-- Each thread reduces over the number of segments
-- each of which is done sequentially
-- Maybe we should select the work of the inner loop
-- based on n_segments and dimensions etc.
segmentedReduction :: Pattern MCMem
                   -> SegSpace
                   -> [SegRedOp MCMem]
                   -> DoSegBody
                   -> MulticoreGen ()
segmentedReduction pat space reds kbody = do
  emit $ Imp.DebugPrint "segmented segRed " Nothing
  sUnpauseProfiling

  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  tid <- dPrim "tid" $ IntType Int32

  n_segments <- dPrim "segment_iter" $ IntType Int32
  n_segments' <- toExp $ Var n_segments

  -- Perform sequential reduce on inner most dimension
  fbody <- collect $ do
    emit $ Imp.DebugPrint "segmented segRed stage 1" Nothing
    sComment "neutral-initialise the accumulators" $
      forM_ reds $ \red->
        forM_ (zip (patternElements pat) (segRedNeutral red)) $ \(pe, ne) ->
          sLoopNest (segRedShape red) $ \vec_is ->
            copyDWIMFix (patElemName pe) (n_segments' : vec_is) ne []


    sComment "function main body" $ do
      let inner_bound = last ns'
      dScope Nothing $ scopeOfLParams $ concatMap (lambdaParams . segRedLambda) reds
      sFor "i" inner_bound $ \i -> do
        dPrimV_ (segFlat space) (n_segments' * inner_bound + i)
        zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
        kbody $ \all_red_res -> do
          let red_res' = chunks (map (length . segRedNeutral) reds) all_red_res
          forM_ (zip reds red_res') $ \(red, res') ->
            sLoopNest (segRedShape red) $ \vec_is -> do

            sComment "load acuum " $ do
              let acc_params = take (length (segRedNeutral red)) $ (lambdaParams . segRedLambda) red
              forM_ (zip acc_params (patternElements pat)) $ \(p, pe) ->
                copyDWIMFix (paramName p) [] (Var $ patElemName pe) (n_segments' : vec_is)

            sComment "load new val" $ do
              let next_params = drop (length (segRedNeutral red)) $ (lambdaParams . segRedLambda) red
              forM_ (zip next_params res') $ \(p, (res, res_is)) ->
                copyDWIMFix (paramName p) [] res (res_is ++ vec_is)

            sComment "apply reduction" $ do
              let lbody = (lambdaBody . segRedLambda) red
              compileStms mempty (bodyStms lbody) $
                sComment "write back to res" $
                forM_ (zip (patternElements pat) (bodyResult lbody)) $
                  \(pe, se') -> copyDWIMFix (patElemName pe) (n_segments' : vec_is) se' []


  let freeVariables = namesToList $ freeIn fbody `namesSubtract`
                                    namesFromList (tid : [n_segments])

  ts <- mapM lookupType freeVariables
  let freeParams = zipWith toParam freeVariables ts

  ntask <- dPrim "num_tasks" $ IntType Int32

  emit $ Imp.Op $ Imp.ParLoop (decideScheduling fbody)
                               ntask n_segments (product $ init ns')
                              (Imp.MulticoreFunc freeParams mempty fbody tid)
