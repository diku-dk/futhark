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
import Futhark.IR.MCMem
import Futhark.Util (chunks)
import Futhark.CodeGen.ImpGen.Multicore.Base

type DoSegBody = (([(SubExp, [Imp.Exp])] -> MulticoreGen ()) -> MulticoreGen ())


-- | Generate code for a SegRed construct
compileSegRed :: Pattern MCMem
              -> SegSpace
              -> [SegBinOp MCMem]
              -> KernelBody MCMem
              -> VName
              -> MulticoreGen Imp.Code
compileSegRed pat space reds kbody nsubtasks =
  compileSegRed' pat space reds nsubtasks $ \red_cont ->
    compileStms mempty (kernelBodyStms kbody) $ do
    let (red_res, map_res) = splitAt (segBinOpResults reds) $ kernelBodyResult kbody

    sComment "save map-out results" $ do
      let map_arrs = drop (segBinOpResults reds) $ patternElements pat
      zipWithM_ (compileThreadResult space) map_arrs map_res

    red_cont $ zip (map kernelResultSubExp red_res) $ repeat []

-- | Like 'compileSegRed', but where the body is a monadic action.
compileSegRed' :: Pattern MCMem
               -> SegSpace
               -> [SegBinOp MCMem]
               -> VName
               -> DoSegBody
               -> MulticoreGen Imp.Code
compileSegRed' pat space reds nsubtasks kbody
  | [_] <- unSegSpace space =
      nonsegmentedReduction pat space reds nsubtasks kbody
  | otherwise =
      segmentedReduction pat space reds kbody


-- | A SegBinOp with auxiliary information.
data SegBinOpSlug =
  SegBinOpSlug
  { slugOp :: SegBinOp MCMem
  , slugAccs :: [(VName, [Imp.Exp])]
  }


slugBody :: SegBinOpSlug -> Body MCMem
slugBody = lambdaBody . segBinOpLambda . slugOp

slugParams :: SegBinOpSlug -> [LParam MCMem]
slugParams = lambdaParams . segBinOpLambda . slugOp

slugNeutral :: SegBinOpSlug -> [SubExp]
slugNeutral = segBinOpNeutral . slugOp

slugShape :: SegBinOpSlug -> Shape
slugShape = segBinOpShape . slugOp

accParams, nextParams :: SegBinOpSlug -> [LParam MCMem]
accParams slug = take (length (slugNeutral slug)) $ slugParams slug
nextParams slug = drop (length (slugNeutral slug)) $ slugParams slug

segBinOpOpSlug :: Imp.Exp
               -> (SegBinOp MCMem, [VName])
               -> MulticoreGen SegBinOpSlug
segBinOpOpSlug local_tid (op, param_arrs) =
  SegBinOpSlug op <$> mapM (\param_arr -> return (param_arr, [local_tid])) param_arrs



nonsegmentedReduction :: Pattern MCMem
                      -> SegSpace
                      -> [SegBinOp MCMem]
                      -> VName
                      -> DoSegBody
                      -> MulticoreGen Imp.Code
nonsegmentedReduction pat space reds nsubtasks kbody = collect $ do

  -- Thread id for indexing into each threads accumulator element(s)
  tid' <- toExp $ Var $ segFlat space
  thread_red_arrs <- groupResultArrays "reduce_stage_1_tid_accum_arr" (Var nsubtasks) reds
  slugs1 <- mapM (segBinOpOpSlug tid') $ zip reds thread_red_arrs
  nsubtasks' <- toExp $ Var nsubtasks

  sFor "i" nsubtasks' $ \i -> do
    segFlat space <-- i
    sComment "neutral-initialise the acc used by tid" $
      forM_ slugs1 $ \slug ->
        forM_ (zip (slugAccs slug) (slugNeutral slug)) $ \((acc, acc_is), ne) ->
          sLoopNest (slugShape slug) $ \vec_is ->
            copyDWIMFix acc (acc_is++vec_is) ne []

  reductionStage1 space slugs1 kbody
  reds2 <- renameSegBinOp reds
  slugs2 <- mapM (segBinOpOpSlug tid') $ zip reds2 thread_red_arrs
  reductionStage2 pat space nsubtasks slugs2

reductionStage1 :: SegSpace
                -> [SegBinOpSlug]
                -> DoSegBody
                -> MulticoreGen ()
reductionStage1 space slugs kbody = do
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns
  flat_idx <- dPrim "iter" int32

  fbody <- collect $ do
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_idx
    dScope Nothing $ scopeOfLParams $ concatMap slugParams slugs
    kbody $ \all_red_res -> do
      let all_red_res' = segBinOpChunks (map slugOp slugs) all_red_res
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

  free_params <- freeParams fbody (segFlat space : [flat_idx])
  let (body_allocs, fbody') = extractAllocations fbody
  emit $ Imp.Op $ Imp.MCFunc "segred_stage_1" flat_idx body_allocs fbody' free_params $ segFlat space

reductionStage2 :: Pattern MCMem
                -> SegSpace
                -> VName
                -> [SegBinOpSlug]
                -> MulticoreGen ()
reductionStage2 pat space nsubtasks slugs = do
  let per_red_pes = segBinOpChunks (map slugOp slugs) $ patternValueElements pat
  nsubtasks' <- toExp $ Var nsubtasks
  sComment "neutral-initialise the output" $
   forM_ (zip (map slugOp slugs) per_red_pes) $ \(red, red_res) ->
     forM_ (zip red_res $ segBinOpNeutral red) $ \(pe, ne) ->
       sLoopNest (segBinOpShape red) $ \vec_is ->
         copyDWIMFix (patElemName pe) vec_is ne []

  dScope Nothing $ scopeOfLParams $ concatMap slugParams slugs

  sFor "i" nsubtasks' $ \i' -> do
    segFlat space <-- i'
    sComment "Apply main thread reduction" $
      forM_ (zip slugs per_red_pes) $ \(slug, red_res) ->
        sLoopNest (slugShape slug) $ \vec_is -> do
          sComment "load acc params" $
            forM_ (zip (accParams slug) red_res) $ \(p, pe) ->
            copyDWIMFix (paramName p) [] (Var $ patElemName pe) vec_is
          sComment "load next params" $
            forM_ (zip (nextParams slug) (slugAccs slug)) $ \(p, (acc, acc_is)) ->
            copyDWIMFix (paramName p) [] (Var acc) (acc_is++vec_is)
          sComment "red body" $
            compileStms mempty (bodyStms $ slugBody slug) $
              forM_ (zip red_res (bodyResult $ slugBody slug)) $
                \(pe, se') -> copyDWIMFix (patElemName pe) vec_is se' []

-- Each thread reduces over the number of segments
-- each of which is done sequentially
-- Maybe we should select the work of the inner loop
-- based on n_segments and dimensions etc.
segmentedReduction :: Pattern MCMem
                   -> SegSpace
                   -> [SegBinOp MCMem]
                   -> DoSegBody
                   -> MulticoreGen Imp.Code
segmentedReduction pat space reds kbody =
  collect $ do
    n_par_segments <- dPrim "segment_iter" $ IntType Int32
    body    <- compileSegRedBody n_par_segments pat space reds kbody
    free_params <- freeParams body (segFlat space : [n_par_segments])
    let (body_allocs, body') = extractAllocations body
    emit $ Imp.Op $ Imp.MCFunc "segmented_segred" n_par_segments body_allocs body' free_params $ segFlat space


compileSegRedBody :: VName
                  -> Pattern MCMem
                  -> SegSpace
                  -> [SegBinOp MCMem]
                  -> DoSegBody
                  -> MulticoreGen Imp.Code
compileSegRedBody n_segments pat space reds kbody = do
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns
  let inner_bound = last ns'

  n_segments' <- toExp $ Var n_segments

  let per_red_pes = segBinOpChunks reds $ patternValueElements pat
  -- Perform sequential reduce on inner most dimension
  collect $ do
    flat_idx <- dPrimV "flat_idx" (n_segments' * inner_bound)
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_idx
    sComment "neutral-initialise the accumulators" $
      forM_ (zip per_red_pes reds) $ \(pes, red) ->
        forM_ (zip pes (segBinOpNeutral red)) $ \(pe, ne) ->
          sLoopNest (segBinOpShape red) $ \vec_is ->
            copyDWIMFix (patElemName pe) (map Imp.vi32 (init is) ++ vec_is) ne []

    sComment "main body" $ do
      dScope Nothing $ scopeOfLParams $ concatMap (lambdaParams . segBinOpLambda) reds
      sFor "i" inner_bound $ \i -> do
        forM_ (zip (init is) $ unflattenIndex (init ns') n_segments') $ uncurry (<--)
        dPrimV_ (last is) i
        kbody $ \all_red_res -> do
          let red_res' = chunks (map (length . segBinOpNeutral) reds) all_red_res
          forM_ (zip3 per_red_pes reds red_res') $ \(pes, red, res') ->
            sLoopNest (segBinOpShape red) $ \vec_is -> do

            sComment "load accum" $ do
              let acc_params = take (length (segBinOpNeutral red)) $ (lambdaParams . segBinOpLambda) red
              forM_ (zip acc_params pes) $ \(p, pe) ->
                copyDWIMFix (paramName p) [] (Var $ patElemName pe) (map Imp.vi32 (init is) ++ vec_is)

            sComment "load new val" $ do
              let next_params = drop (length (segBinOpNeutral red)) $ (lambdaParams . segBinOpLambda) red
              forM_ (zip next_params res') $ \(p, (res, res_is)) ->
                copyDWIMFix (paramName p) [] res (res_is ++ vec_is)

            sComment "apply reduction" $ do
              let lbody = (lambdaBody . segBinOpLambda) red
              compileStms mempty (bodyStms lbody) $
                sComment "write back to res" $
                forM_ (zip pes (bodyResult lbody)) $
                  \(pe, se') -> copyDWIMFix (patElemName pe) (map Imp.vi32 (init is) ++ vec_is) se' []




----------------------------------------------------------
-------- ========= Dead code =========== -----------------
----------------------------------------------------------
-- segmentedReduction pat space reds kbody ModeSequential =
--   collect $ do
--     let ns = map snd $ unSegSpace space
--     ns' <- mapM toExp ns
--     n_seq_segments <- dPrim "segment_iter" $ IntType Int32
--     seq_body   <- compileSegRedBody n_seq_segments pat space reds kbody
--     sFor "i" (product $ init ns') $ \i -> do
--       n_seq_segments <-- i
--       emit seq_body




-- nonsegmentedReduction pat space reds kbody ModeSequential = do
--   let ns = map snd $ unSegSpace space
--   ns' <- mapM toExp ns

--   collect $ localMode ModeSequential $ do
--     flat_seq_idx <- dPrimV "seq_iter" 0
--     seq_code_body <- sequentialRed pat flat_seq_idx space reds kbody
--     let (body_allocs, seq_code_body') = extractAllocations seq_code_body
--     emit body_allocs
--     sFor "i" (product ns') $ \i -> do
--       flat_seq_idx <-- i
--       emit seq_code_body'


-- sequentialRed :: Pattern MCMem
--               -> VName
--               -> SegSpace
--               -> [SegBinOp MCMem]
--               -> DoSegBody
--               -> MulticoreGen Imp.Code
-- sequentialRed pat flat_idx space reds kbody = do
--   let (is, ns) = unzip $ unSegSpace space
--   ns' <- mapM toExp ns
--   collect $ do
--     zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_idx
--     dScope Nothing $ scopeOfLParams $ concatMap (lambdaParams . segBinOpLambda) reds
--     kbody $ \all_red_res -> do
--       let all_red_res' = segBinOpChunks reds all_red_res
--       forM_ (zip all_red_res' reds) $ \(red_res, red) -> do
--         let (xParams, yParams) = splitAt (length (segBinOpNeutral red)) $ (lambdaParams . segBinOpLambda) red
--         sLoopNest (segBinOpShape red) $ \vec_is -> do
--           sComment "load acc params" $
--             forM_ (zip xParams $ patternElements pat) $ \(p, pe) ->
--               copyDWIMFix (paramName p) [] (Var $ patElemName pe) vec_is
--           sComment "load next params" $
--             forM_ (zip yParams red_res) $ \(p, (res, res_is)) ->
--               copyDWIMFix (paramName p) [] res (res_is ++ vec_is)
--           sComment "sequential red body" $
--             compileStms mempty (bodyStms $ (lambdaBody . segBinOpLambda) red) $
--                 forM_ (zip (patternElements pat) (bodyResult $ (lambdaBody . segBinOpLambda) red)) $
--                   \(pe, se) -> copyDWIMFix (patElemName pe) vec_is se []
