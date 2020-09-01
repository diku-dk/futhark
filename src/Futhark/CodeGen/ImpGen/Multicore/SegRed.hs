module Futhark.CodeGen.ImpGen.Multicore.SegRed
  ( compileSegRed,
    compileSegRed',
  )
where

import Control.Monad
import Data.List
import qualified Futhark.CodeGen.ImpCode.Multicore as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.IR.MCMem
import Futhark.MonadFreshNames
import Futhark.Util (chunks)
import Prelude hiding (quot, rem)

type DoSegBody = (([(SubExp, [Imp.TExp Int32])] -> MulticoreGen ()) -> MulticoreGen ())

-- | Generate code for a SegRed construct
compileSegRed ::
  Pattern MCMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  VName ->
  MulticoreGen Imp.Code
compileSegRed pat space reds kbody nsubtasks =
  compileSegRed' pat space reds nsubtasks $ \red_cont ->
    compileStms mempty (kernelBodyStms kbody) $ do
      let (red_res, map_res) = splitAt (segBinOpResults reds) $ kernelBodyResult kbody

      sComment "save map-out results" $ do
        let map_arrs = drop (segBinOpResults reds) $ patternElements pat
        zipWithM_ (compileThreadResult space) map_arrs map_res

      red_cont $ zip (map kernelResultSubExp red_res) $ repeat []

-- | Like 'compileSegRed', but where the body is a monadic action.
compileSegRed' ::
  Pattern MCMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  VName ->
  DoSegBody ->
  MulticoreGen Imp.Code
compileSegRed' pat space reds nsubtasks kbody
  | [_] <- unSegSpace space =
    nonsegmentedReduction pat space reds nsubtasks kbody
  | otherwise =
    segmentedReduction pat space reds kbody

-- | A SegBinOp with auxiliary information.
data SegBinOpSlug = SegBinOpSlug
  { slugOp :: SegBinOp MCMem,
    slugAccs :: [(VName, [Imp.TExp Int32])]
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

segBinOpOpSlug ::
  Imp.TExp Int32 ->
  (SegBinOp MCMem, [VName]) ->
  MulticoreGen SegBinOpSlug
segBinOpOpSlug local_tid (op, param_arrs) =
  SegBinOpSlug op <$> mapM (\param_arr -> return (param_arr, [local_tid])) param_arrs

nonsegmentedReduction ::
  Pattern MCMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  VName ->
  DoSegBody ->
  MulticoreGen Imp.Code
nonsegmentedReduction pat space reds nsubtasks kbody = collect $ do
  -- Thread id for indexing into each threads accumulator element(s)
  let tid' = Imp.vi32 $ segFlat space
  thread_red_arrs <- groupResultArrays "reduce_stage_1_tid_accum_arr" (Var nsubtasks) reds
  slugs1 <- mapM (segBinOpOpSlug tid') $ zip reds thread_red_arrs
  let nsubtasks' = Imp.vi32 nsubtasks

  sFor "i" nsubtasks' $ \i -> do
    segFlat space <-- i
    sComment "neutral-initialise the acc used by tid" $
      forM_ slugs1 $ \slug ->
        forM_ (zip (slugAccs slug) (slugNeutral slug)) $ \((acc, acc_is), ne) ->
          sLoopNest (slugShape slug) $ \vec_is ->
            copyDWIMFix acc (acc_is ++ vec_is) ne []

  reductionStage1 space slugs1 kbody
  reds2 <- renameSegBinOp reds
  slugs2 <- mapM (segBinOpOpSlug tid') $ zip reds2 thread_red_arrs
  reductionStage2 pat space nsubtasks slugs2

reductionStage1 ::
  SegSpace ->
  [SegBinOpSlug] ->
  DoSegBody ->
  MulticoreGen ()
reductionStage1 space slugs kbody = do
  let (is, ns) = unzip $ unSegSpace space
      ns' = map (sExt64 . toInt32Exp) ns
  flat_idx <- dPrim "iter" int64

  -- Create new redout names
  redouts <- forM (map slugOp slugs) $ \(SegBinOp _ lam _ _) ->
    forM (lambdaReturnType lam) $ \_ -> newVName "redout"

  -- Whenever the return value of the lambda is scalar value (collection of)
  -- we use a local variable as a readout variable.
  -- This can in some cases let GCC use registers to hold these variable,
  -- requiring less memory instructions.
  -- TODO this is a VERY hacky way; should refactor at some point
  prebody <- collect $
    -- Declare accumulator variables
    forM_ (zip slugs redouts) $ \(slug, redout) -> do
      let lam = segBinOpLambda $ slugOp slug
          shape = segBinOpShape $ slugOp slug
          vec_is = shapeDims $ slugShape slug
      forM (zip3 (slugAccs slug) redout $ lambdaReturnType lam) $ \((acc, acc_is), redout', t) -> do
        let array_shape = shapeDims $ shape <> arrayShape t
        case (vec_is, array_shape) of
          ([], []) -> do
            let pt = elemType t
            dPrim_ redout' pt
            -- Initialize redouts
            copyDWIMFix redout' [] (Var acc) acc_is
          _ -> return mempty

  fbody <- collect $ do
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi64 flat_idx
    dScope Nothing $ scopeOfLParams $ concatMap slugParams slugs
    kbody $ \all_red_res -> do
      let all_red_res' = segBinOpChunks (map slugOp slugs) all_red_res
      forM_ (zip3 all_red_res' slugs redouts) $ \(red_res, slug, redout) ->
        sLoopNest (slugShape slug) $ \vec_is -> do
          let shape = segBinOpShape $ slugOp slug
          let lamtypes = lambdaReturnType $ segBinOpLambda $ slugOp slug
          -- Load accum params
          sComment "Load accum params" $
            case vec_is of
              [] -> forM_ (zip4 (accParams slug) redout lamtypes $ slugAccs slug) $ \(p, redout', t, (acc, acc_is)) ->
                case shapeDims $ shape <> arrayShape t of
                  [] -> copyDWIMFix (paramName p) [] (Var redout') []
                  _ -> copyDWIMFix (paramName p) [] (Var acc) (acc_is ++ vec_is)
              _ -> forM_ (zip (accParams slug) (slugAccs slug)) $ \(p, (acc, acc_is)) ->
                copyDWIMFix (paramName p) [] (Var acc) (acc_is ++ vec_is)

          sComment "Load next params" $
            forM_ (zip (nextParams slug) red_res) $ \(p, (res, res_is)) ->
              copyDWIMFix (paramName p) [] res (res_is ++ vec_is)

          sComment "Red body" $
            compileStms mempty (bodyStms $ slugBody slug) $
              case vec_is of
                [] -> forM_ (zip4 redout (bodyResult $ slugBody slug) lamtypes $ slugAccs slug) $ \(redout', se, t, (acc, acc_is)) ->
                  case shapeDims $ shape <> arrayShape t of
                    [] -> copyDWIMFix redout' [] se []
                    _ -> copyDWIMFix acc (acc_is ++ vec_is) se []
                _ -> forM_ (zip (slugAccs slug) (bodyResult $ slugBody slug)) $
                  \((acc, acc_is), se) ->
                    copyDWIMFix acc (acc_is ++ vec_is) se []

  post_body <- collect $
    forM_ (zip slugs redouts) $ \(slug, redout) -> do
      let lam = segBinOpLambda $ slugOp slug
      forM (zip3 (slugAccs slug) redout $ lambdaReturnType lam) $ \((acc, acc_is), redout', t) -> do
        let vec_is = shapeDims $ slugShape slug
            shape = segBinOpShape $ slugOp slug
            array_shape = shapeDims $ shape <> arrayShape t
        case (vec_is, array_shape) of
          ([], []) ->
            -- write back result to accum array redouts
            copyDWIMFix acc acc_is (Var redout') []
          _ -> return mempty

  free_params <- freeParams (prebody <> fbody) (segFlat space : [flat_idx])
  let (body_allocs, fbody') = extractAllocations fbody
  emit $ Imp.Op $ Imp.ParLoop "segred_stage_1" flat_idx (body_allocs <> prebody) fbody' post_body free_params $ segFlat space

reductionStage2 ::
  Pattern MCMem ->
  SegSpace ->
  VName ->
  [SegBinOpSlug] ->
  MulticoreGen ()
reductionStage2 pat space nsubtasks slugs = do
  let per_red_pes = segBinOpChunks (map slugOp slugs) $ patternValueElements pat
      nsubtasks' = Imp.vi32 nsubtasks
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
              copyDWIMFix (paramName p) [] (Var acc) (acc_is ++ vec_is)
          sComment "red body" $
            compileStms mempty (bodyStms $ slugBody slug) $
              forM_ (zip red_res (bodyResult $ slugBody slug)) $
                \(pe, se') -> copyDWIMFix (patElemName pe) vec_is se' []

-- Each thread reduces over the number of segments
-- each of which is done sequentially
-- Maybe we should select the work of the inner loop
-- based on n_segments and dimensions etc.
segmentedReduction ::
  Pattern MCMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  DoSegBody ->
  MulticoreGen Imp.Code
segmentedReduction pat space reds kbody =
  collect $ do
    n_par_segments <- dPrim "segment_iter" $ IntType Int64
    body <- compileSegRedBody n_par_segments pat space reds kbody
    free_params <- freeParams body (segFlat space : [n_par_segments])
    let (body_allocs, body') = extractAllocations body
    emit $ Imp.Op $ Imp.ParLoop "segmented_segred" n_par_segments body_allocs body' mempty free_params $ segFlat space

compileSegRedBody ::
  VName ->
  Pattern MCMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  DoSegBody ->
  MulticoreGen Imp.Code
compileSegRedBody n_segments pat space reds kbody = do
  let (is, ns) = unzip $ unSegSpace space
      ns_64 = map (sExt64 . toInt32Exp) ns
      inner_bound = last ns_64
      n_segments' = Imp.vi32 n_segments

  let per_red_pes = segBinOpChunks reds $ patternValueElements pat
  -- Perform sequential reduce on inner most dimension
  collect $ do
    flat_idx <- dPrimVE "flat_idx" $ sExt64 n_segments' * inner_bound
    zipWithM_ dPrimV_ is $ map sExt32 $ unflattenIndex ns_64 flat_idx
    sComment "neutral-initialise the accumulators" $
      forM_ (zip per_red_pes reds) $ \(pes, red) ->
        forM_ (zip pes (segBinOpNeutral red)) $ \(pe, ne) ->
          sLoopNest (segBinOpShape red) $ \vec_is ->
            copyDWIMFix (patElemName pe) (map Imp.vi32 (init is) ++ vec_is) ne []

    sComment "main body" $ do
      dScope Nothing $ scopeOfLParams $ concatMap (lambdaParams . segBinOpLambda) reds
      sFor "i" inner_bound $ \i -> do
        forM_ (zip (init is) $ map sExt32 $ unflattenIndex (init ns_64) (sExt64 n_segments')) $ uncurry (<--)
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
