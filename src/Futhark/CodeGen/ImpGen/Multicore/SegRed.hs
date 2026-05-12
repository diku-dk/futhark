module Futhark.CodeGen.ImpGen.Multicore.SegRed
  ( compileSegRed,
    compileSegRed',
    DoSegBody,
  )
where

import Control.Monad
import Futhark.CodeGen.ImpCode.Multicore qualified as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.IR.MCMem
import Futhark.Transform.Rename (renameLambda)
import Prelude hiding (quot, rem)

type DoSegBody = (([[(SubExp, [Imp.TExp Int64])]] -> MulticoreGen ()) -> MulticoreGen ())

-- | Generate code for a SegRed construct
compileSegRed ::
  Pat LetDecMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  TV Int32 ->
  MulticoreGen ()
compileSegRed pat space reds kbody nsubtasks =
  compileSegRed' pat space reds nsubtasks $ \red_cont ->
    compileStms mempty (bodyStms kbody) $ do
      let (red_res, map_res) = splitAt (segBinOpResults reds) $ bodyResult kbody

      sComment "save map-out results" $ do
        let map_arrs = drop (segBinOpResults reds) $ patElems pat
        zipWithM_ (compileThreadResult space) map_arrs map_res

      red_cont $ segBinOpChunks reds $ map ((,[]) . kernelResultSubExp) red_res

-- | Like 'compileSegRed', but where the body is a monadic action.
compileSegRed' ::
  Pat LetDecMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  TV Int32 ->
  DoSegBody ->
  MulticoreGen ()
compileSegRed' pat space reds nsubtasks kbody
  | [_] <- unSegSpace space =
      nonsegmentedReduction pat space reds nsubtasks kbody
  | otherwise =
      segmentedReduction pat space reds kbody

-- | A SegBinOp with auxiliary information.
data SegBinOpSlug = SegBinOpSlug
  { slugOp :: SegBinOp MCMem,
    -- | The array in which we write the intermediate results, indexed
    -- by the flat/physical thread ID.
    slugResArrs :: [VName]
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

renameSlug :: SegBinOpSlug -> MulticoreGen SegBinOpSlug
renameSlug slug = do
  let op = slugOp slug
  let lambda = segBinOpLambda op
  lambda' <- renameLambda lambda
  let op' = op {segBinOpLambda = lambda'}
  pure slug {slugOp = op'}

-- | Arrays for storing group results shared between threads
groupResultArrays ::
  Name ->
  SubExp ->
  [SegBinOp MCMem] ->
  MulticoreGen [[VName]]
groupResultArrays s num_threads reds =
  forM reds $ \(SegBinOp _ lam _ shape) ->
    forM (lambdaReturnType lam) $ \t -> do
      let full_shape = Shape [num_threads] <> shape <> arrayShape t
      sAllocArray s (elemType t) full_shape DefaultSpace

nonsegmentedReduction ::
  Pat LetDecMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  TV Int32 ->
  DoSegBody ->
  MulticoreGen ()
nonsegmentedReduction pat space reds nsubtasks kbody = do
  thread_res_arrs <- groupResultArrays "reduce_stage_1_tid_res_arr" (tvSize nsubtasks) reds
  let slugs1 = zipWith SegBinOpSlug reds thread_res_arrs
      nsubtasks' = tvExp nsubtasks

  -- Are all the operators commutative?
  let comm = all ((== Commutative) . segBinOpComm) reds
  let dims = map (shapeDims . slugShape) slugs1
  let isScalar x = case x of MemPrim _ -> True; _ -> False
  -- Are we only working on scalar arrays?
  let scalars = all (all (isScalar . paramDec) . slugParams) slugs1 && all (== []) dims
  -- Are we working with vectorized inner maps?
  let inner_map = [] `notElem` dims

  let path
        | comm && scalars = reductionStage1CommScalar
        | inner_map = reductionStage1Array
        | scalars = reductionStage1NonCommScalar
        | otherwise = reductionStage1Fallback
  path space slugs1 kbody

  reds2 <- renameSegBinOp reds
  let slugs2 = zipWith SegBinOpSlug reds2 thread_res_arrs
  reductionStage2 pat space nsubtasks' slugs2

-- Generate code that declares the params for the binop
genBinOpParams :: [SegBinOpSlug] -> MulticoreGen ()
genBinOpParams slugs =
  dScope Nothing $ scopeOfLParams $ concatMap slugParams slugs

-- Generate code that declares accumulators, return a list of these
genAccumulators :: [SegBinOpSlug] -> MulticoreGen [[VName]]
genAccumulators slugs =
  forM slugs $ \slug -> do
    let shape = segBinOpShape $ slugOp slug
    forM (zip (accParams slug) (slugNeutral slug)) $ \(p, ne) -> do
      -- Declare accumulator variable.
      acc <-
        case paramType p of
          Prim pt
            | shape == mempty ->
                dPrimS "local_acc" pt
            | otherwise ->
                sAllocArray "local_acc" pt shape DefaultSpace
          _ ->
            pure $ paramName p

      -- Now neutral-initialise the accumulator.
      sLoopNest (slugShape slug) $ \vec_is ->
        copyDWIMFix acc vec_is ne []

      pure acc

-- Datatype to represent all the different ways we can generate
-- code for a reduction.
data RedLoopType
  = RedSeq -- Fully sequential
  | RedComm -- Commutative scalar
  | RedNonComm -- Noncommutative scalar
  | RedNested -- Nested vectorized operator
  | RedUniformize -- Uniformize over scalar acc

-- Given a type of reduction and the loop index, should we wrap
-- the loop body in some extra code?
getRedLoop ::
  RedLoopType ->
  Imp.TExp Int64 ->
  (Imp.TExp Int64 -> MulticoreGen ()) ->
  MulticoreGen ()
getRedLoop RedNonComm _ = generateUniformizeLoop
getRedLoop RedUniformize uni = \body -> body uni
getRedLoop _ _ = \body -> body 0

-- Given a type of reduction, should we perform extracts on
-- the accumulator?
getExtract ::
  RedLoopType ->
  Imp.TExp Int64 ->
  MulticoreGen Imp.MCCode ->
  MulticoreGen ()
getExtract RedNonComm = extractVectorLane
getExtract RedUniformize = extractVectorLane
getExtract _ = \_ body -> body >>= emit

-- Given a type of reduction, should we vectorize the inner
-- map, if it exists?
getNestLoop ::
  RedLoopType ->
  Shape ->
  ([Imp.TExp Int64] -> MulticoreGen ()) ->
  MulticoreGen ()
getNestLoop RedNested = sLoopNestVectorized
getNestLoop _ = sLoopNest

-- Given a list of accumulators, use them as the source
-- data for reduction.
redSourceAccs :: [[VName]] -> DoSegBody
redSourceAccs slug_local_accs m =
  m $ map (map (\x -> (Var x, []))) slug_local_accs

-- Generate a reduction loop for uniformizing vectors
genPostbodyReductionLoop ::
  [[VName]] ->
  [SegBinOpSlug] ->
  [[VName]] ->
  SegSpace ->
  Imp.TExp Int64 ->
  MulticoreGen ()
genPostbodyReductionLoop accs =
  genReductionLoop RedUniformize (redSourceAccs accs)

-- Generate a potentially vectorized body of code that performs reduction
-- when put inside a chunked loop.
genReductionLoop ::
  RedLoopType ->
  DoSegBody ->
  [SegBinOpSlug] ->
  [[VName]] ->
  SegSpace ->
  Imp.TExp Int64 ->
  MulticoreGen ()
genReductionLoop typ kbodymap slugs slug_local_accs space i = do
  let (is, ns) = unzip $ unSegSpace space
      ns' = map pe64 ns
  zipWithM_ dPrimV_ is $ unflattenIndex ns' i
  kbodymap $ \all_red_res' -> do
    forM_ (zip3 all_red_res' slugs slug_local_accs) $ \(red_res, slug, local_accs) ->
      getNestLoop typ (slugShape slug) $ \vec_is -> do
        let lamtypes = lambdaReturnType $ segBinOpLambda $ slugOp slug
        getRedLoop typ i $ \uni -> do
          sComment "Load accum params" $
            forM_ (zip3 (accParams slug) local_accs lamtypes) $
              \(p, local_acc, t) ->
                when (primType t) $ do
                  copyDWIMFix (paramName p) [] (Var local_acc) vec_is

          sComment "Load next params" $
            forM_ (zip (nextParams slug) red_res) $ \(p, (res, res_is)) -> do
              getExtract typ uni $
                collect $
                  copyDWIMFix (paramName p) [] res (res_is ++ vec_is)

          sComment "SegRed body" $
            compileStms mempty (bodyStms $ slugBody slug) $
              forM_ (zip local_accs $ map resSubExp $ bodyResult $ slugBody slug) $
                \(local_acc, se) ->
                  copyDWIMFix local_acc vec_is se []

-- Generate code to write back results from the accumulators
genWriteBack :: [SegBinOpSlug] -> [[VName]] -> SegSpace -> MulticoreGen ()
genWriteBack slugs slug_local_accs space =
  forM_ (zip slugs slug_local_accs) $ \(slug, local_accs) ->
    forM (zip (slugResArrs slug) local_accs) $ \(acc, local_acc) ->
      copyDWIMFix acc [Imp.le64 $ segFlat space] (Var local_acc) []

type ReductionStage1 = SegSpace -> [SegBinOpSlug] -> DoSegBody -> MulticoreGen ()

-- Pure sequential codegen with no fancy vectorization
reductionStage1Fallback :: ReductionStage1
reductionStage1Fallback space slugs kbody = do
  fbody <- collect $ do
    dPrim_ (segFlat space) int64
    sOp $ Imp.GetTaskId (segFlat space)
    -- Declare params
    genBinOpParams slugs
    slug_local_accs <- genAccumulators slugs
    -- Generate main reduction loop
    generateChunkLoop "SegRed" Scalar $
      genReductionLoop RedSeq kbody slugs slug_local_accs space
    -- Write back results
    genWriteBack slugs slug_local_accs space
  free_params <- freeParams fbody
  emit $ Imp.Op $ Imp.ParLoop "segred_stage_1" fbody free_params

-- Codegen for noncommutative scalar reduction. We vectorize the
-- kernel body, and do the reduction sequentially.
reductionStage1NonCommScalar :: ReductionStage1
reductionStage1NonCommScalar space slugs kbody = do
  fbody <- collect $ do
    dPrim_ (segFlat space) int64
    sOp $ Imp.GetTaskId (segFlat space)
    inISPC $ do
      -- Declare params
      genBinOpParams slugs
      slug_local_accs <- genAccumulators slugs
      -- Generate main reduction loop
      generateChunkLoop "SegRed" Vectorized $
        genReductionLoop RedNonComm kbody slugs slug_local_accs space
      -- Write back results
      genWriteBack slugs slug_local_accs space
  free_params <- freeParams fbody
  emit $ Imp.Op $ Imp.ParLoop "segred_stage_1" fbody free_params

-- Codegen for a commutative reduction on scalar arrays
-- In this case, we can generate an efficient interleaved reduction
reductionStage1CommScalar :: ReductionStage1
reductionStage1CommScalar space slugs kbody = do
  fbody <- collect $ do
    dPrim_ (segFlat space) int64
    sOp $ Imp.GetTaskId (segFlat space)
    -- Rename lambda params in slugs to get a new set of them
    slugs' <- mapM renameSlug slugs
    inISPC $ do
      -- Declare one set of params uniform
      genBinOpParams slugs'
      slug_local_accs_uni <- genAccumulators slugs'
      -- Declare the other varying
      genBinOpParams slugs
      slug_local_accs <- genAccumulators slugs
      -- Generate the main reduction loop over vectors
      generateChunkLoop "SegRed" Vectorized $
        genReductionLoop RedComm kbody slugs slug_local_accs space
      -- Now reduce over those vector accumulators to get scalar results
      generateUniformizeLoop $
        genPostbodyReductionLoop slug_local_accs slugs' slug_local_accs_uni space
      -- And write back the results
      genWriteBack slugs slug_local_accs_uni space
  free_params <- freeParams fbody
  emit $ Imp.Op $ Imp.ParLoop "segred_stage_1" fbody free_params

-- Codegen for a reduction on arrays, where the body is a perfect nested map.
-- We vectorize just the inner map.
reductionStage1Array :: ReductionStage1
reductionStage1Array space slugs kbody = do
  fbody <- collect $ do
    dPrim_ (segFlat space) int64
    sOp $ Imp.GetTaskId (segFlat space)
    -- Declare params
    lparams <- collect $ genBinOpParams slugs
    (slug_local_accs, uniform_prebody) <- collect' $ genAccumulators slugs
    -- Put the accumulators outside of the kernel, so they are forced uniform
    emit uniform_prebody
    inISPC $ do
      -- Put the lambda params inside the kernel so they are varying
      emit lparams
      -- Generate the main reduction loop
      generateChunkLoop "SegRed" Scalar $
        genReductionLoop RedNested kbody slugs slug_local_accs space
      -- Write back results
      genWriteBack slugs slug_local_accs space
  free_params <- freeParams fbody
  emit $ Imp.Op $ Imp.ParLoop "segred_stage_1" fbody free_params

reductionStage2 ::
  Pat LetDecMem ->
  SegSpace ->
  Imp.TExp Int32 ->
  [SegBinOpSlug] ->
  MulticoreGen ()
reductionStage2 pat space nsubtasks slugs = do
  let per_red_pes = segBinOpChunks (map slugOp slugs) $ patElems pat
      phys_id = Imp.le64 (segFlat space)
  sComment "neutral-initialise the output" $
    forM_ (zip (map slugOp slugs) per_red_pes) $ \(red, red_res) ->
      forM_ (zip red_res $ segBinOpNeutral red) $ \(pe, ne) ->
        sLoopNest (segBinOpShape red) $ \vec_is ->
          copyDWIMFix (patElemName pe) vec_is ne []

  dScope Nothing $ scopeOfLParams $ concatMap slugParams slugs

  sFor "i" nsubtasks $ \i' -> do
    mkTV (segFlat space) <-- i'
    sComment "Apply main thread reduction" $
      forM_ (zip slugs per_red_pes) $ \(slug, red_res) ->
        sLoopNest (slugShape slug) $ \vec_is -> do
          sComment "load acc params" $
            forM_ (zip (accParams slug) red_res) $ \(p, pe) ->
              copyDWIMFix (paramName p) [] (Var $ patElemName pe) vec_is
          sComment "load next params" $
            forM_ (zip (nextParams slug) (slugResArrs slug)) $ \(p, acc) ->
              copyDWIMFix (paramName p) [] (Var acc) (phys_id : vec_is)
          sComment "red body" $
            compileStms mempty (bodyStms $ slugBody slug) $
              forM_ (zip red_res $ map resSubExp $ bodyResult $ slugBody slug) $
                \(pe, se') -> copyDWIMFix (patElemName pe) vec_is se' []

-- Each thread reduces over the number of segments
-- each of which is done sequentially
-- Maybe we should select the work of the inner loop
-- based on n_segments and dimensions etc.
segmentedReduction ::
  Pat LetDecMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  DoSegBody ->
  MulticoreGen ()
segmentedReduction pat space reds kbody = do
  body <- collect $ compileSegRedBody pat space reds kbody
  free_params <- freeParams body
  emit $ Imp.Op $ Imp.ParLoop "segmented_segred" body free_params

-- Currently, this is only used as part of SegHist calculations, never alone.
compileSegRedBody ::
  Pat LetDecMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  DoSegBody ->
  MulticoreGen ()
compileSegRedBody pat space reds kbody = do
  let (is, ns) = unzip $ unSegSpace space
      ns_64 = map pe64 ns
      inner_bound = last ns_64
  dPrim_ (segFlat space) int64
  sOp $ Imp.GetTaskId (segFlat space)

  let per_red_pes = segBinOpChunks reds $ patElems pat
  -- Perform sequential reduce on inner most dimension
  inISPC $ generateChunkLoop "SegRed" Vectorized $ \n_segments -> do
    flat_idx <- dPrimVE "flat_idx" $ n_segments * inner_bound
    zipWithM_ dPrimV_ is $ unflattenIndex ns_64 flat_idx
    sComment "neutral-initialise the accumulators" $
      forM_ (zip per_red_pes reds) $ \(pes, red) ->
        forM_ (zip pes (segBinOpNeutral red)) $ \(pe, ne) ->
          sLoopNest (segBinOpShape red) $ \vec_is ->
            copyDWIMFix (patElemName pe) (map Imp.le64 (init is) ++ vec_is) ne []

    sComment "main body" $ do
      dScope Nothing $ scopeOfLParams $ concatMap (lambdaParams . segBinOpLambda) reds
      sFor "i" inner_bound $ \i -> do
        zipWithM_
          (<--)
          (map mkTV $ init is)
          (unflattenIndex (init ns_64) (sExt64 n_segments))
        dPrimV_ (last is) i
        kbody $ \red_res' -> do
          forM_ (zip3 per_red_pes reds red_res') $ \(pes, red, res') ->
            sLoopNest (segBinOpShape red) $ \vec_is -> do
              sComment "load accum" $ do
                let acc_params = take (length (segBinOpNeutral red)) $ (lambdaParams . segBinOpLambda) red
                forM_ (zip acc_params pes) $ \(p, pe) ->
                  copyDWIMFix (paramName p) [] (Var $ patElemName pe) (map Imp.le64 (init is) ++ vec_is)

              sComment "load new val" $ do
                let next_params = drop (length (segBinOpNeutral red)) $ (lambdaParams . segBinOpLambda) red
                forM_ (zip next_params res') $ \(p, (res, res_is)) ->
                  copyDWIMFix (paramName p) [] res (res_is ++ vec_is)

              sComment "apply reduction" $ do
                let lbody = (lambdaBody . segBinOpLambda) red
                compileStms mempty (bodyStms lbody) $
                  sComment "write back to res" $
                    forM_ (zip pes $ map resSubExp $ bodyResult lbody) $
                      \(pe, se') -> copyDWIMFix (patElemName pe) (map Imp.le64 (init is) ++ vec_is) se' []
