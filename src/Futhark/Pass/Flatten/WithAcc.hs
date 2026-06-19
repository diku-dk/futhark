-- | Flattening of 'WithAcc'.
--
-- The basic idea is that in the nonuniform case, we change the 'WithAcc' to be
-- over the data array of the irregular representation. We then update all the
-- 'UpdateAcc' operations to compute flat indexes, via the usual metadata
-- arrays.
module Futhark.Pass.Flatten.WithAcc
  ( transformWithAcc,
  )
where

import Control.Monad
import Data.Foldable
import Data.List qualified as L
import Data.Map qualified as M
import Debug.Trace
import Futhark.IR.GPU
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Pass.ExtractKernels.ToGPU (soacsLambdaToGPU)
import Futhark.Pass.Flatten.Builtins
import Futhark.Pass.Flatten.Distribute
import Futhark.Pass.Flatten.Monad
import Futhark.Tools
import Prelude hiding (div, rem)

indexIrreg ::
  (MonadBuilder m) =>
  Segments ->
  DistEnv ->
  IrregularRep ->
  SubExp ->
  ShapeBase SubExp ->
  [SubExp] ->
  m SubExp
indexIrreg _segments _env rep is shape js = do
  offset <- letSubExp "uacc_segment_offset" =<< eIndex (irregularO rep) [eSubExp is]
  letSubExp "flat_uacc_idx" <=< toExp $
    pe64 offset
      + flattenIndex
        (map pe64 (shapeDims shape))
        (map pe64 js)

-- If just one input is nonuniform, we treat them all as nonuniform.
transformWithAcc ::
  FlattenOps ->
  Segments ->
  DistEnv ->
  DistInputs ->
  [DistResult] ->
  Pat Type ->
  StmAux () ->
  [WithAccInput SOACS] ->
  Lambda SOACS ->
  Builder GPU DistEnv
transformWithAcc ops segments env inps distres _withacc_pat withacc_aux withacc_inputs acc_lam = do
  let inputTypes (_, arrs, _) = mapM (lookupInputType inps) arrs

  lam_params' <- newAccLamParams $ lambdaParams acc_lam

  iota_w <- genShapeIota (flattenSegLevel ops) $ segmentsShape segments

  iota_p <- newParam "iota_p" $ Prim int64
  -- Type in DistInputFree is parameter type
  let iota_w_t = Prim int64
  let iota_se = Var (paramName iota_p)

  -- Potentially change to distres option.
  nonuniform <-
    any (any (any (isVariant inps env) . arrayDims))
      <$> mapM inputTypes withacc_inputs

  (withacc_inputs', trAccIndex, non_uniform_reps) <-
    if nonuniform
      then do
        (withacc_inputs', input_reps) <-
          mapAndUnzipM onNonuniformInput withacc_inputs
        let trAccIndex c is = do
              ((shape, _, _), rep : _) <-
                L.lookup c $
                  zip (map paramName lam_params') $
                    zip withacc_inputs input_reps
              Just $ L.singleton <$> indexIrreg segments env rep iota_se shape is
        pure (withacc_inputs', trAccIndex, concat input_reps)
      else do
        withacc_inputs' <- mapM onUniformInput withacc_inputs
        let trAccIndex c is = do
              _ <- L.lookup c $ zip (map paramName lam_params') withacc_inputs
              Just $ do
                iota_se_unflat <-
                  mapM (letSubExp "iota_idx" <=< toExp) $
                    unflattenIndex (segmentDims segments) (pe64 iota_se)
                pure $ iota_se_unflat ++ is
        pure (withacc_inputs', trAccIndex, [])
  let trAccShape c = do
        (ispace, _, _) <- L.lookup c $ zip (map paramName lam_params') withacc_inputs'
        pure ispace
      sf = (trAccShape, trAccIndex)

  acc_lam_body <-
    runBodyBuilder $
      localScope (scopeOfLParams lam_params') $
        bodyBind . lambdaBody =<< trLam sf acc_lam

  scope <- askScope
  let orig_acc_params = drop num_accs $ lambdaParams acc_lam
      lam_params_tr = map (trParam sf) lam_params'
      acc_params_tr = drop num_accs lam_params_tr
      interchanged_inps =
        (paramName iota_p, DistInputFree iota_w iota_w_t)
          : [ (paramName p, DistInputFree (paramName acc) (paramType acc))
            | -- This could potentially be wrong but since it's acc type it should be fine.
              (p, acc) <- zip orig_acc_params acc_params_tr
            ]
          ++ inps

  traceM $
    unlines
      [ "transformWithAcc",
        show nonuniform,
        prettyString interchanged_inps,
        prettyString acc_lam_body
      ]

  let (withacc_new_inputs, withacc_dstms) =
        distributeBody
          (flattenFunHasParallelism ops)
          scope
          segments
          interchanged_inps
          acc_lam_body

  withacc_lam' <- localScope (scopeOfDistInputs inps)
    . mkLambda (map (trParam sf) lam_params')
    $ do
      env' <- foldM (flattenDistStm ops segments) env withacc_dstms
      reps <-
        mapM
          (liftWithAccResult (flattenSegLevel ops) segments withacc_new_inputs env')
          (zip distres (bodyResult $ lambdaBody acc_lam))
      concat <$> mapM repToResults reps

  withacc_out_vs <-
    certifying (distCerts inps withacc_aux env) $
      letTupExp "withacc_flatten_out" (WithAcc withacc_inputs' withacc_lam')

  -- The accumulator results are handled differently in nonuniform casesince we do not have metadata
  -- for them and since all of them are turned flat even when they might be actually regular.
  -- we can still here turn the actul disrest that are regular to regualars.
  let (withacc_out_vs_wo, withacc_out_vs_no) = splitAt num_accs withacc_out_vs
      (distres_withacc, distres_normal) = splitAt num_accs distres

  let out_reps_normal = mkNormalResReps distres_normal withacc_out_vs_no
  out_reps_withacc <-
    if nonuniform
      then mapM mkNonuniformWithAccRep (zip3 withacc_out_vs_wo non_uniform_reps distres_withacc)
      else pure $ map Regular withacc_out_vs_wo
  pure $ insertReps (zip (map distResTag $ distres_withacc ++ distres_normal) (out_reps_withacc ++ out_reps_normal)) env
  where
    newAccLamParams ps = do
      let (cert_ps, acc_ps) = splitAt num_accs ps
      -- Should not rename the certificates.
      acc_ps' <- forM acc_ps $ \(Param attrs v t) ->
        Param attrs <$> newName v <*> pure t
      pure $ cert_ps <> acc_ps'

    num_accs = length withacc_inputs

    onOpWithIndexRank index_rank (op_lam, nes) = do
      -- We need to add an additional index parameter because we are
      -- extending the index space of the accumulator.
      -- In the unifrom case we have the full index space of the segments, while in the nonuniform case we only have one additional dimension.
      idx_ps <- replicateM index_rank $ newParam "idx" $ Prim int64
      pure
        ( soacsLambdaToGPU $
            op_lam {lambdaParams = idx_ps <> lambdaParams op_lam},
          nes
        )

    -- Let's use liftSubExpRegular here
    onUniformInput (shape, arrs, op) =
      (segmentsShape segments <> shape,,)
        <$> mapM onArr arrs
        <*> traverse (onOpWithIndexRank (segmentsRank segments)) op
      where
        onArr arr = do
          arr_t <- lookupInputType inps arr
          let arr_shape = arrayShape arr_t
              expected_shape = segmentsShape segments <> arr_shape
          liftSubExpRegular (flattenSegLevel ops) segments inps env expected_shape (Var arr)

    onNonuniformInput (_shape, arrs, op) = do
      reps <- mapM (getIrregRep (flattenSegLevel ops) segments env inps) arrs
      -- We need to ensure that the irregular arrays are dense
      reps_dense <- mapM (ensureDenseIrregular (flattenSegLevel ops) "withacc_input") reps
      let arrs' = map irregularD reps_dense
      w <- fmap (arraySize 0) . lookupType $ head arrs'
      (,reps_dense) . (Shape [w],arrs',) <$> traverse (onOpWithIndexRank 1) op

    liftWithAccResult lvl segs inputs env' (dist_res, res) =
      case resSubExp res of
        Var v -> do
          let (Just (t, rep)) = M.lookup v $ inputReps inputs env'
          if isAcc t
            then
              pure rep
            else
              liftDistResultRep lvl segs inputs env' dist_res res
        Constant _ -> liftDistResultRep lvl segs inputs env' dist_res res

    repToResults (Regular v) =
      pure [SubExpRes mempty $ Var v]
    repToResults (Irregular irreg) =
      map (SubExpRes mempty . Var) <$> irregResults irreg

    irregResults
      ( IrregularRep
          { irregularS = segs,
            irregularF = flags,
            irregularO = offsets,
            irregularD = elems
          }
        ) = do
        flags_t <- lookupType flags
        t <- lookupType elems
        num_data <- letExp "num_data" =<< toExp (product $ map pe64 $ arrayDims t)
        let shape = Shape [Var num_data]
        flags' <- letExp "flags" $ BasicOp $ Reshape flags $ reshapeAll (arrayShape flags_t) shape
        elems' <- letExp "elems" $ BasicOp $ Reshape elems $ reshapeAll (arrayShape t) shape
        pure [num_data, segs, flags', offsets, elems']

    mkNonuniformWithAccRep (v, rep, dist_res)
      | isRegularDistResult dist_res = do
          let DistType _ _ t = distResType dist_res
              expectedShape = segmentsShape segments <> arrayShape t
          v_t <- lookupType v
          v_reshaped <-
            letExp "actual_regular_with_acc_res" . BasicOp $
              Reshape v (reshapeAll (arrayShape v_t) expectedShape)
          pure $ Regular v_reshaped
      | otherwise =
          pure $ Irregular $ rep {irregularD = v}

    mkNormalResReps :: [DistResult] -> [VName] -> [ResRep]
    mkNormalResReps dist_res results =
      snd $
        L.mapAccumL
          ( \rs dist_res' ->
              if isRegularDistResult dist_res'
                then
                  let (v : rs') = rs
                   in (rs', Regular v)
                else
                  let (_ : segs : flags : offsets : elems : rs') = rs
                   in (rs', Irregular $ IrregularRep segs flags offsets elems Dense)
          )
          results
          dist_res

    trType ::
      (VName -> Maybe Shape, VName -> [SubExp] -> Maybe (Builder SOACS [SubExp])) ->
      TypeBase shape u ->
      TypeBase shape u
    trType sf (Acc acc _ ts u)
      | Just shape <- fst sf acc =
          Acc acc shape ts u
    trType _ t = t

    trParam ::
      (VName -> Maybe Shape, VName -> [SubExp] -> Maybe (Builder SOACS [SubExp])) ->
      Param (TypeBase Shape u) ->
      Param (TypeBase Shape u)
    trParam sf = fmap $ trType sf

    trBody sf (Body dec stms res) =
      Body dec <$> collectStms_ (traverse_ onStm stms) <*> pure res
      where
        onStm (Let pat aux e) =
          addStm . Let (fmap (trType sf) pat) aux =<< trExp sf pat e

    trLam sf (Lambda params ret body) =
      Lambda (map (trParam sf) params) (map (trType sf) ret) <$> trBody sf body

    trSOAC sf = mapSOACM mapper
      where
        mapper =
          identitySOACMapper {mapOnSOACLambda = trLam sf}

    trExp sf _ (WithAcc acc_inputs lam) =
      WithAcc acc_inputs <$> trLam sf lam
    trExp sf (Pat [PatElem _ acc_t]) (BasicOp (UpdateAcc safety acc is ses)) = do
      case acc_t of
        Acc cert _ _ _
          | Just mk <- snd sf cert is -> do
              is' <- mk
              pure $ BasicOp $ UpdateAcc safety acc is' ses
        _ ->
          pure $ BasicOp $ UpdateAcc safety acc is ses
    trExp sf _ e = mapExpM mapper e
      where
        mapper =
          identityMapper
            { mapOnBody = \_ -> trBody sf,
              mapOnRetType = pure . trType sf,
              mapOnBranchType = pure . trType sf,
              mapOnFParam = pure . trParam sf,
              mapOnLParam = pure . trParam sf,
              mapOnOp = trSOAC sf
            }
