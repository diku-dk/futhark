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

flatSegmentIndex :: Segments -> [SubExp] -> TPrimExp Int64 VName
flatSegmentIndex segments = flattenIndex (segmentDims segments) . map pe64

indexIrreg ::
  (MonadBuilder m) =>
  Segments ->
  DistEnv ->
  IrregularRep ->
  [SubExp] ->
  ShapeBase SubExp ->
  [SubExp] ->
  m SubExp
indexIrreg segments _env rep is shape js = do
  offset <- letSubExp "uacc_segment_offset" =<< eIndex (irregularO rep) [toExp $ flatSegmentIndex segments is]
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
  let inputTypes (_, arrs, _) = mapM lookupType arrs

  lam_params' <- newAccLamParams $ lambdaParams acc_lam

  iota_w <- genShapeIota (flattenSegLevel ops) $ segmentsShape segments

  iota_p <- newParam "iota_p" $ Prim int64
  iota_w_t <- lookupType iota_w
  let iota_se = Var (paramName iota_p)

  nonuniform <-
    localScope (scopeOfDistInputs inps) $
      any (any (any (isVariant inps env) . arrayDims))
        <$> mapM inputTypes withacc_inputs

  (withacc_inputs', trAccIndex) <-
    if nonuniform
      then do
        (withacc_inputs', input_reps) <-
          mapAndUnzipM onNonuniformInput withacc_inputs
        let trAccIndex c is = do
              ((shape, _, _), rep : _) <-
                L.lookup c $
                  zip (map paramName lam_params') $
                    zip withacc_inputs input_reps
              Just $ L.singleton <$> indexIrreg segments env rep [iota_se] shape is
        pure (withacc_inputs', trAccIndex)
      else do
        withacc_inputs' <- mapM onUniformInput withacc_inputs
        let trAccIndex c is = do
              _ <-
                L.lookup c $ zip (map paramName lam_params') withacc_inputs
              Just $ pure $ iota_se : is
        pure (withacc_inputs', trAccIndex)

  let trAccShape c = do
        (ispace, _, _) <- L.lookup c $ zip (map paramName lam_params') withacc_inputs'
        pure ispace
      sf = (trAccShape, trAccIndex)

  acc_lam_body <-
    runBodyBuilder $
      localScope (scopeOfLParams lam_params') $
        bodyBind . lambdaBody =<< trLam sf acc_lam

  scope <- askScope
  let acc_params = drop num_accs lam_params'
      orig_acc_params = drop num_accs $ lambdaParams acc_lam
      interchanged_inps =
        (paramName iota_p, DistInputFree iota_w iota_w_t)
          : [ (paramName p, DistInputFree (paramName acc) (paramType acc))
            | (p, acc) <- zip orig_acc_params acc_params
            ]
          ++ inps

  traceM $
    unlines
      [ "transformWithAcc",
        show nonuniform,
        prettyString interchanged_inps,
        prettyString acc_lam_body
      ]

  -- FIXME: we are not using withacc_new_inputs, which has got to be wrong.
  let (withacc_new_inputs, withacc_dstms) =
        distributeBody
          scope
          segments
          interchanged_inps
          acc_lam_body

  withacc_lam' <- mkLambda (map (trParam sf) lam_params') $ do
    env' <- foldM (flattenDistStm ops segments) env withacc_dstms
    -- TODO: Isn't this the fix that we need?
    concat <$> mapM (liftResult (flattenSegLevel ops) segments withacc_new_inputs env') (bodyResult $ lambdaBody acc_lam)

  withacc_out_vs <-
    certifying (distCerts inps withacc_aux env) $
      letTupExp "withacc_flatten_out" (WithAcc withacc_inputs' withacc_lam')

  let out_reps = map Regular withacc_out_vs
  pure $ insertReps (zip (map distResTag distres) out_reps) env
  where
    newAccLamParams ps = do
      let (cert_ps, acc_ps) = splitAt num_accs ps
      -- Should not rename the certificates.
      acc_ps' <- forM acc_ps $ \(Param attrs v t) ->
        Param attrs <$> newName v <*> pure t
      pure $ cert_ps <> acc_ps'

    num_accs = length withacc_inputs

    onOp (op_lam, nes) = do
      -- We need to add an additional index parameter because we are
      -- extending the index space of the accumulator.
      idx_p <- newParam "idx" $ Prim int64
      pure
        ( soacsLambdaToGPU $ op_lam {lambdaParams = idx_p : lambdaParams op_lam},
          nes
        )

    onUniformInput (shape, arrs, op) =
      (segmentsShape segments <> shape,,)
        <$> mapM onArr arrs
        <*> traverse onOp op
      where
        onArr =
          readInputVar segments env [] inps

    onNonuniformInput (_shape, arrs, op) = do
      reps <- mapM (getIrregRep (flattenSegLevel ops) segments env inps) arrs
      let arrs' = map irregularD reps
      w <- fmap (arraySize 0) . lookupType $ head arrs'
      (,reps) . (Shape [w],arrs',) <$> traverse onOp op

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
