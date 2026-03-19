-- | Flattening of 'WithAcc'.
module Futhark.Pass.Flatten.WithAcc
  ( transformWithAcc,
  )
where

import Control.Monad
import Control.Monad.Identity
import Data.Foldable
import Data.List.NonEmpty qualified as NE
import Futhark.IR.GPU
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Pass.ExtractKernels.ToGPU (soacsLambdaToGPU)
import Futhark.Pass.Flatten.Builtins
import Futhark.Pass.Flatten.Distribute
import Futhark.Pass.Flatten.Monad
import Futhark.Tools
import Prelude hiding (div, rem)

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
  variant <-
    localScope (scopeOfDistInputs inps) $
      any (any (any (isVariant inps env) . arrayDims))
        <$> mapM inputTypes withacc_inputs
  when variant $ error "Cannot yet handle variant WithAccs"

  withacc_inputs' <- mapM onInput withacc_inputs
  lam_params' <- newAccLamParams $ lambdaParams acc_lam

  iota_w <- genShapeIota $ segmentsShape segments

  iota_p <- newParam "iota_p" $ Prim int64

  iota_w_t <- lookupType iota_w
  let iota_se = Var (paramName iota_p)

  acc_lam_body <-
    runBodyBuilder $
      localScope (scopeOfLParams lam_params') $
        bodyBind (lambdaBody (trLam iota_se acc_lam))

  scope <- askScope
  let acc_params = drop num_accs lam_params'
      orig_acc_params = drop num_accs $ lambdaParams acc_lam
      interchanged_inps =
        (paramName iota_p, DistInputFree iota_w iota_w_t)
          : [ (paramName p, DistInputFree (paramName acc) (paramType acc))
            | (p, acc) <- zip orig_acc_params acc_params
            ]
          ++ inps
      [w] = NE.toList segments
      -- FIXME: we are not using withacc_new_inputs, which has got to be wrong.
      (withacc_new_inputs, withacc_dstms) =
        distributeBody
          scope
          segments
          interchanged_inps
          acc_lam_body

  withacc_lam' <- mkLambda (map trParam lam_params') $ do
    env' <- foldM (flattenDistStm ops segments) env withacc_dstms
    concat <$> mapM (liftResult segments inps env') (bodyResult $ lambdaBody acc_lam)

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
    acc_certs = map paramName $ take num_accs $ lambdaParams acc_lam

    onOp (op_lam, nes) = do
      -- We need to add an additional index parameter because we are
      -- extending the index space of the accumulator.
      idx_p <- newParam "idx" $ Prim int64
      pure
        ( soacsLambdaToGPU $ op_lam {lambdaParams = idx_p : lambdaParams op_lam},
          nes
        )

    onInput (shape, arrs, op) =
      (segmentsShape segments <> shape,,)
        <$> mapM onArr arrs
        <*> traverse onOp op

    onArr = readInputVar segments env [] inps

    trType :: TypeBase shape u -> TypeBase shape u
    trType (Acc acc ispace ts u)
      | acc `elem` acc_certs =
          Acc acc (segmentsShape segments <> ispace) ts u
    trType t = t

    trParam :: Param (TypeBase shape u) -> Param (TypeBase shape u)
    trParam = fmap trType

    trStm i (Let pat aux e) =
      Let (fmap trType pat) aux $ trExp i pat e

    trBody i (Body dec stms res) =
      Body dec (fmap (trStm i) stms) res

    trLam i (Lambda params ret body) =
      Lambda (map trParam params) (map trType ret) (trBody i body)

    trSOAC i = runIdentity . mapSOACM mapper
      where
        mapper =
          identitySOACMapper {mapOnSOACLambda = pure . trLam i}

    trExp i _ (WithAcc acc_inputs lam) =
      WithAcc acc_inputs $ trLam i lam
    trExp i (Pat [PatElem _ acc_t]) (BasicOp (UpdateAcc safety acc is ses)) = do
      case acc_t of
        Acc cert _ _ _
          | cert `elem` acc_certs ->
              BasicOp $ UpdateAcc safety acc (i : is) ses
        _ ->
          BasicOp $ UpdateAcc safety acc is ses
    trExp i _ e = mapExp mapper e
      where
        mapper =
          identityMapper
            { mapOnBody = \_ -> pure . trBody i,
              mapOnRetType = pure . trType,
              mapOnBranchType = pure . trType,
              mapOnFParam = pure . trParam,
              mapOnLParam = pure . trParam,
              mapOnOp = pure . trSOAC i
            }
