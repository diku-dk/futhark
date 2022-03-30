{-# LANGUAGE TypeFamilies #-}

module Futhark.Optimise.SeqAccs (seqAccsGPU) where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as M
import Futhark.IR.GPU
import Futhark.MonadFreshNames
import Futhark.Pass
import Futhark.Tools
import Futhark.Transform.Rename
import Prelude hiding (quot)

-- | A mapping from accumulator variables to their source.
type Accs rep = M.Map VName (WithAccInput rep)

type OptM = ReaderT (Scope GPU) (State VNameSource)

optimiseBody :: Accs GPU -> Body GPU -> OptM (Body GPU)
optimiseBody accs body = mkBody <$> optimiseStms accs (bodyStms body) <*> pure (bodyResult body)

updateType :: Accs GPU -> TypeBase Shape u -> TypeBase Shape u
updateType accs (Acc c _ [Prim pt] u)
  | Just (ispace, [_], _) <- M.lookup c accs =
    Array pt ispace u
updateType _ t = t

updatePat :: Accs GPU -> Pat Type -> Pat Type
updatePat accs = fmap (updateType accs)

optimiseExp :: Accs GPU -> Exp GPU -> OptM (Exp GPU)
optimiseExp accs = mapExpM mapper
  where
    mapper =
      identityMapper
        { mapOnBody = \scope body -> localScope scope $ optimiseBody accs body,
          mapOnFParam = onFParam
        }
    onFParam = pure . fmap (updateType accs)

optimiseStm :: Accs GPU -> Stm GPU -> OptM (Stms GPU)
optimiseStm accs (Let pat aux (WithAcc inputs lam)) = do
  localScope (scopeOfLParams $ map (fmap (updateType accs')) (lambdaParams lam)) $ do
    body <- optimiseBody accs' $ lambdaBody lam
    runBuilder_ $ do
      forM_ (zip acc_params inputs) $ \(p, (_, [arr], _)) ->
        auxing aux $ letBindNames [paramName p] $ BasicOp $ SubExp $ Var arr
      res <- bodyBind body
      forM_ (zip (patNames pat) res) $ \(v, r) ->
        certifying (resCerts r) $ letBindNames [v] $ BasicOp $ SubExp $ resSubExp r
  where
    acc_params = drop (length inputs) $ lambdaParams lam
    acc_names = map paramName $ take (length inputs) $ lambdaParams lam
    accs' = M.fromList (zip acc_names inputs) <> accs
optimiseStm accs (Let pat@(Pat [PatElem _ (Acc c _ _ _)]) aux (BasicOp (UpdateAcc acc is [v])))
  | Just (ispace, _, Just (lam, _)) <- M.lookup c accs = runBuilder_ $ do
    acc_t <- lookupType acc
    let slice = fullSlice acc_t $ map DimFix is
    ~[res] <- bodyBind <=< renameBody <=< runBodyBuilder $ do
      let [x_param, y_param] = drop (shapeRank ispace) $ lambdaParams lam
      letBindNames [paramName x_param] $ BasicOp $ Index acc slice
      letBindNames [paramName y_param] $ BasicOp $ SubExp v
      pure $ lambdaBody lam
    auxing aux $ letBind (updatePat accs pat) $ BasicOp $ Update Safe acc slice $ resSubExp res
optimiseStm accs (Let pat aux e) =
  oneStm . Let (updatePat accs pat) aux <$> optimiseExp accs e

optimiseStms :: Accs GPU -> Stms GPU -> OptM (Stms GPU)
optimiseStms accs stms =
  case stmsHead stms of
    Just (stms_h, stms_t) -> do
      stms_h' <- optimiseStm accs stms_h
      (stms_h' <>) <$> localScope (scopeOf stms_h') (optimiseStms accs stms_t)
    Nothing -> pure mempty

optimiseKernelBody :: Accs GPU -> KernelBody GPU -> OptM (KernelBody GPU)
optimiseKernelBody accs (KernelBody dec stms res) =
  KernelBody dec <$> optimiseStms accs stms <*> pure res

parallelBody :: Body GPU -> OptM (Body GPU)
parallelBody body =
  mkBody <$> parallelStms (bodyStms body) <*> pure (bodyResult body)

parallelExp :: Exp GPU -> OptM (Exp GPU)
parallelExp (Op (SegOp (SegMap lvl@SegThread {} space ts kbody))) = do
  kbody' <- optimiseKernelBody mempty kbody
  pure $ Op $ SegOp $ SegMap lvl space ts kbody'
parallelExp e = mapExpM mapper e
  where
    mapper =
      identityMapper
        { mapOnBody = \scope body -> localScope scope $ parallelBody body
        }

parallelStm :: Stm GPU -> OptM (Stm GPU)
parallelStm (Let pat aux e) = Let pat aux <$> parallelExp e

parallelStms :: Stms GPU -> OptM (Stms GPU)
parallelStms stms = localScope (scopeOf stms) $ traverse parallelStm stms

-- | The pass for GPU kernels.
seqAccsGPU :: Pass GPU GPU
seqAccsGPU =
  Pass "seq accs" "Turn sequential accumulators into in-place updates" $
    intraproceduralTransformation onStms
  where
    onStms scope stms =
      modifyNameSource . runState $
        runReaderT (parallelStms stms) scope
