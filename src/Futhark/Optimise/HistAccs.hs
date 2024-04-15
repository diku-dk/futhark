{-# LANGUAGE TypeFamilies #-}

-- | Turn certain uses of accumulators into SegHists.
module Futhark.Optimise.HistAccs (histAccsGPU) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Map.Strict qualified as M
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

optimiseExp :: Accs GPU -> Exp GPU -> OptM (Exp GPU)
optimiseExp accs = mapExpM mapper
  where
    mapper =
      identityMapper
        { mapOnBody = \scope body -> localScope scope $ optimiseBody accs body
        }

extractUpdate ::
  Accs rep ->
  VName ->
  Stms rep ->
  Maybe ((WithAccInput rep, VName, [SubExp], [SubExp]), Stms rep)
extractUpdate accs v stms = do
  (stm, stms') <- stmsHead stms
  case stm of
    Let (Pat [PatElem pe_v _]) _ (BasicOp (UpdateAcc _ acc is vs))
      | pe_v == v -> do
          acc_input <- M.lookup acc accs
          Just ((acc_input, acc, is, vs), stms')
    _ -> do
      (x, stms'') <- extractUpdate accs v stms'
      pure (x, oneStm stm <> stms'')

mkHistBody :: Accs GPU -> KernelBody GPU -> Maybe (KernelBody GPU, WithAccInput GPU, VName)
mkHistBody accs (KernelBody () stms [Returns rm cs (Var v)]) = do
  ((acc_input, acc, is, vs), stms') <- extractUpdate accs v stms
  pure
    ( KernelBody () stms' $ map (Returns rm cs) is ++ map (Returns rm cs) vs,
      acc_input,
      acc
    )
mkHistBody _ _ = Nothing

withAccLamToHistLam :: (MonadFreshNames m) => Shape -> Lambda GPU -> m (Lambda GPU)
withAccLamToHistLam shape lam =
  renameLambda $ lam {lambdaParams = drop (shapeRank shape) (lambdaParams lam)}

addArrsToAcc ::
  (MonadBuilder m, Rep m ~ GPU) =>
  SegLevel ->
  Shape ->
  [VName] ->
  VName ->
  m (Exp GPU)
addArrsToAcc lvl shape arrs acc = do
  flat <- newVName "phys_tid"
  gtids <- replicateM (shapeRank shape) (newVName "gtid")
  let space = SegSpace flat $ zip gtids $ shapeDims shape

  (acc', stms) <- localScope (scopeOfSegSpace space) . collectStms $ do
    vs <- forM arrs $ \arr -> do
      arr_t <- lookupType arr
      letSubExp (baseString arr <> "_elem") $
        BasicOp $
          Index arr $
            fullSlice arr_t $
              map (DimFix . Var) gtids
    letExp (baseString acc <> "_upd") $
      BasicOp $
        UpdateAcc Safe acc (map Var gtids) vs

  acc_t <- lookupType acc
  pure . Op . SegOp . SegMap lvl space [acc_t] $
    KernelBody () stms [Returns ResultMaySimplify mempty (Var acc')]

flatKernelBody ::
  (MonadBuilder m) =>
  SegSpace ->
  KernelBody (Rep m) ->
  m (SegSpace, KernelBody (Rep m))
flatKernelBody space kbody = do
  gtid <- newVName "gtid"
  dims_prod <-
    letSubExp "dims_prod"
      =<< foldBinOp (Mul Int64 OverflowUndef) (intConst Int64 1) (segSpaceDims space)

  let space' = SegSpace (segFlat space) [(gtid, dims_prod)]

  kbody_stms <- localScope (scopeOfSegSpace space') . collectStms_ $ do
    let new_inds =
          unflattenIndex (map pe64 (segSpaceDims space)) (pe64 $ Var gtid)
    zipWithM_ letBindNames (map (pure . fst) (unSegSpace space))
      =<< mapM toExp new_inds
    addStms $ kernelBodyStms kbody

  pure (space', kbody {kernelBodyStms = kbody_stms})

optimiseStm :: Accs GPU -> Stm GPU -> OptM (Stms GPU)
-- TODO: this is very restricted currently, but shows the idea.
optimiseStm accs (Let pat aux (WithAcc inputs lam)) = do
  localScope (scopeOfLParams (lambdaParams lam)) $ do
    body' <- optimiseBody accs' $ lambdaBody lam
    let lam' = lam {lambdaBody = body'}
    pure $ oneStm $ Let pat aux $ WithAcc inputs lam'
  where
    acc_names = map paramName $ drop (length inputs) $ lambdaParams lam
    accs' = M.fromList (zip acc_names inputs) <> accs
optimiseStm accs (Let pat aux (Op (SegOp (SegMap lvl space _ kbody))))
  | accs /= mempty,
    Just (kbody', (acc_shape, _, Just (acc_lam, acc_nes)), acc) <-
      mkHistBody accs kbody,
    all primType $ lambdaReturnType acc_lam = runBuilder_ $ do
      hist_dests <- forM acc_nes $ \ne ->
        letExp "hist_dest" $ BasicOp $ Replicate acc_shape ne

      acc_lam' <- withAccLamToHistLam acc_shape acc_lam

      let ts' =
            replicate (shapeRank acc_shape) (Prim int64)
              ++ lambdaReturnType acc_lam
          histop =
            HistOp
              { histShape = acc_shape,
                histRaceFactor = intConst Int64 1,
                histDest = hist_dests,
                histNeutral = acc_nes,
                histOpShape = mempty,
                histOp = acc_lam'
              }

      (space', kbody'') <- flatKernelBody space kbody'

      hist_dest_upd <-
        letTupExp "hist_dest_upd" $ Op $ SegOp $ SegHist lvl space' [histop] ts' kbody''

      addStm . Let pat aux =<< addArrsToAcc lvl acc_shape hist_dest_upd acc
optimiseStm accs (Let pat aux e) =
  oneStm . Let pat aux <$> optimiseExp accs e

optimiseStms :: Accs GPU -> Stms GPU -> OptM (Stms GPU)
optimiseStms accs stms =
  localScope (scopeOf stms) $
    mconcat <$> mapM (optimiseStm accs) (stmsToList stms)

-- | The pass for GPU kernels.
histAccsGPU :: Pass GPU GPU
histAccsGPU =
  Pass "hist accs" "Turn certain accumulations into histograms" $
    intraproceduralTransformation onStms
  where
    onStms scope stms =
      modifyNameSource . runState $
        runReaderT (optimiseStms mempty stms) scope
