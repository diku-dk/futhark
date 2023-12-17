{-# LANGUAGE TypeFamilies #-}

-- | Extraction of parallelism from a SOACs program.  This generates
-- parallel constructs aimed at CPU execution, which in particular may
-- involve ad-hoc irregular nested parallelism.
module Futhark.Pass.ExtractMulticore (extractMulticore) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Bitraversable
import Futhark.IR
import Futhark.IR.MC
import Futhark.IR.MC qualified as MC
import Futhark.IR.SOACS hiding
  ( Body,
    Exp,
    LParam,
    Lambda,
    Pat,
    Stm,
  )
import Futhark.IR.SOACS qualified as SOACS
import Futhark.Pass
import Futhark.Pass.ExtractKernels.DistributeNests
import Futhark.Pass.ExtractKernels.ToGPU (injectSOACS)
import Futhark.Tools
import Futhark.Transform.Rename (Rename, renameSomething)
import Futhark.Util.Log

newtype ExtractM a = ExtractM (ReaderT (Scope MC) (State VNameSource) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      HasScope MC,
      LocalScope MC,
      MonadFreshNames
    )

-- XXX: throwing away the log here...
instance MonadLogger ExtractM where
  addLog _ = pure ()

indexArray :: VName -> LParam SOACS -> VName -> Stm MC
indexArray i (Param _ p t) arr =
  Let (Pat [PatElem p t]) (defAux ()) . BasicOp $
    case t of
      Acc {} -> SubExp $ Var arr
      _ -> Index arr $ Slice $ DimFix (Var i) : map sliceDim (arrayDims t)

mapLambdaToBody ::
  (Body SOACS -> ExtractM (Body MC)) ->
  VName ->
  Lambda SOACS ->
  [VName] ->
  ExtractM (Body MC)
mapLambdaToBody onBody i lam arrs = do
  let indexings = zipWith (indexArray i) (lambdaParams lam) arrs
  Body () stms res <- inScopeOf indexings $ onBody $ lambdaBody lam
  pure $ Body () (stmsFromList indexings <> stms) res

mapLambdaToKernelBody ::
  (Body SOACS -> ExtractM (Body MC)) ->
  VName ->
  Lambda SOACS ->
  [VName] ->
  ExtractM (KernelBody MC)
mapLambdaToKernelBody onBody i lam arrs = do
  Body () stms res <- mapLambdaToBody onBody i lam arrs
  let ret (SubExpRes cs se) = Returns ResultMaySimplify cs se
  pure $ KernelBody () stms $ map ret res

reduceToSegBinOp :: Reduce SOACS -> ExtractM (Stms MC, SegBinOp MC)
reduceToSegBinOp (Reduce comm lam nes) = do
  ((lam', nes', shape), stms) <- runBuilder $ determineReduceOp lam nes
  lam'' <- transformLambda lam'
  let comm'
        | commutativeLambda lam' = Commutative
        | otherwise = comm
  pure (stms, SegBinOp comm' lam'' nes' shape)

scanToSegBinOp :: Scan SOACS -> ExtractM (Stms MC, SegBinOp MC)
scanToSegBinOp (Scan lam nes) = do
  ((lam', nes', shape), stms) <- runBuilder $ determineReduceOp lam nes
  lam'' <- transformLambda lam'
  pure (stms, SegBinOp Noncommutative lam'' nes' shape)

histToSegBinOp :: SOACS.HistOp SOACS -> ExtractM (Stms MC, MC.HistOp MC)
histToSegBinOp (SOACS.HistOp num_bins rf dests nes op) = do
  ((op', nes', shape), stms) <- runBuilder $ determineReduceOp op nes
  op'' <- transformLambda op'
  pure (stms, MC.HistOp num_bins rf dests nes' shape op'')

mkSegSpace :: (MonadFreshNames m) => SubExp -> m (VName, SegSpace)
mkSegSpace w = do
  flat <- newVName "flat_tid"
  gtid <- newVName "gtid"
  let space = SegSpace flat [(gtid, w)]
  pure (gtid, space)

transformStm :: Stm SOACS -> ExtractM (Stms MC)
transformStm (Let pat aux (BasicOp op)) =
  pure $ oneStm $ Let pat aux $ BasicOp op
transformStm (Let pat aux (Apply f args ret info)) =
  pure $ oneStm $ Let pat aux $ Apply f args ret info
transformStm (Let pat aux (Loop merge form body)) = do
  body' <-
    localScope (scopeOfFParams (map fst merge) <> scopeOfLoopForm form) $
      transformBody body
  pure $ oneStm $ Let pat aux $ Loop merge form body'
transformStm (Let pat aux (Match ses cases defbody ret)) =
  oneStm . Let pat aux
    <$> (Match ses <$> mapM transformCase cases <*> transformBody defbody <*> pure ret)
  where
    transformCase (Case vs body) = Case vs <$> transformBody body
transformStm (Let pat aux (WithAcc inputs lam)) =
  oneStm . Let pat aux
    <$> (WithAcc <$> mapM transformInput inputs <*> transformLambda lam)
  where
    transformInput (shape, arrs, op) =
      (shape,arrs,) <$> traverse (bitraverse transformLambda pure) op
transformStm (Let pat aux (Op op)) =
  fmap (certify (stmAuxCerts aux)) <$> transformSOAC pat (stmAuxAttrs aux) op

transformLambda :: Lambda SOACS -> ExtractM (Lambda MC)
transformLambda (Lambda params ret body) =
  Lambda params ret
    <$> localScope (scopeOfLParams params) (transformBody body)

transformStms :: Stms SOACS -> ExtractM (Stms MC)
transformStms stms =
  case stmsHead stms of
    Nothing -> pure mempty
    Just (stm, stms') -> do
      stm_stms <- transformStm stm
      inScopeOf stm_stms $ (stm_stms <>) <$> transformStms stms'

transformBody :: Body SOACS -> ExtractM (Body MC)
transformBody (Body () stms res) =
  Body () <$> transformStms stms <*> pure res

sequentialiseBody :: Body SOACS -> ExtractM (Body MC)
sequentialiseBody = pure . runIdentity . rephraseBody toMC
  where
    toMC = injectSOACS OtherOp

transformFunDef :: FunDef SOACS -> ExtractM (FunDef MC)
transformFunDef (FunDef entry attrs name rettype params body) = do
  body' <- localScope (scopeOfFParams params) $ transformBody body
  pure $ FunDef entry attrs name rettype params body'

-- Code generation for each parallel basic block is parameterised over
-- how we handle parallelism in the body (whether it's sequentialised
-- by keeping it as SOACs, or turned into SegOps).

data NeedsRename = DoRename | DoNotRename

renameIfNeeded :: (Rename a) => NeedsRename -> a -> ExtractM a
renameIfNeeded DoRename = renameSomething
renameIfNeeded DoNotRename = pure

transformMap ::
  NeedsRename ->
  (Body SOACS -> ExtractM (Body MC)) ->
  SubExp ->
  Lambda SOACS ->
  [VName] ->
  ExtractM (SegOp () MC)
transformMap rename onBody w map_lam arrs = do
  (gtid, space) <- mkSegSpace w
  kbody <- mapLambdaToKernelBody onBody gtid map_lam arrs
  renameIfNeeded rename $
    SegMap () space (lambdaReturnType map_lam) kbody

transformRedomap ::
  NeedsRename ->
  (Body SOACS -> ExtractM (Body MC)) ->
  SubExp ->
  [Reduce SOACS] ->
  Lambda SOACS ->
  [VName] ->
  ExtractM ([Stms MC], SegOp () MC)
transformRedomap rename onBody w reds map_lam arrs = do
  (gtid, space) <- mkSegSpace w
  kbody <- mapLambdaToKernelBody onBody gtid map_lam arrs
  (reds_stms, reds') <- mapAndUnzipM reduceToSegBinOp reds
  op' <-
    renameIfNeeded rename $
      SegRed () space reds' (lambdaReturnType map_lam) kbody
  pure (reds_stms, op')

transformHist ::
  NeedsRename ->
  (Body SOACS -> ExtractM (Body MC)) ->
  SubExp ->
  [SOACS.HistOp SOACS] ->
  Lambda SOACS ->
  [VName] ->
  ExtractM ([Stms MC], SegOp () MC)
transformHist rename onBody w hists map_lam arrs = do
  (gtid, space) <- mkSegSpace w
  kbody <- mapLambdaToKernelBody onBody gtid map_lam arrs
  (hists_stms, hists') <- mapAndUnzipM histToSegBinOp hists
  op' <-
    renameIfNeeded rename $
      SegHist () space hists' (lambdaReturnType map_lam) kbody
  pure (hists_stms, op')

transformSOAC :: Pat Type -> Attrs -> SOAC SOACS -> ExtractM (Stms MC)
transformSOAC _ _ JVP {} =
  error "transformSOAC: unhandled JVP"
transformSOAC _ _ VJP {} =
  error "transformSOAC: unhandled VJP"
transformSOAC pat _ (Screma w arrs form)
  | Just lam <- isMapSOAC form = do
      seq_op <- transformMap DoNotRename sequentialiseBody w lam arrs
      if lambdaContainsParallelism lam
        then do
          par_op <- transformMap DoRename transformBody w lam arrs
          pure $ oneStm (Let pat (defAux ()) $ Op $ ParOp (Just par_op) seq_op)
        else pure $ oneStm (Let pat (defAux ()) $ Op $ ParOp Nothing seq_op)
  | Just (reds, map_lam) <- isRedomapSOAC form = do
      (seq_reds_stms, seq_op) <-
        transformRedomap DoNotRename sequentialiseBody w reds map_lam arrs
      if lambdaContainsParallelism map_lam
        then do
          (par_reds_stms, par_op) <-
            transformRedomap DoRename transformBody w reds map_lam arrs
          pure $
            mconcat (seq_reds_stms <> par_reds_stms)
              <> oneStm (Let pat (defAux ()) $ Op $ ParOp (Just par_op) seq_op)
        else
          pure $
            mconcat seq_reds_stms
              <> oneStm (Let pat (defAux ()) $ Op $ ParOp Nothing seq_op)
  | Just (scans, map_lam) <- isScanomapSOAC form = do
      (gtid, space) <- mkSegSpace w
      kbody <- mapLambdaToKernelBody transformBody gtid map_lam arrs
      (scans_stms, scans') <- mapAndUnzipM scanToSegBinOp scans
      pure $
        mconcat scans_stms
          <> oneStm
            ( Let pat (defAux ()) $
                Op $
                  ParOp Nothing $
                    SegScan () space scans' (lambdaReturnType map_lam) kbody
            )
  | otherwise = do
      -- This screma is too complicated for us to immediately do
      -- anything, so split it up and try again.
      scope <- castScope <$> askScope
      transformStms =<< runBuilderT_ (dissectScrema pat w form arrs) scope
transformSOAC pat _ (Scatter w ivs lam dests) = do
  (gtid, space) <- mkSegSpace w

  Body () kstms res <- mapLambdaToBody transformBody gtid lam ivs

  (rets, kres) <- fmap unzip $ forM (groupScatterResults dests res) $ \(_a_w, a, is_vs) -> do
    a_t <- lookupType a
    let cs =
          foldMap (foldMap resCerts . fst) is_vs
            <> foldMap (resCerts . snd) is_vs
        is_vs' = [(fullSlice a_t $ map (DimFix . resSubExp) is, resSubExp v) | (is, v) <- is_vs]
    pure (a_t, WriteReturns cs a is_vs')
  pure . oneStm . Let pat (defAux ()) . Op . ParOp Nothing $
    SegMap () space rets (KernelBody () kstms kres)
transformSOAC pat _ (Hist w arrs hists map_lam) = do
  (seq_hist_stms, seq_op) <-
    transformHist DoNotRename sequentialiseBody w hists map_lam arrs

  if lambdaContainsParallelism map_lam
    then do
      (par_hist_stms, par_op) <-
        transformHist DoRename transformBody w hists map_lam arrs
      pure $
        mconcat (seq_hist_stms <> par_hist_stms)
          <> oneStm (Let pat (defAux ()) $ Op $ ParOp (Just par_op) seq_op)
    else
      pure $
        mconcat seq_hist_stms
          <> oneStm (Let pat (defAux ()) $ Op $ ParOp Nothing seq_op)
transformSOAC pat _ (Stream w arrs nes lam) = do
  -- Just remove the stream and transform the resulting stms.
  soacs_scope <- castScope <$> askScope
  stream_stms <-
    flip runBuilderT_ soacs_scope $
      sequentialStreamWholeArray pat w nes lam arrs
  transformStms stream_stms

transformProg :: Prog SOACS -> PassM (Prog MC)
transformProg prog =
  modifyNameSource $ runState (runReaderT m mempty)
  where
    ExtractM m = do
      consts' <- transformStms $ progConsts prog
      funs' <- inScopeOf consts' $ mapM transformFunDef $ progFuns prog
      pure $
        prog
          { progConsts = consts',
            progFuns = funs'
          }

-- | Transform a program using SOACs to a program in the 'MC'
-- representation, using some amount of flattening.
extractMulticore :: Pass SOACS MC
extractMulticore =
  Pass
    { passName = "extract multicore parallelism",
      passDescription = "Extract multicore parallelism",
      passFunction = transformProg
    }
