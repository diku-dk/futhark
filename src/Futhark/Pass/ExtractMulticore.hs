{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | Extraction of parallelism from a SOACs program.  This generates
-- parallel constructs aimed at CPU execution, which in particular may
-- involve ad-hoc irregular nested parallelism.
module Futhark.Pass.ExtractMulticore (extractMulticore) where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Bitraversable
import Futhark.Analysis.Rephrase
import Futhark.IR
import Futhark.IR.MC
import qualified Futhark.IR.MC as MC
import Futhark.IR.SOACS hiding
  ( Body,
    Exp,
    LParam,
    Lambda,
    Pat,
    Stm,
  )
import qualified Futhark.IR.SOACS as SOACS
import qualified Futhark.IR.SOACS.Simplify as SOACS
import Futhark.Pass
import Futhark.Pass.ExtractKernels.DistributeNests
import Futhark.Pass.ExtractKernels.ToGPU (injectSOACS)
import Futhark.Tools
import qualified Futhark.Transform.FirstOrderTransform as FOT
import Futhark.Transform.Rename (Rename, renameSomething)
import Futhark.Util (takeLast)
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

mkSegSpace :: MonadFreshNames m => SubExp -> m (VName, SegSpace)
mkSegSpace w = do
  flat <- newVName "flat_tid"
  gtid <- newVName "gtid"
  let space = SegSpace flat [(gtid, w)]
  pure (gtid, space)

transformLoopForm :: LoopForm SOACS -> LoopForm MC
transformLoopForm (WhileLoop cond) = WhileLoop cond
transformLoopForm (ForLoop i it bound params) = ForLoop i it bound params

transformStm :: Stm SOACS -> ExtractM (Stms MC)
transformStm (Let pat aux (BasicOp op)) =
  pure $ oneStm $ Let pat aux $ BasicOp op
transformStm (Let pat aux (Apply f args ret info)) =
  pure $ oneStm $ Let pat aux $ Apply f args ret info
transformStm (Let pat aux (DoLoop merge form body)) = do
  let form' = transformLoopForm form
  body' <-
    localScope (scopeOfFParams (map fst merge) <> scopeOf form') $
      transformBody body
  pure $ oneStm $ Let pat aux $ DoLoop merge form' body'
transformStm (Let pat aux (If cond tbranch fbranch ret)) =
  oneStm . Let pat aux
    <$> (If cond <$> transformBody tbranch <*> transformBody fbranch <*> pure ret)
transformStm (Let pat aux (WithAcc inputs lam)) =
  oneStm . Let pat aux
    <$> (WithAcc <$> mapM transformInput inputs <*> transformLambda lam)
  where
    transformInput (shape, arrs, op) =
      (shape,arrs,) <$> traverse (bitraverse transformLambda pure) op
transformStm (Let pat aux (Op op)) =
  fmap (certify (stmAuxCerts aux)) <$> transformSOAC pat (stmAuxAttrs aux) op

transformLambda :: Lambda SOACS -> ExtractM (Lambda MC)
transformLambda (Lambda params body ret) =
  Lambda params
    <$> localScope (scopeOfLParams params) (transformBody body)
    <*> pure ret

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

-- Sets the chunk size to one.
unstreamLambda :: Attrs -> [SubExp] -> Lambda SOACS -> ExtractM (Lambda SOACS)
unstreamLambda attrs nes lam = do
  let (chunk_param, acc_params, slice_params) =
        partitionChunkedFoldParameters (length nes) (lambdaParams lam)

  inp_params <- forM slice_params $ \(Param _ p t) ->
    newParam (baseString p) (rowType t)

  body <- runBodyBuilder $
    localScope (scopeOfLParams inp_params) $ do
      letBindNames [paramName chunk_param] $
        BasicOp $ SubExp $ intConst Int64 1

      forM_ (zip acc_params nes) $ \(p, ne) ->
        letBindNames [paramName p] $ BasicOp $ SubExp ne

      forM_ (zip slice_params inp_params) $ \(slice, v) ->
        letBindNames [paramName slice] $
          BasicOp $ ArrayLit [Var $ paramName v] (paramType v)

      (red_res, map_res) <- splitAt (length nes) <$> bodyBind (lambdaBody lam)

      map_res' <- forM map_res $ \(SubExpRes cs se) -> do
        v <- letExp "map_res" $ BasicOp $ SubExp se
        v_t <- lookupType v
        certifying cs . letSubExp "chunk" . BasicOp $
          Index v $ fullSlice v_t [DimFix $ intConst Int64 0]

      pure $ mkBody mempty $ red_res <> subExpsRes map_res'

  let (red_ts, map_ts) = splitAt (length nes) $ lambdaReturnType lam
      map_lam =
        Lambda
          { lambdaReturnType = red_ts ++ map rowType map_ts,
            lambdaParams = inp_params,
            lambdaBody = body
          }

  soacs_scope <- castScope <$> askScope
  map_lam' <- runReaderT (SOACS.simplifyLambda map_lam) soacs_scope

  if "sequential_inner" `inAttrs` attrs
    then FOT.transformLambda map_lam'
    else pure map_lam'

-- Code generation for each parallel basic block is parameterised over
-- how we handle parallelism in the body (whether it's sequentialised
-- by keeping it as SOACs, or turned into SegOps).

data NeedsRename = DoRename | DoNotRename

renameIfNeeded :: Rename a => NeedsRename -> a -> ExtractM a
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
  (reds_stms, reds') <- unzip <$> mapM reduceToSegBinOp reds
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
  (hists_stms, hists') <- unzip <$> mapM histToSegBinOp hists
  op' <-
    renameIfNeeded rename $
      SegHist () space hists' (lambdaReturnType map_lam) kbody
  pure (hists_stms, op')

transformParStream ::
  NeedsRename ->
  (Body SOACS -> ExtractM (Body MC)) ->
  SubExp ->
  Commutativity ->
  Lambda SOACS ->
  [SubExp] ->
  Lambda SOACS ->
  [VName] ->
  ExtractM (Stms MC, SegOp () MC)
transformParStream rename onBody w comm red_lam red_nes map_lam arrs = do
  (gtid, space) <- mkSegSpace w
  kbody <- mapLambdaToKernelBody onBody gtid map_lam arrs
  (red_stms, red) <- reduceToSegBinOp $ Reduce comm red_lam red_nes
  op <-
    renameIfNeeded rename $
      SegRed () space [red] (lambdaReturnType map_lam) kbody
  pure (red_stms, op)

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
      (scans_stms, scans') <- unzip <$> mapM scanToSegBinOp scans
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

  let rets = takeLast (length dests) $ lambdaReturnType lam
      kres = do
        (a_w, a, is_vs) <- groupScatterResults dests res
        let cs =
              foldMap (foldMap resCerts . fst) is_vs
                <> foldMap (resCerts . snd) is_vs
            is_vs' = [(Slice $ map (DimFix . resSubExp) is, resSubExp v) | (is, v) <- is_vs]
        pure $ WriteReturns cs a_w a is_vs'
      kbody = KernelBody () kstms kres
  pure $
    oneStm $
      Let pat (defAux ()) $
        Op $
          ParOp Nothing $
            SegMap () space rets kbody
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
transformSOAC pat attrs (Stream w arrs (Parallel _ comm red_lam) red_nes fold_lam)
  | not $ null red_nes = do
      map_lam <- unstreamLambda attrs red_nes fold_lam
      (seq_red_stms, seq_op) <-
        transformParStream
          DoNotRename
          sequentialiseBody
          w
          comm
          red_lam
          red_nes
          map_lam
          arrs

      if lambdaContainsParallelism map_lam
        then do
          (par_red_stms, par_op) <-
            transformParStream DoRename transformBody w comm red_lam red_nes map_lam arrs
          pure $
            seq_red_stms <> par_red_stms
              <> oneStm (Let pat (defAux ()) $ Op $ ParOp (Just par_op) seq_op)
        else
          pure $
            seq_red_stms
              <> oneStm (Let pat (defAux ()) $ Op $ ParOp Nothing seq_op)
transformSOAC pat _ (Stream w arrs _ nes lam) = do
  -- Just remove the stream and transform the resulting stms.
  soacs_scope <- castScope <$> askScope
  stream_stms <-
    flip runBuilderT_ soacs_scope $
      sequentialStreamWholeArray pat w nes lam arrs
  transformStms stream_stms

transformProg :: Prog SOACS -> PassM (Prog MC)
transformProg (Prog consts funs) =
  modifyNameSource $ runState (runReaderT m mempty)
  where
    ExtractM m = do
      consts' <- transformStms consts
      funs' <- inScopeOf consts' $ mapM transformFunDef funs
      pure $ Prog consts' funs'

-- | Transform a program using SOACs to a program in the 'MC'
-- representation, using some amount of flattening.
extractMulticore :: Pass SOACS MC
extractMulticore =
  Pass
    { passName = "extract multicore parallelism",
      passDescription = "Extract multicore parallelism",
      passFunction = transformProg
    }
