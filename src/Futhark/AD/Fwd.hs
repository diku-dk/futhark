{-# LANGUAGE TypeFamilies #-}

module Futhark.AD.Fwd (fwdJVP) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifunctor (bimap, second)
import Data.Foldable
import Data.Functor.Product
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as M
import Data.Tuple (Solo (..), getSolo)
import Futhark.AD.Derivatives
import Futhark.AD.Shared
import Futhark.Analysis.PrimExp.Convert
import Futhark.Builder
import Futhark.IR.SOACS
import Futhark.Tools
import Futhark.Transform.Rename (renameLambda)
import Futhark.Util (interleave, splitAt3, unterleave)

zeroExp :: Type -> Exp SOACS
zeroExp (Prim pt) =
  BasicOp $ SubExp $ Constant $ blankPrimValue pt
zeroExp (Array pt shape _) =
  BasicOp $ Replicate shape $ Constant $ blankPrimValue pt
zeroExp t = error $ "zeroExp: " ++ show t

tanType :: (ArrayShape s, Monoid u) => TypeBase s u -> ADM (TypeBase s u)
tanType (Acc acc ispace ts u) = do
  acc_tan <- tangent acc
  tan_shape <- askShape
  pure $ Acc acc_tan (tan_shape <> ispace) ts u
tanType t = do
  shape <- askShape
  pure $ arrayOf (Prim (elemType t)) (shape `prependShape` arrayShape t) u
  where
    u = case t of
      Array _ _ u' -> u'
      _ -> mempty

slocal' :: ADM a -> ADM a
slocal' = slocal id

slocal :: (RState -> RState) -> ADM a -> ADM a
slocal f m = do
  s <- get
  modify f
  a <- m
  modify $ \s' -> s' {stateTans = stateTans s}
  pure a

data RState = RState
  { stateTans :: M.Map VName VName,
    stateNameSource :: VNameSource
  }

data FEnv = FEnv
  { envTanShape :: Shape,
    envAttrs :: Attrs
  }

newtype ADM a = ADM (BuilderT SOACS (ReaderT FEnv (State RState)) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState RState,
      MonadReader FEnv,
      MonadFreshNames,
      HasScope SOACS,
      LocalScope SOACS
    )

instance MonadBuilder ADM where
  type Rep ADM = SOACS
  mkExpDecM pat e = ADM $ mkExpDecM pat e
  mkBodyM bnds res = ADM $ mkBodyM bnds res
  mkLetNamesM pat e = ADM $ mkLetNamesM pat e

  addStms = ADM . addStms
  collectStms (ADM m) = ADM $ collectStms m

instance MonadFreshNames (State RState) where
  getNameSource = gets stateNameSource
  putNameSource src = modify (\env -> env {stateNameSource = src})

askShape :: ADM Shape
askShape = ADM $ lift $ asks envTanShape

runADM :: (MonadFreshNames m) => Shape -> Attrs -> ADM a -> m a
runADM shape attrs (ADM m) =
  modifyNameSource $ \vn ->
    second stateNameSource $
      runState
        ( runReaderT
            (fst <$> runBuilderT m mempty)
            (FEnv shape attrs)
        )
        (RState mempty vn)

tanVName :: VName -> ADM VName
tanVName v = newVName (baseName v <> "_tan")

insertTan :: VName -> VName -> ADM ()
insertTan v v' =
  modify $ \env -> env {stateTans = M.insert v v' (stateTans env)}

class TanBuilder a where
  newTan :: a -> ADM a
  bundleNew :: a -> ADM (a, a)

bundleNewList :: (TanBuilder a) => [a] -> ADM [a]
bundleNewList = fmap (uncurry interleave . unzip) . mapM bundleNew

instance (ArrayShape s, Monoid u) => TanBuilder (PatElem (TypeBase s u)) where
  newTan (PatElem p t) = do
    p' <- tanVName p
    insertTan p p'
    t' <- tanType t
    pure $ PatElem p' t'
  bundleNew pe = do
    pe' <- newTan pe
    pure (pe, pe')

newTanPat :: (TanBuilder (PatElem t)) => Pat t -> ADM (Pat t)
newTanPat (Pat pes) = Pat <$> mapM newTan pes

bundleNewPat :: (TanBuilder (PatElem t)) => Pat t -> ADM (Pat t)
bundleNewPat (Pat pes) = Pat <$> bundleNewList pes

instance (ArrayShape s, Monoid u) => TanBuilder (Param (TypeBase s u)) where
  newTan (Param _ p t) = do
    PatElem p' t' <- newTan $ PatElem p t
    pure $ Param mempty p' t'
  bundleNew param = do
    param' <- newTan param
    pure (param, param')

instance (TanBuilder a, Tangent b) => TanBuilder (a, b) where
  newTan (p, x) = (,) <$> newTan p <*> tangent x
  bundleNew (p, x) = do
    p' <- newTan p
    x_tan <- tangent x
    pure ((p, x), (p', x_tan))

class Tangent a where
  tangent :: a -> ADM a
  bundleTan :: a -> ADM (a, a)

instance (ArrayShape s, Monoid u) => Tangent (TypeBase s u) where
  tangent = tanType
  bundleTan t = do
    t' <- tangent t
    pure (t, t')

bundleTangents :: (Tangent a) => [a] -> ADM [a]
bundleTangents = fmap (uncurry interleave . unzip) . mapM bundleTan

instance Tangent VName where
  tangent v = do
    maybeTan <- gets $ M.lookup v . stateTans
    case maybeTan of
      Just v_tan -> pure v_tan
      Nothing -> do
        t <- lookupType v
        when (isAcc t) $
          error $
            "Missing tangent for accumulator " <> prettyString v
        tan_shape <- askShape
        letExp (baseName v <> "_implicit_tan") $ zeroExp $ t `arrayOfShape` tan_shape
  bundleTan v = do
    v_tan <- tangent v
    pure (v, v_tan)

instance Tangent SubExp where
  tangent (Constant c) = do
    tan_shape <- askShape
    if tan_shape == mempty
      then pure $ constant $ blankPrimValue pt
      else letSubExp "const_implicit_tan" $ zeroExp $ Prim pt `arrayOfShape` tan_shape
    where
      pt = primValueType c
  tangent (Var v) = Var <$> tangent v
  bundleTan c@Constant {} = do
    c_tan <- tangent c
    pure (c, c_tan)
  bundleTan (Var v) = bimap Var Var <$> bundleTan v

instance Tangent SubExpRes where
  tangent (SubExpRes cs se) = SubExpRes cs <$> tangent se
  bundleTan (SubExpRes cs se) = bimap (SubExpRes cs) (SubExpRes cs) <$> bundleTan se

withTan ::
  SubExp ->
  (SubExp -> ADM (Exp SOACS)) ->
  ADM (Exp SOACS)
withTan x f = do
  shape <- askShape
  x_tan <- tangent x
  mapNest shape (MkSolo x_tan) (f . getSolo)

withTansI ::
  VName ->
  [SubExp] ->
  ([SubExp] -> VName -> [SubExp] -> ADM (Exp SOACS)) ->
  ADM (Exp SOACS)
withTansI x ys f = do
  shape <- askShape
  x_tan <- tangent x
  ys_tan <- mapM tangent ys
  if shape == mempty
    then f [] x_tan ys_tan
    else do
      let w = shapeSize 0 shape
      ys_tan_vs <- mapM asVName ys_tan
      iota_p <- newParam "iota_p" $ Prim int64
      x_tan_p <- newParam "x_tanp" . rowType =<< lookupType x_tan
      ys_tan_ps <- mapM (newParam "y_tanp" . rowType <=< lookupType) ys_tan_vs
      lam <- mkLambda (iota_p : x_tan_p : ys_tan_ps) $ do
        fmap (subExpsRes . pure) . letSubExp "tan"
          =<< f
            [Var $ paramName iota_p]
            (paramName x_tan_p)
            (map (Var . paramName) ys_tan_ps)
      iota_v <- letExp "iota" $ iota64 w
      Op . Screma w (iota_v : x_tan : ys_tan_vs) <$> mapSOAC lam

withTans ::
  PrimType ->
  SubExp ->
  SubExp ->
  (PrimExp VName -> PrimExp VName -> PrimExp VName) ->
  ADM (Exp SOACS)
withTans t x y f = do
  shape <- askShape
  x_tan <- tangent x
  y_tan <- tangent y
  mapNest shape (Pair (Identity x_tan) (Identity y_tan)) $ \xy -> do
    Pair (Identity x_tan_v) (Identity y_tan_v) <- traverse asVName xy
    toExp $ f (LeafExp x_tan_v t) (LeafExp y_tan_v t)

withAnyTans ::
  (Traversable f) =>
  f SubExp ->
  ([PrimExp VName] -> PrimExp VName) ->
  ADM (Exp SOACS)
withAnyTans xs f = do
  shape <- askShape
  xs_tan <- traverse tangent xs
  mapNest shape xs_tan $ \xs_tan' -> do
    xs_tan'' <- forM xs_tan' $ \se -> do
      ~(Prim t) <- subExpType se
      pure $ primExpFromSubExp t se
    toExp $ f $ toList xs_tan''

bindTanPat :: Pat Type -> StmAux () -> Exp SOACS -> ADM ()
bindTanPat pat_tan aux e = do
  attrs <- asks envAttrs
  auxing aux . attributing attrs . letBind pat_tan $ e

bindTan ::
  Pat Type ->
  StmAux () ->
  SubExp ->
  (SubExp -> ADM (Exp SOACS)) ->
  ADM ()
bindTan pat_tan aux x f = do
  bindTanPat pat_tan aux =<< withTan x f

bindTans ::
  Pat Type ->
  StmAux () ->
  PrimType ->
  SubExp ->
  SubExp ->
  (PrimExp VName -> PrimExp VName -> PrimExp VName) ->
  ADM ()
bindTans pat_tan aux t x y f = do
  bindTanPat pat_tan aux =<< withTans t x y f

basicFwd :: Pat Type -> StmAux () -> BasicOp -> ADM ()
basicFwd pat aux op = do
  pat_tan <- newTanPat pat
  case op of
    SubExp se -> do
      se_tan <- tangent se
      addStm $ Let pat_tan aux $ BasicOp $ SubExp se_tan
    Opaque opaqueop se -> do
      se_tan <- tangent se
      addStm $ Let pat_tan aux $ BasicOp $ Opaque opaqueop se_tan
    ArrayLit ses t -> do
      tan_shape <- askShape
      ses_tan <- mapM tangent ses
      if tan_shape == mempty
        then
          addStm $ Let pat_tan aux $ BasicOp $ ArrayLit ses_tan t
        else do
          pat_tan_tr <- letExp "pat_tan_tr" $ BasicOp $ ArrayLit ses_tan $ t `arrayOfShape` tan_shape
          pat_tan_tr_t <- lookupType pat_tan_tr
          let perm = vecPerm tan_shape pat_tan_tr_t
          addStm $ Let pat_tan aux $ BasicOp $ Rearrange pat_tan_tr perm
    UnOp unop x -> do
      let t = unOpType unop
          x_pe = primExpFromSubExp t x
          dx = pdUnOp unop x_pe
      bindTan pat_tan aux x $ \x_tan ->
        toExp $ primExpFromSubExp t x_tan ~*~ dx
    BinOp bop x y -> do
      let t = binOpType bop
      bindTans pat_tan aux t x y $ \x_tan y_tan ->
        let (wrt_x, wrt_y) =
              pdBinOp bop (primExpFromSubExp t x) (primExpFromSubExp t y)
         in x_tan ~*~ wrt_x ~+~ y_tan ~*~ wrt_y
    CmpOp {} -> do
      tan_shape <- askShape
      addStm $ Let pat_tan aux $ zeroExp $ Prim Bool `arrayOfShape` tan_shape
    ConvOp cop x ->
      bindTan pat_tan aux x $ \x_tan ->
        pure $ BasicOp $ ConvOp cop x_tan
    Assert {} -> pure ()
    Index arr slice -> do
      dims <- shapeDims <$> askShape
      arr_tan <- tangent arr
      let slice' = Slice $ map sliceDim dims <> unSlice slice
      addStm $ Let pat_tan aux $ BasicOp $ Index arr_tan slice'
    Update safety arr slice se -> do
      dims <- shapeDims <$> askShape
      arr_tan <- tangent arr
      se_tan <- tangent se
      let slice' = Slice $ map sliceDim dims <> unSlice slice
      addStm $ Let pat_tan aux $ BasicOp $ Update safety arr_tan slice' se_tan
    Concat d (arr :| arrs) w -> do
      r <- shapeRank <$> askShape
      arr_tan <- tangent arr
      arrs_tans <- mapM tangent arrs
      addStm $ Let pat_tan aux $ BasicOp $ Concat (d + r) (arr_tan :| arrs_tans) w
    Manifest arr ds -> do
      r <- shapeRank <$> askShape
      arr_tan <- tangent arr
      addStm . Let pat_tan aux . BasicOp $
        Manifest arr_tan ([0 .. r - 1] ++ map (+ r) ds)
    Iota n _ _ it -> do
      shape <- askShape
      addStm . Let pat_tan aux . BasicOp $
        Replicate (shape <> Shape [n]) (intConst it 0)
    Replicate n x ->
      bindTan pat_tan aux x $ \x_tan ->
        pure $ BasicOp $ Replicate n x_tan
    Scratch t shape -> do
      tan_shape <- askShape
      addStm $ Let pat_tan aux $ BasicOp $ Scratch t $ shapeDims tan_shape <> shape
    Reshape arr reshape -> do
      shape <- askShape
      arr_tan <- tangent arr
      addStm $ Let pat_tan aux $ BasicOp $ Reshape arr_tan (newshapeInner shape reshape)
    Rearrange arr perm -> do
      r <- shapeRank <$> askShape
      arr_tan <- tangent arr
      addStm . Let pat_tan aux . BasicOp $
        Rearrange arr_tan ([0 .. r - 1] <> map (+ r) perm)
    _ -> error $ "basicFwd: Unsupported op " ++ prettyString op

fwdLambda :: Lambda SOACS -> ADM (Lambda SOACS)
fwdLambda (Lambda params _ body) = do
  params' <- bundleNewList params
  mkLambda params' $ bodyBind =<< fwdBody body

fwdWithAccLambda :: [WithAccInput SOACS] -> Lambda SOACS -> ADM (Lambda SOACS)
fwdWithAccLambda inputs (Lambda params _ body) = do
  let (cert_params, acc_params) = splitAt (length inputs) params
  cert_params_tan <- replicateM (length inputs) $ newParam "acc_cert_tan" $ Prim Unit
  acc_params_tan <- zipWithM mkAccParam (map paramName cert_params_tan) inputs

  mkLambda (cert_params <> cert_params_tan <> acc_params <> acc_params_tan) $ do
    zipWithM_
      insertTan
      (map paramName (cert_params <> acc_params))
      (map paramName (cert_params_tan <> acc_params_tan))
    bodyBind =<< fwdBody body
  where
    mkAccParam c (shape, arrs, _) = do
      tan_shape <- askShape
      ts <- map (stripArray (shapeRank shape)) <$> mapM lookupType arrs
      newParam "acc_p_tan" $ Acc c (tan_shape <> shape) ts NoUniqueness

fwdStreamLambda :: Int -> Lambda SOACS -> ADM (Lambda SOACS)
fwdStreamLambda num_accs (Lambda params _ body) = do
  tan_shape <- askShape
  let (chunk_params, acc_params, arr_params) = splitAt3 1 num_accs params
  acc_params' <- bundleNewList acc_params
  (arr_params', arr_params'_tan) <- mapAndUnzipM onArrParam arr_params
  let params' =
        chunk_params <> acc_params' <> interleave arr_params' arr_params'_tan
  mkLambda params' $ do
    zipWithM_ (trArrParamTan tan_shape) arr_params' arr_params'_tan
    (acc_res, map_res) <- fmap (splitAt (num_accs * 2)) . bodyBind =<< fwdBody body
    let (map_res_primal, map_res_tan) = unterleave map_res
    map_res_tan' <- mapM (trMapResTan tan_shape) map_res_tan
    pure $ acc_res <> interleave map_res_primal map_res_tan'
  where
    -- Array parameters need to be treated specially as the chunk parameter
    -- must always be outermost.
    onArrParam p = do
      shape <- askShape
      (p', p_tan) <- bundleNew p
      let perm = auxPerm shape $ paramType p_tan
      pure (p', p_tan {paramDec = rearrangeType perm (paramType p_tan)})

    -- Put the tangent shape back in the outermost position.
    trArrParamTan tan_shape p p_tan = do
      let perm = rearrangeInverse $ auxPerm tan_shape $ paramType p_tan
      v <-
        letExp (baseName (paramName p_tan)) . BasicOp $
          Rearrange (paramName p_tan) perm
      insertTan (paramName p) v

    -- Put the chunk size back in the outermost position.
    trMapResTan tan_shape (SubExpRes cs ~(Var v)) = do
      v_t <- lookupType v
      let perm = auxPerm tan_shape v_t
      fmap varRes . certifying cs $ letExp (baseName v) . BasicOp $ Rearrange v perm

vecPerm :: Shape -> Type -> [Int]
vecPerm = auxPerm

pushTanShape :: VName -> ADM VName
pushTanShape v = do
  tan_shape <- askShape
  v_t <- lookupType v
  if tan_shape == mempty || arrayShape v_t == tan_shape || isAcc v_t
    then pure v
    else do
      let perm = vecPerm tan_shape v_t
      letExp (baseName v <> "_tr") $ BasicOp $ Rearrange v perm

soacInputsWithTangents :: [VName] -> ADM [VName]
soacInputsWithTangents xs = do
  xs_tans <- mapM (pushTanShape <=< tangent) xs
  pure $ interleave xs xs_tans

soacResPat :: Int -> Int -> Pat Type -> ADM (Pat Type, [(Pat Type, VName)])
soacResPat scan_res red_res (Pat pes) = do
  pes_tan <- mapM newTan pes
  bimap (Pat . interleave pes) mconcat . unzip <$> zipWithM tweakPatElem [0 ..] pes_tan
  where
    isRedRes i = i >= scan_res && i < scan_res + red_res
    tweakPatElem i pe@(PatElem v v_t) = do
      tan_shape <- askShape
      if isRedRes i || tan_shape == mempty || arrayShape v_t == tan_shape || isAcc v_t
        then pure (pe, [])
        else do
          let perm = vecPerm tan_shape v_t
          v' <- newName v
          pure (PatElem v' $ rearrangeType perm v_t, [(Pat [pe], v')])

fwdSOAC :: Pat Type -> StmAux () -> SOAC SOACS -> ADM ()
fwdSOAC pat aux (Screma size xs (ScremaForm f scs reds post_lam)) = do
  (pat', to_transpose) <- soacResPat (scanResults scs) (redResults reds) pat
  xs' <- soacInputsWithTangents xs
  f' <- fwdLambda f
  scs' <- mapM fwdScan scs
  reds' <- mapM fwdRed reds
  post_lam' <- fwdLambda post_lam
  addStm $ Let pat' aux $ Op $ Screma size xs' $ ScremaForm f' scs' reds' post_lam'
  tan_shape <- askShape
  forM_ to_transpose $ \(rpat, v) -> do
    v_t <- lookupType v
    let perm = rearrangeInverse $ vecPerm tan_shape v_t
    letBind rpat $ BasicOp $ Rearrange v perm
  where
    zeroTans lam =
      mapM (letSubExp "zero" . zeroExp <=< tanType) $ lambdaReturnType lam

    fwdScan :: Scan SOACS -> ADM (Scan SOACS)
    fwdScan sc = do
      op' <- fwdLambda $ scanLambda sc
      neutral_tans <- zeroTans $ scanLambda sc
      pure $
        Scan
          { scanNeutral = scanNeutral sc `interleave` neutral_tans,
            scanLambda = op'
          }
    fwdRed :: Reduce SOACS -> ADM (Reduce SOACS)
    fwdRed red = do
      op' <- fwdLambda $ redLambda red
      neutral_tans <- zeroTans $ redLambda red
      pure $
        Reduce
          { redComm = redComm red,
            redLambda = op',
            redNeutral = redNeutral red `interleave` neutral_tans
          }
fwdSOAC pat aux (Stream size xs accs lam) = do
  pat' <- bundleNewPat pat
  lam' <- fwdStreamLambda (length accs) lam
  xs' <- soacInputsWithTangents xs
  accs_tan <- mapM (letSubExp "zero" . zeroExp <=< tanType <=< subExpType) accs
  let accs' = interleave accs accs_tan
  addStm $ Let pat' aux $ Op $ Stream size xs' accs' lam'
fwdSOAC pat aux (Hist w arrs ops bucket_fun) = do
  -- Forward-mode differentiation of Hist:
  --   1. Compute bucket_fun and its tangents with a Map SOAC.
  --   2. Emit the primal Hist (unchanged).
  --   3. For tangents:
  --      - Non-vectorised: a Hist with the same indices but tangent values.
  --      - Vectorised: map over the tangent vector shape, each iteration
  --        performing a Hist on a slice of the tangent values.
  tan_shape <- askShape
  arrs' <- soacInputsWithTangents arrs

  -- Step 1: Map the forward-differentiated bucket_fun over the inputs.
  -- The lambda returns: [indices..., primal_values..., tangent_values...]
  -- In the non-vectorised case tangent_values are scalars; in the vectorised
  -- case they are arrays of shape tan_shape.
  bucket_fun' <- fwdHistBucketTansLast bucket_fun
  map_res <- letTupExp "hist_map" . Op . Screma w arrs' =<< mapSOAC bucket_fun'

  -- Split the map results into indices, primal values, and tangent values.
  let (idx_arrs, rest) = splitAt n_indices map_res
      (val_arrs, tan_val_arrs) = splitAt n_vals rest

  -- Step 2: Emit the primal Hist.
  let Pat pes = pat
      primal_map_lam_ts = map Prim (replicate n_indices $ IntType Int64) ++ concatMap (lambdaReturnType . histOp) ops
  primal_map_lam <- mkIdentityLambda primal_map_lam_ts
  addStm $ Let (Pat pes) aux $ Op $ Hist w (idx_arrs ++ val_arrs) ops primal_map_lam

  -- Step 3: Emit the tangent Hist.
  -- The tangent Hist uses fwdLambda on the operator, which needs interleaved
  -- (primal, tangent) values. Its output is interleaved (primal_aux, tangent).
  if tan_shape == mempty
    then do
      -- Non-vectorised: a single Hist with fwd-differentiated ops.
      tan_hist_ops <- mapM (mkFwdTanHistOp False) ops
      -- Input to the tangent Hist: indices ++ interleaved(primal_vals, tangent_vals)
      let interleaved_vals = interleave val_arrs tan_val_arrs
          tan_hist_input_ts =
            map Prim (replicate n_indices $ IntType Int64)
              ++ concatMap (\op -> interleave (lambdaReturnType (histOp op)) (lambdaReturnType (histOp op))) ops
      tan_hist_lam <- mkIdentityLambda tan_hist_input_ts
      -- Output pat: for each op, interleaved (aux_primal_pe, tangent_pe)
      (tan_pes, aux_pes) <- fmap unzip $
        forM pes $ \(PatElem p t) -> do
          p_tan <- tanVName p
          insertTan p p_tan
          t_tan <- tanType t
          p_aux <- newVName (baseName p <> "_hist_aux")
          pure (PatElem p_tan t_tan, PatElem p_aux t)
      let out_pes = concatMap (\(a, t) -> [a, t]) $ zip aux_pes tan_pes
      letBind (Pat out_pes) . Op $
        Hist w (idx_arrs ++ interleaved_vals) tan_hist_ops tan_hist_lam
    else do
      -- Vectorised: map over tan_shape, each iteration performing a Hist on
      -- the corresponding slice of the tangent values.
      -- tan_val_arrs have type [w][tan_shape...]elem_type (from the map output).
      -- We need [tan_shape...][w]elem_type, so transpose first.
      tan_val_arrs_tr <- forM tan_val_arrs $ \v -> do
        v_t <- lookupType v
        let perm = vecPerm tan_shape v_t
        letExp (baseName v <> "_tr") $ BasicOp $ Rearrange v perm
      -- Now tan_val_arrs_tr have shape [tan_shape...][w]elem_type.
      tan_pes <- forM pes $ \(PatElem p t) -> do
        p_tan <- tanVName p
        insertTan p p_tan
        t_tan <- tanType t
        pure $ PatElem p_tan t_tan
      mapOverTanShape tan_shape idx_arrs val_arrs tan_val_arrs_tr ops (Pat tan_pes)
  where
    op_ranks = map (shapeRank . histShape) ops
    op_n_vals = map (length . lambdaReturnType . histOp) ops
    n_indices = sum op_ranks
    n_vals = sum op_n_vals

    splitInto :: [Int] -> [a] -> [[a]]
    splitInto [] _ = []
    splitInto (n : ns) xs = take n xs : splitInto ns (drop n xs)

    -- Forward-differentiate the bucket function body, returning:
    -- [indices, primal_values, tangent_values] (tangents last, not interleaved).
    fwdBodyHistTansLast (Body _ stms res) = buildBody_ $ do
      mapM_ fwdStm stms
      let (res_is, res_vs) = splitAt n_indices res
      res_vs_tan <- mapM tangent res_vs
      pure $ res_is ++ res_vs ++ res_vs_tan

    fwdHistBucketTansLast (Lambda params _ body) = do
      params' <- bundleNewList params
      mkLambda params' $ bodyBind =<< fwdBodyHistTansLast body

    -- Create a forward-differentiated tangent HistOp.
    -- The operator is fwdLambda(op) run in non-vectorised mode (scalar tangents),
    -- taking interleaved (primal, tangent) scalar params and producing interleaved
    -- (primal, tangent) scalar results.
    mkFwdTanHistOp :: Bool -> HistOp SOACS -> ADM (HistOp SOACS)
    mkFwdTanHistOp _inMap (HistOp shape rf dest nes op) = do
      -- Auxiliary primal dest arrays (copies of original dest).
      dest_aux <- mapM
        (\d -> letExp (baseName d <> "_hist_aux") $ BasicOp $ Replicate mempty (Var d))
        dest
      -- Tangent dest arrays (zeros).
      dest_tan <- forM (lambdaReturnType op) $ \t ->
        letExp "hist_tan_dest" $ BasicOp $ Replicate shape $ Constant $ blankPrimValue $ elemType t
      -- Zero tangent neutral elements.
      nes_tan <- mapM (letSubExp "zero" . zeroExp) $ lambdaReturnType op
      -- Forward-differentiated operator (in non-vectorised mode since op is scalar).
      op' <- local (\env -> env {envTanShape = mempty}) $ fwdLambda op
      pure $
        HistOp
          shape
          rf
          (interleave dest_aux dest_tan)
          (interleave nes nes_tan)
          op'

    -- Map over the tangent vector shape. Inside the map, each iteration extracts
    -- a scalar slice from the tangent arrays and runs a Hist with fwdLambda ops.
    -- val_as: primal value arrays [w]elem_type (same for all tangent positions).
    -- tan_as: tangent arrays [tan_shape...][w]elem_type (after transpose).
    mapOverTanShape :: Shape -> [VName] -> [VName] -> [VName] -> [HistOp SOACS] -> Pat Type -> ADM ()
    mapOverTanShape ts idx_as val_as tan_as hist_ops tan_pat = do
      let outer_dim = shapeSize 0 ts
          inner_shape = Shape $ drop 1 $ shapeDims ts

      -- Parameters for the outer map: one param per tangent array.
      tan_params <- forM tan_as $ \v -> do
        v_t <- lookupType v
        newParam "tan_slice" $ rowType v_t

      map_lam <- mkLambda tan_params $ do
        let tan_param_vs = map paramName tan_params
        if inner_shape == mempty
          then do
            -- Base case: tangent values are [w]elem_type.
            -- Build a Hist with fwdLambda ops, interleaving primal + tangent values.
            tan_hist_ops <- mapM (mkScalarFwdTanHistOp val_as) hist_ops
            let interleaved_vals = interleave val_as tan_param_vs
                lam_ts =
                  map Prim (replicate n_indices $ IntType Int64)
                    ++ concatMap (\op -> interleave (lambdaReturnType (histOp op)) (lambdaReturnType (histOp op))) hist_ops
            id_lam <- mkIdentityLambda lam_ts
            -- The Hist output is interleaved (aux, tan) for each op.
            all_res <- letTupExp "hist_tan_res" (Op $ Hist w (idx_as ++ interleaved_vals) tan_hist_ops id_lam)
            -- Extract only tangent results (every other output starting at index 1).
            let n_per_op = map (\op -> length (lambdaReturnType (histOp op)) * 2) hist_ops
                res_groups = splitInto n_per_op all_res
                tan_results = concatMap (map snd . filter (odd . fst) . zip [0..]) res_groups
            pure $ varsRes tan_results
          else do
            -- Multi-dimensional tangent shape: recurse with nested map.
            let next_dim = shapeSize 0 inner_shape
                remaining = Shape $ drop 1 $ shapeDims inner_shape
            inner_params <- forM tan_param_vs $ \v -> do
              v_t <- lookupType v
              newParam "tan_inner" $ rowType v_t
            inner_lam <- mkLambda inner_params $ do
              let inner_vs = map paramName inner_params
              if remaining == mempty
                then do
                  tan_hist_ops <- mapM (mkScalarFwdTanHistOp val_as) hist_ops
                  let interleaved_vals = interleave val_as inner_vs
                      lam_ts =
                        map Prim (replicate n_indices $ IntType Int64)
                          ++ concatMap (\op -> interleave (lambdaReturnType (histOp op)) (lambdaReturnType (histOp op))) hist_ops
                  id_lam <- mkIdentityLambda lam_ts
                  all_res <- letTupExp "hist_tan_res" (Op $ Hist w (idx_as ++ interleaved_vals) tan_hist_ops id_lam)
                  let n_per_op = map (\op -> length (lambdaReturnType (histOp op)) * 2) hist_ops
                      res_groups = splitInto n_per_op all_res
                      tan_results = concatMap (map snd . filter (odd . fst) . zip [0..]) res_groups
                  pure $ varsRes tan_results
                else
                  error "fwdSOAC Hist: tangent shapes deeper than 2D not supported"
            inner_res <- letTupExp "hist_tan_inner" . Op . Screma next_dim tan_param_vs =<< mapSOAC inner_lam
            pure $ varsRes inner_res

      res <- letTupExp "hist_tan_vec" . Op . Screma outer_dim tan_as =<< mapSOAC map_lam
      forM_ (zip (patElems tan_pat) res) $ \(pe, v) ->
        letBind (Pat [pe]) $ BasicOp $ SubExp $ Var v

    -- Create a fwd-differentiated tangent HistOp for use inside the map
    -- (scalar tangent values, primal values accessed as free variables).
    mkScalarFwdTanHistOp :: [VName] -> HistOp SOACS -> ADM (HistOp SOACS)
    mkScalarFwdTanHistOp _primal_val_as (HistOp shape rf _dest nes op) = do
      -- Auxiliary primal dest and tangent dest: zero-filled arrays.
      dest_aux <- forM (lambdaReturnType op) $ \t ->
        letExp "hist_aux_dest" $ BasicOp $ Replicate shape $ Constant $ blankPrimValue $ elemType t
      dest_tan <- forM (lambdaReturnType op) $ \t ->
        letExp "hist_tan_dest" $ BasicOp $ Replicate shape $ Constant $ blankPrimValue $ elemType t
      nes_tan <- mapM (letSubExp "zero" . zeroExp) $ lambdaReturnType op
      op' <- local (\env -> env {envTanShape = mempty}) $ fwdLambda op
      pure $
        HistOp
          shape
          rf
          (interleave dest_aux dest_tan)
          (interleave nes nes_tan)
          op'
fwdSOAC pat aux (WithVJP args lam _) = do
  -- You have a custom adjoint? Too bad we are in tangent land.
  (mapM_ fwdStm <=< runBuilder_) $ do
    lam_res <- auxing aux $ eLambda lam $ map eSubExp args
    forM (zip (patNames pat) lam_res) $ \(v, SubExpRes cs se) ->
      certifying cs $ letBindNames [v] $ BasicOp $ SubExp se
fwdSOAC _ _ JVP {} =
  error "fwdSOAC: nested JVP not allowed."
fwdSOAC _ _ VJP {} =
  error "fwdSOAC: nested VJP not allowed."

fwdStm :: Stm SOACS -> ADM ()
fwdStm (Let pat aux (BasicOp (UpdateAcc safety acc i x))) = do
  pat_tan <- newTanPat pat
  addStm $ Let pat aux $ BasicOp $ UpdateAcc safety acc i x
  addStm . Let pat_tan aux <=< withTansI acc x $ \is acc_tan x_tan' -> do
    pure $ BasicOp $ UpdateAcc safety acc_tan (is <> i) x_tan'
fwdStm stm@(Let pat aux (BasicOp e)) = do
  -- XXX: this has to be too naive.
  unless (any isAcc $ patTypes pat) $ addStm stm
  basicFwd pat aux e
fwdStm stm@(Let pat aux (Apply f args _ _))
  | Just (ret, argts) <- M.lookup f builtInFunctions = do
      addStm stm
      pat_tan <- newTanPat pat
      let arg_pes = zipWith primExpFromSubExp argts (map fst args)
      case pdBuiltin f arg_pes of
        Nothing ->
          error $ "No partial derivative defined for builtin function: " ++ prettyString f
        Just derivs -> do
          let convertTo tt e
                | e_t == tt = e
                | otherwise =
                    case (tt, e_t) of
                      (IntType tt', IntType ft) -> ConvOpExp (SExt ft tt') e
                      (FloatType tt', FloatType ft) -> ConvOpExp (FPConv ft tt') e
                      (Bool, FloatType ft) -> ConvOpExp (FToB ft) e
                      (FloatType tt', Bool) -> ConvOpExp (BToF tt') e
                      _ -> error $ "fwdStm.convertTo: " ++ prettyString (f, tt, e_t)
                where
                  e_t = primExpType e

          auxing aux . letBind pat_tan <=< withAnyTans (map fst args) $
            \arg_tans' ->
              foldl1 (~+~) $ zipWith (~*~) (map (convertTo ret) arg_tans') derivs
fwdStm (Let pat aux (Match ses cases defbody (MatchDec ret ifsort))) = do
  cases' <- slocal' $ mapM (traverse fwdBody) cases
  defbody' <- slocal' $ fwdBody defbody
  pat' <- bundleNewPat pat
  ret' <- bundleTangents ret
  addStm $ Let pat' aux $ Match ses cases' defbody' $ MatchDec ret' ifsort
fwdStm (Let pat aux (Loop val_pats loop@(WhileLoop v) body)) = do
  val_pats' <- bundleNewList val_pats
  pat' <- bundleNewPat pat
  body' <-
    localScope (scopeOfFParams (map fst val_pats') <> scopeOfLoopForm loop) . slocal' $
      fwdBody body
  addStm $ Let pat' aux $ Loop val_pats' (WhileLoop v) body'
fwdStm (Let pat aux (Loop val_pats loop@(ForLoop i it bound) body)) = do
  pat' <- bundleNewPat pat
  val_pats' <- bundleNewList val_pats
  body' <-
    localScope (scopeOfFParams (map fst val_pats') <> scopeOfLoopForm loop) . slocal' $
      fwdBody body
  addStm $ Let pat' aux $ Loop val_pats' (ForLoop i it bound) body'
fwdStm (Let pat aux (WithAcc inputs lam)) = do
  inputs_tan <- forM inputs $ \(shape, arrs, op) -> do
    arrs_tan <- mapM tangent arrs
    tan_shape <- askShape
    op' <- case op of
      Nothing -> pure Nothing
      Just (op_lam, nes) -> do
        -- We assume that op_lam has unit partial derivatives (i.e., is some
        -- kind of addition). This is the case for all WithAccs produced by VJP.
        lams <- mapM addLambda $ lambdaReturnType op_lam
        -- Horizontally fuse the lambdas to produce a single one.
        idx_params <- replicateM (shapeRank shape) $ newParam "idx" $ Prim int64
        let (xs, ys) = bimap concat concat $ unzip $ map (splitAt 1 . lambdaParams) lams
        op_lam' <- mkLambda (idx_params <> xs <> ys) $ mconcat <$> mapM (bodyBind . lambdaBody) lams
        pure $ Just (op_lam', nes)
    pure (tan_shape <> shape, arrs_tan, op')
  pat' <- bundleNewPat pat
  lam' <- fwdWithAccLambda inputs lam
  addStm $ Let pat' aux $ WithAcc (interleave inputs inputs_tan) lam'
fwdStm (Let pat aux (Op soac)) = fwdSOAC pat aux soac
fwdStm stm =
  error $ "unhandled forward mode AD for Stm: " ++ prettyString stm ++ "\n" ++ show stm

fwdBody :: Body SOACS -> ADM (Body SOACS)
fwdBody (Body _ stms res) = buildBody_ $ do
  mapM_ fwdStm stms
  bundleTangents res

fwdBodyTansLast :: Body SOACS -> ADM (Body SOACS)
fwdBodyTansLast (Body _ stms res) = buildBody_ $ do
  mapM_ fwdStm stms
  (res <>) <$> mapM tangent res

fwdJVP ::
  (MonadFreshNames m) =>
  Scope SOACS ->
  Shape ->
  Attrs ->
  Lambda SOACS ->
  m (Lambda SOACS)
fwdJVP scope shape attrs (Lambda params _ body) =
  runADM shape attrs . localScope scope $ do
    params_tan <- mapM newTan params
    mkLambda (params <> params_tan) $
      bodyBind =<< fwdBodyTansLast body

-- Note [Forward-Mode vectorised AD]
--
-- An primal variable of type 't' has a tangent of type '[tan_shape]t', where
-- 'tan_shape' is the vector shape (which may be empty in the non-vectorised
-- case). This requires some care for SOACs, which always map across the
-- outermost dimension: basically we have to transpose the inputs and the
-- outputs.
