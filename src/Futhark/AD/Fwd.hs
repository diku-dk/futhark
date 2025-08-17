{-# LANGUAGE TypeFamilies #-}

module Futhark.AD.Fwd (fwdJVP) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifunctor (bimap, second)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as M
import Futhark.AD.Derivatives
import Futhark.Analysis.PrimExp.Convert
import Futhark.Builder
import Futhark.IR.SOACS
import Futhark.Tools
import Futhark.Util (interleave)

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
  pure $
    arrayOf (Prim (elemType t)) (shape `prependShape` arrayShape t) u
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

newtype ADM a = ADM (BuilderT SOACS (ReaderT Shape (State RState)) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState RState,
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
askShape = ADM $ lift ask

runADM :: (MonadFreshNames m) => Shape -> ADM a -> m a
runADM shape (ADM m) =
  modifyNameSource $ \vn ->
    second stateNameSource $
      runState
        (runReaderT (fst <$> runBuilderT m mempty) shape)
        (RState mempty vn)

tanVName :: VName -> ADM VName
tanVName v = newVName (baseString v <> "_tan")

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
        letExp (baseString v <> "_implicit_tan") $ zeroExp $ t `arrayOfShape` tan_shape
  bundleTan v = do
    v_tan <- tangent v
    pure (v, v_tan)

instance Tangent SubExp where
  tangent (Constant c) = pure $ constant $ blankPrimValue $ primValueType c
  tangent (Var v) = Var <$> tangent v
  bundleTan c@Constant {} = do
    c_tan <- tangent c
    pure (c, c_tan)
  bundleTan (Var v) = bimap Var Var <$> bundleTan v

instance Tangent SubExpRes where
  tangent (SubExpRes cs se) = SubExpRes cs <$> tangent se
  bundleTan (SubExpRes cs se) = bimap (SubExpRes cs) (SubExpRes cs) <$> bundleTan se

asVName :: SubExp -> ADM VName
asVName (Var v) = pure v
asVName (Constant x) = letExp "v" $ BasicOp $ SubExp $ Constant x

withTan ::
  SubExp ->
  (SubExp -> ADM (Exp SOACS)) ->
  ADM (Exp SOACS)
withTan x f = do
  shape <- askShape
  x_tan <- tangent x
  if shape == mempty
    then f x_tan
    else do
      let w = shapeSize 0 shape
      x_tan_v <- asVName x_tan
      x_tan_p <- newParam "x_tanp" . rowType =<< lookupType x_tan_v
      lam <- mkLambda [x_tan_p] $ do
        fmap (subExpsRes . pure) . letSubExp "tan"
          =<< f (Var (paramName x_tan_p))
      pure $ Op $ Screma w [x_tan_v] (mapSOAC lam)

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
      pure $ Op $ Screma w (iota_v : x_tan : ys_tan_vs) (mapSOAC lam)

withTans ::
  PrimType ->
  SubExp ->
  SubExp ->
  (PrimExp VName -> PrimExp VName -> PrimExp VName) ->
  ADM (Exp SOACS)
withTans t x y f = do
  shape <- askShape
  x_tan <- asVName =<< tangent x
  y_tan <- asVName =<< tangent y
  if shape == mempty
    then toExp $ f (LeafExp x_tan t) (LeafExp y_tan t)
    else do
      let w = shapeSize 0 shape
      x_tan_p <- newParam "x_tanp" . rowType =<< lookupType x_tan
      y_tan_p <- newParam "y_tanp" . rowType =<< lookupType y_tan
      lam <- mkLambda [x_tan_p, y_tan_p] $ do
        fmap (subExpsRes . pure) . letSubExp "tan" <=< toExp $
          f
            (LeafExp (paramName x_tan_p) t)
            (LeafExp (paramName y_tan_p) t)
      pure $ Op $ Screma w [x_tan, y_tan] (mapSOAC lam)

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
      ses_tan <- mapM tangent ses
      addStm $ Let pat_tan aux $ BasicOp $ ArrayLit ses_tan t
    UnOp unop x -> do
      let t = unOpType unop
          x_pe = primExpFromSubExp t x
          dx = pdUnOp unop x_pe
      auxing aux $ letBind pat_tan <=< withTan x $ \x_tan ->
        toExp $ primExpFromSubExp t x_tan ~*~ dx
    BinOp bop x y -> do
      let t = binOpType bop
      auxing aux . letBind pat_tan <=< withTans t x y $
        \x_tan y_tan ->
          let (wrt_x, wrt_y) =
                pdBinOp bop (primExpFromSubExp t x) (primExpFromSubExp t y)
           in x_tan ~*~ wrt_x ~+~ y_tan ~*~ wrt_y
    CmpOp {} -> do
      tan_shape <- askShape
      addStm $ Let pat_tan aux $ zeroExp $ Prim Bool `arrayOfShape` tan_shape
    ConvOp cop x ->
      auxing aux $ letBind pat_tan <=< withTan x $ \x_tan ->
        pure $ BasicOp $ ConvOp cop x_tan
    Assert {} -> pure ()
    Index arr slice -> do
      arr_tan <- tangent arr
      dims <- shapeDims <$> askShape
      let slice' = Slice $ map sliceDim dims <> unSlice slice
      addStm $ Let pat_tan aux $ BasicOp $ Index arr_tan slice'
    Update safety arr slice se -> do
      arr_tan <- tangent arr
      se_tan <- tangent se
      addStm $ Let pat_tan aux $ BasicOp $ Update safety arr_tan slice se_tan
    Concat d (arr :| arrs) w -> do
      arr_tan <- tangent arr
      arrs_tans <- mapM tangent arrs
      r <- shapeRank <$> askShape
      addStm $ Let pat_tan aux $ BasicOp $ Concat (d + r) (arr_tan :| arrs_tans) w
    Manifest arr ds -> do
      arr_tan <- tangent arr
      r <- shapeRank <$> askShape
      addStm . Let pat_tan aux . BasicOp $
        Manifest arr_tan ([0 .. r - 1] ++ map (+ r) ds)
    Iota n _ _ it -> do
      shape <- askShape
      addStm . Let pat_tan aux . BasicOp $
        Replicate (shape <> Shape [n]) (intConst it 0)
    Replicate n x ->
      auxing aux $ letBind pat_tan <=< withTan x $ \x_tan ->
        pure $ BasicOp $ Replicate n x_tan
    Scratch t shape -> do
      tan_shape <- askShape
      addStm $ Let pat_tan aux $ BasicOp $ Scratch t $ shapeDims tan_shape <> shape
    Reshape arr reshape -> do
      arr_tan <- tangent arr
      shape <- askShape
      addStm $ Let pat_tan aux $ BasicOp $ Reshape arr_tan (newshapeInner shape reshape)
    Rearrange arr perm -> do
      arr_tan <- tangent arr
      r <- shapeRank <$> askShape
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

fwdStreamLambda :: Lambda SOACS -> ADM (Lambda SOACS)
fwdStreamLambda (Lambda params _ body) = do
  params' <- (take 1 params ++) <$> bundleNewList (drop 1 params)
  mkLambda params' $ bodyBind =<< fwdBody body

zeroFromSubExp :: SubExp -> ADM VName
zeroFromSubExp (Constant c) =
  letExp "zero" . BasicOp . SubExp . Constant $
    blankPrimValue (primValueType c)
zeroFromSubExp (Var v) = do
  t <- lookupType v
  letExp "zero" $ zeroExp t

vecPerm :: Shape -> Type -> [Int]
vecPerm tan_shape t =
  [shapeRank tan_shape]
    ++ [0 .. shapeRank tan_shape - 1]
    ++ [shapeRank tan_shape + 1 .. arrayRank t - 1]

pushTanShape :: VName -> ADM VName
pushTanShape v = do
  tan_shape <- askShape
  v_t <- lookupType v
  if tan_shape == mempty || arrayShape v_t == tan_shape || isAcc v_t
    then pure v
    else do
      let perm = vecPerm tan_shape v_t
      letExp (baseString v <> "_tr") $ BasicOp $ Rearrange v perm

soacInputsWithTangents :: [VName] -> ADM [VName]
soacInputsWithTangents xs = do
  xs_tans <- mapM (pushTanShape <=< tangent) xs
  pure $ interleave xs xs_tans

soacResPat :: Pat Type -> ADM (Pat Type, [(Pat Type, VName)])
soacResPat (Pat pes) = do
  pes_tan <- mapM newTan pes
  bimap (Pat . interleave pes) mconcat . unzip <$> mapM tweakPatElem pes_tan
  where
    tweakPatElem pe@(PatElem v v_t) = do
      tan_shape <- askShape
      if tan_shape == mempty || arrayShape v_t == tan_shape || isAcc v_t
        then pure (pe, [])
        else do
          let perm = vecPerm tan_shape v_t
          v' <- newName v
          pure (PatElem v' $ rearrangeType perm v_t, [(Pat [pe], v')])

fwdSOAC :: Pat Type -> StmAux () -> SOAC SOACS -> ADM ()
fwdSOAC pat aux (Screma size xs (ScremaForm f scs reds)) = do
  (pat', to_transpose) <- soacResPat pat
  xs' <- soacInputsWithTangents xs
  f' <- fwdLambda f
  scs' <- mapM fwdScan scs
  reds' <- mapM fwdRed reds
  addStm $ Let pat' aux $ Op $ Screma size xs' $ ScremaForm f' scs' reds'
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
fwdSOAC pat aux (Stream size xs nes lam) = do
  pat' <- bundleNewPat pat
  lam' <- fwdStreamLambda lam
  xs' <- bundleTangents xs
  nes_tan <- mapM (fmap Var . zeroFromSubExp) nes
  let nes' = interleave nes nes_tan
  addStm $ Let pat' aux $ Op $ Stream size xs' nes' lam'
fwdSOAC pat aux (Hist w arrs ops bucket_fun) = do
  pat' <- bundleNewPat pat
  ops' <- mapM fwdHist ops
  bucket_fun' <- fwdHistBucket bucket_fun
  arrs' <- bundleTangents arrs
  addStm $ Let pat' aux $ Op $ Hist w arrs' ops' bucket_fun'
  where
    n_indices = sum $ map (shapeRank . histShape) ops
    fwdBodyHist (Body _ stms res) = buildBody_ $ do
      mapM_ fwdStm stms
      let (res_is, res_vs) = splitAt n_indices res
      (res_is ++) <$> bundleTangents res_vs
    fwdHistBucket l@(Lambda params ret body) =
      let (r_is, r_vs) = splitAt n_indices ret
       in Lambda
            <$> bundleNewList params
            <*> ((r_is ++) <$> bundleTangents r_vs)
            <*> inScopeOf l (fwdBodyHist body)

    fwdHist :: HistOp SOACS -> ADM (HistOp SOACS)
    fwdHist (HistOp shape rf dest nes op) = do
      dest' <- bundleTangents dest
      nes_tan <- mapM (fmap Var . zeroFromSubExp) nes
      op' <- fwdLambda op
      pure $
        HistOp
          { histShape = shape,
            histRaceFactor = rf,
            histDest = dest',
            histNeutral = interleave nes nes_tan,
            histOp = op'
          }
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
fwdStm stm@(Let pat _ (Apply f args _ _))
  | Just (ret, argts) <- M.lookup f builtInFunctions = do
      addStm stm
      arg_tans <-
        zipWith primExpFromSubExp argts <$> mapM (tangent . fst) args
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
          zipWithM_ (letBindNames . pure) (patNames pat_tan)
            =<< mapM toExp (zipWith (~*~) (map (convertTo ret) arg_tans) derivs)
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
    localScope (scopeOfFParams (map fst val_pats) <> scopeOfLoopForm loop) . slocal' $
      fwdBody body
  addStm $ Let pat' aux $ Loop val_pats' (WhileLoop v) body'
fwdStm (Let pat aux (Loop val_pats loop@(ForLoop i it bound) body)) = do
  pat' <- bundleNewPat pat
  val_pats' <- bundleNewList val_pats
  body' <-
    localScope (scopeOfFParams (map fst val_pats) <> scopeOfLoopForm loop) . slocal' $
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
  Lambda SOACS ->
  m (Lambda SOACS)
fwdJVP scope shape (Lambda params _ body) =
  runADM shape . localScope scope $ do
    params_tan <- mapM newTan params
    mkLambda (params <> params_tan) $
      bodyBind =<< fwdBodyTansLast body
