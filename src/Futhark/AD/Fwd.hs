{-# LANGUAGE TypeFamilies #-}

module Futhark.AD.Fwd (fwdJVP) where

import Control.Monad
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import Data.Bifunctor (second)
import Data.List (transpose)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as M
import Futhark.AD.Derivatives
import Futhark.Analysis.PrimExp.Convert
import Futhark.Builder
import Futhark.Construct
import Futhark.IR.SOACS

zeroTan :: Type -> ADM SubExp
zeroTan (Prim t) = pure $ constant $ blankPrimValue t
zeroTan t = error $ "zeroTan on non-primitive type: " ++ prettyString t

zeroExp :: Type -> Exp SOACS
zeroExp (Prim pt) =
  BasicOp $ SubExp $ Constant $ blankPrimValue pt
zeroExp (Array pt shape _) =
  BasicOp $ Replicate shape $ Constant $ blankPrimValue pt
zeroExp t = error $ "zeroExp: " ++ show t

tanType :: TypeBase s u -> ADM (TypeBase s u)
tanType (Acc acc ispace ts u) = do
  ts_tan <- mapM tanType ts
  pure $ Acc acc ispace (ts ++ ts_tan) u
tanType t = pure t

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

newtype ADM a = ADM (BuilderT SOACS (State RState) a)
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

runADM :: (MonadFreshNames m) => ADM a -> m a
runADM (ADM m) =
  modifyNameSource $ \vn ->
    second stateNameSource $
      runState
        (fst <$> runBuilderT m mempty)
        (RState mempty vn)

tanVName :: VName -> ADM VName
tanVName v = newVName (baseString v <> "_tan")

insertTan :: VName -> VName -> ADM ()
insertTan v v' =
  modify $ \env -> env {stateTans = M.insert v v' (stateTans env)}

class TanBuilder a where
  newTan :: a -> ADM a
  bundleNew :: a -> ADM [a]

bundleNewList :: (TanBuilder a) => [a] -> ADM [a]
bundleNewList = fmap mconcat . mapM bundleNew

instance TanBuilder (PatElem (TypeBase s u)) where
  newTan (PatElem p t)
    | isAcc t = do
        insertTan p p
        t' <- tanType t
        pure $ PatElem p t'
    | otherwise = do
        p' <- tanVName p
        insertTan p p'
        t' <- tanType t
        pure $ PatElem p' t'
  bundleNew pe@(PatElem _ t) = do
    pe' <- newTan pe
    if isAcc t
      then pure [pe']
      else pure [pe, pe']

newTanPat :: (TanBuilder (PatElem t)) => Pat t -> ADM (Pat t)
newTanPat (Pat pes) = Pat <$> mapM newTan pes

bundleNewPat :: (TanBuilder (PatElem t)) => Pat t -> ADM (Pat t)
bundleNewPat (Pat pes) = Pat <$> bundleNewList pes

instance TanBuilder (Param (TypeBase s u)) where
  newTan (Param _ p t) = do
    PatElem p' t' <- newTan $ PatElem p t
    pure $ Param mempty p' t'
  bundleNew param@(Param _ _ (Prim Unit)) =
    pure [param]
  bundleNew param@(Param _ _ t) = do
    param' <- newTan param
    if isAcc t
      then pure [param']
      else pure [param, param']

instance (Tangent a) => TanBuilder (Param (TypeBase s u), a) where
  newTan (p, x) = (,) <$> newTan p <*> tangent x
  bundleNew (p, x) = do
    b <- bundleNew p
    x_tan <- tangent x
    pure $ zip b [x, x_tan]

class Tangent a where
  tangent :: a -> ADM a
  bundleTan :: a -> ADM [a]

instance Tangent (TypeBase s u) where
  tangent = tanType
  bundleTan t
    | isAcc t = do
        t' <- tangent t
        pure [t']
    | otherwise = do
        t' <- tangent t
        pure [t, t']

bundleTangents :: (Tangent a) => [a] -> ADM [a]
bundleTangents = (mconcat <$>) . mapM bundleTan

instance Tangent VName where
  tangent v = do
    maybeTan <- gets $ M.lookup v . stateTans
    case maybeTan of
      Just v_tan -> pure v_tan
      Nothing -> do
        t <- lookupType v
        letExp (baseString v <> "_implicit_tan") $ zeroExp t
  bundleTan v = do
    t <- lookupType v
    if isAcc t
      then pure [v]
      else do
        v_tan <- tangent v
        pure [v, v_tan]

instance Tangent SubExp where
  tangent (Constant c) = zeroTan $ Prim $ primValueType c
  tangent (Var v) = Var <$> tangent v
  bundleTan c@Constant {} = do
    c_tan <- tangent c
    pure [c, c_tan]
  bundleTan (Var v) = fmap Var <$> bundleTan v

instance Tangent SubExpRes where
  tangent (SubExpRes cs se) = SubExpRes cs <$> tangent se
  bundleTan (SubExpRes cs se) = map (SubExpRes cs) <$> bundleTan se

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
      x_tan <- primExpFromSubExp t <$> tangent x
      auxing aux $ letBindNames (patNames pat_tan) <=< toExp $ x_tan ~*~ dx
    BinOp bop x y -> do
      let t = binOpType bop
      x_tan <- primExpFromSubExp t <$> tangent x
      y_tan <- primExpFromSubExp t <$> tangent y
      let (wrt_x, wrt_y) =
            pdBinOp bop (primExpFromSubExp t x) (primExpFromSubExp t y)
      auxing aux $
        letBindNames (patNames pat_tan) <=< toExp $
          x_tan ~*~ wrt_x ~+~ y_tan ~*~ wrt_y
    CmpOp {} ->
      addStm $ Let pat_tan aux $ zeroExp $ Prim Bool
    ConvOp cop x -> do
      x_tan <- tangent x
      addStm $ Let pat_tan aux $ BasicOp $ ConvOp cop x_tan
    Assert {} -> pure ()
    Index arr slice -> do
      arr_tan <- tangent arr
      addStm $ Let pat_tan aux $ BasicOp $ Index arr_tan slice
    Update safety arr slice se -> do
      arr_tan <- tangent arr
      se_tan <- tangent se
      addStm $ Let pat_tan aux $ BasicOp $ Update safety arr_tan slice se_tan
    Concat d (arr :| arrs) w -> do
      arr_tan <- tangent arr
      arrs_tans <- mapM tangent arrs
      addStm $ Let pat_tan aux $ BasicOp $ Concat d (arr_tan :| arrs_tans) w
    Manifest arr ds -> do
      arr_tan <- tangent arr
      addStm $ Let pat_tan aux $ BasicOp $ Manifest arr_tan ds
    Iota n _ _ it -> do
      addStm $ Let pat_tan aux $ BasicOp $ Replicate (Shape [n]) (intConst it 0)
    Replicate n x -> do
      x_tan <- tangent x
      addStm $ Let pat_tan aux $ BasicOp $ Replicate n x_tan
    Scratch t shape ->
      addStm $ Let pat_tan aux $ BasicOp $ Scratch t shape
    Reshape arr reshape -> do
      arr_tan <- tangent arr
      addStm $ Let pat_tan aux $ BasicOp $ Reshape arr_tan reshape
    Rearrange arr perm -> do
      arr_tan <- tangent arr
      addStm $ Let pat_tan aux $ BasicOp $ Rearrange arr_tan perm
    _ -> error $ "basicFwd: Unsupported op " ++ prettyString op

fwdLambda :: Lambda SOACS -> ADM (Lambda SOACS)
fwdLambda l@(Lambda params ret body) =
  Lambda <$> bundleNewList params <*> bundleTangents ret <*> inScopeOf l (fwdBody body)

fwdStreamLambda :: Lambda SOACS -> ADM (Lambda SOACS)
fwdStreamLambda l@(Lambda params ret body) =
  Lambda <$> ((take 1 params ++) <$> bundleNewList (drop 1 params)) <*> bundleTangents ret <*> inScopeOf l (fwdBody body)

interleave :: [a] -> [a] -> [a]
interleave xs ys = concat $ transpose [xs, ys]

zeroFromSubExp :: SubExp -> ADM VName
zeroFromSubExp (Constant c) =
  letExp "zero" . BasicOp . SubExp . Constant $
    blankPrimValue (primValueType c)
zeroFromSubExp (Var v) = do
  t <- lookupType v
  letExp "zero" $ zeroExp t

fwdSOAC :: Pat Type -> StmAux () -> SOAC SOACS -> ADM ()
fwdSOAC pat aux (Screma size xs (ScremaForm f scs reds post_lam)) = error "Not implemented" $ do
  pat' <- bundleNewPat pat
  xs' <- bundleTangents xs
  f' <- fwdLambda f
  scs' <- mapM fwdScan scs
  reds' <- mapM fwdRed reds
  addStm $ Let pat' aux $ Op $ Screma size xs' $ ScremaForm f' scs' reds' post_lam
  where
    fwdScan :: Scan SOACS -> ADM (Scan SOACS)
    fwdScan sc = do
      op' <- fwdLambda $ scanLambda sc
      neutral_tans <- mapM zeroFromSubExp $ scanNeutral sc
      pure $
        Scan
          { scanNeutral = scanNeutral sc `interleave` map Var neutral_tans,
            scanLambda = op'
          }
    fwdRed :: Reduce SOACS -> ADM (Reduce SOACS)
    fwdRed red = do
      op' <- fwdLambda $ redLambda red
      neutral_tans <- mapM zeroFromSubExp $ redNeutral red
      pure $
        Reduce
          { redComm = redComm red,
            redLambda = op',
            redNeutral = redNeutral red `interleave` map Var neutral_tans
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
  pat' <- bundleNewPat pat
  x' <- bundleTangents x
  acc_tan <- tangent acc
  addStm $ Let pat' aux $ BasicOp $ UpdateAcc safety acc_tan i x'
fwdStm stm@(Let pat aux (BasicOp e)) = do
  -- XXX: this has to be too naive.
  unless (any isAcc $ patTypes pat) $
    addStm stm
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
  inputs' <- forM inputs $ \(shape, arrs, op) -> do
    arrs_tan <- mapM tangent arrs
    op' <- case op of
      Nothing -> pure Nothing
      Just (op_lam, nes) -> do
        nes_tan <- mapM (fmap Var . zeroFromSubExp) nes
        op_lam' <- fwdLambda op_lam
        case op_lam' of
          Lambda ps ret body -> do
            let op_lam'' = Lambda (removeIndexTans (shapeRank shape) ps) ret body
            pure $ Just (op_lam'', interleave nes nes_tan)
    pure (shape, arrs <> arrs_tan, op')
  pat' <- bundleNewPat pat
  lam' <- fwdLambda lam
  addStm $ Let pat' aux $ WithAcc inputs' lam'
  where
    removeIndexTans 0 ps = ps
    removeIndexTans i (p : _ : ps) = p : removeIndexTans (i - 1) ps
    removeIndexTans _ ps = ps
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

fwdJVP :: (MonadFreshNames m) => Scope SOACS -> Lambda SOACS -> m (Lambda SOACS)
fwdJVP scope l@(Lambda params ret body) =
  runADM . localScope scope . inScopeOf l $ do
    params_tan <- mapM newTan params
    body_tan <- fwdBodyTansLast body
    ret_tan <- mapM tangent ret
    pure $ Lambda (params ++ params_tan) (ret <> ret_tan) body_tan
