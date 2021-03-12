{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- Naming scheme:
--
-- An adjoint-related object for "x" is named "x_adj".  This means
-- both actual adjoints and statements.
--
-- Do not assume "x'" means anything related to derivatives.
module Futhark.AD.Rev (revVJP) where

import Control.Monad
import Control.Monad.State.Strict
import Data.Bifunctor (first, second)
import Data.Either (partitionEithers)
import qualified Data.Map as M
import Futhark.AD.Derivatives
import Futhark.Analysis.PrimExp.Convert
import Futhark.Binder
import Futhark.IR.SOACS
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Util (splitAt3, takeLast)

--- First some general utility functions that are not specific to AD.

eReverse :: MonadBinder m => VName -> m VName
eReverse arr = do
  arr_t <- lookupType arr
  let w = arraySize 0 arr_t
  start <-
    letSubExp "rev_start" $
      BasicOp $ BinOp (Sub Int64 OverflowUndef) w (intConst Int64 1)
  let stride = intConst Int64 (-1)
      slice = fullSlice arr_t [DimSlice start w stride]
  letExp (baseString arr <> "_rev") $ BasicOp $ Index arr slice

eRotate :: MonadBinder m => [SubExp] -> VName -> m VName
eRotate rots arr = letExp (baseString arr <> "_rot") $ BasicOp $ Rotate rots arr

scanExc ::
  (MonadBinder m, Lore m ~ SOACS) =>
  String ->
  Scan SOACS ->
  [VName] ->
  m [VName]
scanExc desc scan arrs = do
  w <- arraysSize 0 <$> mapM lookupType arrs
  form <- scanSOAC [scan]
  res_incl <- letTupExp (desc <> "_incl") $ Op $ Screma w form arrs
  res_incl_rot <- mapM (eRotate [intConst Int64 (-1)]) res_incl

  iota <-
    letExp "iota" . BasicOp $
      Iota w (intConst Int64 0) (intConst Int64 1) Int64

  iparam <- newParam "iota_param" $ Prim int64
  vparams <- mapM (newParam "vp") ts
  let params = iparam : vparams

  body <- runBodyBinder . localScope (scopeOfLParams params) $ do
    let first_elem =
          eCmpOp
            (CmpEq int64)
            (eSubExp (Var (paramName iparam)))
            (eSubExp (intConst Int64 0))
    eBody
      [ eIf
          first_elem
          (resultBodyM nes)
          (resultBodyM $ map (Var . paramName) vparams)
      ]

  let lam = Lambda params body ts
  letTupExp desc $ Op $ Screma w (mapSOAC lam) (iota : res_incl_rot)
  where
    nes = scanNeutral scan
    ts = lambdaReturnType $ scanLambda scan

--- Now comes the AD.

data RState = RState
  { stateAdjs :: M.Map VName VName,
    stateNameSource :: VNameSource
  }

newtype ADM a = ADM (BinderT SOACS (State RState) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState RState,
      MonadFreshNames,
      HasScope SOACS,
      LocalScope SOACS
    )

instance MonadBinder ADM where
  type Lore ADM = SOACS
  mkExpDecM pat e = ADM $ mkExpDecM pat e
  mkBodyM bnds res = ADM $ mkBodyM bnds res
  mkLetNamesM pat e = ADM $ mkLetNamesM pat e

  addStms = ADM . addStms
  collectStms (ADM m) = ADM $ collectStms m

instance MonadFreshNames (State RState) where
  getNameSource = gets stateNameSource
  putNameSource src = modify (\env -> env {stateNameSource = src})

runADM :: MonadFreshNames m => ADM a -> m a
runADM (ADM m) =
  modifyNameSource $ \vn ->
    second stateNameSource $
      runState
        (fst <$> runBinderT m mempty)
        (RState mempty vn)

adjVName :: VName -> ADM VName
adjVName v = newVName (baseString v <> "_adj")

zeroExp :: Type -> Exp
zeroExp (Prim pt) =
  BasicOp $ SubExp $ Constant $ blankPrimValue pt
zeroExp (Array (ElemPrim pt) shape _) =
  BasicOp $ Replicate shape $ Constant $ blankPrimValue pt
zeroExp t = error $ "zeroExp: " ++ pretty t

insAdj :: VName -> VName -> ADM ()
insAdj v v_adj = modify $ \env ->
  env {stateAdjs = M.insert v v_adj $ stateAdjs env}

newAdj :: VName -> ADM VName
newAdj v = do
  v_adj <- adjVName v
  t <- lookupType v
  insAdj v v_adj
  letBindNames [v_adj] $ zeroExp t
  pure v_adj

class Adjoint a where
  lookupAdj :: a -> ADM VName
  updateAdj :: a -> VName -> ADM VName
  updateAdjSlice :: Slice SubExp -> a -> VName -> ADM VName

addBinOp :: PrimType -> BinOp
addBinOp (IntType it) = Add it OverflowWrap
addBinOp (FloatType ft) = FAdd ft
addBinOp Bool = LogAnd
addBinOp Cert = LogAnd

tabNest :: Int -> [VName] -> ([VName] -> [VName] -> ADM [VName]) -> ADM [VName]
tabNest = tabNest' []
  where
    tabNest' is 0 vs f = f (reverse is) vs
    tabNest' is n vs f = do
      vs_ts <- mapM lookupType vs
      let w = arraysSize 0 vs_ts
      iota <-
        letExp "tab_iota" . BasicOp $
          Iota w (intConst Int64 0) (intConst Int64 1) Int64
      iparam <- newParam "i" $ Prim int64
      params <- forM vs $ \v ->
        newParam (baseString v <> "_p") . rowType =<< lookupType v
      ((ret, res), stms) <- collectStms . localScope (scopeOfLParams (iparam : params)) $ do
        res <- tabNest' (paramName iparam : is) (n -1) (map paramName params) f
        ret <- mapM lookupType res
        pure (ret, map Var res)
      let lam = Lambda (iparam : params) (Body () stms res) ret
      letTupExp "tab" $ Op $ Screma w (mapSOAC lam) (iota : vs)

-- Construct a lambda for adding two values of the given type.
addLambda :: Type -> ADM Lambda
addLambda (Prim pt) = binOpLambda (addBinOp pt) pt
addLambda t@(Array (ElemPrim _) _ _) = do
  xs <- newVName "xs"
  ys <- newVName "ys"
  let t' = rowType t
  lam <- addLambda t'
  body <- insertStmsM $ do
    res <- letSubExp "lam_map" $ Op $ Screma (arraySize 0 t) (mapSOAC lam) [xs, ys]
    return $ resultBody [res]
  pure
    Lambda
      { lambdaParams = [Param xs t', Param ys t'],
        lambdaReturnType = [t'],
        lambdaBody = body
      }
addLambda t =
  error $ "addLambda: " ++ show t

-- Construct an expression for adding the two variables.
addExp :: VName -> VName -> ADM Exp
addExp x y = do
  x_t <- lookupType x
  case x_t of
    Prim pt ->
      pure $ BasicOp $ BinOp (addBinOp pt) (Var x) (Var y)
    Array {} -> do
      lam <- addLambda $ rowType x_t
      pure $ Op $ Screma (arraySize 0 x_t) (mapSOAC lam) [x, y]
    _ ->
      error $ "addExp: unexpected type: " ++ pretty x_t

setAdj :: VName -> Exp -> ADM VName
setAdj v e = do
  v_adj <- adjVName v
  letBindNames [v_adj] e
  insAdj v v_adj
  return v_adj

instance Adjoint VName where
  lookupAdj v = do
    maybeAdj <- gets $ M.lookup v . stateAdjs
    case maybeAdj of
      Nothing -> newAdj v
      Just v_adj -> return v_adj

  updateAdj v d = do
    maybeAdj <- gets $ M.lookup v . stateAdjs
    case maybeAdj of
      Nothing -> setAdj v $ BasicOp $ SubExp $ Var d
      Just v_adj -> do
        v_adj_t <- lookupType v_adj
        case v_adj_t of
          Acc _ -> do
            dims <- arrayDims <$> lookupType d
            v_adj_arr <-
              letExp (baseString v_adj <> "_arr") $
                BasicOp $ Replicate (Shape dims) $ Var v_adj
            ~[v_adj_arr'] <-
              tabNest (length dims) [v_adj_arr, d] $ \is [v_adj_arr', d'] ->
                letTupExp "acc" $
                  BasicOp $ UpdateAcc v_adj_arr' (map Var is) [Var d']
            v_adj' <-
              -- FIXME: we need a JoinAcc construct.
              letExp (baseString v <> "_adj") . BasicOp . Index v_adj_arr' $
                replicate (length dims) $ DimFix $ intConst Int64 0
            insAdj v v_adj'
            pure v_adj'
          _ -> do
            v_adj' <- letExp (baseString v <> "_adj") =<< addExp v_adj d
            insAdj v v_adj'
            pure v_adj'

  updateAdjSlice slice v d = do
    maybeAdj <- gets $ M.lookup v . stateAdjs
    t <- lookupType v
    case maybeAdj of
      Nothing -> do
        void $ lookupAdj v -- Initialise adjoint.
        updateAdjSlice slice v d
      Just v_adj -> do
        let isDimFix (DimFix i) = i
            isDimFix _ =
              error $ "Invalid slice for accumulator update: " ++ pretty slice
        v_adj_t <- lookupType v_adj
        v_adj' <- case v_adj_t of
          Acc _ ->
            letExp (baseString v_adj) . BasicOp $
              UpdateAcc v_adj (map isDimFix slice) [Var d]
          _ -> do
            v_adjslice <-
              if primType t
                then return v_adj
                else
                  letExp (baseString v ++ "_slice") $
                    BasicOp $ Index v_adj slice
            letInPlace "updated_adj" v_adj slice =<< addExp v_adjslice d
        insAdj v v_adj'
        pure v_adj'

instance Adjoint SubExp where
  lookupAdj (Constant c) =
    letExp "const_adj" $
      BasicOp $ SubExp $ Constant $ blankPrimValue $ primValueType c
  lookupAdj (Var v) = lookupAdj v

  updateAdj se@Constant {} _ = lookupAdj se
  updateAdj (Var v) d = updateAdj v d

  updateAdjSlice _ se@Constant {} _ = lookupAdj se
  updateAdjSlice slice (Var v) d = updateAdjSlice slice v d

patternName :: Pattern -> ADM VName
patternName (Pattern [] [pe]) = pure $ patElemName pe
patternName pat = error $ "Expected single-element pattern: " ++ pretty pat

-- The vast majority of BasicOps require no special treatment in the
-- forward pass and produce one value (and hence once adjoint).  We
-- deal with that case here.
commonBasicOp :: Pattern -> StmAux () -> BasicOp -> ADM () -> ADM (VName, VName)
commonBasicOp pat aux op m = do
  addStm $ Let pat aux $ BasicOp op
  m
  pat_v <- patternName pat
  pat_adj <- lookupAdj pat_v
  pure (pat_v, pat_adj)

diffBasicOp :: Pattern -> StmAux () -> BasicOp -> ADM () -> ADM ()
diffBasicOp pat aux e m =
  case e of
    CmpOp cmp x y -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      let t = cmpOpType cmp
          update contrib = do
            void $ updateAdj x contrib
            void $ updateAdj y contrib

      case t of
        FloatType ft ->
          update <=< letExp "contrib" $
            If
              (Var pat_adj)
              (resultBody [constant (floatValue ft (1 :: Int))])
              (resultBody [constant (floatValue ft (0 :: Int))])
              (IfDec [Prim (FloatType ft)] IfNormal)
        IntType it ->
          update <=< letExp "contrib" $ BasicOp $ ConvOp (BToI it) (Var pat_adj)
        Bool ->
          update pat_adj
        Cert ->
          pure ()
    --
    ConvOp op x -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      contrib <-
        letExp "contrib" $ BasicOp $ ConvOp (flipConvOp op) $ Var pat_adj
      void $ updateAdj x contrib
    --
    UnOp op x -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m

      let t = unOpType op
      contrib <- do
        let x_pe = primExpFromSubExp t x
            pat_adj' = primExpFromSubExp t (Var pat_adj)
            dx = pdUnOp op x_pe
        letExp "contrib" <=< toExp $ pat_adj' ~*~ dx

      void $ updateAdj x contrib
    --
    BinOp op x y -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m

      let t = binOpType op
          (wrt_x, wrt_y) =
            pdBinOp op (primExpFromSubExp t x) (primExpFromSubExp t y)

          pat_adj' = primExpFromSubExp t $ Var pat_adj

      adj_x <- letExp "binop_x_adj" <=< toExp $ pat_adj' ~*~ wrt_x
      adj_y <- letExp "binop_y_adj" <=< toExp $ pat_adj' ~*~ wrt_y
      void $ updateAdj x adj_x
      void $ updateAdj y adj_y
    --
    SubExp se -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      void $ updateAdj se pat_adj
    --
    Assert {} ->
      void $ commonBasicOp pat aux e m
    --
    ArrayLit elems t -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      forM_ (zip [(0 :: Int64) ..] elems) $ \(i, se) -> do
        let slice = fullSlice t [DimFix (constant i)]
        updateAdj se <=< letExp "elem_adj" $ BasicOp $ Index pat_adj slice
    --
    Index arr slice -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      void $ updateAdjSlice slice arr pat_adj
    --
    Opaque se -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      void $ updateAdj se pat_adj
    --
    Reshape _ arr -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      arr_dims <- arrayDims <$> lookupType arr
      void $
        updateAdj arr <=< letExp "adj_reshape" $
          BasicOp $ Reshape (map DimNew arr_dims) pat_adj
    --
    Rearrange perm arr -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      void $
        updateAdj arr <=< letExp "adj_rearrange" $
          BasicOp $ Rearrange (rearrangeInverse perm) pat_adj
    --
    Rotate rots arr -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      let neg = BasicOp . BinOp (Sub Int64 OverflowWrap) (intConst Int64 0)
      rots' <- mapM (letSubExp "rot_neg" . neg) rots
      void $
        updateAdj arr <=< letExp "adj_rotate" $
          BasicOp $ Rotate rots' pat_adj
    --
    Replicate (Shape ns) x -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      x_t <- subExpType x
      lam <- addLambda x_t
      ne <- letSubExp "zero" $ zeroExp x_t
      n <- letSubExp "rep_size" =<< foldBinOp (Mul Int64 OverflowUndef) (intConst Int64 1) ns
      pat_adj_flat <-
        letExp (baseString pat_adj <> "_flat") $ BasicOp $ Reshape [DimNew n] pat_adj
      reduce <- reduceSOAC [Reduce Commutative lam [ne]]
      void $
        updateAdj x
          =<< letExp "rep_contrib" (Op $ Screma n reduce [pat_adj_flat])
    --
    Concat d arr arrs _ -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      let sliceAdj _ [] = pure []
          sliceAdj start (v : vs) = do
            v_t <- lookupType v
            let w = arraySize 0 v_t
                slice = DimSlice start w (intConst Int64 1)
            pat_adj_slice <-
              letExp (baseString pat_adj <> "_slice") $
                BasicOp $ Index pat_adj (sliceAt v_t d [slice])
            start' <- letSubExp "start" $ BasicOp $ BinOp (Add Int64 OverflowUndef) start w
            slices <- sliceAdj start' vs
            pure $ pat_adj_slice : slices

      slices <- sliceAdj (intConst Int64 0) $ arr : arrs

      zipWithM_ updateAdj (arr : arrs) slices
    --
    Copy se -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      void $ updateAdj se pat_adj
    --
    Manifest _ se -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      void $ updateAdj se pat_adj
    --
    Scratch {} ->
      void $ commonBasicOp pat aux e m
    --
    Iota n _ _ t -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      ne <- letSubExp "zero" $ zeroExp $ Prim $ IntType t
      lam <- addLambda $ Prim $ IntType t
      reduce <- reduceSOAC [Reduce Commutative lam [ne]]
      void $
        updateAdj n
          =<< letExp "iota_contrib" (Op $ Screma n reduce [pat_adj])
    --
    Update {} -> error "Reverse-mode Update not handled yet."
    UnAcc {} -> error "Reverse-mode UnAcc not handled yet."
    UpdateAcc {} -> error "Reverse-mode UpdateAcc not handled yet."

commonSOAC :: Pattern -> StmAux () -> SOAC SOACS -> ADM () -> ADM [VName]
commonSOAC pat aux soac m = do
  addStm $ Let pat aux $ Op soac
  m
  mapM lookupAdj $ patternNames pat

diffMap :: [VName] -> SubExp -> Lambda -> [VName] -> ADM ()
diffMap pat_adj w map_lam as = do
  pat_adj_params <-
    mapM (newParam "map_adj_p" . rowType <=< lookupType) pat_adj
  map_lam' <- renameLambda map_lam

  let free_in_map_lam = namesToList $ freeIn map_lam'

  (prim_free, arrs_and_accs) <-
    partitionEithers <$> mapM decideOnFreeVar free_in_map_lam

  acc_params <-
    mapM
      (newParam "map_acc_p" . rowType <=< lookupType . snd . snd)
      arrs_and_accs

  let adjs_for =
        map paramName (lambdaParams map_lam')
          ++ map fst prim_free
          ++ map fst arrs_and_accs
      lam_rev_params = lambdaParams map_lam' ++ acc_params ++ pat_adj_params
      lam_rev_ret =
        map paramType (lambdaParams map_lam')
          ++ map (Prim . snd) prim_free
          ++ map paramType acc_params
  lam_rev <-
    mkLambda lam_rev_params lam_rev_ret $ do
      zipWithM_ insAdj (map fst arrs_and_accs) (map paramName acc_params)
      bodyBind . lambdaBody
        =<< diffLambda (map paramName pat_adj_params) adjs_for map_lam'

  (param_contribs, free_contribs, acc_contribs) <-
    fmap (splitAt3 (length (lambdaParams map_lam')) (length prim_free)) $
      letTupExp "map_adjs" . Op . Screma w (mapSOAC lam_rev) $
        as ++ map (snd . snd) arrs_and_accs ++ pat_adj

  zipWithM_ updateAdj as param_contribs
  zipWithM_ freeContrib prim_free free_contribs
  zipWithM_ accContrib arrs_and_accs acc_contribs
  where
    addIdxParams n lam = do
      idxs <- replicateM n $ newParam "idx" $ Prim int64
      pure $ lam {lambdaParams = idxs ++ lambdaParams lam}

    accAddLambda n t = addIdxParams n =<< addLambda t

    decideOnFreeVar v = do
      v_adj <- lookupAdj v
      v_adj_t <- lookupType v_adj
      case v_adj_t of
        Array (ElemPrim pt) (Shape ds) _ -> do
          add_lam <- accAddLambda (length ds) $ Prim pt
          zero <- letSubExp "zero" $ zeroExp $ Prim pt
          fmap (Right . (v,) . (v_adj_t,)) . letExp (baseString v <> "_acc") $
            MkAcc (Shape [w]) [v_adj] (Shape ds) (Just (add_lam, [zero]))
        Acc _ ->
          fmap (Right . (v,) . (v_adj_t,))
            . letExp (baseString v_adj <> "_rep")
            $ BasicOp $ Replicate (Shape [w]) (Var v_adj)
        Prim pt ->
          pure $ Left (v, pt)
        _ ->
          error $ "decideOnFreeVar: cannot handle type " ++ pretty v_adj_t

    accContrib (arr, (arr_adj_t, _)) contrib_acc =
      -- A hack to determine whether the adjoint was an accumulator in
      -- the first place.
      insAdj arr =<< case arr_adj_t of
        Acc _ ->
          letExp (baseString arr <> "_arr") $
            -- FIXME: we need a JoinAcc construct.
            BasicOp $ Index contrib_acc [DimFix $ intConst Int64 0]
        _ ->
          letExp (baseString arr <> "_arr") $
            BasicOp $ UnAcc contrib_acc [arr_adj_t]

    freeContrib (v, v_pt) contrib = do
      lam <- addLambda $ Prim v_pt
      zero <- letSubExp "zero" $ zeroExp $ Prim v_pt
      reduce <- reduceSOAC [Reduce Commutative lam [zero]]
      contrib_sum <-
        letExp (baseString v <> "_contrib_sum") $
          Op $ Screma w reduce [contrib]
      void $ updateAdj v contrib_sum

diffSOAC :: Pattern -> StmAux () -> SOAC SOACS -> ADM () -> ADM ()
diffSOAC pat aux soac@(Screma w form as) m
  | Just red <- singleReduce <$> isReduceSOAC form = do
    pat_adj <- commonSOAC pat aux soac m
    red' <- renameRed red
    flip_red <- renameRed =<< flipReduce red
    ls <- scanExc "ls" (redToScan red') as
    rs <-
      mapM eReverse
        =<< scanExc "ls" (redToScan flip_red)
        =<< mapM eReverse as

    (as_params, f) <- mkF $ redLambda red

    f_adj <- diffLambda pat_adj as_params f

    as_adj <- letTupExp "adjs" $ Op $ Screma w (mapSOAC f_adj) $ ls ++ as ++ rs

    zipWithM_ updateAdj as as_adj
  where
    renameRed (Reduce comm lam nes) =
      Reduce comm <$> renameLambda lam <*> pure nes

    redToScan :: Reduce SOACS -> Scan SOACS
    redToScan (Reduce _ lam nes) = Scan lam nes
    flipReduce (Reduce comm lam nes) = do
      lam' <- renameLambda lam {lambdaParams = flipParams $ lambdaParams lam}
      pure $ Reduce comm lam' nes
    flipParams ps = uncurry (flip (++)) $ splitAt (length ps `div` 2) ps

    mkF lam = do
      lam_l <- renameLambda lam
      lam_r <- renameLambda lam
      let n = length $ lambdaReturnType lam
          (lps, aps) = splitAt n $ lambdaParams lam_l
          (ips, rps) = splitAt n $ lambdaParams lam_r
      lam' <- mkLambda (lps <> aps <> rps) (lambdaReturnType lam) $ do
        lam_l_res <- bodyBind $ lambdaBody lam_l
        forM_ (zip ips lam_l_res) $ \(ip, se) ->
          letBindNames [paramName ip] $ BasicOp $ SubExp se
        bodyBind $ lambdaBody lam_r
      pure (map paramName aps, lam')
--
diffSOAC pat aux soac@(Screma w form as) m
  | Just lam <- isMapSOAC form = do
    pat_adj <- commonSOAC pat aux soac m
    diffMap pat_adj w lam as
--
diffSOAC pat _aux (Screma w form as) m
  | Just (Reduce comm red_lam nes, map_lam) <-
      first singleReduce <$> isRedomapSOAC form = do
    (mapstm, redstm) <-
      redomapToMapAndReduce pat (w, comm, red_lam, map_lam, nes, as)
    diffStm mapstm $ diffStm redstm m
diffSOAC _ _ soac _ =
  error $ "diffSOAC unhandled:\n" ++ pretty soac

diffStm :: Stm -> ADM () -> ADM ()
diffStm (Let pat aux (BasicOp e)) m =
  diffBasicOp pat aux e m
diffStm stm@(Let pat _ (Apply f args _ _)) m
  | Just (ret, argts) <- M.lookup f builtInFunctions = do
    addStm stm
    m

    pat_adj <- lookupAdj =<< patternName pat
    let arg_pes = zipWith primExpFromSubExp argts (map fst args)
        pat_adj' = primExpFromSubExp ret (Var pat_adj)

    contribs <-
      case pdBuiltin f arg_pes of
        Nothing ->
          error $ "No partial derivative defined for builtin function: " ++ pretty f
        Just derivs ->
          mapM (letExp "contrib" <=< toExp . (pat_adj' ~*~)) derivs

    zipWithM_ updateAdj (map fst args) contribs
diffStm stm@(Let pat _ (If cond tbody fbody _)) m = do
  addStm stm
  m

  let tbody_free = freeIn tbody
      fbody_free = freeIn fbody
      branches_free = namesToList $ tbody_free <> fbody_free

  adjs <- mapM lookupAdj $ patternValueNames pat

  -- We need to discard any context, as this never contributes to
  -- adjoints.
  branches_free_adj <-
    ( pure . takeLast (length branches_free)
        <=< letTupExp "branch_adj"
        <=< renameExp
      )
      =<< eIf
        (eSubExp cond)
        (diffBody adjs branches_free tbody)
        (diffBody adjs branches_free fbody)

  zipWithM_ insAdj branches_free branches_free_adj
diffStm (Let pat aux (Op soac)) m =
  diffSOAC pat aux soac m
diffStm stm _ = error $ "diffStm unhandled:\n" ++ pretty stm

diffStms :: Stms SOACS -> ADM ()
diffStms all_stms
  | Just (stm, stms) <- stmsHead all_stms =
    diffStm stm $ diffStms stms
  | otherwise =
    pure ()

subAD :: ADM a -> ADM a
subAD m = do
  old_state_adjs <- gets stateAdjs
  x <- m
  modify $ \s -> s {stateAdjs = old_state_adjs}
  pure x

diffBody :: [VName] -> [VName] -> Body -> ADM Body
diffBody res_adjs get_adjs_for (Body () stms res) = subAD $ do
  let onResult (Constant _) _ = pure ()
      onResult (Var v) v_adj = void $ updateAdj v v_adj
  (adjs, stms') <- collectStms $ do
    zipWithM_ onResult (takeLast (length res_adjs) res) res_adjs
    diffStms stms
    mapM lookupAdj get_adjs_for
  pure $ Body () stms' $ res <> map Var adjs

diffLambda :: [VName] -> [VName] -> Lambda -> ADM Lambda
diffLambda res_adjs get_adjs_for (Lambda params body _) =
  localScope (scopeOfLParams params) $ do
    Body () stms res <- diffBody res_adjs get_adjs_for body
    let body' = Body () stms $ takeLast (length get_adjs_for) res
    ts' <- mapM lookupType get_adjs_for
    pure $ Lambda params body' ts'

revVJP :: MonadFreshNames m => Scope SOACS -> Lambda -> m Lambda
revVJP scope (Lambda params body ts) =
  runADM . localScope (scope <> scopeOfLParams params) $ do
    params_adj <- forM (zip (bodyResult body) ts) $ \(se, t) ->
      Param <$> maybe (newVName "const_adj") adjVName (subExpVar se) <*> pure t

    Body () stms res <-
      localScope (scopeOfLParams params_adj) $
        diffBody (map paramName params_adj) (map paramName params) body
    let body' = Body () stms $ takeLast (length params) res

    pure $ Lambda (params ++ params_adj) body' (map paramType params)
