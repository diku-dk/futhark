{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- Naming scheme:
--
-- An adjoint-related object for "x" is named "x_adj".  This means
-- both actual adjoints and statements.
--
-- Do not assume "x'" means anything related to derivatives.
module Futhark.AD.Rev (revVJP) where

import Debug.Trace
import Control.Monad
import Control.Monad.State.Strict
import Data.Bifunctor (second)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Futhark.Transform.Substitute
import Futhark.AD.Derivatives
import Futhark.Analysis.PrimExp.Convert
import Futhark.Binder
import Futhark.Construct
import Futhark.IR.SOACS
import Futhark.Transform.Rename
import Futhark.Util (takeLast)

----------------------
-- Helper functions --
----------------------

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

onePrimValue :: PrimType -> PrimValue
onePrimValue (IntType Int8) = IntValue $ Int8Value 1
onePrimValue (IntType Int16) = IntValue $ Int16Value 1
onePrimValue (IntType Int32) = IntValue $ Int32Value 1
onePrimValue (IntType Int64) = IntValue $ Int64Value 1
onePrimValue (FloatType Float32) = FloatValue $ Float32Value 1.0
onePrimValue (FloatType Float64) = FloatValue $ Float64Value 1.0
onePrimValue Bool = BoolValue True
onePrimValue Cert = error "In onePrimValue, Rev.hs: input Cert not supported"

getBinOpMul :: PrimType -> BinOp
getBinOpMul (IntType x) = Mul x OverflowUndef
getBinOpMul (FloatType f) = FMul f
getBinOpMul _ = error "In getBinOpMul, Rev.hs: input Cert not supported"

getBinOpPlus :: PrimType -> BinOp
getBinOpPlus (IntType x) = Add x OverflowUndef
getBinOpPlus (FloatType f) = FAdd f
getBinOpPlus _ = error "In getBinOpMul, Rev.hs: input Cert not supported"




-- Succeeds for (\ x y -> x + y) or (\x y -> y + x)
isRedPlus :: Lambda -> Bool
isRedPlus lam =
  isRedStm (map paramName $ lambdaParams lam)
           (bodyResult $ lambdaBody lam)
           (stmsToList $ bodyStms $ lambdaBody lam)
  where
    isAddOp (Add _ _) = True
    isAddOp (FAdd  _) = True
    isAddOp _         = False
    isRedStm [a,b] [r] [Let (Pattern [] [pe]) _aux (BasicOp (BinOp op x y)) ] =
      isAddOp op && (r == Var (patElemName pe)) && ( (x == Var a && y == Var b) || (x == Var b && y == Var a) )
    isRedStm _ _ _ = False

adBlank :: Type -> BasicOp
adBlank (Prim pt) = SubExp $ Constant $ blankPrimValue pt
adBlank (Array (ElemPrim t) shape _) = Replicate shape $ Constant $ blankPrimValue t
adBlank (Array (ElemAcc _) _ _) = error "adBlank: cannot create array of accumulators YET (?)"
adBlank Acc {} = error "adBlank: cannot create blank accumulator"
adBlank Mem {} = error "adBlank: cannot create blank memory"

adOne :: Type -> BasicOp
adOne (Prim pt) = SubExp $ Constant $ onePrimValue pt
adOne (Array (ElemPrim t) shape _) = Replicate shape $ Constant $ onePrimValue t
adOne (Array (ElemAcc _) _ _) = error "adOne: cannot create array of accumulators YET (?)"
adOne Acc {} = error "adOne: cannot create blank accumulator"
adOne Mem {} = error "adOne: cannot create blank memory"

getPrimElemType :: Type -> PrimType
getPrimElemType (Prim ptp) = ptp
getPrimElemType (Array (ElemPrim ptp) _ _) = ptp
getPrimElemType _ = error "In Rev.hs, getPrimElemType: unsupported input type!"

-- | Turn the conversion the other way around.  Note that most
-- conversions are lossy, so there is no guarantee the value will
-- round-trip.
flipConvOp :: ConvOp -> ConvOp
flipConvOp (ZExt from to) = ZExt to from
flipConvOp (SExt from to) = SExt to from
flipConvOp (FPConv from to) = FPConv to from
flipConvOp (FPToUI from to) = UIToFP to from
flipConvOp (FPToSI from to) = SIToFP to from
flipConvOp (UIToFP from to) = FPToSI to from
flipConvOp (SIToFP from to) = FPToSI to from
flipConvOp (IToB from) = BToI from
flipConvOp (BToI to) = IToB to

------------------------
--- Helpers for Scan ---
------------------------
liftPExp2MapNest :: [PrimExp VName] -> M.Map VName (VName,Type) -> ADM ([VName], Stms SOACS)
liftPExp2MapNest pexps tab
  | not (null pexps),
    not (M.null tab),
    tp_cand <- snd (head (M.elems tab)),
    depth <- arrayRank tp_cand,
    fvts <- foldl S.union S.empty (map leafExpTypes pexps),
    fvs  <- map fst (S.toList fvts),
    all (isValidPrimType depth) fvts,
    arr_arg_tps <- mapMaybe (`M.lookup` tab) fvs,
    (length arr_arg_tps == length fvs) = do
      genMapNest fvs depth (map (uncurry Param) arr_arg_tps)
    where
      isValidPrimType d (v,pt) =
        case M.lookup v tab of
          Just (_, t) -> isPrimTypeAtDepthOf d pt t
          Nothing -> False
      isPrimTypeAtDepthOf d t (Prim t') =
        t==t' && d == 0
      isPrimTypeAtDepthOf d t (Array (ElemPrim t') shape _) =
        t == t' && d == length (shapeDims shape)
      isPrimTypeAtDepthOf _ _ _ = False
      genMapNest :: [VName] -> Int -> [Param Type] -> ADM ([VName], Stms SOACS)
      genMapNest fvs 0 scal_ps = do
        runBinderT' . localScope (scopeOfLParams scal_ps) $ do
          forM_ (zip fvs scal_ps) $ \(v,p) -> do
            letBindNames [v] $ BasicOp $ SubExp $ Var $ paramName p
          forM pexps $ \pe -> letExp "r" =<< toExp pe
      genMapNest fvs d arr_arg_tps = do
        -- create names and type bindings for lambda args
        let (arr_nms, arr_tps) = (map paramName arr_arg_tps, map paramDec arr_arg_tps)
        let l_ts = map (stripArray 1) arr_tps
        l_ps <- mapM (newParam "lam_arg") l_ts
        (rs, stms) <- genMapNest fvs (d-1) l_ps
        let lam_body = mkBody stms $ map Var rs
            arr_tp = head arr_tps
            lam_rtps = map (mkRetTp arr_tp . primExpType) pexps
            lam = Lambda l_ps lam_body lam_rtps
            n = arraySize 0 arr_tp
        runBinderT' . localScope (scopeOfLParams l_ps) $
            letTupExp "map_res" $ Op (Screma n (ScremaForm [] [] lam) arr_nms)
        where
          mkRetTp (Array (ElemPrim _) shape u) ptp =
            stripArray 1 $ Array (ElemPrim ptp) shape u
          mkRetTp _ _  = error "unreachable case reached!"
liftPExp2MapNest _ _ = error "In Rev.hs, function liftPExp2MapNest: unreachable case reached!"

-- computes `d(x op y)/dx` when keep_first=true, 
-- and `d(x op y)/dy otherwise. 
-- `op` is given as `lam`
mkScanAdjointLam :: Lambda -> Bool -> ADM Lambda
mkScanAdjointLam lam0 keep_first = do
  let len = length $ lambdaReturnType lam0
  lam <- renameLambda lam0
  let p2diff  = if keep_first
                then take len $ lambdaParams lam
                else drop len $ lambdaParams lam
  p_adjs <- mapM (\ tp -> newParam "elem_adj" tp) (lambdaReturnType lam)
  lam' <- localScope (scopeOfLParams p_adjs) $
    diffLambda (map paramName p_adjs) (map paramName p2diff) lam
  stms' <- runBinderT'_  $
          mapM_ (\ p -> letBindNames [paramName p] $ BasicOp $ adOne $ paramDec p) p_adjs
  let lam_bdy' = lambdaBody lam'
      lam'' = lam' { lambdaBody = lam_bdy' { bodyStms = stms' <> bodyStms lam_bdy' } }
  return lam''

-- Should generate something like:
-- `\ j -> let i = n - 1 - j
--         if i < n-1 then ( ys_adj[i], df2dx ys[i] xs[i+1]) else (0,1) )`
-- where `ys` is  the result of scan
--       `xs` is  the input  of scan
--       `ys_adj` is the known adjoint of ys
--       `j` draw values from `iota n`
mkScanFusedMapLam :: SubExp -> Lambda -> [VName] -> [VName] -> [VName] -> ADM Lambda
mkScanFusedMapLam n scn_lam xs ys ys_adj = do
  lam <- mkScanAdjointLam scn_lam True
  ystp  <- mapM lookupType ys
  let rtp = lambdaReturnType lam
  let lam_arg = map paramName $ lambdaParams lam
  let (lam_as, lam_bs) = splitAt (length rtp) lam_arg
  par_i <- newParam "i" $ Prim int64
  let i = paramName par_i
  let pars  = zipWith Param ys_adj ystp
  body <- runBodyBinder . localScope (scopeOfLParams (par_i:pars)) $
      eBody
        [ eIf ( toExp $ le64 i .>. pe64 (intConst Int64 0) )
              ( do
                  (nms,stms) <- runBinderT' . localScope (scopeOfLParams (par_i:pars)) $ do
                    j  <- letSubExp "j" =<< toExp (pe64 n - (le64 i + pe64 (intConst Int64 1)))
                    j1 <- letSubExp "j1"=<< toExp (pe64 n - le64 i)
                    y_s<- forM (zip (zip3 ys xs ys_adj) (zip3 lam_as lam_bs rtp)) $
                            \((y, x, y_), (a, b, t)) -> do
                              yj <- letSubExp (baseString y ++ "_elem") $ BasicOp $ Index y $ DimFix j : fullSlice t []
                              y_j<- letSubExp (baseString y_++ "_elem") $ BasicOp $ Index y_$ DimFix j : fullSlice t []
                              xj <- letSubExp (baseString x ++ "_elem") $ BasicOp $ Index x $ DimFix j1: fullSlice t []
                              letBindNames [a] $ BasicOp $ SubExp yj
                              letBindNames [b] $ BasicOp $ SubExp xj
                              return y_j
                    lam_rs <- bodyBind $ lambdaBody lam
                    return $ interleave $ zip y_s lam_rs
                  addStms stms
                  resultBodyM nms
              )
              ( do
                  (rs, stms) <- runBinderT' $ do
                    zs <- mapM (letSubExp "ct_zero" . BasicOp . adBlank) rtp
                    os <- mapM (letSubExp "ct_one"  . BasicOp . adOne  ) rtp
                    return $ interleave $ zip zs os
                  addStms stms
                  resultBodyM rs
              )
        ]
  return $ Lambda [par_i] body (interleave $ zip rtp rtp)
  where
    interleave [] = []
    interleave ((a,b):l) = a:b:interleave l

-- let lin_o (a1: real, b1: real) (a2:real, b2: real) = (a2 \bar{+} b2 \bar{*} a1, b1 \bar{*} b2)
mkScanLinFunO :: Type -> ADM (Scan SOACS)
mkScanLinFunO tp = do
  let ptp = getPrimElemType tp
      (zero, one) = (Constant $ blankPrimValue ptp, Constant $ onePrimValue ptp)   
  tmp <- mapM newVName ["a1", "b1", "a2", "b2"]
  arr_nms <- mapM newVName ["a1s", "b1s", "a2s", "b2s"]
  let [a1, b1, a2, b2] = tmp
      ps = map (`Param` tp) arr_nms
      -- lift scalar computation `a2 + b2*a1, b1*b2` to a map nest
      (pet, plus, mul) = (primExpFromSubExp ptp . Var, getBinOpPlus ptp, getBinOpMul ptp)
      pexps = [ BinOpExp plus (pet a2) $ BinOpExp mul (pet b2) (pet a1)
              , BinOpExp mul (pet b1) (pet b2)
              ]
      tab = M.fromList $ zipWith (\ v vs -> (v, (vs, tp))) tmp arr_nms
  (rs, stms) <- liftPExp2MapNest pexps tab
  let body = mkBody stms $ map Var rs
      lam  = Lambda ps body [tp,tp]
  return $ Scan lam [zero, one]

-- build the map following the scan with linear-function-composition:
-- for each (ds,cs) length-n array results of the scan, combine them as:
--    `let rs = map2 (\ d_i c_i -> d_i + c_i * y_adj[n-1]) d c |> reverse`
-- but insert explicit indexing to reverse inside the map.
mkScan2ndMaps :: SubExp -> (Type, VName, (VName, VName)) -> ADM (VName, Stms SOACS)
mkScan2ndMaps n (arr_tp, y_adj, (ds,cs)) = do
  let ptp  = getPrimElemType arr_tp
      eltp = stripArray 1 arr_tp
  par_i <- newParam "i" $ Prim int64
  let i = paramName par_i
  let pars = zipWith Param [y_adj, ds, cs] (replicate 3 arr_tp)
  (nms, bdy_stms_1) <- runBinderT' . localScope (scopeOfLParams (par_i:pars)) $ do
      -- ys_adj_last = ys_adj[n-1]  will be hoisted outside
      nm1 <- letSubExp "nm1" =<< toExp (pe64 n + pe64 (intConst Int64 (-1)))
      y_adj_last <- letExp (baseString y_adj ++ "_last") $ BasicOp $ Index y_adj $ fullSlice arr_tp [DimFix nm1]
      j  <- letSubExp "j" =<< toExp (pe64 n - (le64 i + pe64 (intConst Int64 1)))
      dj <- letExp (baseString ds ++ "_elem") $ BasicOp $ Index ds $ DimFix j : fullSlice eltp []
      cj <- letExp (baseString cs ++ "_elem") $ BasicOp $ Index cs $ DimFix j : fullSlice eltp []
      return [y_adj_last, dj, cj]
  tmp <- mapM newVName ["y_adj_last", "d_j", "c_j"]
  let [y_l, d_j, c_j] = tmp
      (pet, plus, mul) = (primExpFromSubExp ptp . Var, getBinOpPlus ptp, getBinOpMul ptp)
      pexps = [ BinOpExp plus (pet d_j) $ BinOpExp mul (pet c_j) (pet y_l) ]
      tab = M.fromList $ zipWith (\ v vs -> (v, (vs, eltp))) tmp nms
  (rs, bdy_stms_2) <- liftPExp2MapNest pexps tab
  let lam = Lambda [par_i] (mkBody (bdy_stms_1<>bdy_stms_2) $ map Var rs) [eltp]
  (rs_map, map_stms) <- runBinderT' $ do
      iota <- letExp "iota" $ BasicOp $ Iota n (intConst Int64 0) (intConst Int64 1) Int64
      letTupExp "after_scan" $ Op (Screma n (ScremaForm [] [] lam) [iota])
  return (head rs_map, map_stms)
-- perform the final map, which is fusable with the maps obtained from `mkScan2ndMaps`
-- let xs_contribs =
--    map3 (\ i a r -> if i==0 then r else (df2dy (ys[i-1]) a) \bar{*} r
--         ) (iota n) xs rs
mkScanFinalMap :: SubExp -> Lambda -> [VName] -> [VName] -> [VName] -> ADM ([VName], Stms SOACS)
mkScanFinalMap n scn_lam xs ys rs = do
  let eltps = lambdaReturnType scn_lam
  lam <- mkScanAdjointLam scn_lam False
  par_i <- newParam "i" $ Prim int64
  let i = paramName par_i
  par_x <- mapM (\ (x,t) -> newParam (baseString x++"_elem") t) $ zip xs eltps
  par_r <- mapM (\ (r,t) -> newParam (baseString r++"_elem") t) $ zip rs eltps
  -- unfortunately, we need to handle the ` lam_res \bar{*} r` outside the runBinderT'
  let lam_rs0 = bodyResult $ lambdaBody lam
  (lam_rs, cpy_stms) <- runBinderT' . localScope (scopeOfLParams (par_i:par_r++par_x)) $ do
      forM lam_rs0 $ \lam_r0 -> mkVName lam_r0
  tmp_res <- forM (zip3 lam_rs (map paramName par_r) eltps) $ \(lam_r, r, eltp) -> do
      lr0 <- newVName (baseString lam_r ++ "_0_")
      r0  <- newVName (baseString r ++ "_0_")
      let ptp = getPrimElemType eltp
          (pet, mul) = (primExpFromSubExp ptp . Var, getBinOpMul ptp)
          pexps = [ BinOpExp mul (pet lr0) (pet r0) ]
          tab = M.fromList $ zipWith (\ v vs -> (v, (vs, eltp))) [lr0, r0] [lam_r, r]
      (rs_i, stms_i) <- liftPExp2MapNest pexps tab
      return (head rs_i, stms_i)
  let (th_res, stms2_lst) = unzip tmp_res
      stms2 = mconcat stms2_lst
  (cs, lam_stms) <- runBinderT' . localScope (scopeOfLParams (par_i:par_r++par_x)) $
      letTupExp "scan_contribs"
        =<< eIf ( toExp $ le64 i .>. pe64 (intConst Int64 0) )
                ( do  (_,stms_1) <- runBinderT' . localScope (scopeOfLParams (par_i:par_r++par_x)) $ do
                        im1 <- letSubExp "im1" =<< toExp (le64 i - pe64 (intConst Int64 1))
                        ys_im1 <- forM (zip ys eltps) $ \(y,t) -> do
                          letSubExp (baseString y++ "_elem") $ BasicOp $ Index y $ DimFix im1 : fullSlice t []
                        forM_ (zip (ys_im1 ++ map (Var . paramName) par_x) (map paramName (lambdaParams lam))) $
                              \(act,frm) -> letBindNames [frm] $ BasicOp $ SubExp act
                        bodyBind $ lambdaBody lam
                      addStms (stms_1 <> cpy_stms <> stms2)
                      resultBodyM (map Var th_res)
                )
                (resultBodyM $ map (Var . paramName) par_r)
  runBinderT' $ do
      let map_lam = Lambda (par_i:par_x++par_r) (mkBody lam_stms (map Var cs)) eltps
      iota <- letExp "iota" $ BasicOp $ Iota n (intConst Int64 0) (intConst Int64 1) Int64
      letTupExp "scan_contribs" $ Op (Screma n (ScremaForm [] [] map_lam) (iota:xs++rs))
  where
    mkVName (Var v) = return v
    mkVName ct = letExp "ct" $ BasicOp $ SubExp ct

-------------------------
-- Core of Reverse AD --
------------------------

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

newAdj :: VName -> ADM VName
newAdj v = do
  v_adj <- adjVName v
  t <- lookupType v
  let update = M.singleton v v_adj
  modify $ \env -> env {stateAdjs = update `M.union` stateAdjs env}
  letBindNames [v_adj] $ zeroExp t
  pure v_adj

insAdj :: VName -> VName -> ADM ()
insAdj v v_adj = modify $ \env ->
  env {stateAdjs = M.insert v v_adj $ stateAdjs env}

insAdjMap :: M.Map VName VName -> ADM ()
insAdjMap update = modify $ \env ->
  env {stateAdjs = update `M.union` stateAdjs env}

class Adjoint a where
  lookupAdj :: a -> ADM VName
  updateAdjoint :: a -> VName -> ADM VName
  updateAdjointSlice :: Slice SubExp -> a -> VName -> ADM VName

addBinOp :: PrimType -> BinOp
addBinOp (IntType it) = Add it OverflowWrap
addBinOp (FloatType ft) = FAdd ft
addBinOp Bool = LogAnd
addBinOp Cert = LogAnd

-- Construct a lambda for adding two values of the given type.
addLambda :: Type -> ADM Lambda
addLambda (Prim pt) = binOpLambda (addBinOp pt) pt
addLambda (Array (ElemPrim t) (Shape (s : ss)) u) = do
  xs <- newVName "xs"
  ys <- newVName "ys"
  let t' = Array (ElemPrim t) (Shape ss) u
  lam <- addLambda t'
  body <- insertStmsM $ do
    res <- letSubExp "lam_map" $ Op $ Screma s (mapSOAC lam) [xs, ys]
    return $ resultBody [res]
  pure
    Lambda
      { lambdaParams = [Param xs t', Param ys t'],
        lambdaReturnType = [t'],
        lambdaBody = body
      }
addLambda t =
  error $ "addLambda: " ++ pretty t

instance Adjoint VName where
  lookupAdj v = do
    maybeAdj <- gets $ M.lookup v . stateAdjs
    case maybeAdj of
      Nothing -> newAdj v
      Just v_adj -> return v_adj

  updateAdjoint v d = do
    maybeAdj <- gets $ M.lookup v . stateAdjs
    case maybeAdj of
      Nothing -> setAdjoint v (BasicOp . SubExp . Var $ d)
      Just v_adj -> do
        t <- lookupType v
        v_adj' <- letExp "adj" $
          case t of
            Prim pt ->
              BasicOp $ BinOp (addBinOp pt) (Var v_adj) (Var d)
            _ ->
              error $ "updateAdjoint: unexpected type " <> pretty t
        let update = M.singleton v v_adj'
        insAdjMap update
        pure v_adj'

  updateAdjointSlice slice v d = do
    maybeAdj <- gets $ M.lookup v . stateAdjs
    t <- lookupType v
    case maybeAdj of
      Nothing -> do
        void $ lookupAdj v -- Initialise adjoint.
        updateAdjointSlice slice v d
      Just v_adj -> do
        v_adjslice <-
          if primType t
            then return v_adj
            else letExp (baseString v ++ "_slice") $ BasicOp $ Index v_adj slice
        t' <- lookupType v_adjslice
        v_adjslice' <- addArrays t' v_adjslice d
        v_adj' <- letInPlace "updated_adj" v_adj slice v_adjslice'
        insAdjMap $ M.singleton v v_adj'
        pure v_adj'
    where
      addArrays t xs ys =
        case t of
          Prim pt -> return $ BasicOp $ BinOp (addBinOp pt) (Var xs) (Var ys)
          Array {} -> do
            lam <- addLambda $ stripArray 1 t
            return $ Op $ Screma (arraySize 0 t) (mapSOAC lam) [xs, ys]
          _ ->
            error $ "addArrays: " ++ pretty t

instance Adjoint SubExp where
  lookupAdj (Constant c) =
    letExp "const_adj" $
      BasicOp $ SubExp $ Constant $ blankPrimValue $ primValueType c
  lookupAdj (Var v) = lookupAdj v

  updateAdjoint se@Constant {} _ = lookupAdj se
  updateAdjoint (Var v) d = updateAdjoint v d

  updateAdjointSlice _ se@Constant {} _ = lookupAdj se
  updateAdjointSlice slice (Var v) d = updateAdjointSlice slice v d

setAdjoint :: VName -> Exp -> ADM VName
setAdjoint v e = do
  v_adj <- adjVName v
  letBindNames [v_adj] e
  let update = M.singleton v v_adj
  insAdjMap update
  return v_adj

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
            void $ updateAdjoint x contrib
            void $ updateAdjoint y contrib

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
      void $ updateAdjoint x contrib
    --
    UnOp op x -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m

      let t = unOpType op
      contrib <- do
        let x_pe = primExpFromSubExp t x
            pat_adj' = primExpFromSubExp t (Var pat_adj)
            dx = pdUnOp op x_pe
        letExp "contrib" <=< toExp $ pat_adj' ~*~ dx

      void $ updateAdjoint x contrib
    --
    BinOp op x y -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m

      let t = binOpType op
          (wrt_x, wrt_y) =
            pdBinOp op (primExpFromSubExp t x) (primExpFromSubExp t y)

          pat_adj' = primExpFromSubExp t $ Var pat_adj

      adj_x <- letExp "adj" <=< toExp $ pat_adj' ~*~ wrt_x
      adj_y <- letExp "adj" <=< toExp $ pat_adj' ~*~ wrt_y
      void $ updateAdjoint x adj_x
      void $ updateAdjoint y adj_y
    --
    SubExp se -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      void $ updateAdjoint se pat_adj
    --
    Assert {} ->
      void $ commonBasicOp pat aux e m
    --
    ArrayLit elems t -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      forM_ (zip [(0 :: Int64) ..] elems) $ \(i, se) -> do
        let slice = fullSlice t [DimFix (constant i)]
        updateAdjoint se <=< letExp "elem_adj" $ BasicOp $ Index pat_adj slice
    --
    Index arr slice -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      void $ updateAdjointSlice slice arr pat_adj
    --
    Opaque se -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      void $ updateAdjoint se pat_adj
    --
    Reshape _ arr -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      arr_dims <- arrayDims <$> lookupType arr
      void $
        updateAdjoint arr <=< letExp "adj_reshape" $
          BasicOp $ Reshape (map DimNew arr_dims) pat_adj
    --
    Rearrange perm arr -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      void $
        updateAdjoint arr <=< letExp "adj_rearrange" $
          BasicOp $ Rearrange (rearrangeInverse perm) pat_adj
    --
    Rotate rots arr -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      let neg = BasicOp . BinOp (Sub Int64 OverflowWrap) (intConst Int64 0)
      rots' <- mapM (letSubExp "rot_neg" . neg) rots
      void $
        updateAdjoint arr <=< letExp "adj_rotate" $
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
        updateAdjoint x
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

      zipWithM_ updateAdjoint (arr : arrs) slices
    --
    Copy se -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      void $ updateAdjoint se pat_adj
    --
    Manifest _ se -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      void $ updateAdjoint se pat_adj
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
        updateAdjoint n
          =<< letExp "iota_contrib" (Op $ Screma n reduce [pat_adj])
    --
    Update {} -> error "Reverse-mode Update not handled yet."
    UnAcc {} -> error "Reverse-mode UnAcc not handled yet."
    UpdateAcc {} -> error "Reverse-mode UpdateAcc not handled yet."

diffSOAC :: Pattern -> StmAux () -> SOAC SOACS -> ADM () -> ADM ()
-- special case: reduce (+)
diffSOAC pat@(Pattern [] [pe]) aux soac@(Screma _ form [as]) m
  | Just [red] <- isReduceSOAC form,
    lam <- redLambda red,
    isRedPlus lam = do
    addStm $ Let pat aux $ Op soac
    m
    pe_adj <- lookupAdj $ patElemName pe
    as_tp <- lookupType as
    v_rep <- letTupExp "v_rep" $ BasicOp $ Replicate (shpFstDim $ arrayShape as_tp) $ Var pe_adj
    zipWithM_ updateAdjoint [as] v_rep
  where
    shpFstDim (Shape []) = error "error in shpFstDim in Rev.hs"
    shpFstDim (Shape (d:_)) = Shape [d]
--
-- Differentiating: let y = reduce \odot ne xs
-- is implemented as:
-- ls = scan_exc \odot  ne xs
-- rs = scan_exc \odot' ne (reverse xs)
-- xs_c = map3 (f_bar y_bar) ls as (reverse rs)
-- where
--   x \odot' y = y \odot x
--   y_bar is the adjoint of the result y
--   f l_i x_i r_i = l_i \odot x_i \odot r_i
--   f_bar = the reverse diff of f with respect to x_i under the adjoint y_bar
-- The plan is to create
--   one scanomap SOAC which computes ls and rs
--   another map which computes xs_c
diffSOAC pat@(Pattern [] pes) aux soac@(Screma n form xs) m
  | Just [red] <- isReduceSOAC form = do
    addStm $ Let pat aux $ Op soac
    m
    ys_adj <- mapM (lookupAdj . patElemName) pes
    let (lam0, ne) = (redLambda red, redNeutral red)
    let alphas = lambdaReturnType lam0
    lam   <- renameLambda lam0
    lam'  <- renameLambda lam0 { lambdaParams = flipParams $ lambdaParams lam0 }
    par_i <- newParam "i" $ Prim int64
    -- phase1: build the scanomap soac
    g_bdy <- mkFusedMapBody par_i ne alphas
    let g_lam = Lambda [par_i] g_bdy (alphas++alphas)
    (r_scan, scan_stms) <- runBinderT' $ do
        iota <- letExp "iota" $ BasicOp $ Iota n (intConst Int64 0) (intConst Int64 1) Int64
        letTupExp "adj_ctrb_scan" $ Op (Screma n (ScremaForm [Scan lam ne, Scan lam' ne] [] g_lam) [iota])
    addStms scan_stms
    let (ls_arr, rs_arr) = splitAt (length ne) r_scan
    -- phase2: build the following map:
    (as_params, f) <- mkF $ redLambda red
    f_rev  <- diffLambda ys_adj as_params f
    par_i' <- newParam "i" $ Prim int64
    let i' = paramName par_i'
        (ls_as_ps, rs_ps) = splitAt (2*length ne) $ lambdaParams f_rev
        scp_params = par_i':ls_as_ps++zipWith Param ys_adj alphas
    f_adj_body <- runBodyBinder . localScope (scopeOfLParams scp_params) $ do
        nmim1 <- letSubExp "n_i_1" =<< toExp (pe64 n - (le64 i' + pe64 (intConst Int64 1)))
        let idx_stms = map (mkIdxStm nmim1) $ zip3 alphas rs_arr $ map paramName rs_ps
        addStms (stmsFromList idx_stms)
        pure $ lambdaBody f_rev
    let map_lam = Lambda (par_i':ls_as_ps) f_adj_body alphas
    (r_map, map_stms) <- runBinderT' $ do
        iota <- letExp "iota" $ BasicOp $ Iota n (intConst Int64 0) (intConst Int64 1) Int64
        letTupExp "adj_ctrb_scan" $ Op (Screma n (ScremaForm [] [] map_lam) (iota:ls_arr++xs))
    addStms map_stms
    -- finally add contributions to the adjoint of xs
    zipWithM_ updateAdjoint xs r_map
  where
    flipParams ps = uncurry (flip (++)) $ splitAt (length ps `div` 2) ps
    mkFusedMapBody par_i ne alphas = do
      let i = paramName par_i
      runBodyBinder . localScope (scopeOfLParams [par_i]) $
        eBody
          [ eIf ( toExp $ le64 i .>. pe64 (intConst Int64 0) )
                ( do
                    ((rs1,rs2), stms) <- runBinderT' . localScope (scopeOfLParams [par_i]) $ do
                      im1 <- letSubExp "i_1" =<< toExp (le64 i - pe64 (intConst Int64 1))
                      nmi <- letSubExp "n_i" =<< toExp (pe64 n - le64 i)
                      tmp <- forM (zip xs alphas) $ \(x,t) -> do
                                x1 <- letSubExp (baseString x ++ "_elem_1") $ BasicOp $ Index x $ DimFix im1 : fullSlice t []
                                x2 <- letSubExp (baseString x ++ "_elem_2") $ BasicOp $ Index x $ DimFix nmi : fullSlice t []
                                return (x1,x2)
                      return (unzip tmp)
                    addStms stms
                    resultBodyM (rs1 ++ rs2)
                )
                ( resultBodyM (ne ++ ne) )
          ]
    mkIdxStm idx (t,r_arr,r) =
      mkLet [] [Ident r t] $ BasicOp $ Index r_arr $ DimFix idx : fullSlice t []
    mkF lam = do
      lam_l <- renameLambda lam
      lam_r <- renameLambda lam
      let w = length $ lambdaReturnType lam
          (lps, aps) = splitAt w $ lambdaParams lam_l
          (ips, rps) = splitAt w $ lambdaParams lam_r
      body <- runBodyBinder $
        localScope (scopeOfLParams $ lps <> aps <> rps) $ do
          lam_l_res <- bodyBind $ lambdaBody lam_l
          forM_ (zip ips lam_l_res) $ \(ip, se) ->
            letBindNames [paramName ip] $ BasicOp $ SubExp se
          pure $ lambdaBody lam_r
      pure ( map paramName aps , Lambda (lps <> aps <> rps) body $ lambdaReturnType lam )
-- Troels reduce
diffSOAC pat aux soac@(Screma w form as) m
  | Just red <- singleReduce <$> isReduceSOAC form = do
    addStm $ Let pat aux $ Op soac
    m
    pat_adj <- mapM lookupAdj $ patternNames pat
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

    zipWithM_ updateAdjoint as as_adj
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
      body <- runBodyBinder $
        localScope (scopeOfLParams $ lps <> aps <> rps) $ do
          lam_l_res <- bodyBind $ lambdaBody lam_l
          forM_ (zip ips lam_l_res) $ \(ip, se) ->
            letBindNames [paramName ip] $ BasicOp $ SubExp se
          pure $ lambdaBody lam_r
      pure
        ( map paramName aps,
          Lambda (lps <> aps <> rps) body $ lambdaReturnType lam
        )
--
-- only single scan supported for now:  ys = scan odot xs
-- Implementation "should" handle associative operators on tuples and arrays
diffSOAC pat@(Pattern [] pes) aux soac@(Screma n (ScremaForm [scn] [] f) xs) m
  | isIdentityLambda f = do
      addStm $ Let pat aux $ Op soac
      m
      let ys = map patElemName pes
      ys_adj <- mapM lookupAdj ys
      map1_lam <- mkScanFusedMapLam n (scanLambda scn) xs ys ys_adj

      scans_lin_fun_o <- mapM mkScanLinFunO $ lambdaReturnType $ scanLambda scn
      let scp = zip ys_adj $ map (LParamName . patElemDec) pes
      (r_scan, scan_stms) <- runBinderT' $ inScopeOf scp $ do
            iota <- letExp "iota" $ BasicOp $ Iota n (intConst Int64 0) (intConst Int64 1) Int64
            letTupExp "adj_ctrb_scan" $ Op (Screma n (ScremaForm scans_lin_fun_o [] map1_lam) [iota])
      addStms scan_stms
      (red_nms, snd_maps) <- unzip <$> mapM (mkScan2ndMaps n) (zip3 (map patElemDec pes) ys_adj (chunk2 r_scan))
      addStms (mconcat snd_maps) 
      (xs_contribs, fin_map_stms) <- mkScanFinalMap n (scanLambda scn) xs ys red_nms
      addStms fin_map_stms
      inScopeOf scp $ zipWithM_ updateAdjoint xs xs_contribs
    where
      chunk2 [] = []
      chunk2 (a:b:r) = (a,b) : chunk2 r
      chunk2 _ = error "Rev.hs, chunk2: impossible case reached!"
-- a map translates to generalized reduction
diffSOAC pat@(Pattern [] pes) aux soac@(Screma w form as) m
  | Just lam <- isMapSOAC form = do
    addStm $ Let pat aux $ Op soac
    m
    let fvs = namesToList $ freeIn lam
    fvs_adj <- mapM lookupAdj fvs
    fvs_adj_tp <- mapM lookupType fvs_adj
    let fvs_all = zip3 fvs_adj_tp fvs fvs_adj 
        fvs_scal = map (\(x,y,_) -> (x,y)) $ filter (\(t_adj,_,_) -> primType t_adj) fvs_all
        fvs_acc  = filter (\(t_adj,_,_) -> accType  t_adj) fvs_all
        fvs_arr  = filter (\(t_adj,_,_) -> arrType  t_adj) fvs_all
    map_fv_scal <- forM fvs_scal $ \(_, fv) -> do
      let repl_exp = BasicOp $ Replicate (Shape [w]) $ Var fv
      letExp (baseString fv ++ "_adj_repl") repl_exp
    lam_ps_scal <- mapM (newParam "lam_fv_scal") (map fst fvs_scal)
    let lam_scal0 = lam { lambdaParams = lambdaParams lam ++ lam_ps_scal }
        substs = M.fromList $ zip (map snd fvs_scal) $ map paramName lam_ps_scal 
        lam_scal = substituteNames substs lam_scal0

    ys_adj <- mapM lookupAdj $ map patElemName pes
    lam_adj_ps <- mapM (newParam "lam_adj" . rowType . patElemDec) pes
    lam' <- renameLambda lam_scal
    lam_rev <- localScope (scopeOfLParams lam_adj_ps) $
      diffLambda (map paramName lam_adj_ps) (map paramName (lambdaParams lam')) lam'
    let lam_rev' = Lambda (lambdaParams lam' ++ lam_adj_ps) (lambdaBody lam_rev) $
                          map paramType $ lambdaParams lam'
    contribs <- letTupExp "map_adjs" $ Op $ Screma w (mapSOAC lam_rev') $ as ++ map_fv_scal ++ ys_adj
    let (ctrb_pure, ctrb_other0) = splitAt (length as) contribs
        (ctrb_scal0, ctrb_other) = splitAt (length lam_ps_scal) ctrb_other0
    ctrb_scal <- forM (zip fvs_scal ctrb_scal0) $ \((tp,fv),ctrb) -> do --Screma n form xs
      red <- mkReducePlus (prmTypeRep tp)
      id_lam <- mkIdentityLambda [tp]
      let soac_exp = Op $ Screma w (ScremaForm [] [red] id_lam) $ [ctrb]
      res <- letTupExp (baseString fv++"_adj_sum") soac_exp
      trace ("\nidlam:\n "++pretty soac_exp++"\n\n") $ return $ head res
    zipWithM_ updateAdjoint (map snd fvs_scal) ctrb_scal
    trace ("Lam':\n" ++ pretty lam' ++ "\nLam_rev:\n"++pretty lam_rev'++"\n fv scal: "++pretty fvs_scal++" fvs_arr: "++pretty fvs_arr++" fvs acc: "++pretty fvs_acc) $
      zipWithM_ updateAdjoint as ctrb_pure
    where
      accType (Acc arrs) = True
      accType _ = False
      arrType (Array (ElemPrim _) _ _) = True
      arrType _ = False
      accTypeRep (Acc arrs) = arrs
      accTypeRep _ = error "ERROR 1111!!!"
      arrTypeRep (Array (ElemPrim ptp) shape u) = (ptp, shape, u)
      arrTypeRep _ = error "ERROR 2222!!!"
      prmTypeRep (Prim ptp) = ptp
      prmTypeRep _ = error "ERROR 3333!!!"
      mkReducePlus ptp = do
        let tp = Prim ptp
        lam_ps <- zipWithM newParam ["a", "b"] [tp, tp]
        let [a,b] = map paramName lam_ps
        lam_rs <- newVName "r"
        let lam_stm = mkLet [] [Ident lam_rs tp] $ BasicOp $ BinOp (getBinOpPlus ptp) (Var a) (Var b) 
            lam = Lambda lam_ps (mkBody (oneStm lam_stm) [Var lam_rs]) [Prim ptp]
        trace ("redplus: "++pretty lam) $ return $ Reduce Commutative lam [Constant $ blankPrimValue ptp]
-- everything else unsuported
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

    zipWithM_ updateAdjoint (map fst args) contribs
diffStm stm@(Let pat _ (If cond tbody fbody _)) m = do
  addStm stm
  m

  let tbody_free = freeIn tbody
      fbody_free = freeIn fbody
      branches_free = namesToList $ tbody_free <> fbody_free

  adjs <- mapM lookupAdj $ patternValueNames pat

  -- We need to discard any context, as this never contributes to
  -- adjoints.
  contribs <-
    (pure . takeLast (length branches_free) <=< letTupExp "branch_contrib" <=< renameExp)
      =<< eIf
        (eSubExp cond)
        (diffBody adjs branches_free tbody)
        (diffBody adjs branches_free fbody)

  zipWithM_ updateAdjoint branches_free contribs
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
  modify $ \s -> s {stateAdjs = mempty}
  x <- m
  modify $ \s -> s {stateAdjs = old_state_adjs}
  pure x

diffBody :: [VName] -> [VName] -> Body -> ADM Body
diffBody res_adjs get_adjs_for (Body () stms res) = subAD $ do
  let onResult (Constant _) _ = pure ()
      onResult (Var v) v_adj = insAdj v v_adj
  zipWithM_ onResult (takeLast (length res_adjs) res) res_adjs
  (adjs, stms') <- collectStms $ do
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
