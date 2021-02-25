{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.AD.Rev (revVJP) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifunctor (second)
import Data.List (partition, sortOn, (\\))
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace
import Futhark.AD.Derivatives
import Futhark.Analysis.PrimExp.Convert
import Futhark.Binder
import Futhark.Construct
import Futhark.IR.SOACS
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util (splitAt3)

----------------------
-- Helper functions --
----------------------

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
  (lam, _, _) <- revLambda =<< renameLambda lam0
  let (lam_ps, p_adjs) = splitAt (2 * len) $ lambdaParams lam
  stms <- runBinderT'_ . localScope (scopeOfLParams lam_ps) $ do
          mapM_ (\ p -> letBindNames [paramName p] $ BasicOp $ adOne $ paramDec p) p_adjs
  let lam_bdy = lambdaBody lam
      lam_bdy'= lam_bdy { bodyStms = stms <> bodyStms lam_bdy }
      (bdy_res', lam_ret') =
          if keep_first
          then (take len $ bodyResult lam_bdy, take len $ lambdaReturnType lam)
          else (drop len $ bodyResult lam_bdy, drop len $ lambdaReturnType lam)
  return $ lam { lambdaParams = lam_ps, lambdaReturnType = lam_ret', lambdaBody = lam_bdy' { bodyResult = bdy_res' } }

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
--------------------------
--- Helpers for Reduce ---
--------------------------

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

revReduce2Map :: Lambda -> [VName] -> ADM Lambda
revReduce2Map lam res_adj = do
  let bdy_lam = lambdaBody lam
  let arg_nms_lam = map paramName $ lambdaParams lam
  let lam_ret_tps = lambdaReturnType lam
  let red_arg_len = length arg_nms_lam `div` 2
  let fst_arg_nms = take red_arg_len arg_nms_lam
  let snd_arg_nms = drop red_arg_len arg_nms_lam
  lft_nms <- mapM (newVNameStr "lft_red_arg") [1 .. red_arg_len]
  rht_nms <- mapM (newVNameStr "rht_red_arg") [1 .. red_arg_len]
  dif_nms <- mapM (newVNameStr "dif_red_arg") [1 .. red_arg_len]
  tmp_nms <- mapM (newVNameStr "tmp_red_arg") [1 .. red_arg_len]
  -- make the body corresponding to (lambda lft_nms dif_nms -> ... in res)
  let subs1 = M.fromList $ zip fst_arg_nms lft_nms ++ zip snd_arg_nms dif_nms
  bdy_lam_1 <- renameBody $ substituteNames subs1 bdy_lam
  let res_bdy_1 = bodyResult bdy_lam_1
  -- insert copy statements "let tmp_nms = res"
  let ini_stms =
        foldl
          (\acc (nm, tp, ret) -> acc <> oneStm (mkLet [] [Ident nm tp] (BasicOp $ SubExp ret)))
          mempty
          (zip3 tmp_nms lam_ret_tps res_bdy_1)
  -- make the body corresponding to (lambda tmp_nms rht_nms -> ...)
  let subs2 = M.fromList $ zip fst_arg_nms tmp_nms ++ zip snd_arg_nms rht_nms
  bdy_lam_2 <- renameBody $ substituteNames subs2 bdy_lam
  -- create the body corresponding to (lft_nms \odot dif_nms \odot rht_nms),
  -- where \odot is assumed to be the reduce operator
  let fin_body_stms = bodyStms bdy_lam_1 <> ini_stms <> bodyStms bdy_lam_2
  let fin_body_ret = bodyResult bdy_lam_2
  let fin_body = mkBody fin_body_stms fin_body_ret
  -- differentiate the body while linking the adjoint of the result
  let tp_scope = map LParamName lam_ret_tps
  inScopeOf (zip res_adj tp_scope) $
    mapM_ (uncurry insAdj) $ mapMaybe nameSE $ zip fin_body_ret res_adj
  let args_scope = zip res_adj tp_scope ++ zip lft_nms tp_scope ++ zip rht_nms tp_scope ++ zip dif_nms tp_scope
  (_, fwd_bdy, rev_bdy) <- inScopeOf args_scope $ revBody fin_body
  -- get the adjoint results of the differentiated body
  (adj_dif_nms, empties1, empties2) <- unzip3 <$> mapM lookupAdj dif_nms
  if all M.null empties1 && all null empties2
    then do
      -- create the lambda
      let dif_lam_stms = bodyStms fwd_bdy <> bodyStms rev_bdy
          dif_lam_res = map Var adj_dif_nms
          dif_lam_bdy = mkBody dif_lam_stms dif_lam_res
          dif_lam_par =
            zipWith Param lft_nms lam_ret_tps
              ++ zipWith Param rht_nms lam_ret_tps
              ++ zipWith Param dif_nms lam_ret_tps
      return $ Lambda dif_lam_par dif_lam_bdy lam_ret_tps
    else error "Impossible case reached in revReduce2Map!"
  where
    newVNameStr :: String -> Int -> ADM VName
    newVNameStr str i = newVName (str ++ pretty i ++ "_")
    nameSE :: (SubExp, VName) -> Maybe (VName, VName)
    nameSE (Var nm1, nm2) = Just (nm1, nm2)
    nameSE _ = Nothing

mkScanExc :: SubExp -> Lambda -> [(SubExp, VName)] -> ADM ([VName], Stms SOACS)
mkScanExc n red_op ne_arrs = do
  let (red_nes, arrs) = unzip ne_arrs
  let lam_ret_tps = lambdaReturnType red_op
  -- create the statements of the mapped lambda:
  i_nm <- newVName "ind"
  let lam_par = Param i_nm $ Prim $ IntType Int64
  (lam_res, lam_stms) <-
    runBinderT' $ do
      im1 <- letSubExp "ind_m1" =<< toExp (le64 i_nm + pe64 (intConst Int64 (-1)))
      letTupExp "res_elem"
        =<< eIf
          (toExp $ pe64 im1 .>=. pe64 (intConst Int64 0))
          ( do
              then_info <- forM (zip arrs lam_ret_tps) $ \(arr_nm, res_tp) -> do
                nm <- newVName (baseString arr_nm ++ "_elem")
                let slc = DimFix im1 : fullSlice res_tp []
                let stm = mkLet [] [Ident nm res_tp] $ BasicOp $ Index arr_nm slc
                return (nm, stm)
              let (then_nms, then_stms) = unzip then_info
              addStms $ stmsFromList then_stms
              resultBodyM $ map Var then_nms
          )
          (eBody $ map (pure . BasicOp . SubExp) red_nes)
  -- create the scanomap stm:
  (nms, stms) <- runBinderT' $ do
    iota_se <- letSubExp "iota_arr" $ BasicOp $ Iota n (intConst Int64 0) (intConst Int64 1) Int64
    let lam_body = mkBody lam_stms $ map Var lam_res
    let f_lam = Lambda [lam_par] lam_body lam_ret_tps
    -- f_lam <- mkIdentityLambda $ lambdaReturnType red_op
    case iota_se of
      Var iota_nm ->
        letTupExp "scaned_arrays" $ Op (Screma n (ScremaForm [Scan red_op red_nes] [] f_lam) [iota_nm])
      _ -> error "In mkScanExc function of Rev.hs file: a iota should be held in a new variable!"
  return (nms, stms)

mkReverse :: SubExp -> [(VName, Type)] -> ADM ([VName], Stms SOACS)
mkReverse n arrtps = do
  let (arrs, tps) = unzip arrtps
  runBinderT' $ do
    nm1 <- letExp "n_min_1" =<< toExp (le64 n + le64 (intConst Int64 (-1)))
    iota_se <- letSubExp "iota_arr" $ BasicOp $ Iota n (Var nm1) (intConst Int64 (-1)) Int64
    i_nm <- newVName "ind"
    let lam_par = Param i_nm $ Prim $ IntType Int64
    let lam_ret_tps = map (stripArray 1) tps
    lam_res_stms <- forM (zip arrs lam_ret_tps) $ \(arr_nm, res_tp) -> do
      nm <- newVName (baseString arr_nm ++ "_elem")
      let slc = DimFix (Var i_nm) : fullSlice res_tp []
      let stm = mkLet [] [Ident nm res_tp] $ BasicOp $ Index arr_nm slc
      return (nm, stm)
    let (lam_res, lam_stms_lst) = unzip lam_res_stms
    let lam_stms = stmsFromList lam_stms_lst
    let lam_body = mkBody lam_stms $ map Var lam_res
    let lam = Lambda [lam_par] lam_body lam_ret_tps
    case iota_se of
      Var iota_nm ->
        letTupExp "rev_arrays" $ Op (Screma n (ScremaForm [] [] lam) [iota_nm])
      _ -> error "In mkReverse function of Rev.hs file: a iota should be held in a new variable!"

------------------------
-- Core of Reverse AD --
------------------------

data Env = Env
  { adjs :: M.Map VName VName,
    tape :: M.Map VName VName,
    vns :: VNameSource
  }

data REnv = REnv
  { tans :: M.Map VName VName,
    envScope :: Scope SOACS
  }

newtype ADM a = ADM (ReaderT REnv (State Env) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader REnv,
      MonadState Env,
      MonadFreshNames
    )

instance MonadFreshNames (State Env) where
  getNameSource = gets vns
  putNameSource vns' = modify (\env -> env {vns = vns'})

instance HasScope SOACS ADM where
  askScope = asks envScope

instance LocalScope SOACS ADM where
  localScope scope = local $ \env -> env {envScope = scope <> envScope env}

runADM :: MonadFreshNames m => ADM a -> REnv -> m a
runADM (ADM m) renv =
  modifyNameSource $ \vn -> second vns $ runState (runReaderT m renv) (Env mempty mempty vn)

adjVName :: VName -> ADM VName
adjVName v = newVName (baseString v <> "_adj")

newAdj :: VName -> ADM (VName, M.Map VName VName, Stms SOACS)
newAdj v = do
  v_adj <- adjVName v
  t <- lookupType v
  let update = M.singleton v v_adj
  modify $ \env -> env {adjs = update `M.union` adjs env}
  -- Cosmin commented/changed since does not zero out arrays!
  stms <- runBinder_ $ letBindNames [v_adj] (BasicOp $ adBlank t)
  return (v_adj, update, stms)

accVName :: VName -> ADM VName
accVName v = newVName (baseString v <> "_acc")

insTape :: VName -> VName -> ADM ()
insTape v acc = modify $ \env -> env {tape = M.insert v acc (tape env)}

insAdj :: VName -> VName -> ADM ()
insAdj v _v = modify $ \env -> env {adjs = M.insert v _v (adjs env)}

insAdjMap :: M.Map VName VName -> ADM ()
insAdjMap update = modify $ \env -> env {adjs = update `M.union` adjs env}

lookupTape :: VName -> ADM (Maybe VName)
lookupTape v = gets $ M.lookup v . tape

class Adjoint a where
  lookupAdj :: a -> ADM (VName, M.Map VName VName, Stms SOACS)
  updateAdjoint :: a -> VName -> ADM (VName, M.Map VName VName, Stms SOACS)
  updateAdjointArray :: Maybe (Slice SubExp) -> a -> VName -> ADM (VName, M.Map VName VName, Stms SOACS)

instance Adjoint VName where
  lookupAdj v = do
    maybeAdj <- gets $ M.lookup v . adjs
    case maybeAdj of
      Nothing -> newAdj v
      Just v_adj -> return (v_adj, mempty, mempty)

  updateAdjoint v d = do
    maybeAdj <- gets $ M.lookup v . adjs
    case maybeAdj of
      Nothing -> setAdjoint v (BasicOp . SubExp . Var $ d)
      Just _v -> do
        t <- lookupType v
        (_v', stms) <- runBinder . letExp "adj" $
          case t of
            Prim (IntType it) ->
              BasicOp $ BinOp (Add it OverflowWrap) (Var _v) (Var d)
            Prim (FloatType ft) ->
              BasicOp $ BinOp (FAdd ft) (Var _v) (Var d)
            tp -> error ("In Rev.hs, updateAdjoint: unsupported type: "++pretty tp)
        let update = M.singleton v _v'
        insAdjMap update
        return (_v', update, stms)

  updateAdjointArray maybe_slice v d = do
    maybeAdj <- gets $ M.lookup v . adjs
    t <- lookupType v
    case maybeAdj of
      Nothing -> do
        (_v, us1, s1) <- lookupAdj v
        (_v', us2, s2) <- updateAdjointArray maybe_slice v d
        return (_v', us2 <> us1, s1 <> s2)
      Just _v -> do
        (_v', stms) <- inScopeOf (_v, LParamName t) $
          runBinderT' $
            case maybe_slice of
              Nothing -> do
                t' <- lookupType _v
                letExp "updated_adj" =<< addArrays t' _v d
              Just slice -> do
                _vslice <-
                  if primType t
                    then return _v
                    else letExp (baseString _v ++ "_slice") $ BasicOp $ Index _v slice
                t' <- lookupType _vslice
                _vslice' <- addArrays t' _vslice d
                letInPlace "updated_adj" _v slice _vslice'
        let us = M.singleton v _v'
        insAdjMap us
        return (_v', us, stms)
    where
      bop t = case elemType t of
        ElemPrim (IntType it) -> Add it OverflowWrap
        ElemPrim (FloatType ft) -> FAdd ft
        tt -> error ("In Rev.hs, updateAdjointArray: unsupported element type: "++pretty tt)

      addArrays t xs ys =
        case (shapeDims . arrayShape) t of
          [] -> return $ BasicOp $ BinOp (bop t) (Var xs) (Var ys)
          (s : ss) -> do
            lam <- addArrays' $ t `setArrayShape` Shape ss
            return $ Op $ Screma s (mapSOAC lam) [xs, ys]
      addArrays' t =
        case (shapeDims . arrayShape) t of
          [] -> binOpLambda (bop t) $ case elemType t of ElemPrim pt -> pt
          (s : ss) -> do
            xs <- newVName "xs"
            ys <- newVName "ys"
            let t' = t `setArrayShape` Shape ss
            lam <- addArrays' t'
            body <- insertStmsM $ do
              res <- letSubExp "lam_map" $ Op $ Screma s (mapSOAC lam) [xs, ys]
              return $ resultBody [res]
            return
              Lambda
                { lambdaParams = [Param xs t', Param ys t'],
                  lambdaReturnType = [t'],
                  lambdaBody = body
                }

instance Adjoint SubExp where
  lookupAdj (Constant c) = do
    (_v, stms) <- runBinderT' $ letExp "const_adj" =<< eBlank (Prim $ primValueType c)
    return (_v, mempty, stms)
  lookupAdj (Var v) = lookupAdj v

  updateAdjoint se@Constant {} _ = lookupAdj se
  updateAdjoint (Var v) d = updateAdjoint v d

  updateAdjointArray _ se@Constant {} _ = lookupAdj se
  updateAdjointArray maybe_slice (Var v) d = updateAdjointArray maybe_slice v d

localS :: MonadState s m => (s -> s) -> m a -> m a
localS f m = do
  save <- get
  modify f
  a <- m
  put save
  return a

eIndex :: MonadBinder m => VName -> SubExp -> m (ExpT (Lore m))
eIndex arr i =
  return . BasicOp . Index arr . pure $ DimFix i

setAdjoint :: VName -> Exp -> ADM (VName, M.Map VName VName, Stms SOACS)
setAdjoint v e = do
  _v <- adjVName v
  stms <- runBinderT'_ $ letBindNames [_v] e
  let update = M.singleton v _v
  insAdjMap update
  return (_v, update, stms)

revFwdStm :: Stm -> ADM (Stms SOACS)
revFwdStm (Let (Pattern [] pats) aux (DoLoop [] valpats (ForLoop v it bound []) (Body () stms res))) = do
  accs <- mapM (accVName . patElemName) pats
  accsLoop <- mapM (accVName . paramName . fst) valpats

  runBinderT'_ $ do
    let accTs = map (accType bound NoUniqueness . patElemDec) pats
        accTsLoop = map (accType bound Unique . paramDec . fst) valpats
        accPats = zipWith PatElem accs accTs
    emptyAccs <- forM (zip3 accsLoop accTsLoop accTs) $ \(accLoop, accTLoop, accT) -> do
      blankV <- letSubExp "empty_acc" =<< eBlank accT
      return (Param accLoop accTLoop, blankV)
    (accsLoop', loop_body_stms) <- runBinderT' $ do
      accsLoop' <- forM (zip3 accsLoop accTs valpats) $ \(accLoop, accT, (param, _)) ->
        inScopeOf (accLoop, LParamName accT) $ do
          arr_t <- lookupType accLoop
          is' <- mapM (letSubExp "write_i") =<< sequence [toExp v]
          v' <- letSubExp "write_v" =<< toExp (paramName param)
          fmap Var $
            letInPlace
              "update_acc"
              accLoop
              (fullSlice arr_t (map DimFix is'))
              $ BasicOp $ SubExp v'
      addStms stms
      return accsLoop'
    let body' = Body () loop_body_stms $ res ++ accsLoop'
    addStm $
      Let (Pattern [] (pats ++ accPats)) aux $
        DoLoop [] (valpats ++ emptyAccs) (ForLoop v it bound []) body'
    lift $ zipWithM_ (insTape . patElemName) pats accs
  where
    accType n u (Prim t) = Array (ElemPrim t) (Shape [n]) u
    accType n _ (Array t (Shape dims) u) = Array t (Shape (n : dims)) u
    accType _ _ Acc {} = error "Accumulator encountered."
    accType _ _ Mem {} = error "Mem type encountered."
revFwdStm stm = return $ oneStm stm

revStm :: Stm -> ADM (M.Map VName VName, Stms SOACS)
revStm stm@(Let (Pattern [] pats) aux (DoLoop [] valpats (ForLoop v it bound []) (Body () stms_ res_))) =
  --fwdStm <- revFwdStm stm
  inScopeOf stm $
    localScope (scopeOfFParams $ map fst valpats ++ [Param v (Prim (IntType it))]) $ do
      -- Populate the body with a filler statement if there are none (makes taking adjoints easier).
      body@(Body () stms res) <-
        if stms_ == mempty
          then do
            (res', stms) <-
              runBinderT' $
                mapM (letSubExp "filler" . BasicOp . SubExp) res_
            return $ Body () stms res'
          else return $ Body () stms_ res_

      -- Get the adjoints of the iteration variables.
      let iter_vars = map (paramName . fst) valpats
      (_iter_vars, _iter_map, iter_stms) <- unzip3 <$> mapM lookupAdj iter_vars

      -- "Reset" expressions for the iteration adjoints. Reset
      -- expressions just zero-out the adjoint so that the adjoint on
      -- each loop iteration starts from 0. (If you unroll a loop,
      -- each iteration adjoint would be unique and thus start from
      -- 0.)
      (_iter_reset, _iter_reset_stms) <-
        runBinderT' $
          mapM (letExp "reset" <=< eBlank <=< lookupType) iter_vars

      -- Construct param-value bindings for the iteration adjoints.
      _iter_params <- inScopeOf (_iter_reset_stms : iter_stms) $ mkBindings _iter_vars _iter_reset

      -- Get adjoints for the free variables in the loop. Iteration
      -- variables are free in the body but bound by the loop, which
      -- is why they're subtracted off.
      let fv = namesToList (freeIn body) \\ iter_vars

      -- Get the adjoints of the result variables
      (_free_vars, _free_map, free_stms) <- unzip3 <$> mapM lookupAdj fv

      -- Generate new names to bind `_free_vars` to `valpats` and
      -- link them to the free variables.
      _free_binds <- forM _free_vars $ newVName . baseString
      zipWithM_ insAdj fv _free_binds

      -- Construct param-value bindings the free variable adjoints.
      _free_params <- inScopeOf free_stms $ mkBindings _free_binds _free_vars

      -- Make adjoints for each result variable of the original body.
      -- The result adjoints of the ith iteration must be set to the
      -- adjoints of the saved loop variables of the i+1th iteration.
      -- Important: this must be done *before* computing the
      -- reverse of the body.
      _original_res <- forM (subExpVars res) $ \res_v -> do
        res_v' <- adjVName res_v
        insAdj res_v res_v'
        return res_v'

      -- return (Param _b (toDecl t Unique), Var _v)
      -- Compute the reverse of the body.
      (body_update_map, Body () _stms _res) <-
        localScope (scopeOfFParams $ map fst $ _free_params ++ _iter_params) $
          revBody' body

      (_body_res_vars, _body_res_map, _) <- unzip3 <$> mapM lookupAdj (subExpVars res)

      zipWithM_ insAdj fv _free_binds

      let body_update_map_free = M.restrictKeys body_update_map $ S.fromList fv

      (_iter_vars', _, _) <- unzip3 <$> mapM lookupAdj iter_vars
      let _res' = map Var $ _iter_reset ++ _iter_vars' ++ M.elems body_update_map_free

      -- Remove any free paramters that weren't actually updated in the loop body
      let _free_params' = map fst $ filter ((`M.member` body_update_map_free) . snd) $ zip _free_params fv

      -- Construct the new return patterns.
      _pats_iter <- inScopeOf (mconcat iter_stms) $ mkPats _iter_vars
      _pats_body_res <- inScopeOf stms $ mkPats' (subExpVars res) _body_res_vars
      _pats_free_vars <- inScopeOf _stms $ mkPats $ M.elems body_update_map_free

      let _pats = _pats_iter ++ _pats_body_res ++ _pats_free_vars

      -- Construct value bindings for the body result adjoints. The initial binding is simply the
      -- adjoint of the nth iteration, which is given by the variables in the original pattern of the let-bind.
      (_loopres, _loopres_map, loopres_stms) <- unzip3 <$> forM pats (\(PatElem p _) -> lookupAdj p)

      let _body_params =
            zipWith3
              (\_b (PatElem _ t) _l -> (Param _b (toDecl t Unique), Var _l))
              _original_res
              _pats_body_res
              _loopres

      (bound', boundStms) <-
        runBinderT' $
          letSubExp "bound" $
            BasicOp (BinOp (Sub it OverflowWrap) bound (Constant $ IntValue $ intValue it (1 :: Int)))

      -- Look-up the stored loop iteration variables. Iteration
      -- variables are the variables bound in `valpats`. Every
      -- iteration of the loop, they are rebound to the result of the
      -- loop body.
      saved_iter_vars_maybe <- sequence <$> mapM (lookupTape . patElemName) pats

      (saved_iter_vars, fwdStms) <- case saved_iter_vars_maybe of
        Just saved_iter_vars -> return (saved_iter_vars, mempty)
        Nothing -> do
          fwdStms <- revFwdStm stm
          saved_iter_vars <- sequence <$> mapM (lookupTape . patElemName) pats
          case saved_iter_vars of
            Just saved_iter_vars' -> return (saved_iter_vars', fwdStms)
            Nothing -> error "oops"

      inScopeOf fwdStms $ do
        -- Loop body set-up
        (v', _loopSetup) <- runBinderT' $ do
          -- Go backwards
          v' <- letSubExp "idx" $ BasicOp (BinOp (Sub it OverflowWrap) bound' (Var v))

          -- Bind the accumulators
          forM_ (zip saved_iter_vars valpats) $ \(iter_var, (param, _)) ->
            letBindNames [paramName param] =<< eIndex iter_var v'

          return v'

        let subst = case v' of Constant {} -> error "oops"; Var v'' -> M.singleton v v''
            _valpats = _iter_params ++ _body_params ++ _free_params'
            _body = Body () (_loopSetup <> substituteNames subst _stms) _res'
            _stm = Let (Pattern [] _pats) aux (DoLoop [] _valpats (ForLoop v it bound []) _body)

        -- Update the free variables to point to new correct adjoints
        zipWithM_ insAdj fv $ map patElemName _pats_free_vars

        -- If any free variables weren't updated, fix their adjoint bindings
        mapM_ (uncurry insAdj) $
          filter ((`notElem` M.keys body_update_map_free) . fst) $
            zip fv _free_vars

        (_, _, final_contrib_stms) <-
          inScopeOf _stm $
            unzip3
              <$> mapM
                (uncurry updateAdjoint)
                ( mapMaybe
                    ( \(se, p) -> case se of
                        Var se_v -> Just (se_v, p)
                        _ -> Nothing
                    )
                    $ zip (map snd valpats) (map patElemName _pats_body_res)
                )

        adj_map <- gets adjs

        let changed_fv_map = M.restrictKeys adj_map $ S.fromList (fv ++ namesToList (freeIn (map snd valpats)))

        return (changed_fv_map, fwdStms <> boundStms <> _iter_reset_stms <> mconcat free_stms <> mconcat loopres_stms <> oneStm _stm <> mconcat final_contrib_stms)
  where
    mkBindings =
      zipWithM $ \_b _v -> do
        t <- lookupType _v
        return (Param _b (toDecl t Unique), Var _v)
    mkPats = mapM $ \_v -> do
      t <- lookupType _v
      _p <- newVName $ baseString _v <> "_res"
      return $ PatElem _p t

    mkPats' = zipWithM $ \resv _resv -> do
      t <- lookupType resv
      _p <- newVName $ baseString _resv <> "_res"
      return $ PatElem _p t
revStm stm@(Let _ _ (BasicOp CmpOp {})) =
  return (mempty, oneStm stm)
revStm stm@(Let (Pattern [] [pe]) _aux (BasicOp (UnOp op x))) = do
  let t = unOpType op
  (pe_adj, us1, s1) <- inScopeOf stm $ lookupAdj $ patElemName pe
  (contrib, contrib_stms) <- runBinder $ do
    let x_pe = primExpFromSubExp t x
        pe_adj' = primExpFromSubExp t (Var pe_adj)
        dx = pdUnOp op x_pe
    letExp "contrib" <=< toExp $ pe_adj' ~*~ dx

  (_, us2, s2) <- inScopeOf contrib_stms $ updateAdjoint x contrib

  pure
    ( us2 <> us1,
      s1 <> contrib_stms <> s2
    )
revStm stm@(Let (Pattern [] [pe]) _aux (BasicOp (BinOp op x y))) = do
  let t = binOpType op
  (_p, us1, s1) <- inScopeOf stm $ lookupAdj $ patElemName pe
  (_x, us2, s2) <- lookupAdj x
  (_y, us3, s3) <- lookupAdj y

  let (wrt_x, wrt_y) =
        pdBinOp op (primExpFromSubExp t x) (primExpFromSubExp t y)

      _p' = primExpFromSubExp t $ Var _p

  (adj_x, adj_x_s) <- runBinder $ letExp "adj" <=< toExp $ _p' ~*~ wrt_x
  (adj_y, adj_y_s) <- runBinder $ letExp "adj" <=< toExp $ _p' ~*~ wrt_y
  (_, x_us, x_s) <- updateAdjoint x adj_x
  (_, y_us, y_s) <- updateAdjoint y adj_y

  pure
    ( y_us <> x_us <> us3 <> us2 <> us1,
      s1 <> s2 <> s3 <> adj_x_s <> adj_y_s <> x_s <> y_s
    )
revStm stm@(Let (Pattern [] pats) aux (If cond t@(Body _ t_stms t_res) f@(Body _ f_stms f_res) attr)) = do
  (_pats, _uspats, _stm_pats) <- unzip3 <$> mapM (lookupAdj . patElemName) pats
  fwdStms <- revFwdStm stm
  t_fwd <- revFwdStms t_stms
  f_fwd <- revFwdStms f_stms
  zipWithM_ insAdj (subExpVars t_res) _pats
  zipWithM_ insAdj (subExpVars f_res) _pats
  saved_adjs <- gets adjs
  (t_map, _, _t@(Body t_desc t_stms_rev _t_res)) <- revBody t
  modify $ \env -> env {adjs = saved_adjs}
  (f_map, _, _f@(Body f_desc f_stms_rev _f_res)) <- revBody f
  modify $ \env -> env {adjs = saved_adjs}

  let deltas = sortOn baseTag $ M.keys $ t_map `M.union` f_map

  (_deltas, _, delta_stms) <- unzip3 <$> mapM lookupAdj deltas

  _t_res' <- localS (\env -> env {adjs = t_map `M.union` adjs env}) $ do
    (_t_res', _, _) <- unzip3 <$> mapM lookupAdj deltas
    return _t_res'

  _f_res' <- localS (\env -> env {adjs = f_map `M.union` adjs env}) $ do
    (_f_res', _, _) <- unzip3 <$> mapM lookupAdj deltas
    return _f_res'

  (_pats', res_map) <-
    unzip
      <$> forM
        deltas
        ( \v -> do
            v_t <- lookupType v
            v' <- adjVName v
            insAdj v v'
            return (PatElem v' v_t, M.singleton v v')
        )

  let _t' = Body t_desc (t_fwd <> t_stms_rev) $ map Var _t_res'
      _f' = Body f_desc (f_fwd <> f_stms_rev) $ map Var _f_res'

  ifret <- staticShapes <$> forM deltas lookupType
  let attr' = attr {ifReturns = ifret}

  return (mconcat res_map, fwdStms <> mconcat delta_stms <> oneStm (Let (Pattern [] _pats') aux (If cond _t' _f' attr')))
revStm (Let (Pattern [] [pat@(PatElem p t)]) _ (BasicOp (Index v slice))) = do
  (_p, us1, s1) <- inScopeOf (p, LParamName t) $ lookupAdj $ patElemName pat
  (_, us2, s2) <- updateAdjointArray (Just slice) v _p
  return (us2 <> us1, s1 <> s2)
revStm (Let (Pattern [] [pat@(PatElem p t)]) _ (BasicOp (Update v slice se))) = do
  (_p, us1, s1) <- inScopeOf (p, LParamName t) $ lookupAdj $ patElemName pat
  (_pslice, s2) <- inScopeOf (_p, LParamName t) $ runBinderT' $ letExp (baseString _p ++ "_slice") $ BasicOp $ Index _p slice
  (_se, us3, s3) <- updateAdjointArray Nothing se _pslice
  (_v, us4, s4) <- lookupAdj v
  t' <- case se of
    Constant c -> return $ Prim $ primValueType c
    Var se_v -> lookupType se_v
  (_vslice, s5) <- inScopeOf (_v, LParamName t') $ runBinderT' $ letExp (baseString _v ++ "_slice") $ BasicOp $ Index _v slice
  (_se', us6, s6) <- updateAdjoint se _vslice
  return (us6 <> us4 <> us3 <> us1, s1 <> s2 <> s3 <> s4 <> s5 <> s6)
revStm (Let (Pattern [] [pe]) _ (BasicOp (SubExp se)))
  | Var v <- se = do
    (_p, us1, s1) <- localScope (scopeOfPatElem pe) $ lookupAdj $ patElemName pe
    (_, us2, s2) <- updateAdjoint v _p
    return (us2 <> us1, s1 <> s2)
  | otherwise = return (mempty, mempty)
revStm (Let (Pattern [] [_]) _ (BasicOp (Reshape change v))) = do
  -- FIXME: how it be right to ignore the pattern?
  maybeAdj <- gets $ M.lookup v . adjs
  case maybeAdj of
    Nothing -> return (mempty, mempty)
    Just _v -> do
      (_v', stms) <- runBinderT' $ letExp "reshape_adj" (BasicOp (Reshape change _v))
      return (M.singleton v _v', stms)
revStm (Let (Pattern [] [pe]) _ (Op (Screma n (ScremaForm [] [] f) [xs]))) = do
  (_p, us1, s1) <- lookupAdj $ patElemName pe
  (_f, bound, fv) <- localS id $ revLambda f
  (paramsL, paramsR) <-
    splitAt (length (lambdaReturnType _f))
      <$> mapM (newParam "lam_adj") (lambdaReturnType _f ++ lambdaReturnType _f)
  (red_res, red_stms) <- runBinderT' $
    forM (drop (length bound) $ zip paramsL paramsR) $
      \(Param l t, Param r _) -> do
        let _op = case t of
              Prim (IntType it) -> Add it OverflowWrap
              Prim (FloatType ft) -> FAdd ft
              _ -> error "oops"
        letExp "*" =<< eBinOp _op (toExp l) (toExp r)

  let red_f =
        Lambda
          { lambdaParams = drop (length bound) paramsL ++ drop (length bound) paramsR,
            lambdaBody = mkBody red_stms $ map Var red_res,
            lambdaReturnType = drop (length bound) $ lambdaReturnType _f
          }
  (neutral, neutral_stms) <-
    runBinderT' $
      mapM (letSubExp "neut_adj" <=< eBlank . paramType) $
        drop (length bound) paramsL
  let red =
        Reduce
          { redComm = Commutative,
            redLambda = red_f,
            redNeutral = neutral
          }

  (_ds, d_stms) <- runBinderT' $ letTupExp "adj_updates" $ Op (Screma n (ScremaForm [] [] _f) [xs, _p])

  idf <- mkIdentityLambda $ drop (length bound) $ lambdaReturnType _f

  (_d_red, d_stms_red) <- runBinderT' $ letTupExp "adj_updates" $ Op (Screma n (ScremaForm [] [red] idf) (drop (length bound) _ds))

  (_fv, fv_us, fv_stms) <- inScopeOf d_stms_red $ unzip3 <$> zipWithM updateAdjoint fv _d_red

  (_xs', us2, s3) <- updateAdjointArray Nothing xs (head _ds)
  return (mconcat fv_us <> us2 <> us1, s1 <> neutral_stms <> d_stms <> d_stms_red <> mconcat fv_stms <> s3)

-- special case: reduce (+)
revStm (Let (Pattern [] [pe]) _ (Op (Screma _ form [as])))
  | Just [red] <- isReduceSOAC form,
    lam <- redLambda red,
    isRedPlus lam = do
    (pe_adj, pe_adj_us, pe_adj_stms) <- lookupAdj $ patElemName pe
    --(as_adj, as_adj_us, as_adj_stms) <- lookupAdj as
    v_rep <- newVName "v_rep"
    as_tp <- lookupType as
    rep_stms <- runBinder_ $
      letBindNames [v_rep] (BasicOp $ Replicate (shpFstDim $ arrayShape as_tp) $ Var pe_adj)
    (_, us, stms) <- updateAdjointArray Nothing as v_rep
    trace ("Reduce (+) stms: "++pretty (pe_adj_stms <> rep_stms <> stms)) $
      pure (pe_adj_us <> us, pe_adj_stms <> rep_stms <> stms)
  where
    shpFstDim (Shape []) = error "error in shpFstDim in Rev.hs"
    shpFstDim (Shape (d:_)) = Shape [d]
--
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
revStm (Let (Pattern [] pes) _ (Op (Screma n form xs)))
  | Just [red] <- isReduceSOAC form = do
    (ys_adj, pe_adj_us, pe_adj_stms) <- unzip3 <$> mapM (lookupAdj . patElemName) pes
    let (lam0, ne) = (redLambda red, redNeutral red)
    let alphas = lambdaReturnType lam0
    lam  <- renameLambda lam0
    lam' <- renameLambda lam0 {lambdaParams = flipParams $ lambdaParams lam0}
    par_i <- newParam "i" $ Prim int64
    -- phase1: build the scanomap soac
    g_bdy <- mkFusedMapBody par_i ne alphas
    let g_lam = Lambda [par_i] g_bdy (alphas++alphas)
    (r_scan, scan_stms) <- runBinderT' $ do
        iota <- letExp "iota" $ BasicOp $ Iota n (intConst Int64 0) (intConst Int64 1) Int64
        letTupExp "adj_ctrb_scan" $ Op (Screma n (ScremaForm [Scan lam ne, Scan lam' ne] [] g_lam) [iota])
    let (ls_arr, rs_arr) = splitAt (length ne) r_scan
    -- phase2: build the following map:
    f <- mkF $ redLambda red
    (f_rev, _, _) <- revLambda f
    par_i' <- newParam "i" $ Prim int64
    let i' = paramName par_i'
        (ls_as_ps, rs_adj_ps) = splitAt (2*length ne) $ lambdaParams f_rev
        (rs_ps, adj_ps) = splitAt (length ne) $ map paramName rs_adj_ps
        scp_params = par_i':ls_as_ps++zipWith Param ys_adj alphas
    f_adj_body <- runBodyBinder . localScope (scopeOfLParams scp_params) $ do
        nmim1 <- letSubExp "n_i_1" =<< toExp (pe64 n - (le64 i' + pe64 (intConst Int64 1)))
        let idx_stms = map (mkIdxStm nmim1) $ zip3 alphas rs_arr rs_ps
        addStms (stmsFromList idx_stms)            
        forM_ (zip ys_adj adj_ps) $ \ (y_bar, adj_par) ->
            letBindNames [adj_par] $ BasicOp $ SubExp $ Var y_bar
        pure $ lambdaBody f_rev
    let f_adj_body_res = take (length ne) $ drop (length ne) $ bodyResult f_adj_body
        map_lam = Lambda (par_i':ls_as_ps) (f_adj_body {bodyResult = f_adj_body_res}) alphas
    (r_map, map_stms) <- runBinderT' $ do
        iota <- letExp "iota" $ BasicOp $ Iota n (intConst Int64 0) (intConst Int64 1) Int64
        letTupExp "adj_ctrb_scan" $ Op (Screma n (ScremaForm [] [] map_lam) (iota:ls_arr++xs))
    -- finally add contributions to the adjoint of xs
    (_mystery, adj_upd_us, adj_upd_stms) <- fmap unzip3 . forM (zip xs r_map) $ \(x, x_adj) ->
        updateAdjointArray Nothing x x_adj
    let all_stms = mconcat pe_adj_stms <> scan_stms <> map_stms <> mconcat adj_upd_stms
    return (mconcat pe_adj_us <> mconcat adj_upd_us, all_stms)
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
      pure $ Lambda (lps <> aps <> rps) body $ lambdaReturnType lam
--
revStm (Let (Pattern [] [pe]) _ (Op (Screma w form as)))
  | Just [red] <- isReduceSOAC form = do
    (pe_adj, pe_adj_us, pe_adj_stms) <- lookupAdj $ patElemName pe
    (us, stms) <- runBinderT' $ do
      flip_red <- flipReduce red
      ls <- scanExc "ls" (redToScan red) as
      rs <-
        mapM eReverse
          =<< scanExc "ls" (redToScan flip_red)
          =<< mapM eReverse as

      f <- mkF $ redLambda red
      (f_rev, _, _) <- lift $ revLambda f
      traceM $ pretty f
      traceM $ pretty f_rev

      f_adj_params <-
        zipWithM
          newParam
          (init (map (baseString . paramName) (lambdaParams f_rev)))
          (init (map paramType (lambdaParams f_rev)))
      f_adj_body <- runBodyBinder $
        localScope (scopeOfLParams f_adj_params) $ do
          let adj_ses = map (Var . paramName) f_adj_params ++ [Var pe_adj]
          forM_ (zip (lambdaParams f_rev) adj_ses) $ \(p, adj) ->
            letBindNames [paramName p] $ BasicOp $ SubExp adj
          pure $ lambdaBody f_rev

      let f_adj = Lambda f_adj_params f_adj_body $ lambdaReturnType f_rev

      all_adjs <-
        letTupExp "adjs" $ Op $ Screma w (mapSOAC f_adj) $ ls ++ as ++ rs

      -- We only want the adjoints with respect to the 'as' parameters.
      let (_, as_adj, _) = splitAt3 (length as) (length as) all_adjs

      (_mystery, us, stms) <- fmap unzip3 . forM (zip as as_adj) $ \(a, a_adj) ->
        lift $ updateAdjointArray Nothing a a_adj

      addStms $ mconcat stms
      pure us

    pure (pe_adj_us <> mconcat us, pe_adj_stms <> stms)
  where
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
      pure $ Lambda (lps <> aps <> rps) body $ lambdaReturnType lam    
--
revStm stmt@(Let (Pattern [] pes) _ (Op (Screma n (ScremaForm [] [red] f) xs)))
  | isIdentityLambda f = do
    let (red_op, red_ne) = (redLambda red, redNeutral red)
        red_args_1 = take (length red_ne) $ lambdaParams red_op
        red_args_2 = drop (length red_ne) $ lambdaParams red_op
        red_op_rev = red_op {lambdaParams = red_args_2 ++ red_args_1}
    arr_tps <- mapM lookupType xs
    let full_out_slice = DimSlice (intConst Int64 0) n (intConst Int64 1)
    let full_slices = map (\arr_tp -> fullSlice arr_tp [full_out_slice]) arr_tps
    (res_adj, empty1, empty2) <- unzip3 <$> mapM (lookupAdj . patElemName) pes
    if all M.null empty1 && all null empty2
      then do
        -- ls = scan_exc red_op red_ne xs
        (lft_nms, stms_scan_1) <- mkScanExc n red_op (zip red_ne xs)
        -- rs = reverse <| scan_exc red_op_rev ne <| reverse xs
        (arrs1, stms_revs_1) <- mkReverse n (zip xs arr_tps)
        (arrs2, stms_scan_2) <- mkScanExc n red_op_rev (zip red_ne arrs1)
        (rgt_nms, stms_revs_2) <- mkReverse n (zip arrs2 arr_tps)
        -- cs = map3 (f_bar y_bar) ls rs xs
        map_lam <- revReduce2Map red_op res_adj
        let map_arr_args = lft_nms ++ rgt_nms ++ xs
        (adj_contribs, stms_map_ctrib) <-
          runBinderT' $
            letTupExp "adj_contribs" $ Op (Screma n (ScremaForm [] [] map_lam) map_arr_args)
        -- xs_bar = map2 (vect +) xs_bar_0 cs
        (new_map_adj, stms_map_upd) <-
          foldM
            ( \(m_acc, stms_acc) (x, c_, full_slice) -> do
                (_, m, ss) <- updateAdjointArray (Just full_slice) x c_
                return (m_acc <> m, stms_acc <> ss)
            )
            (M.empty, mempty)
            (zip3 xs adj_contribs full_slices)
        --(xs_adj_0, map_xs_adj, stms_xs_adj) <- unzip3 <$> mapM lookupAdj xs
        --(xs_adj, stms_map_upd) <- mkMapUpdate adj_contribs
        trace
          ( "Cosmin reduce: orig-stm: " ++ pretty stmt
              ++ "\n adjoint of result: "
              ++ pretty res_adj
              ++ "\n stms_scan_1: "
              ++ pretty stms_scan_1
              ++ "\n stms_revs_1: "
              ++ pretty stms_revs_1
              ++ "\n stms_scan_2: "
              ++ pretty stms_scan_2
              ++ "\n stms_revs_2: "
              ++ pretty stms_revs_2
              ++ "\n stms_map_ctrib: "
              ++ pretty stms_map_ctrib
              ++ "\n stms_map_upd: "
              ++ pretty stms_map_upd
              ++ "\n reduce_adjoint_nms: "
              ++ pretty new_map_adj
          )
          $ return (new_map_adj, stms_scan_1 <> stms_revs_1 <> stms_scan_2 <> stms_revs_2 <> stms_map_ctrib <> stms_map_upd)
      else error "Unreachable case in revStm for single reduce!"
revStm (Let Pattern {} _ (BasicOp Assert {})) =
  return (mempty, mempty)
revStm stm@(Let (Pattern [] [pe]) _ (Apply f args _ _))
  | Just (ret, argts) <- M.lookup f builtInFunctions = do
    (pe_adj, us1, s1) <- inScopeOf stm $ lookupAdj $ patElemName pe
    (contribs, contribs_stms) <- runBinder $ do
      let arg_pes = zipWith primExpFromSubExp argts (map fst args)
          pe_adj' = primExpFromSubExp ret (Var pe_adj)

      case pdBuiltin f arg_pes of
        Nothing ->
          error $ "No partial derivative defined for builtin function: " ++ pretty f
        Just derivs ->
          mapM (letExp "contrib" <=< toExp . (pe_adj' ~*~)) derivs

    let updateArgAdj (Var x, _) x_contrib = Just <$> updateAdjoint x x_contrib
        updateArgAdj _ _ = pure Nothing

    (_, us2, s2) <-
      inScopeOf contribs_stms $ unzip3 . catMaybes <$> zipWithM updateArgAdj args contribs

    pure
      ( mconcat us2 <> us1,
        s1 <> contribs_stms <> mconcat s2
      )
--
-- only single scan supported for now:  ys = scan odot xs
-- Implementation restricted to scalar operators for now, including tuples;
-- generalization should handle \bar{+} and \bar{*}
-- including for the lin_fun_comp operator
revStm (Let (Pattern [] pes) _ (Op (Screma n (ScremaForm [scn] [] f) xs)))
  | isIdentityLambda f = do
      let ys = map patElemName pes
      (ys_adj, ys_adj_us, ys_adj_stms) <- unzip3 <$> mapM lookupAdj ys
      map1_lam <- mkScanFusedMapLam n (scanLambda scn) xs ys ys_adj

      scans_lin_fun_o <- mapM mkScanLinFunO $ lambdaReturnType $ scanLambda scn
      let scp = zip ys_adj $ map (LParamName . patElemDec) pes
      (r_scan, scan_stms) <- runBinderT' $ inScopeOf scp $ do
            iota <- letExp "iota" $ BasicOp $ Iota n (intConst Int64 0) (intConst Int64 1) Int64
            letTupExp "adj_ctrb_scan" $ Op (Screma n (ScremaForm scans_lin_fun_o [] map1_lam) [iota])

      (red_nms, snd_maps) <- unzip <$> mapM (mkScan2ndMaps n) (zip3 (map patElemDec pes) ys_adj (chunk2 r_scan))

      (xs_contribs, fin_map_stms) <- mkScanFinalMap n (scanLambda scn) xs ys red_nms

      (new_map_adj, stms_map_upd) <- inScopeOf scp $ do
          foldM
            ( \(m_acc, stms_acc) (x, c_) -> do
                (_, m, ss) <- updateAdjointArray Nothing x c_
                return (m_acc <> m, stms_acc <> ss)
            )
            (M.empty, mempty)
            (zip xs xs_contribs)
      let all_stms = mconcat ys_adj_stms<>scan_stms<>mconcat snd_maps<>fin_map_stms<>stms_map_upd
      --trace ("COSMIN SCAN: "++pretty all_stms) $
      return (mconcat ys_adj_us <> new_map_adj, all_stms)
    where
      chunk2 [] = []
      chunk2 (a:b:r) = (a,b) : chunk2 r
      chunk2 _ = error "Rev.hs, chunk2: impossible case reached!"
-- everything else is unsupported at the moment
revStm stm = error $ "unsupported stm: " ++ pretty stm ++ "\n\n\n" ++ show stm

revLambda :: Lambda -> ADM (Lambda, [VName], [VName])
revLambda (Lambda params body@(Body () stms res) _) = inScopeOf stms $ do
  let rvars = subExpVars res
  params_adj <- forM rvars $ \v -> do
    v_adj <- adjVName v
    insAdj v v_adj
    Param v_adj <$> lookupType v

  (body_us, Body () fwdStms _, _body@(Body () rev_stms _)) <-
    localScope (scopeOfLParams params) $ revBody body

  (params_adjs, _, params_adjs_stms) <-
    unzip3 <$> mapM (lookupAdj . paramName) params

  (Body () rev_stms' rev_adj_res, subs) <-
    renameBody' $
      Body () (rev_stms <> mconcat params_adjs_stms) (map Var params_adjs)

  let body_us' = fmap (subs M.!) body_us

  let _rvars = subExpVars rev_adj_res
      bound = M.restrictKeys body_us' $ S.fromList $ map paramName params
      (bound_sort, fv_sort) = partition (`elem` M.elems bound) _rvars
      _res_sort = map Var $ bound_sort ++ fv_sort -- jank, fix
      rev =
        Lambda
          { lambdaParams = params ++ params_adj,
            lambdaBody = Body () (fwdStms <> rev_stms') rev_adj_res,
            lambdaReturnType = map paramType params
          }
  return
    ( rev,
      map (invert body_us' M.!) bound_sort,
      map (invert body_us' M.!) fv_sort
    )
  where
    invert m = M.fromList [(v, k) | (k, v) <- M.toList m]
    --renameBody' b = modifyNameSource $ runRenamer $ do
    --    b' <- rename b
    --    subs <- renamerSubstitutions
    --    return $ (b', subs)
    --runRenamer :: RenameM a -> VNameSource -> (a, VNameSource)
    --runRenamer (RenameM m) src = runReader (runStateT m src) env
    --  where env = RenameEnv M.empty
    renameBody' b = do
      let vs = namesToList $ boundInBody b
      subs <-
        mconcat
          <$> forM
            vs
            ( \v -> do
                v' <- newVName (baseString v)
                return $ M.singleton v v'
            )
      return (substituteNames subs b, subs)

-- revLambdaAcc :: Lambda -> ADM (Lambda, VName)
-- revLambdaAcc lambda@(Lambda params body@(Body decs stms res) ret) = do
--     let rvars  = concatMap (\se -> case se of Constant{} -> []; Var v -> [v]) res
--
--     _params <- zipWithM (\v t -> do
--                            _v <- adjVName v
--                            insAdj v _v
--                            _t <- lookupType v
--                            return $ Param _v t) rvars ret
--
--     --let _paramMap = mconcat <$> zipWithM (\v (Param _v _) -> M.singleton v _v) rvars _params
--
--     (body_us, fwdBody@(Body fwdDecs fwdStms fwdRes), _body@(Body _ _ _res')) <- localScope (scopeOfLParams params) $ revBody body
--
--     (Body _decs _stms _res, subs) <- renameBody' _body
--
--     let body_us' = fmap (subs M.!) body_us
--
--     let _rvars = concatMap (\se -> case se of Constant{} -> []; Var v -> [v]) _res
--         bound = M.restrictKeys body_us' $ S.fromList $ map paramName params
--         fv = M.restrictKeys body_us' $ S.fromList $ namesToList (freeIn body) \\ M.keys bound
--         (bound_sort, fv_sort) = partition (`elem` M.elems bound) _rvars
--         _res_sort = map Var $ bound_sort ++ fv_sort-- jank, fix
--
--     _ret <- inScopeOf (stms <> _stms) $ concat <$> forM _res (\se ->
--       case se of
--         Constant{} -> return []
--         Var v -> pure <$> lookupType v)
--
--     let rev = Lambda { lambdaParams = params ++ _params
--                      , lambdaBody = Body _decs (fwdStms <> _stms) _res_sort
--                      , lambdaReturnType = _ret
--                      }
--     return (rev, map (invert body_us' M.!) bound_sort, map (invert body_us' M.!) fv_sort)
--
--     where invert m = M.fromList [(v, k) | (k, v) <- M.toList m]
--           --renameBody' b = modifyNameSource $ runRenamer $ do
--           --    b' <- rename b
--           --    subs <- renamerSubstitutions
--           --    return $ (b', subs)
--           --runRenamer :: RenameM a -> VNameSource -> (a, VNameSource)
--           --runRenamer (RenameM m) src = runReader (runStateT m src) env
--           --  where env = RenameEnv M.empty
--           renameBody' b = do
--             let vs = namesToList $ boundInBody b
--             subs <- mconcat <$> forM vs (\v -> do
--               v' <- newVName (baseString v)
--               return $ M.singleton v v')
--             return (substituteNames subs b, subs)

revFwdStms :: Stms SOACS -> ADM (Stms SOACS)
revFwdStms = fmap mconcat . mapM revFwdStm . stmsToList

revStms :: Stms SOACS -> ADM (M.Map VName VName, Stms SOACS, Stms SOACS)
revStms all_stms
  | Just (stms, stm) <- stmsLast all_stms = do
    stm' <- revFwdStm stm
    (u, _stm) <- inScopeOf stm' $ revStm stm
    (us, stms', _stms) <- inScopeOf _stm $ revStms stms
    pure (us <> u, stms' <> stm', _stm <> _stms)
  | otherwise = pure (M.empty, mempty, mempty)

revStms' :: Stms SOACS -> ADM (M.Map VName VName, Stms SOACS)
revStms' stms = do
  (us, stms_adj) <- unzip <$> mapM revStm (stmsToList stms)
  pure (mconcat us, mconcat stms_adj)

revBody :: Body -> ADM (M.Map VName VName, Body, Body)
revBody b@(Body desc stms res) = do
  (us, stms', _stms) <- inScopeOf stms $ revStms stms
  let fv = namesToList $ freeIn b
      us' = M.filterWithKey (\k _ -> k `elem` fv) us
  let body' = Body desc _stms $ map Var $ M.elems us'
  return (us', Body desc stms' res, body')

revBody' :: Body -> ADM (M.Map VName VName, Body)
revBody' b@(Body desc stms _) = do
  (us, _stms) <- inScopeOf stms $ revStms' stms
  let fv = namesToList $ freeIn b
      us' = M.filterWithKey (\k _ -> k `elem` fv) us
  let body' = Body desc _stms $ map Var $ M.elems us'
  return (us, body')

revVJP :: MonadFreshNames m => Scope SOACS -> Lambda -> m Lambda
revVJP scope (Lambda params body@(Body () stms res) _) = do
  let initial_renv = REnv {tans = mempty, envScope = scope}
  flip runADM initial_renv . localScope (scopeOfLParams params) . inScopeOf stms $ do
    let rvars = subExpVars res
    params_adj <- forM rvars $ \v -> do
      v_adj <- adjVName v
      insAdj v v_adj
      Param v_adj <$> lookupType v

    (_body_us, Body () fwd_stms _, Body () rev_stms _) <- revBody body

    (params_adjs, _, params_adjs_stms) <-
      unzip3 <$> mapM (lookupAdj . paramName) params

    Body () rev_stms' rev_adj_res <-
      renameBody $ Body () (rev_stms <> mconcat params_adjs_stms) (map Var params_adjs)

    let lam =
          Lambda
            (params ++ params_adj)
            (Body () (fwd_stms <> rev_stms') rev_adj_res)
            $ map paramType params

    pure lam
