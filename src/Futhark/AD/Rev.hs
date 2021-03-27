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

import Control.Monad
import Control.Monad.State.Strict
import Data.Bifunctor (second)
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
import Futhark.Util (takeLast)

----------------------
-- Helper functions --
----------------------

--- First some general utility functions that are not specific to AD.
eReverse :: VName -> ADM VName
eReverse arr = do
  arr_t <- lookupType arr
  let w = arraySize 0 arr_t
      arr_t' = stripArray 1 arr_t
  -- generate the reverse indexing expression
  ip <- newParam "i" (Prim p_int64)
  let i = paramName ip
  (arr_i, ind_stms) <- runBinderT' . localScope (scopeOfLParams [ip, Param arr arr_t]) $ do
    inv_ind <- letSubExp "inv_ind" =<< toExp (pe64 w - (le64 i + pe64 (intConst Int64 1)))
    letExp (baseString arr ++ "_inv_elem") $ BasicOp $ Index arr $ fullSlice arr_t [DimFix inv_ind]
  -- produce the inner map nest enumerating the elements on each dimension
  v_elm <- newVName "elem"
  let pexps = [LeafExp v_elm (getPrimElemType arr_t)]
      tab = M.fromList [(v_elm, (arr_i, arr_t'))]
  (rs, map_stms) <- liftPExp2MapNest pexps tab
  -- put back the results into a map soac
  let lam_bdy = mkBody (ind_stms <> map_stms) $ map Var rs
      lam = Lambda [ip] lam_bdy [arr_t']
  iota_v <-
    letExp "iota" . BasicOp $
      Iota w (intConst Int64 0) (intConst Int64 1) Int64
  letExp (baseString arr ++ "_inv") $ Op $ Screma w (mapSOAC lam) [iota_v]

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

chunk2 :: [VName] -> [(VName, VName)]
chunk2 [] = []
chunk2 (a : b : r) = (a, b) : chunk2 r
chunk2 _ = error "Rev.hs, chunk2: impossible case reached!"

p_int64 :: PrimType
p_int64 = IntType Int64

onePrimValue :: PrimType -> PrimValue
onePrimValue (IntType Int8) = IntValue $ Int8Value 1
onePrimValue (IntType Int16) = IntValue $ Int16Value 1
onePrimValue (IntType Int32) = IntValue $ Int32Value 1
onePrimValue (IntType Int64) = IntValue $ Int64Value 1
onePrimValue (FloatType Float32) = FloatValue $ Float32Value 1.0
onePrimValue (FloatType Float64) = FloatValue $ Float64Value 1.0
onePrimValue Bool = BoolValue True
onePrimValue Cert = error "In onePrimValue, Rev.hs: input Cert not supported"

getBinOpPlus :: PrimType -> BinOp
getBinOpPlus (IntType x) = Add x OverflowUndef
getBinOpPlus (FloatType f) = FAdd f
getBinOpPlus _ = error "In getBinOpMul, Rev.hs: input Cert not supported"

getBinOpMul :: PrimType -> BinOp
getBinOpMul (IntType x) = Mul x OverflowUndef
getBinOpMul (FloatType f) = FMul f
getBinOpMul _ = error "In getBinOpMul, Rev.hs: input Cert not supported"

-- Pattern Matches special lambda cases:
--   plus, multiplication, min, max, which are all commutative.
-- Succeeds for (\ x y -> x binop y) or (\x y -> y binop x).
isSpecOpLam :: (BinOp -> Maybe BinOp) -> Lambda -> Maybe BinOp
isSpecOpLam isOp lam =
  isRedStm
    (map paramName $ lambdaParams lam)
    (bodyResult $ lambdaBody lam)
    (stmsToList $ bodyStms $ lambdaBody lam)
  where
    isRedStm [a, b] [r] [Let (Pattern [] [pe]) _aux (BasicOp (BinOp op x y))] =
      if (r == Var (patElemName pe)) && ((x == Var a && y == Var b) || (x == Var b && y == Var a))
        then isOp op
        else Nothing
    isRedStm _ _ _ = Nothing

isAddTowLam :: Lambda -> Maybe BinOp
isAddTowLam lam = isSpecOpLam isAddOp $ filterMapOp lam
  where
    isAddOp bop@(Add _ _) = Just bop
    isAddOp bop@(FAdd _) = Just bop
    isAddOp _ = Nothing
    filterMapOp (Lambda [pa1, pa2] lam_body _)
      | [r] <- bodyResult lam_body,
        [map_stm] <- stmsToList (bodyStms lam_body),
        (Let pat _ (Op scrm)) <- map_stm,
        (Pattern [] [pe]) <- pat,
        (Screma _ (ScremaForm [] [] map_lam) [a1, a2]) <- scrm,
        (a1 == paramName pa1 && a2 == paramName pa2) || (a1 == paramName pa2 && a2 == paramName pa1),
        r == Var (patElemName pe) =
        filterMapOp map_lam
    filterMapOp other_lam = other_lam

isMulLam :: Lambda -> Maybe BinOp
isMulLam lam = isSpecOpLam isMulOp lam
  where
    isMulOp bop@(Mul _ _) = Just bop
    isMulOp bop@(FMul _) = Just bop
    isMulOp _ = Nothing

isMinMaxLam :: Lambda -> Maybe BinOp
isMinMaxLam lam = isSpecOpLam isMinMaxOp lam
  where
    isMinMaxOp bop@(SMin _) = Just bop
    isMinMaxOp bop@(UMin _) = Just bop
    isMinMaxOp bop@(FMin _) = Just bop
    isMinMaxOp bop@(SMax _) = Just bop
    isMinMaxOp bop@(UMax _) = Just bop
    isMinMaxOp bop@(FMax _) = Just bop
    isMinMaxOp _ = Nothing

adBlank :: Type -> BasicOp
adBlank (Prim pt) = SubExp $ Constant $ blankPrimValue pt
adBlank (Array (ElemPrim t) shape _) = Replicate shape $ Constant $ blankPrimValue t
adBlank (Array (ElemAcc _ _ _) _ _) = error "adBlank: cannot create array of accumulators YET (?)"
adBlank Acc {} = error "adBlank: cannot create blank accumulator"
adBlank Mem {} = error "adBlank: cannot create blank memory"

adOne :: Type -> BasicOp
adOne (Prim pt) = SubExp $ Constant $ onePrimValue pt
adOne (Array (ElemPrim t) shape _) = Replicate shape $ Constant $ onePrimValue t
adOne (Array (ElemAcc _ _ _) _ _) = error "adOne: cannot create array of accumulators YET (?)"
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

----------------------------------------------------
--- Helpers for Histogram, Reduction and Scatter ---
----------------------------------------------------

withinBounds :: [(SubExp,VName)] -> TPrimExp Bool VName
withinBounds [] = TPrimExp $ ValueExp (BoolValue True)
withinBounds [(q,i)] = (le64 i .<. pe64 q) .&&. (pe64 (intConst Int64 (-1)) .<. le64 i) 
withinBounds (qi:qis) = (withinBounds [qi]) .&&. (withinBounds qis)

peElemEq0 :: PrimType -> Param Type -> PrimExp VName
peElemEq0 ptp farg =
  CmpOpExp
    (CmpEq ptp)
    (LeafExp (paramName farg) ptp)
    (ValueExp (blankPrimValue ptp))

helperMulOp1 :: PrimType -> BinOp -> ADM (Lambda, Lambda)
helperMulOp1 ptp bop = do
  -- on forward sweep: create the map lambda
  let eltp = Prim ptp
  farg <- newParam "arg" eltp
  map_lam_bdy <-
    runBodyBinder . localScope (scopeOfLParams [farg]) $
      eBody
        [ eIf
            (toExp $ peElemEq0 ptp farg)
            (resultBodyM [Constant $ onePrimValue ptp, intConst Int64 1])
            (resultBodyM [Var (paramName farg), intConst Int64 0])
        ]
  let map_lam = Lambda [farg] map_lam_bdy [eltp, Prim p_int64]
  -- still on forward sweep: create the reduce lambda [*, +]
  fargs_prod <- mapM (`newParam` eltp) ["acc_prod", "arg_val"]
  fargs_count <- mapM (`newParam` (Prim p_int64)) ["acc_count", "arg_count"]
  let ([acc_v, arg_v], [acc_c, arg_c]) = (fargs_prod, fargs_count)
  red_lam_bdy <- runBodyBinder . localScope (scopeOfLParams (fargs_prod ++ fargs_count)) $ do
    r_prod <-
      letSubExp "res_prod" $
        BasicOp $
          BinOp
            bop
            (Var $ paramName acc_v)
            (Var $ paramName arg_v)
    r_count <- letSubExp "res_count" =<< toExp (le64 (paramName acc_c) + le64 (paramName arg_c))
    resultBodyM [r_prod, r_count]
  let red_lam = Lambda [acc_v, acc_c, arg_v, arg_c] red_lam_bdy [eltp, Prim p_int64]
  return (map_lam, red_lam)

helperMulOp2 :: PrimType -> VName -> VName -> VName -> ADM (Stms SOACS)
helperMulOp2 ptp nz_prod zr_count prod = do
  -- on forward sweep: if statement to recover the potentially-zero product
  let ps = [Param nz_prod $ Prim ptp, Param zr_count $ Prim p_int64]
  runBinder_ . localScope (scopeOfLParams ps) $ do
    tmps <-
      letTupExp "tmp_if_res"
        =<< eIf
          (toExp $ le64 zr_count .>. pe64 (intConst Int64 0))
          (resultBodyM [Constant $ blankPrimValue ptp])
          (resultBodyM [Var nz_prod])
    addStm (mkLet [] [Ident prod $ Prim ptp] $ BasicOp $ SubExp $ Var $ head tmps)

helperMulOp3 :: PrimType -> BinOp -> VName -> VName -> Param Type -> VName -> ADM Body
helperMulOp3 ptp bop nz_prod zr_count fa_orig prod_bar = do
  -- if zero_count == 0 then (nz_prod / a) * p_bar
  --                            else if zero_count == 1 && a == 0
  --                                 then nz_prod * p_bar
  --                                 else 0
  let params = zipWith Param [nz_prod, zr_count, prod_bar] [Prim ptp, Prim p_int64, Prim ptp]
  runBodyBinder . localScope (scopeOfLParams (fa_orig : params)) $
    eBody
      [ eIf
          (toExp $ le64 zr_count .<. pe64 (intConst Int64 1))
          ( do
              div_se <-
                letSubExp "div_res" $
                  BasicOp $
                    BinOp
                      (getBinOpDiv bop)
                      (Var nz_prod)
                      (Var $ paramName fa_orig)
              res_se <- letSubExp "res_ctrb" $ BasicOp $ BinOp bop div_se (Var prod_bar)
              resultBodyM [res_se]
          )
          ( eBody
              [ eIf
                  (toExp $ BinOpExp LogAnd (peElemEq0 ptp fa_orig) (eqToOne zr_count))
                  ( do
                      res_se <-
                        letSubExp "res_ctrb" $
                          BasicOp $
                            BinOp
                              bop
                              (Var nz_prod)
                              (Var prod_bar)
                      resultBodyM [res_se]
                  )
                  (resultBodyM [Constant $ blankPrimValue ptp])
              ]
          )
      ]
  where
    eqToOne v_nm = CmpOpExp (CmpEq p_int64) (LeafExp v_nm p_int64) (ValueExp $ IntValue $ Int64Value 1)
    getBinOpDiv (Mul t _) = SDiv t Unsafe
    getBinOpDiv (FMul t) = FDiv t
    getBinOpDiv _ = error "In Rev.hs, getBinOpDiv, unreachable case reached!"

-- generates the lambda for the extended operator of min/max:
-- meaning the min/max value tupled with the minimal index where
--   that value was found (when duplicates exist). We use `-1` as
--   the neutral element of the index.
mkMinMaxIndLam :: PrimType -> BinOp -> ADM Lambda
mkMinMaxIndLam ptp minmax_op = do
  fargs_vals <- mapM (`newParam` (Prim ptp)) ["acc_v", "arg_v"]
  fargs_inds <- mapM (`newParam` (Prim p_int64)) ["acc_ind", "arg_ind"]
  let ([facc_v, farg_v], [facc_i, farg_i]) = (fargs_vals, fargs_inds)
  let [acc_v, arg_v, acc_i, arg_i] = map paramName (fargs_vals ++ fargs_inds)
  let (cmp1, cmp2) = get_cmp_pexp minmax_op acc_v arg_v
  red_lam_bdy <-
    runBodyBinder . localScope (scopeOfLParams (fargs_vals ++ fargs_inds)) $
      eBody
        [ eIf
            (toExp cmp1)
            ( do
                res_ind <- letSubExp "minmax" =<< toExp (min_idx_pexp acc_i arg_i)
                resultBodyM [Var acc_v, res_ind]
            )
            ( eBody
                [ eIf
                    (toExp cmp2)
                    (resultBodyM [Var acc_v, Var acc_i])
                    (resultBodyM [Var arg_v, Var arg_i])
                ]
            )
        ]
  return $ Lambda [facc_v, facc_i, farg_v, farg_i] red_lam_bdy [Prim ptp, Prim p_int64]
  where
    min_idx_pexp i1 i2 = BinOpExp (SMin Int64) (LeafExp i1 p_int64) (LeafExp i2 p_int64)
    get_cmp_pexp bop facc farg =
      let [leaf_acc, leaf_arg] = map (`LeafExp` ptp) [facc, farg]
      in  ( CmpOpExp (CmpEq ptp) leaf_acc leaf_arg,
            CmpOpExp (CmpEq ptp) leaf_acc $ BinOpExp bop leaf_acc leaf_arg
          )

-- Generates: `ind == -1`
mind_eq_min1 :: VName -> PrimExp VName
mind_eq_min1 ind =
      CmpOpExp
        (CmpEq (IntType Int64))
        (LeafExp ind p_int64)
        (ValueExp (IntValue $ Int64Value (-1)))

-- Generates a potential tower-of-maps lambda body for an indexing operation.
-- Assuming parameters: 
--   `arr`   the array that is indexed
--   `[(w_1, i_1), (w_2, i_2), ..., (w_k, i_k)]` outer lambda formal parameters and their bounds
--   `[n_1,n_2,...]ptp` the type of the index expression `arr[i_1,i_2,...,i_k]`
-- Generates something like:
-- (\ i_1 i_2 ->
--    map (\j_1 -> ... if (i_1 >= 0 && i_1 < w_1) &&
--                        (i_2 >= 0 && i_2 < w_2) && ...
--                     then arr[i_1, i_2, ... j_1, ...]
--                     else 0
--        ) (iota n_1)
-- )
-- The idea is that you do not want to put under the `if` something
--     that is an array because it would not flatten well!
genIdxLamBdy :: VName -> [(SubExp, Param Type)] -> Type -> ADM Body
genIdxLamBdy as wpis tp =
  genRecLamBdy as wpis [] tp 
  where
    genRecLamBdy :: VName -> [(SubExp, Param Type)] -> [Param Type] -> Type -> ADM Body
    genRecLamBdy arr w_pis nest_pis (Array (ElemPrim t) (Shape []) _) =
      genRecLamBdy arr w_pis nest_pis (Prim t)
    genRecLamBdy arr w_pis nest_pis (Array (ElemPrim t) (Shape (s : ss)) u) = do
      new_ip <- newParam "i" (Prim p_int64)
      let t' = if null ss then Prim t else Array (ElemPrim t) (Shape ss) u
      inner_lam_bdy <- genRecLamBdy arr w_pis (nest_pis ++ [new_ip]) t'
      let inner_lam = Lambda [new_ip] inner_lam_bdy [t']
          (_, orig_pis) = unzip w_pis
      (r, stms) <- runBinderT' . localScope (scopeOfLParams (orig_pis++nest_pis)) $ do
        iota_v <- letExp "iota" $ BasicOp $ Iota s (intConst Int64 0) (intConst Int64 1) Int64
        letSubExp (baseString arr ++ "_elem") $ Op $ Screma s (mapSOAC inner_lam) [iota_v]
      mkBodyM stms [r]
    genRecLamBdy arr w_pis nest_pis (Prim ptp) = do
      let (ws, orig_pis) = unzip w_pis
      let inds = map paramName (orig_pis++nest_pis)
      runBodyBinder . localScope (scopeOfLParams (orig_pis++nest_pis)) $
        eBody
          [ eIf
              ( toExp $ withinBounds $ zip ws $ map paramName orig_pis )
              ( do
                  r <- letSubExp "r" $ BasicOp $ Index arr $ map (DimFix . Var) inds
                  resultBodyM [r]
              )
              ( resultBodyM [Constant $ blankPrimValue ptp] )
          ]
    genRecLamBdy _ _ _ _ = error "In Rev.hs, helper function genRecLamBdy, unreachable case reached!"

------------------------
--- Helpers for Scan ---
------------------------
liftPExp2MapNest :: [PrimExp VName] -> M.Map VName (VName, Type) -> ADM ([VName], Stms SOACS)
liftPExp2MapNest pexps tab
  | not (null pexps),
    not (M.null tab),
    tp_cand <- snd (head (M.elems tab)),
    depth <- arrayRank tp_cand,
    fvts <- foldl S.union S.empty (map leafExpTypes pexps),
    fvs <- map fst (S.toList fvts),
    all (isValidPrimType depth) fvts,
    arr_arg_tps <- mapMaybe (`M.lookup` tab) fvs,
    (length arr_arg_tps == length fvs) = do
    genMapNest fvs depth (map (uncurry Param) arr_arg_tps)
  where
    isValidPrimType d (v, pt) =
      case M.lookup v tab of
        Just (_, t) -> isPrimTypeAtDepthOf d pt t
        Nothing -> False
    isPrimTypeAtDepthOf d t (Prim t') =
      t == t' && d == 0
    isPrimTypeAtDepthOf d t (Array (ElemPrim t') shape _) =
      t == t' && d == length (shapeDims shape)
    isPrimTypeAtDepthOf _ _ _ = False
    genMapNest :: [VName] -> Int -> [Param Type] -> ADM ([VName], Stms SOACS)
    genMapNest fvs 0 scal_ps = do
      runBinderT' . localScope (scopeOfLParams scal_ps) $ do
        forM_ (zip fvs scal_ps) $ \(v, p) -> do
          letBindNames [v] $ BasicOp $ SubExp $ Var $ paramName p
        forM pexps $ \pe -> letExp "r" =<< toExp pe
    genMapNest fvs d arr_arg_tps = do
      -- create names and type bindings for lambda args
      let (arr_nms, arr_tps) = (map paramName arr_arg_tps, map paramDec arr_arg_tps)
      let l_ts = map (stripArray 1) arr_tps
      l_ps <- mapM (newParam "lam_arg") l_ts
      (rs, stms) <- genMapNest fvs (d -1) l_ps
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
        mkRetTp _ _ = error "unreachable case reached!"
liftPExp2MapNest _ _ = error "In Rev.hs, function liftPExp2MapNest: unreachable case reached!"

-- computes `d(x op y)/dx` when keep_first=true,
-- and `d(x op y)/dy otherwise.
-- `op` is given as `lam`
mkScanAdjointLam :: Lambda -> Bool -> ADM Lambda
mkScanAdjointLam lam0 keep_first = do
  let len = length $ lambdaReturnType lam0
  lam <- renameLambda lam0
  let p2diff =
        if keep_first
          then take len $ lambdaParams lam
          else drop len $ lambdaParams lam
  p_adjs <- mapM (\tp -> newParam "elem_adj" tp) (lambdaReturnType lam)
  lam' <-
    localScope (scopeOfLParams p_adjs) $
      diffLambda (map paramName p_adjs) (map paramName p2diff) lam
  stms' <-
    runBinderT'_ $
      mapM_ (\p -> letBindNames [paramName p] $ BasicOp $ adOne $ paramDec p) p_adjs
  let lam_bdy' = lambdaBody lam'
      lam'' = lam' {lambdaBody = lam_bdy' {bodyStms = stms' <> bodyStms lam_bdy'}}
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
  ystp <- mapM lookupType ys
  let rtp = lambdaReturnType lam
  let lam_arg = map paramName $ lambdaParams lam
  let (lam_as, lam_bs) = splitAt (length rtp) lam_arg
  par_i <- newParam "i" $ Prim int64
  let i = paramName par_i
  let pars = zipWith Param ys_adj ystp
  body <-
    runBodyBinder . localScope (scopeOfLParams (par_i : pars)) $
      eBody
        [ eIf
            (toExp $ le64 i .>. pe64 (intConst Int64 0))
            ( do
                (nms, stms) <- runBinderT' . localScope (scopeOfLParams (par_i : pars)) $ do
                  j <- letSubExp "j" =<< toExp (pe64 n - (le64 i + pe64 (intConst Int64 1)))
                  j1 <- letSubExp "j1" =<< toExp (pe64 n - le64 i)
                  y_s <- forM (zip (zip3 ys xs ys_adj) (zip3 lam_as lam_bs rtp)) $
                    \((y, x, y_), (a, b, t)) -> do
                      yj <- letSubExp (baseString y ++ "_elem") $ BasicOp $ Index y $ DimFix j : fullSlice t []
                      y_j <- letSubExp (baseString y_ ++ "_elem") $ BasicOp $ Index y_ $ DimFix j : fullSlice t []
                      xj <- letSubExp (baseString x ++ "_elem") $ BasicOp $ Index x $ DimFix j1 : fullSlice t []
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
                  os <- mapM (letSubExp "ct_one" . BasicOp . adOne) rtp
                  return $ interleave $ zip zs os
                addStms stms
                resultBodyM rs
            )
        ]
  return $ Lambda [par_i] body (interleave $ zip rtp rtp)
  where
    interleave [] = []
    interleave ((a, b) : l) = a : b : interleave l

-- let lin_o (a1: real, b1: real) (a2:real, b2: real) = (a2 \bar{+} b2 \bar{*} a1, b1 \bar{*} b2)
mkScanLinFunO :: Type -> ADM (Scan SOACS)
mkScanLinFunO tp = do
  let ptp = getPrimElemType tp
  zero <- mkCtOrReplicate "zeros" tp (Constant $ blankPrimValue ptp)
  one <- mkCtOrReplicate "ones" tp (Constant $ onePrimValue ptp)
  tmp <- mapM newVName ["a1", "b1", "a2", "b2"]
  arr_nms <- mapM newVName ["a1s", "b1s", "a2s", "b2s"]
  let [a1, b1, a2, b2] = tmp
      ps = map (`Param` tp) arr_nms
      -- lift scalar computation `a2 + b2*a1, b1*b2` to a map nest
      (pet, plus, mul) = (primExpFromSubExp ptp . Var, getBinOpPlus ptp, getBinOpMul ptp)
      pexps =
        [ BinOpExp plus (pet a2) $ BinOpExp mul (pet b2) (pet a1),
          BinOpExp mul (pet b1) (pet b2)
        ]
      tab = M.fromList $ zipWith (\v vs -> (v, (vs, tp))) tmp arr_nms
  (rs, stms) <- liftPExp2MapNest pexps tab
  let body = mkBody stms $ map Var rs
      lam = Lambda ps body [tp, tp]
  return $ Scan lam [zero, one]
  where
    mkCtOrReplicate _ (Prim _) ct = return ct
    mkCtOrReplicate str (Array (ElemPrim _) shape _) ct =
      letSubExp str $ BasicOp $ Replicate shape ct
    mkCtOrReplicate _ _ _ = error "In Rev.hs, mkCtOrReplicate, unsupported type!"

-- build the map following the scan with linear-function-composition:
-- for each (ds,cs) length-n array results of the scan, combine them as:
--    `let rs = map2 (\ d_i c_i -> d_i + c_i * y_adj[n-1]) d c |> reverse`
-- but insert explicit indexing to reverse inside the map.
mkScan2ndMaps :: SubExp -> (Type, VName, (VName, VName)) -> ADM (VName, Stms SOACS)
mkScan2ndMaps n (arr_tp, y_adj, (ds, cs)) = do
  let ptp = getPrimElemType arr_tp
      eltp = stripArray 1 arr_tp
  par_i <- newParam "i" $ Prim int64
  let i = paramName par_i
  let pars = zipWith Param [y_adj, ds, cs] (replicate 3 arr_tp)
  (nms, bdy_stms_1) <- runBinderT' . localScope (scopeOfLParams (par_i : pars)) $ do
    -- ys_adj_last = ys_adj[n-1]  will be hoisted outside
    nm1 <- letSubExp "nm1" =<< toExp (pe64 n + pe64 (intConst Int64 (-1)))
    y_adj_last <- letExp (baseString y_adj ++ "_last") $ BasicOp $ Index y_adj $ fullSlice arr_tp [DimFix nm1]
    j <- letSubExp "j" =<< toExp (pe64 n - (le64 i + pe64 (intConst Int64 1)))
    dj <- letExp (baseString ds ++ "_elem") $ BasicOp $ Index ds $ DimFix j : fullSlice eltp []
    cj <- letExp (baseString cs ++ "_elem") $ BasicOp $ Index cs $ DimFix j : fullSlice eltp []
    return [y_adj_last, dj, cj]
  tmp <- mapM newVName ["y_adj_last", "d_j", "c_j"]
  let [y_l, d_j, c_j] = tmp
      (pet, plus, mul) = (primExpFromSubExp ptp . Var, getBinOpPlus ptp, getBinOpMul ptp)
      pexps = [BinOpExp plus (pet d_j) $ BinOpExp mul (pet c_j) (pet y_l)]
      tab = M.fromList $ zipWith (\v vs -> (v, (vs, eltp))) tmp nms
  (rs, bdy_stms_2) <- liftPExp2MapNest pexps tab
  let lam = Lambda [par_i] (mkBody (bdy_stms_1 <> bdy_stms_2) $ map Var rs) [eltp]
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
  par_x <- mapM (\(x, t) -> newParam (baseString x ++ "_elem") t) $ zip xs eltps
  par_r <- mapM (\(r, t) -> newParam (baseString r ++ "_elem") t) $ zip rs eltps
  -- unfortunately, we need to handle the ` lam_res \bar{*} r` outside the runBinderT'
  let lam_rs0 = bodyResult $ lambdaBody lam
  (lam_rs, cpy_stms) <- runBinderT' . localScope (scopeOfLParams (par_i : par_r ++ par_x)) $ do
    forM lam_rs0 $ \lam_r0 -> mkVName lam_r0
  tmp_res <- forM (zip3 lam_rs (map paramName par_r) eltps) $ \(lam_r, r, eltp) -> do
    lr0 <- newVName (baseString lam_r ++ "_0_")
    r0 <- newVName (baseString r ++ "_0_")
    let ptp = getPrimElemType eltp
        (pet, mul) = (primExpFromSubExp ptp . Var, getBinOpMul ptp)
        pexps = [BinOpExp mul (pet lr0) (pet r0)]
        tab = M.fromList $ zipWith (\v vs -> (v, (vs, eltp))) [lr0, r0] [lam_r, r]
    (rs_i, stms_i) <- liftPExp2MapNest pexps tab
    return (head rs_i, stms_i)
  let (th_res, stms2_lst) = unzip tmp_res
      stms2 = mconcat stms2_lst
  (cs, lam_stms) <-
    runBinderT' . localScope (scopeOfLParams (par_i : par_r ++ par_x)) $
      letTupExp "scan_contribs"
        =<< eIf
          (toExp $ le64 i .>. pe64 (intConst Int64 0))
          ( do
              (_, stms_1) <- runBinderT' . localScope (scopeOfLParams (par_i : par_r ++ par_x)) $ do
                im1 <- letSubExp "im1" =<< toExp (le64 i - pe64 (intConst Int64 1))
                ys_im1 <- forM (zip ys eltps) $ \(y, t) -> do
                  letSubExp (baseString y ++ "_elem") $ BasicOp $ Index y $ DimFix im1 : fullSlice t []
                forM_ (zip (ys_im1 ++ map (Var . paramName) par_x) (map paramName (lambdaParams lam))) $
                  \(act, frm) -> letBindNames [frm] $ BasicOp $ SubExp act
                bodyBind $ lambdaBody lam
              addStms (stms_1 <> cpy_stms <> stms2)
              resultBodyM (map Var th_res)
          )
          (resultBodyM $ map (Var . paramName) par_r)
  runBinderT' $ do
    let map_lam = Lambda (par_i : par_x ++ par_r) (mkBody lam_stms (map Var cs)) eltps
    iota <- letExp "iota" $ BasicOp $ Iota n (intConst Int64 0) (intConst Int64 1) Int64
    letTupExp "scan_contribs" $ Op (Screma n (ScremaForm [] [] map_lam) (iota : xs ++ rs))
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
addLambda tp@(Array (ElemPrim t) (Shape (s : ss)) u) = do
  xs <- newVName "xs"
  ys <- newVName "ys"
  let t' = if null ss then Prim t else Array (ElemPrim t) (Shape ss) u
  lam <- addLambda t'
  body <- insertStmsM $ do
    res <- letSubExp "lam_map" $ Op $ Screma s (mapSOAC lam) [xs, ys]
    return $ resultBody [res]
  pure
    Lambda
      { lambdaParams = [Param xs tp, Param ys tp],
        lambdaReturnType = [tp],
        lambdaBody = body
      }
addLambda t =
  error $ "addLambda: " ++ pretty t

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

instance Adjoint VName where
  lookupAdj v = do
    maybeAdj <- gets $ M.lookup v . stateAdjs
    case maybeAdj of
      Nothing -> newAdj v
      Just v_adj -> return v_adj

  updateAdjoint v d = do
    maybeAdj <- gets $ M.lookup v . stateAdjs
    case maybeAdj of
      Nothing -> setAdjoint v $ BasicOp $ SubExp $ Var d
      Just v_adj -> do
        v_adj_t <- lookupType v_adj
        case v_adj_t of
          Acc {} -> do
            dims <- arrayDims <$> lookupType d
            v_adj_arr <-
              letExp (baseString v_adj <> "_arr") $
                BasicOp $ Replicate (Shape dims) $ Var v_adj
            ~[v_adj_arr'] <-
              tabNest (length dims) [v_adj_arr, d] $ \is [v_adj_arr', d'] ->
                letTupExp "acc" $
                  BasicOp $ UpdateAcc v_adj_arr' (map Var is) [Var d']
            v_adj' <-
              letExp (baseString v <> "_adj") $ BasicOp $ JoinAcc v_adj_arr'
            insAdj v v_adj'
            pure v_adj'
          _ -> do
            v_adj' <- letExp (baseString v <> "_adj") =<< addExp v_adj d
            insAdj v v_adj'
            pure v_adj'

  updateAdjointSlice slice v d = do
    maybeAdj <- gets $ M.lookup v . stateAdjs
    t <- lookupType v
    case maybeAdj of
      Nothing -> do
        void $ lookupAdj v -- Initialise adjoint.
        updateAdjointSlice slice v d
      Just v_adj -> do
        let isDimFix (DimFix i) = i
            isDimFix _ =
              error $ "Invalid slice for accumulator update: " ++ pretty slice
        v_adj_t <- lookupType v_adj
        v_adj' <- case v_adj_t of
          Acc {} ->
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
    JoinAcc {} -> error "Reverse-mode JoinAcc not handled yet."
    UpdateAcc {} -> error "Reverse-mode UpdateAcc not handled yet."

diffSOAC :: Pattern -> StmAux () -> SOAC SOACS -> ADM () -> ADM ()
-- special reduce case: + or (map +)
diffSOAC pat@(Pattern [] [pe]) aux soac@(Screma _ form [as]) m
  | Just [red] <- isReduceSOAC form,
    lam <- redLambda red,
    Just _ <- isAddTowLam lam = do
    addStm $ Let pat aux $ Op soac
    m
    pe_bar <- lookupAdj $ patElemName pe
    as_tp <- lookupType as
    v_rep <- letExp "v_rep" $ BasicOp $ Replicate (shpFstDim $ arrayShape as_tp) $ Var pe_bar
    void $ updateAdjoint as v_rep
  where
    --let [v_rep] = v_reps
    --void $ updateAdjointSlice (fullSlice as_tp []) as v_rep

    shpFstDim (Shape []) = error "error in shpFstDim in Rev.hs"
    shpFstDim (Shape (d : _)) = Shape [d]
-- special histo case: + or (map +)
-- ToDo: not sure if correct since we do not consider what was already inside the histogram!!!
diffSOAC pat@(Pattern [] [pe]) aux (Hist n [hist_add] f [is, vs]) m
  | isIdentityLambda f,
    HistOp w rf [orig_dst] [ne] add_lam <- hist_add,
    Just _ <- isAddTowLam add_lam = do
    -- need to create a copy of the orig histo, because the reverse trace might need
    -- the values of the original histogram input!
    dst_cpy <- letExp (baseString orig_dst ++ "_copy") $ BasicOp $ Copy $ orig_dst
    let histo' = Hist n [HistOp w rf [dst_cpy] [ne] add_lam] f [is, vs]
    addStm $ Let pat aux $ Op histo'
    m
    let eltp = head $ lambdaReturnType add_lam
    pe_bar <- lookupAdj $ patElemName pe
    -- already update orig_dst bar
    void $ updateAdjoint orig_dst pe_bar
    -- update the vs bar; create a map nest with the branch innermost so all
    -- parallelism can be exploited.
    pind <- newParam "ind" (Prim p_int64)
    map_bar_lam_bdy <- genIdxLamBdy pe_bar [(w,pind)] eltp
    let map_bar_lam = Lambda [pind] map_bar_lam_bdy [eltp]
    vs_bar <- letExp (baseString vs ++ "_bar") $ Op $ Screma n (mapSOAC map_bar_lam) [is]
    void $ updateAdjoint vs vs_bar
--
-- Special reduce case: p = reduce (*) 1 as
-- Forward trace:
--    let (nz_prod, zero_count) =
--      reduce (*,+) (1,0) o map (\a -> if a==0 then (1,1) else (a,0)) as
--    let p = if zero_count > 0 then 0 else nz_prod
-- Reverse trace:
--    let as_bar = map (\a -> if zero_count == 0 then (nz_prod / a) * p_bar
--                            else if zero_count == 1 && a == 0
--                                 then nz_prod * p_bar
--                                 else 0
--                     ) as
diffSOAC (Pattern [] [pe]) aux (Screma w form [as]) m
  | Just [red] <- isReduceSOAC form,
    lam <- redLambda red,
    Just bop <- isMulLam lam,
    [eltp] <- lambdaReturnType lam,
    Prim ptp <- eltp = do
    -- Forward pass modifies the reduce (*) original soac to compute the
    --     product of non-zero elements and the number of zero elements.
    -- create the redomap soac:
    (map_lam, red_lam) <- helperMulOp1 ptp bop
    nz_prod <- newVName "non_zero_prod"
    zr_count <- newVName "zero_count"
    let red_frm = Reduce Commutative red_lam [Constant $ onePrimValue ptp, intConst Int64 0]
    let soac_pat = Pattern [] [PatElem nz_prod eltp, PatElem zr_count $ Prim p_int64]
    let soac_exp = Op $ Screma w (ScremaForm [] [red_frm] map_lam) [as]
    auxing aux $ letBind soac_pat soac_exp
    -- addStm soac_stm
    -- still on forward trace: insert an if statement to recover the zero product
    if_stms <- helperMulOp2 ptp nz_prod zr_count (patElemName pe)
    addStms if_stms
    m
    -- Reverse trase requires a map (see opening comment)
    prod_bar <- lookupAdj $ patElemName pe
    farg_orig <- newParam "arg" eltp
    map_bar_lam_bdy <- helperMulOp3 ptp bop nz_prod zr_count farg_orig prod_bar
    let map_bar_lam = Lambda [farg_orig] map_bar_lam_bdy [eltp]
    as_bar <- letTupExp "as_bar" $ Op $ Screma w (ScremaForm [] [] map_bar_lam) [as]
    zipWithM_ updateAdjoint [as] as_bar
-- special histo case: *
diffSOAC (Pattern [] [pe]) aux (Hist n [hist_mul] f [is, vs]) m
  | isIdentityLambda f,
    HistOp w rf [orig_dst] [ne] mul_lam <- hist_mul,
    Just mulop <- isMulLam mul_lam,
    [eltp] <- lambdaReturnType mul_lam,
    Prim ptp <- eltp = do
    -- starts here:
    let pe_tp = patElemDec pe
    (map_lam, _) <- helperMulOp1 ptp mulop
    vs_lift <- letTupExp "nzel_zrct" $ Op $ Screma n (ScremaForm [] [] map_lam) [vs]
    let [nz_vs, one_zrs] = vs_lift
    zr_counts0 <- letExp "zr_cts" $ BasicOp $ Replicate (Shape [w]) (intConst Int64 0)
    nz_prods0 <- letExp "nz_prd" $ BasicOp $ Replicate (Shape [w]) ne
    nz_prods <- newVName "non_zero_prod"
    zr_counts <- newVName "zero_count"
    lam_add <- mkLamAddI64
    let hist_zrn = HistOp w rf [zr_counts0] [intConst Int64 0] lam_add
    let hist_nzp = HistOp w rf [nz_prods0] [ne] mul_lam
    f' <- mkIdentityLambda [Prim p_int64, Prim p_int64, eltp, Prim p_int64]
    let soac_pat =
          Pattern
            []
            [ PatElem nz_prods pe_tp,
              PatElem zr_counts $
                Array (ElemPrim p_int64) (Shape [w]) NoUniqueness
            ]
    let soac_exp = Op $ Hist n [hist_nzp, hist_zrn] f' [is, is, nz_vs, one_zrs]
    auxing aux $ letBind soac_pat soac_exp
    -- construct the histo result:
    res_part <- newVName "res_part"
    ps2 <- zipWithM newParam ["nz_pr", "zr_ct"] [eltp, Prim p_int64]
    let [nz_prod, zr_count] = map paramName ps2
    if_stms <- helperMulOp2 ptp nz_prod zr_count res_part
    lam_bdy_2 <- runBodyBinder . localScope (scopeOfLParams ps2) $ do
      addStms if_stms
      resultBodyM [Var res_part]
    h_part <-
      letExp "hist_part" $
        Op $
          Screma
            w
            (ScremaForm [] [] (Lambda ps2 lam_bdy_2 [eltp]))
            [nz_prods, zr_counts]
    ps3 <- zipWithM newParam ["h_orig", "h_part"] [eltp, eltp]
    let [ph_orig, ph_part] = map paramName ps3
    lam_pe_bdy <- runBodyBinder . localScope (scopeOfLParams ps3) $ do
      r <- letSubExp "res" $ BasicOp $ BinOp mulop (Var ph_orig) (Var ph_part)
      resultBodyM [r]
    auxing aux $
      letBind (Pattern [] [pe]) $
        Op $
          Screma
            w
            (ScremaForm [] [] (Lambda ps3 lam_pe_bdy [eltp]))
            [orig_dst, h_part]
    m
    -- reverse trace
    pe_bar <- lookupAdj $ patElemName pe
    -- updates the orig_dst with its proper bar
    mul_lam' <- renameLambda mul_lam
    orig_bar <-
      letTupExp (baseString orig_dst ++ "_bar") $
        Op $
          Screma
            w
            (ScremaForm [] [] mul_lam')
            [h_part, pe_bar]
    zipWithM_ updateAdjoint [orig_dst] orig_bar
    -- updates the partial histo result with its proper bar
    mul_lam'' <- renameLambda mul_lam
    part_bars <-
      letTupExp (baseString h_part ++ "_bar") $
        Op $
          Screma
            w
            (ScremaForm [] [] mul_lam'')
            [orig_dst, pe_bar]
    let [part_bar] = part_bars
    -- add the contributions to each array element
    pj <- newParam "j" (Prim p_int64)
    pv <- newParam "v" eltp
    let j = paramName pj
    ((zr_cts, pr_bar, nz_prd), tmp_stms) <- runBinderT' . localScope (scopeOfLParams [pj, pv]) $ do
      zr_cts <- letExp "zr_cts" $ BasicOp $ Index zr_counts $ DimFix (Var j) : fullSlice eltp []
      pr_bar <- letExp "pr_bar" $ BasicOp $ Index part_bar $ DimFix (Var j) : fullSlice eltp []
      nz_prd <- letExp "nz_prd" $ BasicOp $ Index nz_prods $ DimFix (Var j) : []
      return (zr_cts, pr_bar, nz_prd)
    bdy_tmp <- helperMulOp3 ptp mulop nz_prd zr_cts pv pr_bar
    lam_bar <-
      runBodyBinder . localScope (scopeOfLParams [pj, pv]) $
        eBody
          [ eIf
              ( toExp $ withinBounds [(w,j)] )
              ( do
                  addStms (tmp_stms <> bodyStms bdy_tmp)
                  resultBodyM (bodyResult bdy_tmp)
              )
              ( resultBodyM [Constant $ blankPrimValue ptp] )
          ]
    vs_bar <-
      letTupExp (baseString vs ++ "_bar") $
        Op $
          Screma
            n
            (ScremaForm [] [] (Lambda [pj, pv] lam_bar [eltp]))
            [is, vs]
    zipWithM_ updateAdjoint [vs] vs_bar
  where
    mkLamAddI64 = do
      pab <- zipWithM newParam ["a", "b"] [Prim p_int64, Prim p_int64]
      let [a, b] = map paramName pab
      let addop = (Add Int64 OverflowUndef)
      lam_bdy <- runBodyBinder . localScope (scopeOfLParams pab) $ do
        r <- letSubExp "r" $ BasicOp $ BinOp addop (Var a) (Var b)
        resultBodyM [r]
      return $ Lambda pab lam_bdy [Prim p_int64]
--
-- Special case of reduce with min/max:
--    let m = reduce minmax ne_min as
-- Forward trace (assuming w = length as):
--    let (m_tmp, m_ind) = map2 (\ v i -> (v,i)) as (iota w)
--      |> reduce (\ acc_v acc_i v i ->
--                    if (acc_v == v) then (acc_v, min acc_i, i)
--                    else if (acc_v == minmax acc_v v)
--                         then (acc_v, acc_i)
--                         else (v, i)
--                ) (ne_min, -1)
--    let m = m_tmp
-- Reverse trace:
--    num_elems = i64.bool not (m_ind == -1)
--    m_bar_repl = replicate num_elems m_bar
--    as_bar[m_ind:num_elems:1] += m_bar_repl
diffSOAC (Pattern [] [pe]) aux (Screma w form [as]) m
  | Just [red] <- isReduceSOAC form,
    lam <- redLambda red,
    nes <- redNeutral red,
    Just bop <- isMinMaxLam lam,
    [eltp] <- lambdaReturnType lam,
    Prim ptp <- eltp = do
    -- primal trace constructs redomap
    map_lam <- mkIdentityLambda [eltp, Prim p_int64]
    red_lam <- mkMinMaxIndLam ptp bop
    let red_frm = Reduce Commutative red_lam (nes ++ [intConst Int64 (-1)])
    iota <- letExp "iota" $ BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
    mm_ind <- newVName "mm_ind"
    let soac_pat = Pattern [] [pe, PatElem mm_ind $ Prim p_int64]
    auxing aux $ letBind soac_pat $ Op $ Screma w (ScremaForm [] [red_frm] map_lam) [as, iota]
    m
    -- reverse trace is really simple, but we need to take care of the
    --   empty-array case => we create a possibly empty slice to update.
    pe_bar <- lookupAdj $ patElemName pe
    num_elems <- letExp "num_elems" =<< toExp (ConvOpExp (BToI Int64) $ UnOpExp Not $ mind_eq_min1 mm_ind)
    pe_bar_repl <-
      letExp (baseString pe_bar ++ "_repl") $
        BasicOp $ Replicate (Shape [Var num_elems]) $ Var pe_bar
    let as_slice = [DimSlice (Var mm_ind) (Var num_elems) (intConst Int64 1)]
    void $ updateAdjointSlice as_slice as pe_bar_repl
--
-- special case of histogram with min/max as operator.
-- we use max for notation but should hold for both.
-- Original, assuming `is: [n]i64` and `histo_orig: [w]btp`
--     let histo = reduce_by_index histo_orig max ne is vs
-- Forward Sweep:
--     let histo_orig' = copy histo_orig
--     let (histo, histo_inds) = map3 (\i v j -> (i,(v,j))) is vs (iota n)
--                       |> reduce_by_index histo_orig' max_ind_op (ne,-1)
--     where `max_ind_op` is constructed similarly to `reduce max/min`
-- Reverse Sweep knowns `histo_bar` and partially `vs_bar`;  
--  it first initializes the bar of `histo_orig` to:
--     histo_orig_bar <- map (\m_ind el -> if m_ind == -1 then el else 0
--                           ) histo_inds histo_bar
--   and then economically updates vs_bar:
--     vs_ctrbs = map2 (\ i h_v -> if i == -1 then 0 else vs_bar[i]+h_v) histo_inds histo_bar 
--     vs_bar <- scatter vs_bar histo_inds vs_ctrbs
diffSOAC (Pattern [] [pe]) aux (Hist n [hist_max] f [is, vs]) m
  | isIdentityLambda f,
    HistOp w rf [orig_dst] [ne] max_lam <- hist_max,
    Just bop <- isMinMaxLam max_lam,
    [eltp] <- lambdaReturnType max_lam,
    Prim ptp <- eltp = do
    -- forward sweep makes a copy of the consumed array and computes a lifted histo
    orig_dst_cpy <- letExp (baseString orig_dst++"_cpy") $ BasicOp $ Copy orig_dst
    f' <- mkIdentityLambda [Prim p_int64, eltp, Prim p_int64]
    repl <- letExp "minus_ones" $ BasicOp $ Replicate (Shape [w]) (intConst Int64 (-1))
    maxind_lam <- mkMinMaxIndLam ptp bop
    let hist_op = HistOp w rf [orig_dst_cpy, repl] [ne, intConst Int64 (-1)] maxind_lam
    iota_n <- letExp "iota_n" $ BasicOp $ Iota n (intConst Int64 0) (intConst Int64 1) Int64
    hist_inds <- newVName "hist_inds"
    let histo_pat = Pattern [] [pe, PatElem hist_inds $ mkI64ArrType w]
    auxing aux $ letBind histo_pat $ Op $ Hist n [hist_op] f' [is, vs, iota_n]
    m
    -- reverse sweep:
    pe_bar <- lookupAdj $ patElemName pe
    -- create the bar of `orig_dst` by means of a map:
    pis_h <- zipWithM newParam ["min_ind", "h_elem"] [Prim p_int64, eltp]
    let [min_ind_h, h_elem_h] = map paramName pis_h
    lam_bdy_hist_bar <-
      runBodyBinder . localScope (scopeOfLParams pis_h) $
        eBody
          [ eIf
              ( toExp $ mind_eq_min1 min_ind_h )
              ( resultBodyM [Var h_elem_h] )
              ( resultBodyM [Constant $ blankPrimValue ptp] )
          ]
    let lam_hist_bar = Lambda pis_h lam_bdy_hist_bar [eltp]
    hist_bar <- letExp  (baseString orig_dst ++ "_bar") $ Op $
                        Screma w (ScremaForm [] [] lam_hist_bar) [hist_inds, pe_bar]
    insAdj orig_dst hist_bar
    -- update vs_bar with a map and a scatter
    vs_bar  <- lookupAdj vs
    pis_v <- zipWithM newParam ["min_ind", "h_elem"] [Prim p_int64, eltp]
    let [min_ind_v, h_elem_v] = map paramName pis_v
    lam_bdy_vs_bar <-
      runBodyBinder . localScope (scopeOfLParams pis_v) $
        eBody
          [ eIf
              ( toExp $ mind_eq_min1 min_ind_v )
              ( resultBodyM [Constant $ blankPrimValue ptp] )
              ( do vs_bar_i <- letSubExp (baseString vs_bar ++ "_el") $ BasicOp $
                                            Index vs_bar [DimFix $ Var min_ind_v]
                   let plus_op = getBinOpPlus ptp
                   r <- letSubExp "r" $ BasicOp $ BinOp plus_op vs_bar_i $ Var h_elem_v
                   resultBodyM [r]
              )
          ]
    let lam_vs_bar = Lambda pis_v lam_bdy_vs_bar [eltp]
    vs_bar_p <- letExp (baseString vs_bar ++ "_partial") $ Op $
                      Screma w (ScremaForm [] [] lam_vs_bar) [hist_inds, pe_bar]
    f'' <- mkIdentityLambda [Prim p_int64, eltp]
    let scatter_soac = Scatter w  f'' [hist_inds, vs_bar_p] [(Shape [n],1,vs_bar)]
    vs_bar' <- letExp (baseString vs ++ "_bar") $ Op scatter_soac
    insAdj vs vs_bar'
    where
      mkI64ArrType len = Array (ElemPrim p_int64) (Shape [len]) NoUniqueness
-- no other histograms supported for now!
diffSOAC _ _ (Hist _ _ _ _) _ = error "AD, Rev.hs, unsupported histogram case!"
--
-- Differentiating a general single reduce:
--    let y = reduce \odot ne xs
-- Forward sweep:
--    let ls = scan_exc \odot  ne xs
--    let rs = scan_exc \odot' ne (reverse xs)
-- Reverse sweep:
--    let xs_c = map3 (f_bar y_bar) ls as (reverse rs)
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
    lam <- renameLambda lam0
    lam' <- renameLambda lam0 {lambdaParams = flipParams $ lambdaParams lam0}
    par_i <- newParam "i" $ Prim int64
    -- phase1: build the scanomap soac
    g_bdy <- mkFusedMapBody par_i ne alphas
    let g_lam = Lambda [par_i] g_bdy (alphas ++ alphas)
    (r_scan, scan_stms) <- runBinderT' $ do
      iota <- letExp "iota" $ BasicOp $ Iota n (intConst Int64 0) (intConst Int64 1) Int64
      letTupExp "adj_ctrb_scan" $ Op (Screma n (ScremaForm [Scan lam ne, Scan lam' ne] [] g_lam) [iota])
    addStms scan_stms
    let (ls_arr, rs_arr) = splitAt (length ne) r_scan
    -- phase2: build the following map:
    (as_params, f) <- mkF $ redLambda red
    f_rev <- diffLambda ys_adj as_params f
    par_i' <- newParam "i" $ Prim int64
    let i' = paramName par_i'
        (ls_as_ps, rs_ps) = splitAt (2 * length ne) $ lambdaParams f_rev
        scp_params = par_i' : ls_as_ps ++ zipWith Param ys_adj alphas
    f_adj_body <- runBodyBinder . localScope (scopeOfLParams scp_params) $ do
      nmim1 <- letSubExp "n_i_1" =<< toExp (pe64 n - (le64 i' + pe64 (intConst Int64 1)))
      let idx_stms = map (mkIdxStm nmim1) $ zip3 alphas rs_arr $ map paramName rs_ps
      addStms (stmsFromList idx_stms)
      pure $ lambdaBody f_rev
    let map_lam = Lambda (par_i' : ls_as_ps) f_adj_body alphas
    (r_maps, map_stms) <- runBinderT' $ do
      iota <- letExp "iota" $ BasicOp $ Iota n (intConst Int64 0) (intConst Int64 1) Int64
      letTupExp "adj_ctrb_scan" $ Op (Screma n (ScremaForm [] [] map_lam) (iota : ls_arr ++ xs))
    addStms map_stms
    -- finally add contributions to the adjoint of xs
    zipWithM_ updateAdjoint xs r_maps
  where
    flipParams ps = uncurry (flip (++)) $ splitAt (length ps `div` 2) ps
    mkFusedMapBody par_i ne alphas = do
      let i = paramName par_i
      runBodyBinder . localScope (scopeOfLParams [par_i]) $
        eBody
          [ eIf
              (toExp $ le64 i .>. pe64 (intConst Int64 0))
              ( do
                  ((rs1, rs2), stms) <- runBinderT' . localScope (scopeOfLParams [par_i]) $ do
                    im1 <- letSubExp "i_1" =<< toExp (le64 i - pe64 (intConst Int64 1))
                    nmi <- letSubExp "n_i" =<< toExp (pe64 n - le64 i)
                    tmp <- forM (zip xs alphas) $ \(x, t) -> do
                      x1 <- letSubExp (baseString x ++ "_elem_1") $ BasicOp $ Index x $ DimFix im1 : fullSlice t []
                      x2 <- letSubExp (baseString x ++ "_elem_2") $ BasicOp $ Index x $ DimFix nmi : fullSlice t []
                      return (x1, x2)
                    return (unzip tmp)
                  addStms stms
                  resultBodyM (rs1 ++ rs2)
              )
              (resultBodyM (ne ++ ne))
          ]
    mkIdxStm idx (t, r_arr, r) =
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
      pure (map paramName aps, Lambda (lps <> aps <> rps) body $ lambdaReturnType lam)
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
-- special case: scan with vectorized plus
diffSOAC pat@(Pattern [] [pe]) aux soac@(Screma n form [as]) m
  | Just [scn] <- isScanSOAC form,
    lam <- scanLambda scn,
    Just _ <- isAddTowLam lam = do
    addStm $ Let pat aux $ Op soac
    m
    pe_bar <- lookupAdj $ patElemName pe
    as_tp <- lookupType as
    as_bar <- eReverse =<< (mkScan as_tp scn) =<< eReverse pe_bar
    --void $ updateAdjointSlice (fullSlice as_tp []) as as_bar
    void $ updateAdjoint as as_bar
  where
    mkScan xs_tp scn xs = do
      f' <- mkIdentityLambda [stripArray 1 xs_tp]
      lam' <- renameLambda (scanLambda scn)
      let form' = ScremaForm [scn {scanLambda = lam'}] [] f'
      rs <- letTupExp (baseString xs ++ "_scaned") $ Op $ Screma n form' [xs]
      return $ head rs
--
-- General scan implem "should" handle associative ops on tuples and arrays
-- Only single scan supported for now:  ys = scan odot xs
diffSOAC pat@(Pattern [] pes) aux soac@(Screma n (ScremaForm [scn] [] f) xs) m
  | isIdentityLambda f = do
    addStm $ Let pat aux $ Op soac
    m
    let ys = map patElemName pes
    ys_adj <- mapM lookupAdj ys
    map1_lam <- mkScanFusedMapLam n (scanLambda scn) xs ys ys_adj
    scans_lin_fun_o <- mapM mkScanLinFunO $ lambdaReturnType $ scanLambda scn
    (r_scan, scan_stms) <- runBinderT' $ do
      iota <- letExp "iota" $ BasicOp $ Iota n (intConst Int64 0) (intConst Int64 1) Int64
      letTupExp "adj_ctrb_scan" $ Op (Screma n (ScremaForm scans_lin_fun_o [] map1_lam) [iota])
    addStms scan_stms
    (red_nms, snd_maps) <- unzip <$> mapM (mkScan2ndMaps n) (zip3 (map patElemDec pes) ys_adj (chunk2 r_scan))
    addStms (mconcat snd_maps)
    (xs_contribs, fin_map_stms) <- mkScanFinalMap n (scanLambda scn) xs ys red_nms
    addStms fin_map_stms
    zipWithM_ updateAdjoint xs xs_contribs
--
-- Original:
--   let ys = scatter xs is vs
-- Assumes no duplicate indices in `is`
-- Forward Sweep:
--   let xs_save = gather xs is
--   let ys = scatter xs is vs
-- Backward Sweep:
--   let vs_ctrbs = gather is ys_bar
--   let vs_bar \overline{+}= vs_ctrbs -- by map or generalized reduction
--   let xs_bar = scatter ys_bar is \overline{0}
--   let xs = scatter ys is xs_save
diffSOAC pat@(Pattern [] [pys]) aux scat@(Scatter n f0 ass [(shp, num_vals, xs)]) m
  | isIdentityLambda f0 = do
    let rank = length $ shapeDims shp
        (all_inds, val_as) = splitAt (rank * num_vals) ass
        inds_as = chunk rank all_inds
    tp_xs <- lookupType xs
    let ptp = getPrimElemType tp_xs
    -- computing xs_save
    xs_saves <- mkGather inds_as xs tp_xs
    -- performing the scatter
    addStm $ Let pat aux $ Op scat
    m
    let ys = patElemName pys
    ys_bar <- lookupAdj ys
    -- computing vs_ctrbs and updating vs_bar
    vs_ctrbs <- mkGather inds_as ys_bar tp_xs
    zipWithM_ updateAdjoint val_as vs_ctrbs -- use Slice?
    -- creating xs_bar
    let zero = Constant $ blankPrimValue $ getPrimElemType tp_xs
    zeros <- forM val_as $ \_ -> letExp "zeros" $ BasicOp $ Replicate (Shape [n]) zero
    let f_tps = replicate (rank * num_vals) (Prim p_int64) ++ replicate num_vals (Prim ptp)
    f <- mkIdentityLambda f_tps
    let soac = Scatter n f (all_inds ++ zeros) [(shp, num_vals, ys_bar)]
    xs_bar <- letExp (baseString xs ++ "_bar") $ Op soac
    insAdj xs xs_bar -- reusing the ys_bar for xs_bar!
    -- re-creating xs -- here we will unsafely use the same name as in fwd sweep:
    f' <- mkIdentityLambda f_tps
    addStm $
      Let (Pattern [] [PatElem xs tp_xs]) aux $
        Op $
          Scatter n f' (all_inds ++ xs_saves) [(shp, num_vals, ys)]
  where
    chunk _ [] = []
    chunk k lst = take k lst : chunk k (drop k lst)
    -- Creates a potential map-nest that indexes in full the array,
    --   and applies the condition of indices within bounds at the
    --   deepest level in the nest so that everything can be parallel.
    mkGather :: [[VName]] -> VName -> Type -> ADM [VName]
    mkGather inds_as arr tp_arr = do
      let ptp = getPrimElemType tp_arr
      ips <- forM inds_as $ \idxs ->
        mapM (\idx -> newParam (baseString idx ++ "_elem") (Prim p_int64)) idxs
      let params = Param arr tp_arr : concat ips
      rss_stmss <- forM ips $ \idxs -> do
          let q = length idxs
          let (ws, eltp) = (take q $ arrayDims tp_arr, stripArray q tp_arr)
          cur_bdy <- genIdxLamBdy arr (zip ws idxs) eltp
          runBinderT' . localScope (scopeOfLParams params) $ bodyBind cur_bdy
      let (rss, stmss) = unzip rss_stmss
          (rs, stms) = (concat rss, mconcat stmss)
          gather_lam = 
            if length rs == num_vals
            then Lambda (concat ips) (mkBody stms rs) (replicate num_vals (Prim ptp))
            else error "In Rev.hs, helper mkGather, unreachable case reached!"
      localScope (scopeOfLParams [Param arr tp_arr]) $ do
        let soac = Screma n (mapSOAC gather_lam) (concat inds_as)
        letTupExp (baseString arr ++ "_gather") $ Op soac
-- scatter dispatcher
diffSOAC (Pattern [] pes) aux (Scatter n f0 ass written_info) m
  | isIdentityLambda f0 = do
    let sind = splitInd written_info
        (inds, vals) = splitAt sind ass
    lst_stms <- chunkScatterInps (inds, vals) (zip pes written_info)
    diffScatters (stmsFromList lst_stms)
  where
    splitInd [] = 0
    splitInd ((shp, num_res, _) : rest) =
      num_res * length (shapeDims shp) + splitInd rest
    chunkScatterInps (acc_inds, acc_vals) [] =
      case (acc_inds, acc_vals) of
        ([], []) -> return []
        _ -> error "In Rev.hs, chunkScatterInps, unreachable case reached!"
    chunkScatterInps
      (acc_inds, acc_vals)
      ((pe, info@(shp, num_vals, _)) : rest) = do
        let num_inds = num_vals * length (shapeDims shp)
            (curr_inds, other_inds) = splitAt num_inds acc_inds
            (curr_vals, other_vals) = splitAt num_vals acc_vals
        vtps <- mapM lookupType curr_vals
        f <- mkIdentityLambda (replicate num_inds (Prim p_int64) ++ vtps)
        let stm =
              Let (Pattern [] [pe]) aux $
                Op $
                  Scatter n f (curr_inds ++ curr_vals) [info]
        stms_rest <- chunkScatterInps (other_inds, other_vals) rest
        return $ stm : stms_rest
    diffScatters all_stms
      | Just (stm, stms) <- stmsHead all_stms =
        diffStm stm $ diffStms stms
      | otherwise = m
diffSOAC (Pattern [] _) _ (Scatter _ f0 _ _) _
  | not (isIdentityLambda f0) =
    error "In Rev.hs, diffSOAC, scatter with non-identity map is not supported!"
diffSOAC _ _ (Scatter _ _ _ _) _ =
  error "In Rev.hs, diffSOAC, scatter: unreachable case reached!"
-- a map translates to generalized reduction
diffSOAC pat@(Pattern [] pes) aux soac@(Screma w form as) m
  | Just lam <- isMapSOAC form = do
    addStm $ Let pat aux $ Op soac
    m
    let fvs = namesToList $ freeIn lam
    fvs_adj <- mapM lookupAdj fvs
    fvs_adj_tp <- mapM lookupType fvs_adj
    let fvs_all = zip3 fvs_adj_tp fvs fvs_adj
        fvs_scal = map (\(x, y, _) -> (x, y)) $ filter (\(t_adj, _, _) -> primType t_adj) fvs_all
        _fvs_acc = filter (\(t_adj, _, _) -> accType t_adj) fvs_all
        _fvs_arr = filter (\(t_adj, _, _) -> arrType t_adj) fvs_all
    _map_fv_scal <- forM fvs_scal $ \(_, fv) -> do
      let repl_exp = BasicOp $ Replicate (Shape [w]) $ Var fv
      letExp (baseString fv ++ "_adj_repl") repl_exp
    lam_ps_scal <- mapM (newParam "lam_fv_scal") (map fst fvs_scal)
    let lam_scal0 = lam {lambdaParams = lambdaParams lam ++ lam_ps_scal}
        substs = M.fromList $ zip (map snd fvs_scal) $ map paramName lam_ps_scal
        lam_scal = substituteNames substs lam_scal0

    ys_adj <- mapM lookupAdj $ map patElemName pes
    lam_adj_ps <- mapM (newParam "lam_adj" . rowType . patElemDec) pes
    lam' <- renameLambda lam_scal
    lam_rev0 <-
      localScope (scopeOfLParams lam_adj_ps) $
        diffLambda (map paramName lam_adj_ps) (map paramName (lambdaParams lam')) lam'
    let lam_ps_scal0' = take (length lam_ps_scal) $ drop (length $ lambdaParams lam) $ lambdaParams lam_rev0
        substs' = M.fromList $ zip (map paramName lam_ps_scal0') (map snd fvs_scal)
        lam_rev = substituteNames substs' lam_rev0
        lam_pars = take (length $ lambdaParams lam) $ lambdaParams lam_rev
        lam_rtp = map paramType $ lambdaParams lam_scal0
    let lam_rev' = Lambda (lam_pars ++ lam_adj_ps) (lambdaBody lam_rev) lam_rtp
    --map paramType $ lambdaParams lam'
    contribs <- letTupExp "map_adjs" $ Op $ Screma w (mapSOAC lam_rev') $ as ++ ys_adj
    let (ctrb_pure, ctrb_other0) = splitAt (length as) contribs
        (ctrb_scal0, _ctrb_other) = splitAt (length lam_ps_scal) ctrb_other0
    ctrb_scal <- forM (zip fvs_scal ctrb_scal0) $ \((tp, fv), ctrb) -> do
      --Screma n form xs
      red <- mkReducePlus (prmTypeRep tp)
      id_lam <- mkIdentityLambda [tp]
      let soac_exp = Op $ Screma w (ScremaForm [] [red] id_lam) $ [ctrb]
      res <- letTupExp (baseString fv ++ "_adj_sum") soac_exp
      return $ head res
    zipWithM_ updateAdjoint (map snd fvs_scal) ctrb_scal
    -- trace ("Lam':\n" ++ pretty lam' ++ "\nLam_rev:\n"++pretty lam_rev'++"\n fv scal: "++pretty fvs_scal++" fvs_arr: "++pretty fvs_arr++" fvs acc: "++pretty fvs_acc) $
    --trace ("\n lam_rev'\n"++pretty lam_rev'++"\n substs': "++pretty substs') $
    zipWithM_ updateAdjoint as ctrb_pure
  where
    accType (Acc _ _ _) = True
    accType _ = False
    arrType (Array (ElemPrim _) _ _) = True
    arrType _ = False
    prmTypeRep (Prim ptp) = ptp
    prmTypeRep _ = error "ERROR 3333!!!"
    mkReducePlus ptp = do
      let tp = Prim ptp
      lam_ps <- zipWithM newParam ["a", "b"] [tp, tp]
      let [a, b] = map paramName lam_ps
      lam_rs <- newVName "r"
      let lam_stm = mkLet [] [Ident lam_rs tp] $ BasicOp $ BinOp (getBinOpPlus ptp) (Var a) (Var b)
          lam = Lambda lam_ps (mkBody (oneStm lam_stm) [Var lam_rs]) [Prim ptp]
      --trace ("redplus: "++pretty lam) $
      return $ Reduce Commutative lam [Constant $ blankPrimValue ptp]
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
    vs_adj <- forM (map paramName params_adj) $ \ v ->
                newVName (baseString v++"_cpy")
    let cpy_stms = stmsFromList $ map mkCopyStm $ zip vs_adj params_adj
        params_adj' = zipWith Param vs_adj ts
    Body () stms res <-
      localScope (scopeOfLParams params_adj) $
        diffBody (map paramName params_adj) (map paramName params) body
    let body' = Body () (cpy_stms <> stms) $ takeLast (length params) res
    let lam = Lambda (params ++ params_adj') body' (map paramType params)
    lam' <- renameLambda lam
    --trace ("prg:\n" ++ pretty lam' ++ "\n orig prog: \n" ++ pretty body) $
    pure lam'
-- pure $ Lambda (params ++ params_adj) body' (map paramType params)
  where
    isCopyType (Array (ElemPrim _) (Shape _) _) = True
    isCopyType _ = False
    mkCopyStm (v, Param p t) =
        if isCopyType t
        then mkLet [] [Ident p t] $ BasicOp $ Copy v
        else mkLet [] [Ident p t] $ BasicOp $ SubExp $ Var  v
{--
    mkUnique :: Type -> TypeBase (ShapeBase SubExp) Uniqueness
    mkUnique (Array (ElemPrim t) (Shape s) _) =
        Array (ElemPrim t) (Shape s) Unique
    mkUnique (Prim pt) = Prim pt
    mkUnique _ = error "In Rev.hs, mkUnique, unsupported type!"
    convType :: Type -> TypeBase (ShapeBase SubExp) Uniqueness
    convType (Array (ElemPrim t) (Shape s) _) =
        Array (ElemPrim t) (Shape s) Nonunique
    convType (Prim pt) = Prim pt
    convType _ = error "In Rev.hs, convType, unsupported type!"
--}