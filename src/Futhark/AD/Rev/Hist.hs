{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.AD.Rev.Hist
  ( diffMinMaxHist,
    diffMulHist,
    diffAddHist,
    diffVecHist,
    diffHist,
  )
where

import Control.Monad
import Futhark.AD.Rev.Monad
import Futhark.Analysis.PrimExp.Convert
import Futhark.Builder
import Futhark.IR.SOACS
import Futhark.Tools
import Futhark.Transform.Rename

getBinOpPlus :: PrimType -> BinOp
getBinOpPlus (IntType x) = Add x OverflowUndef
getBinOpPlus (FloatType f) = FAdd f
getBinOpPlus _ = error "In getBinOpMul, Hist.hs: input not supported"

getBinOpDiv :: PrimType -> BinOp
getBinOpDiv (IntType t) = SDiv t Unsafe
getBinOpDiv (FloatType t) = FDiv t
getBinOpDiv _ = error "In getBinOpDiv, Hist.hs: input not supported"

withinBounds :: [(SubExp, VName)] -> TPrimExp Bool VName
withinBounds [] = TPrimExp $ ValueExp (BoolValue True)
withinBounds [(q, i)] = (le64 i .<. pe64 q) .&&. (pe64 (intConst Int64 (-1)) .<. le64 i)
withinBounds (qi : qis) = withinBounds [qi] .&&. withinBounds qis

elseIf :: PrimType -> [(ADM (Exp SOACS), ADM (Exp SOACS))] -> [ADM (Body SOACS)] -> ADM (Exp SOACS)
elseIf t [(c1, c2)] [bt, bf] =
  eIf
    (eCmpOp (CmpEq t) c1 c2)
    bt
    bf
elseIf t ((c1, c2) : cs) (bt : bs) =
  eIf
    (eCmpOp (CmpEq t) c1 c2)
    bt
    $ eBody
    $ pure
    $ elseIf t cs bs
elseIf _ _ _ = error "In elseIf, Hist.hs: input not supported"

bindSubExpRes :: String -> [SubExpRes] -> ADM [VName]
bindSubExpRes s =
  traverse
    ( \(SubExpRes cs se) -> do
        bn <- newVName s
        certifying cs $ letBindNames [bn] $ BasicOp $ SubExp se
        pure bn
    )

nestedmap :: [SubExp] -> [PrimType] -> Lambda SOACS -> ADM (Lambda SOACS)
nestedmap [] _ lam = pure lam
nestedmap s@(h : r) pt lam = do
  params <- traverse (\tp -> newParam "x" $ Array tp (Shape s) NoUniqueness) pt
  body <- nestedmap r pt lam
  mkLambda params $
    fmap varsRes . letTupExp "res" . Op $
      Screma h (map paramName params) (mapSOAC body)

-- \ds hs -> map2 lam ds hs
mkF' :: Lambda SOACS -> [Type] -> SubExp -> ADM ([VName], [VName], Lambda SOACS)
mkF' lam tps n = do
  lam' <- renameLambda lam

  ds_params <- traverse (newParam "ds_param") tps
  hs_params <- traverse (newParam "hs_param") tps
  let ds_pars = fmap paramName ds_params
  let hs_pars = fmap paramName hs_params
  lam_map <-
    mkLambda (ds_params <> hs_params) $
      fmap varsRes . letTupExp "map_f'" . Op $
        Screma n (ds_pars <> hs_pars) (mapSOAC lam')

  pure (ds_pars, hs_pars, lam_map)

-- \ls as rs -> map3 (\li ai ri -> li `lam` ai `lam` ri) ls as rs
mkF :: Lambda SOACS -> [Type] -> SubExp -> ADM ([VName], Lambda SOACS)
mkF lam tps n = do
  lam_l <- renameLambda lam
  lam_r <- renameLambda lam
  let q = length $ lambdaReturnType lam
      (lps, aps) = splitAt q $ lambdaParams lam_l
      (ips, rps) = splitAt q $ lambdaParams lam_r
  lam' <- mkLambda (lps <> aps <> rps) $ do
    lam_l_res <- bodyBind $ lambdaBody lam_l
    forM_ (zip ips lam_l_res) $ \(ip, SubExpRes cs se) ->
      certifying cs $ letBindNames [paramName ip] $ BasicOp $ SubExp se
    bodyBind $ lambdaBody lam_r

  ls_params <- traverse (newParam "ls_param") tps
  as_params <- traverse (newParam "as_param") tps
  rs_params <- traverse (newParam "rs_param") tps
  let map_params = ls_params <> as_params <> rs_params
  lam_map <-
    mkLambda map_params $
      fmap varsRes . letTupExp "map_f" $
        Op $
          Screma n (map paramName map_params) $
            mapSOAC lam'

  pure (map paramName as_params, lam_map)

mapout :: VName -> SubExp -> SubExp -> ADM VName
mapout is n w = do
  par_is <- newParam "is" $ Prim int64
  is'_lam <-
    mkLambda [par_is] $
      fmap varsRes . letTupExp "is'"
        =<< eIf
          (toExp $ withinBounds $ pure (w, paramName par_is))
          (eBody $ pure $ eParam par_is)
          (eBody $ pure $ eSubExp w)

  letExp "is'" $ Op $ Screma n (pure is) $ mapSOAC is'_lam

multiScatter :: SubExp -> [VName] -> VName -> [VName] -> ADM [VName]
multiScatter n dst is vs = do
  tps <- traverse lookupType vs
  par_i <- newParam "i" $ Prim int64
  scatter_params <- traverse (newParam "scatter_param" . rowType) tps
  scatter_lam <-
    mkLambda (par_i : scatter_params) $
      fmap subExpsRes . mapM (letSubExp "scatter_map_res") =<< do
        p1 <- replicateM (length scatter_params) $ eParam par_i
        p2 <- traverse eParam scatter_params
        pure $ p1 <> p2

  letTupExp "scatter_res" . Op $
    Scatter n (is : vs) scatter_lam $
      zipWith (\t -> (,,) (Shape $ pure $ arraySize 0 t) 1) tps dst

multiIndex :: [VName] -> [DimIndex SubExp] -> ADM [VName]
multiIndex vs s = do
  traverse
    ( \x -> do
        t <- lookupType x
        letExp "sorted" $ BasicOp $ Index x (fullSlice t s)
    )
    vs

--
-- special case of histogram with min/max as operator.
-- Original, assuming `is: [n]i64` and `dst: [w]btp`
--     let x = reduce_by_index dst minmax ne is vs
-- Forward sweep:
--     need to copy dst: reverse sweep might use it 7
--       (see ex. in reducebyindexminmax6.fut where the first map requires the original dst to be differentiated).
--     let dst_cpy = copy dst
--     let (x, x_inds) = zip vs (iota n)
--                       |> reduce_by_index (dst_cpy,-1s) argminmax (ne,-1) is
--
-- Reverse sweep:
--     dst_bar += map2 (\i b -> if i == -1
--                              then b
--                              else 0
--                     ) x_inds x_bar

--     vs_ctrbs = map2 (\i b -> if i == -1
--                              then 0
--                              else vs_bar[i] + b
--                     ) x_inds x_bar
--     vs_bar <- scatter vs_bar x_inds vs_ctrbs
diffMinMaxHist ::
  VjpOps -> VName -> StmAux () -> SubExp -> BinOp -> SubExp -> VName -> VName -> SubExp -> SubExp -> VName -> ADM () -> ADM ()
diffMinMaxHist _ops x aux n minmax ne is vs w rf dst m = do
  let t = binOpType minmax
  vs_type <- lookupType vs
  let vs_elm_type = elemType vs_type
  let vs_dims = arrayDims vs_type
  let inner_dims = tail vs_dims
  let nr_dims = length vs_dims
  dst_type <- lookupType dst
  let dst_dims = arrayDims dst_type

  dst_cpy <-
    letExp (baseString dst <> "_copy") . BasicOp $
      Replicate mempty (Var dst)

  acc_v_p <- newParam "acc_v" $ Prim t
  acc_i_p <- newParam "acc_i" $ Prim int64
  v_p <- newParam "v" $ Prim t
  i_p <- newParam "i" $ Prim int64
  hist_lam_inner <-
    mkLambda [acc_v_p, acc_i_p, v_p, i_p] $
      fmap varsRes . letTupExp "idx_res"
        =<< eIf
          (eCmpOp (CmpEq t) (eParam acc_v_p) (eParam v_p))
          ( eBody
              [ eParam acc_v_p,
                eBinOp (SMin Int64) (eParam acc_i_p) (eParam i_p)
              ]
          )
          ( eBody
              [ eIf
                  ( eCmpOp
                      (CmpEq t)
                      (eParam acc_v_p)
                      (eBinOp minmax (eParam acc_v_p) (eParam v_p))
                  )
                  (eBody [eParam acc_v_p, eParam acc_i_p])
                  (eBody [eParam v_p, eParam i_p])
              ]
          )
  hist_lam <- nestedmap inner_dims [vs_elm_type, int64, vs_elm_type, int64] hist_lam_inner

  dst_minus_ones <-
    letExp "minus_ones" . BasicOp $
      Replicate (Shape dst_dims) (intConst Int64 (-1))
  ne_minus_ones <-
    letSubExp "minus_ones" . BasicOp $
      Replicate (Shape inner_dims) (intConst Int64 (-1))
  iota_n <-
    letExp "red_iota" . BasicOp $
      Iota n (intConst Int64 0) (intConst Int64 1) Int64

  inp_iota <- do
    if nr_dims == 1
      then pure iota_n
      else do
        i <- newParam "i" $ Prim int64
        lam <-
          mkLambda [i] $
            fmap varsRes . letTupExp "res" =<< do
              pure $ BasicOp $ Replicate (Shape inner_dims) $ Var $ paramName i

        letExp "res" $ Op $ Screma n [iota_n] $ mapSOAC lam

  let hist_op = HistOp (Shape [w]) rf [dst_cpy, dst_minus_ones] [ne, if nr_dims == 1 then intConst Int64 (-1) else ne_minus_ones] hist_lam
  f' <- mkIdentityLambda [Prim int64, rowType vs_type, rowType $ Array int64 (Shape vs_dims) NoUniqueness]
  x_inds <- newVName (baseString x <> "_inds")
  auxing aux $
    letBindNames [x, x_inds] $
      Op $
        Hist n [is, vs, inp_iota] [hist_op] f'

  m

  x_bar <- lookupAdjVal x

  x_ind_dst <- newParam (baseString x <> "_ind_param") $ Prim int64
  x_bar_dst <- newParam (baseString x <> "_bar_param") $ Prim t
  dst_lam_inner <-
    mkLambda [x_ind_dst, x_bar_dst] $
      fmap varsRes . letTupExp "dst_bar"
        =<< eIf
          (toExp $ le64 (paramName x_ind_dst) .==. -1)
          (eBody $ pure $ eParam x_bar_dst)
          (eBody $ pure $ eSubExp $ Constant $ blankPrimValue t)
  dst_lam <- nestedmap inner_dims [int64, vs_elm_type] dst_lam_inner

  dst_bar <-
    letExp (baseString dst <> "_bar") . Op $
      Screma w [x_inds, x_bar] (mapSOAC dst_lam)

  updateAdj dst dst_bar

  vs_bar <- lookupAdjVal vs

  inds' <- traverse (letExp "inds" . BasicOp . Replicate (Shape [w]) . Var) =<< mk_indices inner_dims []
  let inds = x_inds : inds'

  par_x_ind_vs <- replicateM nr_dims $ newParam (baseString x <> "_ind_param") $ Prim int64
  par_x_bar_vs <- newParam (baseString x <> "_bar_param") $ Prim t
  vs_lam_inner <-
    mkLambda (par_x_bar_vs : par_x_ind_vs) $
      fmap varsRes . letTupExp "res"
        =<< eIf
          (toExp $ le64 (paramName $ head par_x_ind_vs) .==. -1)
          (eBody $ pure $ eSubExp $ Constant $ blankPrimValue t)
          ( eBody $
              pure $ do
                vs_bar_i <-
                  letSubExp (baseString vs_bar <> "_el") . BasicOp $
                    Index vs_bar . Slice $
                      fmap (DimFix . Var . paramName) par_x_ind_vs
                eBinOp (getBinOpPlus t) (eParam par_x_bar_vs) (eSubExp vs_bar_i)
          )
  vs_lam <- nestedmap inner_dims (vs_elm_type : replicate nr_dims int64) vs_lam_inner

  vs_bar_p <-
    letExp (baseString vs <> "_partial") . Op $
      Screma w (x_bar : inds) (mapSOAC vs_lam)

  q <-
    letSubExp "q"
      =<< foldBinOp (Mul Int64 OverflowUndef) (intConst Int64 1) dst_dims

  scatter_inps <- do
    -- traverse (letExp "flat" . BasicOp . Reshape [DimNew q]) $ inds ++ [vs_bar_p]
    -- ToDo: Cosmin asks: is the below the correct translation of the line above?
    traverse (letExp "flat" . BasicOp . Reshape ReshapeArbitrary (Shape [q])) $
      inds ++ [vs_bar_p]

  f'' <- mkIdentityLambda $ replicate nr_dims (Prim int64) ++ [Prim t]
  vs_bar' <-
    letExp (baseString vs <> "_bar") . Op $
      Scatter q scatter_inps f'' [(Shape vs_dims, 1, vs_bar)]
  insAdj vs vs_bar'
  where
    mk_indices :: [SubExp] -> [SubExp] -> ADM [VName]
    mk_indices [] _ = pure []
    mk_indices [d] iotas = do
      reps <- traverse (letExp "rep" . BasicOp . Replicate (Shape [d])) iotas
      iota_d <-
        letExp "red_iota" . BasicOp $
          Iota d (intConst Int64 0) (intConst Int64 1) Int64
      pure $ reps ++ [iota_d]
    mk_indices (d : dims) iotas = do
      iota_d <-
        letExp "red_iota" . BasicOp $
          Iota d (intConst Int64 0) (intConst Int64 1) Int64

      i_param <- newParam "i" $ Prim int64
      lam <-
        mkLambda [i_param] $
          fmap varsRes $
            mk_indices dims $
              iotas ++ [Var $ paramName i_param]

      letTupExp "res" $ Op $ Screma d [iota_d] $ mapSOAC lam

--
-- special case of histogram with multiplication as operator.
-- Original, assuming `is: [n]i64` and `dst: [w]btp`
--     let x = reduce_by_index dst (*) ne is vs
-- Forward sweep:
--     dst does not need to be copied: dst is not overwritten
--     let (ps, zs) = map (\v -> if v == 0 then (1,1) else (v,0)) vs
--     let non_zero_prod = reduce_by_index nes (*) ne is ps
--     let zero_count = reduce_by_index 0s (+) 0 is zs
--     let h_part = map2 (\p c -> if c == 0 then p else 0
--                       ) non_zero_prod zero_count
--     let x = map2 (*) dst h_part
--
-- Reverse sweep:
--     dst_bar += map2 (*) h_part x_bar

--     let part_bar = map2 (*) dst x_bar
--     vs_bar += map2 (\i v -> let zr_cts = zero_count[i]
--                             let pr_bar = part_bar[i]
--                             let nz_prd = non_zero_prod[i]
--                             in if zr_cts == 0
--                             then pr_bar * (nz_prd / v)
--                             else if zr_cts == 1 and v == 0
--                             then nz_prd * pr_bar
--                             else 0
--                    ) is vs
diffMulHist ::
  VjpOps -> VName -> StmAux () -> SubExp -> BinOp -> SubExp -> VName -> VName -> SubExp -> SubExp -> VName -> ADM () -> ADM ()
diffMulHist _ops x aux n mul ne is vs w rf dst m = do
  let t = binOpType mul
  vs_type <- lookupType vs
  let vs_dims = arrayDims vs_type
  let vs_elm_type = elemType vs_type
  dst_type <- lookupType dst
  let dst_dims = arrayDims dst_type
  let inner_dims = tail vs_dims

  v_param <- newParam "v" $ Prim t
  lam_ps_zs_inner <-
    mkLambda [v_param] $
      fmap varsRes . letTupExp "map_res"
        =<< eIf
          (eCmpOp (CmpEq t) (eParam v_param) (eSubExp $ Constant $ blankPrimValue t))
          (eBody $ fmap eSubExp [Constant $ onePrimValue t, intConst Int64 1])
          (eBody [eParam v_param, eSubExp $ intConst Int64 0])
  lam_ps_zs <- nestedmap vs_dims [vs_elm_type] lam_ps_zs_inner
  ps_zs_res <- eLambda lam_ps_zs [eSubExp $ Var vs]
  ps_zs <- bindSubExpRes "ps_zs" ps_zs_res
  let [ps, zs] = ps_zs

  lam_mul_inner <- binOpLambda mul t
  lam_mul <- nestedmap inner_dims [vs_elm_type, vs_elm_type] lam_mul_inner
  nz_prods0 <- letExp "nz_prd" $ BasicOp $ Replicate (Shape [w]) ne
  let hist_nzp = HistOp (Shape [w]) rf [nz_prods0] [ne] lam_mul

  lam_add_inner <- binOpLambda (Add Int64 OverflowUndef) int64
  lam_add <- nestedmap inner_dims [int64, int64] lam_add_inner
  zr_counts0 <- letExp "zr_cts" $ BasicOp $ Replicate (Shape dst_dims) (intConst Int64 0)
  zrn_ne <- letSubExp "zr_ne" $ BasicOp $ Replicate (Shape inner_dims) (intConst Int64 0)
  let hist_zrn = HistOp (Shape [w]) rf [zr_counts0] [if length vs_dims == 1 then intConst Int64 0 else zrn_ne] lam_add

  f' <- mkIdentityLambda [Prim int64, Prim int64, rowType vs_type, rowType $ Array int64 (Shape vs_dims) NoUniqueness]
  nz_prods <- newVName "non_zero_prod"
  zr_counts <- newVName "zero_count"
  auxing aux $
    letBindNames [nz_prods, zr_counts] $
      Op $
        Hist n [is, is, ps, zs] [hist_nzp, hist_zrn] f'

  p_param <- newParam "prod" $ Prim t
  c_param <- newParam "count" $ Prim int64
  lam_h_part_inner <-
    mkLambda [p_param, c_param] $
      fmap varsRes . letTupExp "h_part"
        =<< eIf
          (toExp $ 0 .==. le64 (paramName c_param))
          (eBody $ pure $ eParam p_param)
          (eBody $ pure $ eSubExp $ Constant $ blankPrimValue t)
  lam_h_part <- nestedmap dst_dims [vs_elm_type, int64] lam_h_part_inner
  h_part_res <- eLambda lam_h_part $ map (eSubExp . Var) [nz_prods, zr_counts]
  h_part' <- bindSubExpRes "h_part" h_part_res
  let [h_part] = h_part'

  lam_mul_inner' <- binOpLambda mul t
  lam_mul' <- nestedmap dst_dims [vs_elm_type, vs_elm_type] lam_mul_inner'
  x_res <- eLambda lam_mul' $ map (eSubExp . Var) [dst, h_part]
  x' <- bindSubExpRes "x" x_res
  auxing aux $ letBindNames [x] $ BasicOp $ SubExp $ Var $ head x'

  m

  x_bar <- lookupAdjVal x

  lam_mul'' <- renameLambda lam_mul'
  dst_bar_res <- eLambda lam_mul'' $ map (eSubExp . Var) [h_part, x_bar]
  dst_bar <- bindSubExpRes (baseString dst <> "_bar") dst_bar_res
  updateAdj dst $ head dst_bar

  lam_mul''' <- renameLambda lam_mul'
  part_bar_res <- eLambda lam_mul''' $ map (eSubExp . Var) [dst, x_bar]
  part_bar' <- bindSubExpRes "part_bar" part_bar_res
  let [part_bar] = part_bar'

  inner_params <- zipWithM newParam ["zr_cts", "pr_bar", "nz_prd", "a"] $ map Prim [int64, t, t, t]
  let [zr_cts, pr_bar, nz_prd, a_param] = inner_params
  lam_vsbar_inner <-
    mkLambda inner_params $
      fmap varsRes . letTupExp "vs_bar" =<< do
        eIf
          (eCmpOp (CmpEq int64) (eSubExp $ intConst Int64 0) (eParam zr_cts))
          (eBody $ pure $ eBinOp mul (eParam pr_bar) $ eBinOp (getBinOpDiv t) (eParam nz_prd) $ eParam a_param)
          ( eBody $
              pure $
                eIf
                  ( eBinOp
                      LogAnd
                      (eCmpOp (CmpEq int64) (eSubExp $ intConst Int64 1) (eParam zr_cts))
                      (eCmpOp (CmpEq t) (eSubExp $ Constant $ blankPrimValue t) $ eParam a_param)
                  )
                  (eBody $ pure $ eBinOp mul (eParam nz_prd) (eParam pr_bar))
                  (eBody $ pure $ eSubExp $ Constant $ blankPrimValue t)
          )

  lam_vsbar_middle <- nestedmap inner_dims [int64, t, t, t] lam_vsbar_inner

  i_param <- newParam "i" $ Prim int64
  a_param' <- newParam "a" $ rowType vs_type
  lam_vsbar <-
    mkLambda [i_param, a_param'] $
      fmap varsRes . letTupExp "vs_bar"
        =<< eIf
          (toExp $ withinBounds $ pure (w, paramName i_param))
          ( buildBody_ $ do
              let i = fullSlice vs_type [DimFix $ Var $ paramName i_param]
              names <- traverse newVName ["zr_cts", "pr_bar", "nz_prd"]
              zipWithM_ (\name -> letBindNames [name] . BasicOp . flip Index i) names [zr_counts, part_bar, nz_prods]
              eLambda lam_vsbar_middle $ map (eSubExp . Var) names <> [eParam a_param']
          )
          (eBody $ pure $ pure $ zeroExp $ rowType dst_type)

  vs_bar <-
    letExp (baseString vs <> "_bar") $ Op $ Screma n [is, vs] $ mapSOAC lam_vsbar

  updateAdj vs vs_bar

--
-- special case of histogram with add as operator.
-- Original, assuming `is: [n]i64` and `dst: [w]btp`
--     let x = reduce_by_index dst (+) ne is vs
-- Forward sweep:
--     need to copy dst: reverse sweep might use it 7
--       (see ex. in reducebyindexminmax6.fut where the first map requires the original dst to be differentiated).
--     let dst_cpy = copy dst
--     let x = reduce_by_index dst_cpy (+) ne is vs
--
-- Reverse sweep:
--     dst_bar += x_bar
--
--     vs_bar += map (\i -> x_bar[i]) is
diffAddHist ::
  VjpOps -> VName -> StmAux () -> SubExp -> Lambda SOACS -> SubExp -> VName -> VName -> SubExp -> SubExp -> VName -> ADM () -> ADM ()
diffAddHist _ops x aux n add ne is vs w rf dst m = do
  let t = paramDec $ head $ lambdaParams add

  dst_cpy <-
    letExp (baseString dst <> "_copy") . BasicOp $
      Replicate mempty (Var dst)

  f <- mkIdentityLambda [Prim int64, t]
  auxing aux . letBindNames [x] . Op $
    Hist n [is, vs] [HistOp (Shape [w]) rf [dst_cpy] [ne] add] f

  m

  x_bar <- lookupAdjVal x

  updateAdj dst x_bar

  x_type <- lookupType x
  i_param <- newParam (baseString vs <> "_i") $ Prim int64
  let i = paramName i_param
  lam_vsbar <-
    mkLambda [i_param] $
      fmap varsRes . letTupExp "vs_bar"
        =<< eIf
          (toExp $ withinBounds $ pure (w, i))
          (eBody $ pure $ pure $ BasicOp $ Index x_bar $ fullSlice x_type [DimFix $ Var i])
          (eBody $ pure $ eSubExp ne)

  vs_bar <- letExp (baseString vs <> "_bar") $ Op $ Screma n [is] $ mapSOAC lam_vsbar
  updateAdj vs vs_bar

-- Special case for vectorised combining operator. Rewrite
--   reduce_by_index dst (map2 op) nes is vss
-- to
--   map3 (\dst_col vss_col ne ->
--           reduce_by_index dst_col op ne is vss_col
--        ) (transpose dst) (transpose vss) nes |> transpose
-- before differentiating.
diffVecHist ::
  VjpOps ->
  VName ->
  StmAux () ->
  SubExp ->
  Lambda SOACS ->
  VName ->
  VName ->
  VName ->
  SubExp ->
  SubExp ->
  VName ->
  ADM () ->
  ADM ()
diffVecHist ops x aux n op nes is vss w rf dst m = do
  stms <- collectStms_ $ do
    rank <- arrayRank <$> lookupType vss
    let dims = [1, 0] ++ drop 2 [0 .. rank - 1]

    dstT <- letExp "dstT" $ BasicOp $ Rearrange dims dst
    vssT <- letExp "vssT" $ BasicOp $ Rearrange dims vss
    t_dstT <- lookupType dstT
    t_vssT <- lookupType vssT
    t_nes <- lookupType nes

    dst_col <- newParam "dst_col" $ rowType t_dstT
    vss_col <- newParam "vss_col" $ rowType t_vssT
    ne <- newParam "ne" $ rowType t_nes

    f <- mkIdentityLambda (Prim int64 : lambdaReturnType op)
    map_lam <-
      mkLambda [dst_col, vss_col, ne] $ do
        -- TODO Have to copy dst_col, but isn't it already unique?
        dst_col_cpy <-
          letExp "dst_col_cpy" . BasicOp $
            Replicate mempty (Var $ paramName dst_col)
        fmap (varsRes . pure) . letExp "col_res" $
          Op $
            Hist
              n
              [is, paramName vss_col]
              [HistOp (Shape [w]) rf [dst_col_cpy] [Var $ paramName ne] op]
              f
    histT <-
      letExp "histT" $
        Op $
          Screma (arraySize 0 t_dstT) [dstT, vssT, nes] $
            mapSOAC map_lam
    auxing aux . letBindNames [x] . BasicOp $ Rearrange dims histT
  foldr (vjpStm ops) m stms

--
-- a step in the radix sort implementation
-- it assumes the key we are sorting
-- after is [n]i64 and it is the first VName
--
-- local def radix_sort_step [n] 't (xs: [n]t) (get_bit: i32 -> t -> i32)
--                                  (digit_n: i32): [n]t =
--   let num x = get_bit (digit_n+1) x * 2 + get_bit digit_n x
--   let pairwise op (a1,b1,c1,d1) (a2,b2,c2,d2) =
--     (a1 `op` a2, b1 `op` b2, c1 `op` c2, d1 `op` d2)
--   let bins = xs |> map num
--   let flags = bins |> map (\x -> if x == 0 then (1,0,0,0)
--                                  else if x == 1 then (0,1,0,0)
--                                  else if x == 2 then (0,0,1,0)
--                                  else (0,0,0,1))
--   let offsets = scan (pairwise (+)) (0,0,0,0) flags
--   let (na,nb,nc,_nd) = last offsets
--   let f bin (a,b,c,d) = match bin
--                         case 0 -> a-1
--                         case 1 -> na+b-1
--                         case 2 -> na+nb+c-1
--                         case _ -> na+nb+nc+d-1
--   let is = map2 f bins offsets
--   in scatter scratch is xs
radixSortStep :: [VName] -> [Type] -> SubExp -> SubExp -> SubExp -> ADM [VName]
radixSortStep xs tps bit n w = do
  -- let is = head xs
  is <- mapout (head xs) n w

  num_param <- newParam "num" $ Prim int64
  num_lam <-
    mkLambda [num_param] $
      fmap varsRes . letTupExp "num_res"
        =<< eBinOp
          (Add Int64 OverflowUndef)
          ( eBinOp
              (And Int64)
              (eBinOp (AShr Int64) (eParam num_param) (eSubExp bit))
              (iConst 1)
          )
          ( eBinOp
              (Mul Int64 OverflowUndef)
              (iConst 2)
              ( eBinOp
                  (And Int64)
                  (eBinOp (AShr Int64) (eParam num_param) (eBinOp (Add Int64 OverflowUndef) (eSubExp bit) (iConst 1)))
                  (iConst 1)
              )
          )

  bins <- letExp "bins" $ Op $ Screma n [is] $ mapSOAC num_lam
  flag_param <- newParam "flag" $ Prim int64
  flag_lam <-
    mkLambda [flag_param] $
      fmap varsRes . letTupExp "flag_res"
        =<< elseIf
          int64
          (map ((,) (eParam flag_param) . iConst) [0 .. 2])
          (map (eBody . fmap iConst . (\i -> map (\j -> if i == j then 1 else 0) [0 .. 3])) ([0 .. 3] :: [Integer]))

  flags <- letTupExp "flags" $ Op $ Screma n [bins] $ mapSOAC flag_lam

  scan_params <- traverse (flip newParam $ Prim int64) ["a1", "b1", "c1", "d1", "a2", "b2", "c2", "d2"]
  scan_lam <-
    mkLambda scan_params $
      fmap subExpsRes . mapM (letSubExp "scan_res") =<< do
        uncurry (zipWithM (eBinOp $ Add Int64 OverflowUndef)) $ splitAt 4 $ map eParam scan_params

  scan <- scanSOAC $ pure $ Scan scan_lam $ map (intConst Int64) [0, 0, 0, 0]
  offsets <- letTupExp "offsets" $ Op $ Screma n flags scan

  ind <- letSubExp "ind_last" =<< eBinOp (Sub Int64 OverflowUndef) (eSubExp n) (iConst 1)
  let i = Slice [DimFix ind]
  nabcd <- traverse newVName ["na", "nb", "nc", "nd"]
  zipWithM_ (\abcd -> letBindNames [abcd] . BasicOp . flip Index i) nabcd offsets

  let vars = map Var nabcd
  map_params <- traverse (flip newParam $ Prim int64) ["bin", "a", "b", "c", "d"]
  map_lam <-
    mkLambda map_params $
      fmap varsRes . letTupExp "map_res"
        =<< elseIf
          int64
          (map ((,) (eParam $ head map_params) . iConst) [0 .. 2])
          ( zipWith
              ( \j p ->
                  eBody $
                    pure $ do
                      t <- letSubExp "t" =<< eBinOp (Sub Int64 OverflowUndef) (eParam p) (iConst 1)
                      foldBinOp (Add Int64 OverflowUndef) (intConst Int64 0) (t : take j vars)
              )
              [0 .. 3]
              (tail map_params)
          )

  nis <- letExp "nis" $ Op $ Screma n (bins : offsets) $ mapSOAC map_lam

  scatter_dst <- traverse (\t -> letExp "scatter_dst" $ BasicOp $ Scratch (elemType t) (arrayDims t)) tps
  multiScatter n scatter_dst nis xs
  where
    iConst c = eSubExp $ intConst Int64 c

--
-- the radix sort implementation
-- def radix_sort [n] 't (xs: [n]i64) =
--   let iters = if n == 0 then 0 else 32
--   in loop xs for i < iters do radix_sort_step xs i64.get_bit (i*2)
radixSort :: [VName] -> SubExp -> SubExp -> ADM [VName]
radixSort xs n w = do
  logw <- log2 =<< letSubExp "w1" =<< toExp (pe64 w + 1)
  -- ceil logw by (logw + 1) / 2
  iters <- letSubExp "iters" =<< toExp (untyped (pe64 logw + 1) ~/~ untyped (pe64 (intConst Int64 2)))

  types <- traverse lookupType xs
  params <- zipWithM (\x -> newParam (baseString x) . flip toDecl Nonunique) xs types
  i <- newVName "i"
  loopbody <- buildBody_ . localScope (scopeOfFParams params) $
    fmap varsRes $ do
      bit <- letSubExp "bit" =<< toExp (le64 i * 2)
      radixSortStep (map paramName params) types bit n w

  letTupExp "sorted" $
    Loop
      (zip params $ map Var xs)
      (ForLoop i Int64 iters)
      loopbody
  where
    log2 :: SubExp -> ADM SubExp
    log2 m = do
      params <- zipWithM newParam ["cond", "r", "i"] $ map Prim [Bool, int64, int64]
      let [cond, r, i] = params

      body <- buildBody_ . localScope (scopeOfFParams params) $ do
        r' <- letSubExp "r'" =<< toExp (le64 (paramName r) .>>. 1)
        cond' <- letSubExp "cond'" =<< toExp (bNot $ pe64 r' .==. 0)
        i' <- letSubExp "i'" =<< toExp (le64 (paramName i) + 1)
        pure $ subExpsRes [cond', r', i']

      cond_init <- letSubExp "test" =<< toExp (bNot $ pe64 m .==. 0)

      l <-
        letTupExp' "log2res" $
          Loop
            (zip params [cond_init, m, Constant $ blankPrimValue int64])
            (WhileLoop $ paramName cond)
            body

      let [_, _, res] = l
      pure res

radixSort' :: [VName] -> SubExp -> SubExp -> ADM [VName]
radixSort' xs n w = do
  iota_n <-
    letExp "red_iota" . BasicOp $
      Iota n (intConst Int64 0) (intConst Int64 1) Int64

  radres <- radixSort [head xs, iota_n] n w
  let [is', iota'] = radres

  i_param <- newParam "i" $ Prim int64
  let slice = [DimFix $ Var $ paramName i_param]
  map_lam <- mkLambda [i_param] $ varsRes <$> multiIndex (tail xs) slice

  sorted <- letTupExp "sorted" $ Op $ Screma n [iota'] $ mapSOAC map_lam
  pure $ iota' : is' : sorted

--
-- generic case of histogram.
-- Original, assuming `is: [n]i64` and `dst: [w]btp`
--   let xs = reduce_by_index dst odot ne is as
-- Forward sweep:
-- let h_part = reduce_by_index (replicate w ne) odot ne is as
-- let xs = map2 odot dst h_part
-- Reverse sweep:
-- h_part_bar += f'' dst h_part
-- dst_bar += f' dst h_part

-- let flag = map (\i -> i == 0 || sis[i] != sis[i-1]) (iota n)
-- let flag_rev = map (\i -> i==0 || flag[n-i]) (iota n)
-- let ls = seg_scan_exc odot ne flag sas
-- let rs = reverse sas |>
--          seg_scan_exc odot ne flag_rev |> reverse
-- let f_bar = map (\i -> if i < w && -1 < w
--                        then h_part_bar[i]
--                        else 0s
--                 ) sis
-- let sas_bar = f f_dst ls sas rs
-- as_bar += scatter (Scratch alpha n) siota sas_bar
-- Where:
--  siota: 'iota n' sorted wrt 'is'
--  sis: 'is' sorted wrt 'is'
--  sas: 'as' sorted wrt 'is'
--  f'' = vjpLambda xs_bar h_part (map2 odot)
--  f' = vjpLambda xs_bar dst (map2 odot)
--  f  = vjpLambda f_bar sas (map4 (\di li ai ri -> di odot li odot ai odot ri))
--  0s is an alpha-dimensional array with 0 (possibly 0-dim)
diffHist :: VjpOps -> [VName] -> StmAux () -> SubExp -> Lambda SOACS -> [SubExp] -> [VName] -> [SubExp] -> SubExp -> [VName] -> ADM () -> ADM ()
diffHist ops xs aux n lam0 ne as w rf dst m = do
  as_type <- traverse lookupType $ tail as
  dst_type <- traverse lookupType dst

  nes <- traverse (letExp "new_dst" . BasicOp . Replicate (Shape $ pure $ head w)) ne

  h_map <- mkIdentityLambda $ Prim int64 : map rowType as_type
  h_part <- traverse (newVName . flip (<>) "_h_part" . baseString) xs
  auxing aux . letBindNames h_part . Op $
    Hist n as [HistOp (Shape w) rf nes ne lam0] h_map

  lam0' <- renameLambda lam0
  auxing aux . letBindNames xs . Op $
    Screma (head w) (dst <> h_part) (mapSOAC lam0')

  m

  xs_bar <- traverse lookupAdjVal xs

  (dst_params, hp_params, f') <- mkF' lam0 dst_type $ head w
  f'_adj_dst <- vjpLambda ops (map adjFromVar xs_bar) dst_params f'
  f'_adj_hp <- vjpLambda ops (map adjFromVar xs_bar) hp_params f'

  dst_bar' <- eLambda f'_adj_dst $ map (eSubExp . Var) $ dst <> h_part
  dst_bar <- bindSubExpRes "dst_bar" dst_bar'
  zipWithM_ updateAdj dst dst_bar

  h_part_bar' <- eLambda f'_adj_hp $ map (eSubExp . Var) $ dst <> h_part
  h_part_bar <- bindSubExpRes "h_part_bar" h_part_bar'

  lam <- renameLambda lam0
  lam' <- renameLambda lam0

  -- is' <- mapout (head as) n (head w)
  -- sorted <- radixSort' (is' : tail as) n $ head w
  sorted <- radixSort' as n $ head w
  let siota = head sorted
  let sis = head $ tail sorted
  let sas = drop 2 sorted

  iota_n <-
    letExp "iota" $ BasicOp $ Iota n (intConst Int64 0) (intConst Int64 1) Int64

  par_i <- newParam "i" $ Prim int64
  flag_lam <- mkFlagLam par_i sis
  flag <- letExp "flag" $ Op $ Screma n [iota_n] $ mapSOAC flag_lam

  -- map (\i -> (if flag[i] then (true,ne) else (false,vs[i-1]), if i==0 || flag[n-i] then (true,ne) else (false,vs[n-i]))) (iota n)
  par_i' <- newParam "i" $ Prim int64
  let i' = paramName par_i'
  g_lam <-
    mkLambda [par_i'] $
      fmap subExpsRes . mapM (letSubExp "scan_inps") =<< do
        im1 <- letSubExp "i_1" =<< toExp (le64 i' - 1)
        nmi <- letSubExp "n_i" =<< toExp (pe64 n - le64 i')
        let s1 = [DimFix im1]
        let s2 = [DimFix nmi]

        -- flag array for left scan
        f1 <- letSubExp "f1" $ BasicOp $ Index flag $ Slice [DimFix $ Var i']

        -- array for left scan
        r1 <-
          letTupExp' "r1"
            =<< eIf
              (eSubExp f1)
              (eBody $ fmap eSubExp ne)
              (eBody . fmap (eSubExp . Var) =<< multiIndex sas s1)

        -- array for right scan inc flag
        r2 <-
          letTupExp' "r2"
            =<< eIf
              (toExp $ le64 i' .==. 0)
              (eBody $ fmap eSubExp $ Constant (onePrimValue Bool) : ne)
              ( eBody $
                  pure $ do
                    eIf
                      (pure $ BasicOp $ Index flag $ Slice s2)
                      (eBody $ fmap eSubExp $ Constant (onePrimValue Bool) : ne)
                      ( eBody . fmap eSubExp . (Constant (blankPrimValue Bool) :) . fmap Var
                          =<< multiIndex sas s2
                      )
              )

        traverse eSubExp $ f1 : r1 ++ r2

  -- scan (\(f1,v1) (f2,v2) ->
  --   let f = f1 || f2
  --   let v = if f2 then v2 else g v1 v2
  --   in (f,v) ) (false,ne) (zip flags vals)
  scan_lams <-
    traverse
      ( \l -> do
          f1 <- newParam "f1" $ Prim Bool
          f2 <- newParam "f2" $ Prim Bool
          ps <- lambdaParams <$> renameLambda lam0
          let (p1, p2) = splitAt (length ne) ps

          mkLambda (f1 : p1 ++ f2 : p2) $
            fmap varsRes . letTupExp "scan_res" =<< do
              let f = eBinOp LogOr (eParam f1) (eParam f2)
              eIf
                (eParam f2)
                (eBody $ f : fmap eParam p2)
                ( eBody . (f :) . fmap (eSubExp . Var)
                    =<< bindSubExpRes "gres"
                    =<< eLambda l (fmap eParam ps)
                )
      )
      [lam, lam']

  let ne' = Constant (BoolValue False) : ne

  scansres <-
    letTupExp "adj_ctrb_scan" . Op $
      Screma n [iota_n] (scanomapSOAC (map (`Scan` ne') scan_lams) g_lam)

  let (_ : ls_arr, _ : rs_arr_rev) = splitAt (length ne + 1) scansres

  -- map (\i -> if i < w && -1 < w then (xs_bar[i], dst[i]) else (0,ne)) sis
  par_i'' <- newParam "i" $ Prim int64
  let i'' = paramName par_i''
  map_lam <-
    mkLambda [par_i''] $
      fmap varsRes . letTupExp "scan_res"
        =<< eIf
          (toExp $ withinBounds $ pure (head w, i''))
          (eBody . fmap (eSubExp . Var) =<< multiIndex h_part_bar [DimFix $ Var i''])
          ( eBody $ do
              map (\t -> pure $ BasicOp $ Replicate (Shape $ tail $ arrayDims t) (Constant $ blankPrimValue $ elemType t)) as_type
          )

  f_bar <- letTupExp "f_bar" $ Op $ Screma n [sis] $ mapSOAC map_lam

  (as_params, f) <- mkF lam0 as_type n
  f_adj <- vjpLambda ops (map adjFromVar f_bar) as_params f

  -- map (\i -> rs_arr_rev[n-i-1]) (iota n)
  par_i''' <- newParam "i" $ Prim int64
  let i''' = paramName par_i'''
  rev_lam <- mkLambda [par_i'''] $ do
    nmim1 <- letSubExp "n_i_1" =<< toExp (pe64 n - le64 i''' - 1)
    varsRes <$> multiIndex rs_arr_rev [DimFix nmim1]

  rs_arr <- letTupExp "rs_arr" $ Op $ Screma n [iota_n] $ mapSOAC rev_lam

  sas_bar <-
    bindSubExpRes "sas_bar"
      =<< eLambda f_adj (map (eSubExp . Var) $ ls_arr <> sas <> rs_arr)

  scatter_dst <- traverse (\t -> letExp "scatter_dst" $ BasicOp $ Scratch (elemType t) (arrayDims t)) as_type
  as_bar <- multiScatter n scatter_dst siota sas_bar

  zipWithM_ updateAdj (tail as) as_bar
  where
    -- map (\i -> if i == 0 then true else is[i] != is[i-1]) (iota n)
    mkFlagLam :: LParam SOACS -> VName -> ADM (Lambda SOACS)
    mkFlagLam par_i sis =
      mkLambda [par_i] $
        fmap varsRes . letTupExp "flag" =<< do
          let i = paramName par_i
          eIf
            (toExp (le64 i .==. 0))
            (eBody $ pure $ eSubExp $ Constant $ onePrimValue Bool)
            ( eBody $
                pure $ do
                  i_p <- letExp "i_p" =<< toExp (le64 i - 1)
                  vs <- traverse (letExp "vs" . BasicOp . Index sis . Slice . pure . DimFix . Var) [i, i_p]
                  let [vs_i, vs_p] = vs
                  toExp $ bNot $ le64 vs_i .==. le64 vs_p
            )
