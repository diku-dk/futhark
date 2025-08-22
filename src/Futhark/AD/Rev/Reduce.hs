{-# LANGUAGE TypeFamilies #-}

module Futhark.AD.Rev.Reduce
  ( diffReduce,
    diffMinMaxReduce,
    diffVecReduce,
    diffMulReduce,
  )
where

import Control.Monad
import Data.Tuple
import Futhark.AD.Rev.Monad
import Futhark.AD.Shared
import Futhark.Analysis.PrimExp.Convert
import Futhark.Builder
import Futhark.IR.SOACS
import Futhark.Tools
import Futhark.Transform.Rename

eReverse :: (MonadBuilder m) => VName -> m VName
eReverse arr = do
  arr_t <- lookupType arr
  let w = arraySize 0 arr_t
  start <-
    letSubExp "rev_start" . BasicOp $
      BinOp (Sub Int64 OverflowUndef) w (intConst Int64 1)
  let stride = intConst Int64 (-1)
      slice = fullSlice arr_t [DimSlice start w stride]
  letExp (baseString arr <> "_rev") $ BasicOp $ Index arr slice

scanExc ::
  (MonadBuilder m, Rep m ~ SOACS) =>
  String ->
  Scan SOACS ->
  [VName] ->
  m [VName]
scanExc desc scan arrs = do
  w <- arraysSize 0 <$> mapM lookupType arrs
  form <- scanSOAC [scan]
  res_incl <- letTupExp (desc <> "_incl") $ Op $ Screma w arrs form

  iota <-
    letExp "iota" . BasicOp $
      Iota w (intConst Int64 0) (intConst Int64 1) Int64

  iparam <- newParam "iota_param" $ Prim int64

  lam <- mkLambda [iparam] $ do
    let first_elem =
          eCmpOp
            (CmpEq int64)
            (eSubExp (Var (paramName iparam)))
            (eSubExp (intConst Int64 0))
        prev = toExp $ le64 (paramName iparam) - 1
    fmap subExpsRes . letTupExp' "scan_ex_res"
      =<< eIf
        first_elem
        (resultBodyM $ scanNeutral scan)
        (eBody $ map (`eIndex` [prev]) res_incl)

  letTupExp desc $ Op $ Screma w [iota] (mapSOAC lam)

mkF :: Lambda SOACS -> ADM ([VName], Lambda SOACS)
mkF lam = do
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
  pure (map paramName aps, lam')

diffReduce :: VjpOps -> [VName] -> SubExp -> [VName] -> Reduce SOACS -> ADM ()
diffReduce _ops [adj] w [a] red
  | Just [(op, _, _, _)] <- lamIsBinOp $ redLambda red,
    isAdd op = do
      adj_rep <-
        transposeIfNeeded <=< letExp (baseString adj <> "_rep") $
          BasicOp (Replicate (Shape [w]) (Var adj))
      void $ updateAdj a adj_rep
  where
    transposeIfNeeded v = do
      adj_shape <- askShape
      if adj_shape == mempty
        then pure v
        else do
          v_t <- lookupType v
          let perm = [1 .. shapeRank adj_shape] ++ [0] ++ [shapeRank adj_shape + 1 .. arrayRank v_t - 1]
          letExp (baseString v <> "_tr") $ BasicOp $ Rearrange v perm

    isAdd FAdd {} = True
    isAdd Add {} = True
    isAdd _ = False
--
-- Differentiating a general single reduce:
--    let y = reduce \odot ne as
-- Forward sweep:
--    let ls = scan_exc \odot  ne as
--    let rs = scan_exc \odot' ne (reverse as)
-- Reverse sweep:
--    let as_c = map3 (f_bar y_bar) ls as (reverse rs)
-- where
--   x \odot' y = y \odot x
--   y_bar is the adjoint of the result y
--   f l_i a_i r_i = l_i \odot a_i \odot r_i
--   f_bar = the reverse diff of f with respect to a_i under the adjoint y_bar
-- The plan is to create
--   one scanomap SOAC which computes ls and rs
--   another map which computes as_c
--
diffReduce ops pat_adj w as red = do
  red' <- renameRed red
  flip_red <- renameRed =<< flipReduce red
  ls <- scanExc "ls" (redToScan red') as
  rs <-
    mapM eReverse
      =<< scanExc "ls" (redToScan flip_red)
      =<< mapM eReverse as

  (as_params, f) <- mkF $ redLambda red

  f_adj <- vjpLambda ops (map adjFromVar pat_adj) as_params f

  as_adj <-
    letTupExp "red_contribs" $ Op $ Screma w (ls ++ as ++ rs) (mapSOAC f_adj)

  zipWithM_ updateAdj as =<< mapM transposeIfNeeded as_adj
  where
    transposeIfNeeded v = do
      adj_shape <- askShape
      if adj_shape == mempty
        then pure v
        else do
          v_t <- lookupType v
          letExp (baseString v <> "_tr") $ BasicOp $ Rearrange v (auxPerm adj_shape v_t)

    renameRed (Reduce comm lam nes) =
      Reduce comm <$> renameLambda lam <*> pure nes

    redToScan :: Reduce SOACS -> Scan SOACS
    redToScan (Reduce _ lam nes) = Scan lam nes
    flipReduce (Reduce comm lam nes) = do
      lam' <- renameLambda lam {lambdaParams = flipParams $ lambdaParams lam}
      pure $ Reduce comm lam' nes
    flipParams ps = uncurry (flip (++)) $ splitAt (length ps `div` 2) ps

--
-- Special case of reduce with min/max:
--    let x = reduce minmax ne as
-- Forward trace (assuming w = length as):
--    let (x, x_ind) =
--      reduce (\ acc_v acc_i v i ->
--                 if (acc_v == v) then (acc_v, min acc_i i)
--                 else if (acc_v == minmax acc_v v)
--                      then (acc_v, acc_i)
--                      else (v, i))
--             (ne_min, -1)
--             (zip as (iota w))
-- Reverse trace:
--    num_elems = i64.bool (0 <= x_ind)
--    m_bar_repl = replicate num_elems m_bar
--    as_bar[x_ind:num_elems:1] += m_bar_repl
diffMinMaxReduce ::
  VjpOps -> VName -> StmAux () -> SubExp -> BinOp -> SubExp -> VName -> ADM () -> ADM ()
diffMinMaxReduce _ops x aux w minmax ne as m = do
  let t = binOpType minmax

  acc_v_p <- newParam "acc_v" $ Prim t
  acc_i_p <- newParam "acc_i" $ Prim int64
  v_p <- newParam "v" $ Prim t
  i_p <- newParam "i" $ Prim int64
  red_lam <-
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

  red_iota <-
    letExp "red_iota" $
      BasicOp $
        Iota w (intConst Int64 0) (intConst Int64 1) Int64
  form <- reduceSOAC [Reduce Commutative red_lam [ne, intConst Int64 (-1)]]
  x_ind <- newVName (baseString x <> "_ind")
  auxing aux $ letBindNames [x, x_ind] $ Op $ Screma w [as, red_iota] form

  m

  x_adj <- lookupAdjVal x
  in_bounds <-
    letSubExp "minmax_in_bounds" . BasicOp $
      CmpOp (CmpSlt Int64) (intConst Int64 0) w
  updateAdjIndex as (CheckBounds (Just in_bounds), Var x_ind) (Var x_adj)

--
-- Special case of vectorised reduce:
--    let x = reduce (map2 op) nes as
-- Idea:
--    rewrite to
--      let x = map2 (\as ne -> reduce op ne as) (transpose as) nes
--    and diff
diffVecReduce ::
  VjpOps -> Pat Type -> StmAux () -> SubExp -> Commutativity -> Lambda SOACS -> VName -> VName -> ADM () -> ADM ()
diffVecReduce ops x aux w iscomm lam ne as m = do
  stms <- collectStms_ $ do
    rank <- arrayRank <$> lookupType as
    let rear = [1, 0] ++ drop 2 [0 .. rank - 1]

    tran_as <- letExp "tran_as" $ BasicOp $ Rearrange as rear
    ts <- lookupType tran_as
    t_ne <- lookupType ne

    as_param <- newParam "as_param" $ rowType ts
    ne_param <- newParam "ne_param" $ rowType t_ne

    reduce_form <- reduceSOAC [Reduce iscomm lam [Var $ paramName ne_param]]

    map_lam <-
      mkLambda [as_param, ne_param] $
        fmap varsRes . letTupExp "idx_res" $
          Op $
            Screma w [paramName as_param] reduce_form
    addStm $ Let x aux $ Op $ Screma (arraySize 0 ts) [tran_as, ne] $ mapSOAC map_lam

  foldr (vjpStm ops) m stms

--
-- Special case of reduce with mul:
--    let x = reduce (*) ne as
-- Forward trace (assuming w = length as):
--    let (p, z) = map (\a -> if a == 0 then (1, 1) else (a, 0)) as
--    non_zero_prod = reduce (*) ne p
--    zr_count = reduce (+) 0 z
--    let x =
--      if 0 == zr_count
--      then non_zero_prod
--      else 0
-- Reverse trace:
--    as_bar = map2
--      (\a a_bar ->
--        if zr_count == 0
--        then a_bar + non_zero_prod/a * x_bar
--        else if zr_count == 1
--        then a_bar + (if a == 0 then non_zero_prod * x_bar else 0)
--        else as_bar
--      ) as as_bar
diffMulReduce ::
  VjpOps -> VName -> StmAux () -> SubExp -> BinOp -> SubExp -> VName -> ADM () -> ADM ()
diffMulReduce _ops x aux w mul ne as m = do
  let t = binOpType mul
  let zero = Constant $ blankPrimValue t
      const_zero = eSubExp zero

  a_param <- newParam "a" $ Prim t
  map_lam <-
    mkLambda [a_param] $
      fmap varsRes . letTupExp "map_res"
        =<< eIf
          (eCmpOp (CmpEq t) (eParam a_param) const_zero)
          (eBody $ fmap eSubExp [Constant $ onePrimValue t, intConst Int64 1])
          (eBody [eParam a_param, eSubExp $ intConst Int64 0])

  ps <- newVName "ps"
  zs <- newVName "zs"
  auxing aux $
    letBindNames [ps, zs] $
      Op $
        Screma w [as] $
          mapSOAC map_lam

  red_lam_mul <- binOpLambda mul t
  red_lam_add <- binOpLambda (Add Int64 OverflowUndef) int64

  red_form_mul <- reduceSOAC $ pure $ Reduce Commutative red_lam_mul $ pure ne
  red_form_add <- reduceSOAC $ pure $ Reduce Commutative red_lam_add $ pure $ intConst Int64 0

  nz_prods <- newVName "non_zero_prod"
  zr_count <- newVName "zero_count"
  auxing aux $ letBindNames [nz_prods] $ Op $ Screma w [ps] red_form_mul
  auxing aux $ letBindNames [zr_count] $ Op $ Screma w [zs] red_form_add

  auxing aux $
    letBindNames [x]
      =<< eIf
        (toExp $ 0 .==. le64 zr_count)
        (eBody $ pure $ eSubExp $ Var nz_prods)
        (eBody $ pure const_zero)

  m

  x_adj <- lookupAdjVal x

  adj_shape <- askShape

  zero_contrib <- letExp "zero_contrib" $ BasicOp $ Replicate adj_shape zero

  a_param_rev <- newParam "a" $ Prim t
  map_lam_rev <-
    mkLambda [a_param_rev] $
      fmap varsRes . letTupExp "adj_res"
        =<< eIf
          (toExp $ 0 .==. le64 zr_count)
          ( eBody
              [ mapNest adj_shape (MkSolo (Var x_adj)) $ \(MkSolo x_adj') ->
                  eBinOp mul (eSubExp x_adj') $
                    eBinOp (getDiv t) (eVar nz_prods) $
                      eParam a_param_rev
              ]
          )
          ( eBody
              [ eIf
                  (toExp $ 1 .==. le64 zr_count)
                  ( eBody
                      [ eIf
                          (eCmpOp (CmpEq t) (eParam a_param_rev) const_zero)
                          ( eBody
                              [ mapNest adj_shape (MkSolo (Var x_adj)) $
                                  \(MkSolo x_adj') ->
                                    eBinOp mul (eSubExp x_adj') $ eVar nz_prods
                              ]
                          )
                          (eBody [eVar zero_contrib])
                      ]
                  )
                  (eBody [eVar zero_contrib])
              ]
          )

  as_adjup <- letExp "prod_contrib" $ Op $ Screma w [as] $ mapSOAC map_lam_rev

  updateAdj as as_adjup
  where
    getDiv :: PrimType -> BinOp
    getDiv (IntType t) = SDiv t Unsafe
    getDiv (FloatType t) = FDiv t
    getDiv _ = error "In getDiv, Reduce.hs: input not supported"
