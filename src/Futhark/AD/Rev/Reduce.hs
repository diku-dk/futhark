{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.AD.Rev.Reduce
  ( diffReduce,
    diffMinMaxReduce,
  )
where

import Control.Monad
import Futhark.AD.Rev.Monad
import Futhark.Analysis.PrimExp.Convert
import Futhark.Builder
import Futhark.IR.SOACS
import Futhark.Tools
import Futhark.Transform.Rename

eReverse :: MonadBuilder m => VName -> m VName
eReverse arr = do
  arr_t <- lookupType arr
  let w = arraySize 0 arr_t
  start <-
    letSubExp "rev_start" $
      BasicOp $ BinOp (Sub Int64 OverflowUndef) w (intConst Int64 1)
  let stride = intConst Int64 (-1)
      slice = fullSlice arr_t [DimSlice start w stride]
  letExp (baseString arr <> "_rev") $ BasicOp $ Index arr slice

eRotate :: MonadBuilder m => [SubExp] -> VName -> m VName
eRotate rots arr = letExp (baseString arr <> "_rot") $ BasicOp $ Rotate rots arr

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
  res_incl_rot <- mapM (eRotate [intConst Int64 (-1)]) res_incl

  iota <-
    letExp "iota" . BasicOp $
      Iota w (intConst Int64 0) (intConst Int64 1) Int64

  iparam <- newParam "iota_param" $ Prim int64
  vparams <- mapM (newParam "vp") ts
  let params = iparam : vparams

  body <- runBodyBuilder . localScope (scopeOfLParams params) $ do
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
  letTupExp desc $ Op $ Screma w (iota : res_incl_rot) (mapSOAC lam)
  where
    nes = scanNeutral scan
    ts = lambdaReturnType $ scanLambda scan

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
      letExp (baseString adj <> "_rep") $
        BasicOp $ Replicate (Shape [w]) $ Var adj
    void $ updateAdj a adj_rep
  where
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

  as_adj <- letTupExp "adjs" $ Op $ Screma w (ls ++ as ++ rs) (mapSOAC f_adj)

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
      BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
  form <- reduceSOAC [Reduce Commutative red_lam [ne, intConst Int64 (-1)]]
  x_ind <- newVName (baseString x <> "_ind")
  auxing aux $ letBindNames [x, x_ind] $ Op $ Screma w [as, red_iota] form

  m

  x_adj <- lookupAdjVal x
  in_bounds <-
    letSubExp "minmax_in_bounds" . BasicOp $
      CmpOp (CmpSlt Int64) (intConst Int64 0) w
  updateAdjIndex as (CheckBounds (Just in_bounds), Var x_ind) (Var x_adj)
