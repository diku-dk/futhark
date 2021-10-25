{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.AD.Rev.SOAC (vjpSOAC) where

import Control.Monad
import Futhark.AD.Rev.Map
import Futhark.AD.Rev.Monad
import Futhark.AD.Rev.Reduce
import Futhark.AD.Rev.Scan
import Futhark.AD.Rev.Scatter
import Futhark.Analysis.PrimExp.Convert
import Futhark.Builder
import Futhark.IR.SOACS
import Futhark.Tools
import Futhark.Util (chunks)

-- We split any multi-op scan or reduction into multiple operations so
-- we can detect special cases.  Post-AD, the result may be fused
-- again.
splitScanRed ::
  VjpOps ->
  ([a] -> ADM (ScremaForm SOACS), a -> [SubExp]) ->
  (Pat, StmAux (), [a], SubExp, [VName]) ->
  ADM () ->
  ADM ()
splitScanRed vjpops (opSOAC, opNeutral) (pat, aux, ops, w, as) m = do
  let ks = map (length . opNeutral) ops
      pat_per_op = map Pat $ chunks ks $ patElems pat
      as_per_op = chunks ks as
      onOps (op : ops') (op_pat : op_pats') (op_as : op_as') = do
        op_form <- opSOAC [op]
        vjpSOAC vjpops op_pat aux (Screma w op_as op_form) $
          onOps ops' op_pats' op_as'
      onOps _ _ _ = m
  onOps ops pat_per_op as_per_op

commonSOAC :: Pat -> StmAux () -> SOAC SOACS -> ADM () -> ADM [Adj]
commonSOAC pat aux soac m = do
  addStm $ Let pat aux $ Op soac
  m
  returnSweepCode $ mapM lookupAdj $ patNames pat

vjpSOAC :: VjpOps -> Pat -> StmAux () -> SOAC SOACS -> ADM () -> ADM ()
vjpSOAC ops pat aux soac@(Screma w as form) m
  | Just reds <- isReduceSOAC form,
    length reds > 1 =
    splitScanRed ops (reduceSOAC, redNeutral) (pat, aux, reds, w, as) m
  | Just [red] <- isReduceSOAC form,
    [x] <- patNames pat,
    [ne] <- redNeutral red,
    [a] <- as,
    Just [(op, _, _, _)] <- lamIsBinOp $ redLambda red,
    isMinMaxOp op =
    diffMinMaxReduce ops x aux w op ne a m
  | Just red <- singleReduce <$> isReduceSOAC form = do
    pat_adj <- mapM adjVal =<< commonSOAC pat aux soac m
    diffReduce ops pat_adj w as red
  where
    isMinMaxOp (SMin _) = True
    isMinMaxOp (UMin _) = True
    isMinMaxOp (FMin _) = True
    isMinMaxOp (SMax _) = True
    isMinMaxOp (UMax _) = True
    isMinMaxOp (FMax _) = True
    isMinMaxOp _ = False
vjpSOAC ops pat aux soac@(Screma w as form) m
  | Just scans <- isScanSOAC form,
    length scans > 1 =
    splitScanRed ops (scanSOAC, scanNeutral) (pat, aux, scans, w, as) m
  | Just red <- singleScan <$> isScanSOAC form = do
    void $ commonSOAC pat aux soac m
    diffScan ops (patNames pat) w as red
vjpSOAC ops pat aux soac@(Screma w as form) m
  | Just lam <- isMapSOAC form = do
    pat_adj <- commonSOAC pat aux soac m
    vjpMap ops pat_adj w lam as
vjpSOAC ops pat _aux (Screma w as form) m
  | Just (reds, map_lam) <-
      isRedomapSOAC form = do
    (mapstm, redstm) <-
      redomapToMapAndReduce pat (w, reds, map_lam, as)
    vjpStm ops mapstm $ vjpStm ops redstm m
vjpSOAC ops pat aux (Scatter w lam ass written_info) m =
  vjpScatter ops pat aux (w, lam, ass, written_info) m
vjpSOAC _ _ _ soac _ =
  error $ "vjpSOAC unhandled:\n" ++ pretty soac
