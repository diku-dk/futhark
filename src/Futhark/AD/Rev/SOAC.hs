{-# LANGUAGE TypeFamilies #-}

module Futhark.AD.Rev.SOAC (vjpSOAC) where

import Control.Monad
import Futhark.AD.Rev.Hist
import Futhark.AD.Rev.Map
import Futhark.AD.Rev.Monad
import Futhark.AD.Rev.Reduce
import Futhark.AD.Rev.Scan
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
  (Pat Type, StmAux (), [a], SubExp, [VName]) ->
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

-- We split multi-op histograms into multiple operations so we
-- can take advantage of special cases. Post-AD, the result may
-- be fused again.
splitHist :: VjpOps -> Pat Type -> StmAux () -> [HistOp SOACS] -> SubExp -> [VName] -> [VName] -> ADM () -> ADM ()
splitHist vjpops pat aux ops w is as m = do
  let ks = map (length . histNeutral) ops
      pat_per_op = map Pat $ chunks ks $ patElems pat
      as_per_op = chunks ks as
      onOps (op : ops') (op_pat : op_pats') (op_is : op_is') (op_as : op_as') = do
        f <- mkIdentityLambda . (Prim int64 :) =<< traverse lookupType op_as
        vjpSOAC vjpops op_pat aux (Hist w (op_is : op_as) [op] f) $
          onOps ops' op_pats' op_is' op_as'
      onOps _ _ _ _ = m
  onOps ops pat_per_op is as_per_op

-- unfusing a map-histogram construct into a map and a histogram.
histomapToMapAndHist :: Pat Type -> (SubExp, [HistOp SOACS], Lambda SOACS, [VName]) -> ADM (Stm SOACS, Stm SOACS)
histomapToMapAndHist (Pat pes) (w, histops, map_lam, as) = do
  map_pat <- traverse accMapPatElem $ lambdaReturnType map_lam
  let map_stm = mkLet map_pat $ Op $ Screma w as $ mapSOAC map_lam
  new_lam <- mkIdentityLambda $ lambdaReturnType map_lam
  let hist_stm = Let (Pat pes) (defAux ()) $ Op $ Hist w (map identName map_pat) histops new_lam
  pure (map_stm, hist_stm)
  where
    accMapPatElem =
      newIdent "hist_map_res" . (`arrayOfRow` w)

commonSOAC :: Pat Type -> StmAux () -> SOAC SOACS -> ADM () -> ADM [Adj]
commonSOAC pat aux soac m = do
  addStm $ Let pat aux $ Op soac
  m
  returnSweepCode $ mapM lookupAdj $ patNames pat

-- Reverse-mode differentiation of SOACs
vjpSOAC :: VjpOps -> Pat Type -> StmAux () -> SOAC SOACS -> ADM () -> ADM ()
-- Differentiating Reduces
vjpSOAC ops pat aux soac@(Screma w as form) m
  | Just [Reduce iscomm lam [Var ne]] <- isReduceSOAC form,
    [a] <- as,
    Just op <- mapOp lam =
      diffVecReduce ops pat aux w iscomm op ne a m
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
  | Just [red] <- isReduceSOAC form,
    [x] <- patNames pat,
    [ne] <- redNeutral red,
    [a] <- as,
    Just [(op, _, _, _)] <- lamIsBinOp $ redLambda red,
    isMulOp op =
      diffMulReduce ops x aux w op ne a m
  | Just red <- singleReduce <$> isReduceSOAC form = do
      pat_adj <- mapM adjVal =<< commonSOAC pat aux soac m
      diffReduce ops pat_adj w as red

-- Differentiating Scans
vjpSOAC ops pat aux soac@(Screma w as form) m
  | Just [Scan lam [ne]] <- isScanSOAC form,
    [x] <- patNames pat,
    [a] <- as,
    Just [(op, _, _, _)] <- lamIsBinOp lam,
    isAddOp op = do
      void $ commonSOAC pat aux soac m
      diffScanAdd ops x w lam ne a
  | Just [Scan lam ne] <- isScanSOAC form,
    Just op <- mapOp lam = do
      diffScanVec ops (patNames pat) aux w op ne as m
  | Just scans <- isScanSOAC form,
    length scans > 1 =
      splitScanRed ops (scanSOAC, scanNeutral) (pat, aux, scans, w, as) m
  | Just red <- singleScan <$> isScanSOAC form = do
      void $ commonSOAC pat aux soac m
      diffScan ops (patNames pat) w as red

-- Differentiating Maps
vjpSOAC ops pat aux soac@(Screma w as form) m
  | Just lam <- isMapSOAC form = do
      pat_adj <- commonSOAC pat aux soac m
      vjpMap ops pat_adj aux w lam as

-- Differentiating Redomaps
vjpSOAC ops pat _aux (Screma w as form) m
  | Just (reds, map_lam) <-
      isRedomapSOAC form = do
      (mapstm, redstm) <-
        redomapToMapAndReduce pat (w, reds, map_lam, as)
      vjpStm ops mapstm $ vjpStm ops redstm m

-- Differentiating Scanomaps
vjpSOAC ops pat _aux (Screma w as form) m
  | Just (scans, map_lam) <-
      isScanomapSOAC form = do
      (mapstm, scanstm) <-
        scanomapToMapAndScan pat (w, scans, map_lam, as)
      vjpStm ops mapstm $ vjpStm ops scanstm m

-- Differentiating Histograms
vjpSOAC ops pat aux (Hist n as histops f) m
  | isIdentityLambda f,
    length histops > 1 = do
      let (is, vs) = splitAt (length histops) as
      splitHist ops pat aux histops n is vs m
vjpSOAC ops pat aux (Hist n [is, vs] [histop] f) m
  | isIdentityLambda f,
    [x] <- patNames pat,
    HistOp (Shape [w]) rf [dst] [Var ne] lam <- histop,
    -- Note that the operator is vectorised, so `ne` cannot be a 'PrimValue'.
    Just op <- mapOp lam =
      diffVecHist ops x aux n op ne is vs w rf dst m
  | isIdentityLambda f,
    [x] <- patNames pat,
    HistOp (Shape [w]) rf [dst] [ne] lam <- histop,
    lam' <- nestedMapOp lam,
    Just [(op, _, _, _)] <- lamIsBinOp lam',
    isMinMaxOp op =
      diffMinMaxHist ops x aux n op ne is vs w rf dst m
  | isIdentityLambda f,
    [x] <- patNames pat,
    HistOp (Shape [w]) rf [dst] [ne] lam <- histop,
    lam' <- nestedMapOp lam,
    Just [(op, _, _, _)] <- lamIsBinOp lam',
    isMulOp op =
      diffMulHist ops x aux n op ne is vs w rf dst m
  | isIdentityLambda f,
    [x] <- patNames pat,
    HistOp (Shape [w]) rf [dst] [ne] lam <- histop,
    lam' <- nestedMapOp lam,
    Just [(op, _, _, _)] <- lamIsBinOp lam',
    isAddOp op =
      diffAddHist ops x aux n lam ne is vs w rf dst m
vjpSOAC ops pat aux (Hist w as [histop] f) m
  | isIdentityLambda f,
    HistOp (Shape n) rf dst ne lam <- histop = do
      diffHist ops (patNames pat) aux w lam ne as n rf dst m
vjpSOAC ops pat _aux (Hist w as histops f) m
  | not (isIdentityLambda f) = do
      (mapstm, redstm) <-
        histomapToMapAndHist pat (w, histops, f, as)
      vjpStm ops mapstm $ vjpStm ops redstm m
vjpSOAC ops pat aux (Stream w as accs lam) m = do
  stms <- collectStms_ $ auxing aux $ sequentialStreamWholeArray pat w accs lam as
  foldr (vjpStm ops) m stms
vjpSOAC _ _ _ soac _ =
  error $ "vjpSOAC unhandled:\n" ++ prettyString soac

---------------
--- Helpers ---
---------------

isMinMaxOp :: BinOp -> Bool
isMinMaxOp (SMin _) = True
isMinMaxOp (UMin _) = True
isMinMaxOp (FMin _) = True
isMinMaxOp (SMax _) = True
isMinMaxOp (UMax _) = True
isMinMaxOp (FMax _) = True
isMinMaxOp _ = False

isMulOp :: BinOp -> Bool
isMulOp (Mul _ _) = True
isMulOp (FMul _) = True
isMulOp _ = False

isAddOp :: BinOp -> Bool
isAddOp (Add _ _) = True
isAddOp (FAdd _) = True
isAddOp _ = False

-- Identifies vectorized operators (lambdas):
--   if the lambda argument is a map, then returns
--   just the map's lambda; otherwise nothing.
mapOp :: Lambda SOACS -> Maybe (Lambda SOACS)
mapOp (Lambda [pa1, pa2] _ lam_body)
  | [SubExpRes cs r] <- bodyResult lam_body,
    cs == mempty,
    [map_stm] <- stmsToList (bodyStms lam_body),
    (Let (Pat [pe]) _ (Op scrm)) <- map_stm,
    (Screma _ [a1, a2] (ScremaForm map_lam [] [])) <- scrm,
    (a1 == paramName pa1 && a2 == paramName pa2) || (a1 == paramName pa2 && a2 == paramName pa1),
    r == Var (patElemName pe) =
      Just map_lam
mapOp _ = Nothing

-- getting the innermost lambda of a perfect-map nest
--   (i.e., the first lambda that does not consists of exactly a map)
nestedMapOp :: Lambda SOACS -> Lambda SOACS
nestedMapOp lam =
  maybe lam nestedMapOp (mapOp lam)
