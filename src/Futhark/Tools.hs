{-# LANGUAGE TypeFamilies #-}

-- | An unstructured grab-bag of various tools and inspection
-- functions that didn't really fit anywhere else.
module Futhark.Tools
  ( module Futhark.Construct,
    redomapToMapAndReduce,
    scanomapToMapAndScan,
    dissectScrema,
    sequentialStreamWholeArray,
    partitionChunkedFoldParameters,

    -- * Primitive expressions
    module Futhark.Analysis.PrimExp.Convert,
  )
where

import Control.Monad
import Futhark.Analysis.PrimExp.Convert
import Futhark.Construct
import Futhark.IR
import Futhark.IR.SOACS.SOAC
import Futhark.Util

-- | Turns a binding of a @redomap@ into two seperate bindings, a
-- @map@ binding and a @reduce@ binding (returned in that order).
--
-- Reuses the original pattern for the @reduce@, and creates a new
-- pattern with new 'Ident's for the result of the @map@.
redomapToMapAndReduce ::
  ( MonadFreshNames m,
    Buildable rep,
    ExpDec rep ~ (),
    Op rep ~ SOAC rep
  ) =>
  Pat (LetDec rep) ->
  ( SubExp,
    [Reduce rep],
    Lambda rep,
    [VName]
  ) ->
  m (Stm rep, Stm rep)
redomapToMapAndReduce (Pat pes) (w, reds, map_lam, arrs) = do
  (map_pat, red_pat, red_arrs) <-
    splitScanOrRedomap pes w map_lam $ map redNeutral reds
  let map_stm = mkLet map_pat $ Op $ Screma w arrs (mapSOAC map_lam)
  red_stm <-
    Let red_pat (defAux ()) . Op
      <$> (Screma w red_arrs <$> reduceSOAC reds)
  pure (map_stm, red_stm)

scanomapToMapAndScan ::
  ( MonadFreshNames m,
    Buildable rep,
    ExpDec rep ~ (),
    Op rep ~ SOAC rep
  ) =>
  Pat (LetDec rep) ->
  ( SubExp,
    [Scan rep],
    Lambda rep,
    [VName]
  ) ->
  m (Stm rep, Stm rep)
scanomapToMapAndScan (Pat pes) (w, scans, map_lam, arrs) = do
  (map_pat, scan_pat, scan_arrs) <-
    splitScanOrRedomap pes w map_lam $ map scanNeutral scans
  let map_stm = mkLet map_pat $ Op $ Screma w arrs (mapSOAC map_lam)
  scan_stm <-
    Let scan_pat (defAux ()) . Op
      <$> (Screma w scan_arrs <$> scanSOAC scans)
  pure (map_stm, scan_stm)

splitScanOrRedomap ::
  (Typed dec, MonadFreshNames m) =>
  [PatElem dec] ->
  SubExp ->
  Lambda rep ->
  [[SubExp]] ->
  m ([Ident], Pat dec, [VName])
splitScanOrRedomap pes w map_lam nes = do
  let (acc_pes, arr_pes) =
        splitAt (length $ concat nes) pes
      (acc_ts, _arr_ts) =
        splitAt (length (concat nes)) $ lambdaReturnType map_lam
  map_accpat <- zipWithM accMapPatElem acc_pes acc_ts
  map_arrpat <- mapM arrMapPatElem arr_pes
  let map_pat = map_accpat ++ map_arrpat
  pure (map_pat, Pat acc_pes, map identName map_accpat)
  where
    accMapPatElem pe acc_t =
      newIdent (baseString (patElemName pe) ++ "_map_acc") $ acc_t `arrayOfRow` w
    arrMapPatElem = pure . patElemIdent

-- | Turn a Screma into a Scanomap (possibly with mapout parts) and a
-- Redomap.  This is used to handle Scremas that are so complicated
-- that we cannot directly generate efficient parallel code for them.
-- In essense, what happens is the opposite of horisontal fusion.
dissectScrema ::
  ( MonadBuilder m,
    Op (Rep m) ~ SOAC (Rep m),
    Buildable (Rep m)
  ) =>
  Pat (LetDec (Rep m)) ->
  SubExp ->
  ScremaForm (Rep m) ->
  [VName] ->
  m ()
dissectScrema pat w (ScremaForm map_lam scans reds) arrs = do
  let num_reds = redResults reds
      num_scans = scanResults scans
      (scan_res, red_res, map_res) = splitAt3 num_scans num_reds $ patNames pat

  to_red <- replicateM num_reds $ newVName "to_red"

  let scanomap = scanomapSOAC scans map_lam
  letBindNames (scan_res <> to_red <> map_res) $
    Op (Screma w arrs scanomap)

  reduce <- reduceSOAC reds
  letBindNames red_res $ Op $ Screma w to_red reduce

-- | Turn a stream SOAC into statements that apply the stream lambda
-- to the entire input.
sequentialStreamWholeArray ::
  (MonadBuilder m, Buildable (Rep m)) =>
  Pat (LetDec (Rep m)) ->
  SubExp ->
  [SubExp] ->
  Lambda (Rep m) ->
  [VName] ->
  m ()
sequentialStreamWholeArray pat w nes lam arrs = do
  -- We just set the chunksize to w and inline the lambda body.  There
  -- is no difference between parallel and sequential streams here.
  let (chunk_size_param, fold_params, arr_params) =
        partitionChunkedFoldParameters (length nes) $ lambdaParams lam

  -- The chunk size is the full size of the array.
  letBindNames [paramName chunk_size_param] $ BasicOp $ SubExp w

  -- The accumulator parameters are initialised to the neutral element.
  forM_ (zip fold_params nes) $ \(p, ne) ->
    letBindNames [paramName p] $ BasicOp $ SubExp ne

  -- Finally, the array parameters are set to the arrays (but reshaped
  -- to make the types work out; this will be simplified rapidly).
  forM_ (zip arr_params arrs) $ \(p, arr) ->
    letBindNames [paramName p] $
      shapeCoerce (arrayDims $ paramType p) arr

  -- Then we just inline the lambda body.
  mapM_ addStm $ bodyStms $ lambdaBody lam

  -- The number of results in the body matches exactly the size (and
  -- order) of 'pat', so we bind them up here, again with a reshape to
  -- make the types work out.
  forM_ (zip (patElems pat) $ bodyResult $ lambdaBody lam) $ \(pe, SubExpRes cs se) ->
    certifying cs $ case (arrayDims $ patElemType pe, se) of
      (dims, Var v)
        | not $ null dims ->
            letBindNames [patElemName pe] $ shapeCoerce dims v
      _ -> letBindNames [patElemName pe] $ BasicOp $ SubExp se

-- | Split the parameters of a stream reduction lambda into the chunk
-- size parameter, the accumulator parameters, and the input chunk
-- parameters.  The integer argument is how many accumulators are
-- used.
partitionChunkedFoldParameters ::
  Int ->
  [Param dec] ->
  (Param dec, [Param dec], [Param dec])
partitionChunkedFoldParameters _ [] =
  error "partitionChunkedFoldParameters: lambda takes no parameters"
partitionChunkedFoldParameters num_accs (chunk_param : params) =
  let (acc_params, arr_params) = splitAt num_accs params
   in (chunk_param, acc_params, arr_params)
