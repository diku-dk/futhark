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
    withAcc,
    doScatter,

    -- * Primitive expressions
    module Futhark.Analysis.PrimExp.Convert,
  )
where

import Control.Monad
import Futhark.Analysis.PrimExp.Convert
import Futhark.Construct
import Futhark.IR
import Futhark.IR.SOACS.SOAC

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
  map_stm <- mkLet map_pat . Op . Screma w arrs <$> mapSOAC map_lam
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
  map_stm <- mkLet map_pat . Op . Screma w arrs <$> mapSOAC map_lam
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
dissectScrema pat w (ScremaForm map_lam scans reds post_lam) arrs = do
  let num_reds = redResults reds
      num_scans = scanResults scans
      reds_ts = concatMap (lambdaReturnType . redLambda) reds
      (red_res, scan_map_res) = splitAt num_reds $ patNames pat
      (scan_pars, map_pars) = splitAt num_scans $ lambdaParams post_lam
      post_res = bodyResult $ lambdaBody post_lam

  to_red <- replicateM num_reds $ newVName "to_red"
  red_pars <- mapM (newParam "x") reds_ts
  let red_post_res = paramName <$> red_pars

  let post_lam' =
        post_lam
          { lambdaParams = scan_pars <> red_pars <> map_pars,
            lambdaBody =
              (lambdaBody post_lam)
                { bodyResult = varsRes red_post_res <> post_res
                },
            lambdaReturnType = reds_ts <> lambdaReturnType post_lam
          }

  let maposcanomap = maposcanomapSOAC post_lam' scans map_lam
  letBindNames (to_red <> scan_map_res) $
    Op (Screma w arrs maposcanomap)

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
      if null (arrayDims $ paramType p)
        then BasicOp $ SubExp $ Var arr
        else shapeCoerce (arrayDims $ paramType p) arr

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

-- | Construct a one-dimensional scatter-like 'WithAcc'. The closure is invoked
-- with the accumulators.
withAcc ::
  (MonadBuilder m, LParam (Rep m) ~ Param Type) =>
  [VName] ->
  Int ->
  ([VName] -> m [SubExp]) ->
  m (Exp (Rep m))
withAcc dest rank mk = do
  cert_ps <- replicateM (length dest) $ newParam "acc_cert" $ Prim Unit
  dest_ts <- mapM lookupType dest
  let acc_shape = Shape $ take rank $ arrayDims $ head dest_ts
      mkT cert elem_t = Acc cert acc_shape [elem_t] NoUniqueness
      acc_ts =
        zipWith mkT (map paramName cert_ps) $
          map (stripArray rank) dest_ts
  acc_ps <- mapM (newParam "acc_p") acc_ts

  withacc_lam <- mkLambda (cert_ps <> acc_ps) $ subExpsRes <$> mk (map paramName acc_ps)

  pure $ WithAcc [(acc_shape, [v], Nothing) | v <- dest] withacc_lam

-- | Perform a scatter-like operation using accumulators and map.
doScatter ::
  (MonadBuilder m, Buildable (Rep m), Op (Rep m) ~ SOAC (Rep m)) =>
  String ->
  Int ->
  [VName] ->
  [VName] ->
  ([LParam (Rep m)] -> m [SubExp]) ->
  m [VName]
doScatter desc rank dest arrs mk = do
  cert_ps <- replicateM (length dest) $ newParam "acc_cert" $ Prim Unit
  dest_ts <- mapM lookupType dest
  let acc_shape = Shape $ take rank $ arrayDims $ head dest_ts
      mkT cert elem_t = Acc cert acc_shape [elem_t] NoUniqueness
      acc_ts =
        zipWith mkT (map paramName cert_ps) $
          map (stripArray rank) dest_ts
  acc_ps <- mapM (newParam "acc_p") acc_ts
  arrs_ts <- mapM lookupType arrs

  withacc_lam <- mkLambda (cert_ps <> acc_ps) $ do
    acc_ps_inner <- mapM (newParam "acc_p") acc_ts
    params <- mapM (newParam "v" . stripArray 1) arrs_ts
    map_lam <-
      mkLambda (acc_ps_inner <> params) $ do
        (is, vs) <- splitAt rank <$> mk params
        fmap subExpsRes $ forM (zip acc_ps_inner vs) $ \(acc_p_inner, v) ->
          letSubExp "scatter_acc" . BasicOp $
            UpdateAcc Safe (paramName acc_p_inner) is [v]

    -- NOTE: Is this problematic?
    let w = arraysSize 0 arrs_ts
    (fmap varsRes . letTupExp "acc_res")
      . Op
      . Screma w (map paramName acc_ps <> arrs)
      =<< mapSOAC map_lam

  letTupExp desc $ WithAcc [(acc_shape, [v], Nothing) | v <- dest] withacc_lam
