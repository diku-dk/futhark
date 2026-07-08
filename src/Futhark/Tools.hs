{-# LANGUAGE TypeFamilies #-}

-- | An unstructured grab-bag of various tools and inspection
-- functions that didn't really fit anywhere else.
module Futhark.Tools
  ( module Futhark.Construct,
    redomapToMapAndReduce,
    scanomapToMapAndScan,
    maposcanomapToMapScanAndMap,
    dissectScrema,
    extractPostLambda,
    sequentialStreamWholeArray,
    partitionChunkedFoldParameters,
    withAcc,
    doScatter,
    doHist,
    addBinOp,
    addLambda,

    -- * Primitive expressions
    module Futhark.Analysis.PrimExp.Convert,
  )
where

import Control.Monad
import Data.List qualified as L
import Data.Maybe
import Futhark.Analysis.PrimExp.Convert
import Futhark.Construct
import Futhark.IR
import Futhark.IR.SOACS.SOAC
import Futhark.Util (chunks, mapAccumLM)

splitScanOrRedomap ::
  (MonadFreshNames m) =>
  [PatElem Type] ->
  SubExp ->
  Lambda rep ->
  [[SubExp]] ->
  m (Pat Type, Pat Type, [VName], Lambda rep)
splitScanOrRedomap pes w map_lam nes = do
  let (nonmap_pes, map_pes) =
        splitAt (length $ concat nes) pes
      (nonmap_ts, map_ts) =
        splitAt (length (concat nes)) $ lambdaReturnType map_lam
      (nonmap_res, map_res) =
        splitAt (length (concat nes)) $ bodyResult $ lambdaBody map_lam

  -- Put some care into not having duplicate results from the map function.
  (red_arrs, acc_info) <-
    unzip . snd
      <$> mapAccumLM
        accMapPatElem
        (zip map_res map_pes)
        (zip3 nonmap_pes nonmap_ts nonmap_res)

  let (nonmap_tmppes, nonmap_ts', nonmap_res') =
        L.unzip3 $ catMaybes acc_info
      map_lam' =
        map_lam
          { lambdaBody = (lambdaBody map_lam) {bodyResult = nonmap_res' <> map_res},
            lambdaReturnType = nonmap_ts' <> map_ts
          }
      map_pat = nonmap_tmppes <> map_pes

  pure (Pat map_pat, Pat nonmap_pes, red_arrs, map_lam')
  where
    accMapPatElem res_to_pe (pe, nonmap_t, res) =
      case res `L.lookup` res_to_pe of
        Just pe' -> pure (res_to_pe, (patElemName pe', Nothing))
        Nothing -> do
          pe' <-
            PatElem
              <$> newVName (baseName (patElemName pe) <> "_map_acc")
              <*> pure (nonmap_t `arrayOfRow` w)
          pure ((res, pe') : res_to_pe, (patElemName pe', Just (pe', nonmap_t, res)))

-- | Turns a binding of a @redomap@ into two seperate bindings, a
-- @map@ binding and a @reduce@ binding (returned in that order).
--
-- Reuses the original pattern for the @reduce@, and creates a new
-- pattern with new 'Ident's for the result of the @map@.
redomapToMapAndReduce ::
  ( MonadFreshNames m,
    LetDec rep ~ Type,
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
  (map_pat, red_pat, red_arrs, map_lam') <-
    splitScanOrRedomap pes w map_lam $ map redNeutral reds

  map_stm <- Let map_pat (defAux ()) . Op . Screma w arrs <$> mapSOAC map_lam'
  red_stm <-
    Let red_pat (defAux ()) . Op
      <$> (Screma w red_arrs <$> reduceSOAC reds)
  pure (map_stm, red_stm)

scanomapToMapAndScan ::
  ( MonadFreshNames m,
    Buildable rep,
    LetDec rep ~ Type,
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
  (map_pat, scan_pat, scan_arrs, map_lam') <-
    splitScanOrRedomap pes w map_lam $ map scanNeutral scans
  map_stm <- Let map_pat (defAux ()) . Op . Screma w arrs <$> mapSOAC map_lam'
  scan_stm <-
    Let scan_pat (defAux ()) . Op
      <$> (Screma w scan_arrs <$> scanSOAC scans)
  pure (map_stm, scan_stm)

maposcanomapToMapScanAndMap ::
  ( MonadFreshNames m,
    Buildable rep,
    Op rep ~ SOAC rep
  ) =>
  Pat (LetDec rep) ->
  ( SubExp,
    Lambda rep,
    [Scan rep],
    Lambda rep,
    [VName]
  ) ->
  m (Stm rep, Stm rep, Stm rep)
maposcanomapToMapScanAndMap (Pat pes) (w, post_lam, scans, map_lam, arrs) = do
  map_res <- mapM tempRes $ lambdaReturnType map_lam
  map_stm <- mkLet map_res . Op . Screma w arrs <$> mapSOAC map_lam
  scan_res <- mapM tempRes $ take (scanResults scans) $ lambdaReturnType map_lam
  let (scan_arrs, map_arrs) = splitAt (scanResults scans) $ map identName map_res
  scan_stm <- mkLet scan_res . Op . Screma w scan_arrs <$> scanSOAC scans
  let post_arrs = map identName scan_res <> map_arrs
      res = patElemIdent <$> pes
  post_stm <- mkLet res . Op . Screma w post_arrs <$> mapSOAC post_lam
  pure (map_stm, scan_stm, post_stm)
  where
    tempRes res = newIdent "temp_res" $ res `arrayOfRow` w

-- | Turn a Screma into a maposcanomap (possibly with mapout parts) and a
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

  maposcanomap <- maposcanomapSOAC map_lam scans post_lam'
  letBindNames (to_red <> scan_map_res) $ Op (Screma w arrs maposcanomap)

  reduce <- reduceSOAC reds
  letBindNames red_res $ Op $ Screma w to_red reduce

-- | Remove the post lambda from a screma, producing the screma with an identity
-- post-lambda, and a new map screma that is just the post-lambda. You can apply
-- this indefinitely in case the post-lambda is already an identity lambda, so
-- be careful.
extractPostLambda ::
  ( MonadBuilder m,
    Op (Rep m) ~ SOAC (Rep m),
    Buildable (Rep m)
  ) =>
  Pat (LetDec (Rep m)) ->
  SubExp ->
  [VName] ->
  ScremaForm (Rep m) ->
  m ()
extractPostLambda pat w arrs (ScremaForm pre_lam scans reds post_lam) = do
  tmp_names <-
    mapM (newVName . (<> "_extract") . baseName . paramName) (lambdaParams post_lam)
  id_lam <- mkIdentityLambda $ map paramType $ lambdaParams post_lam
  letBindNames (map patElemName red_res <> tmp_names) $
    Op (Screma w arrs $ ScremaForm pre_lam scans reds id_lam)
  letBind (Pat nonred_res) . Op . Screma w tmp_names =<< mapSOAC post_lam
  where
    (red_res, nonred_res) = splitAt (redResults reds) (patElems pat)

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
  Name ->
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

    let w = arraysSize 0 arrs_ts
    (fmap varsRes . letTupExp "acc_res")
      . Op
      . Screma w (map paramName acc_ps <> arrs)
      =<< mapSOAC map_lam

  letTupExp desc $ WithAcc [(acc_shape, [v], Nothing) | v <- dest] withacc_lam

-- | Perform a histogram-like operation using accumulators and map.
doHist ::
  (MonadBuilder m, Buildable (Rep m), Op (Rep m) ~ SOAC (Rep m)) =>
  Name ->
  [HistOp (Rep m)] ->
  [VName] ->
  ([LParam (Rep m)] -> m [SubExp]) ->
  m [[VName]]
doHist desc ops arrs mk = do
  (inputs, cert_ps, acc_ts) <- unzip3 <$> mapM onOp ops
  acc_ps <- mapM (newParam "acc_p") acc_ts
  arrs_ts <- mapM lookupType arrs

  withacc_lam <- mkLambda (cert_ps <> acc_ps) $ do
    acc_ps_inner <- mapM (newParam "acc_p") acc_ts
    params <- mapM (newParam "v" . stripArray 1) arrs_ts
    map_lam <-
      mkLambda (acc_ps_inner <> params) $ do
        let num_is_per_op = map (shapeRank . histShape) ops
            num_vs_per_op = map (length . histDest) ops
        (is, vs) <- splitAt (sum num_is_per_op) <$> mk params
        let is_per_op = chunks num_is_per_op is
            vs_per_op = chunks num_vs_per_op vs
        fmap subExpsRes $ forM (zip3 acc_ps_inner is_per_op vs_per_op) $ \(acc_p_inner, op_is, op_vs) ->
          letSubExp "scatter_acc" . BasicOp $
            UpdateAcc Safe (paramName acc_p_inner) op_is op_vs

    let w = arraysSize 0 arrs_ts
    (fmap varsRes . letTupExp "acc_res")
      . Op
      . Screma w (map paramName acc_ps <> arrs)
      =<< mapSOAC map_lam
  fmap (chunks (map (length . histDest) ops)) $
    letTupExp desc $
      WithAcc inputs withacc_lam
  where
    onOp op = do
      idx_params <- replicateM (shapeRank (histShape op)) $ newParam "idx" $ Prim int64
      let addIdxParams lam = lam {lambdaParams = idx_params <> lambdaParams lam}
          input = (histShape op, histDest op, Just (addIdxParams (histOp op), histNeutral op))
          shape = histShape op
      elem_ts <- fmap (map (stripArray (shapeRank shape))) $ mapM lookupType $ histDest op
      cert_p <- newParam "acc_cert" $ Prim Unit
      let cert = paramName cert_p
      pure (input, cert_p, Acc cert shape elem_ts NoUniqueness)

-- | The most addition-like binary operator for some primitive type.
addBinOp :: PrimType -> BinOp
addBinOp (IntType it) = Add it OverflowWrap
addBinOp (FloatType ft) = FAdd ft
addBinOp Bool = LogAnd
addBinOp Unit = LogAnd

-- | Construct a lambda for adding two values of the given type, Using SOACs to handle arrays.
addLambda ::
  ( OpC (Rep m) ~ SOAC,
    MonadBuilder m,
    Buildable (Rep m)
  ) =>
  TypeBase Shape NoUniqueness ->
  m (Lambda (Rep m))
addLambda (Prim pt) = binOpLambda (addBinOp pt) pt
addLambda t@Array {} = do
  xs_p <- newParam "xs" t
  ys_p <- newParam "ys" t
  lam <- addLambda $ rowType t
  body <- insertStmsM $ do
    res <-
      letSubExp "lam_map"
        . Op
        . Screma (arraySize 0 t) [paramName xs_p, paramName ys_p]
        =<< mapSOAC lam
    pure $ resultBody [res]
  pure
    Lambda
      { lambdaParams = [xs_p, ys_p],
        lambdaReturnType = [t],
        lambdaBody = body
      }
addLambda t =
  error $ "addLambda: " ++ show t
