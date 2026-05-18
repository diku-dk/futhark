{-# LANGUAGE TypeFamilies #-}

module Futhark.Pass.Flatten.Builtins
  ( flatteningBuiltins,
    determineReduceOp,
    segMap,
    genFlags,
    genScan,
    genFilter,
    genSegScan,
    genSegScanomap,
    genSegScanomapWithPost,
    genUniformSegScanomapWithPost,
    genUniformSegRed,
    genSegRed,
    genSegRedomap,
    genScatter,
    genShapeIota,
    exScanAndSum,
    doSegIota,
    doPrefixSum,
    doRepIota,
    doPartition,
  )
where

import Control.Monad (forM, forM_, (<=<))
import Control.Monad.State.Strict
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Futhark.IR.GPU
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Pass.ExtractKernels.ToGPU (soacsLambdaToGPU)
import Futhark.Pass.Flatten.StreamKernel
import Futhark.Tools
import Futhark.Transform.Rename (renameBody, renameLambda)
import Futhark.Util (unsnoc)

mkSegSpace :: (MonadFreshNames m) => [(VName, SubExp)] -> m SegSpace
mkSegSpace dims = SegSpace <$> newVName "phys_tid" <*> pure dims

builtinName :: T.Text -> Name
builtinName = nameFromText . ("builtin#" <>)

segIotaName, repIotaName, prefixSumName, partitionName :: Name
segIotaName = builtinName "segiota"
repIotaName = builtinName "repiota"
prefixSumName = builtinName "prefixsum"
partitionName = builtinName "partition"

inlineBuiltinAtLevel :: SegLevel -> Bool
inlineBuiltinAtLevel SegThreadInBlock {} = True
inlineBuiltinAtLevel _ = False

regularSegLevel :: SegLevel
regularSegLevel = SegThread SegVirt Nothing

-- FIXME: We use segThreadCapped here because otherwise we may get
-- out-of-bounds writes for SegOps with non-primitive return types.
capThreadSegLevel :: (Foldable t) => t SubExp -> Name -> SegLevel -> ThreadRecommendation -> Builder GPU SegLevel
capThreadSegLevel segments desc lvl tr =
  case lvl of
    SegThread {} -> segThreadCapped (toList segments) desc tr
    _ -> pure lvl

determineReduceOp ::
  (MonadBuilder m) =>
  Lambda SOACS ->
  [SubExp] ->
  m (Lambda SOACS, [SubExp], Shape)
determineReduceOp lam nes =
  -- FIXME? We are assuming that the accumulator is a replicate, and
  -- we fish out its value in a gross way.
  case mapM subExpVar nes of
    Just ne_vs' -> do
      let (shape, lam') = isVectorMap lam
      nes' <- forM ne_vs' $ \ne_v -> do
        ne_v_t <- lookupType ne_v
        letSubExp "hist_ne" $
          BasicOp $
            Index ne_v $
              fullSlice ne_v_t $
                replicate (shapeRank shape) $
                  DimFix $
                    intConst Int64 0
      pure (lam', nes', shape)
    Nothing ->
      pure (lam, nes, mempty)

isVectorMap :: Lambda SOACS -> (Shape, Lambda SOACS)
isVectorMap lam
  | [Let (Pat pes) _ (Op (Screma w arrs form))] <-
      stmsToList $ bodyStms $ lambdaBody lam,
    map resSubExp (bodyResult (lambdaBody lam)) == map (Var . patElemName) pes,
    Just map_lam <- isMapSOAC form,
    arrs == map paramName (lambdaParams lam) =
      let (shape, lam') = isVectorMap map_lam
       in (Shape [w] <> shape, lam')
  | otherwise = (mempty, lam)

segMap :: (Traversable f) => SegLevel -> f SubExp -> (f SubExp -> Builder GPU Result) -> Builder GPU (Exp GPU)
segMap lvl segments f = do
  gtids <- traverse (const $ newVName "gtid") segments
  space <- mkSegSpace $ zip (toList gtids) (toList segments)
  ((res, ts), stms) <- collectStms $ localScope (scopeOfSegSpace space) $ do
    res <- f $ fmap Var gtids
    ts <- mapM (subExpType . resSubExp) res
    pure (map mkResult res, ts)
  let kbody = Body () stms res
  let tr = if all primType ts then ManyThreads else NoRecommendation SegVirt
  lvl' <- capThreadSegLevel segments "uniform_segred" lvl tr
  pure $ Op $ SegOp $ SegMap lvl' space ts kbody
  where
    mkResult (SubExpRes cs se) = Returns ResultMaySimplify cs se

genScanWithKernelBody ::
  (Traversable f) =>
  SegLevel ->
  Name ->
  f SubExp ->
  Lambda GPU ->
  [SubExp] ->
  (f SubExp -> Builder GPU Result) ->
  Builder GPU [VName]
genScanWithKernelBody lvl desc segments lam nes =
  genScanWithKernelBodyAndPost
    lvl
    desc
    segments
    (\_ -> pure lam)
    mempty
    nes
    (\_ res_t -> mkIdentityLambda res_t)

genUniformSegRed ::
  SegLevel ->
  Name ->
  [SubExp] ->
  Reduce GPU ->
  Shape ->
  Lambda GPU ->
  [VName] ->
  ([SubExp] -> Builder GPU ()) ->
  Builder GPU [VName]
genUniformSegRed lvl desc segments red_op shape map_lam arrs readFree = do
  let red_lam = redLambda red_op
      nes = redNeutral red_op
      comm = redComm red_op
  gtids <- traverse (const $ newVName "gtid") segments
  space <- mkSegSpace $ zip (toList gtids) (toList segments)
  let gtids' = fmap Var gtids
  ((res, res_t), stms) <- runBuilder . localScope (scopeOfSegSpace space) $ do
    readFree gtids'
    bindLambdaInputArrays gtids' map_lam arrs
    res <- bodyBind (lambdaBody map_lam)
    res_t <- mapM (subExpType . resSubExp) res
    pure (map mkResult res, res_t)

  red_lam' <-
    localScope (scopeOfSegSpace space) $
      withReadFree red_lam readFree gtids'
  -- We have to rename since we are using a global readFree
  red_lam'' <- renameLambda red_lam'

  kbody <- renameBody $ Body () stms res
  let op = SegBinOp comm red_lam'' nes shape
  lvl' <- capThreadSegLevel segments "uniform_segred" lvl $ NoRecommendation SegVirt
  letTupExp desc $ Op $ SegOp $ SegRed lvl' space res_t kbody [op]
  where
    mkResult (SubExpRes cs se) = Returns ResultMaySimplify cs se

genScanWithKernelBodyAndPost ::
  (Traversable f) =>
  SegLevel ->
  Name ->
  f SubExp ->
  (f SubExp -> Builder GPU (Lambda GPU)) ->
  Shape ->
  [SubExp] ->
  (f SubExp -> [Type] -> Builder GPU (Lambda GPU)) ->
  (f SubExp -> Builder GPU Result) ->
  Builder GPU [VName]
genScanWithKernelBodyAndPost lvl desc segments mkScanLam shape nes mkPostLam m = do
  gtids <- traverse (const $ newVName "gtid") segments
  space <- mkSegSpace $ zip (toList gtids) (toList segments)
  let gtids' = fmap Var gtids
  ((res, res_t), stms) <- runBuilder . localScope (scopeOfSegSpace space) $ do
    res <- m gtids'
    res_t <- mapM (subExpType . resSubExp) res
    pure (map mkResult res, res_t)

  scan_lam <-
    localScope (scopeOfSegSpace space) $
      mkScanLam gtids'
  post_lam <-
    localScope (scopeOfSegSpace space) $
      mkPostLam gtids' res_t
  -- We have to rename since we are using a global readFree
  scan_lam' <- renameLambda scan_lam
  post_lam' <- renameLambda post_lam

  kbody <- renameBody $ Body () stms res
  let op = SegBinOp Noncommutative scan_lam' nes shape
  lvl' <- capThreadSegLevel segments "segscan" lvl $ NoRecommendation SegVirt
  letTupExp desc $ Op $ SegOp $ SegScan lvl' space res_t kbody [op] (SegPostOp post_lam')
  where
    mkResult (SubExpRes cs se) = Returns ResultMaySimplify cs se

bindLambdaInputArrays ::
  (Traversable f) =>
  f SubExp ->
  Lambda GPU ->
  [VName] ->
  Builder GPU ()
bindLambdaInputArrays gtids lam arrs = do
  let idxs = toList gtids
  forM_ (zip (lambdaParams lam) arrs) $ \(p, arr) ->
    letBindNames [paramName p]
      =<< case paramType p of
        Acc {} ->
          eSubExp $ Var arr
        _ ->
          eIndex arr $ map eSubExp idxs

genScan :: (Traversable f) => SegLevel -> Name -> f SubExp -> Lambda GPU -> [SubExp] -> [VName] -> Builder GPU [VName]
genScan lvl desc segments lam nes arrs =
  genScanWithKernelBody lvl desc segments lam nes $ \gtids ->
    subExpsRes
      <$> forM
        arrs
        ( \arr ->
            letSubExp (baseName arr <> "_elem") =<< eIndex arr (toList $ fmap eSubExp gtids)
        )

genExScan :: (Traversable f) => SegLevel -> Name -> f SubExp -> Lambda GPU -> [SubExp] -> [VName] -> Builder GPU [VName]
genExScan lvl desc segments lam nes arrs =
  genScanWithKernelBody lvl desc segments lam nes $ \gtids ->
    let Just (outerDims, innerDim) = unsnoc $ toList gtids
     in do
          prescan <-
            letTupExp' "to_prescan"
              =<< eIf
                (toExp $ pe64 innerDim .==. 0)
                (eBody (map eSubExp nes))
                (eBody (map (`eIndex` (map toExp outerDims ++ [toExp $ pe64 innerDim - 1])) arrs))
          pure $ subExpsRes prescan

segScanLambda ::
  Lambda GPU ->
  ([SubExp] -> Builder GPU ()) ->
  [SubExp] ->
  Builder GPU (Lambda GPU)
segScanLambda lam readFree gtids = do
  x_flag_p <- newParam "x_flag" $ Prim Bool
  y_flag_p <- newParam "y_flag" $ Prim Bool
  let ts = lambdaReturnType lam
      (xps, yps) = splitAt (length ts) $ lambdaParams lam
  mkLambda ([x_flag_p] ++ xps ++ [y_flag_p] ++ yps) $
    bodyBind
      =<< eBody
        [ eBinOp LogOr (eParam x_flag_p) (eParam y_flag_p),
          eIf
            (eParam y_flag_p)
            (eBody (map eParam yps))
            ( do
                readFree gtids
                pure $ lambdaBody lam
            )
        ]

genSegScan :: SegLevel -> Name -> Lambda GPU -> [SubExp] -> VName -> [VName] -> Builder GPU [VName]
genSegScan lvl desc lam nes flags arrs = do
  w <- arraySize 0 <$> lookupType flags
  lam' <- segScanLambda lam (const $ pure ()) []
  drop 1 <$> genScan lvl desc [w] lam' (constant False : nes) (flags : arrs)

segScanomapPostLambda ::
  Lambda GPU ->
  ([SubExp] -> Builder GPU ()) ->
  [SubExp] ->
  Builder GPU (Lambda GPU)
segScanomapPostLambda lam readFree gtids = do
  flag_p <- newParam "seg_flag" $ Prim Bool
  mkLambda (flag_p : lambdaParams lam) $ do
    readFree gtids
    bodyBind $ lambdaBody lam

genSegScanomap ::
  SegLevel ->
  Name ->
  Lambda GPU ->
  [SubExp] ->
  VName ->
  Lambda GPU ->
  [VName] ->
  ([SubExp] -> Builder GPU ()) ->
  Builder GPU [VName]
genSegScanomap lvl desc scan_lam nes flags map_lam arrs readFree = do
  post_lam <- mkIdentityLambda $ lambdaReturnType map_lam
  genSegScanomapWithPost lvl desc scan_lam nes flags post_lam map_lam arrs readFree

genSegScanomapWithPost ::
  SegLevel ->
  Name ->
  Lambda GPU ->
  [SubExp] ->
  VName ->
  Lambda GPU ->
  Lambda GPU ->
  [VName] ->
  ([SubExp] -> Builder GPU ()) ->
  Builder GPU [VName]
genSegScanomapWithPost lvl desc scan_lam nes flags post_lam map_lam arrs readFree = do
  w <- arraySize 0 <$> lookupType flags

  genScanWithKernelBodyAndPost
    lvl
    desc
    [w]
    (segScanLambda scan_lam readFree)
    mempty
    (constant False : nes)
    ( \gtids _res_t ->
        segScanomapPostLambda post_lam readFree gtids
    )
    ( \gtids -> do
        let [gtid] = toList gtids
        flag <- letSubExp "flag" =<< eIndex flags [eSubExp gtid]
        readFree gtids
        bindLambdaInputArrays gtids map_lam arrs
        map_res <- bodyBind (lambdaBody map_lam)
        pure (subExpRes flag : map_res)
    )

withReadFree ::
  Lambda GPU ->
  ([SubExp] -> Builder GPU ()) ->
  [SubExp] ->
  Builder GPU (Lambda GPU)
withReadFree lam readFree gtids =
  mkLambda (lambdaParams lam) $ do
    readFree gtids
    bodyBind $ lambdaBody lam

genUniformSegScanomapWithPost ::
  SegLevel ->
  [SubExp] ->
  Name ->
  Lambda GPU ->
  Shape ->
  [SubExp] ->
  Lambda GPU ->
  Lambda GPU ->
  [VName] ->
  ([SubExp] -> Builder GPU ()) ->
  Builder GPU [VName]
genUniformSegScanomapWithPost lvl segments desc scan_lam shape nes post_lam map_lam arrs readFree = do
  genScanWithKernelBodyAndPost
    lvl
    desc
    segments
    (withReadFree scan_lam readFree)
    shape
    nes
    (\gtids _res_t -> withReadFree post_lam readFree gtids)
    ( \gtids -> do
        readFree gtids
        bindLambdaInputArrays gtids map_lam arrs
        bodyBind (lambdaBody map_lam)
    )

genPrefixSum :: SegLevel -> Name -> VName -> Builder GPU VName
genPrefixSum lvl desc ns = do
  ws <- arrayDims <$> lookupType ns
  add_lam <- binOpLambda (Add Int64 OverflowUndef) int64
  head <$> genScan lvl desc ws add_lam [intConst Int64 0] [ns]

genExPrefixSum :: SegLevel -> Name -> VName -> Builder GPU VName
genExPrefixSum lvl desc ns = do
  ws <- arrayDims <$> lookupType ns
  add_lam <- binOpLambda (Add Int64 OverflowUndef) int64
  head <$> genExScan lvl desc ws add_lam [intConst Int64 0] [ns]

genSegPrefixSum :: SegLevel -> Name -> VName -> VName -> Builder GPU VName
genSegPrefixSum lvl desc flags ns = do
  add_lam <- binOpLambda (Add Int64 OverflowUndef) int64
  head <$> genSegScan lvl desc add_lam [intConst Int64 0] flags [ns]

genScatter :: SegLevel -> VName -> SubExp -> (SubExp -> Builder GPU (VName, SubExp)) -> Builder GPU (Exp GPU)
genScatter lvl dest n f = do
  gtid <- newVName "gtid"
  space <- mkSegSpace [(gtid, n)]
  withAcc [dest] 1 $ \ ~[acc] -> do
    kbody <- buildBody_ $ localScope (scopeOfSegSpace space) $ do
      (i, v) <- f $ Var gtid
      acc' <- letExp (baseName acc) $ BasicOp $ UpdateAcc Safe acc [Var i] [v]
      pure [Returns ResultMaySimplify mempty $ Var acc']
    acc_t <- lookupType acc
    lvl' <- capThreadSegLevel [n] "genScatter" lvl $ NoRecommendation SegVirt
    letTupExp' "scatter" $ Op $ SegOp $ SegMap lvl' space [acc_t] kbody

genTabulate :: SegLevel -> SubExp -> (SubExp -> Builder GPU [SubExp]) -> Builder GPU (Exp GPU)
genTabulate lvl w m = do
  gtid <- newVName "gtid"
  space <- mkSegSpace [(gtid, w)]
  ((res, ts), stms) <- collectStms $ localScope (scopeOfSegSpace space) $ do
    ses <- m $ Var gtid
    ts <- mapM subExpType ses
    pure (map (Returns ResultMaySimplify mempty) ses, ts)
  let kbody = Body () stms res
  lvl' <- capThreadSegLevel [w] "genTabulate" lvl $ NoRecommendation SegVirt
  pure $ Op $ SegOp $ SegMap lvl' space ts kbody

genFlags :: SegLevel -> SubExp -> VName -> Builder GPU VName
genFlags lvl m offsets = do
  flags_allfalse <-
    letExp "flags_allfalse" . BasicOp $
      Replicate (Shape [m]) (constant False)
  n <- arraySize 0 <$> lookupType offsets
  letExp "flags" <=< genScatter lvl flags_allfalse n $ \gtid -> do
    i <- letExp "i" =<< eIndex offsets [eSubExp gtid]
    pure (i, constant True)

genSegRed :: SegLevel -> VName -> VName -> VName -> [VName] -> Reduce SOACS -> Builder GPU [VName]
genSegRed lvl segments flags offsets elems red = do
  scanned <-
    genSegScan
      lvl
      "red"
      (soacsLambdaToGPU $ redLambda red)
      (redNeutral red)
      flags
      elems
  num_segments <- arraySize 0 <$> lookupType offsets
  letTupExp "segred" <=< genTabulate lvl num_segments $ \i -> do
    n <- letSubExp "n" =<< eIndex segments [eSubExp i]
    offset <- letSubExp "offset" =<< eIndex offsets [toExp (pe64 i)]
    letTupExp' "segment_res" <=< eIf (toExp $ pe64 n .==. 0) (eBody $ map eSubExp nes) $
      eBody $
        map (`eIndex` [toExp $ pe64 offset + pe64 n - 1]) scanned
  where
    nes = redNeutral red

genSegRedomap ::
  SegLevel ->
  VName ->
  VName ->
  VName ->
  [VName] ->
  Reduce SOACS ->
  Lambda GPU ->
  ([SubExp] -> Builder GPU ()) ->
  Builder GPU ([VName], [VName])
genSegRedomap lvl segments flags offsets elems red map_lam readFree = do
  scanned_and_map <-
    genSegScanomap
      lvl
      "redomap"
      (soacsLambdaToGPU $ redLambda red)
      (redNeutral red)
      flags
      map_lam
      elems
      readFree
  let (scanned, mapout) = splitAt (length nes) scanned_and_map
  num_segments <- arraySize 0 <$> lookupType offsets
  reds <- letTupExp "segred" <=< genTabulate lvl num_segments $ \i -> do
    n <- letSubExp "n" =<< eIndex segments [eSubExp i]
    offset <- letSubExp "offset" =<< eIndex offsets [toExp (pe64 i)]
    letTupExp' "segment_res" <=< eIf (toExp $ pe64 n .==. 0) (eBody $ map eSubExp nes) $
      eBody $
        map (`eIndex` [toExp $ pe64 offset + pe64 n - 1]) scanned
  pure (reds, mapout)
  where
    nes = redNeutral red

-- | Produces a multidimensional iota for the given shape.
genShapeIota :: SegLevel -> Shape -> Builder GPU VName
genShapeIota lvl shape = do
  let dims = shapeDims shape
  letExp "shape_iota" <=< segMap lvl dims $ \gtids -> do
    i <-
      toSubExp "shape_iota_elem" $
        flattenIndex (map pe64 dims) (map pe64 gtids)
    pure [subExpRes i]

-- Returns (#segments, segment start offsets, sum of segment sizes)
-- Note: If given a multi-dimensional array,
-- `#segments` and `sum of segment sizes` will be arrays, not scalars.
-- `segment start offsets` will always have the same shape as `ks`.
exScanAndSum :: SegLevel -> VName -> Builder GPU (SubExp, VName, SubExp)
exScanAndSum lvl ks = do
  ns <- arrayDims <$> lookupType ks
  -- If `ks` only has a single dimension
  -- the size will be a scalar, otherwise it's an array.
  ns' <- letExp "ns" $ BasicOp $ case ns of
    [] -> error $ "exScanAndSum: Given non-array argument: " ++ prettyString ks
    [n] -> SubExp n
    _ -> ArrayLit ns (Prim int64)
  -- Check if the innermost dimension is empty.
  is_empty <-
    letExp "is_empty"
      =<< ( case ns of
              [n] -> toExp (pe64 n .==. 0)
              _ -> eLast ns' >>= letSubExp "n" >>= (\n -> toExp $ pe64 n .==. 0)
          )
  offsets <- letExp "offsets" =<< toExp =<< genExPrefixSum lvl "offsets" ks
  ms <- letExp "ms" <=< segMap lvl (init ns) $ \gtids -> do
    let idxs = map toExp gtids
    offset <- letExp "offset" =<< eIndex offsets idxs
    k <- letExp "k" =<< eIndex ks idxs
    m <-
      letSubExp "m"
        =<< eIf
          (toExp is_empty)
          (eBody [eSubExp $ intConst Int64 0])
          -- Add last size because 'offsets' is an *exclusive* prefix
          -- sum.
          (eBody [eBinOp (Add Int64 OverflowUndef) (eLast offset) (eLast k)])
    pure [subExpRes m]
  pure (Var ns', offsets, Var ms)

genSegIota :: SegLevel -> VName -> Builder GPU (VName, VName, VName)
genSegIota lvl ks = do
  (_n, offsets, m) <- exScanAndSum lvl ks
  flags <- genFlags lvl m offsets
  ones <- letExp "ones" $ BasicOp $ Replicate (Shape [m]) one
  iotas <- genSegPrefixSum lvl "iotas" flags ones
  res <- letExp "res" <=< genTabulate lvl m $ \i -> do
    x <- letSubExp "x" =<< eIndex iotas [eSubExp i]
    letTupExp' "xm1" $ BasicOp $ BinOp (Sub Int64 OverflowUndef) x one
  pure (flags, offsets, res)
  where
    one = intConst Int64 1

genRepIota :: SegLevel -> VName -> Builder GPU (VName, VName, VName)
genRepIota lvl ks = do
  (n, offsets, m) <- exScanAndSum lvl ks
  is <- letExp "is" <=< genTabulate lvl n $ \i -> do
    o <- letSubExp "o" =<< eIndex offsets [eSubExp i]
    k <- letSubExp "n" =<< eIndex ks [eSubExp i]
    letTupExp' "i"
      =<< eIf
        (toExp (pe64 k .==. 0))
        (eBody [eSubExp negone])
        (eBody [toExp $ pe64 o])
  zeroes <- letExp "zeroes" $ BasicOp $ Replicate (Shape [m]) zero
  starts <-
    letExp "starts" <=< genScatter lvl zeroes n $ \gtid -> do
      i <- letExp "i" =<< eIndex is [eSubExp gtid]
      pure (i, gtid)
  flags <- letExp "flags" <=< genTabulate lvl m $ \i -> do
    x <- letSubExp "x" =<< eIndex starts [eSubExp i]
    letTupExp' "nonzero" =<< toExp (pe64 x .>. 0)
  res <- genSegPrefixSum lvl "res" flags starts
  pure (flags, offsets, res)
  where
    zero = intConst Int64 0
    negone = intConst Int64 (-1)

genPartition :: SegLevel -> VName -> VName -> VName -> Builder GPU (VName, VName, VName)
genPartition lvl n k cls = do
  let n' = Var n
  let k' = Var k
  let dims = [k', n']
  -- Create a `[k][n]` array of flags such that `cls_flags[i][j]`
  -- is equal 1 if the j'th element is a member of equivalence class `i` i.e.
  -- the `i`th row is a flag array for equivalence class `i`.
  cls_flags <-
    letExp "flags"
      <=< segMap lvl dims
      $ \[i, j] -> do
        c <- letSubExp "c" =<< eIndex cls [toExp j]
        cls_flag <-
          letSubExp "cls_flag"
            =<< eIf
              (toExp $ pe64 i .==. pe64 c)
              (eBody [toExp $ intConst Int64 1])
              (eBody [toExp $ intConst Int64 0])
        pure [subExpRes cls_flag]

  -- Offsets of each of the individual equivalence classes.
  (_, local_offs, _counts) <- exScanAndSum lvl cls_flags
  -- The number of elems in each class
  counts <- letExp "counts" =<< toExp _counts
  -- Offsets of the whole equivalence classes
  global_offs <- genExPrefixSum lvl "global_offs" counts
  -- Offsets over all of the equivalence classes.
  cls_offs <-
    letExp "cls_offs" =<< do
      segMap lvl dims $ \[i, j] -> do
        global_offset <- letExp "global_offset" =<< eIndex global_offs [toExp i]
        offset <-
          letSubExp "offset"
            =<< eBinOp
              (Add Int64 OverflowUndef)
              (eIndex local_offs [toExp i, toExp j])
              (toExp global_offset)
        pure [subExpRes offset]

  scratch <- letExp "scratch" $ BasicOp $ Scratch int64 [n']
  res <- letExp "scatter_res" <=< genScatter lvl scratch n' $ \gtid -> do
    c <- letExp "c" =<< eIndex cls [toExp gtid]
    ind <- letExp "ind" =<< eIndex cls_offs [toExp c, toExp gtid]
    i <- letSubExp "i" =<< toExp gtid
    pure (ind, i)
  pure (counts, global_offs, res)

genFilter :: SegLevel -> VName -> BuilderT GPU (State VNameSource) (SubExp, VName)
genFilter lvl flags = do
  w <- arraySize 0 <$> lookupType flags
  flags_int <- letExp "flags_int" <=< segMap lvl [w] $ \[i] -> do
    b <- letSubExp "b" =<< eIndex flags [eSubExp i]
    v <-
      letSubExp "v"
        =<< eIf
          (eSubExp b)
          (eBody [toExp $ intConst Int64 1])
          (eBody [toExp $ intConst Int64 0])
    pure [subExpRes v]
  -- offsets <- genExPrefixSum "filter_offs" flags_int
  (_n, offsets, num_true) <- exScanAndSum lvl flags_int
  -- num_true <- letSubExp "num_true"  =<< eIndex flags_int [toExp $ pe64 w - 1]
  scratch <- letExp "scratch" $ BasicOp $ Scratch int64 [num_true]
  -- is this efficient or do i need to do something smarter? like scatter with guard?
  -- offsets' <- letExp "offset" <=< segMap [w] $ \[i] -> do
  --   b' <- letSubExp "b" =<< eIndex flags [eSubExp i]
  --   v' <-
  --     letSubExp "v'"
  --       =<< eIf
  --         (eSubExp b')
  --         (eBody [eIndex offsets [eSubExp i]] )
  --         (eBody [toExp $ intConst Int64 (-1)])
  --   pure [subExpRes v']

  filtered <- letExp "filtered" <=< genScatter lvl scratch w $ \gtid -> do
    b <- letSubExp "b" =<< eIndex flags [eSubExp gtid]
    -- idx <- letExp "idx" =<< eIndex offsets' [eSubExp gtid]
    idx_se <-
      letSubExp "idx"
        =<< eIf
          (eSubExp b)
          (eBody [eIndex offsets [eSubExp gtid]])
          (eBody [toExp $ intConst Int64 (-1)])
    -- maybe cleaner?
    idx <- letExp "idx" =<< toExp idx_se
    pure (idx, gtid)
  pure (num_true, filtered)

buildingBuiltin :: Builder GPU (FunDef GPU) -> FunDef GPU
buildingBuiltin m = fst $ evalState (runBuilderT m mempty) blankNameSource

segIotaBuiltin :: FunDef GPU
segIotaBuiltin = buildingBuiltin $ do
  np <- newParam "n" $ Prim int64
  nsp <- newParam "ns" $ Array int64 (Shape [Var (paramName np)]) Nonunique
  body <-
    localScope (scopeOfFParams [np, nsp]) . buildBody_ $ do
      (flags, offsets, res) <- genSegIota regularSegLevel (paramName nsp)
      m <- arraySize 0 <$> lookupType res
      pure $ subExpsRes [m, Var flags, Var offsets, Var res]
  pure
    FunDef
      { funDefEntryPoint = Nothing,
        funDefAttrs = mempty,
        funDefName = segIotaName,
        funDefRetType =
          map
            (,mempty)
            [ Prim int64,
              Array Bool (Shape [Ext 0]) Unique,
              Array int64 (Shape [Free $ Var $ paramName np]) Unique,
              Array int64 (Shape [Ext 0]) Unique
            ],
        funDefParams = [np, nsp],
        funDefBody = body
      }

repIotaBuiltin :: FunDef GPU
repIotaBuiltin = buildingBuiltin $ do
  np <- newParam "n" $ Prim int64
  nsp <- newParam "ns" $ Array int64 (Shape [Var (paramName np)]) Nonunique
  body <-
    localScope (scopeOfFParams [np, nsp]) . buildBody_ $ do
      (flags, offsets, res) <- genRepIota regularSegLevel (paramName nsp)
      m <- arraySize 0 <$> lookupType res
      pure $ subExpsRes [m, Var flags, Var offsets, Var res]
  pure
    FunDef
      { funDefEntryPoint = Nothing,
        funDefAttrs = mempty,
        funDefName = repIotaName,
        funDefRetType =
          map
            (,mempty)
            [ Prim int64,
              Array Bool (Shape [Ext 0]) Unique,
              Array int64 (Shape [Free $ Var $ paramName np]) Unique,
              Array int64 (Shape [Ext 0]) Unique
            ],
        funDefParams = [np, nsp],
        funDefBody = body
      }

prefixSumBuiltin :: FunDef GPU
prefixSumBuiltin = buildingBuiltin $ do
  np <- newParam "n" $ Prim int64
  nsp <- newParam "ns" $ Array int64 (Shape [Var (paramName np)]) Nonunique
  body <-
    localScope (scopeOfFParams [np, nsp]) . buildBody_ $
      varsRes . pure <$> genPrefixSum regularSegLevel "res" (paramName nsp)
  pure
    FunDef
      { funDefEntryPoint = Nothing,
        funDefAttrs = mempty,
        funDefName = prefixSumName,
        funDefRetType =
          [(Array int64 (Shape [Free $ Var $ paramName np]) Unique, mempty)],
        funDefParams = [np, nsp],
        funDefBody = body
      }

partitionBuiltin :: FunDef GPU
partitionBuiltin = buildingBuiltin $ do
  np <- newParam "n" $ Prim int64
  kp <- newParam "k" $ Prim int64
  csp <- newParam "cs" $ Array int64 (Shape [Var (paramName np)]) Nonunique
  body <-
    localScope (scopeOfFParams [np, kp, csp]) . buildBody_ $ do
      (counts, offsets, res) <- genPartition regularSegLevel (paramName np) (paramName kp) (paramName csp)
      pure $ varsRes [counts, offsets, res]
  pure
    FunDef
      { funDefEntryPoint = Nothing,
        funDefAttrs = mempty,
        funDefName = partitionName,
        funDefRetType =
          map
            (,mempty)
            [ Array int64 (Shape [Free $ Var $ paramName kp]) Unique,
              Array int64 (Shape [Free $ Var $ paramName kp]) Unique,
              Array int64 (Shape [Free $ Var $ paramName np]) Unique
            ],
        funDefParams = [np, kp, csp],
        funDefBody = body
      }

-- | Builtin functions used in flattening.  Must be prepended to a
-- program that is transformed by flattening.  The intention is to
-- avoid the code explosion that would result if we inserted
-- primitives everywhere.
flatteningBuiltins :: [FunDef GPU]
flatteningBuiltins =
  [ segIotaBuiltin,
    repIotaBuiltin,
    prefixSumBuiltin,
    partitionBuiltin
  ]

-- | @[0,1,2,0,1,0,1,2,3,4,...]@.  Returns @(flags,offsets,elems)@.
doSegIota :: SegLevel -> VName -> Builder GPU (VName, VName, VName)
doSegIota lvl ns
  | inlineBuiltinAtLevel lvl =
      genSegIota lvl ns
  | otherwise = do
      ns_t <- lookupType ns
      let n = arraySize 0 ns_t
      m <- newVName "m"
      flags <- newVName "segiota_flags"
      offsets <- newVName "segiota_offsets"
      elems <- newVName "segiota_elems"
      let args = [(n, Prim int64), (Var ns, ns_t)]
          restype =
            fromMaybe (error "doSegIota: bad application") $
              applyRetType
                (map fst $ funDefRetType segIotaBuiltin)
                (funDefParams segIotaBuiltin)
                args
      letBindNames [m, flags, offsets, elems] $
        Apply
          (funDefName segIotaBuiltin)
          [(n, Observe), (Var ns, Observe)]
          (map (,mempty) restype)
          Safe
      pure (flags, offsets, elems)

-- | Produces @[0,0,0,1,1,2,2,2,...]@.  Returns @(flags, offsets,
-- elems)@.
doRepIota :: SegLevel -> VName -> Builder GPU (VName, VName, VName)
doRepIota lvl ns
  | inlineBuiltinAtLevel lvl =
      genRepIota lvl ns
  | otherwise = do
      ns_t <- lookupType ns
      let n = arraySize 0 ns_t
      m <- newVName "m"
      flags <- newVName "repiota_flags"
      offsets <- newVName "repiota_offsets"
      elems <- newVName "repiota_elems"
      let args = [(n, Prim int64), (Var ns, ns_t)]
          restype =
            fromMaybe (error "doRepIota: bad application") $
              applyRetType
                (map fst $ funDefRetType repIotaBuiltin)
                (funDefParams repIotaBuiltin)
                args
      letBindNames [m, flags, offsets, elems] $
        Apply
          (funDefName repIotaBuiltin)
          [(n, Observe), (Var ns, Observe)]
          (map (,mempty) restype)
          Safe
      pure (flags, offsets, elems)

doPrefixSum :: SegLevel -> VName -> Builder GPU VName
doPrefixSum lvl ns
  | inlineBuiltinAtLevel lvl =
      genPrefixSum lvl "prefix_sum" ns
  | otherwise = do
      ns_t <- lookupType ns
      let n = arraySize 0 ns_t
      letExp "prefix_sum" $
        Apply
          (funDefName prefixSumBuiltin)
          [(n, Observe), (Var ns, Observe)]
          [(toDecl (staticShapes1 ns_t) Unique, mempty)]
          Safe

doPartition :: SegLevel -> VName -> VName -> Builder GPU (VName, VName, VName)
doPartition lvl k cs
  | inlineBuiltinAtLevel lvl = do
      cs_t <- lookupType cs
      n <- letExp "n" $ BasicOp $ SubExp $ arraySize 0 cs_t
      genPartition lvl n k cs
  | otherwise = do
      cs_t <- lookupType cs
      let n = arraySize 0 cs_t
      counts <- newVName "partition_counts"
      offsets <- newVName "partition_offsets"
      res <- newVName "partition_res"
      let args = [(n, Prim int64), (Var k, Prim int64), (Var cs, cs_t)]
          restype =
            fromMaybe (error "doPartition: bad application") $
              applyRetType
                (map fst $ funDefRetType partitionBuiltin)
                (funDefParams partitionBuiltin)
                args
      letBindNames [counts, offsets, res] $
        Apply
          (funDefName partitionBuiltin)
          [(n, Observe), (Var k, Observe), (Var cs, Observe)]
          (map (,mempty) restype)
          Safe
      pure (counts, offsets, res)
