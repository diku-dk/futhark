{-# LANGUAGE TypeFamilies #-}

module Futhark.Pass.Flatten.Builtins
  ( flatteningBuiltins,
    segMap,
    genFlags,
    genSegScan,
    genSegRed,
    genScatter,
    exScanAndSum,
    doSegIota,
    doPrefixSum,
    doRepIota,
    doPartition,
  )
where

import Control.Monad (forM, (<=<))
import Control.Monad.State.Strict
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Futhark.IR.GPU
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Pass.ExtractKernels.BlockedKernel (mkSegSpace)
import Futhark.Pass.ExtractKernels.ToGPU (soacsLambdaToGPU)
import Futhark.Tools
import Futhark.Util (unsnoc)

builtinName :: T.Text -> Name
builtinName = nameFromText . ("builtin#" <>)

segIotaName, repIotaName, prefixSumName, partitionName :: Name
segIotaName = builtinName "segiota"
repIotaName = builtinName "repiota"
prefixSumName = builtinName "prefixsum"
partitionName = builtinName "partition"

segMap :: (Traversable f) => f SubExp -> (f SubExp -> Builder GPU Result) -> Builder GPU (Exp GPU)
segMap segments f = do
  gtids <- traverse (const $ newVName "gtid") segments
  space <- mkSegSpace $ zip (toList gtids) (toList segments)
  ((res, ts), stms) <- collectStms $ localScope (scopeOfSegSpace space) $ do
    res <- f $ fmap Var gtids
    ts <- mapM (subExpType . resSubExp) res
    pure (map mkResult res, ts)
  let kbody = Body () stms res
  pure $ Op $ SegOp $ SegMap (SegThread SegVirt Nothing) space ts kbody
  where
    mkResult (SubExpRes cs se) = Returns ResultMaySimplify cs se

genScanomap ::
  (Traversable f) =>
  Name ->
  f SubExp ->
  Lambda GPU ->
  [SubExp] ->
  (f SubExp -> Builder GPU [SubExp]) ->
  Builder GPU [VName]
genScanomap desc segments lam nes m = do
  gtids <- traverse (const $ newVName "gtid") segments
  space <- mkSegSpace $ zip (toList gtids) (toList segments)
  ((res, res_t), stms) <- runBuilder . localScope (scopeOfSegSpace space) $ do
    res <- m $ fmap Var gtids
    res_t <- mapM subExpType res
    pure (map (Returns ResultMaySimplify mempty) res, res_t)
  let kbody = Body () stms res
      op = SegBinOp Commutative lam nes mempty
  letTupExp desc $ Op $ SegOp $ SegScan lvl space res_t kbody [op]
  where
    lvl = SegThread SegVirt Nothing

genScan :: (Traversable f) => Name -> f SubExp -> Lambda GPU -> [SubExp] -> [VName] -> Builder GPU [VName]
genScan desc segments lam nes arrs =
  genScanomap desc segments lam nes $ \gtids -> forM arrs $ \arr ->
    letSubExp (baseName arr <> "_elem") =<< eIndex arr (toList $ fmap eSubExp gtids)

-- Also known as a prescan.
genExScan :: (Traversable f) => Name -> f SubExp -> Lambda GPU -> [SubExp] -> [VName] -> Builder GPU [VName]
genExScan desc segments lam nes arrs =
  genScanomap desc segments lam nes $ \gtids ->
    let Just (outerDims, innerDim) = unsnoc $ toList gtids
     in letTupExp' "to_prescan"
          =<< eIf
            (toExp $ pe64 innerDim .==. 0)
            (eBody (map eSubExp nes))
            (eBody (map (`eIndex` (map toExp outerDims ++ [toExp $ pe64 innerDim - 1])) arrs))

segScanLambda ::
  (MonadBuilder m, BranchType (Rep m) ~ ExtType, LParamInfo (Rep m) ~ Type) =>
  Lambda (Rep m) ->
  m (Lambda (Rep m))
segScanLambda lam = do
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
            (pure $ lambdaBody lam)
        ]

genSegScan :: Name -> Lambda GPU -> [SubExp] -> VName -> [VName] -> Builder GPU [VName]
genSegScan desc lam nes flags arrs = do
  w <- arraySize 0 <$> lookupType flags
  lam' <- segScanLambda lam
  drop 1 <$> genScan desc [w] lam' (constant False : nes) (flags : arrs)

genPrefixSum :: Name -> VName -> Builder GPU VName
genPrefixSum desc ns = do
  ws <- arrayDims <$> lookupType ns
  add_lam <- binOpLambda (Add Int64 OverflowUndef) int64
  head <$> genScan desc ws add_lam [intConst Int64 0] [ns]

genExPrefixSum :: Name -> VName -> Builder GPU VName
genExPrefixSum desc ns = do
  ws <- arrayDims <$> lookupType ns
  add_lam <- binOpLambda (Add Int64 OverflowUndef) int64
  head <$> genExScan desc ws add_lam [intConst Int64 0] [ns]

genSegPrefixSum :: Name -> VName -> VName -> Builder GPU VName
genSegPrefixSum desc flags ns = do
  add_lam <- binOpLambda (Add Int64 OverflowUndef) int64
  head <$> genSegScan desc add_lam [intConst Int64 0] flags [ns]

genScatter :: VName -> SubExp -> (SubExp -> Builder GPU (VName, SubExp)) -> Builder GPU (Exp GPU)
genScatter dest n f = do
  gtid <- newVName "gtid"
  space <- mkSegSpace [(gtid, n)]
  withAcc [dest] 1 $ \ ~[acc] -> do
    kbody <- buildBody_ $ localScope (scopeOfSegSpace space) $ do
      (i, v) <- f $ Var gtid
      acc' <- letExp (baseName acc) $ BasicOp $ UpdateAcc Safe acc [Var i] [v]
      pure [Returns ResultMaySimplify mempty $ Var acc']
    acc_t <- lookupType acc
    letTupExp' "scatter" $ Op $ SegOp $ SegMap (SegThread SegVirt Nothing) space [acc_t] kbody

genTabulate :: SubExp -> (SubExp -> Builder GPU [SubExp]) -> Builder GPU (Exp GPU)
genTabulate w m = do
  gtid <- newVName "gtid"
  space <- mkSegSpace [(gtid, w)]
  ((res, ts), stms) <- collectStms $ localScope (scopeOfSegSpace space) $ do
    ses <- m $ Var gtid
    ts <- mapM subExpType ses
    pure (map (Returns ResultMaySimplify mempty) ses, ts)
  let kbody = Body () stms res
  pure $ Op $ SegOp $ SegMap (SegThread SegVirt Nothing) space ts kbody

genFlags :: SubExp -> VName -> Builder GPU VName
genFlags m offsets = do
  flags_allfalse <-
    letExp "flags_allfalse" . BasicOp $
      Replicate (Shape [m]) (constant False)
  n <- arraySize 0 <$> lookupType offsets
  letExp "flags" <=< genScatter flags_allfalse n $ \gtid -> do
    i <- letExp "i" =<< eIndex offsets [eSubExp gtid]
    pure (i, constant True)

genSegRed :: VName -> VName -> VName -> [VName] -> Reduce SOACS -> Builder GPU [VName]
genSegRed segments flags offsets elems red = do
  scanned <-
    genSegScan
      "red"
      (soacsLambdaToGPU $ redLambda red)
      (redNeutral red)
      flags
      elems
  num_segments <- arraySize 0 <$> lookupType offsets
  letTupExp "segred" <=< genTabulate num_segments $ \i -> do
    n <- letSubExp "n" =<< eIndex segments [eSubExp i]
    offset <- letSubExp "offset" =<< eIndex offsets [toExp (pe64 i)]
    letTupExp' "segment_res" <=< eIf (toExp $ pe64 n .==. 0) (eBody $ map eSubExp nes) $
      eBody $
        map (`eIndex` [toExp $ pe64 offset + pe64 n - 1]) scanned
  where
    nes = redNeutral red

-- Returns (#segments, segment start offsets, sum of segment sizes)
-- Note: If given a multi-dimensional array,
-- `#segments` and `sum of segment sizes` will be arrays, not scalars.
-- `segment start offsets` will always have the same shape as `ks`.
exScanAndSum :: VName -> Builder GPU (SubExp, VName, SubExp)
exScanAndSum ks = do
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
  offsets <- letExp "offsets" =<< toExp =<< genExPrefixSum "offsets" ks
  ms <- letExp "ms" <=< segMap (init ns) $ \gtids -> do
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

genSegIota :: VName -> Builder GPU (VName, VName, VName)
genSegIota ks = do
  (_n, offsets, m) <- exScanAndSum ks
  flags <- genFlags m offsets
  ones <- letExp "ones" $ BasicOp $ Replicate (Shape [m]) one
  iotas <- genSegPrefixSum "iotas" flags ones
  res <- letExp "res" <=< genTabulate m $ \i -> do
    x <- letSubExp "x" =<< eIndex iotas [eSubExp i]
    letTupExp' "xm1" $ BasicOp $ BinOp (Sub Int64 OverflowUndef) x one
  pure (flags, offsets, res)
  where
    one = intConst Int64 1

genRepIota :: VName -> Builder GPU (VName, VName, VName)
genRepIota ks = do
  (n, offsets, m) <- exScanAndSum ks
  is <- letExp "is" <=< genTabulate n $ \i -> do
    o <- letSubExp "o" =<< eIndex offsets [eSubExp i]
    k <- letSubExp "n" =<< eIndex ks [eSubExp i]
    letTupExp' "i"
      =<< eIf
        (toExp (pe64 k .==. 0))
        (eBody [eSubExp negone])
        (eBody [toExp $ pe64 o])
  zeroes <- letExp "zeroes" $ BasicOp $ Replicate (Shape [m]) zero
  starts <-
    letExp "starts" <=< genScatter zeroes n $ \gtid -> do
      i <- letExp "i" =<< eIndex is [eSubExp gtid]
      pure (i, gtid)
  flags <- letExp "flags" <=< genTabulate m $ \i -> do
    x <- letSubExp "x" =<< eIndex starts [eSubExp i]
    letTupExp' "nonzero" =<< toExp (pe64 x .>. 0)
  res <- genSegPrefixSum "res" flags starts
  pure (flags, offsets, res)
  where
    zero = intConst Int64 0
    negone = intConst Int64 (-1)

genPartition :: VName -> VName -> VName -> Builder GPU (VName, VName, VName)
genPartition n k cls = do
  let n' = Var n
  let k' = Var k
  let dims = [k', n']
  -- Create a `[k][n]` array of flags such that `cls_flags[i][j]`
  -- is equal 1 if the j'th element is a member of equivalence class `i` i.e.
  -- the `i`th row is a flag array for equivalence class `i`.
  cls_flags <-
    letExp "flags"
      <=< segMap dims
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
  (_, local_offs, _counts) <- exScanAndSum cls_flags
  -- The number of elems in each class
  counts <- letExp "counts" =<< toExp _counts
  -- Offsets of the whole equivalence classes
  global_offs <- genExPrefixSum "global_offs" counts
  -- Offsets over all of the equivalence classes.
  cls_offs <-
    letExp "cls_offs" =<< do
      segMap dims $ \[i, j] -> do
        global_offset <- letExp "global_offset" =<< eIndex global_offs [toExp i]
        offset <-
          letSubExp "offset"
            =<< eBinOp
              (Add Int64 OverflowUndef)
              (eIndex local_offs [toExp i, toExp j])
              (toExp global_offset)
        pure [subExpRes offset]

  scratch <- letExp "scratch" $ BasicOp $ Scratch int64 [n']
  res <- letExp "scatter_res" <=< genScatter scratch n' $ \gtid -> do
    c <- letExp "c" =<< eIndex cls [toExp gtid]
    ind <- letExp "ind" =<< eIndex cls_offs [toExp c, toExp gtid]
    i <- letSubExp "i" =<< toExp gtid
    pure (ind, i)
  pure (counts, global_offs, res)

buildingBuiltin :: Builder GPU (FunDef GPU) -> FunDef GPU
buildingBuiltin m = fst $ evalState (runBuilderT m mempty) blankNameSource

segIotaBuiltin :: FunDef GPU
segIotaBuiltin = buildingBuiltin $ do
  np <- newParam "n" $ Prim int64
  nsp <- newParam "ns" $ Array int64 (Shape [Var (paramName np)]) Nonunique
  body <-
    localScope (scopeOfFParams [np, nsp]) . buildBody_ $ do
      (flags, offsets, res) <- genSegIota (paramName nsp)
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
      (flags, offsets, res) <- genRepIota (paramName nsp)
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
      varsRes . pure <$> genPrefixSum "res" (paramName nsp)
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
      (counts, offsets, res) <- genPartition (paramName np) (paramName kp) (paramName csp)
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
flatteningBuiltins = [segIotaBuiltin, repIotaBuiltin, prefixSumBuiltin, partitionBuiltin]

-- | @[0,1,2,0,1,0,1,2,3,4,...]@.  Returns @(flags,offsets,elems)@.
doSegIota :: VName -> Builder GPU (VName, VName, VName)
doSegIota ns = do
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
doRepIota :: VName -> Builder GPU (VName, VName, VName)
doRepIota ns = do
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

doPrefixSum :: VName -> Builder GPU VName
doPrefixSum ns = do
  ns_t <- lookupType ns
  let n = arraySize 0 ns_t
  letExp "prefix_sum" $
    Apply
      (funDefName prefixSumBuiltin)
      [(n, Observe), (Var ns, Observe)]
      [(toDecl (staticShapes1 ns_t) Unique, mempty)]
      Safe

doPartition :: VName -> VName -> Builder GPU (VName, VName, VName)
doPartition k cs = do
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
