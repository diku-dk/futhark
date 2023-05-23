{-# LANGUAGE TypeFamilies #-}

module Futhark.Pass.Flatten.Builtins
  ( flatteningBuiltins,
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

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Futhark.IR.GPU
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Pass.ExtractKernels.BlockedKernel (mkSegSpace)
import Futhark.Pass.ExtractKernels.ToGPU (soacsLambdaToGPU)
import Futhark.Tools

builtinName :: T.Text -> Name
builtinName = nameFromText . ("builtin#" <>)

segIotaName, repIotaName, prefixSumName, partitionName :: Name
segIotaName = builtinName "segiota"
repIotaName = builtinName "repiota"
prefixSumName = builtinName "prefixsum"
partitionName = builtinName "partition"

genScanomap :: String -> SubExp -> Lambda GPU -> [SubExp] -> (SubExp -> Builder GPU [SubExp]) -> Builder GPU [VName]
genScanomap desc w lam nes m = do
  gtid <- newVName "gtid"
  space <- mkSegSpace [(gtid, w)]
  ((res, res_t), stms) <- runBuilder . localScope (scopeOfSegSpace space) $ do
    res <- m $ Var gtid
    res_t <- mapM subExpType res
    pure (map (Returns ResultMaySimplify mempty) res, res_t)
  let kbody = KernelBody () stms res
      op = SegBinOp Commutative lam nes mempty
  letTupExp desc $ Op $ SegOp $ SegScan lvl space [op] res_t kbody
  where
    lvl = SegThread SegVirt Nothing

genScan :: String -> SubExp -> Lambda GPU -> [SubExp] -> [VName] -> Builder GPU [VName]
genScan desc w lam nes arrs =
  genScanomap desc w lam nes $ \gtid -> forM arrs $ \arr ->
    letSubExp (baseString arr <> "_elem") =<< eIndex arr [eSubExp gtid]

-- Also known as a prescan.
genExScan :: String -> SubExp -> Lambda GPU -> [SubExp] -> [VName] -> Builder GPU [VName]
genExScan desc w lam nes arrs =
  genScanomap desc w lam nes $ \gtid ->
    letTupExp' "to_prescan"
      =<< eIf
        (toExp $ pe64 gtid .==. 0)
        (eBody (map eSubExp nes))
        (eBody (map (`eIndex` [toExp $ pe64 gtid - 1]) arrs))

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

genSegScan :: String -> Lambda GPU -> [SubExp] -> VName -> [VName] -> Builder GPU [VName]
genSegScan desc lam nes flags arrs = do
  w <- arraySize 0 <$> lookupType flags
  lam' <- segScanLambda lam
  drop 1 <$> genScan desc w lam' (constant False : nes) (flags : arrs)

genPrefixSum :: String -> VName -> Builder GPU VName
genPrefixSum desc ns = do
  w <- arraySize 0 <$> lookupType ns
  add_lam <- binOpLambda (Add Int64 OverflowUndef) int64
  head <$> genScan desc w add_lam [intConst Int64 0] [ns]

genExPrefixSum :: String -> VName -> Builder GPU VName
genExPrefixSum desc ns = do
  w <- arraySize 0 <$> lookupType ns
  add_lam <- binOpLambda (Add Int64 OverflowUndef) int64
  head <$> genExScan desc w add_lam [intConst Int64 0] [ns]

genSegPrefixSum :: String -> VName -> VName -> Builder GPU VName
genSegPrefixSum desc flags ns = do
  add_lam <- binOpLambda (Add Int64 OverflowUndef) int64
  head <$> genSegScan desc add_lam [intConst Int64 0] flags [ns]

genScatter :: VName -> SubExp -> (SubExp -> Builder GPU (VName, SubExp)) -> Builder GPU (Exp GPU)
genScatter dest n f = do
  m <- arraySize 0 <$> lookupType dest
  gtid <- newVName "gtid"
  space <- mkSegSpace [(gtid, n)]
  ((res, v_t), stms) <- collectStms $ localScope (scopeOfSegSpace space) $ do
    (i, v) <- f $ Var gtid
    v_t <- subExpType v
    pure (WriteReturns mempty (Shape [m]) dest [(Slice [DimFix (Var i)], v)], v_t)
  let kbody = KernelBody () stms [res]
  pure $ Op $ SegOp $ SegMap (SegThread SegVirt Nothing) space [v_t] kbody

genTabulate :: SubExp -> (SubExp -> Builder GPU [SubExp]) -> Builder GPU (Exp GPU)
genTabulate w m = do
  gtid <- newVName "gtid"
  space <- mkSegSpace [(gtid, w)]
  ((res, ts), stms) <- collectStms $ localScope (scopeOfSegSpace space) $ do
    ses <- m $ Var gtid
    ts <- mapM subExpType ses
    pure (map (Returns ResultMaySimplify mempty) ses, ts)
  let kbody = KernelBody () stms res
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
exScanAndSum :: VName -> Builder GPU (SubExp, VName, SubExp)
exScanAndSum ks = do
  n <- arraySize 0 <$> lookupType ks
  is_empty <- letSubExp "is_empty" =<< toExp (pe64 n .==. 0)
  offsets <- genExPrefixSum "offsets" ks
  m <-
    letSubExp "m"
      =<< eIf
        (eSubExp is_empty)
        (eBody [eSubExp $ intConst Int64 0])
        -- Add last size because 'offsets' is an *exclusive* prefix
        -- sum.
        (eBody [eBinOp (Add Int64 OverflowUndef) (eLast offsets) (eLast ks)])
  pure (n, offsets, m)

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

genPartition :: VName -> VName -> VName -> Builder GPU (VName, VName)
genPartition n k cls = do
  let n' = Var n
  let k' = Var k
  let dims = [k', n']
  let reshape shp = BasicOp . Reshape ReshapeArbitrary shp

  m <- letSubExp "m" =<< eBinOp (Mul Int64 OverflowUndef) (toExp k') (toExp n')
  -- Calculate `[k][n]` (flat) array of flags such that `cls_flags[i][j]` is equal 1 if
  -- the j'th element is a member of equivalence class `i`.
  -- `seg_flags` is a flag array describing the shape of `cls_flags` i.e. every `k`'th element is `True`
  ~[cls_flags, seg_flags] <-
    mapM (letExp "flags_flat" . reshape (Shape [m])) =<< letTupExp "flags" =<< do
      gtids <- traverse (const $ newVName "gtid") dims
      space <- mkSegSpace $ zip gtids dims
      ((res, ts), stms) <- collectStms $ localScope (scopeOfSegSpace space) $ do
        let [i, j] = gtids
        c <- letSubExp "c" =<< eIndex cls [toExp j]
        cls_flag <-
          letSubExp "cls_flag"
            =<< eIf
              (toExp $ pe64 (Var i) .==. pe64 c)
              (eBody [toExp $ intConst Int64 1])
              (eBody [toExp $ intConst Int64 0])
        seg_flag <-
          letSubExp "seg_flag"
            =<< eIf
              (toExp $ pe64 (Var j) .==. 0)
              (eBody [toExp $ constant True])
              (eBody [toExp $ constant False])
        let res_ts = [(cls_flag, Prim int64), (seg_flag, Prim Bool)]
        let (res, ts) = unzip res_ts
        pure (map (Returns ResultMaySimplify mempty) res, ts)
      let kbody = KernelBody () stms res
      pure $ Op $ SegOp $ SegMap (SegThread SegVirt Nothing) space ts kbody

  -- Offsets of each of the individual equivalence classes. Note: not an exclusive scan!
  local_offs <- letExp "local_offs" . reshape (Shape dims) =<< genSegPrefixSum "local_offs_flat" seg_flags cls_flags
  -- The number of elems in each class
  counts <- letExp "counts" <=< genTabulate k' $ \i -> do
    row <- letExp "offs_row" =<< eIndex local_offs [toExp i]
    letTupExp' "count" =<< eLast row
  -- Offsets of the whole equivalence classes
  global_offs <- genExPrefixSum "global_offs" counts
  -- Offsets over all of the equivalence classes.
  cls_offs <-
    letExp "cls_offs" =<< do
      gtids <- traverse (const $ newVName "gtid") dims
      space <- mkSegSpace $ zip gtids dims
      ((res, ts), stms) <- collectStms $ localScope (scopeOfSegSpace space) $ do
        let [i, j] = gtids
        global_offset <- letExp "global_offset" =<< eIndex global_offs [toExp i]
        offset <-
          letSubExp "offset"
            =<< eBinOp
              (Add Int64 OverflowUndef)
              (eIndex local_offs [toExp i, toExp j])
              (toExp global_offset)
        let res_ts = [(offset, Prim int64)]
        let (res, ts) = unzip res_ts
        pure (map (Returns ResultMaySimplify mempty) res, ts)
      let kbody = KernelBody () stms res
      pure $ Op $ SegOp $ SegMap (SegThread SegVirt Nothing) space ts kbody

  scratch <- letExp "scratch" $ BasicOp $ Scratch int64 [n']
  res <- letExp "scatter_res" <=< genScatter scratch n' $ \gtid -> do
    c <- letExp "c" =<< eIndex cls [toExp gtid]
    offs <- letSubExp "offs" =<< eIndex cls_offs [toExp c, toExp gtid]
    ind <- letExp "ind" =<< toExp (pe64 offs - 1)
    i <- letSubExp "i" =<< toExp gtid
    pure (ind, i)
  pure (counts, res)

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
          [Array int64 (Shape [Free $ Var $ paramName np]) Unique],
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
      (qs, res) <- genPartition (paramName np) (paramName kp) (paramName csp)
      pure $ varsRes [qs, res]
  pure
    FunDef
      { funDefEntryPoint = Nothing,
        funDefAttrs = mempty,
        funDefName = partitionName,
        funDefRetType =
          [ Array int64 (Shape [Free $ Var $ paramName kp]) Unique,
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
            (funDefRetType segIotaBuiltin)
            (funDefParams segIotaBuiltin)
            args
  letBindNames [m, flags, offsets, elems] $
    Apply
      (funDefName segIotaBuiltin)
      [(n, Observe), (Var ns, Observe)]
      restype
      (Safe, mempty, mempty)
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
            (funDefRetType repIotaBuiltin)
            (funDefParams repIotaBuiltin)
            args
  letBindNames [m, flags, offsets, elems] $
    Apply
      (funDefName repIotaBuiltin)
      [(n, Observe), (Var ns, Observe)]
      restype
      (Safe, mempty, mempty)
  pure (flags, offsets, elems)

doPrefixSum :: VName -> Builder GPU VName
doPrefixSum ns = do
  ns_t <- lookupType ns
  let n = arraySize 0 ns_t
  letExp "prefix_sum" $
    Apply
      (funDefName prefixSumBuiltin)
      [(n, Observe), (Var ns, Observe)]
      [toDecl (staticShapes1 ns_t) Unique]
      (Safe, mempty, mempty)

doPartition :: VName -> VName -> Builder GPU (VName, VName)
doPartition k cs = do
  cs_t <- lookupType cs
  let n = arraySize 0 cs_t
  qs <- newVName "partition_splits"
  res <- newVName "partition_res"
  let args = [(n, Prim int64), (Var k, Prim int64), (Var cs, cs_t)]
      restype =
        fromMaybe (error "doPartition: bad application") $
          applyRetType
            (funDefRetType partitionBuiltin)
            (funDefParams partitionBuiltin)
            args
  letBindNames [qs, res] $
    Apply
      (funDefName partitionBuiltin)
      [(n, Observe), (Var k, Observe), (Var cs, Observe)]
      restype
      (Safe, mempty, mempty)
  pure (qs, res)
