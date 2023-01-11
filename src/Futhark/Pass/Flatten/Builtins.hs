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
  )
where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Foldable
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Futhark.IR.GPU
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Pass.ExtractKernels.BlockedKernel (mkSegSpace)
import Futhark.Pass.ExtractKernels.ToGPU
  ( scopeForGPU,
    soacsLambdaToGPU,
    soacsStmToGPU,
  )
import Futhark.Tools

builtinName :: T.Text -> Name
builtinName = nameFromText . ("builtin#" <>)

segIotaName, repIotaName, prefixSumName :: Name
segIotaName = builtinName "segiota"
repIotaName = builtinName "repiota"
prefixSumName = builtinName "prefixsum"

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
    lvl = SegThread SegNoVirt Nothing

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
  pure $ Op $ SegOp $ SegMap (SegThread SegNoVirt Nothing) space [v_t] kbody

genTabulate :: SubExp -> (SubExp -> Builder GPU [SubExp]) -> Builder GPU (Exp GPU)
genTabulate w m = do
  gtid <- newVName "gtid"
  space <- mkSegSpace [(gtid, w)]
  ((res, ts), stms) <- collectStms $ localScope (scopeOfSegSpace space) $ do
    ses <- m $ Var gtid
    ts <- mapM subExpType ses
    pure (map (Returns ResultMaySimplify mempty) ses, ts)
  let kbody = KernelBody () stms res
  pure $ Op $ SegOp $ SegMap (SegThread SegNoVirt Nothing) space ts kbody

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
          [Array int64 (Shape [Free $ Var $ paramName np]) Nonunique],
        funDefParams = [np, nsp],
        funDefBody = body
      }

-- | Builtin functions used in flattening.  Must be prepended to a
-- program that is transformed by flattening.  The intention is to
-- avoid the code explosion that would result if we inserted
-- primitives everywhere.
flatteningBuiltins :: [FunDef GPU]
flatteningBuiltins = [segIotaBuiltin, repIotaBuiltin, prefixSumBuiltin]

-- | Perform a segmented iota. Returns flags,offsets,data.
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

-- | Returns @(flags, offsets, elems)@.
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
