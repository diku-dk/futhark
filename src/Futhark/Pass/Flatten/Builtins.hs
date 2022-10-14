{-# LANGUAGE TypeFamilies #-}

module Futhark.Pass.Flatten.Builtins
  ( flatteningBuiltins,
    genSegScan,
    genSegRed,
    doSegIota,
  )
where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Foldable
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Debug.Trace
import Futhark.IR.GPU
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Pass
import Futhark.Pass.ExtractKernels.BlockedKernel (mkSegSpace, segScan)
import Futhark.Pass.ExtractKernels.ToGPU
  ( scopeForGPU,
    soacsLambdaToGPU,
    soacsStmToGPU,
  )
import Futhark.Pass.Flatten.Distribute
import Futhark.Tools

segIotaName, segRepName :: Name
segIotaName = "builtin#segiota"
segRepName = "builtin#segrep"

genScan :: String -> SubExp -> Lambda GPU -> [SubExp] -> [VName] -> Builder GPU [VName]
genScan desc w lam nes arrs = do
  pat <- fmap Pat $ forM (lambdaReturnType lam) $ \t ->
    PatElem <$> newVName desc <*> pure (arrayOf t (Shape [w]) NoUniqueness)
  let op = SegBinOp Commutative lam nes mempty
  map_lam <- mkIdentityLambda $ lambdaReturnType lam
  addStms =<< segScan lvl pat mempty w [op] map_lam arrs [] []
  pure $ patNames pat
  where
    lvl = SegThread SegNoVirt Nothing

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

genSegPrefixSum :: String -> VName -> VName -> Builder GPU VName
genSegPrefixSum desc flags ns = do
  add_lam <- binOpLambda (Add Int64 OverflowUndef) int64
  head <$> genSegScan desc add_lam [intConst Int64 0] flags [ns]

genScatter :: VName -> VName -> SubExp -> Builder GPU (Exp GPU)
genScatter dest is v = do
  n <- arraySize 0 <$> lookupType is
  m <- arraySize 0 <$> lookupType dest
  gtid <- newVName "gtid"
  space <- mkSegSpace [(gtid, n)]
  v_t <- subExpType v
  (res, stms) <- collectStms $ localScope (scopeOfSegSpace space) $ do
    i <- letSubExp "i" =<< eIndex is (eSubExp $ Var gtid)
    pure $ WriteReturns mempty (Shape [m]) dest [(Slice [DimFix i], v)]
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
  letExp "flags" =<< genScatter flags_allfalse offsets (constant True)

genSegRed :: VName -> VName -> [VName] -> Reduce SOACS -> Builder GPU [VName]
genSegRed flags offsets elems red = do
  scanned <-
    genSegScan
      "red"
      (soacsLambdaToGPU $ redLambda red)
      (redNeutral red)
      flags
      elems
  num_segments <- arraySize 0 <$> lookupType offsets
  letTupExp "segred" <=< genTabulate num_segments $ \i -> do
    next_start <-
      letSubExp "next_start" =<< eIndex offsets (toExp (pe64 i))
    this_end <-
      letSubExp "this_end" =<< toExp (pe64 next_start - 1)
    mapM (letSubExp "res" <=< (`eIndex` eSubExp this_end)) scanned

genSegIota :: VName -> Builder GPU (VName, VName, VName)
genSegIota ns = do
  n <- arraySize 0 <$> lookupType ns
  is_empty <- letSubExp "is_empty" =<< toExp (pe64 n .==. 0)
  offsets <- genPrefixSum "offsets" ns
  m <-
    letSubExp "m"
      =<< eIf
        (eSubExp is_empty)
        (eBody [eSubExp $ intConst Int64 0])
        (eBody [eLast offsets])
  flags <- genFlags m offsets
  ones <- letExp "ones" $ BasicOp $ Replicate (Shape [m]) one
  iotas <- genSegPrefixSum "iotas" flags ones
  res <- letExp "res" <=< genTabulate m $ \i -> do
    x <- letSubExp "x" =<< eIndex iotas (eSubExp i)
    letTupExp' "xm1" $ BasicOp $ BinOp (Sub Int64 OverflowUndef) x one
  pure (flags, offsets, res)
  where
    one = intConst Int64 1

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

-- | Builtin functions used in flattening.  Must be prepended to a
-- program that is transformed by flattening.  The intention is to
-- avoid the code explosion that would result if we inserted
-- primitives everywhere.
flatteningBuiltins :: [FunDef GPU]
flatteningBuiltins = [segIotaBuiltin]

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
