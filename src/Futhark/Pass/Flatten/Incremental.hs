-- | General definitions used for incremental flattening.
module Futhark.Pass.Flatten.Incremental
  ( worthIntrablock,
    worthSequentialising,
    isVersionableMap,
    sufficientParallelism,
    isParallelFunInside,
    kernelAlternatives,
    intraBlockAlternative,

    -- * Transforming code
    factorScremaForParallelism,

    -- * Levels
    defaultSegLevel,
    inBlockSegLevel,
    allowVersioning,

    -- * Various queries
    lambdaHasParallelism,
    mayExploitOuter,
    onlyExploitIntra,
    mayExploitIntra,
  )
where

import Control.Monad
import Data.Foldable
import Data.Maybe (isJust)
import Futhark.IR.GPU
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Pass.Flatten.Distribute
import Futhark.Pass.Flatten.Intrablock qualified as Intrablock
import Futhark.Pass.Flatten.Monad
import Futhark.Pass.Flatten.PreProcess
import Futhark.Tools
import Futhark.Transform.Rename
import Prelude hiding (div, quot, rem)

defaultSegLevel :: SegLevel
defaultSegLevel = SegThread SegVirt Nothing

inBlockSegLevel :: SegLevel
inBlockSegLevel = SegThreadInBlock SegNoVirt

allowVersioning :: SegLevel -> Bool
allowVersioning SegThreadInBlock {} = False
allowVersioning _ = True

kernelAlternatives ::
  Name ->
  [Type] ->
  Body GPU ->
  [(SubExp, Body GPU)] ->
  Builder GPU [VName]
kernelAlternatives desc _ default_body [] = do
  ses <- bodyBind default_body
  forM ses $ \(SubExpRes cs se) ->
    certifying cs $
      letExp desc $
        BasicOp $
          SubExp se
kernelAlternatives desc result_ts default_body ((cond, alt) : alts) = do
  fallback_body <- do
    (fallback_vs, fallback_stms) <-
      collectStms $
        kernelAlternatives desc result_ts default_body alts
    pure $ mkBody fallback_stms $ varsRes fallback_vs

  letTupExp desc $
    Match [cond] [Case [Just $ BoolValue True] alt] fallback_body $
      MatchDec (staticShapes result_ts) MatchEquiv

sufficientParallelism ::
  Name ->
  [SubExp] ->
  KernelPath ->
  Maybe Int64 ->
  Builder GPU (SubExp, Name)
sufficientParallelism desc ws path def = do
  size_key <- nameFromText . prettyText <$> newVName desc

  amount <-
    letSubExp "comparatee"
      =<< foldBinOp (Mul Int64 OverflowUndef) (intConst Int64 1) ws

  cmp_res <-
    letSubExp desc . Op . SizeOp $
      CmpSizeLe size_key (SizeThreshold path def) amount

  pure (cmp_res, size_key)

-- Check if the in the body there is a call to a parallel function.
-- XXX: we use this function to even reject the intra version of
-- maps that call parallel function. We should do better there.
-- One other things to note is that maybe we should create a sequential
-- version of function and replace them in these cases.
isParallelFunInside :: FunHasParallelism -> Body SOACS -> Bool
isParallelFunInside funHasParallelism = inBody
  where
    inLambda = any (callParallelFunction . stmExp) . bodyStms . lambdaBody
    inBody = any (callParallelFunction . stmExp) . bodyStms
    callParallelFunction (Apply fname _ _ _) = funHasParallelism fname
    callParallelFunction (BasicOp _) = False
    callParallelFunction (Match _ cases def_case _) =
      inBody def_case
        || any (inBody . caseBody) cases
    callParallelFunction (Loop _ _ body) = inBody body
    callParallelFunction (WithAcc _ lam) = inLambda lam
    callParallelFunction (Op (Stream _ _ _ lam)) = inLambda lam
    callParallelFunction (Op (Screma _ _ (ScremaForm lam _ _ _))) = inLambda lam
    callParallelFunction (Op (Hist _ _ ops lam)) =
      inLambda lam || any (inLambda . histLambda) ops
      where
        histLambda (Futhark.IR.SOACS.HistOp _ _ _ _ op) = op
    callParallelFunction (Op JVP {}) = error "isParallelFunInside: unexpected JVP"
    callParallelFunction (Op VJP {}) = error "isParallelFunInside: unexpected VJP"
    callParallelFunction (Op WithVJP {}) = error "isParallelFunInside: unexpected WithVJP"

isVersionableMap :: FunHasParallelism -> DistInputs -> DistEnv -> SubExp -> [DistResult] -> Lambda SOACS -> Bool
isVersionableMap funHasParallelism inps env w dist_res map_lam =
  all isRegularDistResult dist_res
    && not (isVariant inps env w)
    && not (isParallelFunInside funHasParallelism (lambdaBody map_lam))

onlyExploitIntra :: Attrs -> Bool
onlyExploitIntra attrs =
  AttrComp "incremental_flattening" ["only_intra"] `inAttrs` attrs

mayExploitOuter :: Attrs -> Bool
mayExploitOuter attrs =
  not $
    AttrComp "incremental_flattening" ["no_outer"]
      `inAttrs` attrs
      || AttrComp "incremental_flattening" ["only_inner"]
        `inAttrs` attrs

mayExploitIntra :: Attrs -> Bool
mayExploitIntra attrs =
  not $
    AttrComp "incremental_flattening" ["no_intra"]
      `inAttrs` attrs
      || AttrComp "incremental_flattening" ["only_inner"]
        `inAttrs` attrs

intraBlockAlternative ::
  Intrablock.IntrablockResult ->
  Builder GPU (SubExp, Body GPU)
intraBlockAlternative intra = do
  addStms $ Intrablock.intraPreludeStms intra
  max_tblock_size <-
    letSubExp "max_tblock_size" $ Op $ SizeOp $ GetSizeMax SizeThreadBlock
  fits <-
    letSubExp "fits" $
      BasicOp $
        CmpOp
          (CmpSle Int64)
          (Intrablock.intraThreadBlockSize intra)
          max_tblock_size
  (intra_suff, _) <-
    sufficientParallelism
      "suff_intra_map"
      [Intrablock.intraAvailPar intra]
      mempty
      (Just Intrablock.intraMinInnerPar)
  intra_ok <-
    letSubExp "intra_suff_and_fits" $
      BasicOp $
        BinOp LogAnd fits intra_suff
  intra_body <-
    renameBody $
      mkBody
        (Intrablock.intraKernelStms intra)
        (varsRes $ Intrablock.intraResultNames intra)
  pure (intra_ok, intra_body)

-- | Intra-group parallelism is worthwhile if the lambda contains more
-- than one instance of non-map nested parallelism, or any nested
-- parallelism inside a loop.
worthIntrablock :: Lambda SOACS -> Bool
worthIntrablock lam = bodyInterest (lambdaBody lam) > 1
  where
    bodyInterest body =
      sum $ interest <$> bodyStms body
    interest stm
      | "sequential" `inAttrs` attrs =
          0 :: Int
      | Op (Screma w _ form) <- stmExp stm,
        Just lam' <- isMapSOAC form =
          mapLike w lam'
      | Loop _ _ body <- stmExp stm =
          bodyInterest body * 10
      | Match _ cases defbody _ <- stmExp stm =
          foldl
            max
            (bodyInterest defbody)
            (map (bodyInterest . caseBody) cases)
      | Op (Screma w _ (ScremaForm lam' _ _ _)) <- stmExp stm =
          zeroIfTooSmall w + bodyInterest (lambdaBody lam')
      | Op (Stream _ _ _ lam') <- stmExp stm =
          bodyInterest $ lambdaBody lam'
      | WithAcc _ lam' <- stmExp stm =
          bodyInterest $ lambdaBody lam'
      | otherwise =
          0
      where
        attrs = stmAuxAttrs $ stmAux stm
        sequential_inner = "sequential_inner" `inAttrs` attrs

        zeroIfTooSmall (Constant (IntValue x))
          | intToInt64 x < 32 = 0
        zeroIfTooSmall _ = 1

        mapLike w lam' =
          if sequential_inner
            then 0
            else max (zeroIfTooSmall w) (bodyInterest (lambdaBody lam'))

-- | A lambda is worth sequentialising if it contains enough nested
-- parallelism of an interesting kind.
--
-- TODO: maybe update this or just always consider Sequentialising
worthSequentialising :: FunHasParallelism -> Lambda SOACS -> Bool
worthSequentialising funHasParallelism lam = bodyInterest (0 :: Int) (lambdaBody lam) > 1
  where
    bodyInterest depth body =
      sum $ interest depth <$> bodyStms body
    interest depth stm
      | "sequential" `inAttrs` attrs =
          0 :: Int
      | Op (Screma _ _ form@(ScremaForm lam' _ _ _)) <- stmExp stm,
        isJust $ isMapSOAC form =
          if sequential_inner
            then 0
            else bodyInterest (depth + 1) (lambdaBody lam')
      | Loop _ _ body <- stmExp stm =
          bodyInterest (depth + 1) body * 10
      | Match _ cases defbody _ <- stmExp stm =
          foldl
            max
            (bodyInterest (depth + 1) defbody)
            (map (bodyInterest (depth + 1) . caseBody) cases)
      | WithAcc _ withacc_lam <- stmExp stm =
          bodyInterest (depth + 1) (lambdaBody withacc_lam)
      | Op (Screma _ _ form@(ScremaForm lam' _ _ _)) <- stmExp stm =
          1
            + bodyInterest (depth + 1) (lambdaBody lam')
            +
            -- Give this a bigger score if it's a redomap just inside
            -- the the outer lambda, as these are often tileable and
            -- thus benefit more from sequentialisation.
            case (isRedomapSOAC form, depth) of
              (Just _, 0) -> 1
              _ -> 0
      | Op (Stream _ _ _ lam') <- stmExp stm =
          bodyInterest (depth + 1) (lambdaBody lam')
      | otherwise =
          if isParallelStm funHasParallelism stm then 1 else 0
      where
        attrs = stmAuxAttrs $ stmAux stm
        sequential_inner = "sequential_inner" `inAttrs` attrs

lambdaHasParallelism :: FunHasParallelism -> Lambda SOACS -> Bool
lambdaHasParallelism funHasParallelism =
  any (isParallelStm funHasParallelism) . bodyStms . lambdaBody

factorScremaForParallelism ::
  FunHasParallelism ->
  Scope SOACS ->
  Certs ->
  Pat Type ->
  SubExp ->
  [VName] ->
  ScremaForm SOACS ->
  Builder GPU (Maybe (Body SOACS))
factorScremaForParallelism funHasParallelism scope certs pat w arrs form
  | Just (reds, map_lam) <- isRedomapSOAC form,
    lambdaHasParallelism funHasParallelism map_lam = do
      (map_stm, red_stm) <-
        redomapToMapAndReduce
          pat
          (w, reds, map_lam, arrs)
      Just <$> mkFactoredBody (stmsFromList [map_stm, red_stm])
  | Just (post_lam, scans, map_lam) <- isMaposcanomapSOAC form,
    lambdaHasParallelism funHasParallelism map_lam,
    lambdaHasParallelism funHasParallelism post_lam = do
      (map_stm, scan_stm, post_stm) <-
        maposcanomapToMapScanAndMap
          pat
          (w, post_lam, scans, map_lam, arrs)
      Just <$> mkFactoredBody (stmsFromList [map_stm, scan_stm, post_stm])
  | Just (post_lam, scans, map_lam) <- isMaposcanomapSOAC form,
    lambdaHasParallelism funHasParallelism map_lam = do
      (map_stm, scanomap_stm) <-
        maposcanomapToMaposcanAndMap
          pat
          (w, post_lam, scans, map_lam, arrs)
      Just <$> mkFactoredBody (stmsFromList [map_stm, scanomap_stm])
  | Just (post_lam, scans, map_lam) <- isMaposcanomapSOAC form,
    lambdaHasParallelism funHasParallelism post_lam = do
      (map_stm, scan_stm, post_stm) <-
        maposcanomapToMapScanAndMap
          pat
          (w, post_lam, scans, map_lam, arrs)
      Just <$> mkFactoredBody (stmsFromList [map_stm, scan_stm, post_stm])
  | otherwise =
      pure Nothing
  where
    mkFactoredBody stms = do
      stms' <- fmap (certify certs) <$> preprocessStms scope stms
      pure $ mkBody stms' $ varsRes $ patNames pat
