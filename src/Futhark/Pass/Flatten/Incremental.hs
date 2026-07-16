-- | General definitions used for incremental flattening.
--
-- The idea behind incremental flattening is the observation that when
-- flattening a program
--
-- @
--   map f xs
-- @
--
-- we have two options: (i) transform @f@ to exploit any parallelism it may
-- contain, or (ii) turn @f@ into sequential code and only exploit the
-- parallelism in @map@.
--
-- In some cases we do not have a choice, e.g. if @f@ contains sufficiently
-- nonuniform operations that would result in nonuniform allocations. In other
-- cases the choice is obvious, such as when @f@ is completely scalar. However,
-- in the general case either will work, and it depends on the workload which of
-- the options is optimal: if the outer @map@ is big enough, it may be best to
-- efficiently sequentialise @f@ (which can then also permit various locality
-- optimisations, such as tiling). But if the outer @map@ does not have many
-- iterations, then we also need the parallelism in @f@ to fully saturate the
-- machine.
--
-- The idea behind incremental flattening is to generate both versions, and
-- select the appropriate one at run-time:
--
-- @
-- if predicate then sequentialise f...
--                   else parallelise f...
-- @
--
-- The predicate is based on comparing the amount of exploitable parallelism
-- with a threshold parameter. This threshold parameter is given a default value
-- based on run-time hardware characteristics, but usually has to be auto-tuned
-- in order to be optimal for a specific machine, program, and workload.
--
-- The multi-versioning approach is also used to generate more exotic versions,
-- such as one that parallelises @f@ at a deeper hardware level
-- (@Futhark.Pass.Flatten.Intrablock@).
module Futhark.Pass.Flatten.Incremental
  ( worthIntrablock,
    worthSequentialising,
    isVersionableMap,
    sufficientParallelism,
    isParallelFunInside,
    kernelAlternatives,
    intraBlockAlternative,
    propagateVersioningAttrs,

    -- * Transforming code
    factorScremaForParallelism,

    -- * Levels
    defaultSegLevel,
    inBlockSegLevel,
    allowVersioning,

    -- * Various queries
    bodyHasParallelism,
    lambdaHasParallelism,
    mayExploitOuter,
    onlyExploitIntra,
    mayExploitIntra,
  )
where

import Control.Monad
import Control.Monad.State
import Data.Foldable
import Data.Maybe (isJust)
import Data.Set qualified as S
import Futhark.IR.GPU
import Futhark.IR.SOACS
import Futhark.Pass.Flatten.Distribute
import Futhark.Pass.Flatten.General
import Futhark.Pass.Flatten.Intrablock qualified as Intrablock
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
  FlattenM [VName]
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

cmpSizeLe ::
  Name ->
  SizeClass ->
  [SubExp] ->
  FlattenM (SubExp, Name)
cmpSizeLe desc size_class to_what = do
  x <- gets stateThresholdCounter
  modify $ \s -> s {stateThresholdCounter = x + 1}
  let size_key = desc <> "_" <> nameFromString (show x)
  to_what' <-
    letSubExp "comparatee"
      =<< foldBinOp (Mul Int64 OverflowUndef) (intConst Int64 1) to_what
  cmp_res <- letSubExp desc $ Op $ SizeOp $ CmpSizeLe size_key size_class to_what'
  pure (cmp_res, size_key)

sufficientParallelism ::
  Name ->
  [SubExp] ->
  KernelPath ->
  Maybe Int64 ->
  FlattenM (SubExp, Name)
sufficientParallelism desc ws path def =
  cmpSizeLe desc (SizeThreshold path def) ws

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
isVersionableMap funHasParallelism inps _env w dist_res map_lam =
  all isRegularDistResult dist_res
    && not (isVariant inps w)
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
  FlattenM (SubExp, Body GPU)
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
      "suff_intra_par"
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

-- | A lambda is worth sequentialising if it contains enough nested parallelism
-- of an interesting kind, or if distributing it would fragment sequential
-- control flow - that is, if it contains meaningful parallelism nested inside a
-- sequential loop or branch. Distribution must then split the loop or branch
-- into separate kernel launches (and possibly host-evaluated control flow) per
-- sequential step, so a version that instead sequentialises the nested
-- parallelism is always worth offering.
--
-- TODO: maybe update this or just always consider Sequentialising
worthSequentialising :: Lambda SOACS -> Bool
worthSequentialising lam = bodyInterest (0 :: Int) (lambdaBody lam) > 1
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
          (2 *) $
            maximum $
              map (bodyInterest (depth + 1)) $
                defbody : map caseBody cases
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
          0
      where
        attrs = stmAuxAttrs $ stmAux stm
        sequential_inner = "sequential_inner" `inAttrs` attrs

bodyHasParallelism :: FunHasParallelism -> Body SOACS -> Bool
bodyHasParallelism funHasParallelism =
  any (isParallelStm funHasParallelism) . bodyStms

lambdaHasParallelism :: FunHasParallelism -> Lambda SOACS -> Bool
lambdaHasParallelism funHasParallelism =
  bodyHasParallelism funHasParallelism . lambdaBody

-- | Like 'lambdaHasParallelism', but only counts meaningful
-- parallelism: a SOAC, a call to a parallel function, or a statement
-- with an irregular result, which requires flattening to exploit.
-- Basic operations such as 'Replicate' of invariant size do not
-- provide enough parallelism on their own to make multi-versioning
-- worthwhile.  See Note [Meaningful Parallelism] in
-- Futhark.Pass.Flatten.Distribute.
lambdaHasMeaningfulParallelism :: FunHasParallelism -> Lambda SOACS -> Bool
lambdaHasMeaningfulParallelism funHasParallelism lam =
  any interesting $ bodyStms $ lambdaBody lam
  where
    free_in_lam = freeIn lam
    invariantDim (Var v) = v `nameIn` free_in_lam
    invariantDim Constant {} = True
    irregularResult =
      not . all (all invariantDim . arrayDims) . patTypes . stmPat
    interesting stm =
      stmHasMeaningfulParallelism funHasParallelism stm || irregularResult stm

-- | Produce a body suitable for full flattening from a Screma, or
-- 'Nothing' if none of its lambdas contain meaningful parallelism, in
-- which case multi-versioning is not worthwhile.  See Note
-- [Meaningful Parallelism] in Futhark.Pass.Flatten.Distribute.
factorScremaForParallelism ::
  (MonadBuilder m) =>
  FunHasParallelism ->
  Scope SOACS ->
  Certs ->
  Pat Type ->
  SubExp ->
  [VName] ->
  ScremaForm SOACS ->
  m (Maybe (Body SOACS))
factorScremaForParallelism funHasParallelism scope certs pat w arrs form
  | Just (reds, map_lam) <- isRedomapSOAC form,
    lambdaHasMeaningfulParallelism funHasParallelism map_lam = do
      map_lam' <- preprocessLambda scope map_lam
      (map_stm, red_stm) <-
        redomapToMapAndReduce
          pat
          (w, reds, map_lam', arrs)
      Just <$> mkFactoredBody (stmsFromList [map_stm, red_stm])
  | Just (post_lam, scans, map_lam) <- isMaposcanomapSOAC form,
    lambdaHasMeaningfulParallelism funHasParallelism map_lam,
    lambdaHasMeaningfulParallelism funHasParallelism post_lam = do
      map_lam' <- preprocessLambda scope map_lam
      post_lam' <- preprocessLambda scope post_lam
      (map_stm, scan_stm, post_stm) <-
        maposcanomapToMapScanAndMap
          pat
          (w, post_lam', scans, map_lam', arrs)
      Just <$> mkFactoredBody (stmsFromList [map_stm, scan_stm, post_stm])
  | Just (post_lam, scans, map_lam) <- isMaposcanomapSOAC form,
    lambdaHasMeaningfulParallelism funHasParallelism map_lam = do
      map_lam' <- preprocessLambda scope map_lam
      post_lam' <- preprocessLambda scope post_lam
      (map_stm, scanomap_stm) <-
        maposcanomapToMaposcanAndMap
          pat
          (w, post_lam', scans, map_lam', arrs)
      Just <$> mkFactoredBody (stmsFromList [map_stm, scanomap_stm])
  | Just (post_lam, scans, map_lam) <- isMaposcanomapSOAC form,
    lambdaHasMeaningfulParallelism funHasParallelism post_lam = do
      map_lam' <- preprocessLambda scope map_lam
      post_lam' <- preprocessLambda scope post_lam
      (map_stm, scan_stm, post_stm) <-
        maposcanomapToMapScanAndMap
          pat
          (w, post_lam', scans, map_lam', arrs)
      Just <$> mkFactoredBody (stmsFromList [map_stm, scan_stm, post_stm])
  | otherwise =
      pure Nothing
  where
    mkFactoredBody stms = do
      stms' <- fmap (certify certs) <$> preprocessStms scope stms
      pure $ mkBody stms' $ varsRes $ patNames pat

-- | Propagate incremental flattening attributes to the statements of
-- a map lambda body. Statements that carry their own incremental
-- flattening attributes are left alone.
propagateVersioningAttrs :: Attrs -> Lambda SOACS -> Lambda SOACS
propagateVersioningAttrs attrs lam
  | attrs' == mempty = lam
  | otherwise =
      lam {lambdaBody = (lambdaBody lam) {bodyStms = fmap onStm (bodyStms (lambdaBody lam))}}
  where
    attrs' = versioningAttrs attrs
    onStm stm
      | versioningAttrs (stmAuxAttrs (stmAux stm)) == mempty =
          stm {stmAux = (stmAux stm) {stmAuxAttrs = attrs' <> stmAuxAttrs (stmAux stm)}}
      | otherwise = stm
    versioningAttrs (Attrs s) = Attrs $ S.filter isVersioningAttr s
    isVersioningAttr (AttrComp "incremental_flattening" _) = True
    isVersioningAttr _ = False
