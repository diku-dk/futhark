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
    mapAlternatives,
    scanRedAlternatives,
    propagateVersioningAttrs,
    imposeAttrs,

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
    inLambda :: GLambda SOACS t -> Bool
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
    callParallelFunction (Op (FlatMap _ _ lam)) = inLambda lam
    callParallelFunction (Op JVP {}) = error "isParallelFunInside: unexpected JVP"
    callParallelFunction (Op VJP {}) = error "isParallelFunInside: unexpected VJP"
    callParallelFunction (Op WithVJP {}) = error "isParallelFunInside: unexpected WithVJP"

-- | Should we generate multiple versions for this map? This requires both that
-- we are at a level where versioning is possible ('allowVersioning') and that
-- the map itself produces only regular results (from an invariant width) and
-- does not call any parallel function (which force full flattening).
isVersionableMap :: FunHasParallelism -> SegLevel -> DistInputs -> DistEnv -> SubExp -> [DistResult] -> Lambda SOACS -> Bool
isVersionableMap funHasParallelism lvl inps _env w dist_res map_lam =
  allowVersioning lvl
    && all isRegularDistResult dist_res
    && not (isVariant inps w)
    && not (isParallelFunInside funHasParallelism (lambdaBody map_lam))

-- | Retrieve only those attributes that apply to flattening.
flatteningAttrs :: Attrs -> Attrs
flatteningAttrs = mconcat . mapAttrs p
  where
    p (AttrComp "incremental_flattening" [x]) = oneAttr x
    p (AttrComp "flattening" [x]) = oneAttr x
    p _ = mempty

onlyExploitIntra :: Attrs -> Bool
onlyExploitIntra attrs =
  "only_intra" `inAttrs` flatteningAttrs attrs

mayExploitOuter :: Attrs -> Bool
mayExploitOuter attrs =
  not $ "no_outer" `inAttrs` attrs' || "only_inner" `inAttrs` attrs'
  where
    attrs' = flatteningAttrs attrs

mayExploitIntra :: Attrs -> Bool
mayExploitIntra attrs =
  not $ "no_intra" `inAttrs` attrs' || "only_inner" `inAttrs` attrs'
  where
    attrs' = flatteningAttrs attrs

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

-- | Construct the multi-versioned alternatives for a map, given actions that
-- construct the fully-flattened body and the outer-parallel-only body, and an
-- optional intrablock result. This is the shared versioning policy used both
-- for top-level maps and for maps nested inside a map-nest; the only
-- differences between the two are which bodies are supplied and how their
-- results are consumed, both of which are handled by the caller. The @ws@ are
-- the widths whose product bounds the outer parallelism (used for the threshold
-- comparison). Returns the names bound to the final results.
mapAlternatives ::
  -- | Description for the result bindings.
  Name ->
  [Type] ->
  Attrs ->
  -- | Does the map body call a parallel function? If so we must fully flatten.
  Bool ->
  -- | Is the body worth sequentialising (offering an outer-only version)?
  Bool ->
  [SubExp] ->
  -- | Construct the fully flattened body.
  FlattenM (Body GPU) ->
  -- | Construct the outer-parallelism-only body.
  FlattenM (Body GPU) ->
  Maybe Intrablock.IntrablockResult ->
  FlattenM [VName]
mapAlternatives desc result_ts attrs parallel_fun_inside worth_seq ws mkFullBody mkOuterBody intra' =
  case intra' of
    _
      | parallel_fun_inside ->
          full []
      | "sequential_inner" `inAttrs` attrs -> do
          outer_body <- mkOuterBody
          kernelAlternatives desc result_ts outer_body []
    Nothing
      | not only_intra,
        worth_seq,
        mayExploitOuter attrs ->
          full . pure =<< outerAlternative
      | otherwise ->
          full []
    Just intra_res
      | only_intra -> do
          (_, intra_body) <- intraBlockAlternative intra_res
          kernelAlternatives desc result_ts intra_body []
      | worth_seq,
        mayExploitOuter attrs -> do
          outer_alt <- outerAlternative
          intra_alt <- intraBlockAlternative intra_res
          full [outer_alt, intra_alt]
      | otherwise ->
          full . pure =<< intraBlockAlternative intra_res
  where
    only_intra = onlyExploitIntra attrs

    full alts = do
      full_body <- mkFullBody
      kernelAlternatives desc result_ts full_body alts

    outerAlternative = do
      outer_body <- mkOuterBody
      (outer_suff, _) <- sufficientParallelism suffOuterPar ws mempty Nothing
      pure (outer_suff, outer_body)

-- | Construct the multi-versioned alternatives for a scan or reduce, given
-- actions that construct the fully-flattened body and the outer-parallel-only
-- body. Unlike 'mapAlternatives' there is no intrablock version, and the
-- outer-only version is always offered (subject to attributes).
scanRedAlternatives ::
  Name ->
  [Type] ->
  Attrs ->
  -- | Does the operator body call a parallel function? If so we must fully flatten.
  Bool ->
  -- | Does the seg level permit versioning at all (false in-block)?
  Bool ->
  [SubExp] ->
  -- | Construct the fully flattened body.
  FlattenM (Body GPU) ->
  -- | Construct the outer-parallelism-only body.
  FlattenM (Body GPU) ->
  FlattenM [VName]
scanRedAlternatives desc result_ts attrs parallel_fun_inside allow_versioning ws mkFullBody mkOuterBody
  | parallel_fun_inside =
      fullAlternative
  | "sequential_inner" `inAttrs` attrs =
      outerAlternative
  | mayExploitOuter attrs && allow_versioning =
      fullWithOuterAlternative
  | otherwise =
      fullAlternative
  where
    fullAlternative = do
      full_body <- mkFullBody
      kernelAlternatives desc result_ts full_body []

    outerAlternative = do
      outer_body <- mkOuterBody
      kernelAlternatives desc result_ts outer_body []

    fullWithOuterAlternative = do
      outer_body <- mkOuterBody
      full_body <- mkFullBody
      (outer_suff, _) <- sufficientParallelism suffOuterPar ws mempty Nothing
      kernelAlternatives desc result_ts full_body [(outer_suff, outer_body)]

-- | The name of the threshold parameter that is used to select outer-only
-- parallelism.
suffOuterPar :: Name
suffOuterPar = "suff_outer_par"

-- | Intra-group parallelism is worthwhile if the lambda contains more
-- than one instance of non-map nested parallelism, or any nested
-- parallelism inside a loop.
worthIntrablock :: Lambda SOACS -> Bool
worthIntrablock lam =
  bodyInterest (lambdaBody lam) > 1
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
worthSequentialising :: Lambda SOACS -> Bool
worthSequentialising lam =
  bodyInterest (0 :: Int) (lambdaBody lam) > 1
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
  StmAux () ->
  Pat Type ->
  SubExp ->
  [VName] ->
  ScremaForm SOACS ->
  m (Maybe (Body SOACS))
factorScremaForParallelism funHasParallelism scope aux pat w arrs form
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
      stms' <-
        fmap (propagateAttrs (stmAuxAttrs aux) . certify (stmAuxCerts aux))
          <$> preprocessStms scope stms
      pure $ mkBody stms' $ varsRes $ patNames pat

-- | Add the flattening attributes of the enclosing context to a statement. A
-- statement that carries flattening attributes of its own is left alone, as
-- those are more specific.
propagateAttrs :: Attrs -> Stm SOACS -> Stm SOACS
propagateAttrs attrs stm
  | attrs' == mempty = stm
  | flatteningAttrs (stmAuxAttrs (stmAux stm)) == mempty =
      stm {stmAux = (stmAux stm) {stmAuxAttrs = attrs' <> stmAuxAttrs (stmAux stm)}}
  | otherwise = stm
  where
    -- 'flatteningAttrs' strips the enclosing 'flattening', which has to be put
    -- back for the attributes to be recognised on the statement.
    attrs' =
      mconcat . mapAttrs (oneAttr . AttrComp "flattening" . pure) $
        flatteningAttrs attrs

-- | Impose outside flattening attributes on a statement. Only SOACs are
-- affected, and only those that carry no flattening attributes of their own, as
-- those are more specific.
imposeAttrs :: Attrs -> Stm SOACS -> Stm SOACS
imposeAttrs attrs stm
  | Op {} <- stmExp stm = propagateAttrs attrs stm
  | otherwise = stm

-- | Propagate incremental flattening attributes to the statements of
-- a map lambda body. Statements that carry their own incremental
-- flattening attributes are left alone.
propagateVersioningAttrs :: Attrs -> Lambda SOACS -> Lambda SOACS
propagateVersioningAttrs attrs lam
  | flatteningAttrs attrs == mempty = lam
  | otherwise =
      lam
        { lambdaBody =
            (lambdaBody lam)
              { bodyStms = fmap (propagateAttrs attrs) (bodyStms (lambdaBody lam))
              }
        }
