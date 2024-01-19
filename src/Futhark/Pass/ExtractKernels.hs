{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- | Kernel extraction.
--
-- In the following, I will use the term "width" to denote the amount
-- of immediate parallelism in a map - that is, the outer size of the
-- array(s) being used as input.
--
-- = Basic Idea
--
-- If we have:
--
-- @
--   map
--     map(f)
--     stms_a...
--     map(g)
-- @
--
-- Then we want to distribute to:
--
-- @
--   map
--     map(f)
--   map
--     stms_a
--   map
--     map(g)
-- @
--
-- But for now only if
--
--  (0) it can be done without creating irregular arrays.
--      Specifically, the size of the arrays created by @map(f)@, by
--      @map(g)@ and whatever is created by @stms_a@ that is also used
--      in @map(g)@, must be invariant to the outermost loop.
--
--  (1) the maps are _balanced_.  That is, the functions @f@ and @g@
--      must do the same amount of work for every iteration.
--
-- The advantage is that the map-nests containing @map(f)@ and
-- @map(g)@ can now be trivially flattened at no cost, thus exposing
-- more parallelism.  Note that the @stms_a@ map constitutes array
-- expansion, which requires additional storage.
--
-- = Distributing Sequential Loops
--
-- As a starting point, sequential loops are treated like scalar
-- expressions.  That is, not distributed.  However, sometimes it can
-- be worthwhile to distribute if they contain a map:
--
-- @
--   map
--     loop
--       map
--     map
-- @
--
-- If we distribute the loop and interchange the outer map into the
-- loop, we get this:
--
-- @
--   loop
--     map
--       map
--   map
--     map
-- @
--
-- Now more parallelism may be available.
--
-- = Unbalanced Maps
--
-- Unbalanced maps will as a rule be sequentialised, but sometimes,
-- there is another way.  Assume we find this:
--
-- @
--   map
--     map(f)
--       map(g)
--     map
-- @
--
-- Presume that @map(f)@ is unbalanced.  By the simple rule above, we
-- would then fully sequentialise it, resulting in this:
--
-- @
--   map
--     loop
--   map
--     map
-- @
--
-- == Balancing by Loop Interchange
--
-- The above is not ideal, as we cannot flatten the @map-loop@ nest,
-- and we are thus limited in the amount of parallelism available.
--
-- But assume now that the width of @map(g)@ is invariant to the outer
-- loop.  Then if possible, we can interchange @map(f)@ and @map(g)@,
-- sequentialise @map(f)@ and distribute, interchanging the outer
-- parallel loop into the sequential loop:
--
-- @
--   loop(f)
--     map
--       map(g)
--   map
--     map
-- @
--
-- After flattening the two nests we can obtain more parallelism.
--
-- When distributing a map, we also need to distribute everything that
-- the map depends on - possibly as its own map.  When distributing a
-- set of scalar bindings, we will need to know which of the binding
-- results are used afterwards.  Hence, we will need to compute usage
-- information.
--
-- = Redomap
--
-- Redomap can be handled much like map.  Distributed loops are
-- distributed as maps, with the parameters corresponding to the
-- neutral elements added to their bodies.  The remaining loop will
-- remain a redomap.  Example:
--
-- @
-- redomap(op,
--         fn (v) =>
--           map(f)
--           map(g),
--         e,a)
-- @
--
-- distributes to
--
-- @
-- let b = map(fn v =>
--               let acc = e
--               map(f),
--               a)
-- redomap(op,
--         fn (v,dist) =>
--           map(g),
--         e,a,b)
-- @
--
-- Note that there may be further kernel extraction opportunities
-- inside the @map(f)@.  The downside of this approach is that the
-- intermediate array (@b@ above) must be written to main memory.  An
-- often better approach is to just turn the entire @redomap@ into a
-- single kernel.
module Futhark.Pass.ExtractKernels (extractKernels) where

import Control.Monad
import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Data.Bifunctor (first)
import Data.Maybe
import Futhark.IR.GPU
import Futhark.IR.SOACS
import Futhark.IR.SOACS.Simplify (simplifyStms)
import Futhark.MonadFreshNames
import Futhark.Pass
import Futhark.Pass.ExtractKernels.BlockedKernel
import Futhark.Pass.ExtractKernels.DistributeNests
import Futhark.Pass.ExtractKernels.Distribution
import Futhark.Pass.ExtractKernels.ISRWIM
import Futhark.Pass.ExtractKernels.Intrablock
import Futhark.Pass.ExtractKernels.StreamKernel
import Futhark.Pass.ExtractKernels.ToGPU
import Futhark.Tools
import Futhark.Transform.FirstOrderTransform qualified as FOT
import Futhark.Transform.Rename
import Futhark.Util.Log
import Prelude hiding (log)

-- | Transform a program using SOACs to a program using explicit
-- kernels, using the kernel extraction transformation.
extractKernels :: Pass SOACS GPU
extractKernels =
  Pass
    { passName = "extract kernels",
      passDescription = "Perform kernel extraction",
      passFunction = transformProg
    }

transformProg :: Prog SOACS -> PassM (Prog GPU)
transformProg prog = do
  consts' <- runDistribM $ transformStms mempty $ stmsToList $ progConsts prog
  funs' <- mapM (transformFunDef $ scopeOf consts') $ progFuns prog
  pure $
    prog
      { progConsts = consts',
        progFuns = funs'
      }

-- In order to generate more stable threshold names, we keep track of
-- the numbers used for thresholds separately from the ordinary name
-- source,
data State = State
  { stateNameSource :: VNameSource,
    stateThresholdCounter :: Int
  }

newtype DistribM a = DistribM (RWS (Scope GPU) Log State a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      HasScope GPU,
      LocalScope GPU,
      MonadState State,
      MonadLogger
    )

instance MonadFreshNames DistribM where
  getNameSource = gets stateNameSource
  putNameSource src = modify $ \s -> s {stateNameSource = src}

runDistribM ::
  (MonadLogger m, MonadFreshNames m) =>
  DistribM a ->
  m a
runDistribM (DistribM m) = do
  (x, msgs) <- modifyNameSource $ \src ->
    let (x, s, msgs) = runRWS m mempty (State src 0)
     in ((x, msgs), stateNameSource s)
  addLog msgs
  pure x

transformFunDef ::
  (MonadFreshNames m, MonadLogger m) =>
  Scope GPU ->
  FunDef SOACS ->
  m (FunDef GPU)
transformFunDef scope (FunDef entry attrs name rettype params body) = runDistribM $ do
  body' <-
    localScope (scope <> scopeOfFParams params) $
      transformBody mempty body
  pure $ FunDef entry attrs name rettype params body'

type GPUStms = Stms GPU

transformBody :: KernelPath -> Body SOACS -> DistribM (Body GPU)
transformBody path body = do
  stms <- transformStms path $ stmsToList $ bodyStms body
  pure $ mkBody stms $ bodyResult body

transformStms :: KernelPath -> [Stm SOACS] -> DistribM GPUStms
transformStms _ [] =
  pure mempty
transformStms path (stm : stms) =
  sequentialisedUnbalancedStm stm >>= \case
    Nothing -> do
      stm' <- transformStm path stm
      inScopeOf stm' $
        (stm' <>) <$> transformStms path stms
    Just stms' ->
      transformStms path $ stmsToList stms' <> stms

unbalancedLambda :: Lambda SOACS -> Bool
unbalancedLambda orig_lam =
  unbalancedBody (namesFromList $ map paramName $ lambdaParams orig_lam) $
    lambdaBody orig_lam
  where
    subExpBound (Var i) bound = i `nameIn` bound
    subExpBound (Constant _) _ = False

    unbalancedBody bound body =
      any (unbalancedStm (bound <> boundInBody body) . stmExp) $
        bodyStms body

    -- XXX - our notion of balancing is probably still too naive.
    unbalancedStm bound (Op (Stream w _ _ _)) =
      w `subExpBound` bound
    unbalancedStm bound (Op (Screma w _ _)) =
      w `subExpBound` bound
    unbalancedStm _ Op {} =
      False
    unbalancedStm _ Loop {} = False
    unbalancedStm bound (WithAcc _ lam) =
      unbalancedBody bound (lambdaBody lam)
    unbalancedStm bound (Match ses cases defbody _) =
      any (`subExpBound` bound) ses
        && ( any (unbalancedBody bound . caseBody) cases
               || unbalancedBody bound defbody
           )
    unbalancedStm _ (BasicOp _) =
      False
    unbalancedStm _ (Apply fname _ _ _) =
      not $ isBuiltInFunction fname

sequentialisedUnbalancedStm :: Stm SOACS -> DistribM (Maybe (Stms SOACS))
sequentialisedUnbalancedStm (Let pat _ (Op soac@(Screma _ _ form)))
  | Just (_, lam2) <- isRedomapSOAC form,
    unbalancedLambda lam2,
    lambdaContainsParallelism lam2 = do
      types <- asksScope scopeForSOACs
      Just . snd <$> runBuilderT (FOT.transformSOAC pat soac) types
sequentialisedUnbalancedStm _ =
  pure Nothing

cmpSizeLe ::
  String ->
  SizeClass ->
  [SubExp] ->
  DistribM ((SubExp, Name), Stms GPU)
cmpSizeLe desc size_class to_what = do
  x <- gets stateThresholdCounter
  modify $ \s -> s {stateThresholdCounter = x + 1}
  let size_key = nameFromString $ desc ++ "_" ++ show x
  runBuilder $ do
    to_what' <-
      letSubExp "comparatee"
        =<< foldBinOp (Mul Int64 OverflowUndef) (intConst Int64 1) to_what
    cmp_res <- letSubExp desc $ Op $ SizeOp $ CmpSizeLe size_key size_class to_what'
    pure (cmp_res, size_key)

kernelAlternatives ::
  (MonadFreshNames m, HasScope GPU m) =>
  Pat Type ->
  Body GPU ->
  [(SubExp, Body GPU)] ->
  m (Stms GPU)
kernelAlternatives pat default_body [] = runBuilder_ $ do
  ses <- bodyBind default_body
  forM_ (zip (patNames pat) ses) $ \(name, SubExpRes cs se) ->
    certifying cs $ letBindNames [name] $ BasicOp $ SubExp se
kernelAlternatives pat default_body ((cond, alt) : alts) = runBuilder_ $ do
  alts_pat <- fmap Pat . forM (patElems pat) $ \pe -> do
    name <- newVName $ baseString $ patElemName pe
    pure pe {patElemName = name}

  alt_stms <- kernelAlternatives alts_pat default_body alts
  let alt_body = mkBody alt_stms $ varsRes $ patNames alts_pat

  letBind pat . Match [cond] [Case [Just $ BoolValue True] alt] alt_body $
    MatchDec (staticShapes (patTypes pat)) MatchEquiv

transformLambda :: KernelPath -> Lambda SOACS -> DistribM (Lambda GPU)
transformLambda path (Lambda params ret body) =
  Lambda params ret
    <$> localScope (scopeOfLParams params) (transformBody path body)

versionScanRed ::
  KernelPath ->
  Pat Type ->
  StmAux () ->
  SubExp ->
  Lambda SOACS ->
  DistribM (Stms GPU) ->
  DistribM (Body GPU) ->
  ([(Name, Bool)] -> DistribM (Body GPU)) ->
  DistribM (Stms GPU)
versionScanRed path pat aux w map_lam paralleliseOuter outerParallelBody innerParallelBody =
  if not (lambdaContainsParallelism map_lam)
    || ("sequential_inner" `inAttrs` stmAuxAttrs aux)
    then paralleliseOuter
    else do
      ((outer_suff, outer_suff_key), suff_stms) <-
        sufficientParallelism "suff_outer_screma" [w] path Nothing

      outer_stms <- outerParallelBody
      inner_stms <- innerParallelBody ((outer_suff_key, False) : path)

      (suff_stms <>) <$> kernelAlternatives pat inner_stms [(outer_suff, outer_stms)]

transformStm :: KernelPath -> Stm SOACS -> DistribM GPUStms
transformStm _ stm
  | "sequential" `inAttrs` stmAuxAttrs (stmAux stm) =
      runBuilder_ $ FOT.transformStmRecursively stm
transformStm path (Let pat aux (Op soac))
  | "sequential_outer" `inAttrs` stmAuxAttrs aux =
      transformStms path . stmsToList . fmap (certify (stmAuxCerts aux))
        =<< runBuilder_ (FOT.transformSOAC pat soac)
transformStm path (Let pat aux (Match c cases defbody rt)) = do
  cases' <- mapM (traverse $ transformBody path) cases
  defbody' <- transformBody path defbody
  pure $ oneStm $ Let pat aux $ Match c cases' defbody' rt
transformStm path (Let pat aux (WithAcc inputs lam)) =
  oneStm . Let pat aux
    <$> (WithAcc (map transformInput inputs) <$> transformLambda path lam)
  where
    transformInput (shape, arrs, op) =
      (shape, arrs, fmap (first soacsLambdaToGPU) op)
transformStm path (Let pat aux (Loop merge form body)) =
  localScope (scopeOfLoopForm form <> scopeOfFParams params) $
    oneStm . Let pat aux . Loop merge form <$> transformBody path body
  where
    params = map fst merge
transformStm path (Let pat aux (Op (Screma w arrs form)))
  | Just lam <- isMapSOAC form =
      onMap path $ MapLoop pat aux w lam arrs
transformStm path (Let pat aux@(StmAux cs _ _) (Op (Screma w arrs form)))
  | Just scans <- isScanSOAC form,
    Scan scan_lam nes <- singleScan scans,
    Just do_iswim <- iswim pat w scan_lam $ zip nes arrs = do
      types <- asksScope scopeForSOACs
      transformStms path . stmsToList . snd =<< runBuilderT (certifying cs do_iswim) types
  | Just (scans, map_lam) <- isScanomapSOAC form = do
      let paralleliseOuter = runBuilder_ $ do
            scan_ops <- forM scans $ \(Scan scan_lam nes) -> do
              (scan_lam', nes', shape) <- determineReduceOp scan_lam nes
              let scan_lam'' = soacsLambdaToGPU scan_lam'
              pure $ SegBinOp Noncommutative scan_lam'' nes' shape
            let map_lam_sequential = soacsLambdaToGPU map_lam
            lvl <- segThreadCapped [w] "segscan" $ NoRecommendation SegNoVirt
            addStms . fmap (certify cs)
              =<< segScan lvl pat mempty w scan_ops map_lam_sequential arrs [] []

          outerParallelBody =
            renameBody
              =<< (mkBody <$> paralleliseOuter <*> pure (varsRes (patNames pat)))

          paralleliseInner path' = do
            (mapstm, scanstm) <-
              scanomapToMapAndScan pat (w, scans, map_lam, arrs)
            types <- asksScope scopeForSOACs
            transformStms path' . stmsToList <=< (`runBuilderT_` types) $
              addStms =<< simplifyStms (stmsFromList [certify cs mapstm, certify cs scanstm])

          innerParallelBody path' =
            renameBody
              =<< (mkBody <$> paralleliseInner path' <*> pure (varsRes (patNames pat)))

      versionScanRed path pat aux w map_lam paralleliseOuter outerParallelBody innerParallelBody
transformStm path (Let res_pat aux (Op (Screma w arrs form)))
  | Just [Reduce comm red_fun nes] <- isReduceSOAC form,
    let comm'
          | commutativeLambda red_fun = Commutative
          | otherwise = comm,
    Just do_irwim <- irwim res_pat w comm' red_fun $ zip nes arrs = do
      types <- asksScope scopeForSOACs
      stms <- fst <$> runBuilderT (simplifyStms =<< collectStms_ (auxing aux do_irwim)) types
      transformStms path $ stmsToList stms
transformStm path (Let pat aux@(StmAux cs _ _) (Op (Screma w arrs form)))
  | Just (reds, map_lam) <- isRedomapSOAC form = do
      let paralleliseOuter = runBuilder_ $ do
            red_ops <- forM reds $ \(Reduce comm red_lam nes) -> do
              (red_lam', nes', shape) <- determineReduceOp red_lam nes
              let comm'
                    | commutativeLambda red_lam' = Commutative
                    | otherwise = comm
                  red_lam'' = soacsLambdaToGPU red_lam'
              pure $ SegBinOp comm' red_lam'' nes' shape
            let map_lam_sequential = soacsLambdaToGPU map_lam
            lvl <- segThreadCapped [w] "segred" $ NoRecommendation SegNoVirt
            addStms . fmap (certify cs)
              =<< nonSegRed lvl pat w red_ops map_lam_sequential arrs

          outerParallelBody =
            renameBody
              =<< (mkBody <$> paralleliseOuter <*> pure (varsRes (patNames pat)))

          paralleliseInner path' = do
            (mapstm, redstm) <-
              redomapToMapAndReduce pat (w, reds, map_lam, arrs)
            types <- asksScope scopeForSOACs
            transformStms path' . stmsToList <=< (`runBuilderT_` types) $
              addStms =<< simplifyStms (stmsFromList [certify cs mapstm, certify cs redstm])

          innerParallelBody path' =
            renameBody
              =<< (mkBody <$> paralleliseInner path' <*> pure (varsRes (patNames pat)))

      versionScanRed path pat aux w map_lam paralleliseOuter outerParallelBody innerParallelBody
transformStm path (Let pat (StmAux cs _ _) (Op (Screma w arrs form))) = do
  -- This screma is too complicated for us to immediately do
  -- anything, so split it up and try again.
  scope <- asksScope scopeForSOACs
  transformStms path . map (certify cs) . stmsToList . snd
    =<< runBuilderT (dissectScrema pat w form arrs) scope
transformStm path (Let pat _ (Op (Stream w arrs nes fold_fun))) = do
  -- Remove the stream and leave the body parallel.  It will be
  -- distributed.
  types <- asksScope scopeForSOACs
  transformStms path . stmsToList . snd
    =<< runBuilderT (sequentialStreamWholeArray pat w nes fold_fun arrs) types
--
-- When we are scattering into a multidimensional array, we want to
-- fully parallelise, such that we do not have threads writing
-- potentially large rows. We do this by fissioning the scatter into a
-- map part and a scatter part, where the former is flattened as
-- usual, and the latter has a thread per primitive element to be
-- written.
--
-- TODO: this could be slightly smarter. If we are dealing with a
-- horizontally fused Scatter that targets both single- and
-- multi-dimensional arrays, we could handle the former in the map
-- stage. This would save us from having to store all the intermediate
-- results to memory. Troels suspects such cases are very rare, but
-- they may appear some day.
transformStm path (Let pat aux (Op (Scatter w arrs lam as)))
  | not $ all primType $ lambdaReturnType lam = do
      -- Produce map stage.
      map_pat <- fmap Pat $ forM (lambdaReturnType lam) $ \t ->
        PatElem <$> newVName "scatter_tmp" <*> pure (t `arrayOfRow` w)
      map_stms <- onMap path $ MapLoop map_pat aux w lam arrs

      -- Now do the scatters.
      runBuilder_ $ do
        addStms map_stms
        zipWithM_ doScatter (patElems pat) $ groupScatterResults as $ patNames map_pat
  where
    -- Generate code for a scatter where each thread writes only a scalar.
    doScatter res_pe (scatter_space, arr, is_vs) = do
      kernel_i <- newVName "write_i"
      arr_t <- lookupType arr
      val_t <- stripArray (shapeRank scatter_space) <$> lookupType arr
      val_is <- replicateM (arrayRank val_t) (newVName "val_i")
      (kret, kstms) <- collectStms $ do
        is_vs' <- forM is_vs $ \(is, v) -> do
          v' <- letSubExp (baseString v <> "_elem") $ BasicOp $ Index v $ Slice $ map (DimFix . Var) $ kernel_i : val_is
          is' <- forM is $ \i' ->
            letSubExp (baseString i' <> "_i") $ BasicOp $ Index i' $ Slice [DimFix $ Var kernel_i]
          pure (Slice $ map DimFix $ is' <> map Var val_is, v')
        pure $ WriteReturns mempty arr is_vs'
      (kernel, stms) <-
        mapKernel
          segThreadCapped
          ((kernel_i, w) : zip val_is (arrayDims val_t))
          mempty
          [arr_t]
          (KernelBody () kstms [kret])
      addStms stms
      letBind (Pat [res_pe]) $ Op $ SegOp kernel
--
transformStm _ (Let pat (StmAux cs _ _) (Op (Scatter w ivs lam as))) = runBuilder_ $ do
  let lam' = soacsLambdaToGPU lam
  write_i <- newVName "write_i"
  let krets = do
        (_a_w, a, is_vs) <- groupScatterResults as $ bodyResult $ lambdaBody lam'
        let res_cs =
              foldMap (foldMap resCerts . fst) is_vs
                <> foldMap (resCerts . snd) is_vs
            is_vs' = [(Slice $ map (DimFix . resSubExp) is, resSubExp v) | (is, v) <- is_vs]
        pure $ WriteReturns res_cs a is_vs'
      body = KernelBody () (bodyStms $ lambdaBody lam') krets
      inputs = do
        (p, p_a) <- zip (lambdaParams lam') ivs
        pure $ KernelInput (paramName p) (paramType p) p_a [Var write_i]
  (kernel, stms) <-
    mapKernel
      segThreadCapped
      [(write_i, w)]
      inputs
      (patTypes pat)
      body
  certifying cs $ do
    addStms stms
    letBind pat $ Op $ SegOp kernel
transformStm _ (Let orig_pat (StmAux cs _ _) (Op (Hist w imgs ops bucket_fun))) = do
  let bfun' = soacsLambdaToGPU bucket_fun

  -- It is important not to launch unnecessarily many threads for
  -- histograms, because it may mean we unnecessarily need to reduce
  -- subhistograms as well.
  runBuilder_ $ do
    lvl <- segThreadCapped [w] "seghist" $ NoRecommendation SegNoVirt
    addStms =<< histKernel onLambda lvl orig_pat [] [] cs w ops bfun' imgs
  where
    onLambda = pure . soacsLambdaToGPU
transformStm _ stm =
  runBuilder_ $ FOT.transformStmRecursively stm

sufficientParallelism ::
  String ->
  [SubExp] ->
  KernelPath ->
  Maybe Int64 ->
  DistribM ((SubExp, Name), Stms GPU)
sufficientParallelism desc ws path def =
  cmpSizeLe desc (SizeThreshold path def) ws

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
      | Op (Scatter w _ lam' _) <- stmExp stm =
          mapLike w lam'
      | Loop _ _ body <- stmExp stm =
          bodyInterest body * 10
      | Match _ cases defbody _ <- stmExp stm =
          foldl
            max
            (bodyInterest defbody)
            (map (bodyInterest . caseBody) cases)
      | Op (Screma w _ (ScremaForm _ _ lam')) <- stmExp stm =
          zeroIfTooSmall w + bodyInterest (lambdaBody lam')
      | Op (Stream _ _ _ lam') <- stmExp stm =
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
worthSequentialising :: Lambda SOACS -> Bool
worthSequentialising lam = bodyInterest (lambdaBody lam) > 1
  where
    bodyInterest body =
      sum $ interest <$> bodyStms body
    interest stm
      | "sequential" `inAttrs` attrs =
          0 :: Int
      | Op (Screma _ _ form@(ScremaForm _ _ lam')) <- stmExp stm,
        isJust $ isMapSOAC form =
          if sequential_inner
            then 0
            else bodyInterest (lambdaBody lam')
      | Op Scatter {} <- stmExp stm =
          0 -- Basically a map.
      | Loop _ ForLoop {} body <- stmExp stm =
          bodyInterest body * 10
      | WithAcc _ withacc_lam <- stmExp stm =
          bodyInterest (lambdaBody withacc_lam)
      | Op (Screma _ _ form@(ScremaForm _ _ lam')) <- stmExp stm =
          1
            + bodyInterest (lambdaBody lam')
            +
            -- Give this a bigger score if it's a redomap, as these
            -- are often tileable and thus benefit more from
            -- sequentialisation.
            case isRedomapSOAC form of
              Just _ -> 1
              Nothing -> 0
      | otherwise =
          0
      where
        attrs = stmAuxAttrs $ stmAux stm
        sequential_inner = "sequential_inner" `inAttrs` attrs

onTopLevelStms ::
  KernelPath ->
  Stms SOACS ->
  DistNestT GPU DistribM GPUStms
onTopLevelStms path stms =
  liftInner $ transformStms path $ stmsToList stms

onMap :: KernelPath -> MapLoop -> DistribM GPUStms
onMap path (MapLoop pat aux w lam arrs) = do
  types <- askScope
  let loopnest = MapNesting pat aux w $ zip (lambdaParams lam) arrs
      env path' =
        DistEnv
          { distNest = singleNesting (Nesting mempty loopnest),
            distScope =
              scopeOfPat pat
                <> scopeForGPU (scopeOf lam)
                <> types,
            distOnInnerMap = onInnerMap path',
            distOnTopLevelStms = onTopLevelStms path',
            distSegLevel = segThreadCapped,
            distOnSOACSStms = pure . oneStm . soacsStmToGPU,
            distOnSOACSLambda = pure . soacsLambdaToGPU
          }
      exploitInnerParallelism path' =
        runDistNestT (env path') $
          distributeMapBodyStms acc (bodyStms $ lambdaBody lam)

  let exploitOuterParallelism path' = do
        let lam' = soacsLambdaToGPU lam
        runDistNestT (env path') $
          distribute $
            addStmsToAcc (bodyStms $ lambdaBody lam') acc

  onMap' (newKernel loopnest) path exploitOuterParallelism exploitInnerParallelism pat lam
  where
    acc =
      DistAcc
        { distTargets = singleTarget (pat, bodyResult $ lambdaBody lam),
          distStms = mempty
        }

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

-- The minimum amount of inner parallelism we require (by default) in
-- intra-group versions.  Less than this is usually pointless on a GPU
-- (but we allow tuning to change it).
intraMinInnerPar :: Int64
intraMinInnerPar = 32 -- One NVIDIA warp

onMap' ::
  KernelNest ->
  KernelPath ->
  (KernelPath -> DistribM (Stms GPU)) ->
  (KernelPath -> DistribM (Stms GPU)) ->
  Pat Type ->
  Lambda SOACS ->
  DistribM (Stms GPU)
onMap' loopnest path mk_seq_stms mk_par_stms pat lam = do
  -- Some of the control flow here looks a bit convoluted because we
  -- are trying to avoid generating unneeded threshold parameters,
  -- which means we need to do all the pruning checks up front.

  types <- askScope

  let only_intra = onlyExploitIntra (stmAuxAttrs aux)
      may_intra = worthIntrablock lam && mayExploitIntra attrs

  intra <-
    if only_intra || may_intra
      then flip runReaderT types $ intrablockParallelise loopnest lam
      else pure Nothing

  case intra of
    _ | "sequential_inner" `inAttrs` attrs -> do
      seq_body <- renameBody =<< mkBody <$> mk_seq_stms path <*> pure res
      kernelAlternatives pat seq_body []
    --
    Nothing
      | not only_intra,
        Just m <- mkSeqAlts -> do
          (outer_suff, outer_suff_key, outer_suff_stms, seq_body) <- m
          par_body <-
            renameBody
              =<< mkBody
                <$> mk_par_stms ((outer_suff_key, False) : path)
                <*> pure res
          (outer_suff_stms <>) <$> kernelAlternatives pat par_body [(outer_suff, seq_body)]
      --
      | otherwise -> do
          par_body <- renameBody =<< mkBody <$> mk_par_stms path <*> pure res
          kernelAlternatives pat par_body []
    --
    Just intra'@(_, _, log, intra_prelude, intra_stms)
      | only_intra -> do
          addLog log
          group_par_body <- renameBody $ mkBody intra_stms res
          (intra_prelude <>) <$> kernelAlternatives pat group_par_body []
      --
      | otherwise -> do
          addLog log

          case mkSeqAlts of
            Nothing -> do
              (group_par_body, intra_ok, intra_suff_key, intra_suff_stms) <-
                checkSuffIntraPar path intra'

              par_body <-
                renameBody
                  =<< mkBody
                    <$> mk_par_stms ((intra_suff_key, False) : path)
                    <*> pure res

              (intra_suff_stms <>)
                <$> kernelAlternatives pat par_body [(intra_ok, group_par_body)]
            Just m -> do
              (outer_suff, outer_suff_key, outer_suff_stms, seq_body) <- m

              (group_par_body, intra_ok, intra_suff_key, intra_suff_stms) <-
                checkSuffIntraPar ((outer_suff_key, False) : path) intra'

              par_body <-
                renameBody
                  =<< mkBody
                    <$> mk_par_stms
                      ( [ (outer_suff_key, False),
                          (intra_suff_key, False)
                        ]
                          ++ path
                      )
                    <*> pure res

              ((outer_suff_stms <> intra_suff_stms) <>)
                <$> kernelAlternatives
                  pat
                  par_body
                  [(outer_suff, seq_body), (intra_ok, group_par_body)]
  where
    nest_ws = kernelNestWidths loopnest
    res = varsRes $ patNames pat
    aux = loopNestingAux $ innermostKernelNesting loopnest
    attrs = stmAuxAttrs aux

    mkSeqAlts
      | worthSequentialising lam,
        mayExploitOuter attrs = Just $ do
          ((outer_suff, outer_suff_key), outer_suff_stms) <- checkSuffOuterPar
          seq_body <-
            renameBody
              =<< mkBody
                <$> mk_seq_stms ((outer_suff_key, True) : path)
                <*> pure res
          pure (outer_suff, outer_suff_key, outer_suff_stms, seq_body)
      | otherwise =
          Nothing

    checkSuffOuterPar =
      sufficientParallelism "suff_outer_par" nest_ws path Nothing

    checkSuffIntraPar
      path'
      ((_intra_min_par, intra_avail_par), tblock_size, _, intra_prelude, intra_stms) = do
        -- We must check that all intra-group parallelism fits in a group.
        ((intra_ok, intra_suff_key), intra_suff_stms) <- do
          ((intra_suff, suff_key), check_suff_stms) <-
            sufficientParallelism
              "suff_intra_par"
              [intra_avail_par]
              path'
              (Just intraMinInnerPar)

          runBuilder $ do
            addStms intra_prelude

            max_tblock_size <-
              letSubExp "max_tblock_size" $ Op $ SizeOp $ GetSizeMax SizeThreadBlock
            fits <-
              letSubExp "fits" $
                BasicOp $
                  CmpOp (CmpSle Int64) tblock_size max_tblock_size

            addStms check_suff_stms

            intra_ok <- letSubExp "intra_suff_and_fits" $ BasicOp $ BinOp LogAnd fits intra_suff
            pure (intra_ok, suff_key)

        group_par_body <- renameBody $ mkBody intra_stms res
        pure (group_par_body, intra_ok, intra_suff_key, intra_suff_stms)

removeUnusedMapResults ::
  Pat Type ->
  [SubExpRes] ->
  Lambda rep ->
  Maybe ([Int], Pat Type, Lambda rep)
removeUnusedMapResults (Pat pes) res lam = do
  let (pes', body_res) =
        unzip $ filter (used . fst) $ zip pes $ bodyResult (lambdaBody lam)
  perm <- map (Var . patElemName) pes' `isPermutationOf` map resSubExp res
  pure (perm, Pat pes', lam {lambdaBody = (lambdaBody lam) {bodyResult = body_res}})
  where
    used pe = patElemName pe `nameIn` freeIn res

onInnerMap ::
  KernelPath ->
  MapLoop ->
  DistAcc GPU ->
  DistNestT GPU DistribM (DistAcc GPU)
onInnerMap path maploop@(MapLoop pat aux w lam arrs) acc
  | unbalancedLambda lam,
    lambdaContainsParallelism lam =
      addStmToAcc (mapLoopStm maploop) acc
  | otherwise =
      distributeSingleStm acc (mapLoopStm maploop) >>= \case
        Just (post_kernels, res, nest, acc')
          | Just (perm, pat', lam') <- removeUnusedMapResults pat res lam -> do
              addPostStms post_kernels
              multiVersion perm nest acc' pat' lam'
        _ -> distributeMap maploop acc
  where
    discardTargets acc' =
      -- FIXME: work around bogus targets.
      acc' {distTargets = singleTarget (mempty, mempty)}

    -- GHC 9.2 loops without the type annotation.
    generate ::
      [Int] ->
      KernelNest ->
      Pat Type ->
      Lambda SOACS ->
      DistEnv GPU DistribM ->
      Scope GPU ->
      DistribM (Stms GPU)
    generate perm nest pat' lam' dist_env extra_scope = localScope extra_scope $ do
      let maploop' = MapLoop pat' aux w lam' arrs

          exploitInnerParallelism path' = do
            let dist_env' =
                  dist_env
                    { distOnTopLevelStms = onTopLevelStms path',
                      distOnInnerMap = onInnerMap path'
                    }
            runDistNestT dist_env' . inNesting nest . localScope extra_scope $
              discardTargets
                <$> distributeMap maploop' acc {distStms = mempty}
      -- Normally the permutation is for the output pattern, but
      -- we can't really change that, so we change the result
      -- order instead.
      let lam_res' =
            rearrangeShape (rearrangeInverse perm) $
              bodyResult $
                lambdaBody lam'
          lam'' = lam' {lambdaBody = (lambdaBody lam') {bodyResult = lam_res'}}
          map_nesting = MapNesting pat' aux w $ zip (lambdaParams lam') arrs
          nest' = pushInnerKernelNesting (pat', lam_res') map_nesting nest

      -- XXX: we do not construct a new KernelPath when
      -- sequentialising.  This is only OK as long as further
      -- versioning does not take place down that branch (it currently
      -- does not).
      (sequentialised_kernel, nestw_stms) <- localScope extra_scope $ do
        let sequentialised_lam = soacsLambdaToGPU lam''
        constructKernel segThreadCapped nest' $ lambdaBody sequentialised_lam

      let outer_pat = loopNestingPat $ fst nest
      (nestw_stms <>)
        <$> onMap'
          nest'
          path
          (const $ pure $ oneStm sequentialised_kernel)
          exploitInnerParallelism
          outer_pat
          lam''

    multiVersion perm nest acc' pat' lam' = do
      -- The kernel can be distributed by itself, so now we can
      -- decide whether to just sequentialise, or exploit inner
      -- parallelism.
      dist_env <- ask
      let extra_scope = targetsScope $ distTargets acc'

      stms <- liftInner $ generate perm nest pat' lam' dist_env extra_scope
      postStm stms
      pure acc'
