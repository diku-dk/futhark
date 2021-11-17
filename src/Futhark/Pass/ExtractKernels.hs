{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Data.Bifunctor (first)
import Data.Maybe
import qualified Futhark.IR.GPU as Out
import Futhark.IR.GPU.Op
import Futhark.IR.SOACS
import Futhark.IR.SOACS.Simplify (simplifyStms)
import Futhark.MonadFreshNames
import Futhark.Pass
import Futhark.Pass.ExtractKernels.BlockedKernel
import Futhark.Pass.ExtractKernels.DistributeNests
import Futhark.Pass.ExtractKernels.Distribution
import Futhark.Pass.ExtractKernels.ISRWIM
import Futhark.Pass.ExtractKernels.Intragroup
import Futhark.Pass.ExtractKernels.StreamKernel
import Futhark.Pass.ExtractKernels.ToGPU
import Futhark.Tools
import qualified Futhark.Transform.FirstOrderTransform as FOT
import Futhark.Transform.Rename
import Futhark.Util.Log
import Prelude hiding (log)

-- | Transform a program using SOACs to a program using explicit
-- kernels, using the kernel extraction transformation.
extractKernels :: Pass SOACS Out.GPU
extractKernels =
  Pass
    { passName = "extract kernels",
      passDescription = "Perform kernel extraction",
      passFunction = transformProg
    }

transformProg :: Prog SOACS -> PassM (Prog Out.GPU)
transformProg (Prog consts funs) = do
  consts' <- runDistribM $ transformStms mempty $ stmsToList consts
  funs' <- mapM (transformFunDef $ scopeOf consts') funs
  return $ Prog consts' funs'

-- In order to generate more stable threshold names, we keep track of
-- the numbers used for thresholds separately from the ordinary name
-- source,
data State = State
  { stateNameSource :: VNameSource,
    stateThresholdCounter :: Int
  }

newtype DistribM a = DistribM (RWS (Scope Out.GPU) Log State a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      HasScope Out.GPU,
      LocalScope Out.GPU,
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
  return x

transformFunDef ::
  (MonadFreshNames m, MonadLogger m) =>
  Scope Out.GPU ->
  FunDef SOACS ->
  m (Out.FunDef Out.GPU)
transformFunDef scope (FunDef entry attrs name rettype params body) = runDistribM $ do
  body' <-
    localScope (scope <> scopeOfFParams params) $
      transformBody mempty body
  return $ FunDef entry attrs name rettype params body'

type GPUStms = Stms Out.GPU

transformBody :: KernelPath -> Body -> DistribM (Out.Body Out.GPU)
transformBody path body = do
  stms <- transformStms path $ stmsToList $ bodyStms body
  return $ mkBody stms $ bodyResult body

transformStms :: KernelPath -> [Stm] -> DistribM GPUStms
transformStms _ [] =
  return mempty
transformStms path (stm : stms) =
  sequentialisedUnbalancedStm stm >>= \case
    Nothing -> do
      stm' <- transformStm path stm
      inScopeOf stm' $
        (stm' <>) <$> transformStms path stms
    Just stms' ->
      transformStms path $ stmsToList stms' <> stms

unbalancedLambda :: Lambda -> Bool
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
    unbalancedStm bound (Op (Stream w _ _ _ _)) =
      w `subExpBound` bound
    unbalancedStm bound (Op (Screma w _ _)) =
      w `subExpBound` bound
    unbalancedStm _ Op {} =
      False
    unbalancedStm _ DoLoop {} = False
    unbalancedStm bound (WithAcc _ lam) =
      unbalancedBody bound (lambdaBody lam)
    unbalancedStm bound (If cond tbranch fbranch _) =
      cond `subExpBound` bound
        && (unbalancedBody bound tbranch || unbalancedBody bound fbranch)
    unbalancedStm _ (BasicOp _) =
      False
    unbalancedStm _ (Apply fname _ _ _) =
      not $ isBuiltInFunction fname

sequentialisedUnbalancedStm :: Stm -> DistribM (Maybe (Stms SOACS))
sequentialisedUnbalancedStm (Let pat _ (Op soac@(Screma _ _ form)))
  | Just (_, lam2) <- isRedomapSOAC form,
    unbalancedLambda lam2,
    lambdaContainsParallelism lam2 = do
    types <- asksScope scopeForSOACs
    Just . snd <$> runBuilderT (FOT.transformSOAC pat soac) types
sequentialisedUnbalancedStm _ =
  return Nothing

cmpSizeLe ::
  String ->
  Out.SizeClass ->
  [SubExp] ->
  DistribM ((SubExp, Name), Out.Stms Out.GPU)
cmpSizeLe desc size_class to_what = do
  x <- gets stateThresholdCounter
  modify $ \s -> s {stateThresholdCounter = x + 1}
  let size_key = nameFromString $ desc ++ "_" ++ show x
  runBuilder $ do
    to_what' <-
      letSubExp "comparatee"
        =<< foldBinOp (Mul Int64 OverflowUndef) (intConst Int64 1) to_what
    cmp_res <- letSubExp desc $ Op $ SizeOp $ CmpSizeLe size_key size_class to_what'
    return (cmp_res, size_key)

kernelAlternatives ::
  (MonadFreshNames m, HasScope Out.GPU m) =>
  Out.Pat Out.GPU ->
  Out.Body Out.GPU ->
  [(SubExp, Out.Body Out.GPU)] ->
  m (Out.Stms Out.GPU)
kernelAlternatives pat default_body [] = runBuilder_ $ do
  ses <- bodyBind default_body
  forM_ (zip (patNames pat) ses) $ \(name, SubExpRes cs se) ->
    certifying cs $ letBindNames [name] $ BasicOp $ SubExp se
kernelAlternatives pat default_body ((cond, alt) : alts) = runBuilder_ $ do
  alts_pat <- fmap Pat . forM (patElems pat) $ \pe -> do
    name <- newVName $ baseString $ patElemName pe
    return pe {patElemName = name}

  alt_stms <- kernelAlternatives alts_pat default_body alts
  let alt_body = mkBody alt_stms $ varsRes $ patNames alts_pat

  letBind pat $
    If cond alt alt_body $
      IfDec (staticShapes (patTypes pat)) IfEquiv

transformLambda :: KernelPath -> Lambda -> DistribM (Out.Lambda Out.GPU)
transformLambda path (Lambda params body ret) =
  Lambda params
    <$> localScope (scopeOfLParams params) (transformBody path body)
    <*> pure ret

transformStm :: KernelPath -> Stm -> DistribM GPUStms
transformStm _ stm
  | "sequential" `inAttrs` stmAuxAttrs (stmAux stm) =
    runBuilder_ $ FOT.transformStmRecursively stm
transformStm path (Let pat aux (Op soac))
  | "sequential_outer" `inAttrs` stmAuxAttrs aux =
    transformStms path . stmsToList . fmap (certify (stmAuxCerts aux))
      =<< runBuilder_ (FOT.transformSOAC pat soac)
transformStm path (Let pat aux (If c tb fb rt)) = do
  tb' <- transformBody path tb
  fb' <- transformBody path fb
  return $ oneStm $ Let pat aux $ If c tb' fb' rt
transformStm path (Let pat aux (WithAcc inputs lam)) =
  oneStm . Let pat aux
    <$> (WithAcc (map transformInput inputs) <$> transformLambda path lam)
  where
    transformInput (shape, arrs, op) =
      (shape, arrs, fmap (first soacsLambdaToGPU) op)
transformStm path (Let pat aux (DoLoop merge form body)) =
  localScope (castScope (scopeOf form) <> scopeOfFParams params) $
    oneStm . Let pat aux . DoLoop merge form' <$> transformBody path body
  where
    params = map fst merge
    form' = case form of
      WhileLoop cond ->
        WhileLoop cond
      ForLoop i it bound ps ->
        ForLoop i it bound ps
transformStm path (Let pat aux (Op (Screma w arrs form)))
  | Just lam <- isMapSOAC form =
    onMap path $ MapLoop pat aux w lam arrs
transformStm path (Let res_pat (StmAux cs _ _) (Op (Screma w arrs form)))
  | Just scans <- isScanSOAC form,
    Scan scan_lam nes <- singleScan scans,
    Just do_iswim <- iswim res_pat w scan_lam $ zip nes arrs = do
    types <- asksScope scopeForSOACs
    transformStms path . stmsToList . snd =<< runBuilderT (certifying cs do_iswim) types
  | Just (scans, map_lam) <- isScanomapSOAC form = runBuilder_ $ do
    scan_ops <- forM scans $ \(Scan scan_lam nes) -> do
      (scan_lam', nes', shape) <- determineReduceOp scan_lam nes
      let scan_lam'' = soacsLambdaToGPU scan_lam'
      return $ SegBinOp Noncommutative scan_lam'' nes' shape
    let map_lam_sequential = soacsLambdaToGPU map_lam
    lvl <- segThreadCapped [w] "segscan" $ NoRecommendation SegNoVirt
    addStms . fmap (certify cs)
      =<< segScan lvl res_pat mempty w scan_ops map_lam_sequential arrs [] []
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
            return $ SegBinOp comm' red_lam'' nes' shape
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

    if not (lambdaContainsParallelism map_lam)
      || "sequential_inner" `inAttrs` stmAuxAttrs aux
      then paralleliseOuter
      else do
        ((outer_suff, outer_suff_key), suff_stms) <-
          sufficientParallelism "suff_outer_redomap" [w] path Nothing

        outer_stms <- outerParallelBody
        inner_stms <- innerParallelBody ((outer_suff_key, False) : path)

        (suff_stms <>) <$> kernelAlternatives pat inner_stms [(outer_suff, outer_stms)]

-- Streams can be handled in two different ways - either we
-- sequentialise the body or we keep it parallel and distribute.
transformStm path (Let pat aux@(StmAux cs _ _) (Op (Stream w arrs Parallel {} [] map_fun)))
  | not ("sequential_inner" `inAttrs` stmAuxAttrs aux) = do
    -- No reduction part.  Remove the stream and leave the body
    -- parallel.  It will be distributed.
    types <- asksScope scopeForSOACs
    transformStms path . stmsToList . snd
      =<< runBuilderT (certifying cs $ sequentialStreamWholeArray pat w [] map_fun arrs) types
transformStm path (Let pat aux@(StmAux cs _ _) (Op (Stream w arrs (Parallel o comm red_fun) nes fold_fun)))
  | "sequential_inner" `inAttrs` stmAuxAttrs aux =
    paralleliseOuter path
  | otherwise = do
    ((outer_suff, outer_suff_key), suff_stms) <-
      sufficientParallelism "suff_outer_stream" [w] path Nothing

    outer_stms <- outerParallelBody ((outer_suff_key, True) : path)
    inner_stms <- innerParallelBody ((outer_suff_key, False) : path)

    (suff_stms <>)
      <$> kernelAlternatives pat inner_stms [(outer_suff, outer_stms)]
  where
    paralleliseOuter path'
      | not $ all primType $ lambdaReturnType red_fun = do
        -- Split into a chunked map and a reduction, with the latter
        -- further transformed.
        let fold_fun' = soacsLambdaToGPU fold_fun

        let (red_pat_elems, concat_pat_elems) =
              splitAt (length nes) $ patElems pat
            red_pat = Pat red_pat_elems

        ((num_threads, red_results), stms) <-
          streamMap
            segThreadCapped
            (map (baseString . patElemName) red_pat_elems)
            concat_pat_elems
            w
            Noncommutative
            fold_fun'
            nes
            arrs

        reduce_soac <- reduceSOAC [Reduce comm' red_fun nes]

        (stms <>)
          <$> inScopeOf
            stms
            ( transformStm path' $
                Let red_pat aux {stmAuxAttrs = mempty} $
                  Op (Screma num_threads red_results reduce_soac)
            )
      | otherwise = do
        let red_fun_sequential = soacsLambdaToGPU red_fun
            fold_fun_sequential = soacsLambdaToGPU fold_fun
        fmap (certify cs)
          <$> streamRed
            segThreadCapped
            pat
            w
            comm'
            red_fun_sequential
            fold_fun_sequential
            nes
            arrs

    outerParallelBody path' =
      renameBody
        =<< (mkBody <$> paralleliseOuter path' <*> pure (varsRes (patNames pat)))

    paralleliseInner path' = do
      types <- asksScope scopeForSOACs
      transformStms path' . fmap (certify cs) . stmsToList . snd
        =<< runBuilderT (sequentialStreamWholeArray pat w nes fold_fun arrs) types

    innerParallelBody path' =
      renameBody
        =<< (mkBody <$> paralleliseInner path' <*> pure (varsRes (patNames pat)))

    comm'
      | commutativeLambda red_fun, o /= InOrder = Commutative
      | otherwise = comm
transformStm path (Let pat (StmAux cs _ _) (Op (Screma w arrs form))) = do
  -- This screma is too complicated for us to immediately do
  -- anything, so split it up and try again.
  scope <- asksScope scopeForSOACs
  transformStms path . map (certify cs) . stmsToList . snd
    =<< runBuilderT (dissectScrema pat w form arrs) scope
transformStm path (Let pat _ (Op (Stream w arrs Sequential nes fold_fun))) = do
  -- Remove the stream and leave the body parallel.  It will be
  -- distributed.
  types <- asksScope scopeForSOACs
  transformStms path . stmsToList . snd
    =<< runBuilderT (sequentialStreamWholeArray pat w nes fold_fun arrs) types
transformStm _ (Let pat (StmAux cs _ _) (Op (Scatter w ivs lam as))) = runBuilder_ $ do
  let lam' = soacsLambdaToGPU lam
  write_i <- newVName "write_i"
  let (as_ws, _, _) = unzip3 as
      kstms = bodyStms $ lambdaBody lam'
      krets = do
        (a_w, a, is_vs) <- groupScatterResults as $ bodyResult $ lambdaBody lam'
        let res_cs =
              foldMap (foldMap resCerts . fst) is_vs
                <> foldMap (resCerts . snd) is_vs
            is_vs' = [(Slice $ map (DimFix . resSubExp) is, resSubExp v) | (is, v) <- is_vs]
        return $ WriteReturns res_cs a_w a is_vs'
      body = KernelBody () kstms krets
      inputs = do
        (p, p_a) <- zip (lambdaParams lam') ivs
        return $ KernelInput (paramName p) (paramType p) p_a [Var write_i]
  (kernel, stms) <-
    mapKernel
      segThreadCapped
      [(write_i, w)]
      inputs
      (zipWith (stripArray . length) as_ws $ patTypes pat)
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
  DistribM ((SubExp, Name), Out.Stms Out.GPU)
sufficientParallelism desc ws path def =
  cmpSizeLe desc (Out.SizeThreshold path def) ws

-- | Intra-group parallelism is worthwhile if the lambda contains more
-- than one instance of non-map nested parallelism, or any nested
-- parallelism inside a loop.
worthIntraGroup :: Lambda -> Bool
worthIntraGroup lam = bodyInterest (lambdaBody lam) > 1
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
      | DoLoop _ _ body <- stmExp stm =
        bodyInterest body * 10
      | If _ tbody fbody _ <- stmExp stm =
        max (bodyInterest tbody) (bodyInterest fbody)
      | Op (Screma w _ (ScremaForm _ _ lam')) <- stmExp stm =
        zeroIfTooSmall w + bodyInterest (lambdaBody lam')
      | Op (Stream _ _ Sequential _ lam') <- stmExp stm =
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
worthSequentialising :: Lambda -> Bool
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
      | DoLoop _ ForLoop {} body <- stmExp stm =
        bodyInterest body * 10
      | WithAcc _ withacc_lam <- stmExp stm =
        bodyInterest (lambdaBody withacc_lam)
      | Op (Screma _ _ form@(ScremaForm _ _ lam')) <- stmExp stm =
        1 + bodyInterest (lambdaBody lam')
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
  DistNestT Out.GPU DistribM GPUStms
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
    AttrComp "incremental_flattening" ["no_outer"] `inAttrs` attrs
      || AttrComp "incremental_flattening" ["only_inner"] `inAttrs` attrs

mayExploitIntra :: Attrs -> Bool
mayExploitIntra attrs =
  not $
    AttrComp "incremental_flattening" ["no_intra"] `inAttrs` attrs
      || AttrComp "incremental_flattening" ["only_inner"] `inAttrs` attrs

-- The minimum amount of inner parallelism we require (by default) in
-- intra-group versions.  Less than this is usually pointless on a GPU
-- (but we allow tuning to change it).
intraMinInnerPar :: Int64
intraMinInnerPar = 32 -- One NVIDIA warp

onMap' ::
  KernelNest ->
  KernelPath ->
  (KernelPath -> DistribM (Out.Stms Out.GPU)) ->
  (KernelPath -> DistribM (Out.Stms Out.GPU)) ->
  Pat ->
  Lambda ->
  DistribM (Out.Stms Out.GPU)
onMap' loopnest path mk_seq_stms mk_par_stms pat lam = do
  -- Some of the control flow here looks a bit convoluted because we
  -- are trying to avoid generating unneeded threshold parameters,
  -- which means we need to do all the pruning checks up front.

  types <- askScope

  intra <-
    if onlyExploitIntra (stmAuxAttrs aux)
      || (worthIntraGroup lam && mayExploitIntra attrs)
      then flip runReaderT types $ intraGroupParallelise loopnest lam
      else return Nothing

  case intra of
    _ | "sequential_inner" `inAttrs` attrs -> do
      seq_body <- renameBody =<< mkBody <$> mk_seq_stms path <*> pure res
      kernelAlternatives pat seq_body []
    --
    Nothing
      | Just m <- mkSeqAlts -> do
        (outer_suff, outer_suff_key, outer_suff_stms, seq_body) <- m
        par_body <-
          renameBody =<< mkBody
            <$> mk_par_stms ((outer_suff_key, False) : path) <*> pure res
        (outer_suff_stms <>) <$> kernelAlternatives pat par_body [(outer_suff, seq_body)]
      --
      | otherwise -> do
        par_body <- renameBody =<< mkBody <$> mk_par_stms path <*> pure res
        kernelAlternatives pat par_body []
    --
    Just intra'@(_, _, log, intra_prelude, intra_stms)
      | onlyExploitIntra attrs -> do
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
              renameBody =<< mkBody
                <$> mk_par_stms ((intra_suff_key, False) : path) <*> pure res

            (intra_suff_stms <>)
              <$> kernelAlternatives pat par_body [(intra_ok, group_par_body)]
          Just m -> do
            (outer_suff, outer_suff_key, outer_suff_stms, seq_body) <- m

            (group_par_body, intra_ok, intra_suff_key, intra_suff_stms) <-
              checkSuffIntraPar ((outer_suff_key, False) : path) intra'

            par_body <-
              renameBody =<< mkBody
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
          renameBody =<< mkBody
            <$> mk_seq_stms ((outer_suff_key, True) : path) <*> pure res
        pure (outer_suff, outer_suff_key, outer_suff_stms, seq_body)
      | otherwise =
        Nothing

    checkSuffOuterPar =
      sufficientParallelism "suff_outer_par" nest_ws path Nothing

    checkSuffIntraPar
      path'
      ((_intra_min_par, intra_avail_par), group_size, _, intra_prelude, intra_stms) = do
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

            max_group_size <-
              letSubExp "max_group_size" $ Op $ SizeOp $ Out.GetSizeMax Out.SizeGroup
            fits <-
              letSubExp "fits" $
                BasicOp $
                  CmpOp (CmpSle Int64) group_size max_group_size

            addStms check_suff_stms

            intra_ok <- letSubExp "intra_suff_and_fits" $ BasicOp $ BinOp LogAnd fits intra_suff
            return (intra_ok, suff_key)

        group_par_body <- renameBody $ mkBody intra_stms res
        pure (group_par_body, intra_ok, intra_suff_key, intra_suff_stms)

onInnerMap ::
  KernelPath ->
  MapLoop ->
  DistAcc Out.GPU ->
  DistNestT Out.GPU DistribM (DistAcc Out.GPU)
onInnerMap path maploop@(MapLoop pat aux w lam arrs) acc
  | unbalancedLambda lam,
    lambdaContainsParallelism lam =
    addStmToAcc (mapLoopStm maploop) acc
  | otherwise =
    distributeSingleStm acc (mapLoopStm maploop) >>= \case
      Just (post_kernels, res, nest, acc')
        | Just (perm, _pat_unused) <- permutationAndMissing pat res -> do
          addPostStms post_kernels
          multiVersion perm nest acc'
      _ -> distributeMap maploop acc
  where
    discardTargets acc' =
      -- FIXME: work around bogus targets.
      acc' {distTargets = singleTarget (mempty, mempty)}

    multiVersion perm nest acc' = do
      -- The kernel can be distributed by itself, so now we can
      -- decide whether to just sequentialise, or exploit inner
      -- parallelism.
      dist_env <- ask
      let extra_scope = targetsScope $ distTargets acc'

      stms <- liftInner $
        localScope extra_scope $ do
          let maploop' = MapLoop pat aux w lam arrs

              exploitInnerParallelism path' = do
                let dist_env' =
                      dist_env
                        { distOnTopLevelStms = onTopLevelStms path',
                          distOnInnerMap = onInnerMap path'
                        }
                runDistNestT dist_env' $
                  inNesting nest $
                    localScope extra_scope $
                      discardTargets <$> distributeMap maploop' acc {distStms = mempty}

          -- Normally the permutation is for the output pattern, but
          -- we can't really change that, so we change the result
          -- order instead.
          let lam_res' =
                rearrangeShape (rearrangeInverse perm) $
                  bodyResult $ lambdaBody lam
              lam' = lam {lambdaBody = (lambdaBody lam) {bodyResult = lam_res'}}
              map_nesting = MapNesting pat aux w $ zip (lambdaParams lam) arrs
              nest' = pushInnerKernelNesting (pat, lam_res') map_nesting nest

          -- XXX: we do not construct a new KernelPath when
          -- sequentialising.  This is only OK as long as further
          -- versioning does not take place down that branch (it currently
          -- does not).
          (sequentialised_kernel, nestw_stms) <- localScope extra_scope $ do
            let sequentialised_lam = soacsLambdaToGPU lam'
            constructKernel segThreadCapped nest' $ lambdaBody sequentialised_lam

          let outer_pat = loopNestingPat $ fst nest
          (nestw_stms <>)
            <$> onMap'
              nest'
              path
              (const $ return $ oneStm sequentialised_kernel)
              exploitInnerParallelism
              outer_pat
              lam'

      postStm stms
      return acc'
