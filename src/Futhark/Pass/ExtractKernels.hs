{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
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
--     bnds_a...
--     map(g)
-- @
--
-- Then we want to distribute to:
--
-- @
--   map
--     map(f)
--   map
--     bnds_a
--   map
--     map(g)
-- @
--
-- But for now only if
--
--  (0) it can be done without creating irregular arrays.
--      Specifically, the size of the arrays created by @map(f)@, by
--      @map(g)@ and whatever is created by @bnds_a@ that is also used
--      in @map(g)@, must be invariant to the outermost loop.
--
--  (1) the maps are _balanced_.  That is, the functions @f@ and @g@
--      must do the same amount of work for every iteration.
--
-- The advantage is that the map-nests containing @map(f)@ and
-- @map(g)@ can now be trivially flattened at no cost, thus exposing
-- more parallelism.  Note that the @bnds_a@ map constitutes array
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
--
module Futhark.Pass.ExtractKernels (extractKernels) where

import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Data.Maybe

import Prelude hiding (log)

import Futhark.Representation.SOACS
import Futhark.Representation.SOACS.Simplify (simplifyStms)
import qualified Futhark.Representation.Kernels as Out
import Futhark.Representation.Kernels.Kernel
import Futhark.MonadFreshNames
import Futhark.Tools
import qualified Futhark.Transform.FirstOrderTransform as FOT
import Futhark.Transform.Rename
import Futhark.Pass
import Futhark.Pass.ExtractKernels.Distribution
import Futhark.Pass.ExtractKernels.DistributeNests
import Futhark.Pass.ExtractKernels.ISRWIM
import Futhark.Pass.ExtractKernels.BlockedKernel
import Futhark.Pass.ExtractKernels.Intragroup
import Futhark.Util
import Futhark.Util.Log

-- | Transform a program using SOACs to a program using explicit
-- kernels, using the kernel extraction transformation.
extractKernels :: Pass SOACS Out.Kernels
extractKernels =
  Pass { passName = "extract kernels"
       , passDescription = "Perform kernel extraction"
       , passFunction = fmap Prog . mapM transformFunDef . progFunctions
       }

-- In order to generate more stable threshold names, we keep track of
-- the numbers used for thresholds separately from the ordinary name
-- source,
data State = State { stateNameSource :: VNameSource
                   , stateThresholdCounter :: Int
                   }

newtype DistribM a = DistribM (RWS (Scope Out.Kernels) Log State a)
                   deriving (Functor, Applicative, Monad,
                             HasScope Out.Kernels, LocalScope Out.Kernels,
                             MonadState State,
                             MonadLogger)

instance MonadFreshNames DistribM where
  getNameSource = gets stateNameSource
  putNameSource src = modify $ \s -> s { stateNameSource = src }

runDistribM :: (MonadLogger m, MonadFreshNames m) =>
               DistribM a -> m a
runDistribM (DistribM m) = do
  (x, msgs) <- modifyNameSource $ \src ->
    let (x, s, msgs) = runRWS m mempty (State src 0)
    in ((x, msgs), stateNameSource s)
  addLog msgs
  return x

transformFunDef :: (MonadFreshNames m, MonadLogger m) =>
                   FunDef SOACS -> m (Out.FunDef Out.Kernels)
transformFunDef (FunDef entry name rettype params body) = runDistribM $ do
  body' <- localScope (scopeOfFParams params) $
           transformBody mempty body
  return $ FunDef entry name rettype params body'

transformBody :: KernelPath -> Body -> DistribM (Out.Body Out.Kernels)
transformBody path body = do bnds <- transformStms path $ stmsToList $ bodyStms body
                             return $ mkBody bnds $ bodyResult body

transformStms :: KernelPath -> [Stm] -> DistribM KernelsStms
transformStms _ [] =
  return mempty
transformStms path (bnd:bnds) =
  sequentialisedUnbalancedStm bnd >>= \case
    Nothing -> do
      bnd' <- transformStm path bnd
      inScopeOf bnd' $
        (bnd'<>) <$> transformStms path bnds
    Just bnds' ->
      transformStms path $ stmsToList bnds' <> bnds

unbalancedLambda :: Lambda -> Bool
unbalancedLambda lam =
  unbalancedBody
  (namesFromList $ map paramName $ lambdaParams lam) $
  lambdaBody lam

  where subExpBound (Var i) bound = i `nameIn` bound
        subExpBound (Constant _) _ = False

        unbalancedBody bound body =
          any (unbalancedStm (bound <> boundInBody body) . stmExp) $
          bodyStms body

        -- XXX - our notion of balancing is probably still too naive.
        unbalancedStm bound (Op (Stream w _ _ _)) =
          w `subExpBound` bound
        unbalancedStm bound (Op (Screma w _ _)) =
          w `subExpBound` bound
        unbalancedStm _ Op{} =
          False
        unbalancedStm _ DoLoop{} = False

        unbalancedStm bound (If cond tbranch fbranch _) =
          cond `subExpBound` bound &&
          (unbalancedBody bound tbranch || unbalancedBody bound fbranch)

        unbalancedStm _ (BasicOp _) =
          False
        unbalancedStm _ (Apply fname _ _ _) =
          not $ isBuiltInFunction fname

sequentialisedUnbalancedStm :: Stm -> DistribM (Maybe (Stms SOACS))
sequentialisedUnbalancedStm (Let pat _ (Op soac@(Screma _ form _)))
  | Just (_, lam2) <- isRedomapSOAC form,
    unbalancedLambda lam2, lambdaContainsParallelism lam2 = do
      types <- asksScope scopeForSOACs
      Just . snd <$> runBinderT (FOT.transformSOAC pat soac) types
sequentialisedUnbalancedStm _ =
  return Nothing

cmpSizeLe :: String -> Out.SizeClass -> [SubExp]
          -> DistribM ((SubExp, Name), Out.Stms Out.Kernels)
cmpSizeLe desc size_class to_what = do
  x <- gets stateThresholdCounter
  modify $ \s -> s { stateThresholdCounter = x + 1}
  let size_key = nameFromString $ desc ++ "_" ++ show x
  runBinder $ do
    to_what' <- letSubExp "comparatee" =<<
                foldBinOp (Mul Int32) (intConst Int32 1) to_what
    cmp_res <- letSubExp desc $ Op $ SizeOp $ CmpSizeLe size_key size_class to_what'
    return (cmp_res, size_key)

kernelAlternatives :: (MonadFreshNames m, HasScope Out.Kernels m) =>
                      Out.Pattern Out.Kernels
                   -> Out.Body Out.Kernels
                   -> [(SubExp, Out.Body Out.Kernels)]
                   -> m (Out.Stms Out.Kernels)
kernelAlternatives pat default_body [] = runBinder_ $ do
  ses <- bodyBind default_body
  forM_ (zip (patternNames pat) ses) $ \(name, se) ->
    letBindNames_ [name] $ BasicOp $ SubExp se
kernelAlternatives pat default_body ((cond,alt):alts) = runBinder_ $ do
  alts_pat <- fmap (Pattern []) $ forM (patternElements pat) $ \pe -> do
    name <- newVName $ baseString $ patElemName pe
    return pe { patElemName = name }

  alt_stms <- kernelAlternatives alts_pat default_body alts
  let alt_body = mkBody alt_stms $ map Var $ patternValueNames alts_pat

  letBind_ pat $ If cond alt alt_body $ ifCommon $ patternTypes pat

transformStm :: KernelPath -> Stm -> DistribM KernelsStms

transformStm path (Let pat aux (Op (CmpThreshold what s))) = do
  ((r, _), stms) <- cmpSizeLe s (Out.SizeThreshold path) [what]
  runBinder_ $ do
    addStms stms
    addStm $ Let pat aux $ BasicOp $ SubExp r

transformStm path (Let pat aux (If c tb fb rt)) = do
  tb' <- transformBody path tb
  fb' <- transformBody path fb
  return $ oneStm $ Let pat aux $ If c tb' fb' rt

transformStm path (Let pat aux (DoLoop ctx val form body)) =
  localScope (castScope (scopeOf form) <>
              scopeOfFParams mergeparams) $
    oneStm . Let pat aux . DoLoop ctx val form' <$> transformBody path body
  where mergeparams = map fst $ ctx ++ val
        form' = case form of
                  WhileLoop cond ->
                    WhileLoop cond
                  ForLoop i it bound ps ->
                    ForLoop i it bound ps

transformStm path (Let pat (StmAux cs _) (Op (Screma w form arrs)))
  | Just lam <- isMapSOAC form =
      onMap path $ MapLoop pat cs w lam arrs

transformStm path (Let res_pat (StmAux cs _) (Op (Screma w form arrs)))
  | Just (scan_lam, nes) <- isScanSOAC form,
    Just do_iswim <- iswim res_pat w scan_lam $ zip nes arrs = do
      types <- asksScope scopeForSOACs
      transformStms path =<< (stmsToList . snd <$> runBinderT (certifying cs do_iswim) types)

  -- We are only willing to generate code for scanomaps that do not
  -- involve array accumulators, and do not have parallelism in their
  -- map function.  Such cases will fall through to the
  -- screma-splitting case, and produce an ordinary map and scan.
  -- Hopefully, the scan then triggers the ISWIM case above (otherwise
  -- we will still crash in code generation).
  | Just (scan_lam, nes, map_lam) <- isScanomapSOAC form,
    all primType $ lambdaReturnType scan_lam,
    not $ lambdaContainsParallelism map_lam = runBinder_ $ do
      let scan_lam' = soacsLambdaToKernels scan_lam
          map_lam' = soacsLambdaToKernels map_lam
      lvl <- segThreadCapped [w] "segscan" $ NoRecommendation SegNoVirt
      addStms =<< segScan lvl res_pat w scan_lam' map_lam' nes arrs [] []

transformStm path (Let res_pat (StmAux cs _) (Op (Screma w form arrs)))
  | Just [Reduce comm red_fun nes] <- isReduceSOAC form,
    let comm' | commutativeLambda red_fun = Commutative
              | otherwise                 = comm,
    Just do_irwim <- irwim res_pat w comm' red_fun $ zip nes arrs = do
      types <- asksScope scopeForSOACs
      bnds <- fst <$> runBinderT (simplifyStms =<< collectStms_ (certifying cs do_irwim)) types
      transformStms path $ stmsToList bnds

transformStm path (Let pat (StmAux cs _) (Op (Screma w form arrs)))
  | Just (reds, map_lam) <- isRedomapSOAC form = do

  let paralleliseOuter = runBinder_ $ do
        red_ops <- forM reds $ \(Reduce comm red_lam nes) -> do
          (red_lam', nes', shape) <- determineReduceOp red_lam nes
          let comm' | commutativeLambda red_lam' = Commutative
                    | otherwise = comm
          return $ SegRedOp comm' red_lam' nes' shape
        let map_lam_sequential = soacsLambdaToKernels map_lam
        lvl <- segThreadCapped [w] "segred" $ NoRecommendation SegNoVirt
        addStms =<<
          (fmap (certify cs) <$>
           nonSegRed lvl pat w red_ops map_lam_sequential arrs)

      outerParallelBody =
        renameBody =<<
        (mkBody <$> paralleliseOuter <*> pure (map Var (patternNames pat)))

      paralleliseInner path' = do
        (mapbnd, redbnd) <- redomapToMapAndReduce pat (w, comm', red_lam, map_lam, nes, arrs)
        transformStms path' [certify cs mapbnd, certify cs redbnd]
          where comm' | commutativeLambda red_lam = Commutative
                      | otherwise = comm
                (Reduce comm red_lam nes) = singleReduce reds

      innerParallelBody path' =
        renameBody =<<
        (mkBody <$> paralleliseInner path' <*> pure (map Var (patternNames pat)))

  if not $ lambdaContainsParallelism map_lam
    then paralleliseOuter
    else if incrementalFlattening then do
    ((outer_suff, outer_suff_key), suff_stms) <-
      sufficientParallelism "suff_outer_redomap" [w] path

    outer_stms <- outerParallelBody
    inner_stms <- innerParallelBody ((outer_suff_key, False):path)

    (suff_stms<>) <$> kernelAlternatives pat inner_stms [(outer_suff, outer_stms)]
    else paralleliseOuter

-- Streams can be handled in two different ways - either we
-- sequentialise the body or we keep it parallel and distribute.
transformStm path (Let pat (StmAux cs _) (Op (Stream w (Parallel _ _ _ []) map_fun arrs))) = do
  -- No reduction part.  Remove the stream and leave the body
  -- parallel.  It will be distributed.
  types <- asksScope scopeForSOACs
  transformStms path =<<
    (stmsToList . snd <$> runBinderT (certifying cs $ sequentialStreamWholeArray pat w [] map_fun arrs) types)

transformStm path (Let pat aux@(StmAux cs _) (Op (Stream w (Parallel o comm red_fun nes) fold_fun arrs)))
  | incrementalFlattening = do
      ((outer_suff, outer_suff_key), suff_stms) <-
        sufficientParallelism "suff_outer_stream" [w] path

      outer_stms <- outerParallelBody ((outer_suff_key, True) : path)
      inner_stms <- innerParallelBody ((outer_suff_key, False) : path)

      (suff_stms<>) <$> kernelAlternatives pat inner_stms [(outer_suff, outer_stms)]

  | otherwise = paralleliseOuter path

  where
    paralleliseOuter path'
      | any (not . primType) $ lambdaReturnType red_fun = do
          -- Split into a chunked map and a reduction, with the latter
          -- further transformed.
          let fold_fun' = soacsLambdaToKernels fold_fun

          let (red_pat_elems, concat_pat_elems) =
                splitAt (length nes) $ patternValueElements pat
              red_pat = Pattern [] red_pat_elems

          ((num_threads, red_results), stms) <-
            streamMap (map (baseString . patElemName) red_pat_elems) concat_pat_elems w
            Noncommutative fold_fun' nes arrs

          reduce_soac <- reduceSOAC [Reduce comm' red_fun nes]

          (stms<>) <$>
            inScopeOf stms
            (transformStm path' $ Let red_pat aux $
             Op (Screma num_threads reduce_soac red_results))

      | otherwise = do
          let red_fun_sequential = soacsLambdaToKernels red_fun
              fold_fun_sequential = soacsLambdaToKernels fold_fun
          fmap (certify cs) <$>
            streamRed pat w comm' red_fun_sequential fold_fun_sequential nes arrs

    outerParallelBody path' =
      renameBody =<<
      (mkBody <$> paralleliseOuter path' <*> pure (map Var (patternNames pat)))

    paralleliseInner path' = do
      types <- asksScope scopeForSOACs
      transformStms path' . fmap (certify cs) =<<
        (stmsToList . snd <$> runBinderT (sequentialStreamWholeArray pat w nes fold_fun arrs) types)

    innerParallelBody path' =
      renameBody =<<
      (mkBody <$> paralleliseInner path' <*> pure (map Var (patternNames pat)))

    comm' | commutativeLambda red_fun, o /= InOrder = Commutative
          | otherwise                               = comm

transformStm path (Let pat (StmAux cs _) (Op (Screma w form arrs))) = do
  -- This screma is too complicated for us to immediately do
  -- anything, so split it up and try again.
  scope <- asksScope scopeForSOACs
  transformStms path . map (certify cs) . stmsToList . snd =<<
    runBinderT (dissectScrema pat w form arrs) scope

transformStm path (Let pat _ (Op (Stream w (Sequential nes) fold_fun arrs))) = do
  -- Remove the stream and leave the body parallel.  It will be
  -- distributed.
  types <- asksScope scopeForSOACs
  transformStms path =<<
    (stmsToList . snd <$>
      runBinderT (sequentialStreamWholeArray pat w nes fold_fun arrs) types)

transformStm _ (Let pat (StmAux cs _) (Op (Scatter w lam ivs as))) = runBinder_ $ do
  let lam' = soacsLambdaToKernels lam
  write_i <- newVName "write_i"
  let (as_ws, as_ns, as_vs) = unzip3 as
      (i_res, v_res) = splitAt (sum as_ns) $ bodyResult $ lambdaBody lam'
      kstms = bodyStms $ lambdaBody lam'
      krets = do (a_w, a, is_vs) <- zip3 as_ws as_vs $ chunks as_ns $ zip i_res v_res
                 return $ WriteReturns [a_w] a [ ([i],v) | (i,v) <- is_vs ]
      body = KernelBody () kstms krets
      inputs = do (p, p_a) <- zip (lambdaParams lam') ivs
                  return $ KernelInput (paramName p) (paramType p) p_a [Var write_i]
  (kernel, stms) <-
    mapKernel segThreadCapped [(write_i,w)] inputs (map rowType $ patternTypes pat) body
  certifying cs $ do
    addStms stms
    letBind_ pat $ Op $ SegOp kernel

transformStm _ (Let orig_pat (StmAux cs _) (Op (Hist w ops bucket_fun imgs))) = do
  let bfun' = soacsLambdaToKernels bucket_fun

  -- It is important not to launch unnecessarily many threads for
  -- histograms, because it may mean we unnecessarily need to reduce
  -- subhistograms as well.
  runBinder_ $ do
    lvl <- segThreadCapped [w] "seghist" $ NoRecommendation SegNoVirt
    addStms =<< histKernel lvl orig_pat [] [] cs w ops bfun' imgs

transformStm _ bnd =
  runBinder_ $ FOT.transformStmRecursively bnd

sufficientParallelism :: String -> [SubExp] -> KernelPath
                      -> DistribM ((SubExp, Name), Out.Stms Out.Kernels)
sufficientParallelism desc ws path = cmpSizeLe desc (Out.SizeThreshold path) ws

-- | Returns the sizes of nested parallelism.
nestedParallelism :: Body -> [SubExp]
nestedParallelism = concatMap (parallelism . stmExp) . bodyStms
  where parallelism (Op (Scatter w _ _ _)) = [w]
        parallelism (Op (Screma w _ _)) = [w]
        parallelism (Op (Hist w _ _ _)) = [w]
        parallelism (Op (Stream w Sequential{} lam _))
          | chunk_size_param : _ <- lambdaParams lam =
              let update (Var v) | v == paramName chunk_size_param = w
                  update se = se
              in map update $ nestedParallelism $ lambdaBody lam
        parallelism (DoLoop _ _ _ body) = nestedParallelism body
        parallelism _ = []

-- | Intra-group parallelism is worthwhile if the lambda contains
-- non-map nested parallelism, or any nested parallelism inside a
-- loop.
worthIntraGroup :: Lambda -> Bool
worthIntraGroup lam = interesting $ lambdaBody lam
  where interesting body = not (null $ nestedParallelism body) &&
                           not (onlyMaps $ bodyStms body)
        onlyMaps = all $ isMapOrSeq . stmExp
        isMapOrSeq (Op (Screma _ form@(ScremaForm _ _ lam') _))
          | isJust $ isMapSOAC form = not $ worthIntraGroup lam'
        isMapOrSeq (Op Scatter{}) = True -- Basically a map.
        isMapOrSeq (DoLoop _ _ _ body) =
          null $ nestedParallelism body
        isMapOrSeq (Op _) = False
        isMapOrSeq _ = True

-- | A lambda is worth sequentialising if it contains nested
-- parallelism of an interesting kind.
worthSequentialising :: Lambda -> Bool
worthSequentialising lam = interesting $ lambdaBody lam
  where interesting body = any (interesting' . stmExp) $ bodyStms body
        interesting' (Op (Screma _ form@(ScremaForm _ _ lam') _))
          | isJust $ isMapSOAC form = worthSequentialising lam'
        interesting' (Op Scatter{}) = False -- Basically a map.
        interesting' (DoLoop _ _ _ body) = interesting body
        interesting' (Op _) = True
        interesting' _ = False


onTopLevelStms :: KernelPath -> Stms SOACS -> DistNestT DistribM KernelsStms
onTopLevelStms path stms = do
  scope <- askScope
  lift $ localScope scope $ transformStms path $ stmsToList stms

onMap :: KernelPath -> MapLoop -> DistribM KernelsStms
onMap path (MapLoop pat cs w lam arrs) = do
  types <- askScope
  let loopnest = MapNesting pat cs w $ zip (lambdaParams lam) arrs
      env path' = DistEnv
                  { distNest = singleNesting (Nesting mempty loopnest)
                  , distScope = scopeOfPattern pat <>
                                scopeForKernels (scopeOf lam) <>
                                types
                  , distOnInnerMap = onInnerMap path'
                  , distOnTopLevelStms = onTopLevelStms path'
                  , distSegLevel = segThreadCapped
                  }
      exploitInnerParallelism path' =
        runDistNestT (env path') $
        distributeMapBodyStms acc (bodyStms $ lambdaBody lam)

  if not incrementalFlattening then exploitInnerParallelism path
    else do

    let exploitOuterParallelism path' = do
          let lam' = soacsLambdaToKernels lam
          runDistNestT (env path') $ distribute $
            addStmsToKernel (bodyStms $ lambdaBody lam') acc

    onMap' (newKernel loopnest) path exploitOuterParallelism exploitInnerParallelism pat lam
    where acc = DistAcc { distTargets = singleTarget (pat, bodyResult $ lambdaBody lam)
                        , distStms = mempty
                        }

onMap' :: KernelNest -> KernelPath
       -> (KernelPath -> DistribM (Out.Stms Out.Kernels))
       -> (KernelPath -> DistribM (Out.Stms Out.Kernels))
       -> Pattern
       -> Lambda
       -> DistribM (Out.Stms Out.Kernels)
onMap' loopnest path mk_seq_stms mk_par_stms pat lam = do
  let nest_ws = kernelNestWidths loopnest
      res = map Var $ patternNames pat

  types <- askScope
  ((outer_suff, outer_suff_key), outer_suff_stms) <-
    sufficientParallelism "suff_outer_par" nest_ws path

  intra <- if worthIntraGroup lam then
             flip runReaderT types $ intraGroupParallelise loopnest lam
           else return Nothing
  seq_body <- renameBody =<< mkBody <$>
              mk_seq_stms ((outer_suff_key, True) : path) <*> pure res
  let seq_alts = [(outer_suff, seq_body) | worthSequentialising lam]

  case intra of
    Nothing -> do
      par_body <- renameBody =<< mkBody <$>
                  mk_par_stms ((outer_suff_key, False) : path) <*> pure res

      (outer_suff_stms<>) <$> kernelAlternatives pat par_body seq_alts

    Just ((_intra_min_par, intra_avail_par), group_size, log, intra_prelude, intra_stms) -> do
      addLog log
      -- We must check that all intra-group parallelism fits in a group.
      ((intra_ok, intra_suff_key), intra_suff_stms) <- do

        ((intra_suff, suff_key), check_suff_stms) <-
          sufficientParallelism "suff_intra_par" [intra_avail_par] $
          (outer_suff_key, False) : path

        runBinder $ do

          addStms intra_prelude

          max_group_size <-
            letSubExp "max_group_size" $ Op $ SizeOp $ Out.GetSizeMax Out.SizeGroup
          fits <- letSubExp "fits" $ BasicOp $
                  CmpOp (CmpSle Int32) group_size max_group_size

          addStms check_suff_stms

          intra_ok <- letSubExp "intra_suff_and_fits" $ BasicOp $ BinOp LogAnd fits intra_suff
          return (intra_ok, suff_key)

      group_par_body <- renameBody $ mkBody intra_stms res

      par_body <- renameBody =<< mkBody <$>
                  mk_par_stms ([(outer_suff_key, False),
                                (intra_suff_key, False)]
                                ++ path) <*> pure res

      ((outer_suff_stms<>intra_suff_stms)<>) <$>
        kernelAlternatives pat par_body (seq_alts ++ [(intra_ok, group_par_body)])

onInnerMap :: KernelPath -> MapLoop -> DistAcc -> DistNestT DistribM DistAcc
onInnerMap path maploop@(MapLoop pat cs w lam arrs) acc
  | unbalancedLambda lam, lambdaContainsParallelism lam =
      addStmToKernel (mapLoopStm maploop) acc
  | not incrementalFlattening =
      distributeMap maploop acc
  | otherwise =
      distributeSingleStm acc (mapLoopStm maploop) >>= \case
      Just (post_kernels, res, nest, acc')
        | Just (perm, _pat_unused) <- permutationAndMissing pat res -> do
            addKernels post_kernels
            multiVersion perm nest acc'
      _ -> distributeMap maploop acc

  where
    discardTargets acc' =
      -- FIXME: work around bogus targets.
      acc' { distTargets = singleTarget (mempty, mempty) }

    multiVersion perm nest acc' = do
      -- The kernel can be distributed by itself, so now we can
      -- decide whether to just sequentialise, or exploit inner
      -- parallelism.
      dist_env <- ask
      let extra_scope = targetsScope $ distTargets acc'
      scope <- (extra_scope<>) <$> askScope

      stms <- lift $ localScope scope $ do
        let maploop' = MapLoop pat cs w lam arrs

            exploitInnerParallelism path' = do
              let dist_env' =
                    dist_env { distOnTopLevelStms = onTopLevelStms path'
                             , distOnInnerMap = onInnerMap path'
                             }
              runDistNestT dist_env' $
                inNesting nest $ localScope extra_scope $
                discardTargets <$> distributeMap maploop' acc { distStms = mempty }

        -- Normally the permutation is for the output pattern, but
        -- we can't really change that, so we change the result
        -- order instead.
        let lam_res' = rearrangeShape perm $ bodyResult $ lambdaBody lam
            lam' = lam { lambdaBody = (lambdaBody lam) { bodyResult = lam_res' } }
            map_nesting = MapNesting pat cs w $ zip (lambdaParams lam) arrs
            nest' = pushInnerKernelNesting (pat, lam_res') map_nesting nest

        -- XXX: we do not construct a new KernelPath when
        -- sequentialising.  This is only OK as long as further
        -- versioning does not take place down that branch (it currently
        -- does not).
        (sequentialised_kernel, nestw_bnds) <- localScope extra_scope $ do
          let sequentialised_lam = soacsLambdaToKernels lam'
          constructKernel segThreadCapped nest' $ lambdaBody sequentialised_lam

        let outer_pat = loopNestingPattern $ fst nest
        (nestw_bnds<>) <$>
          onMap' nest' path
          (const $ return $ oneStm sequentialised_kernel)
          exploitInnerParallelism
          outer_pat lam'

      addKernel stms
      return acc'
