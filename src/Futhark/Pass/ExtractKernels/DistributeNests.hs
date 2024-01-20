{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns -Wno-incomplete-patterns -Wno-incomplete-uni-patterns -Wno-incomplete-record-updates #-}

module Futhark.Pass.ExtractKernels.DistributeNests
  ( MapLoop (..),
    mapLoopStm,
    bodyContainsParallelism,
    lambdaContainsParallelism,
    determineReduceOp,
    histKernel,
    DistEnv (..),
    DistAcc (..),
    runDistNestT,
    DistNestT,
    liftInner,
    distributeMap,
    distribute,
    distributeSingleStm,
    distributeMapBodyStms,
    addStmsToAcc,
    addStmToAcc,
    permutationAndMissing,
    addPostStms,
    postStm,
    inNesting,
  )
where

import Control.Arrow (first)
import Control.Monad
import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Strict
import Data.List (find, partition, tails)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as M
import Data.Maybe
import Futhark.IR
import Futhark.IR.GPU.Op (SegVirt (..))
import Futhark.IR.SOACS (SOACS)
import Futhark.IR.SOACS qualified as SOACS
import Futhark.IR.SOACS.SOAC hiding (HistOp, histDest)
import Futhark.IR.SOACS.Simplify (simpleSOACS, simplifyStms)
import Futhark.IR.SegOp
import Futhark.MonadFreshNames
import Futhark.Pass.ExtractKernels.BlockedKernel
import Futhark.Pass.ExtractKernels.Distribution
import Futhark.Pass.ExtractKernels.ISRWIM
import Futhark.Pass.ExtractKernels.Interchange
import Futhark.Tools
import Futhark.Transform.CopyPropagate
import Futhark.Transform.FirstOrderTransform qualified as FOT
import Futhark.Transform.Rename
import Futhark.Util.Log

scopeForSOACs :: (SameScope rep SOACS) => Scope rep -> Scope SOACS
scopeForSOACs = castScope

data MapLoop = MapLoop (Pat Type) (StmAux ()) SubExp (Lambda SOACS) [VName]

mapLoopStm :: MapLoop -> Stm SOACS
mapLoopStm (MapLoop pat aux w lam arrs) =
  Let pat aux $ Op $ Screma w arrs $ mapSOAC lam

data DistEnv rep m = DistEnv
  { distNest :: Nestings,
    distScope :: Scope rep,
    distOnTopLevelStms :: Stms SOACS -> DistNestT rep m (Stms rep),
    distOnInnerMap ::
      MapLoop ->
      DistAcc rep ->
      DistNestT rep m (DistAcc rep),
    distOnSOACSStms :: Stm SOACS -> Builder rep (Stms rep),
    distOnSOACSLambda :: Lambda SOACS -> Builder rep (Lambda rep),
    distSegLevel :: MkSegLevel rep m
  }

data DistAcc rep = DistAcc
  { distTargets :: Targets,
    distStms :: Stms rep
  }

data DistRes rep = DistRes
  { accPostStms :: PostStms rep,
    accLog :: Log
  }

instance Semigroup (DistRes rep) where
  DistRes ks1 log1 <> DistRes ks2 log2 =
    DistRes (ks1 <> ks2) (log1 <> log2)

instance Monoid (DistRes rep) where
  mempty = DistRes mempty mempty

newtype PostStms rep = PostStms {unPostStms :: Stms rep}

instance Semigroup (PostStms rep) where
  PostStms xs <> PostStms ys = PostStms $ ys <> xs

instance Monoid (PostStms rep) where
  mempty = PostStms mempty

typeEnvFromDistAcc :: (DistRep rep) => DistAcc rep -> Scope rep
typeEnvFromDistAcc = scopeOfPat . fst . outerTarget . distTargets

addStmsToAcc :: Stms rep -> DistAcc rep -> DistAcc rep
addStmsToAcc stms acc =
  acc {distStms = stms <> distStms acc}

addStmToAcc ::
  (MonadFreshNames m, DistRep rep) =>
  Stm SOACS ->
  DistAcc rep ->
  DistNestT rep m (DistAcc rep)
addStmToAcc stm acc = do
  onSoacs <- asks distOnSOACSStms
  (stm', _) <- runBuilder $ onSoacs stm
  pure acc {distStms = stm' <> distStms acc}

soacsLambda ::
  (MonadFreshNames m, DistRep rep) =>
  Lambda SOACS ->
  DistNestT rep m (Lambda rep)
soacsLambda lam = do
  onLambda <- asks distOnSOACSLambda
  fst <$> runBuilder (onLambda lam)

newtype DistNestT rep m a
  = DistNestT (ReaderT (DistEnv rep m) (WriterT (DistRes rep) m) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader (DistEnv rep m),
      MonadWriter (DistRes rep)
    )

liftInner :: (LocalScope rep m, DistRep rep) => m a -> DistNestT rep m a
liftInner m = do
  outer_scope <- askScope
  DistNestT $
    lift $
      lift $ do
        inner_scope <- askScope
        localScope (outer_scope `M.difference` inner_scope) m

instance (MonadFreshNames m) => MonadFreshNames (DistNestT rep m) where
  getNameSource = DistNestT $ lift getNameSource
  putNameSource = DistNestT . lift . putNameSource

instance (Monad m, ASTRep rep) => HasScope rep (DistNestT rep m) where
  askScope = asks distScope

instance (Monad m, ASTRep rep) => LocalScope rep (DistNestT rep m) where
  localScope types = local $ \env ->
    env {distScope = types <> distScope env}

instance (Monad m) => MonadLogger (DistNestT rep m) where
  addLog msgs = tell mempty {accLog = msgs}

runDistNestT ::
  (MonadLogger m, DistRep rep) =>
  DistEnv rep m ->
  DistNestT rep m (DistAcc rep) ->
  m (Stms rep)
runDistNestT env (DistNestT m) = do
  (acc, res) <- runWriterT $ runReaderT m env
  addLog $ accLog res
  -- There may be a few final targets remaining - these correspond to
  -- arrays that are identity mapped, and must have statements
  -- inserted here.
  pure $
    unPostStms (accPostStms res) <> identityStms (outerTarget $ distTargets acc)
  where
    outermost = nestingLoop $
      case distNest env of
        (nest, []) -> nest
        (_, nest : _) -> nest
    params_to_arrs =
      map (first paramName) $
        loopNestingParamsAndArrs outermost

    identityStms (rem_pat, res) =
      stmsFromList $ zipWith identityStm (patElems rem_pat) res
    identityStm pe (SubExpRes cs (Var v))
      | Just arr <- lookup v params_to_arrs =
          certify cs . Let (Pat [pe]) (defAux ()) . BasicOp $
            Replicate mempty (Var arr)
    identityStm pe (SubExpRes cs se) =
      certify cs . Let (Pat [pe]) (defAux ()) . BasicOp $
        Replicate (Shape [loopNestingWidth outermost]) se

addPostStms :: (Monad m) => PostStms rep -> DistNestT rep m ()
addPostStms ks = tell $ mempty {accPostStms = ks}

postStm :: (Monad m) => Stms rep -> DistNestT rep m ()
postStm stms = addPostStms $ PostStms stms

withStm ::
  (Monad m, DistRep rep) =>
  Stm SOACS ->
  DistNestT rep m a ->
  DistNestT rep m a
withStm stm = local $ \env ->
  env
    { distScope =
        castScope (scopeOf stm) <> distScope env,
      distNest =
        letBindInInnerNesting provided $
          distNest env
    }
  where
    provided = namesFromList $ patNames $ stmPat stm

leavingNesting ::
  (MonadFreshNames m, DistRep rep) =>
  DistAcc rep ->
  DistNestT rep m (DistAcc rep)
leavingNesting acc =
  case popInnerTarget $ distTargets acc of
    Nothing ->
      error "The kernel targets list is unexpectedly small"
    Just ((pat, res), newtargets)
      | not $ null $ distStms acc -> do
          -- Any statements left over correspond to something that
          -- could not be distributed because it would cause irregular
          -- arrays.  These must be reconstructed into a a Map SOAC
          -- that will be sequentialised. XXX: life would be better if
          -- we were able to distribute irregular parallelism.
          (Nesting _ inner, _) <- asks distNest
          let MapNesting _ aux w params_and_arrs = inner
              body = Body () (distStms acc) res
              used_in_body = freeIn body
              (used_params, used_arrs) =
                unzip $
                  filter ((`nameIn` used_in_body) . paramName . fst) params_and_arrs
              lam' =
                Lambda
                  { lambdaParams = used_params,
                    lambdaBody = body,
                    lambdaReturnType = map rowType $ patTypes pat
                  }
          stms <-
            runBuilder_ . auxing aux . FOT.transformSOAC pat $
              Screma w used_arrs $
                mapSOAC lam'

          pure $ acc {distTargets = newtargets, distStms = stms}
      | otherwise -> do
          -- Any results left over correspond to a Replicate or a Copy in
          -- the parent nesting, depending on whether the argument is a
          -- parameter of the innermost nesting.
          (Nesting _ inner_nesting, _) <- asks distNest
          let w = loopNestingWidth inner_nesting
              aux = loopNestingAux inner_nesting
              inps = loopNestingParamsAndArrs inner_nesting

              remnantStm pe (SubExpRes cs (Var v))
                | Just (_, arr) <- find ((== v) . paramName . fst) inps =
                    certify cs . Let (Pat [pe]) aux . BasicOp $
                      Replicate mempty (Var arr)
              remnantStm pe (SubExpRes cs se) =
                certify cs . Let (Pat [pe]) aux . BasicOp $
                  Replicate (Shape [w]) se

              stms =
                stmsFromList $ zipWith remnantStm (patElems pat) res

          pure $ acc {distTargets = newtargets, distStms = stms}

mapNesting ::
  (MonadFreshNames m, DistRep rep) =>
  Pat Type ->
  StmAux () ->
  SubExp ->
  Lambda SOACS ->
  [VName] ->
  DistNestT rep m (DistAcc rep) ->
  DistNestT rep m (DistAcc rep)
mapNesting pat aux w lam arrs m =
  local extend $ leavingNesting =<< m
  where
    nest =
      Nesting mempty $
        MapNesting pat aux w $
          zip (lambdaParams lam) arrs
    extend env =
      env
        { distNest = pushInnerNesting nest $ distNest env,
          distScope = castScope (scopeOf lam) <> distScope env
        }

inNesting ::
  (Monad m, DistRep rep) =>
  KernelNest ->
  DistNestT rep m a ->
  DistNestT rep m a
inNesting (outer, nests) = local $ \env ->
  env
    { distNest = (inner, nests'),
      distScope = foldMap scopeOfLoopNesting (outer : nests) <> distScope env
    }
  where
    (inner, nests') =
      case reverse nests of
        [] -> (asNesting outer, [])
        (inner' : ns) -> (asNesting inner', map asNesting $ outer : reverse ns)
    asNesting = Nesting mempty

bodyContainsParallelism :: Body SOACS -> Bool
bodyContainsParallelism = any isParallelStm . bodyStms
  where
    isParallelStm stm =
      isMap (stmExp stm)
        && not ("sequential" `inAttrs` stmAuxAttrs (stmAux stm))
    isMap BasicOp {} = False
    isMap Apply {} = False
    isMap Match {} = False
    isMap (Loop _ ForLoop {} body) = bodyContainsParallelism body
    isMap (Loop _ WhileLoop {} _) = False
    isMap (WithAcc _ lam) = bodyContainsParallelism $ lambdaBody lam
    isMap Op {} = True

lambdaContainsParallelism :: Lambda SOACS -> Bool
lambdaContainsParallelism = bodyContainsParallelism . lambdaBody

distributeMapBodyStms ::
  (MonadFreshNames m, LocalScope rep m, DistRep rep) =>
  DistAcc rep ->
  Stms SOACS ->
  DistNestT rep m (DistAcc rep)
distributeMapBodyStms orig_acc = distribute <=< onStms orig_acc . stmsToList
  where
    onStms acc [] = pure acc
    onStms acc (Let pat (StmAux cs _ _) (Op (Stream w arrs accs lam)) : stms) = do
      types <- asksScope scopeForSOACs
      stream_stms <-
        snd <$> runBuilderT (sequentialStreamWholeArray pat w accs lam arrs) types
      stream_stms' <-
        runReaderT (copyPropagateInStms simpleSOACS types stream_stms) types
      onStms acc $ stmsToList (fmap (certify cs) stream_stms') ++ stms
    onStms acc (stm : stms) =
      -- It is important that stm is in scope if 'maybeDistributeStm'
      -- wants to distribute, even if this causes the slightly silly
      -- situation that stm is in scope of itself.
      withStm stm $ maybeDistributeStm stm =<< onStms acc stms

onInnerMap :: (Monad m) => MapLoop -> DistAcc rep -> DistNestT rep m (DistAcc rep)
onInnerMap loop acc = do
  f <- asks distOnInnerMap
  f loop acc

onTopLevelStms :: (Monad m) => Stms SOACS -> DistNestT rep m ()
onTopLevelStms stms = do
  f <- asks distOnTopLevelStms
  postStm =<< f stms

maybeDistributeStm ::
  (MonadFreshNames m, LocalScope rep m, DistRep rep) =>
  Stm SOACS ->
  DistAcc rep ->
  DistNestT rep m (DistAcc rep)
maybeDistributeStm stm acc
  | "sequential" `inAttrs` stmAuxAttrs (stmAux stm) =
      addStmToAcc stm acc
maybeDistributeStm (Let pat aux (Op soac)) acc
  | "sequential_outer" `inAttrs` stmAuxAttrs aux =
      distributeMapBodyStms acc . fmap (certify (stmAuxCerts aux))
        =<< runBuilder_ (FOT.transformSOAC pat soac)
maybeDistributeStm stm@(Let pat _ (Op (Screma w arrs form))) acc
  | Just lam <- isMapSOAC form =
      -- Only distribute inside the map if we can distribute everything
      -- following the map.
      distributeIfPossible acc >>= \case
        Nothing -> addStmToAcc stm acc
        Just acc' -> distribute =<< onInnerMap (MapLoop pat (stmAux stm) w lam arrs) acc'
maybeDistributeStm stm@(Let pat aux (Loop merge form@ForLoop {} body)) acc
  | all (`notNameIn` freeIn (patTypes pat)) (patNames pat),
    bodyContainsParallelism body =
      distributeSingleStm acc stm >>= \case
        Just (kernels, res, nest, acc')
          | -- XXX: We cannot distribute if this loop depends on
            -- certificates bound within the loop nest (well, we could,
            -- but interchange would not be valid).  This is not a
            -- fundamental restriction, but an artifact of our
            -- certificate representation, which we should probably
            -- rethink.
            not $
              (freeIn form <> freeIn aux)
                `namesIntersect` boundInKernelNest nest,
            Just (perm, pat_unused) <- permutationAndMissing pat res ->
              -- We need to pretend pat_unused was used anyway, by adding
              -- it to the kernel nest.
              localScope (typeEnvFromDistAcc acc') $ do
                addPostStms kernels
                nest' <- expandKernelNest pat_unused nest
                types <- asksScope scopeForSOACs

                -- Simplification is key to hoisting out statements that
                -- were variant to the loop, but invariant to the outer maps
                -- (which are now innermost).
                stms <-
                  (`runReaderT` types) $
                    simplifyStms =<< interchangeLoops nest' (SeqLoop perm pat merge form body)
                onTopLevelStms stms
                pure acc'
        _ ->
          addStmToAcc stm acc
maybeDistributeStm stm@(Let pat _ (Match cond cases defbody ret)) acc
  | all (`notNameIn` freeIn pat) (patNames pat),
    any bodyContainsParallelism (defbody : map caseBody cases)
      || not (all primType (matchReturns ret)) =
      distributeSingleStm acc stm >>= \case
        Just (kernels, res, nest, acc')
          | not $
              (freeIn cond <> freeIn ret) `namesIntersect` boundInKernelNest nest,
            Just (perm, pat_unused) <- permutationAndMissing pat res ->
              -- We need to pretend pat_unused was used anyway, by adding
              -- it to the kernel nest.
              localScope (typeEnvFromDistAcc acc') $ do
                nest' <- expandKernelNest pat_unused nest
                addPostStms kernels
                types <- asksScope scopeForSOACs
                let branch = Branch perm pat cond cases defbody ret
                stms <-
                  (`runReaderT` types) $
                    simplifyStms . oneStm =<< interchangeBranch nest' branch
                onTopLevelStms stms
                pure acc'
        _ ->
          addStmToAcc stm acc
maybeDistributeStm stm@(Let pat _ (WithAcc inputs lam)) acc
  | lambdaContainsParallelism lam =
      distributeSingleStm acc stm >>= \case
        Just (kernels, res, nest, acc')
          | not $
              freeIn (drop num_accs (lambdaReturnType lam))
                `namesIntersect` boundInKernelNest nest,
            Just (perm, pat_unused) <- permutationAndMissing pat res ->
              -- We need to pretend pat_unused was used anyway, by adding
              -- it to the kernel nest.
              localScope (typeEnvFromDistAcc acc') $ do
                nest' <- expandKernelNest pat_unused nest
                types <- asksScope scopeForSOACs
                addPostStms kernels
                let withacc = WithAccStm perm pat inputs lam
                stms <-
                  (`runReaderT` types) $
                    simplifyStms . oneStm =<< interchangeWithAcc nest' withacc
                onTopLevelStms stms
                pure acc'
        _ ->
          addStmToAcc stm acc
  where
    num_accs = length inputs
maybeDistributeStm (Let pat aux (Op (Screma w arrs form))) acc
  | Just [Reduce comm lam nes] <- isReduceSOAC form,
    Just m <- irwim pat w comm lam $ zip nes arrs = do
      types <- asksScope scopeForSOACs
      (_, stms) <- runBuilderT (auxing aux m) types
      distributeMapBodyStms acc stms

-- Parallelise segmented scatters.
maybeDistributeStm stm@(Let pat (StmAux cs _ _) (Op (Scatter w ivs lam as))) acc =
  distributeSingleStm acc stm >>= \case
    Just (kernels, res, nest, acc')
      | Just (perm, pat_unused) <- permutationAndMissing pat res ->
          localScope (typeEnvFromDistAcc acc') $ do
            nest' <- expandKernelNest pat_unused nest
            lam' <- soacsLambda lam
            addPostStms kernels
            postStm =<< segmentedScatterKernel nest' perm pat cs w lam' ivs as
            pure acc'
    _ ->
      addStmToAcc stm acc
-- Parallelise segmented Hist.
maybeDistributeStm stm@(Let pat (StmAux cs _ _) (Op (Hist w as ops lam))) acc =
  distributeSingleStm acc stm >>= \case
    Just (kernels, res, nest, acc')
      | Just (perm, pat_unused) <- permutationAndMissing pat res ->
          localScope (typeEnvFromDistAcc acc') $ do
            lam' <- soacsLambda lam
            nest' <- expandKernelNest pat_unused nest
            addPostStms kernels
            postStm =<< segmentedHistKernel nest' perm cs w ops lam' as
            pure acc'
    _ ->
      addStmToAcc stm acc
-- Parallelise Index slices if the result is going to be returned
-- directly from the kernel.  This is because we would otherwise have
-- to sequentialise writing the result, which may be costly.
maybeDistributeStm stm@(Let (Pat [pe]) aux (BasicOp (Index arr slice))) acc
  | not $ null $ sliceDims slice,
    Var (patElemName pe) `elem` map resSubExp (snd (innerTarget (distTargets acc))) =
      distributeSingleStm acc stm >>= \case
        Just (kernels, _res, nest, acc') ->
          localScope (typeEnvFromDistAcc acc') $ do
            addPostStms kernels
            postStm =<< segmentedGatherKernel nest (stmAuxCerts aux) arr slice
            pure acc'
        _ ->
          addStmToAcc stm acc
-- If the scan can be distributed by itself, we will turn it into a
-- segmented scan.
--
-- If the scan cannot be distributed by itself, it will be
-- sequentialised in the default case for this function.
maybeDistributeStm stm@(Let pat (StmAux cs _ _) (Op (Screma w arrs form))) acc
  | Just (scans, map_lam) <- isScanomapSOAC form,
    Scan lam nes <- singleScan scans =
      distributeSingleStm acc stm >>= \case
        Just (kernels, res, nest, acc')
          | Just (perm, pat_unused) <- permutationAndMissing pat res ->
              -- We need to pretend pat_unused was used anyway, by adding
              -- it to the kernel nest.
              localScope (typeEnvFromDistAcc acc') $ do
                nest' <- expandKernelNest pat_unused nest
                map_lam' <- soacsLambda map_lam
                localScope (typeEnvFromDistAcc acc') $
                  segmentedScanomapKernel nest' perm cs w lam map_lam' nes arrs
                    >>= kernelOrNot mempty stm acc kernels acc'
        _ ->
          addStmToAcc stm acc
-- If the map function of the reduction contains parallelism we split
-- it, so that the parallelism can be exploited.
maybeDistributeStm (Let pat aux (Op (Screma w arrs form))) acc
  | Just (reds, map_lam) <- isRedomapSOAC form,
    lambdaContainsParallelism map_lam = do
      (mapstm, redstm) <-
        redomapToMapAndReduce pat (w, reds, map_lam, arrs)
      distributeMapBodyStms acc $ oneStm mapstm {stmAux = aux} <> oneStm redstm
-- if the reduction can be distributed by itself, we will turn it into a
-- segmented reduce.
--
-- If the reduction cannot be distributed by itself, it will be
-- sequentialised in the default case for this function.
maybeDistributeStm stm@(Let pat (StmAux cs _ _) (Op (Screma w arrs form))) acc
  | Just (reds, map_lam) <- isRedomapSOAC form,
    Reduce comm lam nes <- singleReduce reds =
      distributeSingleStm acc stm >>= \case
        Just (kernels, res, nest, acc')
          | Just (perm, pat_unused) <- permutationAndMissing pat res ->
              -- We need to pretend pat_unused was used anyway, by adding
              -- it to the kernel nest.
              localScope (typeEnvFromDistAcc acc') $ do
                nest' <- expandKernelNest pat_unused nest

                lam' <- soacsLambda lam
                map_lam' <- soacsLambda map_lam

                let comm'
                      | commutativeLambda lam = Commutative
                      | otherwise = comm

                regularSegmentedRedomapKernel nest' perm cs w comm' lam' map_lam' nes arrs
                  >>= kernelOrNot mempty stm acc kernels acc'
        _ ->
          addStmToAcc stm acc
maybeDistributeStm (Let pat (StmAux cs _ _) (Op (Screma w arrs form))) acc = do
  -- This Screma is too complicated for us to immediately do
  -- anything, so split it up and try again.
  scope <- asksScope scopeForSOACs
  distributeMapBodyStms acc . fmap (certify cs) . snd
    =<< runBuilderT (dissectScrema pat w form arrs) scope
maybeDistributeStm stm@(Let _ aux (BasicOp (Replicate shape (Var stm_arr)))) acc = do
  distributeSingleUnaryStm acc stm stm_arr $ \nest outerpat arr ->
    if shape == mempty
      then pure $ oneStm $ Let outerpat aux $ BasicOp $ Replicate mempty $ Var arr
      else runBuilder_ $ auxing aux $ do
        arr_t <- lookupType arr
        let arr_r = arrayRank arr_t
            nest_r = length (snd nest) + 1
            res_r = arr_r + shapeRank shape
        -- Move the to-be-replicated dimensions outermost.
        arr_tr <-
          letExp (baseString arr <> "_tr") . BasicOp $
            Rearrange ([nest_r .. arr_r - 1] ++ [0 .. nest_r - 1]) arr
        -- Replicate the now-outermost dimensions appropriately.
        arr_tr_rep <-
          letExp (baseString arr <> "_tr_rep") . BasicOp $
            Replicate shape (Var arr_tr)
        -- Move the replicated dimensions back where they belong.
        letBind outerpat . BasicOp $
          Rearrange ([res_r - nest_r .. res_r - 1] ++ [0 .. res_r - nest_r - 1]) arr_tr_rep
maybeDistributeStm (Let (Pat [pe]) aux (BasicOp (Replicate (Shape (d : ds)) v))) acc = do
  tmp <- newVName "tmp"
  let rowt = rowType $ patElemType pe
      newstm = Let (Pat [pe]) aux $ Op $ Screma d [] $ mapSOAC lam
      tmpstm =
        Let (Pat [PatElem tmp rowt]) aux $ BasicOp $ Replicate (Shape ds) v
      lam =
        Lambda
          { lambdaReturnType = [rowt],
            lambdaParams = [],
            lambdaBody = mkBody (oneStm tmpstm) [varRes tmp]
          }
  maybeDistributeStm newstm acc
-- Opaques are applied to the full array, because otherwise they can
-- drastically inhibit parallelisation in some cases.
maybeDistributeStm stm@(Let (Pat [pe]) aux (BasicOp (Opaque _ (Var stm_arr)))) acc
  | not $ primType $ typeOf pe =
      distributeSingleUnaryStm acc stm stm_arr $ \_ outerpat arr ->
        pure $ oneStm $ Let outerpat aux $ BasicOp $ Replicate mempty $ Var arr
maybeDistributeStm stm@(Let _ aux (BasicOp (Rearrange perm stm_arr))) acc =
  distributeSingleUnaryStm acc stm stm_arr $ \nest outerpat arr -> do
    let r = length (snd nest) + 1
        perm' = [0 .. r - 1] ++ map (+ r) perm
    -- We need to add a copy, because the original map nest
    -- will have produced an array without aliases, and so must we.
    arr' <- newVName $ baseString arr
    arr_t <- lookupType arr
    pure $
      stmsFromList
        [ Let (Pat [PatElem arr' arr_t]) aux $ BasicOp $ Replicate mempty $ Var arr,
          Let outerpat aux $ BasicOp $ Rearrange perm' arr'
        ]
maybeDistributeStm stm@(Let _ aux (BasicOp (Reshape k reshape stm_arr))) acc =
  distributeSingleUnaryStm acc stm stm_arr $ \nest outerpat arr -> do
    let reshape' = Shape (kernelNestWidths nest) <> reshape
    pure $ oneStm $ Let outerpat aux $ BasicOp $ Reshape k reshape' arr
maybeDistributeStm stm@(Let pat aux (BasicOp (Update _ arr slice (Var v)))) acc
  | not $ null $ sliceDims slice =
      distributeSingleStm acc stm >>= \case
        Just (kernels, res, nest, acc')
          | map resSubExp res == map Var (patNames $ stmPat stm),
            Just (perm, pat_unused) <- permutationAndMissing pat res -> do
              addPostStms kernels
              localScope (typeEnvFromDistAcc acc') $ do
                nest' <- expandKernelNest pat_unused nest
                postStm
                  =<< segmentedUpdateKernel nest' perm (stmAuxCerts aux) arr slice v
                pure acc'
        _ -> addStmToAcc stm acc
maybeDistributeStm stm@(Let _ aux (BasicOp (Concat d (x :| xs) w))) acc =
  distributeSingleStm acc stm >>= \case
    Just (kernels, _, nest, acc') ->
      localScope (typeEnvFromDistAcc acc') $
        segmentedConcat nest
          >>= kernelOrNot (stmAuxCerts aux) stm acc kernels acc'
    _ ->
      addStmToAcc stm acc
  where
    segmentedConcat nest =
      isSegmentedOp nest [0] mempty mempty [] (x : xs) $
        \pat _ _ _ (x' : xs') ->
          let d' = d + length (snd nest) + 1
           in addStm $ Let pat aux $ BasicOp $ Concat d' (x' :| xs') w
maybeDistributeStm stm acc =
  addStmToAcc stm acc

distributeSingleUnaryStm ::
  (MonadFreshNames m, LocalScope rep m, DistRep rep) =>
  DistAcc rep ->
  Stm SOACS ->
  VName ->
  (KernelNest -> Pat Type -> VName -> DistNestT rep m (Stms rep)) ->
  DistNestT rep m (DistAcc rep)
distributeSingleUnaryStm acc stm stm_arr f =
  distributeSingleStm acc stm >>= \case
    Just (kernels, res, nest, acc')
      | map resSubExp res == map Var (patNames $ stmPat stm),
        (outer, _) <- nest,
        [(_, arr)] <- loopNestingParamsAndArrs outer,
        boundInKernelNest nest `namesIntersection` freeIn stm
          == oneName stm_arr,
        perfectlyMapped arr nest -> do
          addPostStms kernels
          let outerpat = loopNestingPat $ fst nest
          localScope (typeEnvFromDistAcc acc') $ do
            postStm =<< f nest outerpat arr
            pure acc'
    _ -> addStmToAcc stm acc
  where
    perfectlyMapped arr (outer, nest)
      | [(p, arr')] <- loopNestingParamsAndArrs outer,
        arr == arr' =
          case nest of
            [] -> paramName p == stm_arr
            x : xs -> perfectlyMapped (paramName p) (x, xs)
      | otherwise =
          False

distribute ::
  (MonadFreshNames m, LocalScope rep m, DistRep rep) =>
  DistAcc rep ->
  DistNestT rep m (DistAcc rep)
distribute acc =
  fromMaybe acc <$> distributeIfPossible acc

mkSegLevel ::
  (MonadFreshNames m, LocalScope rep m, DistRep rep) =>
  DistNestT rep m (MkSegLevel rep (DistNestT rep m))
mkSegLevel = do
  mk_lvl <- asks distSegLevel
  pure $ \w desc r -> do
    (lvl, stms) <- lift $ liftInner $ runBuilderT' $ mk_lvl w desc r
    addStms stms
    pure lvl

distributeIfPossible ::
  (MonadFreshNames m, LocalScope rep m, DistRep rep) =>
  DistAcc rep ->
  DistNestT rep m (Maybe (DistAcc rep))
distributeIfPossible acc = do
  nest <- asks distNest
  mk_lvl <- mkSegLevel
  tryDistribute mk_lvl nest (distTargets acc) (distStms acc) >>= \case
    Nothing -> pure Nothing
    Just (targets, kernel) -> do
      postStm kernel
      pure $
        Just
          DistAcc
            { distTargets = targets,
              distStms = mempty
            }

distributeSingleStm ::
  (MonadFreshNames m, LocalScope rep m, DistRep rep) =>
  DistAcc rep ->
  Stm SOACS ->
  DistNestT
    rep
    m
    ( Maybe
        ( PostStms rep,
          Result,
          KernelNest,
          DistAcc rep
        )
    )
distributeSingleStm acc stm = do
  nest <- asks distNest
  mk_lvl <- mkSegLevel
  tryDistribute mk_lvl nest (distTargets acc) (distStms acc) >>= \case
    Nothing -> pure Nothing
    Just (targets, distributed_stms) ->
      tryDistributeStm nest targets stm >>= \case
        Nothing -> pure Nothing
        Just (res, targets', new_kernel_nest) ->
          pure $
            Just
              ( PostStms distributed_stms,
                res,
                new_kernel_nest,
                DistAcc
                  { distTargets = targets',
                    distStms = mempty
                  }
              )

segmentedScatterKernel ::
  (MonadFreshNames m, LocalScope rep m, DistRep rep) =>
  KernelNest ->
  [Int] ->
  Pat Type ->
  Certs ->
  SubExp ->
  Lambda rep ->
  [VName] ->
  [(Shape, Int, VName)] ->
  DistNestT rep m (Stms rep)
segmentedScatterKernel nest perm scatter_pat cs scatter_w lam ivs dests = do
  -- We replicate some of the checking done by 'isSegmentedOp', but
  -- things are different because a scatter is not a reduction or
  -- scan.
  --
  -- First, pretend that the scatter is also part of the nesting.  The
  -- KernelNest we produce here is technically not sensible, but it's
  -- good enough for flatKernel to work.
  let nesting =
        MapNesting scatter_pat (StmAux cs mempty ()) scatter_w $ zip (lambdaParams lam) ivs
      nest' =
        pushInnerKernelNesting (scatter_pat, bodyResult $ lambdaBody lam) nesting nest
  (ispace, kernel_inps) <- flatKernel nest'

  let (as_ws, as_ns, as) = unzip3 dests
      indexes = zipWith (*) as_ns $ map length as_ws

  -- The input/output arrays ('as') _must_ correspond to some kernel
  -- input, or else the original nested scatter would have been
  -- ill-typed.  Find them.
  as_inps <- mapM (findInput kernel_inps) as

  mk_lvl <- mkSegLevel

  let (is, vs) = splitAt (sum indexes) $ bodyResult $ lambdaBody lam
  (is', k_body_stms) <- runBuilder $ do
    addStms $ bodyStms $ lambdaBody lam
    pure is

  let grouped = groupScatterResults (zip3 as_ws as_ns as_inps) (is' ++ vs)
      (_, dest_arrs, _) = unzip3 grouped

  dest_arrs_ts <- mapM (lookupType . kernelInputArray) dest_arrs

  let k_body = KernelBody () k_body_stms (zipWith (inPlaceReturn ispace) dest_arrs_ts grouped)
      -- Remove unused kernel inputs, since some of these might
      -- reference the array we are scattering into.
      kernel_inps' =
        filter ((`nameIn` freeIn k_body) . kernelInputName) kernel_inps

  (k, k_stms) <- mapKernel mk_lvl ispace kernel_inps' dest_arrs_ts k_body

  traverse renameStm <=< runBuilder_ $ do
    addStms k_stms

    let pat =
          Pat . rearrangeShape perm $
            patElems $
              loopNestingPat $
                fst nest

    letBind pat $ Op $ segOp k
  where
    findInput kernel_inps a =
      maybe bad pure $ find ((== a) . kernelInputName) kernel_inps
    bad = error "Ill-typed nested scatter encountered."

    inPlaceReturn ispace arr_t (_, inp, is_vs) =
      WriteReturns
        ( foldMap (foldMap resCerts . fst) is_vs
            <> foldMap (resCerts . snd) is_vs
        )
        (kernelInputArray inp)
        [ (fullSlice arr_t $ map DimFix $ map Var (init gtids) ++ map resSubExp is, resSubExp v)
          | (is, v) <- is_vs
        ]
      where
        (gtids, _ws) = unzip ispace

segmentedUpdateKernel ::
  (MonadFreshNames m, LocalScope rep m, DistRep rep) =>
  KernelNest ->
  [Int] ->
  Certs ->
  VName ->
  Slice SubExp ->
  VName ->
  DistNestT rep m (Stms rep)
segmentedUpdateKernel nest perm cs arr slice v = do
  (base_ispace, kernel_inps) <- flatKernel nest
  let slice_dims = sliceDims slice
  slice_gtids <- replicateM (length slice_dims) (newVName "gtid_slice")

  let ispace = base_ispace ++ zip slice_gtids slice_dims

  ((dest_t, res), kstms) <- runBuilder $ do
    -- Compute indexes into full array.
    v' <-
      certifying cs . letSubExp "v" . BasicOp . Index v $
        Slice (map (DimFix . Var) slice_gtids)
    slice_is <-
      traverse (toSubExp "index") $
        fixSlice (fmap pe64 slice) $
          map (pe64 . Var) slice_gtids

    let write_is = map (Var . fst) base_ispace ++ slice_is
        arr' =
          maybe (error "incorrectly typed Update") kernelInputArray $
            find ((== arr) . kernelInputName) kernel_inps
    arr_t <- lookupType arr'
    pure
      ( arr_t,
        WriteReturns mempty arr' [(Slice $ map DimFix write_is, v')]
      )

  -- Remove unused kernel inputs, since some of these might
  -- reference the array we are scattering into.
  let kernel_inps' =
        filter ((`nameIn` (freeIn kstms <> freeIn res)) . kernelInputName) kernel_inps

  mk_lvl <- mkSegLevel
  (k, prestms) <-
    mapKernel mk_lvl ispace kernel_inps' [dest_t] $
      KernelBody () kstms [res]

  traverse renameStm <=< runBuilder_ $ do
    addStms prestms
    let pat = Pat . rearrangeShape perm $ patElems $ loopNestingPat $ fst nest
    letBind pat $ Op $ segOp k

segmentedGatherKernel ::
  (MonadFreshNames m, LocalScope rep m, DistRep rep) =>
  KernelNest ->
  Certs ->
  VName ->
  Slice SubExp ->
  DistNestT rep m (Stms rep)
segmentedGatherKernel nest cs arr slice = do
  let slice_dims = sliceDims slice
  slice_gtids <- replicateM (length slice_dims) (newVName "gtid_slice")

  (base_ispace, kernel_inps) <- flatKernel nest
  let ispace = base_ispace ++ zip slice_gtids slice_dims

  ((res_t, res), kstms) <- runBuilder $ do
    -- Compute indexes into full array.
    slice'' <-
      subExpSlice . sliceSlice (primExpSlice slice) $
        primExpSlice $
          Slice $
            map (DimFix . Var) slice_gtids
    v' <- certifying cs $ letSubExp "v" $ BasicOp $ Index arr slice''
    v_t <- subExpType v'
    pure (v_t, Returns ResultMaySimplify mempty v')

  mk_lvl <- mkSegLevel
  (k, prestms) <-
    mapKernel mk_lvl ispace kernel_inps [res_t] $
      KernelBody () kstms [res]

  traverse renameStm <=< runBuilder_ $ do
    addStms prestms

    let pat = Pat $ patElems $ loopNestingPat $ fst nest

    letBind pat $ Op $ segOp k

segmentedHistKernel ::
  (MonadFreshNames m, LocalScope rep m, DistRep rep) =>
  KernelNest ->
  [Int] ->
  Certs ->
  SubExp ->
  [SOACS.HistOp SOACS] ->
  Lambda rep ->
  [VName] ->
  DistNestT rep m (Stms rep)
segmentedHistKernel nest perm cs hist_w ops lam arrs = do
  -- We replicate some of the checking done by 'isSegmentedOp', but
  -- things are different because a Hist is not a reduction or
  -- scan.
  (ispace, inputs) <- flatKernel nest
  let orig_pat =
        Pat . rearrangeShape perm $
          patElems $
            loopNestingPat $
              fst nest

  -- The input/output arrays _must_ correspond to some kernel input,
  -- or else the original nested Hist would have been ill-typed.
  -- Find them.
  ops' <- forM ops $ \(SOACS.HistOp num_bins rf dests nes op) ->
    SOACS.HistOp num_bins rf
      <$> mapM (fmap kernelInputArray . findInput inputs) dests
      <*> pure nes
      <*> pure op

  mk_lvl <- asks distSegLevel
  onLambda <- asks distOnSOACSLambda
  let onLambda' = fmap fst . runBuilder . onLambda
  liftInner $
    runBuilderT'_ $ do
      -- It is important not to launch unnecessarily many threads for
      -- histograms, because it may mean we unnecessarily need to reduce
      -- subhistograms as well.
      lvl <- mk_lvl (hist_w : map snd ispace) "seghist" $ NoRecommendation SegNoVirt
      addStms
        =<< histKernel onLambda' lvl orig_pat ispace inputs cs hist_w ops' lam arrs
  where
    findInput kernel_inps a =
      maybe bad pure $ find ((== a) . kernelInputName) kernel_inps
    bad = error "Ill-typed nested Hist encountered."

histKernel ::
  (MonadBuilder m, DistRep (Rep m)) =>
  (Lambda SOACS -> m (Lambda (Rep m))) ->
  SegOpLevel (Rep m) ->
  Pat Type ->
  [(VName, SubExp)] ->
  [KernelInput] ->
  Certs ->
  SubExp ->
  [SOACS.HistOp SOACS] ->
  Lambda (Rep m) ->
  [VName] ->
  m (Stms (Rep m))
histKernel onLambda lvl orig_pat ispace inputs cs hist_w ops lam arrs = runBuilderT'_ $ do
  ops' <- forM ops $ \(SOACS.HistOp dest_shape rf dests nes op) -> do
    (op', nes', shape) <- determineReduceOp op nes
    op'' <- lift $ onLambda op'
    pure $ HistOp dest_shape rf dests nes' shape op''

  let isDest = flip elem $ concatMap histDest ops'
      inputs' = filter (not . isDest . kernelInputArray) inputs

  certifying cs $
    addStms
      =<< traverse renameStm
      =<< segHist lvl orig_pat hist_w ispace inputs' ops' lam arrs

determineReduceOp ::
  (MonadBuilder m) =>
  Lambda SOACS ->
  [SubExp] ->
  m (Lambda SOACS, [SubExp], Shape)
determineReduceOp lam nes =
  -- FIXME? We are assuming that the accumulator is a replicate, and
  -- we fish out its value in a gross way.
  case mapM subExpVar nes of
    Just ne_vs' -> do
      let (shape, lam') = isVectorMap lam
      nes' <- forM ne_vs' $ \ne_v -> do
        ne_v_t <- lookupType ne_v
        letSubExp "hist_ne" $
          BasicOp $
            Index ne_v $
              fullSlice ne_v_t $
                replicate (shapeRank shape) $
                  DimFix $
                    intConst Int64 0
      pure (lam', nes', shape)
    Nothing ->
      pure (lam, nes, mempty)

isVectorMap :: Lambda SOACS -> (Shape, Lambda SOACS)
isVectorMap lam
  | [Let (Pat pes) _ (Op (Screma w arrs form))] <-
      stmsToList $ bodyStms $ lambdaBody lam,
    map resSubExp (bodyResult (lambdaBody lam)) == map (Var . patElemName) pes,
    Just map_lam <- isMapSOAC form,
    arrs == map paramName (lambdaParams lam) =
      let (shape, lam') = isVectorMap map_lam
       in (Shape [w] <> shape, lam')
  | otherwise = (mempty, lam)

segmentedScanomapKernel ::
  (MonadFreshNames m, LocalScope rep m, DistRep rep) =>
  KernelNest ->
  [Int] ->
  Certs ->
  SubExp ->
  Lambda SOACS ->
  Lambda rep ->
  [SubExp] ->
  [VName] ->
  DistNestT rep m (Maybe (Stms rep))
segmentedScanomapKernel nest perm cs segment_size lam map_lam nes arrs = do
  mk_lvl <- asks distSegLevel
  onLambda <- asks distOnSOACSLambda
  let onLambda' = fmap fst . runBuilder . onLambda
  isSegmentedOp nest perm (freeIn lam) (freeIn map_lam) nes [] $
    \pat ispace inps nes' _ -> do
      (lam', nes'', shape) <- determineReduceOp lam nes'
      lam'' <- onLambda' lam'
      let scan_op = SegBinOp Noncommutative lam'' nes'' shape
      lvl <- mk_lvl (segment_size : map snd ispace) "segscan" $ NoRecommendation SegNoVirt
      addStms
        =<< traverse renameStm
        =<< segScan lvl pat cs segment_size [scan_op] map_lam arrs ispace inps

regularSegmentedRedomapKernel ::
  (MonadFreshNames m, LocalScope rep m, DistRep rep) =>
  KernelNest ->
  [Int] ->
  Certs ->
  SubExp ->
  Commutativity ->
  Lambda rep ->
  Lambda rep ->
  [SubExp] ->
  [VName] ->
  DistNestT rep m (Maybe (Stms rep))
regularSegmentedRedomapKernel nest perm cs segment_size comm lam map_lam nes arrs = do
  mk_lvl <- asks distSegLevel
  isSegmentedOp nest perm (freeIn lam) (freeIn map_lam) nes [] $
    \pat ispace inps nes' _ -> do
      let red_op = SegBinOp comm lam nes' mempty
      lvl <- mk_lvl (segment_size : map snd ispace) "segred" $ NoRecommendation SegNoVirt
      addStms
        =<< traverse renameStm
        =<< segRed lvl pat cs segment_size [red_op] map_lam arrs ispace inps

isSegmentedOp ::
  (MonadFreshNames m, LocalScope rep m, DistRep rep) =>
  KernelNest ->
  [Int] ->
  Names ->
  Names ->
  [SubExp] ->
  [VName] ->
  ( Pat Type ->
    [(VName, SubExp)] ->
    [KernelInput] ->
    [SubExp] ->
    [VName] ->
    BuilderT rep m ()
  ) ->
  DistNestT rep m (Maybe (Stms rep))
isSegmentedOp nest perm free_in_op _free_in_fold_op nes arrs m = runMaybeT $ do
  -- We must verify that array inputs to the operation are inputs to
  -- the outermost loop nesting or free in the loop nest.  Nothing
  -- free in the op may be bound by the nest.  Furthermore, the
  -- neutral elements must be free in the loop nest.
  --
  -- We must summarise any names from free_in_op that are bound in the
  -- nest, and describe how to obtain them given segment indices.

  let bound_by_nest = boundInKernelNest nest

  (ispace, kernel_inps) <- flatKernel nest

  when (free_in_op `namesIntersect` bound_by_nest) $
    fail "Non-fold lambda uses nest-bound parameters."

  let indices = map fst ispace

      prepareNe (Var v)
        | v `nameIn` bound_by_nest =
            fail "Neutral element bound in nest"
      prepareNe ne = pure ne

      prepareArr arr =
        case find ((== arr) . kernelInputName) kernel_inps of
          Just inp
            | kernelInputIndices inp == map Var indices ->
                pure $ pure $ kernelInputArray inp
          Nothing
            | arr `notNameIn` bound_by_nest ->
                -- This input is something that is free inside
                -- the loop nesting. We will have to replicate
                -- it.
                pure $
                  letExp
                    (baseString arr ++ "_repd")
                    (BasicOp $ Replicate (Shape $ map snd ispace) $ Var arr)
          _ ->
            fail "Input not free, perfectly mapped, or outermost."

  nes' <- mapM prepareNe nes

  mk_arrs <- mapM prepareArr arrs

  lift $
    liftInner $
      runBuilderT'_ $ do
        nested_arrs <- sequence mk_arrs

        let pat =
              Pat . rearrangeShape perm $
                patElems $
                  loopNestingPat $
                    fst nest

        m pat ispace kernel_inps nes' nested_arrs

permutationAndMissing :: Pat Type -> Result -> Maybe ([Int], [PatElem Type])
permutationAndMissing (Pat pes) res = do
  let (_used, unused) =
        partition ((`nameIn` freeIn res) . patElemName) pes
      res' = map resSubExp res
      res_expanded = res' ++ map (Var . patElemName) unused
  perm <- map (Var . patElemName) pes `isPermutationOf` res_expanded
  pure (perm, unused)

-- Add extra pattern elements to every kernel nesting level.
expandKernelNest ::
  (MonadFreshNames m) => [PatElem Type] -> KernelNest -> m KernelNest
expandKernelNest pes (outer_nest, inner_nests) = do
  let outer_size =
        loopNestingWidth outer_nest
          : map loopNestingWidth inner_nests
      inner_sizes = tails $ map loopNestingWidth inner_nests
  outer_nest' <- expandWith outer_nest outer_size
  inner_nests' <- zipWithM expandWith inner_nests inner_sizes
  pure (outer_nest', inner_nests')
  where
    expandWith nest dims = do
      pes' <- mapM (expandPatElemWith dims) pes
      pure
        nest
          { loopNestingPat =
              Pat $ patElems (loopNestingPat nest) <> pes'
          }

    expandPatElemWith dims pe = do
      name <- newVName $ baseString $ patElemName pe
      pure
        pe
          { patElemName = name,
            patElemDec = patElemType pe `arrayOfShape` Shape dims
          }

kernelOrNot ::
  (MonadFreshNames m, DistRep rep) =>
  Certs ->
  Stm SOACS ->
  DistAcc rep ->
  PostStms rep ->
  DistAcc rep ->
  Maybe (Stms rep) ->
  DistNestT rep m (DistAcc rep)
kernelOrNot cs stm acc _ _ Nothing =
  addStmToAcc (certify cs stm) acc
kernelOrNot cs _ _ kernels acc' (Just stms) = do
  addPostStms kernels
  postStm $ fmap (certify cs) stms
  pure acc'

distributeMap ::
  (MonadFreshNames m, LocalScope rep m, DistRep rep) =>
  MapLoop ->
  DistAcc rep ->
  DistNestT rep m (DistAcc rep)
distributeMap (MapLoop pat aux w lam arrs) acc =
  distribute
    =<< mapNesting
      pat
      aux
      w
      lam
      arrs
      (distribute =<< distributeMapBodyStms acc' lam_stms)
  where
    acc' =
      DistAcc
        { distTargets =
            pushInnerTarget
              (pat, bodyResult $ lambdaBody lam)
              $ distTargets acc,
          distStms = mempty
        }

    lam_stms = bodyStms $ lambdaBody lam
