{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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
import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Strict
import Data.List (find, partition, tails)
import Data.Maybe
import Futhark.IR
import Futhark.IR.SOACS (SOACS)
import qualified Futhark.IR.SOACS as SOACS
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
import qualified Futhark.Transform.FirstOrderTransform as FOT
import Futhark.Transform.Rename
import Futhark.Util
import Futhark.Util.Log

scopeForSOACs :: SameScope lore SOACS => Scope lore -> Scope SOACS
scopeForSOACs = castScope

data MapLoop = MapLoop SOACS.Pattern (StmAux ()) SubExp SOACS.Lambda [VName]

mapLoopStm :: MapLoop -> Stm SOACS
mapLoopStm (MapLoop pat aux w lam arrs) =
  Let pat aux $ Op $ Screma w (mapSOAC lam) arrs

data DistEnv lore m = DistEnv
  { distNest :: Nestings,
    distScope :: Scope lore,
    distOnTopLevelStms :: Stms SOACS -> DistNestT lore m (Stms lore),
    distOnInnerMap ::
      MapLoop ->
      DistAcc lore ->
      DistNestT lore m (DistAcc lore),
    distOnSOACSStms :: Stm SOACS -> Binder lore (Stms lore),
    distOnSOACSLambda :: Lambda SOACS -> Binder lore (Lambda lore),
    distSegLevel :: MkSegLevel lore m
  }

data DistAcc lore = DistAcc
  { distTargets :: Targets,
    distStms :: Stms lore
  }

data DistRes lore = DistRes
  { accPostStms :: PostStms lore,
    accLog :: Log
  }

instance Semigroup (DistRes lore) where
  DistRes ks1 log1 <> DistRes ks2 log2 =
    DistRes (ks1 <> ks2) (log1 <> log2)

instance Monoid (DistRes lore) where
  mempty = DistRes mempty mempty

newtype PostStms lore = PostStms {unPostStms :: Stms lore}

instance Semigroup (PostStms lore) where
  PostStms xs <> PostStms ys = PostStms $ ys <> xs

instance Monoid (PostStms lore) where
  mempty = PostStms mempty

typeEnvFromDistAcc :: DistLore lore => DistAcc lore -> Scope lore
typeEnvFromDistAcc = scopeOfPattern . fst . outerTarget . distTargets

addStmsToAcc :: Stms lore -> DistAcc lore -> DistAcc lore
addStmsToAcc stms acc =
  acc {distStms = stms <> distStms acc}

addStmToAcc ::
  (MonadFreshNames m, DistLore lore) =>
  Stm SOACS ->
  DistAcc lore ->
  DistNestT lore m (DistAcc lore)
addStmToAcc stm acc = do
  onSoacs <- asks distOnSOACSStms
  (stm', _) <- runBinder $ onSoacs stm
  return acc {distStms = stm' <> distStms acc}

soacsLambda ::
  (MonadFreshNames m, DistLore lore) =>
  Lambda SOACS ->
  DistNestT lore m (Lambda lore)
soacsLambda lam = do
  onLambda <- asks distOnSOACSLambda
  fst <$> runBinder (onLambda lam)

newtype DistNestT lore m a
  = DistNestT (ReaderT (DistEnv lore m) (WriterT (DistRes lore) m) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader (DistEnv lore m),
      MonadWriter (DistRes lore)
    )

instance MonadTrans (DistNestT lore) where
  lift = DistNestT . lift . lift

instance MonadFreshNames m => MonadFreshNames (DistNestT lore m) where
  getNameSource = DistNestT $ lift getNameSource
  putNameSource = DistNestT . lift . putNameSource

instance (Monad m, ASTLore lore) => HasScope lore (DistNestT lore m) where
  askScope = asks distScope

instance (Monad m, ASTLore lore) => LocalScope lore (DistNestT lore m) where
  localScope types = local $ \env ->
    env {distScope = types <> distScope env}

instance Monad m => MonadLogger (DistNestT lore m) where
  addLog msgs = tell mempty {accLog = msgs}

runDistNestT ::
  (MonadLogger m, DistLore lore) =>
  DistEnv lore m ->
  DistNestT lore m (DistAcc lore) ->
  m (Stms lore)
runDistNestT env (DistNestT m) = do
  (acc, res) <- runWriterT $ runReaderT m env
  addLog $ accLog res
  -- There may be a few final targets remaining - these correspond to
  -- arrays that are identity mapped, and must have statements
  -- inserted here.
  return $
    unPostStms (accPostStms res)
      <> identityStms (outerTarget $ distTargets acc)
  where
    outermost = nestingLoop $
      case distNest env of
        (nest, []) -> nest
        (_, nest : _) -> nest
    params_to_arrs =
      map (first paramName) $
        loopNestingParamsAndArrs outermost

    identityStms (rem_pat, res) =
      stmsFromList $ zipWith identityStm (patternValueElements rem_pat) res
    identityStm pe (Var v)
      | Just arr <- lookup v params_to_arrs =
        Let (Pattern [] [pe]) (defAux ()) $ BasicOp $ Copy arr
    identityStm pe se =
      Let (Pattern [] [pe]) (defAux ()) $
        BasicOp $
          Replicate (Shape [loopNestingWidth outermost]) se

addPostStms :: Monad m => PostStms lore -> DistNestT lore m ()
addPostStms ks = tell $ mempty {accPostStms = ks}

postStm :: Monad m => Stms lore -> DistNestT lore m ()
postStm stms = addPostStms $ PostStms stms

withStm ::
  (Monad m, DistLore lore) =>
  Stm SOACS ->
  DistNestT lore m a ->
  DistNestT lore m a
withStm stm = local $ \env ->
  env
    { distScope =
        castScope (scopeOf stm) <> distScope env,
      distNest =
        letBindInInnerNesting provided $
          distNest env
    }
  where
    provided = namesFromList $ patternNames $ stmPattern stm

leavingNesting ::
  (MonadFreshNames m, DistLore lore) =>
  DistAcc lore ->
  DistNestT lore m (DistAcc lore)
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
                  lambdaReturnType = map rowType $ patternTypes pat
                }
        stms <-
          runBinder_ $
            auxing aux $
              FOT.transformSOAC pat $
                Screma w (mapSOAC lam') used_arrs

        return $ acc {distTargets = newtargets, distStms = stms}
      | otherwise -> do
        -- Any results left over correspond to a Replicate or a Copy in
        -- the parent nesting, depending on whether the argument is a
        -- parameter of the innermost nesting.
        (Nesting _ inner_nesting, _) <- asks distNest
        let w = loopNestingWidth inner_nesting
            aux = loopNestingAux inner_nesting
            inps = loopNestingParamsAndArrs inner_nesting

            remnantStm pe (Var v)
              | Just (_, arr) <- find ((== v) . paramName . fst) inps =
                Let (Pattern [] [pe]) aux $
                  BasicOp $ Copy arr
            remnantStm pe se =
              Let (Pattern [] [pe]) aux $
                BasicOp $ Replicate (Shape [w]) se

            stms =
              stmsFromList $ zipWith remnantStm (patternElements pat) res

        return $ acc {distTargets = newtargets, distStms = stms}

mapNesting ::
  (MonadFreshNames m, DistLore lore) =>
  PatternT Type ->
  StmAux () ->
  SubExp ->
  Lambda SOACS ->
  [VName] ->
  DistNestT lore m (DistAcc lore) ->
  DistNestT lore m (DistAcc lore)
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
  (Monad m, DistLore lore) =>
  KernelNest ->
  DistNestT lore m a ->
  DistNestT lore m a
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
    isMap Op {} = True
    isMap _ = False

lambdaContainsParallelism :: Lambda SOACS -> Bool
lambdaContainsParallelism = bodyContainsParallelism . lambdaBody

distributeMapBodyStms :: (MonadFreshNames m, DistLore lore) => DistAcc lore -> Stms SOACS -> DistNestT lore m (DistAcc lore)
distributeMapBodyStms orig_acc = distribute <=< onStms orig_acc . stmsToList
  where
    onStms acc [] = return acc
    onStms acc (Let pat (StmAux cs _ _) (Op (Stream w (Sequential accs) lam arrs)) : stms) = do
      types <- asksScope scopeForSOACs
      stream_stms <-
        snd <$> runBinderT (sequentialStreamWholeArray pat w accs lam arrs) types
      (_, stream_stms') <-
        runReaderT (copyPropagateInStms simpleSOACS types stream_stms) types
      onStms acc $ stmsToList (fmap (certify cs) stream_stms') ++ stms
    onStms acc (stm : stms) =
      -- It is important that stm is in scope if 'maybeDistributeStm'
      -- wants to distribute, even if this causes the slightly silly
      -- situation that stm is in scope of itself.
      withStm stm $ maybeDistributeStm stm =<< onStms acc stms

onInnerMap :: Monad m => MapLoop -> DistAcc lore -> DistNestT lore m (DistAcc lore)
onInnerMap loop acc = do
  f <- asks distOnInnerMap
  f loop acc

onTopLevelStms :: Monad m => Stms SOACS -> DistNestT lore m ()
onTopLevelStms stms = do
  f <- asks distOnTopLevelStms
  postStm =<< f stms

maybeDistributeStm ::
  (MonadFreshNames m, DistLore lore) =>
  Stm SOACS ->
  DistAcc lore ->
  DistNestT lore m (DistAcc lore)
maybeDistributeStm stm acc
  | "sequential" `inAttrs` stmAuxAttrs (stmAux stm) =
    addStmToAcc stm acc
maybeDistributeStm (Let pat aux (Op soac)) acc
  | "sequential_outer" `inAttrs` stmAuxAttrs aux =
    distributeMapBodyStms acc . fmap (certify (stmAuxCerts aux))
      =<< runBinder_ (FOT.transformSOAC pat soac)
maybeDistributeStm stm@(Let pat _ (Op (Screma w form arrs))) acc
  | Just lam <- isMapSOAC form =
    -- Only distribute inside the map if we can distribute everything
    -- following the map.
    distributeIfPossible acc >>= \case
      Nothing -> addStmToAcc stm acc
      Just acc' -> distribute =<< onInnerMap (MapLoop pat (stmAux stm) w lam arrs) acc'
maybeDistributeStm bnd@(Let pat _ (DoLoop [] val form@ForLoop {} body)) acc
  | null (patternContextElements pat),
    bodyContainsParallelism body =
    distributeSingleStm acc bnd >>= \case
      Just (kernels, res, nest, acc')
        | not $ freeIn form `namesIntersect` boundInKernelNest nest,
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
                fmap snd . simplifyStms
                  =<< interchangeLoops nest' (SeqLoop perm pat val form body)
            onTopLevelStms stms
            return acc'
      _ ->
        addStmToAcc bnd acc
maybeDistributeStm stm@(Let pat _ (If cond tbranch fbranch ret)) acc
  | null (patternContextElements pat),
    bodyContainsParallelism tbranch || bodyContainsParallelism fbranch
      || not (all primType (ifReturns ret)) =
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
            let branch = Branch perm pat cond tbranch fbranch ret
            stms <-
              (`runReaderT` types) $
                fmap snd . simplifyStms
                  =<< interchangeBranch nest' branch
            onTopLevelStms stms
            return acc'
      _ ->
        addStmToAcc stm acc
maybeDistributeStm (Let pat aux (Op (Screma w form arrs))) acc
  | Just [Reduce comm lam nes] <- isReduceSOAC form,
    Just m <- irwim pat w comm lam $ zip nes arrs = do
    types <- asksScope scopeForSOACs
    (_, bnds) <- runBinderT (auxing aux m) types
    distributeMapBodyStms acc bnds

-- Parallelise segmented scatters.
maybeDistributeStm bnd@(Let pat (StmAux cs _ _) (Op (Scatter w lam ivs as))) acc =
  distributeSingleStm acc bnd >>= \case
    Just (kernels, res, nest, acc')
      | Just (perm, pat_unused) <- permutationAndMissing pat res ->
        localScope (typeEnvFromDistAcc acc') $ do
          nest' <- expandKernelNest pat_unused nest
          lam' <- soacsLambda lam
          addPostStms kernels
          postStm =<< segmentedScatterKernel nest' perm pat cs w lam' ivs as
          return acc'
    _ ->
      addStmToAcc bnd acc
-- Parallelise segmented Hist.
maybeDistributeStm bnd@(Let pat (StmAux cs _ _) (Op (Hist w ops lam as))) acc =
  distributeSingleStm acc bnd >>= \case
    Just (kernels, res, nest, acc')
      | Just (perm, pat_unused) <- permutationAndMissing pat res ->
        localScope (typeEnvFromDistAcc acc') $ do
          lam' <- soacsLambda lam
          nest' <- expandKernelNest pat_unused nest
          addPostStms kernels
          postStm =<< segmentedHistKernel nest' perm cs w ops lam' as
          return acc'
    _ ->
      addStmToAcc bnd acc
-- Parallelise Index slices if the result is going to be returned
-- directly from the kernel.  This is because we would otherwise have
-- to sequentialise writing the result, which may be costly.
maybeDistributeStm
  stm@( Let
          (Pattern [] [pe])
          aux
          (BasicOp (Index arr slice))
        )
  acc
    | not $ null $ sliceDims slice,
      Var (patElemName pe) `elem` snd (innerTarget (distTargets acc)) =
      distributeSingleStm acc stm >>= \case
        Just (kernels, _res, nest, acc') ->
          localScope (typeEnvFromDistAcc acc') $ do
            addPostStms kernels
            postStm =<< segmentedGatherKernel nest (stmAuxCerts aux) arr slice
            return acc'
        _ ->
          addStmToAcc stm acc
-- If the scan can be distributed by itself, we will turn it into a
-- segmented scan.
--
-- If the scan cannot be distributed by itself, it will be
-- sequentialised in the default case for this function.
maybeDistributeStm bnd@(Let pat (StmAux cs _ _) (Op (Screma w form arrs))) acc
  | Just (scans, map_lam) <- isScanomapSOAC form,
    Scan lam nes <- singleScan scans =
    distributeSingleStm acc bnd >>= \case
      Just (kernels, res, nest, acc')
        | Just (perm, pat_unused) <- permutationAndMissing pat res ->
          -- We need to pretend pat_unused was used anyway, by adding
          -- it to the kernel nest.
          localScope (typeEnvFromDistAcc acc') $ do
            nest' <- expandKernelNest pat_unused nest
            map_lam' <- soacsLambda map_lam
            lam' <- soacsLambda lam
            localScope (typeEnvFromDistAcc acc') $
              segmentedScanomapKernel nest' perm w lam' map_lam' nes arrs
                >>= kernelOrNot cs bnd acc kernels acc'
      _ ->
        addStmToAcc bnd acc
-- if the reduction can be distributed by itself, we will turn it into a
-- segmented reduce.
--
-- If the reduction cannot be distributed by itself, it will be
-- sequentialised in the default case for this function.
maybeDistributeStm bnd@(Let pat (StmAux cs _ _) (Op (Screma w form arrs))) acc
  | Just (reds, map_lam) <- isRedomapSOAC form,
    Reduce comm lam nes <- singleReduce reds =
    distributeSingleStm acc bnd >>= \case
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

            regularSegmentedRedomapKernel nest' perm w comm' lam' map_lam' nes arrs
              >>= kernelOrNot cs bnd acc kernels acc'
      _ ->
        addStmToAcc bnd acc
maybeDistributeStm (Let pat (StmAux cs _ _) (Op (Screma w form arrs))) acc = do
  -- This Screma is too complicated for us to immediately do
  -- anything, so split it up and try again.
  scope <- asksScope scopeForSOACs
  distributeMapBodyStms acc . fmap (certify cs) . snd
    =<< runBinderT (dissectScrema pat w form arrs) scope
maybeDistributeStm (Let pat aux (BasicOp (Replicate (Shape (d : ds)) v))) acc
  | [t] <- patternTypes pat = do
    tmp <- newVName "tmp"
    let rowt = rowType t
        newbnd = Let pat aux $ Op $ Screma d (mapSOAC lam) []
        tmpbnd =
          Let (Pattern [] [PatElem tmp rowt]) aux $
            BasicOp $ Replicate (Shape ds) v
        lam =
          Lambda
            { lambdaReturnType = [rowt],
              lambdaParams = [],
              lambdaBody = mkBody (oneStm tmpbnd) [Var tmp]
            }
    maybeDistributeStm newbnd acc
maybeDistributeStm bnd@(Let _ aux (BasicOp Copy {})) acc =
  distributeSingleUnaryStm acc bnd $ \_ outerpat arr ->
    return $ oneStm $ Let outerpat aux $ BasicOp $ Copy arr
-- Opaques are applied to the full array, because otherwise they can
-- drastically inhibit parallelisation in some cases.
maybeDistributeStm bnd@(Let (Pattern [] [pe]) aux (BasicOp Opaque {})) acc
  | not $ primType $ typeOf pe =
    distributeSingleUnaryStm acc bnd $ \_ outerpat arr ->
      return $ oneStm $ Let outerpat aux $ BasicOp $ Copy arr
maybeDistributeStm bnd@(Let _ aux (BasicOp (Rearrange perm _))) acc =
  distributeSingleUnaryStm acc bnd $ \nest outerpat arr -> do
    let r = length (snd nest) + 1
        perm' = [0 .. r -1] ++ map (+ r) perm
    -- We need to add a copy, because the original map nest
    -- will have produced an array without aliases, and so must we.
    arr' <- newVName $ baseString arr
    arr_t <- lookupType arr
    return $
      stmsFromList
        [ Let (Pattern [] [PatElem arr' arr_t]) aux $ BasicOp $ Copy arr,
          Let outerpat aux $ BasicOp $ Rearrange perm' arr'
        ]
maybeDistributeStm bnd@(Let _ aux (BasicOp (Reshape reshape _))) acc =
  distributeSingleUnaryStm acc bnd $ \nest outerpat arr -> do
    let reshape' =
          map DimNew (kernelNestWidths nest)
            ++ map DimNew (newDims reshape)
    return $ oneStm $ Let outerpat aux $ BasicOp $ Reshape reshape' arr
maybeDistributeStm stm@(Let _ aux (BasicOp (Rotate rots _))) acc =
  distributeSingleUnaryStm acc stm $ \nest outerpat arr -> do
    let rots' = map (const $ intConst Int32 0) (kernelNestWidths nest) ++ rots
    return $ oneStm $ Let outerpat aux $ BasicOp $ Rotate rots' arr
maybeDistributeStm stm@(Let pat aux (BasicOp (Update arr slice (Var v)))) acc
  | not $ null $ sliceDims slice =
    distributeSingleStm acc stm >>= \case
      Just (kernels, res, nest, acc')
        | res == map Var (patternNames $ stmPattern stm),
          Just (perm, pat_unused) <- permutationAndMissing pat res -> do
          addPostStms kernels
          localScope (typeEnvFromDistAcc acc') $ do
            nest' <- expandKernelNest pat_unused nest
            postStm
              =<< segmentedUpdateKernel nest' perm (stmAuxCerts aux) arr slice v
            return acc'
      _ -> addStmToAcc stm acc
-- XXX?  This rule is present to avoid the case where an in-place
-- update is distributed as its own kernel, as this would mean thread
-- then writes the entire array that it updated.  This is problematic
-- because the in-place updates is O(1), but writing the array is
-- O(n).  It is OK if the in-place update is preceded, followed, or
-- nested inside a sequential loop or similar, because that will
-- probably be O(n) by itself.  As a hack, we only distribute if there
-- does not appear to be a loop following.  The better solution is to
-- depend on memory block merging for this optimisation, but it is not
-- ready yet.
maybeDistributeStm (Let pat aux (BasicOp (Update arr [DimFix i] v))) acc
  | [t] <- patternTypes pat,
    arrayRank t == 1,
    not $ any (amortises . stmExp) $ distStms acc = do
    let w = arraySize 0 t
        et = stripArray 1 t
        lam =
          Lambda
            { lambdaParams = [],
              lambdaReturnType = [Prim int32, et],
              lambdaBody = mkBody mempty [i, v]
            }
    maybeDistributeStm (Let pat aux $ Op $ Scatter (intConst Int32 1) lam [] [(w, 1, arr)]) acc
  where
    amortises DoLoop {} = True
    amortises Op {} = True
    amortises _ = False
maybeDistributeStm stm@(Let _ aux (BasicOp (Concat d x xs w))) acc =
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
           in addStm $ Let pat aux $ BasicOp $ Concat d' x' xs' w
maybeDistributeStm bnd acc =
  addStmToAcc bnd acc

distributeSingleUnaryStm ::
  (MonadFreshNames m, DistLore lore) =>
  DistAcc lore ->
  Stm SOACS ->
  (KernelNest -> PatternT Type -> VName -> DistNestT lore m (Stms lore)) ->
  DistNestT lore m (DistAcc lore)
distributeSingleUnaryStm acc bnd f =
  distributeSingleStm acc bnd >>= \case
    Just (kernels, res, nest, acc')
      | res == map Var (patternNames $ stmPattern bnd),
        (outer, _) <- nest,
        [(arr_p, arr)] <- loopNestingParamsAndArrs outer,
        boundInKernelNest nest `namesIntersection` freeIn bnd
          == oneName (paramName arr_p),
        perfectlyMapped arr nest -> do
        addPostStms kernels
        let outerpat = loopNestingPattern $ fst nest
        localScope (typeEnvFromDistAcc acc') $ do
          postStm =<< f nest outerpat arr
          return acc'
    _ -> addStmToAcc bnd acc
  where
    perfectlyMapped arr (outer, nest)
      | [(p, arr')] <- loopNestingParamsAndArrs outer,
        arr == arr' =
        case nest of
          [] -> True
          x : xs -> perfectlyMapped (paramName p) (x, xs)
      | otherwise =
        False

distribute :: (MonadFreshNames m, DistLore lore) => DistAcc lore -> DistNestT lore m (DistAcc lore)
distribute acc =
  fromMaybe acc <$> distributeIfPossible acc

mkSegLevel :: (MonadFreshNames m, DistLore lore) => DistNestT lore m (MkSegLevel lore (DistNestT lore m))
mkSegLevel = do
  mk_lvl <- asks distSegLevel
  return $ \w desc r -> do
    scope <- askScope
    (lvl, stms) <- lift $ lift $ runBinderT (mk_lvl w desc r) scope
    addStms stms
    return lvl

distributeIfPossible :: (MonadFreshNames m, DistLore lore) => DistAcc lore -> DistNestT lore m (Maybe (DistAcc lore))
distributeIfPossible acc = do
  nest <- asks distNest
  mk_lvl <- mkSegLevel
  tryDistribute mk_lvl nest (distTargets acc) (distStms acc) >>= \case
    Nothing -> return Nothing
    Just (targets, kernel) -> do
      postStm kernel
      return $
        Just
          DistAcc
            { distTargets = targets,
              distStms = mempty
            }

distributeSingleStm ::
  (MonadFreshNames m, DistLore lore) =>
  DistAcc lore ->
  Stm SOACS ->
  DistNestT
    lore
    m
    ( Maybe
        ( PostStms lore,
          Result,
          KernelNest,
          DistAcc lore
        )
    )
distributeSingleStm acc bnd = do
  nest <- asks distNest
  mk_lvl <- mkSegLevel
  tryDistribute mk_lvl nest (distTargets acc) (distStms acc) >>= \case
    Nothing -> return Nothing
    Just (targets, distributed_bnds) ->
      tryDistributeStm nest targets bnd >>= \case
        Nothing -> return Nothing
        Just (res, targets', new_kernel_nest) ->
          return $
            Just
              ( PostStms distributed_bnds,
                res,
                new_kernel_nest,
                DistAcc
                  { distTargets = targets',
                    distStms = mempty
                  }
              )

segmentedScatterKernel ::
  (MonadFreshNames m, DistLore lore) =>
  KernelNest ->
  [Int] ->
  PatternT Type ->
  Certificates ->
  SubExp ->
  Lambda lore ->
  [VName] ->
  [(SubExp, Int, VName)] ->
  DistNestT lore m (Stms lore)
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

  -- The input/output arrays ('as') _must_ correspond to some kernel
  -- input, or else the original nested scatter would have been
  -- ill-typed.  Find them.
  as_inps <- mapM (findInput kernel_inps) as

  mk_lvl <- mkSegLevel

  let rts =
        concatMap (take 1) $
          chunks as_ns $
            drop (sum as_ns) $ lambdaReturnType lam
      (is, vs) = splitAt (sum as_ns) $ bodyResult $ lambdaBody lam

  -- Maybe add certificates to the indices.
  (is', k_body_stms) <- runBinder $ do
    addStms $ bodyStms $ lambdaBody lam
    forM is $ \i ->
      if cs == mempty
        then return i
        else certifying cs $ letSubExp "scatter_i" $ BasicOp $ SubExp i

  let k_body =
        KernelBody () k_body_stms $
          map (inPlaceReturn ispace) $
            zip3 as_ws as_inps $ chunks as_ns $ zip is' vs

  (k, k_bnds) <- mapKernel mk_lvl ispace kernel_inps rts k_body

  traverse renameStm <=< runBinder_ $ do
    addStms k_bnds

    let pat =
          Pattern [] $
            rearrangeShape perm $
              patternValueElements $ loopNestingPattern $ fst nest

    letBind pat $ Op $ segOp k
  where
    findInput kernel_inps a =
      maybe bad return $ find ((== a) . kernelInputName) kernel_inps
    bad = error "Ill-typed nested scatter encountered."

    inPlaceReturn ispace (aw, inp, is_vs) =
      WriteReturns
        (init ws ++ [aw])
        (kernelInputArray inp)
        [(map DimFix $ map Var (init gtids) ++ [i], v) | (i, v) <- is_vs]
      where
        (gtids, ws) = unzip ispace

segmentedUpdateKernel ::
  (MonadFreshNames m, DistLore lore) =>
  KernelNest ->
  [Int] ->
  Certificates ->
  VName ->
  Slice SubExp ->
  VName ->
  DistNestT lore m (Stms lore)
segmentedUpdateKernel nest perm cs arr slice v = do
  (base_ispace, kernel_inps) <- flatKernel nest
  let slice_dims = sliceDims slice
  slice_gtids <- replicateM (length slice_dims) (newVName "gtid_slice")

  let ispace = base_ispace ++ zip slice_gtids slice_dims

  ((res_t, res), kstms) <- runBinder $ do
    -- Compute indexes into full array.
    v' <-
      certifying cs $
        letSubExp "v" $ BasicOp $ Index v $ map (DimFix . Var) slice_gtids
    let pexp = primExpFromSubExp int32
    slice_is <-
      traverse (toSubExp "index") $
        fixSlice (map (fmap pexp) slice) $ map (pexp . Var) slice_gtids

    let write_is = map (Var . fst) base_ispace ++ slice_is
        arr' =
          maybe (error "incorrectly typed Update") kernelInputArray $
            find ((== arr) . kernelInputName) kernel_inps
    arr_t <- lookupType arr'
    v_t <- subExpType v'
    return
      ( v_t,
        WriteReturns (arrayDims arr_t) arr' [(map DimFix write_is, v')]
      )

  mk_lvl <- mkSegLevel
  (k, prestms) <-
    mapKernel mk_lvl ispace kernel_inps [res_t] $
      KernelBody () kstms [res]

  traverse renameStm <=< runBinder_ $ do
    addStms prestms

    let pat =
          Pattern [] $
            rearrangeShape perm $
              patternValueElements $ loopNestingPattern $ fst nest

    letBind pat $ Op $ segOp k

segmentedGatherKernel ::
  (MonadFreshNames m, DistLore lore) =>
  KernelNest ->
  Certificates ->
  VName ->
  Slice SubExp ->
  DistNestT lore m (Stms lore)
segmentedGatherKernel nest cs arr slice = do
  let slice_dims = sliceDims slice
  slice_gtids <- replicateM (length slice_dims) (newVName "gtid_slice")

  (base_ispace, kernel_inps) <- flatKernel nest
  let ispace = base_ispace ++ zip slice_gtids slice_dims

  ((res_t, res), kstms) <- runBinder $ do
    -- Compute indexes into full array.
    slice'' <-
      subExpSlice $
        sliceSlice (primExpSlice slice) $
          primExpSlice $ map (DimFix . Var) slice_gtids
    v' <- certifying cs $ letSubExp "v" $ BasicOp $ Index arr slice''
    v_t <- subExpType v'
    return (v_t, Returns ResultMaySimplify v')

  mk_lvl <- mkSegLevel
  (k, prestms) <-
    mapKernel mk_lvl ispace kernel_inps [res_t] $
      KernelBody () kstms [res]

  traverse renameStm <=< runBinder_ $ do
    addStms prestms

    let pat = Pattern [] $ patternValueElements $ loopNestingPattern $ fst nest

    letBind pat $ Op $ segOp k

segmentedHistKernel ::
  (MonadFreshNames m, DistLore lore) =>
  KernelNest ->
  [Int] ->
  Certificates ->
  SubExp ->
  [SOACS.HistOp SOACS] ->
  Lambda lore ->
  [VName] ->
  DistNestT lore m (Stms lore)
segmentedHistKernel nest perm cs hist_w ops lam arrs = do
  -- We replicate some of the checking done by 'isSegmentedOp', but
  -- things are different because a Hist is not a reduction or
  -- scan.
  (ispace, inputs) <- flatKernel nest
  let orig_pat =
        Pattern [] $
          rearrangeShape perm $
            patternValueElements $ loopNestingPattern $ fst nest

  -- The input/output arrays _must_ correspond to some kernel input,
  -- or else the original nested Hist would have been ill-typed.
  -- Find them.
  ops' <- forM ops $ \(SOACS.HistOp num_bins rf dests nes op) ->
    SOACS.HistOp num_bins rf
      <$> mapM (fmap kernelInputArray . findInput inputs) dests
      <*> pure nes
      <*> pure op

  mk_lvl <- asks distSegLevel
  scope <- askScope
  onLambda <- asks distOnSOACSLambda
  let onLambda' = fmap fst . runBinder . onLambda
  lift $
    flip runBinderT_ scope $ do
      -- It is important not to launch unnecessarily many threads for
      -- histograms, because it may mean we unnecessarily need to reduce
      -- subhistograms as well.
      lvl <- mk_lvl (hist_w : map snd ispace) "seghist" $ NoRecommendation SegNoVirt
      addStms
        =<< histKernel onLambda' lvl orig_pat ispace inputs cs hist_w ops' lam arrs
  where
    findInput kernel_inps a =
      maybe bad return $ find ((== a) . kernelInputName) kernel_inps
    bad = error "Ill-typed nested Hist encountered."

histKernel ::
  (MonadBinder m, DistLore (Lore m)) =>
  (Lambda SOACS -> m (Lambda (Lore m))) ->
  SegOpLevel (Lore m) ->
  PatternT Type ->
  [(VName, SubExp)] ->
  [KernelInput] ->
  Certificates ->
  SubExp ->
  [SOACS.HistOp SOACS] ->
  Lambda (Lore m) ->
  [VName] ->
  m (Stms (Lore m))
histKernel onLambda lvl orig_pat ispace inputs cs hist_w ops lam arrs = runBinderT'_ $ do
  ops' <- forM ops $ \(SOACS.HistOp num_bins rf dests nes op) -> do
    (op', nes', shape) <- determineReduceOp op nes
    op'' <- lift $ onLambda op'
    return $ HistOp num_bins rf dests nes' shape op''

  let isDest = flip elem $ concatMap histDest ops'
      inputs' = filter (not . isDest . kernelInputArray) inputs

  certifying cs $
    addStms =<< traverse renameStm
      =<< segHist lvl orig_pat hist_w ispace inputs' ops' lam arrs

determineReduceOp ::
  (MonadBinder m, Lore m ~ lore) =>
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
                replicate (shapeRank shape) $ DimFix $ intConst Int32 0
      return (lam', nes', shape)
    Nothing ->
      return (lam, nes, mempty)

isVectorMap :: Lambda SOACS -> (Shape, Lambda SOACS)
isVectorMap lam
  | [Let (Pattern [] pes) _ (Op (Screma w form arrs))] <-
      stmsToList $ bodyStms $ lambdaBody lam,
    bodyResult (lambdaBody lam) == map (Var . patElemName) pes,
    Just map_lam <- isMapSOAC form,
    arrs == map paramName (lambdaParams lam) =
    let (shape, lam') = isVectorMap map_lam
     in (Shape [w] <> shape, lam')
  | otherwise = (mempty, lam)

segmentedScanomapKernel ::
  (MonadFreshNames m, DistLore lore) =>
  KernelNest ->
  [Int] ->
  SubExp ->
  Lambda lore ->
  Lambda lore ->
  [SubExp] ->
  [VName] ->
  DistNestT lore m (Maybe (Stms lore))
segmentedScanomapKernel nest perm segment_size lam map_lam nes arrs = do
  mk_lvl <- asks distSegLevel
  isSegmentedOp nest perm (freeIn lam) (freeIn map_lam) nes [] $
    \pat ispace inps nes' _ -> do
      let scan_op = SegBinOp Noncommutative lam nes' mempty
      lvl <- mk_lvl (segment_size : map snd ispace) "segscan" $ NoRecommendation SegNoVirt
      addStms =<< traverse renameStm
        =<< segScan lvl pat segment_size [scan_op] map_lam arrs ispace inps

regularSegmentedRedomapKernel ::
  (MonadFreshNames m, DistLore lore) =>
  KernelNest ->
  [Int] ->
  SubExp ->
  Commutativity ->
  Lambda lore ->
  Lambda lore ->
  [SubExp] ->
  [VName] ->
  DistNestT lore m (Maybe (Stms lore))
regularSegmentedRedomapKernel nest perm segment_size comm lam map_lam nes arrs = do
  mk_lvl <- asks distSegLevel
  isSegmentedOp nest perm (freeIn lam) (freeIn map_lam) nes [] $
    \pat ispace inps nes' _ -> do
      let red_op = SegBinOp comm lam nes' mempty
      lvl <- mk_lvl (segment_size : map snd ispace) "segred" $ NoRecommendation SegNoVirt
      addStms =<< traverse renameStm
        =<< segRed lvl pat segment_size [red_op] map_lam arrs ispace inps

isSegmentedOp ::
  (MonadFreshNames m, DistLore lore) =>
  KernelNest ->
  [Int] ->
  Names ->
  Names ->
  [SubExp] ->
  [VName] ->
  ( PatternT Type ->
    [(VName, SubExp)] ->
    [KernelInput] ->
    [SubExp] ->
    [VName] ->
    BinderT lore m ()
  ) ->
  DistNestT lore m (Maybe (Stms lore))
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
      prepareNe ne = return ne

      prepareArr arr =
        case find ((== arr) . kernelInputName) kernel_inps of
          Just inp
            | kernelInputIndices inp == map Var indices ->
              return $ return $ kernelInputArray inp
          Nothing
            | not (arr `nameIn` bound_by_nest) ->
              -- This input is something that is free inside
              -- the loop nesting. We will have to replicate
              -- it.
              return $
                letExp
                  (baseString arr ++ "_repd")
                  (BasicOp $ Replicate (Shape $ map snd ispace) $ Var arr)
          _ ->
            fail "Input not free, perfectly mapped, or outermost."

  nes' <- mapM prepareNe nes

  mk_arrs <- mapM prepareArr arrs
  scope <- lift askScope

  lift $
    lift $
      flip runBinderT_ scope $ do
        nested_arrs <- sequence mk_arrs

        let pat =
              Pattern [] $
                rearrangeShape perm $
                  patternValueElements $ loopNestingPattern $ fst nest

        m pat ispace kernel_inps nes' nested_arrs

permutationAndMissing :: PatternT Type -> [SubExp] -> Maybe ([Int], [PatElemT Type])
permutationAndMissing pat res = do
  let pes = patternValueElements pat
      (_used, unused) =
        partition ((`nameIn` freeIn res) . patElemName) pes
      res_expanded = res ++ map (Var . patElemName) unused
  perm <- map (Var . patElemName) pes `isPermutationOf` res_expanded
  return (perm, unused)

-- Add extra pattern elements to every kernel nesting level.
expandKernelNest ::
  MonadFreshNames m =>
  [PatElemT Type] ->
  KernelNest ->
  m KernelNest
expandKernelNest pes (outer_nest, inner_nests) = do
  let outer_size =
        loopNestingWidth outer_nest :
        map loopNestingWidth inner_nests
      inner_sizes = tails $ map loopNestingWidth inner_nests
  outer_nest' <- expandWith outer_nest outer_size
  inner_nests' <- zipWithM expandWith inner_nests inner_sizes
  return (outer_nest', inner_nests')
  where
    expandWith nest dims = do
      pes' <- mapM (expandPatElemWith dims) pes
      return
        nest
          { loopNestingPattern =
              Pattern [] $
                patternElements (loopNestingPattern nest) <> pes'
          }

    expandPatElemWith dims pe = do
      name <- newVName $ baseString $ patElemName pe
      return
        pe
          { patElemName = name,
            patElemDec = patElemType pe `arrayOfShape` Shape dims
          }

kernelOrNot ::
  (MonadFreshNames m, DistLore lore) =>
  Certificates ->
  Stm SOACS ->
  DistAcc lore ->
  PostStms lore ->
  DistAcc lore ->
  Maybe (Stms lore) ->
  DistNestT lore m (DistAcc lore)
kernelOrNot cs bnd acc _ _ Nothing =
  addStmToAcc (certify cs bnd) acc
kernelOrNot cs _ _ kernels acc' (Just bnds) = do
  addPostStms kernels
  postStm $ fmap (certify cs) bnds
  return acc'

distributeMap ::
  (MonadFreshNames m, DistLore lore) =>
  MapLoop ->
  DistAcc lore ->
  DistNestT lore m (DistAcc lore)
distributeMap (MapLoop pat aux w lam arrs) acc =
  distribute
    =<< mapNesting
      pat
      aux
      w
      lam
      arrs
      (distribute =<< distributeMapBodyStms acc' lam_bnds)
  where
    acc' =
      DistAcc
        { distTargets =
            pushInnerTarget
              (pat, bodyResult $ lambdaBody lam)
              $ distTargets acc,
          distStms = mempty
        }

    lam_bnds = bodyStms $ lambdaBody lam
