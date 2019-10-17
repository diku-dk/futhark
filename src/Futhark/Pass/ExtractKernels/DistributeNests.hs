{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Futhark.Pass.ExtractKernels.DistributeNests
  ( KernelsStms
  , MapLoop(..)
  , mapLoopStm

  , bodyContainsParallelism
  , lambdaContainsParallelism
  , determineReduceOp
  , incrementalFlattening
  , histKernel

  , DistEnv (..)
  , DistAcc (..)
  , runDistNestT
  , DistNestT

  , distributeMap

  , distribute
  , distributeSingleStm
  , distributeMapBodyStms
  , postKernelsStms
  , addStmsToKernel
  , addStmToKernel
  , permutationAndMissing
  , addKernels
  , addKernel
  , inNesting
  )
where

import Control.Arrow (first)
import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.List

import Futhark.Representation.SOACS
import qualified Futhark.Representation.SOACS.SOAC as SOAC
import Futhark.Representation.SOACS.Simplify (simpleSOACS)
import qualified Futhark.Representation.Kernels as Out
import Futhark.Representation.Kernels.Kernel
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Transform.CopyPropagate
import Futhark.Pass.ExtractKernels.Distribution
import Futhark.Pass.ExtractKernels.ISRWIM
import Futhark.Pass.ExtractKernels.BlockedKernel hiding (segThread)
import Futhark.Pass.ExtractKernels.Interchange
import Futhark.Util
import Futhark.Util.Log

data MapLoop = MapLoop Pattern Certificates SubExp Lambda [VName]

mapLoopStm :: MapLoop -> Stm
mapLoopStm (MapLoop pat cs w lam arrs) = Let pat (StmAux cs ()) $ Op $ Screma w (mapSOAC lam) arrs

type KernelsStms = Out.Stms Out.Kernels

data DistEnv m =
  DistEnv { distNest :: Nestings
          , distScope :: Scope Out.Kernels
          , distOnTopLevelStms :: Stms SOACS -> DistNestT m (Stms Out.Kernels)
          , distOnInnerMap :: MapLoop -> DistAcc -> DistNestT m DistAcc
          , distSegLevel :: MkSegLevel m
          }

data DistAcc =
  DistAcc { distTargets :: Targets
          , distStms :: KernelsStms
          }

data DistRes =
  DistRes { accPostKernels :: PostKernels
          , accLog :: Log
          }

instance Semigroup DistRes where
  DistRes ks1 log1 <> DistRes ks2 log2 =
    DistRes (ks1 <> ks2) (log1 <> log2)

instance Monoid DistRes where
  mempty = DistRes mempty mempty

newtype PostKernel = PostKernel { unPostKernel :: KernelsStms }

newtype PostKernels = PostKernels [PostKernel]

instance Semigroup PostKernels where
  PostKernels xs <> PostKernels ys = PostKernels $ ys ++ xs

instance Monoid PostKernels where
  mempty = PostKernels mempty

postKernelsStms :: PostKernels -> KernelsStms
postKernelsStms (PostKernels kernels) = mconcat $ map unPostKernel kernels

typeEnvFromDistAcc :: DistAcc -> Scope Out.Kernels
typeEnvFromDistAcc = scopeOfPattern . fst . outerTarget . distTargets

addStmsToKernel :: KernelsStms -> DistAcc -> DistAcc
addStmsToKernel stms acc =
  acc { distStms = stms <> distStms acc }

addStmToKernel :: Monad m => Stm -> DistAcc -> m DistAcc
addStmToKernel stm acc = do
  let stm' = soacsStmToKernels stm
  return acc { distStms = oneStm stm' <> distStms acc }

newtype DistNestT m a = DistNestT (ReaderT (DistEnv m) (WriterT DistRes m) a)
  deriving (Functor, Applicative, Monad,
            MonadReader (DistEnv m),
            MonadWriter DistRes)

instance MonadTrans DistNestT where
  lift = DistNestT . lift . lift

instance MonadFreshNames m => MonadFreshNames (DistNestT m) where
  getNameSource = DistNestT $ lift getNameSource
  putNameSource = DistNestT . lift . putNameSource

instance Monad m => HasScope Out.Kernels (DistNestT m) where
  askScope = asks distScope

instance Monad m => LocalScope Out.Kernels (DistNestT m) where
  localScope types = local $ \env ->
    env { distScope = types <> distScope env }

instance Monad m => MonadLogger (DistNestT m) where
  addLog msgs = tell mempty { accLog = msgs }

runDistNestT :: MonadLogger m =>
                DistEnv m -> DistNestT m DistAcc -> m (Out.Stms Out.Kernels)
runDistNestT env (DistNestT m) = do
  (acc, res) <- runWriterT $ runReaderT m env
  addLog $ accLog res
  -- There may be a few final targets remaining - these correspond to
  -- arrays that are identity mapped, and must have statements
  -- inserted here.
  return $
    postKernelsStms (accPostKernels res) <>
    identityStms (outerTarget $ distTargets acc)
  where outermost = nestingLoop $
                    case distNest env of (nest, []) -> nest
                                         (_, nest : _) -> nest
        params_to_arrs = map (first paramName) $
                         loopNestingParamsAndArrs outermost

        identityStms (rem_pat, res) =
          stmsFromList $ zipWith identityStm (patternValueElements rem_pat) res
        identityStm pe (Var v)
          | Just arr <- lookup v params_to_arrs =
              Let (Pattern [] [pe]) (defAux ()) $ BasicOp $ Copy arr
        identityStm pe se =
          Let (Pattern [] [pe]) (defAux ()) $ BasicOp $
          Replicate (Shape [loopNestingWidth outermost]) se

addKernels :: Monad m => PostKernels -> DistNestT m ()
addKernels ks = tell $ mempty { accPostKernels = ks }

addKernel :: Monad m => KernelsStms -> DistNestT m ()
addKernel bnds = addKernels $ PostKernels [PostKernel bnds]

withStm :: Monad m => Stm -> DistNestT m a -> DistNestT m a
withStm stm = local $ \env ->
  env { distScope =
          scopeForKernels (scopeOf stm) <> distScope env
      , distNest =
          letBindInInnerNesting provided $
          distNest env
      }
  where provided = namesFromList $ patternNames $ stmPattern stm

mapNesting :: Monad m =>
              Pattern -> Certificates -> SubExp -> Lambda -> [VName]
           -> DistNestT m a
           -> DistNestT m a
mapNesting pat cs w lam arrs = local $ \env ->
  env { distNest = pushInnerNesting nest $ distNest env
      , distScope =  scopeForKernels (scopeOf lam) <> distScope env
      }
  where nest = Nesting mempty $
               MapNesting pat cs w $
               zip (lambdaParams lam) arrs

inNesting :: Monad m =>
             KernelNest -> DistNestT m a -> DistNestT m a
inNesting (outer, nests) = local $ \env ->
  env { distNest = (inner, nests')
      , distScope =  mconcat (map scopeOf $ outer : nests) <> distScope env
      }
  where (inner, nests') =
          case reverse nests of
            []           -> (asNesting outer, [])
            (inner' : ns) -> (asNesting inner', map asNesting $ outer : reverse ns)
        asNesting = Nesting mempty

bodyContainsParallelism :: Body -> Bool
bodyContainsParallelism = any (isMap . stmExp) . bodyStms
  where isMap Op{} = True
        isMap _ = False

lambdaContainsParallelism :: Lambda -> Bool
lambdaContainsParallelism = bodyContainsParallelism . lambdaBody

-- Enable if you want the cool new versioned code.  Beware: may be
-- slower in practice.  Caveat emptor (and you are the emptor).
incrementalFlattening :: Bool
incrementalFlattening = isJust $ lookup "FUTHARK_INCREMENTAL_FLATTENING" unixEnvironment

leavingNesting :: Monad m => MapLoop -> DistAcc -> DistNestT m DistAcc
leavingNesting (MapLoop _ cs w lam arrs) acc =
  case popInnerTarget $ distTargets acc of
   Nothing ->
     error "The kernel targets list is unexpectedly small"
   Just ((pat,res), newtargets) -> do
     let acc' = acc { distTargets = newtargets }
     if null $ distStms acc'
       then return acc'
       else do let body = Body () (distStms acc') res
                   used_in_body = freeIn body
                   (used_params, used_arrs) =
                     unzip $
                     filter ((`nameIn` used_in_body) . paramName . fst) $
                     zip (lambdaParams lam) arrs
                   lam' = Lambda { lambdaParams = used_params
                                 , lambdaBody = body
                                 , lambdaReturnType = map rowType $ patternTypes pat
                                 }
               let stms = oneStm $ Let pat (StmAux cs ()) $ Op $
                          OtherOp $ Screma w (mapSOAC lam') used_arrs
               return $ addStmsToKernel stms acc' { distStms = mempty }

distributeMapBodyStms :: MonadFreshNames m => DistAcc -> Stms SOACS -> DistNestT m DistAcc
distributeMapBodyStms orig_acc = distribute <=< onStms orig_acc . stmsToList
  where
    onStms acc [] = return acc

    onStms acc (Let pat (StmAux cs _) (Op (Stream w (Sequential accs) lam arrs)):stms) = do
      types <- asksScope scopeForSOACs
      stream_stms <-
        snd <$> runBinderT (sequentialStreamWholeArray pat w accs lam arrs) types
      stream_stms' <-
        runReaderT (copyPropagateInStms simpleSOACS stream_stms) types
      onStms acc $ stmsToList (fmap (certify cs) stream_stms') ++ stms

    onStms acc (stm:stms) =
      -- It is important that stm is in scope if 'maybeDistributeStm'
      -- wants to distribute, even if this causes the slightly silly
      -- situation that stm is in scope of itself.
      withStm stm $ maybeDistributeStm stm =<< onStms acc stms

onInnerMap :: Monad m => MapLoop -> DistAcc -> DistNestT m DistAcc
onInnerMap loop acc = do
  f <- asks distOnInnerMap
  f loop acc

onTopLevelStms :: Monad m => Stms SOACS -> DistNestT m ()
onTopLevelStms stms = do
  f <- asks distOnTopLevelStms
  addKernel =<< f stms

maybeDistributeStm :: MonadFreshNames m => Stm -> DistAcc -> DistNestT m DistAcc

maybeDistributeStm bnd@(Let pat _ (Op (Screma w form arrs))) acc
  | Just lam <- isMapSOAC form =
  -- Only distribute inside the map if we can distribute everything
  -- following the map.
  distributeIfPossible acc >>= \case
    Nothing -> addStmToKernel bnd acc
    Just acc' -> distribute =<< onInnerMap (MapLoop pat (stmCerts bnd) w lam arrs) acc'

maybeDistributeStm bnd@(Let pat _ (DoLoop [] val form@ForLoop{} body)) acc
  | null (patternContextElements pat), bodyContainsParallelism body =
  distributeSingleStm acc bnd >>= \case
    Just (kernels, res, nest, acc')
      | not $ freeIn form `namesIntersect` boundInKernelNest nest,
        Just (perm, pat_unused) <- permutationAndMissing pat res ->
          -- We need to pretend pat_unused was used anyway, by adding
          -- it to the kernel nest.
          localScope (typeEnvFromDistAcc acc') $ do
          addKernels kernels
          nest' <- expandKernelNest pat_unused nest
          types <- asksScope scopeForSOACs

          bnds <- runReaderT
                  (interchangeLoops nest' (SeqLoop perm pat val form body)) types
          onTopLevelStms bnds
          return acc'
    _ ->
      addStmToKernel bnd acc

maybeDistributeStm stm@(Let pat _ (If cond tbranch fbranch ret)) acc
  | null (patternContextElements pat),
    bodyContainsParallelism tbranch || bodyContainsParallelism fbranch ||
    any (not . primType) (ifReturns ret) =
    distributeSingleStm acc stm >>= \case
      Just (kernels, res, nest, acc')
        | not $
          (freeIn cond <> freeIn ret) `namesIntersect` boundInKernelNest nest,
          Just (perm, pat_unused) <- permutationAndMissing pat res ->
            -- We need to pretend pat_unused was used anyway, by adding
            -- it to the kernel nest.
            localScope (typeEnvFromDistAcc acc') $ do
            nest' <- expandKernelNest pat_unused nest
            addKernels kernels
            types <- asksScope scopeForSOACs
            let branch = Branch perm pat cond tbranch fbranch ret
            stms <- runReaderT (interchangeBranch nest' branch) types
            onTopLevelStms stms
            return acc'
      _ ->
        addStmToKernel stm acc

maybeDistributeStm (Let pat (StmAux cs _) (Op (Screma w form arrs))) acc
  | Just [Reduce comm lam nes] <- isReduceSOAC form,
    Just m <- irwim pat w comm lam $ zip nes arrs = do
      types <- asksScope scopeForSOACs
      (_, bnds) <- runBinderT (certifying cs m) types
      distributeMapBodyStms acc bnds

-- Parallelise segmented scatters.
maybeDistributeStm bnd@(Let pat (StmAux cs _) (Op (Scatter w lam ivs as))) acc =
  distributeSingleStm acc bnd >>= \case
    Just (kernels, res, nest, acc')
      | Just (perm, pat_unused) <- permutationAndMissing pat res ->
        localScope (typeEnvFromDistAcc acc') $ do
          nest' <- expandKernelNest pat_unused nest
          let lam' = soacsLambdaToKernels lam
          addKernels kernels
          addKernel =<< segmentedScatterKernel nest' perm pat cs w lam' ivs as
          return acc'
    _ ->
      addStmToKernel bnd acc

-- Parallelise segmented Hist.
maybeDistributeStm bnd@(Let pat (StmAux cs _) (Op (Hist w ops lam as))) acc =
  distributeSingleStm acc bnd >>= \case
    Just (kernels, res, nest, acc')
      | Just (perm, pat_unused) <- permutationAndMissing pat res ->
        localScope (typeEnvFromDistAcc acc') $ do
          let lam' = soacsLambdaToKernels lam
          nest' <- expandKernelNest pat_unused nest
          addKernels kernels
          addKernel =<< segmentedHistKernel nest' perm cs w ops lam' as
          return acc'
    _ ->
      addStmToKernel bnd acc

-- If the scan can be distributed by itself, we will turn it into a
-- segmented scan.
--
-- If the scan cannot be distributed by itself, it will be
-- sequentialised in the default case for this function.
maybeDistributeStm bnd@(Let pat (StmAux cs _) (Op (Screma w form arrs))) acc
  | Just (lam, nes, map_lam) <- isScanomapSOAC form =
  distributeSingleStm acc bnd >>= \case
    Just (kernels, res, nest, acc')
      | Just (perm, pat_unused) <- permutationAndMissing pat res ->
          -- We need to pretend pat_unused was used anyway, by adding
          -- it to the kernel nest.
          localScope (typeEnvFromDistAcc acc') $ do
          nest' <- expandKernelNest pat_unused nest
          let map_lam' = soacsLambdaToKernels map_lam
              lam' = soacsLambdaToKernels lam
          localScope (typeEnvFromDistAcc acc') $
            segmentedScanomapKernel nest' perm w lam' map_lam' nes arrs >>=
            kernelOrNot cs bnd acc kernels acc'
    _ ->
      addStmToKernel bnd acc

-- if the reduction can be distributed by itself, we will turn it into a
-- segmented reduce.
--
-- If the reduction cannot be distributed by itself, it will be
-- sequentialised in the default case for this function.
maybeDistributeStm bnd@(Let pat (StmAux cs _) (Op (Screma w form arrs))) acc
  | Just (reds, map_lam) <- isRedomapSOAC form,
    Reduce comm lam nes <- singleReduce reds,
    isIdentityLambda map_lam || incrementalFlattening =
  distributeSingleStm acc bnd >>= \case
    Just (kernels, res, nest, acc')
      | Just (perm, pat_unused) <- permutationAndMissing pat res ->
          -- We need to pretend pat_unused was used anyway, by adding
          -- it to the kernel nest.
          localScope (typeEnvFromDistAcc acc') $ do
          nest' <- expandKernelNest pat_unused nest
          let lam' = soacsLambdaToKernels lam
              map_lam' = soacsLambdaToKernels map_lam

          let comm' | commutativeLambda lam = Commutative
                    | otherwise             = comm

          regularSegmentedRedomapKernel nest' perm w comm' lam' map_lam' nes arrs >>=
            kernelOrNot cs bnd acc kernels acc'
    _ ->
      addStmToKernel bnd acc

maybeDistributeStm (Let pat (StmAux cs _) (Op (Screma w form arrs))) acc
  | incrementalFlattening || isNothing (isRedomapSOAC form) = do
  -- This with-loop is too complicated for us to immediately do
  -- anything, so split it up and try again.
  scope <- asksScope scopeForSOACs
  distributeMapBodyStms acc . fmap (certify cs) . snd =<<
    runBinderT (dissectScrema pat w form arrs) scope

maybeDistributeStm (Let pat aux (BasicOp (Replicate (Shape (d:ds)) v))) acc
  | [t] <- patternTypes pat = do
      -- XXX: We need a temporary dummy binding to prevent an empty
      -- map body.  The kernel extractor does not like empty map
      -- bodies.
      tmp <- newVName "tmp"
      let rowt = rowType t
          newbnd = Let pat aux $ Op $ Screma d (mapSOAC lam) []
          tmpbnd = Let (Pattern [] [PatElem tmp rowt]) aux $
                   BasicOp $ Replicate (Shape ds) v
          lam = Lambda { lambdaReturnType = [rowt]
                       , lambdaParams = []
                       , lambdaBody = mkBody (oneStm tmpbnd) [Var tmp]
                       }
      maybeDistributeStm newbnd acc

maybeDistributeStm bnd@(Let _ aux (BasicOp Copy{})) acc =
  distributeSingleUnaryStm acc bnd $ \_ outerpat arr ->
  return $ oneStm $ Let outerpat aux $ BasicOp $ Copy arr

-- Opaques are applied to the full array, because otherwise they can
-- drastically inhibit parallelisation in some cases.
maybeDistributeStm bnd@(Let (Pattern [] [pe]) aux (BasicOp Opaque{})) acc
  | not $ primType $ typeOf pe =
      distributeSingleUnaryStm acc bnd $ \_ outerpat arr ->
      return $ oneStm $ Let outerpat aux $ BasicOp $ Copy arr

maybeDistributeStm bnd@(Let _ aux (BasicOp (Rearrange perm _))) acc =
  distributeSingleUnaryStm acc bnd $ \nest outerpat arr -> do
    let r = length (snd nest) + 1
        perm' = [0..r-1] ++ map (+r) perm
    -- We need to add a copy, because the original map nest
    -- will have produced an array without aliases, and so must we.
    arr' <- newVName $ baseString arr
    arr_t <- lookupType arr
    return $ stmsFromList
      [Let (Pattern [] [PatElem arr' arr_t]) aux $ BasicOp $ Copy arr,
       Let outerpat aux $ BasicOp $ Rearrange perm' arr']

maybeDistributeStm bnd@(Let _ aux (BasicOp (Reshape reshape _))) acc =
  distributeSingleUnaryStm acc bnd $ \nest outerpat arr -> do
    let reshape' = map DimNew (kernelNestWidths nest) ++
                   map DimNew (newDims reshape)
    return $ oneStm $ Let outerpat aux $ BasicOp $ Reshape reshape' arr

maybeDistributeStm stm@(Let _ aux (BasicOp (Rotate rots _))) acc =
  distributeSingleUnaryStm acc stm $ \nest outerpat arr -> do
    let rots' = map (const $ intConst Int32 0) (kernelNestWidths nest) ++ rots
    return $ oneStm $ Let outerpat aux $ BasicOp $ Rotate rots' arr

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
          lam = Lambda { lambdaParams = []
                       , lambdaReturnType = [Prim int32, et]
                       , lambdaBody = mkBody mempty [i, v] }
      maybeDistributeStm (Let pat aux $ Op $ Scatter (intConst Int32 1) lam [] [(w, 1, arr)]) acc
  where amortises DoLoop{} = True
        amortises Op{} = True
        amortises _ = False

maybeDistributeStm stm@(Let _ aux (BasicOp (Concat d x xs w))) acc =
  distributeSingleStm acc stm >>= \case
    Just (kernels, _, nest, acc') ->
      localScope (typeEnvFromDistAcc acc') $
      segmentedConcat nest >>=
      kernelOrNot (stmAuxCerts aux) stm acc kernels acc'
    _ ->
      addStmToKernel stm acc

  where segmentedConcat nest =
          isSegmentedOp nest [0] w mempty mempty [] (x:xs) $
          \pat _ _ _ (x':xs') _ ->
            let d' = d + length (snd nest) + 1
            in addStm $ Let pat aux $ BasicOp $ Concat d' x' xs' w

maybeDistributeStm bnd acc =
  addStmToKernel bnd acc

distributeSingleUnaryStm :: MonadFreshNames m =>
                            DistAcc -> Stm
                         -> (KernelNest -> Pattern -> VName -> DistNestT m (Stms Out.Kernels))
                         -> DistNestT m DistAcc
distributeSingleUnaryStm acc bnd f =
  distributeSingleStm acc bnd >>= \case
    Just (kernels, res, nest, acc')
      | res == map Var (patternNames $ stmPattern bnd),
        (outer, inners) <- nest,
        [(arr_p, arr)] <- loopNestingParamsAndArrs outer,
        boundInKernelNest nest `namesIntersection` freeIn bnd
        == oneName (paramName arr_p) -> do
          addKernels kernels
          let outerpat = loopNestingPattern $ fst nest
          localScope (typeEnvFromDistAcc acc') $ do
            (arr', pre_stms) <- repeatMissing arr (outer:inners)
            f_stms <- inScopeOf pre_stms $ f nest outerpat arr'
            addKernel $ pre_stms <> f_stms
            return acc'
    _ -> addStmToKernel bnd acc
  where -- | For an imperfectly mapped array, repeat the missing
        -- dimensions to make it look like it was in fact perfectly
        -- mapped.
        repeatMissing arr inners = do
          arr_t <- lookupType arr
          let shapes = determineRepeats arr arr_t inners
          if all (==Shape []) shapes then return (arr, mempty)
            else do
            let (outer_shapes, inner_shape) = repeatShapes shapes arr_t
                arr_t' = repeatDims outer_shapes inner_shape arr_t
            arr' <- newVName $ baseString arr
            return (arr', oneStm $ Let (Pattern [] [PatElem arr' arr_t']) (defAux ()) $
                          BasicOp $ Repeat outer_shapes inner_shape arr)

        determineRepeats arr arr_t nests
          | (skipped, arr_nest:nests') <- break (hasInput arr) nests,
            [(arr_p, _)] <- loopNestingParamsAndArrs arr_nest =
              Shape (map loopNestingWidth skipped) :
              determineRepeats (paramName arr_p) (rowType arr_t) nests'
          | otherwise =
              Shape (map loopNestingWidth nests) : replicate (arrayRank arr_t) (Shape [])

        hasInput arr nest
          | [(_, arr')] <- loopNestingParamsAndArrs nest, arr' == arr = True
          | otherwise = False


distribute :: MonadFreshNames m => DistAcc -> DistNestT m DistAcc
distribute acc =
  fromMaybe acc <$> distributeIfPossible acc

mkSegLevel :: MonadFreshNames m => DistNestT m (MkSegLevel (DistNestT m))
mkSegLevel = do
  mk_lvl <- asks distSegLevel
  return $ \w desc r -> do
    scope <- askScope
    (lvl, stms) <- lift $ lift $ runBinderT (mk_lvl w desc r) scope
    addStms stms
    return lvl

distributeIfPossible :: MonadFreshNames m => DistAcc -> DistNestT m (Maybe DistAcc)
distributeIfPossible acc = do
  nest <- asks distNest
  mk_lvl <- mkSegLevel
  tryDistribute mk_lvl nest (distTargets acc) (distStms acc) >>= \case
    Nothing -> return Nothing
    Just (targets, kernel) -> do
      addKernel kernel
      return $ Just DistAcc { distTargets = targets
                            , distStms = mempty
                            }

distributeSingleStm :: MonadFreshNames m =>
                       DistAcc -> Stm
                    -> DistNestT m (Maybe (PostKernels, Result, KernelNest, DistAcc))
distributeSingleStm acc bnd = do
  nest <- asks distNest
  mk_lvl <- mkSegLevel
  tryDistribute mk_lvl nest (distTargets acc) (distStms acc) >>= \case
    Nothing -> return Nothing
    Just (targets, distributed_bnds) ->
      tryDistributeStm nest targets bnd >>= \case
        Nothing -> return Nothing
        Just (res, targets', new_kernel_nest) ->
          return $ Just (PostKernels [PostKernel distributed_bnds],
                         res,
                         new_kernel_nest,
                         DistAcc { distTargets = targets'
                                 , distStms = mempty
                                 })

segmentedScatterKernel :: MonadFreshNames m =>
                          KernelNest
                       -> [Int]
                       -> Pattern
                       -> Certificates
                       -> SubExp
                       -> Out.Lambda Out.Kernels
                       -> [VName] -> [(SubExp,Int,VName)]
                       -> DistNestT m KernelsStms
segmentedScatterKernel nest perm scatter_pat cs scatter_w lam ivs dests = do
  -- We replicate some of the checking done by 'isSegmentedOp', but
  -- things are different because a scatter is not a reduction or
  -- scan.
  --
  -- First, pretend that the scatter is also part of the nesting.  The
  -- KernelNest we produce here is technically not sensible, but it's
  -- good enough for flatKernel to work.
  let nest' = pushInnerKernelNesting (scatter_pat, bodyResult $ lambdaBody lam)
              (MapNesting scatter_pat cs scatter_w $ zip (lambdaParams lam) ivs) nest
  (ispace, kernel_inps) <- flatKernel nest'

  let (as_ws, as_ns, as) = unzip3 dests

  -- The input/output arrays ('as') _must_ correspond to some kernel
  -- input, or else the original nested scatter would have been
  -- ill-typed.  Find them.
  as_inps <- mapM (findInput kernel_inps) as

  mk_lvl <- mkSegLevel

  let rts = concatMap (take 1) $ chunks as_ns $
            drop (sum as_ns) $ lambdaReturnType lam
      (is,vs) = splitAt (sum as_ns) $ bodyResult $ lambdaBody lam
      k_body = KernelBody () (bodyStms $ lambdaBody lam) $
               map (inPlaceReturn ispace) $
               zip3 as_ws as_inps $ chunks as_ns $ zip is vs

  (k, k_bnds) <- mapKernel mk_lvl ispace kernel_inps rts k_body

  runBinder_ $ do
    addStms k_bnds

    let pat = Pattern [] $ rearrangeShape perm $
              patternValueElements $ loopNestingPattern $ fst nest

    certifying cs $ letBind_ pat $ Op $ SegOp k
  where findInput kernel_inps a =
          maybe bad return $ find ((==a) . kernelInputName) kernel_inps
        bad = error "Ill-typed nested scatter encountered."

        inPlaceReturn ispace (aw, inp, is_vs) =
          WriteReturns (init ws++[aw]) (kernelInputArray inp)
          [ (map Var (init gtids)++[i], v) | (i,v) <- is_vs ]
          where (gtids,ws) = unzip ispace

segmentedHistKernel :: MonadFreshNames m =>
                            KernelNest
                         -> [Int]
                         -> Certificates
                         -> SubExp
                         -> [SOAC.HistOp SOACS]
                         -> Out.Lambda Out.Kernels
                         -> [VName]
                         -> DistNestT m KernelsStms
segmentedHistKernel nest perm cs hist_w ops lam arrs = do
  -- We replicate some of the checking done by 'isSegmentedOp', but
  -- things are different because a Hist is not a reduction or
  -- scan.
  (ispace, inputs) <- flatKernel nest
  let orig_pat = Pattern [] $ rearrangeShape perm $
                 patternValueElements $ loopNestingPattern $ fst nest

  -- The input/output arrays _must_ correspond to some kernel input,
  -- or else the original nested Hist would have been ill-typed.
  -- Find them.
  ops' <- forM ops $ \(SOAC.HistOp num_bins rf dests nes op) ->
    SOAC.HistOp num_bins rf
    <$> mapM (fmap kernelInputArray . findInput inputs) dests
    <*> pure nes
    <*> pure op

  mk_lvl <- asks distSegLevel
  scope <- askScope
  lift $ flip runBinderT_ scope $ do
    -- It is important not to launch unnecessarily many threads for
    -- histograms, because it may mean we unnecessarily need to reduce
    -- subhistograms as well.
    lvl <- mk_lvl (hist_w : map snd ispace) "seghist" $ NoRecommendation SegNoVirt
    addStms =<<
      histKernel lvl orig_pat ispace inputs cs hist_w ops' lam arrs
  where findInput kernel_inps a =
          maybe bad return $ find ((==a) . kernelInputName) kernel_inps
        bad = error "Ill-typed nested Hist encountered."

histKernel :: (MonadFreshNames m, HasScope Out.Kernels m) =>
                   SegLevel -> Pattern -> [(VName, SubExp)] -> [KernelInput]
                -> Certificates -> SubExp -> [SOAC.HistOp SOACS]
                -> Out.Lambda Out.Kernels -> [VName]
                -> m KernelsStms
histKernel lvl orig_pat ispace inputs cs hist_w ops lam arrs =
  runBinder_ $ do
    ops' <- forM ops $ \(SOAC.HistOp num_bins rf dests nes op) -> do
      (op', nes', shape) <- determineReduceOp op nes
      return $ Out.HistOp num_bins rf dests nes' shape op'

    let isDest = flip elem $ concatMap Out.histDest ops'
        inputs' = filter (not . isDest . kernelInputArray) inputs

    certifying cs $
      addStms =<< traverse renameStm =<<
      segHist lvl orig_pat hist_w ispace inputs' ops' lam arrs

determineReduceOp :: (MonadBinder m, Lore m ~ Out.Kernels) =>
                     Lambda -> [SubExp] -> m (Out.Lambda Out.Kernels, [SubExp], Shape)
determineReduceOp lam nes =
  -- FIXME? We are assuming that the accumulator is a replicate, and
  -- we fish out its value in a gross way.
  case mapM subExpVar nes of
    Just ne_vs' -> do
      let (shape, lam') = isVectorMap lam
      nes' <- forM ne_vs' $ \ne_v -> do
        ne_v_t <- lookupType ne_v
        letSubExp "hist_ne" $
          BasicOp $ Index ne_v $ fullSlice ne_v_t $
          replicate (shapeRank shape) $ DimFix $ intConst Int32 0
      let lam'' = soacsLambdaToKernels lam'
      return (lam'', nes', shape)
    Nothing -> do
      let lam' = soacsLambdaToKernels lam
      return (lam', nes, mempty)

isVectorMap :: Lambda -> (Shape, Lambda)
isVectorMap lam
  | [Let (Pattern [] pes) _ (Op (Screma w form arrs))] <-
      stmsToList $ bodyStms $ lambdaBody lam,
    bodyResult (lambdaBody lam) == map (Var . patElemName) pes,
    Just map_lam <- isMapSOAC form,
    arrs == map paramName (lambdaParams lam) =
      let (shape, lam') = isVectorMap map_lam
      in (Shape [w] <> shape, lam')
  | otherwise = (mempty, lam)

segmentedScanomapKernel :: MonadFreshNames m =>
                           KernelNest
                        -> [Int]
                        -> SubExp
                        -> Out.Lambda Out.Kernels -> Out.Lambda Out.Kernels
                        -> [SubExp] -> [VName]
                        -> DistNestT m (Maybe KernelsStms)
segmentedScanomapKernel nest perm segment_size lam map_lam nes arrs = do
  mk_lvl <- asks distSegLevel
  isSegmentedOp nest perm segment_size (freeIn lam) (freeIn map_lam) nes arrs $
    \pat ispace inps nes' _ _ -> do
    lvl <- mk_lvl (segment_size : map snd ispace) "segscan" $ NoRecommendation SegNoVirt
    addStms =<< traverse renameStm =<<
      segScan lvl pat segment_size lam map_lam nes' arrs ispace inps

regularSegmentedRedomapKernel :: MonadFreshNames m =>
                                 KernelNest
                              -> [Int]
                              -> SubExp -> Commutativity
                              -> Out.Lambda Out.Kernels -> Out.Lambda Out.Kernels
                              -> [SubExp] -> [VName]
                              -> DistNestT m (Maybe KernelsStms)
regularSegmentedRedomapKernel nest perm segment_size comm lam map_lam nes arrs = do
  mk_lvl <- asks distSegLevel
  isSegmentedOp nest perm segment_size (freeIn lam) (freeIn map_lam) nes arrs $
    \pat ispace inps nes' _ _ -> do
      let red_op = SegRedOp comm lam nes' mempty
      lvl <- mk_lvl (segment_size : map snd ispace) "segred" $ NoRecommendation SegNoVirt
      addStms =<< traverse renameStm =<<
        segRed lvl pat segment_size [red_op] map_lam arrs ispace inps

isSegmentedOp :: MonadFreshNames m =>
                 KernelNest
              -> [Int]
              -> SubExp
              -> Names -> Names
              -> [SubExp] -> [VName]
              -> (Pattern
                  -> [(VName, SubExp)]
                  -> [KernelInput]
                  -> [SubExp] -> [VName]  -> [VName]
                  -> BinderT Out.Kernels m ())
              -> DistNestT m (Maybe KernelsStms)
isSegmentedOp nest perm segment_size free_in_op _free_in_fold_op nes arrs m = runMaybeT $ do
  -- We must verify that array inputs to the operation are inputs to
  -- the outermost loop nesting or free in the loop nest.  Nothing
  -- free in the op may be bound by the nest.  Furthermore, the
  -- neutral elements must be free in the loop nest.
  --
  -- We must summarise any names from free_in_op that are bound in the
  -- nest, and describe how to obtain them given segment indices.

  let bound_by_nest = boundInKernelNest nest

  (ispace, kernel_inps) <- flatKernel nest

  unless (not $ free_in_op `namesIntersect` bound_by_nest) $
    fail "Non-fold lambda uses nest-bound parameters."

  let indices = map fst ispace

      prepareNe (Var v) | v `nameIn` bound_by_nest =
                            fail "Neutral element bound in nest"
      prepareNe ne = return ne

      prepareArr arr =
        case find ((==arr) . kernelInputName) kernel_inps of
          Just inp
            | kernelInputIndices inp == map Var indices ->
                return $ return $ kernelInputArray inp
            | not (kernelInputArray inp `nameIn` bound_by_nest) ->
                return $ replicateMissing ispace inp
          Nothing | not (arr `nameIn` bound_by_nest) ->
                      -- This input is something that is free inside
                      -- the loop nesting. We will have to replicate
                      -- it.
                      return $
                      letExp (baseString arr ++ "_repd")
                      (BasicOp $ Replicate (Shape $ map snd ispace) $ Var arr)
          _ ->
            fail "Input not free or outermost."

  nes' <- mapM prepareNe nes

  mk_arrs <- mapM prepareArr arrs
  scope <- lift askScope

  lift $ lift $ flip runBinderT_ scope $ do
    -- We must make sure all inputs are of size
    -- segment_size*nesting_size.
    total_num_elements <-
      letSubExp "total_num_elements" =<<
      foldBinOp (Mul Int32) segment_size (map snd ispace)

    let flatten arr = do
          arr_shape <- arrayShape <$> lookupType arr
          -- CHECKME: is the length the right thing here?  We want to
          -- reproduce the parameter type.
          let reshape = reshapeOuter [DimNew total_num_elements]
                        (2+length (snd nest)) arr_shape
          letExp (baseString arr ++ "_flat") $
            BasicOp $ Reshape reshape arr

    nested_arrs <- sequence mk_arrs
    arrs' <- mapM flatten nested_arrs

    let pat = Pattern [] $ rearrangeShape perm $
              patternValueElements $ loopNestingPattern $ fst nest

    m pat ispace kernel_inps nes' nested_arrs arrs'

  where replicateMissing ispace inp = do
          t <- lookupType $ kernelInputArray inp
          let inp_is = kernelInputIndices inp
              shapes = determineRepeats ispace inp_is
              (outer_shapes, inner_shape) = repeatShapes shapes t
          letExp "repeated" $ BasicOp $
            Repeat outer_shapes inner_shape $ kernelInputArray inp

        determineRepeats ispace (i:is)
          | (skipped_ispace, ispace') <- span ((/=i) . Var . fst) ispace =
              Shape (map snd skipped_ispace) : determineRepeats (drop 1 ispace') is
        determineRepeats ispace _ =
          [Shape $ map snd ispace]

permutationAndMissing :: Pattern -> [SubExp] -> Maybe ([Int], [PatElem])
permutationAndMissing pat res = do
  let pes = patternValueElements pat
      (_used,unused) =
        partition ((`nameIn` freeIn res) . patElemName) pes
      res_expanded = res ++ map (Var . patElemName) unused
  perm <- map (Var . patElemName) pes `isPermutationOf` res_expanded
  return (perm, unused)

-- Add extra pattern elements to every kernel nesting level.
expandKernelNest :: MonadFreshNames m =>
                    [PatElem] -> KernelNest -> m KernelNest
expandKernelNest pes (outer_nest, inner_nests) = do
  let outer_size = loopNestingWidth outer_nest :
                   map loopNestingWidth inner_nests
      inner_sizes = tails $ map loopNestingWidth inner_nests
  outer_nest' <- expandWith outer_nest outer_size
  inner_nests' <- zipWithM expandWith inner_nests inner_sizes
  return (outer_nest', inner_nests')
  where expandWith nest dims = do
           pes' <- mapM (expandPatElemWith dims) pes
           return nest { loopNestingPattern =
                           Pattern [] $
                           patternElements (loopNestingPattern nest) <> pes'
                       }

        expandPatElemWith dims pe = do
          name <- newVName $ baseString $ patElemName pe
          return pe { patElemName = name
                    , patElemAttr = patElemType pe `arrayOfShape` Shape dims
                    }

kernelOrNot :: MonadFreshNames m =>
               Certificates -> Stm -> DistAcc
            -> PostKernels -> DistAcc -> Maybe KernelsStms
            -> DistNestT m DistAcc
kernelOrNot cs bnd acc _ _ Nothing =
  addStmToKernel (certify cs bnd) acc
kernelOrNot cs _ _ kernels acc' (Just bnds) = do
  addKernels kernels
  addKernel $ fmap (certify cs) bnds
  return acc'

distributeMap :: MonadFreshNames m => MapLoop -> DistAcc -> DistNestT m DistAcc
distributeMap maploop@(MapLoop pat cs w lam arrs) acc =
  distribute =<<
  leavingNesting maploop =<<
  mapNesting pat cs w lam arrs
  (distribute =<< distributeMapBodyStms acc' lam_bnds)

  where acc' = DistAcc { distTargets = pushInnerTarget
                                       (pat, bodyResult $ lambdaBody lam) $
                                       distTargets acc
                       , distStms = mempty
                       }

        lam_bnds = bodyStms $ lambdaBody lam
