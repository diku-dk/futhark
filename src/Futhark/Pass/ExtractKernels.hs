{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Extract kernels.
-- In the following, I will use the term "width" to denote the amount
-- of immediate parallelism in a map - that is, the row size of the
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
-- This is not ideal, as we cannot flatten the @map-loop@ nest, and we
-- are thus limited in the amount of parallelism available.
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
-- Redomap is handled much like map.  Distributed loops are
-- distributed as maps, with the parameters corresponding to the
-- neutral elements added to their bodies.  The remaining loop will
-- remain a redomap.  Example:
--
-- @
-- redomap(op,
--         fn (acc,v) =>
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
--         fn (acc,v,dist) =>
--           map(g),
--         e,a,b)
-- @
--
module Futhark.Pass.ExtractKernels
       (extractKernels)
       where

import Control.Arrow (second)
import Control.Applicative
import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe
import Data.List

import Prelude

import Futhark.Optimise.Simplifier.Simple (bindableSimpleOps)
import Futhark.Representation.SOACS
import Futhark.Representation.SOACS.Simplify (simplifyBindings)
import qualified Futhark.Representation.Kernels as Out
import Futhark.Representation.Kernels.Kernel
import Futhark.MonadFreshNames
import Futhark.Tools
import qualified Futhark.Transform.FirstOrderTransform as FOT
import Futhark.Transform.Rename
import Futhark.Pass
import Futhark.Transform.CopyPropagate
import Futhark.Pass.ExtractKernels.Distribution
import Futhark.Pass.ExtractKernels.ISRWIM
import Futhark.Pass.ExtractKernels.BlockedKernel
import Futhark.Pass.ExtractKernels.SegmentedReduce
import Futhark.Pass.ExtractKernels.Interchange
import qualified Futhark.Pass.ExtractKernels.Kernelise as Kernelise
import Futhark.Util.Log

extractKernels :: Pass SOACS Out.Kernels
extractKernels =
  Pass { passName = "extract kernels"
       , passDescription = "Perform kernel extraction"
       , passFunction = runDistribM . fmap Prog . mapM transformFunDef . progFunctions
       }

newtype DistribM a = DistribM (RWS (Scope Out.Kernels) Log VNameSource a)
                   deriving (Functor, Applicative, Monad,
                             HasScope Out.Kernels,
                             LocalScope Out.Kernels,
                             MonadFreshNames,
                             MonadLogger)

runDistribM :: (MonadLogger m, MonadFreshNames m) =>
               DistribM a -> m a
runDistribM (DistribM m) = do
  (x, msgs) <- modifyNameSource $ positionNameSource . runRWS m HM.empty
  addLog msgs
  return x
  where positionNameSource (x, src, msgs) = ((x, msgs), src)

transformFunDef :: FunDef -> DistribM Out.FunDef
transformFunDef (FunDef entry name rettype params body) = do
  body' <- localScope (scopeOfFParams params) $
           transformBody body
  return $ FunDef entry name rettype params body'

transformBody :: Body -> DistribM Out.Body
transformBody body = do bnds <- transformBindings $ bodyBindings body
                        return $ mkBody bnds $ bodyResult body

transformBindings :: [Binding] -> DistribM [Out.Binding]
transformBindings [] =
  return []
transformBindings (bnd:bnds) =
  sequentialisedUnbalancedBinding bnd >>= \case
    Nothing -> do
      bnd' <- transformBinding bnd
      inScopeOf bnd' $
        (bnd'++) <$> transformBindings bnds
    Just bnds' ->
      transformBindings $ bnds' <> bnds

sequentialisedUnbalancedBinding :: Binding -> DistribM (Maybe [Binding])
sequentialisedUnbalancedBinding (Let pat _ (Op soac@(Map _ _ lam _)))
  | unbalancedLambda lam, lambdaContainsParallelism lam = do
      types <- asksScope scopeForSOACs
      Just . snd <$> runBinderT (FOT.transformSOAC pat soac) types
sequentialisedUnbalancedBinding (Let pat _ (Op soac@(Redomap _ _ _ _ lam2 _ _)))
  | unbalancedLambda lam2, lambdaContainsParallelism lam2 = do
      types <- asksScope scopeForSOACs
      Just . snd <$> runBinderT (FOT.transformSOAC pat soac) types
sequentialisedUnbalancedBinding _ =
  return Nothing

scopeForSOACs :: Scope Out.Kernels -> Scope SOACS
scopeForSOACs = castScope

scopeForKernels :: Scope SOACS -> Scope Out.Kernels
scopeForKernels = castScope

transformBinding :: Binding -> DistribM [Out.Binding]

transformBinding (Let pat () (If c tb fb rt)) = do
  tb' <- transformBody tb
  fb' <- transformBody fb
  return [Let pat () $ If c tb' fb' rt]

transformBinding (Let pat () (DoLoop ctx val form body)) =
  localScope (scopeOfLoopForm form <> scopeOfFParams mergeparams) $ do
    body' <- transformBody body
    return [Let pat () $ DoLoop ctx val form body']
  where mergeparams = map fst $ ctx ++ val

transformBinding (Let pat () (Op (Map cs w lam arrs))) =
  distributeMap pat $ MapLoop cs w lam arrs

transformBinding (Let pat () (Op (Scanomap cs w lam1 lam2 nes arrs))) = do
  lam1_sequential <- FOT.transformLambda lam1
  lam2_sequential <- FOT.transformLambda lam2
  runBinder_ $ blockedScan pat cs w lam1_sequential lam2_sequential nes arrs

transformBinding (Let pat () (Op (Redomap cs w comm lam1 lam2 nes arrs))) =
  if sequentialiseRedomapBody then do
    lam1_sequential <- FOT.transformLambda lam1
    lam2_sequential <- FOT.transformLambda lam2
    blockedReduction pat cs w comm lam1_sequential lam2_sequential nes arrs
  else do
    (mapbnd, redbnd) <- redomapToMapAndReduce pat () (cs, w, comm, lam1, lam2, nes, arrs)
    transformBindings [mapbnd, redbnd]
      where sequentialiseRedomapBody = True

transformBinding (Let res_pat () (Op (Reduce cs w comm red_fun red_input)))
  | Just do_irwim <- irwim res_pat cs w comm red_fun red_input = do
      types <- asksScope scopeForSOACs
      bnds <- fst <$> runBinderT (simplifyBindings =<< collectBindings_ do_irwim) types
      transformBindings bnds

transformBinding (Let pat () (Op (Reduce cs w comm red_fun red_input))) = do
  red_fun_sequential <- FOT.transformLambda red_fun
  red_fun_sequential' <- renameLambda red_fun_sequential
  blockedReduction pat cs w comm red_fun_sequential' red_fun_sequential nes arrs
  where (nes, arrs) = unzip red_input

transformBinding (Let res_pat () (Op (Scan cs w scan_fun scan_input)))
  | Just do_iswim <- iswim res_pat cs w scan_fun scan_input = do
      types <- asksScope scopeForSOACs
      transformBindings =<< (snd <$> runBinderT do_iswim types)

transformBinding (Let pat () (Op (Scan cs w fun input))) = do
  fun_sequential <- FOT.transformLambda fun
  fun_sequential_renamed <- renameLambda fun_sequential
  runBinder_ $
    blockedScan pat cs w fun_sequential fun_sequential_renamed nes arrs
  where (nes, arrs) = unzip input

-- Streams can be handled in two different ways - either we
-- sequentialise the body or we keep it parallel and distribute.

transformBinding (Let pat () (Op (Stream cs w
                                  (RedLike _o comm red_fun nes) fold_fun arrs)))
  | any (not . primType) $ lambdaReturnType red_fun,
    Just fold_fun' <- extLambdaToLambda fold_fun  = do
  -- Split into a chunked map and a reduction, with the latter
  -- distributed.

  fold_fun_sequential <- FOT.transformLambda fold_fun'

  let (red_pat_elems, concat_pat_elems) =
        splitAt (length nes) $ patternValueElements pat
      red_pat = Pattern [] red_pat_elems
      concat_pat = Pattern [] concat_pat_elems

  (map_bnd, map_misc_bnds) <- blockedMap concat_pat cs w InOrder fold_fun_sequential nes arrs
  let num_threads = arraysSize 0 $ patternTypes $ bindingPattern map_bnd
      red_input = zip nes $ patternNames $ bindingPattern map_bnd

  ((map_misc_bnds++[map_bnd])++) <$>
    inScopeOf (map_misc_bnds++[map_bnd])
    (transformBinding $ Let red_pat () $
     Op (Reduce cs num_threads comm red_fun red_input))

transformBinding (Let pat () (Op (Stream cs w
                                  (RedLike _ comm red_fun nes) fold_fun arrs)))
  | Just fold_fun' <- extLambdaToLambda fold_fun = do
  -- Generate a kernel immediately.
  red_fun_sequential <- FOT.transformLambda red_fun
  fold_fun_sequential <- FOT.transformLambda fold_fun'
  blockedReductionStream pat cs w comm red_fun_sequential fold_fun_sequential nes arrs

transformBinding (Let pat () (Op (Stream cs w (Sequential nes) fold_fun arrs))) = do
  -- Remove the stream and leave the body parallel.  It will be
  -- distributed.
  types <- asksScope scopeForSOACs
  transformBindings =<<
    (snd <$> runBinderT (sequentialStreamWholeArray pat cs w nes fold_fun arrs) types)

transformBinding (Let pat () (Op (Stream cs w (MapLike _) map_fun arrs))) = do
  -- Remove the stream and leave the body parallel.  It will be
  -- distributed.
  types <- asksScope scopeForSOACs
  transformBindings =<<
    (snd <$> runBinderT (sequentialStreamWholeArray pat cs w [] map_fun arrs) types)

transformBinding (Let pat () (Op (Write cs len lam ivs as))) = runBinder_ $ do
  lam' <- FOT.transformLambda lam
  thread_index <- newVName "thread_index"
  let lam'' = lam' { lambdaParams =
                       Param thread_index (Prim int32) :
                       lambdaParams lam' }
  letBind_ pat $ Op $ WriteKernel cs len lam'' ivs as

transformBinding bnd =
  runBinder_ $ FOT.transformBindingRecursively bnd

data MapLoop = MapLoop Certificates SubExp Lambda [VName]

mapLoopExp :: MapLoop -> Exp
mapLoopExp (MapLoop cs w lam arrs) = Op $ Map cs w lam arrs

distributeMap :: (HasScope Out.Kernels m,
                  MonadFreshNames m, MonadLogger m) =>
                 Pattern -> MapLoop -> m [Out.Binding]
distributeMap pat (MapLoop cs w lam arrs) = do
  types <- askScope
  let env = KernelEnv { kernelNest =
                        singleNesting (Nesting mempty $
                                       MapNesting pat cs w $
                                       zip (lambdaParams lam) arrs)
                      , kernelScope =
                        types <> scopeForKernels (scopeOf lam)
                      }
  fmap (postKernelBindings . snd) $ runKernelM env $
    distribute =<< distributeMapBodyBindings acc (bodyBindings $ lambdaBody lam)
    where acc = KernelAcc { kernelTargets = singleTarget (pat, bodyResult $ lambdaBody lam)
                          , kernelStms = mempty
                          }

data KernelEnv = KernelEnv { kernelNest :: Nestings
                           , kernelScope :: Scope Out.Kernels
                           }

data KernelAcc = KernelAcc { kernelTargets :: Targets
                           , kernelStms :: [Out.KernelStm Out.Kernels]
                           }

data KernelRes = KernelRes { accPostKernels :: PostKernels
                           , accLog :: Log
                           }

instance Monoid KernelRes where
  KernelRes ks1 log1 `mappend` KernelRes ks2 log2 =
    KernelRes (ks1 <> ks2) (log1 <> log2)
  mempty = KernelRes mempty mempty

newtype PostKernel = PostKernel { unPostKernel :: [Out.Binding] }

newtype PostKernels = PostKernels [PostKernel]

instance Monoid PostKernels where
  mempty = PostKernels mempty
  PostKernels xs `mappend` PostKernels ys = PostKernels $ ys ++ xs

postKernelBindings :: PostKernels -> [Out.Binding]
postKernelBindings (PostKernels kernels) = concatMap unPostKernel kernels

typeEnvFromKernelAcc :: KernelAcc -> Scope Out.Kernels
typeEnvFromKernelAcc = scopeOf . fst . outerTarget . kernelTargets

addStmToKernel :: (HasScope Out.Kernels m, MonadFreshNames m) =>
                  KernelStm Out.Kernels -> KernelAcc -> m KernelAcc
addStmToKernel stm acc =
  return acc { kernelStms = [stm] <> kernelStms acc }

addBindingToKernel :: (LocalScope Out.Kernels m, MonadFreshNames m) =>
                      Binding -> KernelAcc -> m KernelAcc
addBindingToKernel bnd acc = do
  stms <- Kernelise.transformBinding bnd
  return acc { kernelStms = stms <> kernelStms acc }

newtype KernelM a = KernelM (RWS KernelEnv KernelRes VNameSource a)
  deriving (Functor, Applicative, Monad,
            MonadReader KernelEnv,
            MonadWriter KernelRes,
            MonadFreshNames)

instance HasScope Out.Kernels KernelM where
  askScope = asks kernelScope

instance LocalScope Out.Kernels KernelM where
  localScope types = local $ \env ->
    env { kernelScope = kernelScope env <> types }

instance MonadLogger KernelM where
  addLog msgs = tell mempty { accLog = msgs }

runKernelM :: (MonadFreshNames m, MonadLogger m) =>
              KernelEnv -> KernelM a -> m (a, PostKernels)
runKernelM env (KernelM m) = do
  (x, res) <- modifyNameSource $ getKernels . runRWS m env
  addLog $ accLog res
  return (x, accPostKernels res)
  where getKernels (x,s,a) = ((x, a), s)

addKernels :: PostKernels -> KernelM ()
addKernels ks = tell $ mempty { accPostKernels = ks }

addKernel :: [Out.Binding] -> KernelM ()
addKernel bnds = addKernels $ PostKernels [PostKernel bnds]

withBinding :: Binding -> KernelM a -> KernelM a
withBinding bnd = local $ \env ->
  env { kernelScope =
          kernelScope env <> scopeForKernels (scopeOf [bnd])
      , kernelNest =
          letBindInInnerNesting provided $
          kernelNest env
      }
  where provided = HS.fromList $ patternNames $ bindingPattern bnd

mapNesting :: Pattern -> Certificates -> SubExp -> Lambda -> [VName]
           -> KernelM a
           -> KernelM a
mapNesting pat cs w lam arrs = local $ \env ->
  env { kernelNest = pushInnerNesting nest $ kernelNest env
      , kernelScope = kernelScope env <> scopeForKernels (scopeOf lam)
      }
  where nest = Nesting mempty $
               MapNesting pat cs w $
               zip (lambdaParams lam) arrs

unbalancedLambda :: Lambda -> Bool
unbalancedLambda lam =
  unbalancedBody
  (HS.fromList $ map paramName $ lambdaParams lam) $
  lambdaBody lam

  where subExpBound (Var i) bound = i `HS.member` bound
        subExpBound (Constant _) _ = False

        unbalancedBody bound body =
          any (unbalancedBinding (bound <> boundInBody body) . bindingExp) $
          bodyBindings body

        -- XXX - our notion of balancing is probably still too naive.
        unbalancedBinding bound (Op (Map _ w _ _)) =
          w `subExpBound` bound
        unbalancedBinding bound (Op (Reduce _ w _ _ _)) =
          w `subExpBound` bound
        unbalancedBinding bound (Op (Scan _ w _ _)) =
          w `subExpBound` bound
        unbalancedBinding bound (Op (Redomap _ w _ _ _ _ _)) =
          w `subExpBound` bound
        unbalancedBinding bound (Op (Scanomap _ w _ _ _ _)) =
          w `subExpBound` bound
        unbalancedBinding bound (Op (Stream _ w _ _ _)) =
          w `subExpBound` bound
        unbalancedBinding _ (Op Write{}) =
          False
        unbalancedBinding bound (DoLoop _ merge (ForLoop i iterations) body) =
          iterations `subExpBound` bound ||
          unbalancedBody bound' body
          where bound' = foldr HS.insert bound $
                         i : map (paramName . fst) merge
        unbalancedBinding _ (DoLoop _ _ (WhileLoop _) _) =
          False

        unbalancedBinding bound (If _ tbranch fbranch _) =
          unbalancedBody bound tbranch || unbalancedBody bound fbranch

        unbalancedBinding _ (PrimOp _) =
          False
        unbalancedBinding _ (Apply fname _ _) =
          not $ isBuiltInFunction fname

bodyContainsParallelism :: Body -> Bool
bodyContainsParallelism = any (isMap . bindingExp) . bodyBindings
  where isMap Op{} = True
        isMap _ = False

lambdaContainsParallelism :: Lambda -> Bool
lambdaContainsParallelism = bodyContainsParallelism . lambdaBody

distributeInnerMap :: Pattern -> MapLoop -> KernelAcc
                   -> KernelM KernelAcc
distributeInnerMap pat maploop@(MapLoop cs w lam arrs) acc
  | unbalancedLambda lam, lambdaContainsParallelism lam =
      addBindingToKernel (Let pat () $ mapLoopExp maploop) acc
  | otherwise =
      distribute =<<
      leavingNesting maploop =<<
      mapNesting pat cs w lam arrs
      (distribute =<< distributeMapBodyBindings acc' (bodyBindings $ lambdaBody lam))
      where acc' = KernelAcc { kernelTargets = pushInnerTarget
                                               (pat, bodyResult $ lambdaBody lam) $
                                               kernelTargets acc
                             , kernelStms = mempty
                             }

leavingNesting :: MapLoop -> KernelAcc -> KernelM KernelAcc
leavingNesting (MapLoop cs w lam arrs) acc =
  case second reverse $ kernelTargets acc of
   (_, []) ->
     fail "The kernel targets list is unexpectedly small"
   ((pat,res), x:xs) -> do
     let acc' = acc { kernelTargets = (x, reverse xs) }
     case kernelStms acc' of
       []      -> return acc'
       remnant ->
         let body = mkBody (undefined remnant) res
             used_in_body = freeInBody body
             (used_params, used_arrs) =
               unzip $
               filter ((`HS.member` used_in_body) . paramName . fst) $
               zip (lambdaParams lam) arrs
             lam' = Lambda { lambdaBody = body
                           , lambdaReturnType = map rowType $ patternTypes pat
                           , lambdaParams = used_params
                           }
         in addBindingToKernel (Let pat () $ Op $ Map cs w lam' used_arrs)
            acc' { kernelStms = [] }

distributeMapBodyBindings :: KernelAcc -> [Binding] -> KernelM KernelAcc

distributeMapBodyBindings acc [] =
  return acc

distributeMapBodyBindings acc
  (Let pat () (Op (Stream cs w (Sequential accs) lam arrs)):bnds) = do
    types <- asksScope scopeForSOACs
    stream_bnds <-
      snd <$> runBinderT (sequentialStreamWholeArray pat cs w accs lam arrs) types
    stream_bnds' <-
      runReaderT (copyPropagateInBindings bindableSimpleOps stream_bnds) types
    distributeMapBodyBindings acc $ stream_bnds' ++ bnds

distributeMapBodyBindings acc (bnd:bnds) =
  -- It is important that bnd is in scope if 'maybeDistributeBinding'
  -- wants to distribute, even if this causes the slightly silly
  -- situation that bnd is in scope of itself.
  withBinding bnd $
  maybeDistributeBinding bnd =<<
  distributeMapBodyBindings acc bnds

maybeDistributeBinding :: Binding -> KernelAcc
                       -> KernelM KernelAcc
maybeDistributeBinding bnd@(Let pat _ (Op (Map cs w lam arrs))) acc =
  -- Only distribute inside the map if we can distribute everything
  -- following the map.
  distributeIfPossible acc >>= \case
    Nothing -> addBindingToKernel bnd acc
    Just acc' -> distribute =<< distributeInnerMap pat (MapLoop cs w lam arrs) acc'

maybeDistributeBinding bnd@(Let pat _ (DoLoop [] val form body)) acc
  | bodyContainsParallelism body =
  distributeSingleBinding acc bnd >>= \case
    Just (kernels, res, nest, acc')
      | length res == patternSize pat -> do
      addKernels kernels
      localScope (typeEnvFromKernelAcc acc') $ do
        types <- asksScope scopeForSOACs
        bnds <- runReaderT
                (interchangeLoops nest (SeqLoop pat val form body)) types
        -- runDistribM starts out with an empty scope, so we have to
        -- immmediately insert the real one.
        scope <- askScope
        bnds' <- runDistribM $ localScope scope $ transformBindings bnds
        addKernel bnds'
      return acc'
    _ ->
      addBindingToKernel bnd acc

maybeDistributeBinding (Let pat _ (Op (Reduce cs w comm lam input))) acc
  | Just m <- irwim pat cs w comm lam input = do
      types <- asksScope scopeForSOACs
      (_, bnds) <- runBinderT m types
      distributeMapBodyBindings acc bnds

-- If the scan can be distributed by itself, we will turn it into a
-- segmented scan.
--
-- If the scan cannot be distributed by itself, it will be
-- sequentialised in the default case for this function.
maybeDistributeBinding bnd@(Let pat _ (Op (Scan cs w lam input))) acc =
  distributeSingleBinding acc bnd >>= \case
    Just (kernels, res, nest, acc')
      | Just perm <- res `isPermutationOf` map Var (patternNames pat) -> do
          lam' <- FOT.transformLambda lam
          localScope (typeEnvFromKernelAcc acc') $
            segmentedScanKernel nest perm cs w lam' input >>=
            kernelOrNot bnd acc kernels acc'
    _ ->
      addBindingToKernel bnd acc

-- If the reduction can be distributed by itself, we will turn it into a
-- segmented reduce.
--
-- If the reduction cannot be distributed by itself, it will be
-- sequentialised in the default case for this function.
maybeDistributeBinding bnd@(Let pat _ (Op (Reduce cs w comm lam input))) acc =
  distributeSingleBinding acc bnd >>= \case
    Just (kernels, res, nest, acc')
      | Just perm <- map Var (patternNames pat) `isPermutationOf` res ->
          localScope (typeEnvFromKernelAcc acc') $ do
          lam' <- FOT.transformLambda lam
          regularSegmentedReduceKernel nest perm cs w comm lam' input >>=
            kernelOrNot bnd acc kernels acc'
    _ ->
      addBindingToKernel bnd acc

maybeDistributeBinding (Let pat attr (PrimOp (Replicate n v))) acc
  | [t] <- patternTypes pat = do
      -- XXX: We need a temporary dummy binding to prevent an empty
      -- map body.  The kernel extractor does not like empty map
      -- bodies.
      tmp <- newVName "tmp"
      let rowt = rowType t
          newbnd = Let pat attr $ Op $ Map [] n lam []
          tmpbnd = Let (Pattern [] [PatElem tmp BindVar rowt]) () $ PrimOp $ SubExp v
          lam = Lambda { lambdaReturnType = [rowt]
                       , lambdaParams = []
                       , lambdaBody = mkBody [tmpbnd] [Var tmp]
                       }
      maybeDistributeBinding newbnd acc

maybeDistributeBinding bnd@(Let _ _ (PrimOp Copy{})) acc =
  distributeSingleUnaryBinding acc bnd $ \_ outerpat arr ->
  addKernel [Let outerpat () $ PrimOp $ Copy arr]

maybeDistributeBinding bnd@(Let _ _ (PrimOp (Rearrange cs perm _))) acc =
  distributeSingleUnaryBinding acc bnd $ \nest outerpat arr -> do
    let r = length (snd nest) + 1
        perm' = [0..r-1] ++ map (+r) perm
    addKernel [Let outerpat () $ PrimOp $ Rearrange cs perm' arr]

maybeDistributeBinding bnd@(Let _ _ (PrimOp (Reshape cs reshape _))) acc =
  distributeSingleUnaryBinding acc bnd $ \nest outerpat arr -> do
    let reshape' = map DimCoercion (kernelNestWidths nest) ++ reshape
    addKernel [Let outerpat () $ PrimOp $ Reshape cs reshape' arr]

maybeDistributeBinding bnd acc =
  addBindingToKernel bnd acc

distributeSingleUnaryBinding :: KernelAcc
                             -> Binding
                             -> (KernelNest -> Pattern -> VName -> KernelM ())
                             -> KernelM KernelAcc
distributeSingleUnaryBinding acc bnd f =
  distributeSingleBinding acc bnd >>= \case
    Just (kernels, res, nest, acc')
      | res == map Var (patternNames $ bindingPattern bnd),
        (outer, _) <- nest,
        [(_, arr)] <- loopNestingParamsAndArrs outer -> do
          addKernels kernels
          let outerpat = loopNestingPattern $ fst nest
          f nest outerpat arr
          return acc'
    _ -> addBindingToKernel bnd acc

distribute :: KernelAcc -> KernelM KernelAcc
distribute acc =
  fromMaybe acc <$> distributeIfPossible acc

distributeIfPossible :: KernelAcc -> KernelM (Maybe KernelAcc)
distributeIfPossible acc = do
  nest <- asks kernelNest
  tryDistribute nest (kernelTargets acc) (kernelStms acc) >>= \case
    Nothing -> return Nothing
    Just (targets, kernel) -> do
      addKernel kernel
      return $ Just KernelAcc { kernelTargets = targets
                              , kernelStms = []
                              }

distributeSingleBinding :: KernelAcc -> Binding
                        -> KernelM (Maybe (PostKernels, Result, KernelNest, KernelAcc))
distributeSingleBinding acc bnd = do
  nest <- asks kernelNest
  tryDistribute nest (kernelTargets acc) (kernelStms acc) >>= \case
    Nothing -> return Nothing
    Just (targets, distributed_bnds) ->
      tryDistributeBinding nest targets (Thread ThreadsInSpace bnd) >>= \case
        Nothing -> return Nothing
        Just (res, targets', new_kernel_nest) ->
          return $ Just (PostKernels [PostKernel distributed_bnds],
                         res,
                         new_kernel_nest,
                         KernelAcc { kernelTargets = targets'
                                   , kernelStms = []
                                   })

segmentedScanKernel :: KernelNest
                    -> [Int]
                    -> Certificates -> SubExp -> Out.Lambda -> [(SubExp, VName)]
                    -> KernelM (Maybe [Out.Binding])
segmentedScanKernel nest perm cs segment_size lam scan_inps =
  isSegmentedOp nest perm segment_size lam scan_inps $
  \pat flat_pat _num_segments total_num_elements scan_inps' -> do
    blockedSegmentedScan segment_size flat_pat cs total_num_elements lam scan_inps'

    forM_ (zip (patternValueElements pat) (patternNames flat_pat)) $
      \(dst_pat_elem, flat) -> do
        let ident = patElemIdent dst_pat_elem
            bindage = patElemBindage dst_pat_elem
            dims = arrayDims $ identType ident
        addBinding $ mkLet [] [(ident, bindage)] $
          PrimOp $ Reshape [] (map DimNew dims) flat

regularSegmentedReduceKernel :: KernelNest
                             -> [Int]
                             -> Certificates -> SubExp -> Commutativity -> Out.Lambda -> [(SubExp, VName)]
                             -> KernelM (Maybe [Out.Binding])
regularSegmentedReduceKernel nest perm cs segment_size comm lam reduce_inps =
  isSegmentedOp nest perm segment_size lam reduce_inps $
  \pat flat_pat num_segments total_num_elements reduce_inps' ->
    if False -- Using a scan seems an efficient general method.
      then regularSegmentedReduce segment_size num_segments pat cs comm lam reduce_inps'
      else regularSegmentedReduceAsScan
           segment_size num_segments (kernelNestWidths nest)
           flat_pat pat
           cs total_num_elements lam reduce_inps'

isSegmentedOp :: KernelNest
              -> [Int]
              -> SubExp
              -> Out.Lambda
              -> [(SubExp, VName)]
              -> (Pattern
                  -> Pattern
                  -> SubExp
                  -> SubExp
                  -> [(SubExp, VName)]
                  -> Binder Out.Kernels ())
              -> KernelM (Maybe [Out.Binding])
isSegmentedOp nest perm segment_size lam scan_inps m = runMaybeT $ do
  -- We must verify that array inputs to the operation are inputs to
  -- the outermost loop nesting or free in the loop nest, and that
  -- none of the names bound by the loop nest are used in the lambda.
  -- Furthermore, the neutral elements must be free in the loop nest.

  let bound_by_nest = boundInKernelNest nest

  (pre_bnds, nesting_size, ispace, kernel_inps, _rets) <- flatKernel nest

  unless (HS.null $ freeInLambda lam `HS.intersection` bound_by_nest) $
    fail "Lambda uses nest-bound parameters."

  let indices = map fst ispace

      prepareInput (ne, arr) = do
        case ne of
          Var v | v `HS.member` bound_by_nest ->
                    fail "Neutral element bound in nest"
          _ -> return ()

        case find ((==arr) . kernelInputName) kernel_inps of
          Just inp | kernelInputIndices inp == map Var indices ->
            return $ return (ne, kernelInputArray inp)
          Nothing | not (arr `HS.member` bound_by_nest) -> return $ do
                      -- This input is something that is free outside
                      -- the loop nesting. We will have to replicate
                      -- it.
                      arr' <- letExp (baseString arr ++ "_repd") $
                              PrimOp $ Replicate segment_size $ Var arr
                      return (ne, arr')
          _ ->
            fail "Input not free or outermost."

  mk_inps <- mapM prepareInput scan_inps

  lift $ runBinder_ $ do
    mapM_ addBinding pre_bnds

    -- We must make sure all inputs are of size
    -- segment_size*nesting_size.
    total_num_elements <-
      letSubExp "total_num_elements" $ PrimOp $ BinOp (Mul Int32) segment_size nesting_size

    let flatten (ne, arr) = do
          ne_shape <- arrayShape <$> subExpType ne
          arr_shape <- arrayShape <$> lookupType arr
          let reshape = reshapeOuter [DimNew total_num_elements]
                        (shapeRank arr_shape - shapeRank ne_shape)
                        arr_shape
          arr_manifest <- letExp (baseString arr ++ "_manifest") $
                          PrimOp $ Copy arr
          arr' <- letExp (baseString arr ++ "_flat") $
                  PrimOp $ Reshape [] reshape arr_manifest
          return (ne, arr')

    op_inps' <- mapM flatten =<< sequence mk_inps

    let pat = Pattern [] $ rearrangeShape perm $
              patternValueElements $ loopNestingPattern $ fst nest
        flatPatElem pat_elem t = do
          let t' = arrayOfRow t total_num_elements
          name <- newVName $ baseString (patElemName pat_elem) ++ "_flat"
          return $ PatElem name BindVar t'
    flat_pat <- Pattern [] <$>
                zipWithM flatPatElem
                (patternValueElements pat)
                (lambdaReturnType lam)


    m pat flat_pat nesting_size total_num_elements op_inps'

kernelOrNot :: Binding -> KernelAcc
            -> PostKernels -> KernelAcc -> Maybe [Out.Binding]
            -> KernelM KernelAcc
kernelOrNot bnd acc _ _ Nothing =
  addBindingToKernel bnd acc
kernelOrNot _ _ kernels acc' (Just bnds) = do
  addKernels kernels
  addKernel bnds
  return acc'
