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

import Futhark.Representation.SOACS
import Futhark.Representation.SOACS.Simplify (simplifyStms, simpleSOACS)
import qualified Futhark.Representation.Kernels as Out
import Futhark.Representation.Kernels.Kernel
import Futhark.MonadFreshNames
import Futhark.Tools
import qualified Futhark.Transform.FirstOrderTransform as FOT
import qualified Futhark.Pass.ExtractKernels.Kernelise as Kernelise
import Futhark.Transform.Rename
import Futhark.Pass
import Futhark.Transform.CopyPropagate
import Futhark.Pass.ExtractKernels.Distribution
import Futhark.Pass.ExtractKernels.ISRWIM
import Futhark.Pass.ExtractKernels.BlockedKernel
import Futhark.Pass.ExtractKernels.Segmented
import Futhark.Pass.ExtractKernels.Interchange
import Futhark.Util.Log

type KernelsStm = Out.Stm Out.Kernels
type InKernelStm = Out.Stm Out.InKernel
type InKernelLambda = Out.Lambda Out.InKernel

-- | Transform a program using SOACs to a program using explicit
-- kernels, using the kernel extraction transformation.
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

transformFunDef :: FunDef -> DistribM (Out.FunDef Out.Kernels)
transformFunDef (FunDef entry name rettype params body) = do
  body' <- localScope (scopeOfFParams params) $
           transformBody body
  return $ FunDef entry name rettype params body'

transformBody :: Body -> DistribM (Out.Body Out.Kernels)
transformBody body = do bnds <- transformStms $ bodyStms body
                        return $ mkBody bnds $ bodyResult body

transformStms :: [Stm] -> DistribM [KernelsStm]
transformStms [] =
  return []
transformStms (bnd:bnds) =
  sequentialisedUnbalancedStm bnd >>= \case
    Nothing -> do
      bnd' <- transformStm bnd
      inScopeOf bnd' $
        (bnd'++) <$> transformStms bnds
    Just bnds' ->
      transformStms $ bnds' <> bnds

sequentialisedUnbalancedStm :: Stm -> DistribM (Maybe [Stm])
sequentialisedUnbalancedStm (Let pat _ (Op soac@(Map _ _ lam _)))
  | unbalancedLambda lam, lambdaContainsParallelism lam = do
      types <- asksScope scopeForSOACs
      Just . snd <$> runBinderT (FOT.transformSOAC pat soac) types
sequentialisedUnbalancedStm (Let pat _ (Op soac@(Redomap _ _ _ _ lam2 _ _)))
  | unbalancedLambda lam2, lambdaContainsParallelism lam2 = do
      types <- asksScope scopeForSOACs
      Just . snd <$> runBinderT (FOT.transformSOAC pat soac) types
sequentialisedUnbalancedStm _ =
  return Nothing

scopeForSOACs :: Scope Out.Kernels -> Scope SOACS
scopeForSOACs = castScope

scopeForKernels :: Scope SOACS -> Scope Out.Kernels
scopeForKernels = castScope

transformStm :: Stm -> DistribM [KernelsStm]

transformStm (Let pat () (If c tb fb rt)) = do
  tb' <- transformBody tb
  fb' <- transformBody fb
  return [Let pat () $ If c tb' fb' rt]

transformStm (Let pat () (DoLoop ctx val form body)) =
  localScope (scopeOfLoopForm form <> scopeOfFParams mergeparams) $ do
    body' <- transformBody body
    return [Let pat () $ DoLoop ctx val form body']
  where mergeparams = map fst $ ctx ++ val

transformStm (Let pat () (Op (Map cs w lam arrs))) =
  distributeMap pat $ MapLoop cs w lam arrs

transformStm (Let pat () (Op (Scanomap cs w lam1 lam2 nes arrs))) = do
  lam1_sequential <- FOT.transformLambda lam1
  lam2_sequential <- FOT.transformLambda lam2
  runBinder_ $ blockedScan pat cs w lam1_sequential lam2_sequential (intConst Int32 1) [] [] nes arrs

transformStm (Let pat () (Op (Redomap cs w comm lam1 lam2 nes arrs))) =
  if sequentialiseRedomapBody then do
    lam1_sequential <- FOT.transformLambda lam1
    lam2_sequential <- FOT.transformLambda lam2
    blockedReduction pat cs w comm lam1_sequential lam2_sequential nes arrs
  else do
    (mapbnd, redbnd) <- redomapToMapAndReduce pat () (cs, w, comm, lam1, lam2, nes, arrs)
    transformStms [mapbnd, redbnd]
      where sequentialiseRedomapBody = True

transformStm (Let res_pat () (Op (Reduce cs w comm red_fun red_input)))
  | Just do_irwim <- irwim res_pat cs w comm red_fun red_input = do
      types <- asksScope scopeForSOACs
      bnds <- fst <$> runBinderT (simplifyStms =<< collectStms_ do_irwim) types
      transformStms bnds

transformStm (Let pat () (Op (Reduce cs w comm red_fun red_input))) = do
  red_fun_sequential <- FOT.transformLambda red_fun
  red_fun_sequential' <- renameLambda red_fun_sequential
  blockedReduction pat cs w comm red_fun_sequential' red_fun_sequential nes arrs
  where (nes, arrs) = unzip red_input

transformStm (Let res_pat () (Op (Scan cs w scan_fun scan_input)))
  | Just do_iswim <- iswim res_pat cs w scan_fun scan_input = do
      types <- asksScope scopeForSOACs
      transformStms =<< (snd <$> runBinderT do_iswim types)

transformStm (Let pat () (Op (Scan cs w fun input))) = do
  fun_sequential <- FOT.transformLambda fun
  fun_sequential_renamed <- renameLambda fun_sequential
  runBinder_ $
    blockedScan pat cs w fun_sequential fun_sequential_renamed (intConst Int32 1) [] [] nes arrs
  where (nes, arrs) = unzip input

-- Streams can be handled in two different ways - either we
-- sequentialise the body or we keep it parallel and distribute.

transformStm (Let pat () (Op (Stream cs w
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
    (transformStm $ Let red_pat () $
     Op (Reduce cs num_threads comm red_fun red_input))

transformStm (Let pat () (Op (Stream cs w
                                  (RedLike _ comm red_fun nes) fold_fun arrs)))
  | Just fold_fun' <- extLambdaToLambda fold_fun = do
  -- Generate a kernel immediately.
  red_fun_sequential <- FOT.transformLambda red_fun
  fold_fun_sequential <- FOT.transformLambda fold_fun'
  blockedReductionStream pat cs w comm red_fun_sequential fold_fun_sequential nes arrs

transformStm (Let pat () (Op (Stream cs w (Sequential nes) fold_fun arrs))) = do
  -- Remove the stream and leave the body parallel.  It will be
  -- distributed.
  types <- asksScope scopeForSOACs
  transformStms =<<
    (snd <$> runBinderT (sequentialStreamWholeArray pat cs w nes fold_fun arrs) types)

transformStm (Let pat () (Op (Stream cs w (MapLike _) map_fun arrs))) = do
  -- Remove the stream and leave the body parallel.  It will be
  -- distributed.
  types <- asksScope scopeForSOACs
  transformStms =<<
    (snd <$> runBinderT (sequentialStreamWholeArray pat cs w [] map_fun arrs) types)

transformStm (Let pat () (Op (Write cs w lam ivs as))) = runBinder_ $ do
  lam' <- FOT.transformLambda lam
  write_i <- newVName "write_i"
  let (i_res, v_res) = splitAt (length as) $ bodyResult $ lambdaBody lam'
      kstms = bodyStms $ lambdaBody lam'
      krets = do (i, v, (a_w, a)) <- zip3 i_res v_res as
                 return $ WriteReturn a_w a i v
      body = KernelBody () kstms krets
      inputs = do (p, p_a) <- zip (lambdaParams lam') ivs
                  return $ KernelInput (paramName p) (paramType p) p_a [Var write_i]
  (bnds, kernel) <-
    mapKernel cs w [(write_i,w)] inputs (map rowType $ patternTypes pat) body
  mapM_ addStm bnds
  letBind_ pat $ Op kernel

transformStm bnd =
  runBinder_ $ FOT.transformStmRecursively bnd

data MapLoop = MapLoop Certificates SubExp Lambda [VName]

mapLoopExp :: MapLoop -> Exp
mapLoopExp (MapLoop cs w lam arrs) = Op $ Map cs w lam arrs

distributeMap :: (HasScope Out.Kernels m,
                  MonadFreshNames m, MonadLogger m) =>
                 Pattern -> MapLoop -> m [KernelsStm]
distributeMap pat (MapLoop cs w lam arrs) = do
  types <- askScope
  let env = KernelEnv { kernelNest =
                        singleNesting (Nesting mempty $
                                       MapNesting pat cs w $
                                       zip (lambdaParams lam) arrs)
                      , kernelScope =
                        types <> scopeForKernels (scopeOf lam)
                      }
  let res = map Var $ patternNames pat
  par_bnds <- fmap (postKernelsStms . snd) $ runKernelM env $
    distribute =<< distributeMapBodyStms acc (bodyStms $ lambdaBody lam)

  if not versionedCode || not (containsNestedParallelism lam)
    then return par_bnds
    else do
    par_body <- renameBody $ mkBody par_bnds res

    seq_bnds <- do
      soactypes <- asksScope scopeForSOACs
      (seq_lam, _) <- runBinderT (FOT.transformLambda lam) soactypes
      fmap (postKernelsStms . snd) $ runKernelM env $
        distribute =<< distributeMapBodyStms acc (bodyStms $ lambdaBody seq_lam)
    seq_body <- renameBody $ mkBody seq_bnds res
    kernelAlternatives w pat seq_body par_body
    where acc = KernelAcc { kernelTargets = singleTarget (pat, bodyResult $ lambdaBody lam)
                          , kernelStms = mempty
                          }

data KernelEnv = KernelEnv { kernelNest :: Nestings
                           , kernelScope :: Scope Out.Kernels
                           }

data KernelAcc = KernelAcc { kernelTargets :: Targets
                           , kernelStms :: [InKernelStm]
                           }

data KernelRes = KernelRes { accPostKernels :: PostKernels
                           , accLog :: Log
                           }

instance Monoid KernelRes where
  KernelRes ks1 log1 `mappend` KernelRes ks2 log2 =
    KernelRes (ks1 <> ks2) (log1 <> log2)
  mempty = KernelRes mempty mempty

newtype PostKernel = PostKernel { unPostKernel :: [KernelsStm] }

newtype PostKernels = PostKernels [PostKernel]

instance Monoid PostKernels where
  mempty = PostKernels mempty
  PostKernels xs `mappend` PostKernels ys = PostKernels $ ys ++ xs

postKernelsStms :: PostKernels -> [KernelsStm]
postKernelsStms (PostKernels kernels) = concatMap unPostKernel kernels

typeEnvFromKernelAcc :: KernelAcc -> Scope Out.Kernels
typeEnvFromKernelAcc = scopeOf . fst . outerTarget . kernelTargets

addStmsToKernel :: (HasScope Out.Kernels m, MonadFreshNames m) =>
                   [InKernelStm] -> KernelAcc -> m KernelAcc
addStmsToKernel stms acc =
  return acc { kernelStms = stms <> kernelStms acc }

addStmToKernel :: (LocalScope Out.Kernels m, MonadFreshNames m) =>
                      Stm -> KernelAcc -> m KernelAcc
addStmToKernel bnd acc = do
  stms <- runBinder_ $ Kernelise.transformStm bnd
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

collectKernels :: KernelM a -> KernelM (a, PostKernels)
collectKernels m = pass $ do
  (x, res) <- listen m
  return ((x, accPostKernels res),
          const res { accPostKernels = mempty })

addKernels :: PostKernels -> KernelM ()
addKernels ks = tell $ mempty { accPostKernels = ks }

addKernel :: [KernelsStm] -> KernelM ()
addKernel bnds = addKernels $ PostKernels [PostKernel bnds]

withStm :: Stm -> KernelM a -> KernelM a
withStm bnd = local $ \env ->
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

inNesting :: KernelNest -> KernelM a -> KernelM a
inNesting (outer, nests) = local $ \env ->
  env { kernelNest = (inner, nests')
      , kernelScope = kernelScope env <> mconcat (map scopeOf $ outer : nests)
      }
  where (inner, nests') =
          case reverse nests of
            []           -> (asNesting outer, [])
            (inner' : ns) -> (asNesting inner', map asNesting $ outer : reverse ns)
        asNesting = Nesting mempty

unbalancedLambda :: Lambda -> Bool
unbalancedLambda lam =
  unbalancedBody
  (HS.fromList $ map paramName $ lambdaParams lam) $
  lambdaBody lam

  where subExpBound (Var i) bound = i `HS.member` bound
        subExpBound (Constant _) _ = False

        unbalancedBody bound body =
          any (unbalancedStm (bound <> boundInBody body) . bindingExp) $
          bodyStms body

        -- XXX - our notion of balancing is probably still too naive.
        unbalancedStm bound (Op (Map _ w _ _)) =
          w `subExpBound` bound
        unbalancedStm bound (Op (Reduce _ w _ _ _)) =
          w `subExpBound` bound
        unbalancedStm bound (Op (Scan _ w _ _)) =
          w `subExpBound` bound
        unbalancedStm bound (Op (Redomap _ w _ _ _ _ _)) =
          w `subExpBound` bound
        unbalancedStm bound (Op (Scanomap _ w _ _ _ _)) =
          w `subExpBound` bound
        unbalancedStm bound (Op (Stream _ w _ _ _)) =
          w `subExpBound` bound
        unbalancedStm _ (Op Write{}) =
          False
        unbalancedStm bound (DoLoop _ merge (ForLoop i _ iterations) body) =
          iterations `subExpBound` bound ||
          unbalancedBody bound' body
          where bound' = foldr HS.insert bound $
                         i : map (paramName . fst) merge
        unbalancedStm _ (DoLoop _ _ (WhileLoop _) _) =
          False

        unbalancedStm bound (If _ tbranch fbranch _) =
          unbalancedBody bound tbranch || unbalancedBody bound fbranch

        unbalancedStm _ (BasicOp _) =
          False
        unbalancedStm _ (Apply fname _ _) =
          not $ isBuiltInFunction fname

bodyContainsParallelism :: Body -> Bool
bodyContainsParallelism = any (isMap . bindingExp) . bodyStms
  where isMap Op{} = True
        isMap _ = False

bodyContainsMap :: Body -> Bool
bodyContainsMap = any (isMap . bindingExp) . bodyStms
  where isMap (Op Map{}) = True
        isMap _ = False

lambdaContainsParallelism :: Lambda -> Bool
lambdaContainsParallelism = bodyContainsParallelism . lambdaBody

-- Enable if you want the cool new versioned code.  Beware: may be
-- slower in practice.  Caveat emptor (and you are the emptor).
versionedCode :: Bool
versionedCode = False

distributeInnerMap :: Pattern -> MapLoop -> KernelAcc
                   -> KernelM KernelAcc
distributeInnerMap pat maploop@(MapLoop cs w lam arrs) acc
  | unbalancedLambda lam, lambdaContainsParallelism lam =
      addStmToKernel (Let pat () $ mapLoopExp maploop) acc
  | not versionedCode || not (containsNestedParallelism lam) =
      distributeNormally def_acc
  | otherwise =
      distributeSingleStm acc bnd >>= \case
      Nothing ->
        distributeNormally def_acc
      Just (post_kernels, _, nest, acc') -> do
        addKernels post_kernels
        -- The kernel can be distributed by itself, so now we can
        -- decide whether to just sequentialise, or exploit inner
        -- parallelism.
        let map_nesting = MapNesting pat cs w $ zip (lambdaParams lam) arrs
            nest' = pushInnerKernelNesting (pat, lam_res) map_nesting nest
            par_acc = KernelAcc { kernelTargets = pushInnerTarget
                                  (pat, lam_res) $ kernelTargets acc
                                , kernelStms = mempty
                                }
            extra_scope = targetsScope $ kernelTargets acc'
        (_, distributed_kernels) <- collectKernels $
          localScope extra_scope $ inNesting nest' $
          distribute =<< leavingNesting maploop =<< distribute =<<
          distributeMapBodyStms par_acc lam_bnds

        (par_bnds, par, sequentialised_kernel) <- localScope extra_scope $ do
          sequentialised_map_body <-
            localScope (scopeOfLParams (lambdaParams lam)) $ runBinder_ $
            mapM_ FOT.transformStmRecursively $ bodyStms $ lambdaBody lam
          let kbody = KernelBody () sequentialised_map_body $
                      map (ThreadsReturn ThreadsInSpace) lam_res
          constructKernel nest' kbody

        let outer_pat = loopNestingPattern $ fst nest
            res' = map Var $ patternNames outer_pat
        seq_body <- renameBody $ mkBody [sequentialised_kernel] res'
        par_body <- renameBody $ mkBody (postKernelsStms distributed_kernels) res'
        addKernel =<< kernelAlternatives par outer_pat seq_body par_body
        addKernel par_bnds
        return acc'
      where bnd = Let pat () $ mapLoopExp maploop
            lam_bnds = bodyStms $ lambdaBody lam
            lam_res = bodyResult $ lambdaBody lam
            def_acc = KernelAcc { kernelTargets = pushInnerTarget
                                  (pat, bodyResult $ lambdaBody lam) $
                                  kernelTargets acc
                                , kernelStms = mempty
                                }
            distributeNormally acc' =
              distribute =<<
              leavingNesting maploop =<<
              mapNesting pat cs w lam arrs
              (distribute =<< distributeMapBodyStms acc' lam_bnds)

leavingNesting :: MapLoop -> KernelAcc -> KernelM KernelAcc
leavingNesting (MapLoop cs w lam arrs) acc =
  case second reverse $ kernelTargets acc of
   (_, []) ->
     fail "The kernel targets list is unexpectedly small"
   ((pat,res), x:xs) -> do
     let acc' = acc { kernelTargets = (x, reverse xs) }
     case kernelStms acc' of
       []      -> return acc'
       remnant -> do
         let kbody = Body () remnant res
             used_in_body = freeInBody kbody
             (used_params, used_arrs) =
               unzip $
               filter ((`HS.member` used_in_body) . paramName . fst) $
               zip (lambdaParams lam) arrs
         stms <- runBinder_ $ Kernelise.mapIsh pat cs w used_params kbody used_arrs
         addStmsToKernel stms acc' { kernelStms = [] }

distributeMapBodyStms :: KernelAcc -> [Stm] -> KernelM KernelAcc

distributeMapBodyStms acc [] =
  return acc

distributeMapBodyStms acc
  (Let pat () (Op (Stream cs w (Sequential accs) lam arrs)):bnds) = do
    types <- asksScope scopeForSOACs
    stream_bnds <-
      snd <$> runBinderT (sequentialStreamWholeArray pat cs w accs lam arrs) types
    stream_bnds' <-
      runReaderT (copyPropagateInStms simpleSOACS stream_bnds) types
    distributeMapBodyStms acc $ stream_bnds' ++ bnds

distributeMapBodyStms acc (bnd:bnds) =
  -- It is important that bnd is in scope if 'maybeDistributeStm'
  -- wants to distribute, even if this causes the slightly silly
  -- situation that bnd is in scope of itself.
  withStm bnd $
  maybeDistributeStm bnd =<<
  distributeMapBodyStms acc bnds

maybeDistributeStm :: Stm -> KernelAcc
                       -> KernelM KernelAcc
maybeDistributeStm bnd@(Let pat _ (Op (Map cs w lam arrs))) acc =
  -- Only distribute inside the map if we can distribute everything
  -- following the map.
  distributeIfPossible acc >>= \case
    Nothing -> addStmToKernel bnd acc
    Just acc' -> distribute =<< distributeInnerMap pat (MapLoop cs w lam arrs) acc'

maybeDistributeStm bnd@(Let pat _ (DoLoop [] val form body)) acc
  | bodyContainsMap body =
  distributeSingleStm acc bnd >>= \case
    Just (kernels, res, nest, acc')
      | length res == patternSize pat,
        HS.null $ freeIn form `HS.intersection` boundInKernelNest nest -> do
      addKernels kernels
      localScope (typeEnvFromKernelAcc acc') $ do
        types <- asksScope scopeForSOACs
        bnds <- runReaderT
                (interchangeLoops nest (SeqLoop pat val form body)) types
        -- runDistribM starts out with an empty scope, so we have to
        -- immmediately insert the real one.
        scope <- askScope
        bnds' <- runDistribM $ localScope scope $ transformStms bnds
        addKernel bnds'
      return acc'
    _ ->
      addStmToKernel bnd acc

maybeDistributeStm (Let pat _ (Op (Reduce cs w comm lam input))) acc
  | Just m <- irwim pat cs w comm lam input = do
      types <- asksScope scopeForSOACs
      (_, bnds) <- runBinderT m types
      distributeMapBodyStms acc bnds

-- If the scan can be distributed by itself, we will turn it into a
-- segmented scan.
--
-- If the scan cannot be distributed by itself, it will be
-- sequentialised in the default case for this function.
maybeDistributeStm bnd@(Let pat _ (Op (Scanomap cs w lam fold_lam nes arrs))) acc =
  distributeSingleStm acc bnd >>= \case
    Just (kernels, res, nest, acc')
      | Just perm <- res `isPermutationOf` map Var (patternNames pat) -> do
          lam' <- FOT.transformLambda lam
          fold_lam' <- FOT.transformLambda fold_lam
          localScope (typeEnvFromKernelAcc acc') $
            segmentedScanomapKernel nest perm cs w lam' fold_lam' nes arrs >>=
            kernelOrNot bnd acc kernels acc'
    _ ->
      addStmToKernel bnd acc

-- If the reduction can be distributed by itself, we will turn it into a
-- segmented reduce.
--
-- If the reduction cannot be distributed by itself, it will be
-- sequentialised in the default case for this function.
maybeDistributeStm bnd@(Let pat _ (Op (Redomap cs w comm lam foldlam nes arrs))) acc | versionedCode =
  distributeSingleStm acc bnd >>= \case
    Just (kernels, res, nest, acc')
      | Just perm <- map Var (patternNames pat) `isPermutationOf` res ->
          localScope (typeEnvFromKernelAcc acc') $ do
          lam' <- FOT.transformLambda lam
          foldlam' <- FOT.transformLambda foldlam
          regularSegmentedRedomapKernel nest perm cs w comm lam' foldlam' nes arrs >>=
            kernelOrNot bnd acc kernels acc'
    _ ->
      addStmToKernel bnd acc

-- Redomap and Scanomap are general cases, so pretend nested
-- reductions and scans are Redomap and Scanomap.  Well, not for
-- Reduce, because of a hack...
maybeDistributeStm bnd@(Let pat _ (Op (Reduce cs w comm lam input))) acc =
  distributeSingleStm acc bnd >>= \case
    Just (kernels, res, nest, acc')
      | Just perm <- map Var (patternNames pat) `isPermutationOf` res ->
          localScope (typeEnvFromKernelAcc acc') $ do
          let (nes, arrs) = unzip input
          lam' <- FOT.transformLambda lam
          foldlam' <- renameLambda lam'
          regularSegmentedRedomapKernel nest perm cs w comm lam' foldlam' nes arrs >>=
            kernelOrNot bnd acc kernels acc'
    _ ->
      addStmToKernel bnd acc
maybeDistributeStm (Let pat attr (Op (Scan cs w lam input))) acc | versionedCode = do
  let (nes, arrs) = unzip input
  lam_renamed <- renameLambda lam
  let bnd = Let pat attr $ Op $ Scanomap cs w lam lam_renamed nes arrs
  maybeDistributeStm bnd acc

maybeDistributeStm (Let pat attr (BasicOp (Replicate (Shape (d:ds)) v))) acc
  | [t] <- patternTypes pat = do
      -- XXX: We need a temporary dummy binding to prevent an empty
      -- map body.  The kernel extractor does not like empty map
      -- bodies.
      tmp <- newVName "tmp"
      let rowt = rowType t
          newbnd = Let pat attr $ Op $ Map [] d lam []
          tmpbnd = Let (Pattern [] [PatElem tmp BindVar rowt]) () $
                   BasicOp $ Replicate (Shape ds) v
          lam = Lambda { lambdaReturnType = [rowt]
                       , lambdaParams = []
                       , lambdaBody = mkBody [tmpbnd] [Var tmp]
                       }
      maybeDistributeStm newbnd acc

maybeDistributeStm bnd@(Let _ _ (BasicOp Copy{})) acc =
  distributeSingleUnaryStm acc bnd $ \_ outerpat arr ->
  addKernel [Let outerpat () $ BasicOp $ Copy arr]

maybeDistributeStm bnd@(Let _ _ (BasicOp (Rearrange cs perm _))) acc =
  distributeSingleUnaryStm acc bnd $ \nest outerpat arr -> do
    let r = length (snd nest) + 1
        perm' = [0..r-1] ++ map (+r) perm
    addKernel [Let outerpat () $ BasicOp $ Rearrange cs perm' arr]

maybeDistributeStm bnd@(Let _ _ (BasicOp (Reshape cs reshape _))) acc =
  distributeSingleUnaryStm acc bnd $ \nest outerpat arr -> do
    let reshape' = map DimCoercion (kernelNestWidths nest) ++ reshape
    addKernel [Let outerpat () $ BasicOp $ Reshape cs reshape' arr]

maybeDistributeStm bnd acc =
  addStmToKernel bnd acc

distributeSingleUnaryStm :: KernelAcc
                             -> Stm
                             -> (KernelNest -> Pattern -> VName -> KernelM ())
                             -> KernelM KernelAcc
distributeSingleUnaryStm acc bnd f =
  distributeSingleStm acc bnd >>= \case
    Just (kernels, res, nest, acc')
      | res == map Var (patternNames $ bindingPattern bnd),
        (outer, _) <- nest,
        [(_, arr)] <- loopNestingParamsAndArrs outer -> do
          addKernels kernels
          let outerpat = loopNestingPattern $ fst nest
          f nest outerpat arr
          return acc'
    _ -> addStmToKernel bnd acc

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

distributeSingleStm :: KernelAcc -> Stm
                        -> KernelM (Maybe (PostKernels, Result, KernelNest, KernelAcc))
distributeSingleStm acc bnd = do
  nest <- asks kernelNest
  tryDistribute nest (kernelTargets acc) (kernelStms acc) >>= \case
    Nothing -> return Nothing
    Just (targets, distributed_bnds) ->
      tryDistributeStm nest targets bnd >>= \case
        Nothing -> return Nothing
        Just (res, targets', new_kernel_nest) ->
          return $ Just (PostKernels [PostKernel distributed_bnds],
                         res,
                         new_kernel_nest,
                         KernelAcc { kernelTargets = targets'
                                   , kernelStms = []
                                   })

segmentedScanomapKernel :: KernelNest
                        -> [Int]
                        -> Certificates -> SubExp
                        -> InKernelLambda -> InKernelLambda
                        -> [SubExp] -> [VName]
                        -> KernelM (Maybe [KernelsStm])
segmentedScanomapKernel nest perm cs segment_size lam fold_lam nes arrs =
  isSegmentedOp nest perm segment_size
  (lambdaReturnType lam) (freeInLambda lam) (freeInLambda fold_lam) nes arrs $
  \pat flat_pat _num_segments total_num_elements ispace inps nes' arrs' -> do
    regularSegmentedScan segment_size flat_pat cs total_num_elements
      lam fold_lam ispace inps nes' arrs'

    forM_ (zip (patternValueElements pat) (patternNames flat_pat)) $
      \(dst_pat_elem, flat) -> do
        let ident = patElemIdent dst_pat_elem
            bindage = patElemBindage dst_pat_elem
            dims = arrayDims $ identType ident
        addStm $ mkLet [] [(ident, bindage)] $
          BasicOp $ Reshape [] (map DimNew dims) flat

regularSegmentedRedomapKernel :: KernelNest
                              -> [Int]
                              -> Certificates -> SubExp -> Commutativity
                              -> InKernelLambda -> InKernelLambda -> [SubExp] -> [VName]
                              -> KernelM (Maybe [KernelsStm])
regularSegmentedRedomapKernel nest perm cs segment_size _comm lam fold_lam nes arrs =
  isSegmentedOp nest perm segment_size
  (lambdaReturnType fold_lam) (freeInLambda lam) (freeInLambda fold_lam) nes arrs $
  \pat flat_pat num_segments total_num_elements ispace inps nes' arrs' ->
    regularSegmentedRedomapAsScan
    segment_size num_segments (kernelNestWidths nest)
    flat_pat pat cs total_num_elements lam fold_lam ispace inps nes' arrs'

isSegmentedOp :: KernelNest
              -> [Int]
              -> SubExp
              -> [Type]
              -> Names -> Names
              -> [SubExp] -> [VName]
              -> (Pattern
                  -> Pattern
                  -> SubExp
                  -> SubExp
                  -> [(VName, SubExp)]
                  -> [KernelInput]
                  -> [SubExp] -> [VName]
                  -> Binder Out.Kernels ())
              -> KernelM (Maybe [KernelsStm])
isSegmentedOp nest perm segment_size ret free_in_op _free_in_fold_op nes arrs m = runMaybeT $ do
  -- We must verify that array inputs to the operation are inputs to
  -- the outermost loop nesting or free in the loop nest.  Nothing
  -- free in the op may be bound by the nest.  Furthermore, the
  -- neutral elements must be free in the loop nest.
  --
  -- We must summarise any names from free_in_op that are bound in the
  -- nest, and describe how to obtain them given segment indices.

  let bound_by_nest = boundInKernelNest nest

  (pre_bnds, nesting_size, ispace, kernel_inps, _rets) <- flatKernel nest

  unless (HS.null $ free_in_op `HS.intersection` bound_by_nest) $
    fail "Non-fold lambda uses nest-bound parameters."

  let indices = map fst ispace

      prepareNe (Var v) | v `HS.member` bound_by_nest =
                          fail "Neutral element bound in nest"
      prepareNe ne = return ne

      prepareArr arr =
        case find ((==arr) . kernelInputName) kernel_inps of
          Just inp | kernelInputIndices inp == map Var indices ->
            return $ return $ kernelInputArray inp
          Nothing | not (arr `HS.member` bound_by_nest) ->
                      -- This input is something that is free inside
                      -- the loop nesting. We will have to replicate
                      -- it.
                      return $ letExp (baseString arr ++ "_repd") $
                      BasicOp $ Replicate (Shape [nesting_size]) $ Var arr
          _ ->
            fail "Input not free or outermost."

  nes' <- mapM prepareNe nes

  mk_arrs <- mapM prepareArr arrs

  lift $ runBinder_ $ do
    mapM_ addStm pre_bnds

    -- We must make sure all inputs are of size
    -- segment_size*nesting_size.
    total_num_elements <-
      letSubExp "total_num_elements" $ BasicOp $ BinOp (Mul Int32) segment_size nesting_size

    let flatten arr = do
          arr_shape <- arrayShape <$> lookupType arr
          -- CHECKME: is the length the right thing here?  We want to
          -- reproduce the parameter type.
          let reshape = reshapeOuter [DimNew total_num_elements]
                        (2+length (snd nest)) arr_shape
          arr_manifest <- letExp (baseString arr ++ "_manifest") $
                          BasicOp $ Copy arr
          letExp (baseString arr ++ "_flat") $
            BasicOp $ Reshape [] reshape arr_manifest

    arrs' <- mapM flatten =<< sequence mk_arrs

    let pat = Pattern [] $ rearrangeShape perm $
              patternValueElements $ loopNestingPattern $ fst nest
        flatPatElem pat_elem t = do
          let t' = arrayOfRow t total_num_elements
          name <- newVName $ baseString (patElemName pat_elem) ++ "_flat"
          return $ PatElem name BindVar t'
    flat_pat <- Pattern [] <$>
                zipWithM flatPatElem
                (patternValueElements pat) ret

    m pat flat_pat nesting_size total_num_elements ispace kernel_inps nes' arrs'

containsNestedParallelism :: Lambda -> Bool
containsNestedParallelism lam =
  any (parallel . bindingExp) lam_bnds &&
  not (perfectMapNest lam_bnds)
  where lam_bnds = bodyStms $ lambdaBody lam
        parallel Op{} = True
        parallel _    = False
        perfectMapNest [Let _ _ (Op Map{})] = True
        perfectMapNest _                    = False

kernelAlternatives :: (MonadFreshNames m, HasScope Out.Kernels m) =>
                      SubExp -> Out.Pattern Out.Kernels
                   -> Out.Body Out.Kernels -> Out.Body Out.Kernels
                   -> m [Out.Stm Out.Kernels]
kernelAlternatives parallelism pat seq_body par_body = runBinder_ $ do
  suff <- letSubExp "sufficient_parallelism" $ Op $ SufficientParallelism parallelism
  letBind_ pat $ If suff seq_body par_body rettype
  where rettype = staticShapes $ patternTypes pat

kernelOrNot :: Stm -> KernelAcc
            -> PostKernels -> KernelAcc -> Maybe [KernelsStm]
            -> KernelM KernelAcc
kernelOrNot bnd acc _ _ Nothing =
  addStmToKernel bnd acc
kernelOrNot _ _ kernels acc' (Just bnds) = do
  addKernels kernels
  addKernel bnds
  return acc'
