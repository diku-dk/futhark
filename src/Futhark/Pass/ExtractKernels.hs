{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
-- Note that there may be further kernel extraction opportunities
-- inside the @map(f)@.  The downside of this approach is that the
-- intermediate array (@b@ above) must be written to main memory.  An
-- often better approach is to just turn the entire @redomap@ into a
-- single kernel.
--
module Futhark.Pass.ExtractKernels
       (extractKernels)
       where

import Control.Applicative
import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S
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
import Futhark.Util
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
  (x, msgs) <- modifyNameSource $ positionNameSource . runRWS m M.empty
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
sequentialisedUnbalancedStm (Let pat _ (Op soac@(Map _ lam _)))
  | unbalancedLambda lam, lambdaContainsParallelism lam = do
      types <- asksScope scopeForSOACs
      Just . snd <$> runBinderT (FOT.transformSOAC pat soac) types
sequentialisedUnbalancedStm (Let pat _ (Op soac@(Redomap _ _ _ lam2 _ _)))
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

transformStm (Let pat aux (If c tb fb rt)) = do
  tb' <- transformBody tb
  fb' <- transformBody fb
  return [Let pat aux $ If c tb' fb' rt]

transformStm (Let pat aux (DoLoop ctx val form body)) =
  localScope (castScope (scopeOf form) <>
              scopeOfFParams mergeparams) $ do
    body' <- transformBody body
    return [Let pat aux $ DoLoop ctx val form' body']
  where mergeparams = map fst $ ctx ++ val
        form' = case form of
                  WhileLoop cond ->
                    WhileLoop cond
                  ForLoop i it bound ps ->
                    ForLoop i it bound ps

transformStm (Let pat (StmAux cs _) (Op (Map w lam arrs))) =
  distributeMap pat $ MapLoop cs w lam arrs

transformStm (Let pat (StmAux cs _) (Op (Scanomap w lam1 lam2 nes arrs))) = do
  lam1_sequential <- Kernelise.transformLambda lam1
  lam2_sequential <- Kernelise.transformLambda lam2
  runBinder_ $ certifying cs $
    blockedScan pat w lam1_sequential lam2_sequential (intConst Int32 1) [] [] nes arrs

transformStm (Let pat (StmAux cs _) (Op (Redomap w comm lam1 lam2 nes arrs))) =
  if sequentialiseRedomapBody then do
    lam1_sequential <- Kernelise.transformLambda lam1
    lam2_sequential <- Kernelise.transformLambda lam2
    map (certify cs) <$>
      blockedReduction pat w comm' lam1_sequential lam2_sequential nes arrs
  else do
    (mapbnd, redbnd) <- redomapToMapAndReduce pat (w, comm', lam1, lam2, nes, arrs)
    transformStms [certify cs mapbnd, certify cs redbnd]
      where sequentialiseRedomapBody = True
            comm' | commutativeLambda lam1 = Commutative
                  | otherwise              = comm

transformStm (Let res_pat (StmAux cs _) (Op (Reduce w comm red_fun red_input)))
  | Just do_irwim <- irwim res_pat w comm' red_fun red_input = do
      types <- asksScope scopeForSOACs
      bnds <- fst <$> runBinderT (simplifyStms =<< collectStms_ (certifying cs do_irwim)) types
      transformStms bnds
        where comm' | commutativeLambda red_fun = Commutative
                    | otherwise                 = comm


transformStm (Let pat (StmAux cs _) (Op (Reduce w comm red_fun red_input))) = do
  red_fun_sequential <- Kernelise.transformLambda red_fun
  red_fun_sequential' <- renameLambda red_fun_sequential
  map (certify cs) <$>
    blockedReduction pat w comm' red_fun_sequential' red_fun_sequential nes arrs
  where (nes, arrs) = unzip red_input
        comm' | commutativeLambda red_fun = Commutative
              | otherwise                 = comm


transformStm (Let res_pat (StmAux cs _) (Op (Scan w scan_fun scan_input)))
  | Just do_iswim <- iswim res_pat w scan_fun scan_input = do
      types <- asksScope scopeForSOACs
      transformStms =<< (snd <$> runBinderT (certifying cs do_iswim) types)

transformStm (Let pat (StmAux cs _) (Op (Scan w fun input))) = do
  fun_sequential <- Kernelise.transformLambda fun
  fun_sequential_renamed <- renameLambda fun_sequential
  runBinder_ $ certifying cs $
    blockedScan pat w fun_sequential fun_sequential_renamed (intConst Int32 1) [] [] nes arrs
  where (nes, arrs) = unzip input

-- Streams can be handled in two different ways - either we
-- sequentialise the body or we keep it parallel and distribute.
transformStm (Let pat (StmAux cs _) (Op (Stream w (Parallel _ _ _ []) map_fun arrs))) = do
  -- No reduction part.  Remove the stream and leave the body
  -- parallel.  It will be distributed.
  types <- asksScope scopeForSOACs
  transformStms =<<
    (snd <$> runBinderT (certifying cs $ sequentialStreamWholeArray pat w [] map_fun arrs) types)

transformStm (Let pat aux (Op (Stream w (Parallel _o comm red_fun nes) fold_fun arrs)))
  | any (not . primType) $ lambdaReturnType red_fun,
    Just fold_fun' <- extLambdaToLambda fold_fun  = do
  -- Split into a chunked map and a reduction, with the latter
  -- distributed.

  fold_fun_sequential <- Kernelise.transformLambda fold_fun'

  let (red_pat_elems, concat_pat_elems) =
        splitAt (length nes) $ patternValueElements pat
      red_pat = Pattern [] red_pat_elems
      concat_pat = Pattern [] concat_pat_elems

  (map_bnd, map_misc_bnds) <- blockedMap concat_pat w InOrder fold_fun_sequential nes arrs
  let num_threads = arraysSize 0 $ patternTypes $ stmPattern map_bnd
      red_input = zip nes $ patternNames $ stmPattern map_bnd

  ((map_misc_bnds++[map_bnd])++) <$>
    inScopeOf (map_misc_bnds++[map_bnd])
    (transformStm $ Let red_pat aux $
     Op (Reduce num_threads comm' red_fun red_input))
    where comm' | commutativeLambda red_fun = Commutative
                | otherwise                 = comm


transformStm (Let pat _ (Op (Stream w
                               (Parallel o comm red_fun nes) fold_fun arrs)))
  | Just fold_fun' <- extLambdaToLambda fold_fun = do
  -- Generate a kernel immediately.
  red_fun_sequential <- Kernelise.transformLambda red_fun
  fold_fun_sequential <- Kernelise.transformLambda fold_fun'
  blockedReductionStream pat w comm' red_fun_sequential fold_fun_sequential nes arrs
  where comm' | commutativeLambda red_fun, o /= InOrder = Commutative
              | otherwise                               = comm

transformStm (Let pat _ (Op (Stream w (Sequential nes) fold_fun arrs))) = do
  -- Remove the stream and leave the body parallel.  It will be
  -- distributed.
  types <- asksScope scopeForSOACs
  transformStms =<<
    (snd <$> runBinderT (sequentialStreamWholeArray pat w nes fold_fun arrs) types)

transformStm (Let pat (StmAux cs _) (Op (Scatter w lam ivs as))) = runBinder_ $ do
  lam' <- Kernelise.transformLambda lam
  write_i <- newVName "write_i"
  let (i_res, v_res) = splitAt (length as) $ bodyResult $ lambdaBody lam'
      kstms = bodyStms $ lambdaBody lam'
      krets = do (i, v, (a_w, a)) <- zip3 i_res v_res as
                 return $ WriteReturn a_w a i v
      body = KernelBody () kstms krets
      inputs = do (p, p_a) <- zip (lambdaParams lam') ivs
                  return $ KernelInput (paramName p) (paramType p) p_a [Var write_i]
  (bnds, kernel) <-
    mapKernel w (FlatThreadSpace [(write_i,w)]) inputs (map rowType $ patternTypes pat) body
  certifying cs $ do
    mapM_ addStm bnds
    letBind_ pat $ Op kernel

transformStm bnd =
  runBinder_ $ FOT.transformStmRecursively bnd

data MapLoop = MapLoop Certificates SubExp Lambda [VName]

mapLoopExp :: MapLoop -> Exp
mapLoopExp (MapLoop _ w lam arrs) = Op $ Map w lam arrs

distributeMap :: (HasScope Out.Kernels m,
                  MonadFreshNames m, MonadLogger m) =>
                 Pattern -> MapLoop -> m [KernelsStm]
distributeMap pat (MapLoop cs w lam arrs) = do
  types <- askScope
  let loopnest = MapNesting pat cs w $ zip (lambdaParams lam) arrs
      env = KernelEnv { kernelNest =
                        singleNesting (Nesting mempty loopnest)
                      , kernelScope =
                        scopeForKernels (scopeOf lam) <> types
                      }
  let res = map Var $ patternNames pat
  par_stms <- fmap (postKernelsStms . snd) $ runKernelM env $
    distribute =<< distributeMapBodyStms acc (bodyStms $ lambdaBody lam)

  if not versionedCode || not (containsNestedParallelism lam)
    then return par_stms
    else do
    par_body <- renameBody $ mkBody par_stms res

    seq_stms <- do
      soactypes <- asksScope scopeForSOACs
      (seq_lam, _) <- runBinderT (Kernelise.transformLambda lam) soactypes
      fmap (postKernelsStms . snd) $ runKernelM env $ distribute $
        addStmsToKernel (bodyStms $ lambdaBody seq_lam) acc
    seq_body <- renameBody $ mkBody seq_stms res
    (outer_suff, outer_suff_stms) <- runBinder $
      letSubExp "outer_suff_par" $ Op $ SufficientParallelism w

    intra_stms <- flip runReaderT types $ localScope (scopeOfLParams (lambdaParams lam)) $
                  intraGroupParallelise (newKernel loopnest) $ lambdaBody lam
    group_par_body <- renameBody $ mkBody intra_stms res

    (intra_suff, intra_suff_stms) <- runBinder $ do
      group_size <- letSubExp "group_size" $ Op GroupSize
      group_available_par <-
        letSubExp "group_available_par" $ BasicOp $ BinOp (Mul Int32) w group_size
      if isJust $ lookup "FUTHARK_INTRA_GROUP_PARALLELISM" unixEnvironment then
        letSubExp "group_suff_par" $ Op $ SufficientParallelism group_available_par
      else return $ constant False

    ((outer_suff_stms++intra_suff_stms)++) <$>
      kernelAlternatives pat par_body [(outer_suff, seq_body),
                                       (intra_suff, group_par_body)]
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
typeEnvFromKernelAcc = scopeOfPattern . fst . outerTarget . kernelTargets

addStmsToKernel :: [InKernelStm] -> KernelAcc -> KernelAcc
addStmsToKernel stms acc =
  acc { kernelStms = stms <> kernelStms acc }

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
    env { kernelScope = types <> kernelScope env }

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
          scopeForKernels (scopeOf [bnd]) <> kernelScope env
      , kernelNest =
          letBindInInnerNesting provided $
          kernelNest env
      }
  where provided = S.fromList $ patternNames $ stmPattern bnd

mapNesting :: Pattern -> Certificates -> SubExp -> Lambda -> [VName]
           -> KernelM a
           -> KernelM a
mapNesting pat cs w lam arrs = local $ \env ->
  env { kernelNest = pushInnerNesting nest $ kernelNest env
      , kernelScope =  scopeForKernels (scopeOf lam) <> kernelScope env
      }
  where nest = Nesting mempty $
               MapNesting pat cs w $
               zip (lambdaParams lam) arrs

inNesting :: KernelNest -> KernelM a -> KernelM a
inNesting (outer, nests) = local $ \env ->
  env { kernelNest = (inner, nests')
      , kernelScope =  mconcat (map scopeOf $ outer : nests) <> kernelScope env
      }
  where (inner, nests') =
          case reverse nests of
            []           -> (asNesting outer, [])
            (inner' : ns) -> (asNesting inner', map asNesting $ outer : reverse ns)
        asNesting = Nesting mempty

unbalancedLambda :: Lambda -> Bool
unbalancedLambda lam =
  unbalancedBody
  (S.fromList $ map paramName $ lambdaParams lam) $
  lambdaBody lam

  where subExpBound (Var i) bound = i `S.member` bound
        subExpBound (Constant _) _ = False

        unbalancedBody bound body =
          any (unbalancedStm (bound <> boundInBody body) . stmExp) $
          bodyStms body

        -- XXX - our notion of balancing is probably still too naive.
        unbalancedStm bound (Op (Map w _ _)) =
          w `subExpBound` bound
        unbalancedStm bound (Op (Reduce w _ _ _)) =
          w `subExpBound` bound
        unbalancedStm bound (Op (Scan w _ _)) =
          w `subExpBound` bound
        unbalancedStm bound (Op (Redomap w _ _ _ _ _)) =
          w `subExpBound` bound
        unbalancedStm bound (Op (Scanomap w _ _ _ _)) =
          w `subExpBound` bound
        unbalancedStm bound (Op (Stream w _ _ _)) =
          w `subExpBound` bound
        unbalancedStm _ (Op Scatter{}) =
          False
        unbalancedStm bound (DoLoop _ merge (ForLoop i _ iterations _) body) =
          iterations `subExpBound` bound ||
          unbalancedBody bound' body
          where bound' = foldr S.insert bound $
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
bodyContainsParallelism = any (isMap . stmExp) . bodyStms
  where isMap Op{} = True
        isMap _ = False

bodyContainsMap :: Body -> Bool
bodyContainsMap = any (isMap . stmExp) . bodyStms
  where isMap (Op Map{}) = True
        isMap _ = False

lambdaContainsParallelism :: Lambda -> Bool
lambdaContainsParallelism = bodyContainsParallelism . lambdaBody

-- | Returns the sizes of immediate nested parallelism.
nestedParallelism :: Body -> [SubExp]
nestedParallelism = concatMap (parallelism . stmExp) . bodyStms
  where parallelism (Op (Reduce w _ _ _)) = [w]
        parallelism (Op (Scan w _ _)) = [w]
        parallelism (Op (Scanomap w _ _ _ _)) = [w]
        parallelism (Op (Redomap w _ _ _ _ _)) = [w]
        parallelism (Op (Map w _ _)) = [w]
        parallelism (Op (Stream w Sequential{} lam _))
          | chunk_size_param : _ <- extLambdaParams lam =
              let update (Var v) | v == paramName chunk_size_param = w
                  update se = se
              in map update $ nestedParallelism $ extLambdaBody lam
        parallelism _ = []

containsNestedParallelism :: Lambda -> Bool
containsNestedParallelism lam =
  not (null $ nestedParallelism $ lambdaBody lam) &&
  not (perfectMapNest $ bodyStms $ lambdaBody lam)
  where perfectMapNest [Let _ _ (Op Map{})] = True
        perfectMapNest _                    = False

-- Enable if you want the cool new versioned code.  Beware: may be
-- slower in practice.  Caveat emptor (and you are the emptor).
versionedCode :: Bool
versionedCode = isJust $ lookup "FUTHARK_VERSIONED_CODE" unixEnvironment

distributeInnerMap :: Pattern -> MapLoop -> KernelAcc
                   -> KernelM KernelAcc
distributeInnerMap pat maploop@(MapLoop cs w lam arrs) acc
  | unbalancedLambda lam, lambdaContainsParallelism lam =
      addStmToKernel (Let pat (StmAux cs ()) $ mapLoopExp maploop) acc
  | not versionedCode || not (containsNestedParallelism lam) =
      distributeNormally
  | otherwise =
      distributeSingleStm acc (Let pat (StmAux cs ()) $ mapLoopExp maploop) >>= \case
      Nothing ->
        distributeNormally
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

        (parw_bnds, parw, sequentialised_kernel) <- localScope extra_scope $ do
          sequentialised_map_body <-
            localScope (scopeOfLParams (lambdaParams lam)) $ runBinder_ $
            Kernelise.transformStms lam_bnds
          let kbody = KernelBody () sequentialised_map_body $
                      map (ThreadsReturn ThreadsInSpace) lam_res
          constructKernel nest' kbody

        let outer_pat = loopNestingPattern $ fst nest
            res' = map Var $ patternNames outer_pat
        seq_body <- renameBody $ mkBody [sequentialised_kernel] res'
        par_body <- renameBody $ mkBody (postKernelsStms distributed_kernels) res'
        (sufficient_parallelism, sufficient_stms) <- runBinder $
          letSubExp "sufficient_parallelism" $ Op $ SufficientParallelism parw
        addKernel =<< kernelAlternatives outer_pat
          par_body [(sufficient_parallelism,seq_body)]
        addKernel $ parw_bnds ++ sufficient_stms
        return acc'
      where lam_bnds = bodyStms $ lambdaBody lam
            lam_res = bodyResult $ lambdaBody lam

            def_acc = KernelAcc { kernelTargets = pushInnerTarget
                                  (pat, bodyResult $ lambdaBody lam) $
                                  kernelTargets acc
                                , kernelStms = mempty
                                }

            distributeNormally =
              distribute =<<
              leavingNesting maploop =<<
              mapNesting pat cs w lam arrs
              (distribute =<< distributeMapBodyStms def_acc lam_bnds)

leavingNesting :: MapLoop -> KernelAcc -> KernelM KernelAcc
leavingNesting (MapLoop cs w lam arrs) acc =
  case popInnerTarget $ kernelTargets acc of
   Nothing ->
     fail "The kernel targets list is unexpectedly small"
   Just ((pat,res), newtargets) -> do
     let acc' = acc { kernelTargets = newtargets }
     case kernelStms acc' of
       []      -> return acc'
       remnant -> do
         let kbody = Body () remnant res
             used_in_body = freeInBody kbody
             (used_params, used_arrs) =
               unzip $
               filter ((`S.member` used_in_body) . paramName . fst) $
               zip (lambdaParams lam) arrs
         stms <- runBinder_ $ Kernelise.mapIsh pat cs w used_params kbody used_arrs
         return $ addStmsToKernel stms acc' { kernelStms = [] }

distributeMapBodyStms :: KernelAcc -> [Stm] -> KernelM KernelAcc

distributeMapBodyStms acc [] =
  return acc

distributeMapBodyStms acc
  (Let pat (StmAux cs _) (Op (Stream w (Sequential accs) lam arrs)):bnds) = do
    types <- asksScope scopeForSOACs
    stream_bnds <-
      snd <$> runBinderT (sequentialStreamWholeArray pat w accs lam arrs) types
    stream_bnds' <-
      runReaderT (copyPropagateInStms simpleSOACS stream_bnds) types
    distributeMapBodyStms acc $ map (certify cs) stream_bnds' ++ bnds

distributeMapBodyStms acc (bnd:bnds) =
  -- It is important that bnd is in scope if 'maybeDistributeStm'
  -- wants to distribute, even if this causes the slightly silly
  -- situation that bnd is in scope of itself.
  withStm bnd $
  maybeDistributeStm bnd =<<
  distributeMapBodyStms acc bnds

maybeDistributeStm :: Stm -> KernelAcc
                       -> KernelM KernelAcc
maybeDistributeStm bnd@(Let pat _ (Op (Map w lam arrs))) acc =
  -- Only distribute inside the map if we can distribute everything
  -- following the map.
  distributeIfPossible acc >>= \case
    Nothing -> addStmToKernel bnd acc
    Just acc' -> distribute =<< distributeInnerMap pat (MapLoop (stmCerts bnd) w lam arrs) acc'

maybeDistributeStm bnd@(Let pat _ (DoLoop [] val form body)) acc
  | null (patternContextElements pat), bodyContainsMap body =
  distributeSingleStm acc bnd >>= \case
    Just (kernels, res, nest, acc')
      | S.null $ freeIn form `S.intersection` boundInKernelNest nest,
        Just (perm, pat_unused) <- permutationAndMissing pat res ->
          -- We need to pretend pat_unused was used anyway, by adding
          -- it to the kernel nest.
          localScope (typeEnvFromKernelAcc acc') $ do
          nest' <- expandKernelNest pat_unused nest
          addKernels kernels
          types <- asksScope scopeForSOACs
          bnds <- runReaderT
                  (interchangeLoops nest' (SeqLoop perm pat val form body)) types
          -- runDistribM starts out with an empty scope, so we have to
          -- immmediately insert the real one.
          scope <- askScope
          bnds' <- runDistribM $ localScope scope $ transformStms bnds
          addKernel bnds'
          return acc'
    _ ->
      addStmToKernel bnd acc

maybeDistributeStm (Let pat (StmAux cs _) (Op (Reduce w comm lam input))) acc
  | Just m <- irwim pat w comm lam input = do
      types <- asksScope scopeForSOACs
      (_, bnds) <- runBinderT (certifying cs m) types
      distributeMapBodyStms acc bnds

-- If the scan can be distributed by itself, we will turn it into a
-- segmented scan.
--
-- If the scan cannot be distributed by itself, it will be
-- sequentialised in the default case for this function.
maybeDistributeStm bnd@(Let pat (StmAux cs _) (Op (Scanomap w lam fold_lam nes arrs))) acc =
  distributeSingleStm acc bnd >>= \case
    Just (kernels, res, nest, acc')
      | Just (perm, pat_unused) <- permutationAndMissing pat res ->
          -- We need to pretend pat_unused was used anyway, by adding
          -- it to the kernel nest.
          localScope (typeEnvFromKernelAcc acc') $ do
          nest' <- expandKernelNest pat_unused nest
          lam' <- Kernelise.transformLambda lam
          fold_lam' <- Kernelise.transformLambda fold_lam
          localScope (typeEnvFromKernelAcc acc') $
            segmentedScanomapKernel nest' perm w lam' fold_lam' nes arrs >>=
            kernelOrNot cs bnd acc kernels acc'
    _ ->
      addStmToKernel bnd acc

-- If the reduction can be distributed by itself, we will turn it into a
-- segmented reduce.
--
-- If the reduction cannot be distributed by itself, it will be
-- sequentialised in the default case for this function.
maybeDistributeStm bnd@(Let pat (StmAux cs _) (Op (Redomap w comm lam foldlam nes arrs))) acc | versionedCode =
  distributeSingleStm acc bnd >>= \case
    Just (kernels, res, nest, acc')
      | Just (perm, pat_unused) <- permutationAndMissing pat res ->
          -- We need to pretend pat_unused was used anyway, by adding
          -- it to the kernel nest.
          localScope (typeEnvFromKernelAcc acc') $ do
          nest' <- expandKernelNest pat_unused nest
          lam' <- Kernelise.transformLambda lam
          foldlam' <- Kernelise.transformLambda foldlam
          regularSegmentedRedomapKernel nest' perm w comm' lam' foldlam' nes arrs >>=
            kernelOrNot cs bnd acc kernels acc'
    _ ->
      addStmToKernel bnd acc
    where comm' | commutativeLambda lam = Commutative
                | otherwise             = comm

-- Redomap and Scanomap are general cases, so pretend nested
-- reductions and scans are Redomap and Scanomap.  Well, not for
-- Reduce, because of a hack...
maybeDistributeStm bnd@(Let pat (StmAux cs _) (Op (Reduce w comm lam input))) acc =
  distributeSingleStm acc bnd >>= \case
    Just (kernels, res, nest, acc')
      | Just (perm, pat_unused) <- permutationAndMissing pat res ->
          -- We need to pretend pat_unused was used anyway, by adding
          -- it to the kernel nest.
          localScope (typeEnvFromKernelAcc acc') $ do
          let (nes, arrs) = unzip input
          nest' <- expandKernelNest pat_unused nest
          lam' <- Kernelise.transformLambda lam
          foldlam' <- renameLambda lam'
          regularSegmentedRedomapKernel nest' perm w comm' lam' foldlam' nes arrs >>=
            kernelOrNot cs bnd acc kernels acc'
    _ ->
      addStmToKernel bnd acc
    where comm' | commutativeLambda lam = Commutative
                | otherwise             = comm

maybeDistributeStm (Let pat aux (Op (Scan w lam input))) acc = do
  let (nes, arrs) = unzip input
  lam_renamed <- renameLambda lam
  let bnd = Let pat aux $ Op $ Scanomap w lam lam_renamed nes arrs
  maybeDistributeStm bnd acc

maybeDistributeStm (Let pat aux (BasicOp (Replicate (Shape (d:ds)) v))) acc
  | [t] <- patternTypes pat = do
      -- XXX: We need a temporary dummy binding to prevent an empty
      -- map body.  The kernel extractor does not like empty map
      -- bodies.
      tmp <- newVName "tmp"
      let rowt = rowType t
          newbnd = Let pat aux $ Op $ Map d lam []
          tmpbnd = Let (Pattern [] [PatElem tmp BindVar rowt]) aux $
                   BasicOp $ Replicate (Shape ds) v
          lam = Lambda { lambdaReturnType = [rowt]
                       , lambdaParams = []
                       , lambdaBody = mkBody [tmpbnd] [Var tmp]
                       }
      maybeDistributeStm newbnd acc

maybeDistributeStm bnd@(Let _ aux (BasicOp Copy{})) acc =
  distributeSingleUnaryStm acc bnd $ \_ outerpat arr ->
  addKernel [Let outerpat aux $ BasicOp $ Copy arr]

-- Opaques are applied to the full array, because otherwise they can
-- drastically inhibit parallelisation in some cases.
maybeDistributeStm bnd@(Let (Pattern [] [pe]) aux (BasicOp Opaque{})) acc
  | not $ primType $ typeOf pe =
      distributeSingleUnaryStm acc bnd $ \_ outerpat arr ->
      addKernel [Let outerpat aux $ BasicOp $ Copy arr]

maybeDistributeStm bnd@(Let _ aux (BasicOp (Rearrange perm _))) acc =
  distributeSingleUnaryStm acc bnd $ \nest outerpat arr -> do
    let r = length (snd nest) + 1
        perm' = [0..r-1] ++ map (+r) perm
    addKernel [Let outerpat aux $ BasicOp $ Rearrange perm' arr]

maybeDistributeStm bnd@(Let _ aux (BasicOp (Reshape reshape _))) acc =
  distributeSingleUnaryStm acc bnd $ \nest outerpat arr -> do
    let reshape' = map DimNew (kernelNestWidths nest) ++
                   map DimNew (newDims reshape)
    addKernel [Let outerpat aux $ BasicOp $ Reshape reshape' arr]

maybeDistributeStm bnd acc =
  addStmToKernel bnd acc

distributeSingleUnaryStm :: KernelAcc
                             -> Stm
                             -> (KernelNest -> Pattern -> VName -> KernelM ())
                             -> KernelM KernelAcc
distributeSingleUnaryStm acc bnd f =
  distributeSingleStm acc bnd >>= \case
    Just (kernels, res, nest, acc')
      | res == map Var (patternNames $ stmPattern bnd),
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
                        -> SubExp
                        -> InKernelLambda -> InKernelLambda
                        -> [SubExp] -> [VName]
                        -> KernelM (Maybe [KernelsStm])
segmentedScanomapKernel nest perm segment_size lam fold_lam nes arrs =
  isSegmentedOp nest perm segment_size
  (lambdaReturnType fold_lam) (freeInLambda lam) (freeInLambda fold_lam) nes arrs $
  \pat flat_pat _num_segments total_num_elements ispace inps nes' arrs' -> do
    regularSegmentedScan segment_size flat_pat total_num_elements
      lam fold_lam ispace inps nes' arrs'

    forM_ (zip (patternValueElements pat) (patternNames flat_pat)) $
      \(dst_pat_elem, flat) -> do
        let ident = patElemIdent dst_pat_elem
            bindage = patElemBindage dst_pat_elem
            dims = arrayDims $ identType ident
        addStm $ mkLet [] [(ident, bindage)] $
          BasicOp $ Reshape (map DimNew dims) flat

regularSegmentedRedomapKernel :: KernelNest
                              -> [Int]
                              -> SubExp -> Commutativity
                              -> InKernelLambda -> InKernelLambda -> [SubExp] -> [VName]
                              -> KernelM (Maybe [KernelsStm])
regularSegmentedRedomapKernel nest perm segment_size comm lam fold_lam nes arrs =
  isSegmentedOp nest perm segment_size
    (lambdaReturnType fold_lam) (freeInLambda lam) (freeInLambda fold_lam) nes arrs $
    \pat flat_pat num_segments total_num_elements ispace inps nes' arrs' ->
      regularSegmentedRedomap
        segment_size num_segments (kernelNestWidths nest)
        flat_pat pat total_num_elements comm lam fold_lam ispace inps nes' arrs'

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

  unless (S.null $ free_in_op `S.intersection` bound_by_nest) $
    fail "Non-fold lambda uses nest-bound parameters."

  let indices = map fst ispace

      prepareNe (Var v) | v `S.member` bound_by_nest =
                          fail "Neutral element bound in nest"
      prepareNe ne = return ne

      prepareArr arr =
        case find ((==arr) . kernelInputName) kernel_inps of
          Just inp
            | kernelInputIndices inp == map Var indices ->
                return $ return $ kernelInputArray inp
            | not (kernelInputArray inp `S.member` bound_by_nest) ->
                return $ replicateMissing ispace inp
          Nothing | not (arr `S.member` bound_by_nest) ->
                      -- This input is something that is free inside
                      -- the loop nesting. We will have to replicate
                      -- it.
                      return $
                      letExp (baseString arr ++ "_repd")
                      (BasicOp $ Replicate (Shape [nesting_size]) $ Var arr)
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
          letExp (baseString arr ++ "_flat") $
            BasicOp $ Reshape reshape arr

    nested_arrs <- sequence mk_arrs
    arrs' <- mapM flatten nested_arrs

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

  where replicateMissing ispace inp = do
          t <- lookupType $ kernelInputArray inp
          let inp_is = kernelInputIndices inp
              shapes = determineRepeats ispace inp_is
              (outer_shapes, inner_shape) = repeatShapes shapes t
          letExp "repeated" $ BasicOp $
            Repeat outer_shapes inner_shape $ kernelInputArray inp

        determineRepeats ((gtid,n):ispace) (i:is)
          | Var gtid == i =
              Shape [] : determineRepeats ispace is
          | otherwise =
            Shape [n] : determineRepeats ispace (i:is)
        determineRepeats ispace _ =
          [Shape $ map snd ispace]

permutationAndMissing :: Pattern -> [SubExp] -> Maybe ([Int], [PatElem])
permutationAndMissing pat res = do
  let pes = patternValueElements pat
      (_used,unused) =
        partition ((`S.member` freeIn res) . patElemName) pes
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

-- | Convert the statements inside a map nest to kernel statements,
-- attempting to parallelise any remaining (top-level) parallel
-- statements.  Anything that is not a map, scan or reduction will
-- simply be sequentialised.  This includes sequential loops that
-- contain maps, scans or reduction.  In the future, we could probably
-- do something more clever.  Make sure that the amount of parallelism
-- to be exploited does not exceed the group size.
intraGroupParallelise :: (MonadFreshNames m,
                          HasScope Out.Kernels m) =>
                         KernelNest -> Body
                      -> m [Out.Stm Out.Kernels]
intraGroupParallelise knest body = do
  (w_stms, w, ispace, inps, rts) <- flatKernel knest
  let num_groups = w

  ((kspace, read_input_stms), prelude_stms) <- runBinder $ do
    let inputIsUsed input = kernelInputName input `S.member` freeInBody body
        used_inps = filter inputIsUsed inps

    mapM_ addStm w_stms
    group_size_v <- newVName "group_size"
    letBindNames'_ [group_size_v] $ Op GroupSize

    num_threads <- letSubExp "num_threads" $
                   BasicOp $ BinOp (Mul Int32) num_groups (Var group_size_v)

    let ksize = (num_groups, Var group_size_v, num_threads)

    ltid <- newVName "ltid"

    kspace <- newKernelSpace ksize $ FlatThreadSpace $ ispace ++ [(ltid,Var group_size_v)]

    read_input_stms <- mapM readKernelInput used_inps

    return (kspace, read_input_stms)

  kbody <- intraGroupParalleliseBody kspace body

  let kbody' = kbody { kernelBodyStms = read_input_stms ++ kernelBodyStms kbody }
      kstm = Let (loopNestingPattern first_nest) (StmAux cs ()) $ Op $
             Kernel (KernelDebugHints "map_intra_group" []) kspace rts kbody'

  return $ prelude_stms ++ [kstm]
  where first_nest = fst knest
        cs = loopNestingCertificates first_nest

intraGroupParalleliseBody :: (MonadFreshNames m,
                              HasScope Out.Kernels m) =>
                             KernelSpace -> Body -> m (Out.KernelBody Out.InKernel)
intraGroupParalleliseBody kspace body = do
  let ltid = spaceLocalId kspace
  kstms <- runBinder_ $ do
    let processStms = mapM_ processStm

        -- Without this type signature, the GHC 8.0.1 type checker
        -- enters an infinite loop.
        processStm :: Stm -> Binder Out.InKernel ()
        processStm stm@(Let pat _ e) =
          case e of
            Op (Map w fun arrs) -> do
              body_stms <- collectStms_ $ do
                forM_ (zip (lambdaParams fun) arrs) $ \(p, arr) -> do
                  arr_t <- lookupType arr
                  letBindNames' [paramName p] $ BasicOp $ Index arr $
                    fullSlice arr_t [DimFix $ Var ltid]
                Kernelise.transformStms $ bodyStms $ lambdaBody fun
              let comb_body = mkBody body_stms $ bodyResult $ lambdaBody fun
              letBind_ pat $ Op $
                Out.Combine [(ltid,w)] (lambdaReturnType fun) [] comb_body

            Op (Scanomap w scanfun foldfun nes arrs) -> do
              let (scan_pes, map_pes) =
                    splitAt (length nes) $ patternElements pat
              scan_input <- procInput (Pattern [] map_pes) w foldfun nes arrs

              scanfun' <- Kernelise.transformLambda scanfun

              -- A GroupScan lambda needs two more parameters.
              my_index <- newVName "my_index"
              other_index <- newVName "other_index"
              let my_index_param = Param my_index (Prim int32)
                  other_index_param = Param other_index (Prim int32)
                  scanfun'' = scanfun' { lambdaParams = my_index_param :
                                                        other_index_param :
                                                        lambdaParams scanfun'
                                       }
              letBind_ (Pattern [] scan_pes) $
                Op $ Out.GroupScan w scanfun'' $ zip nes scan_input

            Op (Redomap w _ redfun foldfun nes arrs) -> do
              let (red_pes, map_pes) =
                    splitAt (length nes) $ patternElements pat
              red_input <- procInput (Pattern [] map_pes) w foldfun nes arrs

              redfun' <- Kernelise.transformLambda redfun

              -- A GroupReduce lambda needs two more parameters.
              my_index <- newVName "my_index"
              other_index <- newVName "other_index"
              let my_index_param = Param my_index (Prim int32)
                  other_index_param = Param other_index (Prim int32)
                  redfun'' = redfun' { lambdaParams = my_index_param :
                                                      other_index_param :
                                                      lambdaParams redfun'
                                       }
              letBind_ (Pattern [] red_pes) $
                Op $ Out.GroupReduce w redfun'' $ zip nes red_input

            Op (Stream w (Sequential accs) lam arrs) -> do

              types <- asksScope castScope
              ((), stream_bnds) <-
                runBinderT (sequentialStreamWholeArray pat w accs lam arrs) types
              processStms stream_bnds
            _ ->
              Kernelise.transformStm stm

          where procInput :: Out.Pattern Out.InKernel
                          -> SubExp -> Lambda -> [SubExp] -> [VName]
                          -> Binder Out.InKernel [VName]
                procInput map_pat w foldfun nes arrs = do
                  fold_stms <- collectStms_ $ do
                    let (fold_acc_params, fold_arr_params) =
                          splitAt (length nes) $ lambdaParams foldfun

                    forM_ (zip fold_acc_params nes) $ \(p, ne) ->
                      letBindNames'_ [paramName p] $ BasicOp $ SubExp ne

                    forM_ (zip fold_arr_params arrs) $ \(p, arr) -> do
                      arr_t <- lookupType arr
                      letBindNames' [paramName p] $ BasicOp $ Index arr $
                        fullSlice arr_t [DimFix $ Var ltid]

                    Kernelise.transformStms $ bodyStms $ lambdaBody foldfun
                  let fold_body = mkBody fold_stms $ bodyResult $ lambdaBody foldfun

                  op_inps <- replicateM (length nes) (newVName "op_input")
                  letBindNames'_ (op_inps ++ patternNames map_pat) $ Op $
                    Out.Combine [(ltid,w)] (lambdaReturnType foldfun) [] fold_body
                  return op_inps

    processStms $ bodyStms body

  return $ KernelBody () kstms $ map (ThreadsReturn $ OneThreadPerGroup $ intConst Int32 0) $ bodyResult body

kernelAlternatives :: (MonadFreshNames m, HasScope Out.Kernels m) =>
                      Out.Pattern Out.Kernels
                   -> Out.Body Out.Kernels
                   -> [(SubExp, Out.Body Out.Kernels)]
                   -> m [Out.Stm Out.Kernels]
kernelAlternatives pat default_body [] = runBinder_ $ do
  ses <- bodyBind default_body
  forM_ (zip (patternNames pat) ses) $ \(name, se) ->
    letBindNames'_ [name] $ BasicOp $ SubExp se
kernelAlternatives pat default_body ((cond,alt):alts) = runBinder_ $ do
  alts_pat <- fmap (Pattern []) $ forM (patternElements pat) $ \pe -> do
    name <- newVName $ baseString $ patElemName pe
    return pe { patElemName = name }

  alt_stms <- kernelAlternatives alts_pat default_body alts
  let alt_body = mkBody alt_stms $ map Var $ patternValueNames alts_pat

  letBind_ pat $ If cond alt alt_body $ ifCommon $ patternTypes pat

kernelOrNot :: Certificates -> Stm -> KernelAcc
            -> PostKernels -> KernelAcc -> Maybe [KernelsStm]
            -> KernelM KernelAcc
kernelOrNot cs bnd acc _ _ Nothing =
  addStmToKernel (certify cs bnd) acc
kernelOrNot cs _ _ kernels acc' (Just bnds) = do
  addKernels kernels
  addKernel $ map (certify cs) bnds
  return acc'
