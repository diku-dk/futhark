{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Monad.Trans.Maybe
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe
import Data.List

import Prelude

import Futhark.Optimise.Simplifier.Simple (bindableSimpleOps)
import Futhark.Representation.Basic
import Futhark.MonadFreshNames
import Futhark.Tools
import qualified Futhark.Transform.FirstOrderTransform as FOT
import Futhark.Pass
import Futhark.Transform.CopyPropagate
import Futhark.Pass.ExtractKernels.Distribution
import Futhark.Pass.ExtractKernels.ISRWIM
import Futhark.Pass.ExtractKernels.BlockedKernel
import Futhark.Util.Log
import Futhark.Transform.Rename

extractKernels :: Pass Basic Basic
extractKernels =
  Pass { passName = "extract kernels"
       , passDescription = "Perform kernel extraction"
       , passFunction = runDistribM . liftM Prog . mapM transformFunDec . progFunctions
       }

newtype DistribM a = DistribM (RWS TypeEnv Log VNameSource a)
                   deriving (Functor, Applicative, Monad,
                             HasTypeEnv,
                             LocalTypeEnv,
                             MonadFreshNames,
                             MonadLogger)

runDistribM :: (MonadLogger m, MonadFreshNames m) =>
               DistribM a -> m a
runDistribM (DistribM m) = do
  (x, msgs) <- modifyNameSource $ positionNameSource . runRWS m HM.empty
  addLog msgs
  return x
  where positionNameSource (x, src, msgs) = ((x, msgs), src)

transformFunDec :: FunDec -> DistribM FunDec
transformFunDec fundec = do
  body' <- localTypeEnv (typeEnvFromParams $ funDecParams fundec) $
           transformBody $ funDecBody fundec
  return fundec { funDecBody = body' }

transformBody :: Body -> DistribM Body
transformBody body = do bnds <- transformBindings $ bodyBindings body
                        return body { bodyBindings = bnds }

transformBindings :: [Binding] -> DistribM [Binding]
transformBindings [] =
  return []
transformBindings (bnd:bnds) =
  sequentialisedUnbalancedBinding bnd >>= \case
    Nothing -> do
      bnd' <- transformBinding bnd
      localTypeEnv (typeEnvFromBindings bnd') $
        (bnd'++) <$> transformBindings bnds
    Just bnds' ->
      transformBindings $ bnds' <> bnds

sequentialisedUnbalancedBinding :: Binding -> DistribM (Maybe [Binding])
sequentialisedUnbalancedBinding bnd@(Let _ _ (LoopOp (Map _ _ lam _)))
  | unbalancedLambda lam =
    Just <$> runBinder_ (FOT.transformBinding bnd)
sequentialisedUnbalancedBinding bnd@(Let _ _ (LoopOp (Redomap _ _ lam1 lam2 _ _)))
  | unbalancedLambda lam1 || unbalancedLambda lam2 =
    Just <$> runBinder_ (FOT.transformBinding bnd)
sequentialisedUnbalancedBinding _ =
  return Nothing

transformBinding :: Binding -> DistribM [Binding]

transformBinding (Let pat () (If c tb fb rt)) = do
  tb' <- transformBody tb
  fb' <- transformBody fb
  return [Let pat () $ If c tb' fb' rt]

transformBinding (Let pat () (LoopOp (DoLoop res mergepat form body))) =
  localTypeEnv (boundInForm form $ typeEnvFromParams mergeparams) $ do
    body' <- transformBody body
    return [Let pat () $ LoopOp $ DoLoop res mergepat form body']
  where boundInForm (ForLoop i _) = HM.insert i (Basic Int)
        boundInForm (WhileLoop _) = id
        mergeparams = map fst mergepat

transformBinding (Let pat () (LoopOp (Map cs w lam arrs))) =
  distributeMap pat $ MapLoop cs w lam arrs

transformBinding (Let pat () (LoopOp (Redomap cs w lam1 lam2 nes arrs))) =
  if sequentialiseRedomapBody then do
    lam1_sequential <- FOT.transformLambda lam1
    lam2_sequential <- FOT.transformLambda lam2
    blockedReduction pat cs w lam1_sequential lam2_sequential nes arrs
  else do
    (mapbnd, redbnd) <- redomapToMapAndReduce pat () (cs, w, lam1, lam2, nes, arrs)
    return [mapbnd, redbnd]
      where sequentialiseRedomapBody = True

transformBinding (Let pat () (LoopOp (Reduce cs w red_fun red_input))) = do
  red_fun_sequential <- FOT.transformLambda red_fun
  red_fun_sequential' <- renameLambda red_fun_sequential
  blockedReduction pat cs w red_fun_sequential' red_fun_sequential nes arrs
  where (nes, arrs) = unzip red_input

transformBinding (Let pat () (LoopOp (Scan cs w fun input))) = do
  fun_sequential <- FOT.transformLambda fun
  runBinder_ $ blockedScan pat cs w fun_sequential input

-- Streams can be handled in two different ways - either we
-- sequentialise the body or we keep it parallel and distribute.
transformBinding (Let pat () (LoopOp (Stream cs w (RedLike _ red_fun nes) fold_fun arrs _))) = do
  -- We will sequentialise the body.  We do this by turning the stream
  -- into a redomap with the chunk size set to one.
  acc_ts <- mapM subExpType nes
  red_fun_sequential <- FOT.transformLambda red_fun
  fold_fun_unchunked <- singletonChunkRedLikeStreamLambda acc_ts fold_fun
  fold_fun_unchunked_sequential <- FOT.transformLambda fold_fun_unchunked
  blockedReduction pat cs w red_fun_sequential fold_fun_unchunked_sequential nes arrs

transformBinding (Let pat () (LoopOp (Stream cs w (Sequential nes) fold_fun arrs _))) =
  -- Remove the stream and leave the body parallel.  It will be
  -- distributed.
  runBinder_ $ sequentialStreamWholeArray pat cs w nes fold_fun arrs

transformBinding (Let pat () (LoopOp (Stream cs w (MapLike _) map_fun arrs _))) =
  -- Remove the stream and leave the body parallel.  It will be
  -- distributed.
  runBinder_ $ sequentialStreamWholeArray pat cs w [] map_fun arrs

transformBinding (Let res_pat () (LoopOp op))
  | Scan cs w scan_fun scan_input <- op,
    Just do_iswim <- iswim res_pat cs w scan_fun scan_input =
      transformBindings =<< runBinder_ do_iswim

transformBinding bnd = do
  e' <- mapExpM transform $ bindingExp bnd
  return [bnd { bindingExp = e' }]
  where transform = identityMapper { mapOnLambda = transformLambda }

transformLambda :: Lambda -> DistribM Lambda
transformLambda lam =
  localTypeEnv (typeEnvFromParams $ lambdaParams lam) $
  localTypeEnv (HM.singleton (lambdaIndex lam) $ Basic Int ) $ do
    body' <- transformBody $ lambdaBody lam
    return lam { lambdaBody = body' }

data MapLoop = MapLoop Certificates SubExp Lambda [VName]

mapLoopExp :: MapLoop -> Exp
mapLoopExp (MapLoop cs w lam arrs) = LoopOp $ Map cs w lam arrs

distributeMap :: (HasTypeEnv m, MonadFreshNames m, MonadLogger m) =>
                 Pattern -> MapLoop -> m [Binding]
distributeMap pat (MapLoop cs w lam arrs) = do
  types <- askTypeEnv
  let env = KernelEnv { kernelNest =
                        singleNesting (Nesting mempty $
                                       MapNesting pat cs w (lambdaIndex lam) $
                                       zip (lambdaParams lam) arrs)
                      , kernelTypeEnv =
                        types <> typeEnvFromParams (lambdaParams lam)
                      }
  liftM (postKernelBindings . snd) $ runKernelM env $
    distribute =<< distributeMapBodyBindings acc (bodyBindings $ lambdaBody lam)
    where acc = KernelAcc { kernelTargets = singleTarget (pat, bodyResult $ lambdaBody lam)
                          , kernelBindings = mempty
                          }

data KernelEnv = KernelEnv { kernelNest :: Nestings
                           , kernelTypeEnv :: TypeEnv
                           }

data KernelAcc = KernelAcc { kernelTargets :: Targets
                           , kernelBindings :: [Binding]
                           }

data KernelRes = KernelRes { accPostKernels :: PostKernels
                           , accLog :: Log
                           }

instance Monoid KernelRes where
  KernelRes ks1 log1 `mappend` KernelRes ks2 log2 =
    KernelRes (ks1 <> ks2) (log1 <> log2)
  mempty = KernelRes mempty mempty

newtype PostKernel = PostKernel { unPostKernel :: [Binding] }

newtype PostKernels = PostKernels [PostKernel]

instance Monoid PostKernels where
  mempty = PostKernels mempty
  PostKernels xs `mappend` PostKernels ys = PostKernels $ ys ++ xs

postKernelBindings :: PostKernels -> [Binding]
postKernelBindings (PostKernels kernels) = concatMap unPostKernel kernels

typeEnvFromKernelAcc :: KernelAcc -> TypeEnv
typeEnvFromKernelAcc = typeEnvFromPattern . fst . outerTarget . kernelTargets

addBindingToKernel :: (HasTypeEnv m, MonadFreshNames m) =>
                      Binding -> KernelAcc -> m KernelAcc
addBindingToKernel bnd acc = do
  bnds <- runBinder_ $ FOT.transformBindingRecursively bnd
  return acc { kernelBindings = bnds <> kernelBindings acc }

newtype KernelM a = KernelM (RWS KernelEnv KernelRes VNameSource a)
  deriving (Functor, Applicative, Monad,
            MonadReader KernelEnv,
            MonadWriter KernelRes,
            MonadFreshNames)

instance HasTypeEnv KernelM where
  askTypeEnv = asks kernelTypeEnv

instance LocalTypeEnv KernelM where
  localTypeEnv types = local $ \env ->
    env { kernelTypeEnv = kernelTypeEnv env <> types }

instance MonadLogger KernelM where
  addLog msgs = tell mempty { accLog = msgs }

runKernelM :: (HasTypeEnv m, MonadFreshNames m, MonadLogger m) =>
              KernelEnv -> KernelM a -> m (a, PostKernels)
runKernelM env (KernelM m) = do
  (x, res) <- modifyNameSource $ getKernels . runRWS m env
  addLog $ accLog res
  return (x, accPostKernels res)
  where getKernels (x,s,a) = ((x, a), s)

addKernels :: PostKernels -> KernelM ()
addKernels ks = tell $ mempty { accPostKernels = ks }

addKernel :: [Binding] -> KernelM ()
addKernel bnds = addKernels $ PostKernels [PostKernel bnds]

withBinding :: Binding -> KernelM a -> KernelM a
withBinding bnd = local $ \env ->
  env { kernelTypeEnv =
          kernelTypeEnv env <> typeEnvFromBindings [bnd]
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
      , kernelTypeEnv = kernelTypeEnv env <>
                        typeEnvFromParams (lambdaParams lam)
      }
  where nest = Nesting mempty $
               MapNesting pat cs w (lambdaIndex lam) $
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
        unbalancedBinding bound (LoopOp (Map _ w _ _)) =
          w `subExpBound` bound
        unbalancedBinding bound (LoopOp (Reduce _ w _ _)) =
          w `subExpBound` bound
        unbalancedBinding bound (LoopOp (Scan _ w _ _)) =
          w `subExpBound` bound
        unbalancedBinding bound (LoopOp (Redomap _ w _ _ _ _)) =
          w `subExpBound` bound
        unbalancedBinding bound (LoopOp (ConcatMap _ w _ _)) =
          w `subExpBound` bound
        unbalancedBinding bound (LoopOp (Stream _ w _ _ _ _)) =
          w `subExpBound` bound
        unbalancedBinding bound (LoopOp (MapKernel _ w _ _ _ _ _)) =
          w `subExpBound` bound
        unbalancedBinding bound (LoopOp (ReduceKernel _ w _ _ _ _ _)) =
          w `subExpBound` bound
        unbalancedBinding bound (LoopOp (ScanKernel _ w _ _ _ _)) =
          w `subExpBound` bound
        unbalancedBinding bound (LoopOp (DoLoop _ merge (ForLoop i iterations) body)) =
          iterations `subExpBound` bound ||
          unbalancedBody bound' body
          where bound' = foldr HS.insert bound $
                         i : map (paramName . fst) merge
        unbalancedBinding _ (LoopOp (DoLoop _ _ (WhileLoop _) _)) =
          True

        unbalancedBinding bound (If _ tbranch fbranch _) =
          unbalancedBody bound tbranch || unbalancedBody bound fbranch

        unbalancedBinding bound (SegOp (SegReduce _ w _ _ _)) =
          w `subExpBound` bound
        unbalancedBinding bound (SegOp (SegScan _ w _ _ _ _)) =
          w `subExpBound` bound
        unbalancedBinding bound (SegOp (SegReplicate _ w _ _)) =
          w `HS.member` bound

        unbalancedBinding _ (PrimOp _) =
          False
        unbalancedBinding _ (Apply fname _ _) =
          not $ isBuiltInFunction fname

distributeInnerMap :: Pattern -> MapLoop -> KernelAcc
                   -> KernelM KernelAcc
distributeInnerMap pat maploop@(MapLoop cs w lam arrs) acc
  | unbalancedLambda lam =
      addBindingToKernel (Let pat () $ mapLoopExp maploop) acc
  | otherwise =
      distribute =<<
      leavingNesting maploop =<<
      mapNesting pat cs w lam arrs
      (distribute =<< distributeMapBodyBindings acc' (bodyBindings $ lambdaBody lam))
      where acc' = KernelAcc { kernelTargets = pushInnerTarget
                                               (pat, bodyResult $ lambdaBody lam) $
                                               kernelTargets acc
                             , kernelBindings = mempty
                             }

leavingNesting :: MapLoop -> KernelAcc -> KernelM KernelAcc
leavingNesting (MapLoop cs w lam arrs) acc =
  case second reverse $ kernelTargets acc of
   (_, []) ->
     fail "The kernel targets list is unexpectedly small"
   ((pat,res), x:xs) -> do
     let acc' = acc { kernelTargets = (x, reverse xs) }
     case kernelBindings acc' of
       []      -> return acc'
       remnant ->
         let body = mkBody remnant res
             used_in_body = freeInBody body
             (used_params, used_arrs) =
               unzip $
               filter ((`HS.member` used_in_body) . paramName . fst) $
               zip (lambdaParams lam) arrs
             lam' = Lambda { lambdaBody = body
                           , lambdaReturnType = map rowType $ patternTypes pat
                           , lambdaParams = used_params
                           , lambdaIndex = lambdaIndex lam
                           }
         in addBindingToKernel (Let pat () $ LoopOp $ Map cs w lam' used_arrs)
            acc' { kernelBindings = [] }

distributeMapBodyBindings :: KernelAcc -> [Binding] -> KernelM KernelAcc

distributeMapBodyBindings acc [] =
  return acc

distributeMapBodyBindings acc
  (Let pat () (LoopOp (Stream cs w (Sequential accs) lam arrs _)):bnds) = do
  stream_bnds <- runBinder_ $ sequentialStreamWholeArray pat cs w accs lam arrs
  stream_bnds' <- copyPropagateInBindings bindableSimpleOps stream_bnds
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
maybeDistributeBinding bnd@(Let pat _ (LoopOp (Map cs w lam arrs))) acc =
  -- Only distribute inside the map if we can distribute everything
  -- following the map.
  distributeIfPossible acc >>= \case
    Nothing -> addBindingToKernel bnd acc
    Just acc' -> distribute =<< distributeInnerMap pat (MapLoop cs w lam arrs) acc'

maybeDistributeBinding bnd@(Let pat _ (LoopOp (DoLoop ret merge form body))) acc
  | any (isMap . bindingExp) $ bodyBindings body =
  distributeSingleBinding acc bnd >>= \case
    Just (kernels, res, nest, acc')
      | length res == patternSize pat -> do
      addKernels kernels
      localTypeEnv (typeEnvFromKernelAcc acc') $
        addKernel =<<
          interchangeLoops nest (SeqLoop pat ret merge form body)
      return acc'
    _ ->
      addBindingToKernel bnd acc
  where isMap (LoopOp Map{}) = True
        isMap _              = False

-- If the scan can be distributed by itself, we will turn it into a
-- segmented scan.
--
-- If the scan cannot be distributed by itself, it will be
-- sequentialised in the default case for this function.
maybeDistributeBinding bnd@(Let _ _ (LoopOp (Scan cs w lam input))) acc =
  distributeSingleBinding acc bnd >>= \case
    Just (kernels, _, nest, acc') -> do
      lam' <- FOT.transformLambda lam
      localTypeEnv (typeEnvFromKernelAcc acc') $
        segmentedScanKernel nest cs w lam' input >>= \case
          Nothing ->
            addBindingToKernel bnd acc
          Just bnds -> do
            addKernels kernels
            addKernel bnds
            return acc'
    _ ->
      addBindingToKernel bnd acc

maybeDistributeBinding bnd@(Let _ _ (PrimOp Copy{})) acc = do
  acc' <- distribute acc
  distribute =<< addBindingToKernel bnd acc'

maybeDistributeBinding bnd@(Let _ _ (PrimOp Rearrange{})) acc = do
  acc' <- distribute acc
  distribute =<< addBindingToKernel bnd acc'

maybeDistributeBinding bnd@(Let _ _ (PrimOp Reshape{})) acc = do
  acc' <- distribute acc
  distribute =<< addBindingToKernel bnd acc'

maybeDistributeBinding bnd acc =
  addBindingToKernel bnd acc

distribute :: KernelAcc -> KernelM KernelAcc
distribute acc =
  fromMaybe acc <$> distributeIfPossible acc

distributeIfPossible :: KernelAcc -> KernelM (Maybe KernelAcc)
distributeIfPossible acc = do
  nest <- asks kernelNest
  tryDistribute nest (kernelTargets acc) (kernelBindings acc) >>= \case
    Nothing -> return Nothing
    Just (targets, kernel) -> do
      addKernel kernel
      return $ Just KernelAcc { kernelTargets = targets
                              , kernelBindings = []
                              }

distributeSingleBinding :: KernelAcc -> Binding
                        -> KernelM (Maybe (PostKernels, Result, KernelNest, KernelAcc))
distributeSingleBinding acc bnd = do
  nest <- asks kernelNest
  tryDistribute nest (kernelTargets acc) (kernelBindings acc) >>= \case
    Nothing -> return Nothing
    Just (targets, distributed_bnds) ->
      tryDistributeBinding nest targets bnd >>= \case
        Nothing -> return Nothing
        Just (res, targets', new_kernel_nest) ->
          return $ Just (PostKernels [PostKernel distributed_bnds],
                         res,
                         new_kernel_nest,
                         KernelAcc { kernelTargets = targets'
                                   , kernelBindings = []
                                   })

segmentedScanKernel :: KernelNest
                    -> Certificates -> SubExp -> Lambda -> [(SubExp, VName)]
                    -> KernelM (Maybe [Binding])
segmentedScanKernel nest cs segment_size lam scan_inps = runMaybeT $ do
  -- We must verify that array inputs to the scan are inputs to the
  -- outermost loop nesting or free in the loop nest, and that none of
  -- the names bound by the loop nest are used in the lambda.
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
      letSubExp "total_num_elements" $ PrimOp $ BinOp Times segment_size nesting_size Int

    let flatten (ne, arr) = do
          ne_shape <- arrayShape <$> subExpType ne
          arr_shape <- arrayShape <$> lookupType arr
          let reshape = reshapeOuter [DimNew total_num_elements]
                        (shapeRank arr_shape - shapeRank ne_shape)
                        arr_shape
          arr' <- letExp (baseString arr ++ "_flat") $
                  PrimOp $ Reshape [] reshape arr
          return (ne, arr')

    scan_inps' <- mapM flatten =<< sequence mk_inps

    let pat = loopNestingPattern $ fst nest
        flatPatElem pat_elem t = do
          let u = uniqueness $ patElemType pat_elem
              t' = arrayOf t (Shape [total_num_elements]) u
          ident <- newIdent (baseString (patElemName pat_elem) ++ "_flat") t'
          return $ PatElem ident BindVar ()
    flat_pat <- Pattern [] <$>
                zipWithM flatPatElem
                (patternValueElements pat)
                (lambdaReturnType lam)

    blockedSegmentedScan segment_size flat_pat cs total_num_elements lam scan_inps'

    forM_ (zip (patternValueElements pat) (patternNames flat_pat)) $
      \(dst_pat_elem, flat) -> do
        let ident = patElemIdent dst_pat_elem
            bindage = patElemBindage dst_pat_elem
            dims = arrayDims $ identType ident
        addBinding $ mkLet [] [(ident, bindage)] $
          PrimOp $ Reshape [] (map DimNew dims) flat
