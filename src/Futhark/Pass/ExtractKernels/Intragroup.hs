{-# LANGUAGE TypeFamilies #-}

-- | Extract limited nested parallelism for execution inside
-- individual kernel workgroups.
module Futhark.Pass.ExtractKernels.Intragroup (intraGroupParallelise) where

import Control.Monad.Identity
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.GPU hiding (HistOp)
import Futhark.IR.GPU.Op qualified as GPU
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Pass.ExtractKernels.BlockedKernel
import Futhark.Pass.ExtractKernels.DistributeNests
import Futhark.Pass.ExtractKernels.Distribution
import Futhark.Pass.ExtractKernels.ToGPU
import Futhark.Tools
import Futhark.Transform.FirstOrderTransform qualified as FOT
import Futhark.Util.Log
import Prelude hiding (log)

-- | Convert the statements inside a map nest to kernel statements,
-- attempting to parallelise any remaining (top-level) parallel
-- statements.  Anything that is not a map, scan or reduction will
-- simply be sequentialised.  This includes sequential loops that
-- contain maps, scans or reduction.  In the future, we could probably
-- do something more clever.  Make sure that the amount of parallelism
-- to be exploited does not exceed the group size.  Further, as a hack
-- we also consider the size of all intermediate arrays as
-- "parallelism to be exploited" to avoid exploding local memory.
--
-- We distinguish between "minimum group size" and "maximum
-- exploitable parallelism".
intraGroupParallelise ::
  (MonadFreshNames m, LocalScope GPU m) =>
  KernelNest ->
  Lambda SOACS ->
  m
    ( Maybe
        ( (SubExp, SubExp),
          SubExp,
          Log,
          Stms GPU,
          Stms GPU
        )
    )
intraGroupParallelise knest lam = runMaybeT $ do
  (ispace, inps) <- lift $ flatKernel knest

  (num_groups, w_stms) <-
    lift $
      runBuilder $
        letSubExp "intra_num_groups"
          =<< foldBinOp (Mul Int64 OverflowUndef) (intConst Int64 1) (map snd ispace)

  let body = lambdaBody lam

  group_size <- newVName "computed_group_size"
  (wss_min, wss_avail, log, kbody) <-
    lift . localScope (scopeOfLParams $ lambdaParams lam) $
      intraGroupParalleliseBody body

  outside_scope <- lift askScope
  -- outside_scope may also contain the inputs, even though those are
  -- not actually available outside the kernel.
  let available v =
        v `M.member` outside_scope
          && v `notElem` map kernelInputName inps
  unless (all available $ namesToList $ freeIn (wss_min ++ wss_avail)) $
    fail "Irregular parallelism"

  ((intra_avail_par, kspace, read_input_stms), prelude_stms) <- lift $
    runBuilder $ do
      let foldBinOp' _ [] = eSubExp $ intConst Int64 1
          foldBinOp' bop (x : xs) = foldBinOp bop x xs
      ws_min <-
        mapM (letSubExp "one_intra_par_min" <=< foldBinOp' (Mul Int64 OverflowUndef)) $
          filter (not . null) wss_min
      ws_avail <-
        mapM (letSubExp "one_intra_par_avail" <=< foldBinOp' (Mul Int64 OverflowUndef)) $
          filter (not . null) wss_avail

      -- The amount of parallelism available *in the worst case* is
      -- equal to the smallest parallel loop, or *at least* 1.
      intra_avail_par <-
        letSubExp "intra_avail_par" =<< foldBinOp' (SMin Int64) ws_avail

      -- The group size is either the maximum of the minimum parallelism
      -- exploited, or the desired parallelism (bounded by the max group
      -- size) in case there is no minimum.
      letBindNames [group_size]
        =<< if null ws_min
          then
            eBinOp
              (SMin Int64)
              (eSubExp =<< letSubExp "max_group_size" (Op $ SizeOp $ GetSizeMax SizeGroup))
              (eSubExp intra_avail_par)
          else foldBinOp' (SMax Int64) ws_min

      let inputIsUsed input = kernelInputName input `nameIn` freeIn body
          used_inps = filter inputIsUsed inps

      addStms w_stms
      read_input_stms <- runBuilder_ $ mapM readGroupKernelInput used_inps
      space <- mkSegSpace ispace
      pure (intra_avail_par, space, read_input_stms)

  let kbody' = kbody {kernelBodyStms = read_input_stms <> kernelBodyStms kbody}

  let nested_pat = loopNestingPat first_nest
      rts = map (length ispace `stripArray`) $ patTypes nested_pat
      grid = KernelGrid (Count num_groups) (Count $ Var group_size)
      lvl = SegGroup SegNoVirt (Just grid)
      kstm =
        Let nested_pat aux $ Op $ SegOp $ SegMap lvl kspace rts kbody'

  let intra_min_par = intra_avail_par
  pure
    ( (intra_min_par, intra_avail_par),
      Var group_size,
      log,
      prelude_stms,
      oneStm kstm
    )
  where
    first_nest = fst knest
    aux = loopNestingAux first_nest

readGroupKernelInput ::
  (DistRep (Rep m), MonadBuilder m) =>
  KernelInput ->
  m ()
readGroupKernelInput inp
  | Array {} <- kernelInputType inp = do
      v <- newVName $ baseString $ kernelInputName inp
      readKernelInput inp {kernelInputName = v}
      letBindNames [kernelInputName inp] $ BasicOp $ Copy v
  | otherwise =
      readKernelInput inp

data IntraAcc = IntraAcc
  { accMinPar :: S.Set [SubExp],
    accAvailPar :: S.Set [SubExp],
    accLog :: Log
  }

instance Semigroup IntraAcc where
  IntraAcc min_x avail_x log_x <> IntraAcc min_y avail_y log_y =
    IntraAcc (min_x <> min_y) (avail_x <> avail_y) (log_x <> log_y)

instance Monoid IntraAcc where
  mempty = IntraAcc mempty mempty mempty

type IntraGroupM =
  BuilderT GPU (RWS () IntraAcc VNameSource)

instance MonadLogger IntraGroupM where
  addLog log = tell mempty {accLog = log}

runIntraGroupM ::
  (MonadFreshNames m, HasScope GPU m) =>
  IntraGroupM () ->
  m (IntraAcc, Stms GPU)
runIntraGroupM m = do
  scope <- castScope <$> askScope
  modifyNameSource $ \src ->
    let (((), kstms), src', acc) = runRWS (runBuilderT m scope) () src
     in ((acc, kstms), src')

parallelMin :: [SubExp] -> IntraGroupM ()
parallelMin ws =
  tell
    mempty
      { accMinPar = S.singleton ws,
        accAvailPar = S.singleton ws
      }

intraGroupBody :: Body SOACS -> IntraGroupM (Body GPU)
intraGroupBody body = do
  stms <- collectStms_ $ intraGroupStms $ bodyStms body
  pure $ mkBody stms $ bodyResult body

intraGroupStm :: Stm SOACS -> IntraGroupM ()
intraGroupStm stm@(Let pat aux e) = do
  scope <- askScope
  let lvl = SegThread SegNoVirt Nothing

  case e of
    DoLoop merge form loopbody ->
      localScope (scopeOf form') $
        localScope (scopeOfFParams $ map fst merge) $ do
          loopbody' <- intraGroupBody loopbody
          certifying (stmAuxCerts aux) $
            letBind pat $
              DoLoop merge form' loopbody'
      where
        form' = case form of
          ForLoop i it bound inps -> ForLoop i it bound inps
          WhileLoop cond -> WhileLoop cond
    Match cond cases defbody ifdec -> do
      cases' <- mapM (traverse intraGroupBody) cases
      defbody' <- intraGroupBody defbody
      certifying (stmAuxCerts aux) . letBind pat $
        Match cond cases' defbody' ifdec
    Op soac
      | "sequential_outer" `inAttrs` stmAuxAttrs aux ->
          intraGroupStms . fmap (certify (stmAuxCerts aux))
            =<< runBuilder_ (FOT.transformSOAC pat soac)
    Op (Screma w arrs form)
      | Just lam <- isMapSOAC form -> do
          let loopnest = MapNesting pat aux w $ zip (lambdaParams lam) arrs
              env =
                DistEnv
                  { distNest =
                      singleNesting $ Nesting mempty loopnest,
                    distScope =
                      scopeOfPat pat
                        <> scopeForGPU (scopeOf lam)
                        <> scope,
                    distOnInnerMap =
                      distributeMap,
                    distOnTopLevelStms =
                      liftInner . collectStms_ . intraGroupStms,
                    distSegLevel = \minw _ _ -> do
                      lift $ parallelMin minw
                      pure lvl,
                    distOnSOACSStms =
                      pure . oneStm . soacsStmToGPU,
                    distOnSOACSLambda =
                      pure . soacsLambdaToGPU
                  }
              acc =
                DistAcc
                  { distTargets = singleTarget (pat, bodyResult $ lambdaBody lam),
                    distStms = mempty
                  }

          addStms
            =<< runDistNestT env (distributeMapBodyStms acc (bodyStms $ lambdaBody lam))
    Op (Screma w arrs form)
      | Just (scans, mapfun) <- isScanomapSOAC form,
        Scan scanfun nes <- singleScan scans -> do
          let scanfun' = soacsLambdaToGPU scanfun
              mapfun' = soacsLambdaToGPU mapfun
          certifying (stmAuxCerts aux) $
            addStms =<< segScan lvl pat mempty w [SegBinOp Noncommutative scanfun' nes mempty] mapfun' arrs [] []
          parallelMin [w]
    Op (Screma w arrs form)
      | Just (reds, map_lam) <- isRedomapSOAC form,
        Reduce comm red_lam nes <- singleReduce reds -> do
          let red_lam' = soacsLambdaToGPU red_lam
              map_lam' = soacsLambdaToGPU map_lam
          certifying (stmAuxCerts aux) $
            addStms =<< segRed lvl pat mempty w [SegBinOp comm red_lam' nes mempty] map_lam' arrs [] []
          parallelMin [w]
    Op (Hist w arrs ops bucket_fun) -> do
      ops' <- forM ops $ \(HistOp num_bins rf dests nes op) -> do
        (op', nes', shape) <- determineReduceOp op nes
        let op'' = soacsLambdaToGPU op'
        pure $ GPU.HistOp num_bins rf dests nes' shape op''

      let bucket_fun' = soacsLambdaToGPU bucket_fun
      certifying (stmAuxCerts aux) $
        addStms =<< segHist lvl pat w [] [] ops' bucket_fun' arrs
      parallelMin [w]
    Op (Stream w arrs accs lam)
      | chunk_size_param : _ <- lambdaParams lam -> do
          types <- asksScope castScope
          ((), stream_stms) <-
            runBuilderT (sequentialStreamWholeArray pat w accs lam arrs) types
          let replace (Var v) | v == paramName chunk_size_param = w
              replace se = se
              replaceSets (IntraAcc x y log) =
                IntraAcc (S.map (map replace) x) (S.map (map replace) y) log
          censor replaceSets $ intraGroupStms stream_stms
    Op (Scatter w ivs lam dests) -> do
      write_i <- newVName "write_i"
      space <- mkSegSpace [(write_i, w)]

      let lam' = soacsLambdaToGPU lam
          (dests_ws, _, _) = unzip3 dests
          krets = do
            (a_w, a, is_vs) <-
              groupScatterResults dests $ bodyResult $ lambdaBody lam'
            let cs =
                  foldMap (foldMap resCerts . fst) is_vs
                    <> foldMap (resCerts . snd) is_vs
                is_vs' = [(Slice $ map (DimFix . resSubExp) is, resSubExp v) | (is, v) <- is_vs]
            pure $ WriteReturns cs a_w a is_vs'
          inputs = do
            (p, p_a) <- zip (lambdaParams lam') ivs
            pure $ KernelInput (paramName p) (paramType p) p_a [Var write_i]

      kstms <- runBuilder_ $
        localScope (scopeOfSegSpace space) $ do
          mapM_ readKernelInput inputs
          addStms $ bodyStms $ lambdaBody lam'

      certifying (stmAuxCerts aux) $ do
        let ts = zipWith (stripArray . length) dests_ws $ patTypes pat
            body = KernelBody () kstms krets
        letBind pat $ Op $ SegOp $ SegMap lvl space ts body

      parallelMin [w]
    _ ->
      addStm $ soacsStmToGPU stm

intraGroupStms :: Stms SOACS -> IntraGroupM ()
intraGroupStms = mapM_ intraGroupStm

intraGroupParalleliseBody ::
  (MonadFreshNames m, HasScope GPU m) =>
  Body SOACS ->
  m ([[SubExp]], [[SubExp]], Log, KernelBody GPU)
intraGroupParalleliseBody body = do
  (IntraAcc min_ws avail_ws log, kstms) <-
    runIntraGroupM $ intraGroupStms $ bodyStms body
  pure
    ( S.toList min_ws,
      S.toList avail_ws,
      log,
      KernelBody () kstms $ map ret $ bodyResult body
    )
  where
    ret (SubExpRes cs se) = Returns ResultMaySimplify cs se
