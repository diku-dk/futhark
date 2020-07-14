{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Extract limited nested parallelism for execution inside
-- individual kernel workgroups.
module Futhark.Pass.ExtractKernels.Intragroup
  (intraGroupParallelise)
where

import Control.Monad.Identity
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Prelude hiding (log)

import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.SOACS
import qualified Futhark.IR.Kernels as Out
import Futhark.IR.Kernels.Kernel hiding (HistOp)
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Pass.ExtractKernels.DistributeNests
import Futhark.Pass.ExtractKernels.Distribution
import Futhark.Pass.ExtractKernels.BlockedKernel
import Futhark.Pass.ExtractKernels.ToKernels
import qualified Futhark.Transform.FirstOrderTransform as FOT
import Futhark.Util (chunks)
import Futhark.Util.Log

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
intraGroupParallelise :: (MonadFreshNames m, LocalScope Out.Kernels m) =>
                         KernelNest -> Lambda
                      -> m (Maybe ((SubExp, SubExp), SubExp, Log,
                                   Out.Stms Out.Kernels, Out.Stms Out.Kernels))
intraGroupParallelise knest lam = runMaybeT $ do
  (ispace, inps) <- lift $ flatKernel knest

  (num_groups, w_stms) <- lift $ runBinder $
    letSubExp "intra_num_groups" =<<
    foldBinOp (Mul Int32 OverflowUndef) (intConst Int32 1) (map snd ispace)

  let body = lambdaBody lam

  group_size <- newVName "computed_group_size"
  let intra_lvl = SegThread (Count num_groups) (Count $ Var group_size) SegNoVirt

  (wss_min, wss_avail, log, kbody) <-
    lift $ localScope (scopeOfLParams $ lambdaParams lam) $
    intraGroupParalleliseBody intra_lvl body

  outside_scope <- lift askScope
  unless (all (`M.member` outside_scope) $ namesToList $
          freeIn (wss_min ++ wss_avail)) $
    fail "Irregular parallelism"

  ((intra_avail_par, kspace, read_input_stms), prelude_stms) <- lift $ runBinder $ do
    let foldBinOp' _    []    = eSubExp $ intConst Int32 0
        foldBinOp' bop (x:xs) = foldBinOp bop x xs
    ws_min <- mapM (letSubExp "one_intra_par_min" <=< foldBinOp' (Mul Int32 OverflowUndef)) $
              filter (not . null) wss_min
    ws_avail <- mapM (letSubExp "one_intra_par_avail" <=< foldBinOp' (Mul Int32 OverflowUndef)) $
                filter (not . null) wss_avail

    -- The amount of parallelism available *in the worst case* is
    -- equal to the smallest parallel loop.
    intra_avail_par <- letSubExp "intra_avail_par" =<< foldBinOp' (SMin Int32) ws_avail

    -- The group size is either the maximum of the minimum parallelism
    -- exploited, or the desired parallelism (bounded by the max group
    -- size) in case there is no minimum.
    letBindNames [group_size] =<<
      if null ws_min
      then eBinOp (SMin Int32)
           (eSubExp =<< letSubExp "max_group_size" (Op $ SizeOp $ Out.GetSizeMax Out.SizeGroup))
           (eSubExp intra_avail_par)
      else foldBinOp' (SMax Int32) ws_min

    let inputIsUsed input = kernelInputName input `nameIn` freeIn body
        used_inps = filter inputIsUsed inps

    addStms w_stms
    read_input_stms <- runBinder_ $ mapM readKernelInput used_inps
    space <- mkSegSpace ispace
    return (intra_avail_par, space, read_input_stms)

  let kbody' = kbody { kernelBodyStms = read_input_stms <> kernelBodyStms kbody }

  let nested_pat = loopNestingPattern first_nest
      rts = map (length ispace `stripArray`) $ patternTypes nested_pat
      lvl = SegGroup (Count num_groups) (Count $ Var group_size) SegNoVirt
      kstm = Let nested_pat aux $
             Op $ SegOp $ SegMap lvl kspace rts kbody'

  let intra_min_par = intra_avail_par
  return ((intra_min_par, intra_avail_par), Var group_size, log,
           prelude_stms, oneStm kstm)
  where first_nest = fst knest
        aux = loopNestingAux first_nest

data IntraAcc = IntraAcc { accMinPar :: S.Set [SubExp]
                         , accAvailPar :: S.Set [SubExp]
                         , accLog :: Log
                         }

instance Semigroup IntraAcc where
  IntraAcc min_x avail_x log_x <> IntraAcc min_y avail_y log_y =
    IntraAcc (min_x <> min_y) (avail_x <> avail_y) (log_x <> log_y)

instance Monoid IntraAcc where
  mempty = IntraAcc mempty mempty mempty

type IntraGroupM =
  BinderT Out.Kernels (RWS () IntraAcc VNameSource)

instance MonadLogger IntraGroupM where
  addLog log = tell mempty { accLog = log }

runIntraGroupM :: (MonadFreshNames m, HasScope Out.Kernels m) =>
                  IntraGroupM () -> m (IntraAcc, Out.Stms Out.Kernels)
runIntraGroupM m = do
  scope <- castScope <$> askScope
  modifyNameSource $ \src ->
    let (((), kstms), src', acc) = runRWS (runBinderT m scope) () src
    in ((acc, kstms), src')

parallelMin :: [SubExp] -> IntraGroupM ()
parallelMin ws = tell mempty { accMinPar = S.singleton ws
                             , accAvailPar = S.singleton ws
                             }

intraGroupBody :: SegLevel -> Body -> IntraGroupM (Out.Body Out.Kernels)
intraGroupBody lvl body = do
  stms <- collectStms_ $ mapM_ (intraGroupStm lvl) $ bodyStms body
  return $ mkBody stms $ bodyResult body

intraGroupStm :: SegLevel -> Stm -> IntraGroupM ()
intraGroupStm lvl stm@(Let pat aux e) = do
  scope <- askScope
  let lvl' = SegThread (segNumGroups lvl) (segGroupSize lvl) SegNoVirt

  case e of
    DoLoop ctx val form loopbody ->
      localScope (scopeOf form') $
      localScope (scopeOfFParams $ map fst $ ctx ++ val) $ do
      loopbody' <- intraGroupBody lvl loopbody
      certifying (stmAuxCerts aux) $
        letBind pat $ DoLoop ctx val form' loopbody'
          where form' = case form of
                          ForLoop i it bound inps -> ForLoop i it bound inps
                          WhileLoop cond          -> WhileLoop cond

    If cond tbody fbody ifdec -> do
      tbody' <- intraGroupBody lvl tbody
      fbody' <- intraGroupBody lvl fbody
      certifying (stmAuxCerts aux) $
        letBind pat $ If cond tbody' fbody' ifdec

    Op soac
      | "sequential_outer" `inAttrs` stmAuxAttrs aux ->
          mapM_ (intraGroupStm lvl) . fmap (certify (stmAuxCerts aux)) =<<
          runBinder_ (FOT.transformSOAC pat soac)

    Op (Screma w form arrs)
      | Just lam <- isMapSOAC form -> do
      let loopnest = MapNesting pat aux w $ zip (lambdaParams lam) arrs
          env = DistEnv { distNest =
                            singleNesting $ Nesting mempty loopnest
                        , distScope =
                            scopeOfPattern pat <>
                            scopeForKernels (scopeOf lam) <> scope
                        , distOnInnerMap =
                            distributeMap
                        , distOnTopLevelStms =
                            lift . collectStms_ . intraGroupStms lvl
                        , distSegLevel = \minw _ _ -> do
                            lift $ parallelMin minw
                            return lvl
                        , distOnSOACSStms =
                            pure . oneStm . soacsStmToKernels
                        , distOnSOACSLambda =
                            pure . soacsLambdaToKernels
                        }
          acc = DistAcc { distTargets = singleTarget (pat, bodyResult $ lambdaBody lam)
                        , distStms = mempty
                        }

      addStms =<<
        runDistNestT env (distributeMapBodyStms acc (bodyStms $ lambdaBody lam))

    Op (Screma w form arrs)
      | Just (scans, mapfun) <- isScanomapSOAC form,
        Scan scanfun nes <- singleScan scans -> do
      let scanfun' = soacsLambdaToKernels scanfun
          mapfun' = soacsLambdaToKernels mapfun
      certifying (stmAuxCerts aux) $
        addStms =<< segScan lvl' pat w [SegBinOp Noncommutative scanfun' nes mempty] mapfun' arrs [] []
      parallelMin [w]

    Op (Screma w form arrs)
      | Just (reds, map_lam) <- isRedomapSOAC form,
        Reduce comm red_lam nes <- singleReduce reds -> do
      let red_lam' = soacsLambdaToKernels red_lam
          map_lam' = soacsLambdaToKernels map_lam
      certifying (stmAuxCerts aux) $
        addStms =<< segRed lvl' pat w [SegBinOp comm red_lam' nes mempty] map_lam' arrs [] []
      parallelMin [w]


    Op (Hist w ops bucket_fun arrs) -> do
      ops' <- forM ops $ \(HistOp num_bins rf dests nes op) -> do
        (op', nes', shape) <- determineReduceOp op nes
        let op'' = soacsLambdaToKernels op'
        return $ Out.HistOp num_bins rf dests nes' shape op''

      let bucket_fun' = soacsLambdaToKernels bucket_fun
      certifying (stmAuxCerts aux) $
        addStms =<< segHist lvl' pat w [] [] ops' bucket_fun' arrs
      parallelMin [w]

    Op (Stream w (Sequential accs) lam arrs)
      | chunk_size_param : _ <- lambdaParams lam -> do
      types <- asksScope castScope
      ((), stream_bnds) <-
        runBinderT (sequentialStreamWholeArray pat w accs lam arrs) types
      let replace (Var v) | v == paramName chunk_size_param = w
          replace se = se
          replaceSets (IntraAcc x y log) =
            IntraAcc (S.map (map replace) x) (S.map (map replace) y) log
      censor replaceSets $ mapM_ (intraGroupStm lvl) stream_bnds

    Op (Scatter w lam ivs dests) -> do
      write_i <- newVName "write_i"
      space <- mkSegSpace [(write_i, w)]

      let lam' = soacsLambdaToKernels lam
          (dests_ws, dests_ns, dests_vs) = unzip3 dests
          (i_res, v_res) = splitAt (sum dests_ns) $ bodyResult $ lambdaBody lam'
          krets = do (a_w, a, is_vs) <- zip3 dests_ws dests_vs $ chunks dests_ns $ zip i_res v_res
                     return $ WriteReturns [a_w] a [ ([DimFix i],v) | (i,v) <- is_vs ]
          inputs = do (p, p_a) <- zip (lambdaParams lam') ivs
                      return $ KernelInput (paramName p) (paramType p) p_a [Var write_i]

      kstms <- runBinder_ $ localScope (scopeOfSegSpace space) $ do
        mapM_ readKernelInput inputs
        addStms $ bodyStms $ lambdaBody lam'

      certifying (stmAuxCerts aux) $ do
        let ts = map rowType $ patternTypes pat
            body = KernelBody () kstms krets
        letBind pat $ Op $ SegOp $ SegMap lvl' space ts body

      parallelMin [w]

    _ ->
      addStm $ soacsStmToKernels stm

intraGroupStms :: SegLevel -> Stms SOACS -> IntraGroupM ()
intraGroupStms lvl = mapM_ (intraGroupStm lvl)

intraGroupParalleliseBody :: (MonadFreshNames m, HasScope Out.Kernels m) =>
                             SegLevel -> Body
                          -> m ([[SubExp]], [[SubExp]], Log, Out.KernelBody Out.Kernels)
intraGroupParalleliseBody lvl body = do
  (IntraAcc min_ws avail_ws log, kstms) <-
    runIntraGroupM $ intraGroupStms lvl $ bodyStms body
  return (S.toList min_ws, S.toList avail_ws, log,
          KernelBody () kstms $ map (Returns ResultMaySimplify) $ bodyResult body)
