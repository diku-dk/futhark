{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
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

import Futhark.Analysis.PrimExp.Convert
import Futhark.Representation.SOACS
import qualified Futhark.Representation.Kernels as Out
import Futhark.Representation.Kernels.Kernel
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Analysis.DataDependencies
import Futhark.Pass.ExtractKernels.Distribution
import Futhark.Pass.ExtractKernels.BlockedKernel
import Futhark.Util (chunks)

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
                      -> m (Maybe ((SubExp, SubExp), SubExp,
                                   Out.Stms Out.Kernels, Out.Stms Out.Kernels))
intraGroupParallelise knest lam = runMaybeT $ do
  (w_stms, w, ispace, inps) <- lift $ flatKernel knest
  let num_groups = w
      body = lambdaBody lam

  group_size <- newVName "computed_group_size"
  let intra_lvl = SegThread (Count w) (Count $ Var group_size) SegNoVirt

  ltid <- newVName "ltid"
  let group_variant = S.fromList [ltid]
  (wss_min, wss_avail, kbody) <-
    lift $ localScope (scopeOfLParams $ lambdaParams lam) $
    intraGroupParalleliseBody intra_lvl (dataDependencies body) group_variant body

  known_outside <- lift $ M.keys <$> askScope
  unless (all (`elem` known_outside) $ freeIn $ wss_min ++ wss_avail) $
    fail "Irregular parallelism"

  ((intra_avail_par, kspace, read_input_stms), prelude_stms) <- lift $ runBinder $ do
    let foldBinOp' _    []    = eSubExp $ intConst Int32 0
        foldBinOp' bop (x:xs) = foldBinOp bop x xs
    ws_min <- mapM (letSubExp "one_intra_par_min" <=< foldBinOp' (Mul Int32)) $
              filter (not . null) wss_min
    ws_avail <- mapM (letSubExp "one_intra_par_avail" <=< foldBinOp' (Mul Int32)) $
                filter (not . null) wss_avail

    -- The amount of parallelism available *in the worst case* is
    -- equal to the smallest parallel loop.
    intra_avail_par <- letSubExp "intra_avail_par" =<< foldBinOp' (SMin Int32) ws_avail

    -- The group size is either the maximum of the minimum parallelism
    -- exploited, or the desired parallelism (bounded by the max group
    -- size) in case there is no minimum.
    letBindNames_ [group_size] =<<
      if null ws_min
      then eBinOp (SMin Int32)
           (eSubExp =<< letSubExp "max_group_size" (Op $ Out.GetSizeMax Out.SizeGroup))
           (eSubExp intra_avail_par)
      else foldBinOp' (SMax Int32) ws_min

    let inputIsUsed input = kernelInputName input `S.member` freeIn body
        used_inps = filter inputIsUsed inps

    addStms w_stms
    read_input_stms <- runBinder_ $ mapM readKernelInput used_inps
    space <- mkSegSpace ispace
    return (intra_avail_par, space, read_input_stms)

  let kbody' = kbody { kernelBodyStms = read_input_stms <> kernelBodyStms kbody }

  let nested_pat = loopNestingPattern first_nest
      rts = map (length ispace `stripArray`) $ patternTypes nested_pat
      lvl = SegGroup (Count num_groups) (Count $ Var group_size) SegNoVirt
      kstm = Let nested_pat (StmAux cs ()) $ Op $ SegOp $ SegMap lvl kspace rts kbody'

  let intra_min_par = intra_avail_par
  return ((intra_min_par, intra_avail_par), Var group_size,
           prelude_stms, oneStm kstm)
  where first_nest = fst knest
        cs = loopNestingCertificates first_nest

data Env = Env { _dataDeps :: Dependencies
               , _groupVariant :: Names
               }

type IntraGroupM = BinderT Out.Kernels (RWS Env (S.Set [SubExp], S.Set [SubExp]) VNameSource)

runIntraGroupM :: (MonadFreshNames m, HasScope Out.Kernels m) =>
                  Env -> IntraGroupM () -> m ([[SubExp]], [[SubExp]], Out.Stms Out.Kernels)
runIntraGroupM env m = do
  scope <- castScope <$> askScope
  modifyNameSource $ \src ->
    let (((), kstms), src', (ws_min, ws_avail)) = runRWS (runBinderT m scope) env src
    in ((S.toList ws_min, S.toList ws_avail, kstms), src')

parallelMin :: [SubExp] -> IntraGroupM ()
parallelMin ws = tell (S.singleton ws, S.singleton ws)

intraGroupBody :: SegLevel -> Body -> IntraGroupM (Out.Body Out.Kernels)
intraGroupBody lvl body = do
  stms <- collectStms_ $ mapM_ (intraGroupStm lvl) $ bodyStms body
  return $ mkBody stms $ bodyResult body

intraGroupStm :: SegLevel -> Stm -> IntraGroupM ()
intraGroupStm lvl stm@(Let pat aux e) = do
  Env deps group_variant <- ask
  let groupInvariant (Var v) =
        S.null . S.intersection group_variant .
        flip (M.findWithDefault mempty) deps $ v
      groupInvariant Constant{} = True
      lvl' = SegThread (segNumGroups lvl) (segGroupSize lvl) SegNoVirt

  case e of
    DoLoop ctx val form loopbody ->
      localScope (scopeOf form') $
      localScope (scopeOfFParams $ map fst $ ctx ++ val) $ do
      loopbody' <- intraGroupBody lvl loopbody
      certifying (stmAuxCerts aux) $
        letBind_ pat $ DoLoop ctx val form' loopbody'
          where form' = case form of
                          ForLoop i it bound inps -> ForLoop i it bound inps
                          WhileLoop cond          -> WhileLoop cond

    If cond tbody fbody ifattr
      | groupInvariant cond -> do
          tbody' <- intraGroupBody lvl tbody
          fbody' <- intraGroupBody lvl fbody
          certifying (stmAuxCerts aux) $
            letBind_ pat $ If cond tbody' fbody' ifattr

    Op (Screma w form arrs)
      | Just lam <- isMapSOAC form -> do
      let lam' = soacsLambdaToKernels lam
      certifying (stmAuxCerts aux) $
        addStms =<< segMap lvl' pat w lam' arrs [] []
      parallelMin [w]

    Op (Screma w form arrs)
      | Just (scanfun, nes, mapfun) <- isScanomapSOAC form -> do
      let scanfun' = soacsLambdaToKernels scanfun
          mapfun' = soacsLambdaToKernels mapfun
      certifying (stmAuxCerts aux) $
        addStms =<< segScan lvl' pat w scanfun' mapfun' nes arrs [] []
      parallelMin [w]

    Op (Screma w form arrs)
      | Just (reds, map_lam) <- isRedomapSOAC form,
        Reduce comm red_lam nes <- singleReduce reds -> do
      let red_lam' = soacsLambdaToKernels red_lam
          map_lam' = soacsLambdaToKernels map_lam
      certifying (stmAuxCerts aux) $
        addStms =<< segRed lvl' pat w [SegRedOp comm red_lam' nes mempty] map_lam' arrs [] []
      parallelMin [w]

    Op (Stream w (Sequential accs) lam arrs)
      | chunk_size_param : _ <- lambdaParams lam -> do
      types <- asksScope castScope
      ((), stream_bnds) <-
        runBinderT (sequentialStreamWholeArray pat w accs lam arrs) types
      let replace (Var v) | v == paramName chunk_size_param = w
          replace se = se
          replaceSets (x, y) = (S.map (map replace) x, S.map (map replace) y)
      censor replaceSets $ mapM_ (intraGroupStm lvl) stream_bnds

    Op (Scatter w lam ivs dests) -> do
      write_i <- newVName "write_i"
      space <- mkSegSpace [(write_i, w)]

      let lam' = soacsLambdaToKernels lam
          (dests_ws, dests_ns, dests_vs) = unzip3 dests
          (i_res, v_res) = splitAt (sum dests_ns) $ bodyResult $ lambdaBody lam'
          krets = do (a_w, a, is_vs) <- zip3 dests_ws dests_vs $ chunks dests_ns $ zip i_res v_res
                     return $ WriteReturns [a_w] a [ ([i],v) | (i,v) <- is_vs ]
          inputs = do (p, p_a) <- zip (lambdaParams lam') ivs
                      return $ KernelInput (paramName p) (paramType p) p_a [Var write_i]

      kstms <- runBinder_ $ localScope (scopeOfSegSpace space) $ do
        mapM_ readKernelInput inputs
        addStms $ bodyStms $ lambdaBody lam'

      certifying (stmAuxCerts aux) $ do
        let ts = map rowType $ patternTypes pat
            body = KernelBody () kstms krets
        letBind_ pat $ Op $ SegOp $ SegMap lvl' space ts body

      parallelMin [w]

    _ ->
      addStm $ soacsStmToKernels stm

intraGroupStms :: SegLevel -> Stms SOACS -> IntraGroupM ()
intraGroupStms lvl = mapM_ (intraGroupStm lvl)

intraGroupParalleliseBody :: (MonadFreshNames m, HasScope Out.Kernels m) =>
                             SegLevel -> Dependencies -> Names -> Body
                          -> m ([[SubExp]], [[SubExp]], Out.KernelBody Out.Kernels)
intraGroupParalleliseBody lvl deps group_variant body = do
  (min_ws, avail_ws, kstms) <-
    runIntraGroupM (Env deps group_variant) $
    intraGroupStms lvl $ bodyStms body
  return (min_ws, avail_ws,
          KernelBody () kstms $ map Returns $ bodyResult body)
