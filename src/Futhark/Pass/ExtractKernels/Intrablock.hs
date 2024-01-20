{-# LANGUAGE TypeFamilies #-}

-- | Extract limited nested parallelism for execution inside
-- individual kernel threadblocks.
module Futhark.Pass.ExtractKernels.Intrablock (intrablockParallelise) where

import Control.Monad
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
-- "parallelism to be exploited" to avoid exploding shared memory.
--
-- We distinguish between "minimum group size" and "maximum
-- exploitable parallelism".
intrablockParallelise ::
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
intrablockParallelise knest lam = runMaybeT $ do
  (ispace, inps) <- lift $ flatKernel knest

  (num_tblocks, w_stms) <-
    lift $
      runBuilder $
        letSubExp "intra_num_tblocks"
          =<< foldBinOp (Mul Int64 OverflowUndef) (intConst Int64 1) (map snd ispace)

  let body = lambdaBody lam

  tblock_size <- newVName "computed_tblock_size"
  (wss_min, wss_avail, log, kbody) <-
    lift . localScope (scopeOfLParams $ lambdaParams lam) $
      intrablockParalleliseBody body

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
      letBindNames [tblock_size]
        =<< if null ws_min
          then
            eBinOp
              (SMin Int64)
              (eSubExp =<< letSubExp "max_tblock_size" (Op $ SizeOp $ GetSizeMax SizeThreadBlock))
              (eSubExp intra_avail_par)
          else foldBinOp' (SMax Int64) ws_min

      let inputIsUsed input = kernelInputName input `nameIn` freeIn body
          used_inps = filter inputIsUsed inps

      addStms w_stms
      read_input_stms <- runBuilder_ $ mapM readGroupKernelInput used_inps
      space <- SegSpace <$> newVName "phys_tblock_id" <*> pure ispace
      pure (intra_avail_par, space, read_input_stms)

  let kbody' = kbody {kernelBodyStms = read_input_stms <> kernelBodyStms kbody}

  let nested_pat = loopNestingPat first_nest
      rts = map (length ispace `stripArray`) $ patTypes nested_pat
      grid = KernelGrid (Count num_tblocks) (Count $ Var tblock_size)
      lvl = SegBlock SegNoVirt (Just grid)
      kstm =
        Let nested_pat aux $ Op $ SegOp $ SegMap lvl kspace rts kbody'

  let intra_min_par = intra_avail_par
  pure
    ( (intra_min_par, intra_avail_par),
      Var tblock_size,
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
      letBindNames [kernelInputName inp] $ BasicOp $ Replicate mempty $ Var v
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

type IntrablockM =
  BuilderT GPU (RWS () IntraAcc VNameSource)

instance MonadLogger IntrablockM where
  addLog log = tell mempty {accLog = log}

runIntrablockM ::
  (MonadFreshNames m, HasScope GPU m) =>
  IntrablockM () ->
  m (IntraAcc, Stms GPU)
runIntrablockM m = do
  scope <- castScope <$> askScope
  modifyNameSource $ \src ->
    let (((), kstms), src', acc) = runRWS (runBuilderT m scope) () src
     in ((acc, kstms), src')

parallelMin :: [SubExp] -> IntrablockM ()
parallelMin ws =
  tell
    mempty
      { accMinPar = S.singleton ws,
        accAvailPar = S.singleton ws
      }

intrablockBody :: Body SOACS -> IntrablockM (Body GPU)
intrablockBody body = do
  stms <- collectStms_ $ intrablockStms $ bodyStms body
  pure $ mkBody stms $ bodyResult body

intrablockLambda :: Lambda SOACS -> IntrablockM (Lambda GPU)
intrablockLambda lam =
  mkLambda (lambdaParams lam) $
    bodyBind =<< intrablockBody (lambdaBody lam)

intrablockWithAccInput :: WithAccInput SOACS -> IntrablockM (WithAccInput GPU)
intrablockWithAccInput (shape, arrs, Nothing) =
  pure (shape, arrs, Nothing)
intrablockWithAccInput (shape, arrs, Just (lam, nes)) = do
  lam' <- intrablockLambda lam
  pure (shape, arrs, Just (lam', nes))

intrablockStm :: Stm SOACS -> IntrablockM ()
intrablockStm stm@(Let pat aux e) = do
  scope <- askScope
  let lvl = SegThreadInBlock SegNoVirt

  case e of
    Loop merge form loopbody ->
      localScope (scopeOfLoopForm form <> scopeOfFParams (map fst merge)) $ do
        loopbody' <- intrablockBody loopbody
        certifying (stmAuxCerts aux) . letBind pat $
          Loop merge form loopbody'
    Match cond cases defbody ifdec -> do
      cases' <- mapM (traverse intrablockBody) cases
      defbody' <- intrablockBody defbody
      certifying (stmAuxCerts aux) . letBind pat $
        Match cond cases' defbody' ifdec
    WithAcc inputs lam -> do
      inputs' <- mapM intrablockWithAccInput inputs
      lam' <- intrablockLambda lam
      certifying (stmAuxCerts aux) . letBind pat $ WithAcc inputs' lam'
    Op soac
      | "sequential_outer" `inAttrs` stmAuxAttrs aux ->
          intrablockStms . fmap (certify (stmAuxCerts aux))
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
                      liftInner . collectStms_ . intrablockStms,
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
        -- FIXME: Futhark.CodeGen.ImpGen.GPU.Block.compileGroupOp
        -- cannot handle multiple scan operators yet.
        Scan scanfun nes <- singleScan scans -> do
          let scanfun' = soacsLambdaToGPU scanfun
              mapfun' = soacsLambdaToGPU mapfun
          certifying (stmAuxCerts aux) $
            addStms =<< segScan lvl pat mempty w [SegBinOp Noncommutative scanfun' nes mempty] mapfun' arrs [] []
          parallelMin [w]
    Op (Screma w arrs form)
      | Just (reds, map_lam) <- isRedomapSOAC form -> do
          let onReduce (Reduce comm red_lam nes) =
                SegBinOp comm (soacsLambdaToGPU red_lam) nes mempty
              reds' = map onReduce reds
              map_lam' = soacsLambdaToGPU map_lam
          certifying (stmAuxCerts aux) $
            addStms =<< segRed lvl pat mempty w reds' map_lam' arrs [] []
          parallelMin [w]
    Op (Screma w arrs form) ->
      -- This screma is too complicated for us to immediately do
      -- anything, so split it up and try again.
      mapM_ intrablockStm . fmap (certify (stmAuxCerts aux)) . snd
        =<< runBuilderT (dissectScrema pat w form arrs) (scopeForSOACs scope)
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
          censor replaceSets $ intrablockStms stream_stms
    Op (Scatter w ivs lam dests) -> do
      write_i <- newVName "write_i"
      space <- mkSegSpace [(write_i, w)]

      let lam' = soacsLambdaToGPU lam
          krets = do
            (_a_w, a, is_vs) <-
              groupScatterResults dests $ bodyResult $ lambdaBody lam'
            let cs =
                  foldMap (foldMap resCerts . fst) is_vs
                    <> foldMap (resCerts . snd) is_vs
                is_vs' = [(Slice $ map (DimFix . resSubExp) is, resSubExp v) | (is, v) <- is_vs]
            pure $ WriteReturns cs a is_vs'
          inputs = do
            (p, p_a) <- zip (lambdaParams lam') ivs
            pure $ KernelInput (paramName p) (paramType p) p_a [Var write_i]

      kstms <- runBuilder_ $
        localScope (scopeOfSegSpace space) $ do
          mapM_ readKernelInput inputs
          addStms $ bodyStms $ lambdaBody lam'

      certifying (stmAuxCerts aux) $ do
        let body = KernelBody () kstms krets
        letBind pat $ Op $ SegOp $ SegMap lvl space (patTypes pat) body

      parallelMin [w]
    _ ->
      addStm $ soacsStmToGPU stm

intrablockStms :: Stms SOACS -> IntrablockM ()
intrablockStms = mapM_ intrablockStm

intrablockParalleliseBody ::
  (MonadFreshNames m, HasScope GPU m) =>
  Body SOACS ->
  m ([[SubExp]], [[SubExp]], Log, KernelBody GPU)
intrablockParalleliseBody body = do
  (IntraAcc min_ws avail_ws log, kstms) <-
    runIntrablockM $ intrablockStms $ bodyStms body
  pure
    ( S.toList min_ws,
      S.toList avail_ws,
      log,
      KernelBody () kstms $ map ret $ bodyResult body
    )
  where
    ret (SubExpRes cs se) = Returns ResultMaySimplify cs se
