{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Extract limited nested parallelism for execution inside
-- individual kernel workgroups.
module Futhark.Pass.ExtractKernels.Intragroup
  (intraGroupParallelise)
where

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
import qualified Futhark.Pass.ExtractKernels.Kernelise as Kernelise
import Futhark.Pass.ExtractKernels.Distribution
import Futhark.Pass.ExtractKernels.BlockedKernel

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
  (w_stms, w, ispace, inps, rts) <- lift $ flatKernel knest
  let num_groups = w
      body = lambdaBody lam

  ltid <- newVName "ltid"
  let group_variant = S.fromList [ltid]
  (wss_min, wss_avail, kbody) <-
    lift $ localScope (scopeOfLParams $ lambdaParams lam) $
    intraGroupParalleliseBody (dataDependencies body) group_variant ltid body

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
    -- exploited, or some configurable size (bounded by the amount of
    -- exploitable parallelism) in case there is no minimum.
    group_size <- letSubExp "computed_group_size" =<<
                  if null ws_min
                  then eBinOp (SMin Int32)
                       (eSubExp =<< getSize "intra_group_size" Out.SizeGroup)
                       (eSubExp intra_avail_par)
                  else foldBinOp' (SMax Int32) ws_min

    let inputIsUsed input = kernelInputName input `S.member` freeInBody body
        used_inps = filter inputIsUsed inps

    addStms w_stms

    num_threads <- letSubExp "num_threads" $
                   BasicOp $ BinOp (Mul Int32) num_groups group_size

    let ksize = (num_groups, group_size, num_threads)

    kspace <- newKernelSpace ksize $ FlatThreadSpace $ ispace ++ [(ltid,group_size)]

    read_input_stms <- mapM readKernelInput used_inps

    return (intra_avail_par, kspace, read_input_stms)

  let kbody' = kbody { kernelBodyStms = stmsFromList read_input_stms <> kernelBodyStms kbody }

  -- The kernel itself is producing a "flat" result of shape
  -- [num_groups].  We must explicitly reshape it to match the shapes
  -- of our enclosing map-nests.
  let nested_pat = loopNestingPattern first_nest
      flatPatElem pat_elem = do
        let t' = arrayOfRow (length ispace `stripArray` patElemType pat_elem) num_groups
        name <- newVName $ baseString (patElemName pat_elem) ++ "_flat"
        return $ PatElem name t'
  flat_pat <- lift $ Pattern [] <$> mapM flatPatElem (patternValueElements nested_pat)

  let kstm = Let flat_pat (StmAux cs ()) $ Op $
             Kernel (KernelDebugHints "map_intra_group" []) kspace rts kbody'
      reshapeStm nested_pe flat_pe =
        Let (Pattern [] [nested_pe]) (StmAux cs ()) $
        BasicOp $ Reshape (map DimNew $ arrayDims $ patElemType nested_pe) $
        patElemName flat_pe
      reshape_stms = zipWith reshapeStm (patternElements nested_pat)
                                        (patternElements flat_pat)

  let intra_min_par = intra_avail_par
  return ((intra_min_par, intra_avail_par), spaceGroupSize kspace,
           prelude_stms, oneStm kstm <> stmsFromList reshape_stms)
  where first_nest = fst knest
        cs = loopNestingCertificates first_nest

data Env = Env { _localTID :: VName
               , _dataDeps :: Dependencies
               , _groupVariant :: Names
               }

type IntraGroupM = BinderT Out.InKernel (RWS Env (S.Set [SubExp], S.Set [SubExp]) VNameSource)

runIntraGroupM :: (MonadFreshNames m, HasScope Out.Kernels m) =>
                  Env -> IntraGroupM () -> m ([[SubExp]], [[SubExp]], Out.Stms Out.InKernel)
runIntraGroupM env m = do
  scope <- castScope <$> askScope
  modifyNameSource $ \src ->
    let (((), kstms), src', (ws_min, ws_avail)) = runRWS (runBinderT m scope) env src
    in ((S.toList ws_min, S.toList ws_avail, kstms), src')

parallelMin :: [SubExp] -> IntraGroupM ()
parallelMin ws = tell (S.singleton ws, S.singleton ws)

parallelAvail :: [SubExp] -> IntraGroupM ()
parallelAvail ws = tell (mempty, S.singleton ws)

intraGroupBody :: Body -> IntraGroupM (Out.Body Out.InKernel)
intraGroupBody body = do
  stms <- collectStms_ $ mapM_ intraGroupStm $ bodyStms body
  return $ mkBody stms $ bodyResult body

intraGroupStm :: Stm -> IntraGroupM ()
intraGroupStm stm@(Let pat _ e) = do
  Env ltid deps group_variant <- ask
  let groupInvariant (Var v) =
        S.null . S.intersection group_variant .
        flip (M.findWithDefault mempty) deps $ v
      groupInvariant Constant{} = True

  case e of
    DoLoop ctx val (ForLoop i it bound inps) loopbody
      | groupInvariant bound ->
          localScope (scopeOf form) $
          localScope (scopeOfFParams $ map fst $ ctx ++ val) $ do
          loopbody' <- intraGroupBody loopbody
          letBind_ pat $ DoLoop ctx val form loopbody'
              where form = ForLoop i it bound inps

    If cond tbody fbody ifattr
      | groupInvariant cond -> do
          tbody' <- intraGroupBody tbody
          fbody' <- intraGroupBody fbody
          letBind_ pat $ If cond tbody' fbody' ifattr

    Op (Screma w form arrs) | Just fun <- isMapSOAC form -> do
      body_stms <- collectStms_ $ do
        forM_ (zip (lambdaParams fun) arrs) $ \(p, arr) -> do
          arr_t <- lookupType arr
          letBindNames [paramName p] $ BasicOp $ Index arr $
            fullSlice arr_t [DimFix $ Var ltid]
        Kernelise.transformStms $ bodyStms $ lambdaBody fun
      let comb_body = mkBody body_stms $ bodyResult $ lambdaBody fun
      ctid <- newVName "ctid"
      letBind_ pat $ Op $
        Out.Combine (Out.combineSpace [(ctid, w)]) (lambdaReturnType fun) [] comb_body
      mapM_ (parallelMin . arrayDims) $ patternTypes pat
      parallelMin [w]

    Op (Screma w form arrs)
      | Just (scanfun, nes, foldfun) <- isScanomapSOAC form -> do
      let (scan_pes, map_pes) =
            splitAt (length nes) $ patternElements pat
      scan_input <- procInput ltid (Pattern [] map_pes) w foldfun nes arrs

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
      parallelMin [w]

    Op (Screma w form arrs)
      | Just (_, redfun, nes, foldfun) <- isRedomapSOAC form -> do
      let (red_pes, map_pes) =
            splitAt (length nes) $ patternElements pat
      red_input <- procInput ltid (Pattern [] map_pes) w foldfun nes arrs

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
      parallelMin [w]

    Op (Stream w (Sequential accs) lam arrs)
      | chunk_size_param : _ <- lambdaParams lam -> do
      types <- asksScope castScope
      ((), stream_bnds) <-
        runBinderT (sequentialStreamWholeArray pat w accs lam arrs) types
      let replace (Var v) | v == paramName chunk_size_param = w
          replace se = se
          replaceSets (x, y) = (S.map (map replace) x, S.map (map replace) y)
      censor replaceSets $ mapM_ intraGroupStm stream_bnds

    Op (Scatter w lam ivs dests) -> do
      parallelAvail [w]
      ctid <- newVName "ctid"
      let cspace = Out.CombineSpace dests [(ctid, w)]
      body_stms <- collectStms_ $ do
        forM_ (zip (lambdaParams lam) ivs) $ \(p, arr) -> do
          arr_t <- lookupType arr
          letBindNames [paramName p] $ BasicOp $ Index arr $
            fullSlice arr_t [DimFix $ Var ctid]
        Kernelise.transformStms $ bodyStms $ lambdaBody lam
      let body = mkBody body_stms $ bodyResult $ lambdaBody lam
      letBind_ pat $ Op $ Out.Combine cspace (lambdaReturnType lam) mempty body

    BasicOp (Update dest slice (Var v)) -> do
      let ws = sliceDims slice
          activeForDim w i = BasicOp $ CmpOp (CmpSlt Int32) i w
      parallelMin ws
      dest' <- letExp "update_inp" $ Op $ Out.Barrier [Var dest]
      let new_inds = unflattenIndex (map (primExpFromSubExp int32) ws)
                                    (primExpFromSubExp int32 $ Var ltid)
      new_inds' <- mapM (letSubExp "i" <=< toExp) new_inds
      active <- letSubExp "active" =<<
                foldBinOp LogAnd (constant True) =<<
                mapM (letSubExp "active") (zipWith activeForDim ws new_inds')
      (active_res, active_stms) <- collectStms $ do
        slice' <-
          mapM (letSubExp "j" <=< toExp) $
          fixSlice (map (fmap $ primExpFromSubExp int32) slice) new_inds
        letInPlace "update_res" dest' (map DimFix slice') $
          BasicOp $ Index v $ map DimFix new_inds'
      sync <- letSubExp "update_res" =<< eIf (eSubExp active)
        (pure $ mkBody active_stms [Var active_res])
        (pure $ mkBody mempty [Var dest'])
      letBind_ pat $ Op $ Out.Barrier [sync]

    BasicOp (Copy arr) -> do
      arr_t <- lookupType arr
      let w = arraySize 0 arr_t
      parallelAvail [w]
      ctid <- newVName "copy_ctid"
      letBind_ pat . Op . Out.Combine (Out.combineSpace [(ctid, w)]) [rowType arr_t] [] <=<
        localScope (M.singleton ctid $ IndexInfo Int32) $
        insertStmsM $ resultBodyM . pure <=< letSubExp "v" $
        BasicOp $ Index arr $ fullSlice arr_t [DimFix $ Var ctid]

    BasicOp (Replicate (Shape outer_ws) se)
      | [inner_ws] <- map (drop (length outer_ws) . arrayDims) $ patternTypes pat -> do
      let ws = outer_ws ++ inner_ws
      parallelAvail ws
      new_inds' <- replicateM (length ws) $ newVName "new_local_index"
      let inner_inds' = drop (length outer_ws) new_inds'
          space = Out.combineSpace $ zip new_inds' ws
          index = case se of Var v -> BasicOp $ Index v $
                                      map (DimFix . Var) inner_inds'
                             Constant{} -> BasicOp $ SubExp se
      body <- runBodyBinder $ eBody [pure index]
      letBind_ pat $ Op $
        Out.Combine space (map (Prim . elemType) $ patternTypes pat) [] body
      mapM_ (parallelAvail . arrayDims) $ patternTypes pat

    _ ->
      Kernelise.transformStm stm

  where procInput :: VName
                  -> Out.Pattern Out.InKernel
                  -> SubExp -> Lambda -> [SubExp] -> [VName]
                  -> IntraGroupM [VName]
        procInput ltid map_pat w map_fun nes arrs = do
          fold_stms <- collectStms_ $ do
            forM_ (zip (lambdaParams map_fun) arrs) $ \(p, arr) -> do
              arr_t <- lookupType arr
              letBindNames_ [paramName p] $ BasicOp $ Index arr $
                fullSlice arr_t [DimFix $ Var ltid]

            Kernelise.transformStms $ bodyStms $ lambdaBody map_fun
          let fold_body = mkBody fold_stms $ bodyResult $ lambdaBody map_fun

          op_inps <- replicateM (length nes) (newVName "op_input")
          ctid <- newVName "ctid"
          letBindNames_ (op_inps ++ patternNames map_pat) $ Op $
            Out.Combine (Out.combineSpace [(ctid, w)]) (lambdaReturnType map_fun) [] fold_body
          return op_inps

intraGroupParalleliseBody :: (MonadFreshNames m, HasScope Out.Kernels m) =>
                             Dependencies -> Names -> VName -> Body
                          -> m ([[SubExp]], [[SubExp]], Out.KernelBody Out.InKernel)
intraGroupParalleliseBody deps group_variant ltid body = do
  (min_ws, avail_ws, kstms) <- runIntraGroupM (Env ltid deps group_variant) $
                 mapM_ intraGroupStm $ bodyStms body
  return (min_ws, avail_ws,
          KernelBody () kstms $ map (ThreadsReturn OneResultPerGroup) $ bodyResult body)
