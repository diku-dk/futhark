{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Extract limited nested parallelism for execution inside
-- individual kernel workgroups.
module Futhark.Pass.ExtractKernels.Intragroup
  (intraGroupParallelise)
where

import Control.Monad.RWS.Strict
import Control.Monad.Trans.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List

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
-- to be exploited does not exceed the group size.
intraGroupParallelise :: (MonadFreshNames m, HasScope Out.Kernels m) =>
                         KernelNest -> Body
                      -> m (Maybe (SubExp, SubExp,
                                   [Out.Stm Out.Kernels], [Out.Stm Out.Kernels]))
intraGroupParallelise knest body = runMaybeT $ do
  (w_stms, w, ispace, inps, rts) <- lift $ flatKernel knest
  let num_groups = w

  ltid <- newVName "ltid"
  let group_variant = S.fromList [ltid]
  (ws, kbody) <- lift $ intraGroupParalleliseBody (dataDependencies body) group_variant ltid body

  known_outside <- lift $ M.keys <$> askScope
  unless (all (`elem` known_outside) $ freeIn ws) $
    fail "Irregular parallelism"

  ((intra_avail_par, kspace, read_input_stms), prelude_stms) <- lift $ runBinder $ do
    -- Compute a group size that is the maximum of the inner
    -- parallelism exploited.
    group_size <- case ws of
      x:xs -> letSubExp "computed_group_size" =<< foldBinOp (SMax Int32) x xs
      []   -> return $ intConst Int32 0

    -- The amount of parallelism available *in the worst case* is
    -- equal to the smallest parallel loop.
    intra_avail_par <- case ws of
      x:xs -> letSubExp "intra_avail_par" =<< foldBinOp (SMin Int32) x xs
      []   -> return $ intConst Int32 0

    let inputIsUsed input = kernelInputName input `S.member` freeInBody body
        used_inps = filter inputIsUsed inps

    mapM_ addStm w_stms

    num_threads <- letSubExp "num_threads" $
                   BasicOp $ BinOp (Mul Int32) num_groups group_size

    let ksize = (num_groups, group_size, num_threads)

    kspace <- newKernelSpace ksize $ FlatThreadSpace $ ispace ++ [(ltid,group_size)]

    read_input_stms <- mapM readKernelInput used_inps

    return (intra_avail_par, kspace, read_input_stms)

  let kbody' = kbody { kernelBodyStms = read_input_stms ++ kernelBodyStms kbody }

  -- The kernel itself is producing a "flat" result of shape
  -- [num_groups].  We must explicitly reshape it to match the shapes
  -- of our enclosing map-nests.
  let nested_pat = loopNestingPattern first_nest
      flatPatElem pat_elem = do
        let t' = arrayOfRow (length ispace `stripArray` patElemType pat_elem) num_groups
        name <- newVName $ baseString (patElemName pat_elem) ++ "_flat"
        return $ PatElem name BindVar t'
  flat_pat <- lift $ Pattern [] <$> mapM flatPatElem (patternValueElements nested_pat)

  let kstm = Let flat_pat (StmAux cs ()) $ Op $
             Kernel (KernelDebugHints "map_intra_group" []) kspace rts kbody'
      reshapeStm nested_pe flat_pe =
        Let (Pattern [] [nested_pe]) (StmAux cs ()) $
        BasicOp $ Reshape (map DimNew $ arrayDims $ patElemType nested_pe) $
        patElemName flat_pe
      reshape_stms = zipWith reshapeStm (patternElements nested_pat)
                                        (patternElements flat_pat)

  return (intra_avail_par, spaceGroupSize kspace,
          prelude_stms, kstm : reshape_stms)
  where first_nest = fst knest
        cs = loopNestingCertificates first_nest

intraGroupParalleliseBody :: (MonadFreshNames m, HasScope Out.Kernels m) =>
                             Dependencies -> Names -> VName -> Body
                          -> m ([SubExp], Out.KernelBody Out.InKernel)
intraGroupParalleliseBody deps group_variant ltid body = do
  (ws, kstms) <- runBinder $ do
    let processStms = fmap concat . mapM processStm

        -- Without this type signature, the GHC 8.0.1 type checker
        -- enters an infinite loop.
        processStm :: Stm -> Binder Out.InKernel [SubExp]
        processStm stm@(Let pat _ e) =
          case e of
            DoLoop ctx val (ForLoop i it (Var bound) inps) loopbody
              | groupInvariant bound ->
                  localScope (scopeOf form) $
                  localScope (scopeOfFParams $ map fst $ ctx ++ val) $ do
                    (ws, stms) <- collectStms $ processStms $ bodyStms loopbody
                    letBind_ pat $ DoLoop ctx val form $ mkBody stms $ bodyResult loopbody
                    return ws
                      where form = ForLoop i it (Var bound) inps

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
              return [w]

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
              return [w]

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
              return [w]

            Op (Stream w (Sequential accs) lam arrs)
              | chunk_size_param : _ <- extLambdaParams lam -> do
              types <- asksScope castScope
              ((), stream_bnds) <-
                runBinderT (sequentialStreamWholeArray pat w accs lam arrs) types
              let replace (Var v) | v == paramName chunk_size_param = w
                  replace se = se
              map replace <$> processStms stream_bnds

            _ ->
              Kernelise.transformStm stm >> return []

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

  return (nub ws,
          KernelBody () kstms $ map (ThreadsReturn OneResultPerGroup) $ bodyResult body)

  where groupInvariant = S.null . S.intersection group_variant .
                         flip (M.findWithDefault mempty) deps
