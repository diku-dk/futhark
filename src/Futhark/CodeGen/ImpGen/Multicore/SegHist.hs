module Futhark.CodeGen.ImpGen.Multicore.SegHist
  (compileSegHist
  )
  where

import Control.Monad
import Data.List
import Prelude hiding (quot, rem)

import qualified Futhark.CodeGen.ImpCode.Multicore as Imp
import Futhark.Util (chunks, splitFromEnd, takeLast)
import Futhark.CodeGen.ImpGen
import Futhark.Representation.ExplicitMemory

import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.CodeGen.ImpGen.Multicore.SegRed (compileSegRed')
import Futhark.MonadFreshNames


newtype MySegHistSlug = MySegHistSlug
                       { mySlugOp :: HistOp ExplicitMemory }

computeHistoUsage :: HistOp ExplicitMemory
                  -> MulticoreGen MySegHistSlug
computeHistoUsage op =
  return $ MySegHistSlug op

type InitLocalHistograms = [SubExp ->
                            MulticoreGen ([VName], [Imp.Exp] -> MulticoreGen ())]

prepareIntermediateArrays :: VName
                               -> [MySegHistSlug]
                               -> MulticoreGen InitLocalHistograms
prepareIntermediateArrays num_subhistos_per_group =
  mapM onOp
  where
    onOp (MySegHistSlug op) = do
      let mk_op arrs bucket = do
            let op' = histOp op
                (acc_params, _arr_params) = splitAt (length arrs) $ lambdaParams op'
                bind_acc_params =
                  sComment "bind lhs" $
                    forM_ (zip acc_params arrs) $ \(acc_p, arr) ->
                      copyDWIMFix (paramName acc_p) [] (Var arr) bucket
                op_body = sComment "execute operation" $
                          compileBody' acc_params $ lambdaBody op'
                do_hist =
                  sComment "update sub hist result" $
                  zipWithM_ (writeArray bucket) arrs $ map (Var . paramName) acc_params

            sComment "Start of body" $ do
              dLParams acc_params
              bind_acc_params
              op_body
              do_hist

      -- Initialise local-memory sub-histograms.  These are
      -- represented as two-dimensional arrays.
      let init_local_subhistos hist_H_chk = do
            local_subhistos <-
              forM (histType op) $ \t -> do
                let sub_local_shape =
                      Shape [Var num_subhistos_per_group] <>
                      (arrayShape t `setOuterDim` hist_H_chk)
                sAllocArray "subhistogram_local"
                  (elemType t) sub_local_shape DefaultSpace

            return (local_subhistos, mk_op local_subhistos)

      return init_local_subhistos

    writeArray bucket arr val = copyDWIMFix arr bucket val []


compileSegHist  :: Pattern ExplicitMemory
                -> SegSpace
                -> [HistOp ExplicitMemory]
                -> KernelBody ExplicitMemory
                -> MulticoreGen ()
compileSegHist pat space histops kbody
  | segment_depth <- unSegSpace space,
    length segment_depth == 1 =
      nonsegmentedHist pat space histops kbody
  | otherwise =
      segmentedHist pat space histops kbody


segmentedHist :: Pattern ExplicitMemory
                -> SegSpace
                -> [HistOp ExplicitMemory]
                -> KernelBody ExplicitMemory
                -> MulticoreGen ()
segmentedHist pat space histops kbody = do
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  tid <- dPrim "tid" $ IntType Int32

  -- iteration variable
  n_segments <- dPrim "segment_iter" $ IntType Int32
  n_segments' <- toExp $ Var n_segments

  let num_red_res = length histops + sum (map (length . histNeutral) histops)
      (all_red_pes, map_pes) = splitAt num_red_res $ patternValueElements pat


  fbody <- collect $ do
    let inner_bound = last ns'

    sFor "i" inner_bound $ \i -> do
      dPrimV_ (segFlat space) (n_segments' * inner_bound + i)
      zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space


      compileStms mempty (kernelBodyStms kbody) $ do
        let (red_res, map_res) = splitFromEnd (length map_pes) $
                                 map kernelResultSubExp $ kernelBodyResult kbody
            (buckets, vs) = splitAt (length histops) red_res
            perOp = chunks $ map (length . histDest) histops

        forM_ (zip3 histops (perOp vs) buckets) $
           \(HistOp dest_w _ _ _ shape lam, vs', bucket) -> do

             let (is_params, vs_params) = splitAt (length vs') $ lambdaParams lam
                 bucket'   = toExp' int32 bucket
                 dest_w'   = toExp' int32 dest_w
                 bucket_in_bounds = bucket' .<. dest_w' .&&. 0 .<=. bucket'

             sComment "save map-out results" $
               forM_ (zip map_pes map_res) $ \(pe, res) ->
                 copyDWIMFix (patElemName pe) (map Imp.vi32 is) res []

             sComment "perform updates" $
               sWhen bucket_in_bounds $ do
               dLParams $ lambdaParams lam
               sLoopNest shape $ \is' -> do
                 -- Index
                 buck <- toExp bucket
                 forM_ (zip all_red_pes is_params) $ \(pe, p) ->
                   copyDWIMFix (paramName p) [] (Var $ patElemName pe) (map Imp.vi32 (init is)  ++ [buck] ++ is')
                 -- Value at index
                 forM_ (zip vs_params vs') $ \(p, v) ->
                   copyDWIMFix (paramName p) [] v []
                 compileStms mempty (bodyStms $ lambdaBody lam) $
                   forM_ (zip all_red_pes  $ bodyResult $ lambdaBody lam) $
                   \(pe, se) -> copyDWIMFix (patElemName pe) (map Imp.vi32 (init is) ++ [buck] ++ is') se []

  let paramsNames = namesToList $ freeIn fbody `namesSubtract`
                                  namesFromList (tid : [n_segments])

  ts <- mapM lookupType paramsNames
  let params = zipWith toParam paramsNames ts

  ntask <- dPrim "num_tasks" $ IntType Int32

  emit $ Imp.Op $ Imp.ParLoop ntask n_segments (product $ init ns')
                              (Imp.MulticoreFunc params mempty fbody tid)



nonsegmentedHist :: Pattern ExplicitMemory
                -> SegSpace
                -> [HistOp ExplicitMemory]
                -> KernelBody ExplicitMemory
                -> MulticoreGen ()
nonsegmentedHist pat space histops kbody = do
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  let pes = patternElements pat
      num_red_res = length histops + sum (map (length . histNeutral) histops)
      (all_red_pes, map_pes) = splitAt num_red_res pes


  slugs <- mapM computeHistoUsage histops

  -- variable for how many subhistograms to allocate
  hist_B' <- toExp $ Constant $ IntValue $ Int32Value 12 -- just to be sure
  hist_M <- dPrimV "hist_M" hist_B'

  init_histograms <-
    prepareIntermediateArrays hist_M slugs

  hist_H_chks <- forM (map (histWidth . mySlugOp) slugs) $ \w -> do
    w' <- toExp w
    dPrimV "hist_H_chk" w'

  thread_id <- dPrim "thread_id" $ IntType Int32
  tid_exp <- toExp $ Var thread_id


  -- Actually allocate subhistograms
  histograms <- forM (zip init_histograms hist_H_chks) $
                \(init_local_subhistos, hist_H_chk) -> do
    (local_subhistos, do_op) <- init_local_subhistos (Var hist_H_chk)
    return (local_subhistos, hist_H_chk, do_op)

  prebody <- collect $ do
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
    forM_ (zip3 histograms hist_H_chks slugs) $ \((hists, _, _), hist_H_chk, slug) ->
      forM_ (zip3 (patternElements pat) hists (histNeutral $ mySlugOp slug)) $ \(pe, hist, ne) -> do
        hist_H_chk' <- toExp $ Var hist_H_chk

        -- First thread initializes histrogram wtih dest vals
        let is_first_tid = tid_exp .==. 0
        sFor "i" hist_H_chk' $ \i ->
          sIf is_first_tid
              (copyDWIMFix hist (tid_exp : [i]) (Var $ patElemName pe) (map Imp.vi32 (init is) ++ [i]))
              (sLoopNest (histShape $ mySlugOp slug) $ \is' ->
                 copyDWIMFix hist (tid_exp : i : is') ne [])


  -- Generate loop body of parallel function
  body' <- collect $ do
     zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
     compileStms mempty (kernelBodyStms kbody) $ do
       let (red_res, map_res) = splitFromEnd (length map_pes) $ kernelBodyResult kbody
           (buckets, vs) = splitAt (length slugs) red_res
           perOp = chunks $ map (length . histDest . mySlugOp) slugs

       sComment "save map-out results" $
          forM_ (zip map_pes map_res) $ \(pe, res) ->
          copyDWIMFix (patElemName pe)
          (map Imp.vi32 is) (kernelResultSubExp res) []

       forM_ (zip4 (map mySlugOp slugs) histograms buckets (perOp vs)) $
         \(HistOp dest_w _ _ _ shape lam,
           (_, _hist_H_chk, do_op), bucket, vs') -> do

           let bucket' = toExp' int32 $ kernelResultSubExp bucket
               dest_w' = toExp' int32 dest_w
               bucket_in_bounds = bucket' .<. dest_w' .&&. 0 .<=. bucket'
               vs_params = takeLast (length vs') $ lambdaParams lam
               bucket_is = [tid_exp, bucket']

           sComment "perform updates" $
             sWhen bucket_in_bounds $ do
             dLParams $ lambdaParams lam
             sLoopNest shape $ \is' -> do
               forM_ (zip vs_params vs') $ \(p, res) ->
                 copyDWIMFix (paramName p) [] (kernelResultSubExp res) is'
               do_op (bucket_is ++ is')


  let paramsNames = namesToList $ freeIn (prebody <> body') `namesSubtract`
                                  namesFromList (thread_id : [segFlat space])
  ts <- mapM lookupType paramsNames
  let params = zipWith toParam paramsNames ts

  -- How many subtasks was used by scheduler
  num_histos <- dPrim "num_histos" $ IntType Int32

  -- emit $ Imp.DebugPrint "OK" $ Nothing
  emit $ Imp.Op $ Imp.ParLoop num_histos (segFlat space) (product ns')
                              (Imp.MulticoreFunc params prebody body' thread_id)


  let pes_per_op = chunks (map (length . histDest) histops) all_red_pes

  forM_ (zip3 pes_per_op histograms histops) $ \(red_pes, (hists,_,_),  op) -> do
    bucket_id <- newVName "bucket_id"
    subhistogram_id <- newVName "subhistogram_id"

    flat_gtid <- newVName "flat_gtid"
    let unitHistoCase =
       -- This is OK because the memory blocks are at least as
       -- large as the ones we are supposed to use for the result.
         forM_ (zip red_pes hists) $ \(pe, subhisto) -> do
           emit $ Imp.DebugPrint "unitHistoCase" Nothing
           pe_mem <- memLocationName . entryArrayLocation <$>
                     lookupArray (patElemName pe)
           subhisto_mem <- memLocationName . entryArrayLocation <$>
                           lookupArray subhisto
           emit $ Imp.SetMem pe_mem subhisto_mem DefaultSpace

    sIf (Imp.var num_histos int32 .==. 1) unitHistoCase $ do


      let num_buckets = histWidth op

      let segred_space =
            SegSpace flat_gtid $
            segment_dims ++
            [(bucket_id, num_buckets)] ++
            [(subhistogram_id, Var num_histos)]

      let segred_op = SegRedOp Commutative (histOp op) (histNeutral op) (histShape op)
      compileSegRed' (Pattern [] red_pes) segred_space [segred_op] $ \red_cont -> do
        let
        red_cont $ flip map hists $ \subhisto ->
              (Var subhisto, map Imp.vi32 $
                map fst segment_dims ++ [subhistogram_id, bucket_id])
   where segment_dims = init $ unSegSpace space



-- A different version of segHist
-- Takes sequential version and simpley chunks is
-- Implementation does currently not ensure amotic updates

-- compileSegOp pat  (SegHist _ space histops _ kbody) = do
--   let (is, ns) = unzip $ unSegSpace space
--   ns' <- mapM toExp ns
--   let num_red_res = length histops + sum (map (length . histNeutral) histops)
--       (_all_red_pes, map_pes) = splitAt num_red_res $ patternValueElements pat

--   body' <- collect $ do
--     zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
--     compileStms mempty (kernelBodyStms kbody) $ do
--       let (red_res, _map_res) = splitFromEnd (length map_pes) $
--                                map kernelResultSubExp $ kernelBodyResult kbody
--           (buckets, vs) = splitAt (length histops) red_res
--           perOp = chunks $ map (length . histDest) histops

--       forM_ (zip3 histops (perOp vs) buckets) $
--          \(HistOp dest_w _ _ _ shape lam, vs', bucket) -> do

--            let vs_params = takeLast (length vs') $ lambdaParams lam
--                is_params = take (length vs') $ lambdaParams lam
--                bucket'   = toExp' int32 bucket
--                dest_w'   = toExp' int32 dest_w
--                bucket_in_bounds = bucket' .<. dest_w' .&&. 0 .<=. bucket'

--            sComment "perform non-atomic updates" $
--              sWhen bucket_in_bounds $ do
--              dLParams $ lambdaParams lam
--              sLoopNest shape $ \_is' -> do
--                -- Index
--                buck <- toExp bucket
--                forM_ (zip (patternElements pat) is_params) $ \(pe, p) ->
--                  copyDWIMFix (paramName p) [] (Var $ patElemName pe) [buck]
--                -- Value at index
--                forM_ (zip vs_params vs') $ \(p, v) ->
--                  copyDWIMFix (paramName p) [] v []
--                compileStms mempty (bodyStms $ lambdaBody lam) $
--                  forM_ (zip (patternElements pat)  $ bodyResult $ lambdaBody lam) $
--                  \(pe, se) ->
--                    copyDWIMFix (patElemName pe) [buck] se [] -- TODO fix this offset


--   thread_id <- dPrim "thread_id" $ IntType Int32

--   let paramsNames = namesToList (freeIn body' `namesSubtract`
--                                 (namesFromList $ thread_id : [segFlat space]))
--   ts <- mapM lookupType paramsNames
--   let params = zipWith toParam paramsNames ts

--   emit $ Imp.Op $ Imp.ParLoop (segFlat space) (product ns')
--                               (Imp.MulticoreFunc params mempty body' thread_id)
