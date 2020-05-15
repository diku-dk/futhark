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
import Futhark.Representation.MCMem
import Futhark.Transform.Rename

import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.CodeGen.ImpGen.Multicore.SegRed (compileSegRed')
import Futhark.MonadFreshNames


newtype MySegHistSlug = MySegHistSlug
                       { mySlugOp :: HistOp MCMem }

computeHistoUsage :: HistOp MCMem
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
                acc_params = take (length arrs) $ lambdaParams op'
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


compileSegHist  :: Pattern MCMem
                -> SegSpace
                -> [HistOp MCMem]
                -> KernelBody MCMem
                -> MulticoreGen ()
compileSegHist pat space histops kbody
  | [_] <- unSegSpace space =
      nonsegmentedHist pat space histops kbody
  | otherwise =
      segmentedHist pat space histops kbody


segmentedHist :: Pattern MCMem
                -> SegSpace
                -> [HistOp MCMem]
                -> KernelBody MCMem
                -> MulticoreGen ()
segmentedHist pat space histops kbody = do
  emit $ Imp.DebugPrint "Segmented segHist" Nothing
  sUnpauseProfiling

  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  tid <- dPrim "tid" $ IntType Int32

  flat_idx <- dPrim "iter" int32

  -- iteration variable
  n_segments <- dPrim "segment_iter" $ IntType Int32
  n_segments' <- toExp $ Var n_segments

  let num_red_res = length histops + sum (map (length . histNeutral) histops)
      (all_red_pes, map_pes) = splitAt num_red_res $ patternValueElements pat

  dPrimV_ (segFlat space) 0

  fbody <- collect $ do
    emit $ Imp.DebugPrint "Segmented segHist" Nothing
    let inner_bound = last ns'

    sFor "i" inner_bound $ \i -> do
      dPrimV_ flat_idx (n_segments' * inner_bound + i)
      zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_idx

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
                   copyDWIMFix (paramName p) [] v is'
                 compileStms mempty (bodyStms $ lambdaBody lam) $
                   forM_ (zip all_red_pes  $ bodyResult $ lambdaBody lam) $
                   \(pe, se) -> copyDWIMFix (patElemName pe) (map Imp.vi32 (init is) ++ [buck] ++ is') se []

  let paramsNames = namesToList $ freeIn fbody `namesSubtract`
                                  namesFromList (tid : [n_segments])

  ts <- mapM lookupType paramsNames
  let params = zipWith toParam paramsNames ts

  ntask <- dPrim "num_tasks" $ IntType Int32

  emit $ Imp.Op $ Imp.ParLoop Imp.Static ntask n_segments (product $ init ns')
                              (Imp.MulticoreFunc params mempty fbody tid)


renameHistOpLambda :: [HistOp MCMem] -> MulticoreGen [HistOp MCMem]
renameHistOpLambda hist_ops =
  forM hist_ops $ \(HistOp w rf dest neutral shape lam) -> do
    lam' <- renameLambda lam
    return $ HistOp w rf dest neutral shape lam'

nonsegmentedHist :: Pattern MCMem
                -> SegSpace
                -> [HistOp MCMem]
                -> KernelBody MCMem
                -> MulticoreGen ()
nonsegmentedHist pat space histops kbody = do
  emit $ Imp.DebugPrint "nonsegmented segHist" Nothing
  sUnpauseProfiling

  let ns = map snd $ unSegSpace space
  ns' <- mapM toExp ns

  dPrimV_ (segFlat space) 0
  -- variable for how many subhistograms to allocate
  num_threads <- getNumThreads
  num_threads' <- toExp $ Var num_threads
  hist_width <- toExp $ histWidth $ head histops
  -- TODO we should find a proper condition
  let use_small_dest_histogram = (num_threads' * hist_width) .<=. product ns'
  histops' <- renameHistOpLambda histops

  sIf use_small_dest_histogram
     (smallDestHistogram pat space histops num_threads kbody)
     (largeDestHistogram pat space histops' kbody)

-- Generates num_threads sub histograms of the size
-- of the destination histogram
-- Then for each chunk of the input each subhistogram
-- is computed and finally merged through a reduction
-- across the histogram indicies.
-- This is expected to be fast if len(histDest) is small
smallDestHistogram :: Pattern MCMem
                   -> SegSpace
                   -> [HistOp MCMem]
                   -> VName
                   -> KernelBody MCMem
                   -> MulticoreGen ()
smallDestHistogram pat space histops num_threads kbody = do
  emit $ Imp.DebugPrint "smallDestHistogram segHist" Nothing

  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  flat_idx <- dPrim "iter" int32

  num_threads' <- toExp $ Var num_threads
  let pes = patternElements pat
      num_red_res = length histops + sum (map (length . histNeutral) histops)
      (all_red_pes, map_pes) = splitAt num_red_res pes
  hist_M <- dPrimV "hist_M" num_threads'

  slugs <- mapM computeHistoUsage histops

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
    emit $ Imp.DebugPrint "nonsegmented segHist stage 1" Nothing
    segFlat space <-- 0
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_idx
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
     zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_idx
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
                                  namesFromList (thread_id : [flat_idx])
  ts <- mapM lookupType paramsNames
  let params = zipWith toParam paramsNames ts

  -- How many subtasks was used by scheduler
  num_histos <- dPrim "num_histos" $ IntType Int32

  emit $ Imp.Op $ Imp.ParLoop Imp.Static num_histos flat_idx (product ns')
                              (Imp.MulticoreFunc params prebody body' thread_id)

  emit $ Imp.DebugPrint "nonsegmented hist stage 2"  Nothing
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

      emit $ Imp.DebugPrint "multiHistocase" Nothing

      let num_buckets = histWidth op

      let segred_space =
            SegSpace flat_gtid $
            segment_dims ++
            [(bucket_id, num_buckets)] ++
            [(subhistogram_id, Var num_histos)]

      let segred_op = SegBinOp Commutative (histOp op) (histNeutral op) (histShape op)
      compileSegRed' (Pattern [] red_pes) segred_space [segred_op] $ \red_cont ->
        red_cont $ flip map hists $ \subhisto ->
              (Var subhisto, map Imp.vi32 $
                map fst segment_dims ++ [subhistogram_id, bucket_id])
   where segment_dims = init $ unSegSpace space

  -- emit $ Imp.DebugPrint "smallDestHistogram segHist end " Nothing



-- |
-- A different version of segHist
-- Takes sequential version and simpley chunks is
-- Implementation does currently not ensure amotic updates
-- but just doing it sequentially, single threaded seems faster
largeDestHistogram :: Pattern MCMem
                   -> SegSpace
                   -> [HistOp MCMem]
                   -> KernelBody MCMem
                   -> MulticoreGen ()
largeDestHistogram pat space histops kbody = do
  emit $ Imp.DebugPrint "largeDestHistogram segHist" Nothing

  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns
  let num_red_res = length histops + sum (map (length . histNeutral) histops)
      (all_red_pes, map_pes) = splitAt num_red_res $ patternValueElements pat


  flat_idx <- dPrim "iter" int32
  emit $ Imp.DebugPrint "largeDestHistogram segHist body" Nothing
  -- body' <- collect $ do
  sFor "i" (product ns') $ \i -> do
    dPrimV_ flat_idx i
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_idx
    compileStms mempty (kernelBodyStms kbody) $ do
      let (red_res, map_res) = splitFromEnd (length map_pes) $ kernelBodyResult kbody
          (buckets, vs) = splitAt (length histops) red_res
          perOp = chunks $ map (length . histDest) histops

      let pes_per_op = chunks (map (length . histDest) histops) all_red_pes
      forM_ (zip4 histops (perOp vs) buckets pes_per_op) $
         \(HistOp dest_w _ _ _ shape lam, vs', bucket, dest_res) -> do

           let (is_params, vs_params) = splitAt (length vs') $ lambdaParams lam
               bucket'   = toExp' int32 $ kernelResultSubExp bucket
               dest_w'   = toExp' int32 dest_w
               bucket_in_bounds = bucket' .<. dest_w' .&&. 0 .<=. bucket'

           sComment "save map-out results" $
             forM_ (zip map_pes map_res) $ \(pe, res) ->
               copyDWIMFix (patElemName pe) (map Imp.vi32 is) (kernelResultSubExp res) []

           sComment "perform non-atomic updates" $
             sWhen bucket_in_bounds $ do
               dLParams $ lambdaParams lam
               let segment_dims =  map Imp.vi32 $ init is
               sLoopNest shape $ \is' -> do
                 forM_ (zip vs_params vs') $ \(p, res) ->
                   copyDWIMFix (paramName p) [] (kernelResultSubExp res) is'
                 -- sComment "bind lhs" $
                 forM_ (zip is_params dest_res) $ \(acc_p, pe) ->
                   copyDWIMFix (paramName acc_p) [] (Var $ patElemName pe) (segment_dims ++ bucket' : is')
                 -- sComment "execute operation" $
                 compileStms (freeIn $ bodyResult $ lambdaBody lam) (bodyStms $ lambdaBody lam) $
                   forM_ (zip dest_res $ bodyResult $ lambdaBody lam) $
                     \(pe, se) -> copyDWIMFix  (patElemName pe) (segment_dims ++ bucket' : is') se []


  -- thread_id <- dPrim "thread_id" $ IntType Int32
  -- let paramsNames = namesToList (freeIn body' `namesSubtract`
  --                               (namesFromList $ thread_id : [flat_idx]))
  -- ts <- mapM lookupType paramsNames
  -- let params = zipWith toParam paramsNames ts

  -- -- How many subtasks was used by scheduler
  -- num_subtasks <- dPrim "num_histos" $ IntType Int32

  -- emit $ Imp.Op $ Imp.ParLoop Imp.Static num_subtasks flat_idx (product ns')
  --                             (Imp.MulticoreFunc params mempty body' thread_id)
