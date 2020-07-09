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
import Futhark.IR.MCMem
import Futhark.Transform.Rename

import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.CodeGen.ImpGen.Multicore.SegRed (compileSegRed')
import Futhark.MonadFreshNames


type InitLocalHistograms = SubExp -> MulticoreGen ([VName], [Imp.Exp] -> MulticoreGen ())

onOp :: VName -> HistOp MCMem -> MulticoreGen InitLocalHistograms
onOp num_subhistos_per_group op = do
  let mk_op arrs bucket = do
        let acc_params = take (length arrs) $ lambdaParams $ histOp op
            bind_acc_params =
              sComment "bind lhs" $
                forM_ (zip acc_params arrs) $ \(acc_p, arr) ->
                  copyDWIMFix (paramName acc_p) [] (Var arr) bucket
            op_body = sComment "execute operation" $
                      compileBody' acc_params $ lambdaBody $ histOp op
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
            let sub_local_shape = Shape [Var num_subhistos_per_group] <>
                                  (arrayShape t `setOuterDim` hist_H_chk)
            sAllocArray "subhistogram" (elemType t) sub_local_shape DefaultSpace

        return (local_subhistos, mk_op local_subhistos)
  return init_local_subhistos

  where
    writeArray bucket arr val = copyDWIMFix arr bucket val []

prepareIntermediateArrays :: VName
                          -> [HistOp MCMem]
                          -> MulticoreGen [InitLocalHistograms]
prepareIntermediateArrays num_subhistos_per_group =
  mapM (onOp num_subhistos_per_group)

compileSegHist  :: Pattern MCMem
                -> SegSpace
                -> [HistOp MCMem]
                -> KernelBody MCMem
                -> Mode
                -> MulticoreGen Imp.Code
compileSegHist pat space histops kbody mode
  | [_] <- unSegSpace space =
      nonsegmentedHist pat space histops kbody mode
  | otherwise =
      segmentedHist pat space histops kbody mode

-- | Split some list into chunks equal to the number of values
-- returned by each 'SegBinOp'
segHistOpChunks :: [HistOp lore] -> [a] -> [[a]]
segHistOpChunks = chunks . map (length . histNeutral)


compileSegHistBody :: VName
                   -> Pattern MCMem
                   -> SegSpace
                   -> [HistOp MCMem]
                   -> KernelBody MCMem
                   -> MulticoreGen Imp.Code
compileSegHistBody idx pat space histops kbody = do
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  let num_red_res = length histops + sum (map (length . histNeutral) histops)
      (all_red_pes, map_pes) = splitAt num_red_res $ patternValueElements pat

  idx' <- toExp $ Var idx
  flat_idx <- dPrim "iter" int32
  collect $ do
    emit $ Imp.DebugPrint "Segmented segHist" Nothing
    let inner_bound = last ns'

    sFor "i" inner_bound $ \i -> do
      dPrimV_ flat_idx (idx' * inner_bound + i)
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






segmentedHist :: Pattern MCMem
              -> SegSpace
              -> [HistOp MCMem]
              -> KernelBody MCMem
              -> Mode
              -> MulticoreGen Imp.Code
segmentedHist pat space histops kbody ModeParallel = do
  emit $ Imp.DebugPrint "Segmented segHist" Nothing

  -- iteration variable
  n_segments <- dPrim "segment_iter" $ IntType Int32
  collect $ do
    par_body <- compileSegHistBody n_segments pat space histops kbody
    ntasks <- dPrim "num_tasks" $ IntType Int32
    free_params <- freeParams par_body [segFlat space, n_segments]
    let sched = decideScheduling par_body
    emit $ Imp.Op $ Imp.MCFunc n_segments mempty par_body free_params $
      Imp.MulticoreInfo ntasks sched (segFlat space)

segmentedHist pat space histops kbody ModeSequential = do
  let ns = map snd $ unSegSpace space
  ns' <- mapM toExp ns

  collect $ do
    n_seq_segments <- dPrim "segment_iter" $ IntType Int32
    seq_body <- compileSegHistBody n_seq_segments pat space histops kbody
    sFor "i" (product $ init ns') $ \i -> do
      n_seq_segments <-- i
      emit seq_body


renameHistOpLambda :: [HistOp MCMem] -> MulticoreGen [HistOp MCMem]
renameHistOpLambda hist_ops =
  forM hist_ops $ \(HistOp w rf dest neutral shape lam) -> do
    lam' <- renameLambda lam
    return $ HistOp w rf dest neutral shape lam'

nonsegmentedHist :: Pattern MCMem
                -> SegSpace
                -> [HistOp MCMem]
                -> KernelBody MCMem
                -> Mode
                -> MulticoreGen Imp.Code
nonsegmentedHist pat space histops kbody ModeParallel = do
  emit $ Imp.DebugPrint "nonsegmented segHist" Nothing
  let ns = map snd $ unSegSpace space
  ns' <- mapM toExp ns

  num_threads <- getNumThreads
  num_threads' <- toExp $ Var num_threads
  hist_width <- toExp $ histWidth $ head histops
  -- TODO we should find a proper condition
  let use_small_dest_histogram =  (num_threads' * hist_width) .<=. product ns'
  histops' <- renameHistOpLambda histops

  collect $ do
    flat_idx <- dPrim "iter" int32
    sIf use_small_dest_histogram
     (smallDestHistogram pat flat_idx space histops num_threads kbody)
     (largeDestHistogram pat space histops' kbody)

nonsegmentedHist pat space histops kbody ModeSequential = do
  let ns = map snd $ unSegSpace space
  ns' <- mapM toExp ns

  collect $ localMode ModeSequential $ do
    flat_seq_idx <- dPrim "iter" int32
    seq_body <- sequentialHist flat_seq_idx pat space histops kbody
    emit $ Imp.DebugPrint "SegMap sequential" Nothing
    sFor "i" (product ns') $ \i -> do
      flat_seq_idx <-- i
      emit seq_body


-- Generates num_threads sub histograms of the size
-- of the destination histogram
-- Then for each chunk of the input each subhistogram
-- is computed and finally merged through a reduction
-- across the histogram indicies.
-- This is expected to be fast if len(histDest) is small
smallDestHistogram :: Pattern MCMem
                   -> VName
                   -> SegSpace
                   -> [HistOp MCMem]
                   -> VName
                   -> KernelBody MCMem
                   -> MulticoreGen ()
smallDestHistogram pat flat_idx space histops num_threads kbody = do
  emit $ Imp.DebugPrint "smallDestHistogram segHist" Nothing

  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns


  num_threads' <- toExp $ Var num_threads
  let pes = patternElements pat
      num_red_res = length histops + sum (map (length . histNeutral) histops)
      (all_red_pes, map_pes) = splitAt num_red_res pes

  let per_red_pes = segHistOpChunks histops $ patternValueElements pat
  hist_M <- dPrimV "hist_M" num_threads'

  init_histograms <-
    prepareIntermediateArrays hist_M histops

  hist_H_chks <- forM (map histWidth histops) $ \w -> do
    w' <- toExp w
    dPrimV "hist_H_chk" w'

  tid' <- toExp $ Var $ segFlat space

  -- Actually allocate subhistograms
  histograms <- forM (zip init_histograms hist_H_chks) $
                \(init_local_subhistos, hist_H_chk) -> do
    (local_subhistos, do_op) <- init_local_subhistos (Var hist_H_chk)
    return (local_subhistos, hist_H_chk, do_op)

  prebody <- collect $ do
    emit $ Imp.DebugPrint "nonsegmented segHist stage 1" Nothing
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_idx
    forM_ (zip4 per_red_pes histograms hist_H_chks histops) $ \(pes', (hists, _, _), hist_H_chk, histop) ->
      forM_ (zip3 pes' hists (histNeutral histop)) $ \(pe, hist, ne) -> do
        hist_H_chk' <- toExp $ Var hist_H_chk

        -- First thread initializes histrogram wtih dest vals
        -- Others initialize with neutral element
        let is_first_tid = tid' .==. 0
        sFor "i" hist_H_chk' $ \i ->
          sIf is_first_tid
              (copyDWIMFix hist (tid' : [i])
                           (Var $ patElemName pe) (map Imp.vi32 (init is) ++ [i]))
              (sLoopNest (histShape histop) $ \is' ->
                 copyDWIMFix hist (tid' : i : is') ne [])


  -- Generate loop body of parallel function
  body <- collect $ do
     zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_idx
     compileStms mempty (kernelBodyStms kbody) $ do
       let (red_res, map_res) = splitFromEnd (length map_pes) $ kernelBodyResult kbody
           (buckets, vs) = splitAt (length histops) red_res
           perOp = chunks $ map (length . histDest) histops

       sComment "save map-out results" $
          forM_ (zip map_pes map_res) $ \(pe, res) ->
          copyDWIMFix (patElemName pe)
          (map Imp.vi32 is) (kernelResultSubExp res) []

       forM_ (zip4 histops histograms buckets (perOp vs)) $
         \(HistOp dest_w _ _ _ shape lam,
           (_, _hist_H_chk, do_op), bucket, vs') -> do

           let bucket' = toExp' int32 $ kernelResultSubExp bucket
               dest_w' = toExp' int32 dest_w
               bucket_in_bounds = bucket' .<. dest_w' .&&. 0 .<=. bucket'
               vs_params = takeLast (length vs') $ lambdaParams lam
               bucket_is = [tid', bucket']

           sComment "perform updates" $
             sWhen bucket_in_bounds $ do
             dLParams $ lambdaParams lam
             sLoopNest shape $ \is' -> do
               forM_ (zip vs_params vs') $ \(p, res) ->
                 copyDWIMFix (paramName p) [] (kernelResultSubExp res) is'
               do_op (bucket_is ++ is')

  free_params <- freeParams (prebody <> body) (segFlat space : [flat_idx])
  let sched = decideScheduling body

  -- How many subtasks was used by scheduler
  num_histos <- dPrim "num_histos" $ IntType Int32
  emit $ Imp.Op $ Imp.MCFunc flat_idx prebody body free_params $
      Imp.MulticoreInfo num_histos sched (segFlat space)


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
      red_code <- compileSegRed' (Pattern [] red_pes) segred_space [segred_op] ModeParallel $ \red_cont ->
        red_cont $ flip map hists $ \subhisto ->
              (Var subhisto, map Imp.vi32 $
                map fst segment_dims ++ [subhistogram_id, bucket_id])
      emit red_code

   where segment_dims = init $ unSegSpace space




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
                 forM_ (zip is_params dest_res) $ \(acc_p, pe) ->
                   copyDWIMFix (paramName acc_p) [] (Var $ patElemName pe) (segment_dims ++ bucket' : is')
                 compileStms (freeIn $ bodyResult $ lambdaBody lam) (bodyStms $ lambdaBody lam) $
                   forM_ (zip dest_res $ bodyResult $ lambdaBody lam) $
                     \(pe, se) -> copyDWIMFix  (patElemName pe) (segment_dims ++ bucket' : is') se []


sequentialHist :: VName
               -> Pattern MCMem
               -> SegSpace
               -> [HistOp MCMem]
               -> KernelBody MCMem
               -> MulticoreGen Imp.Code
sequentialHist flat_idx pat space histops kbody = do
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns
  let num_red_res = length histops + sum (map (length . histNeutral) histops)
      (all_red_pes, map_pes) = splitAt num_red_res $ patternValueElements pat

  collect $ do
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
