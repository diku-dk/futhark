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

import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.CodeGen.ImpGen.Multicore.SegRed (compileSegRed')
import Futhark.MonadFreshNames



onOpCas :: HistOp MCMem -> MulticoreGen ([VName] -> [Imp.Exp] -> MulticoreGen())
onOpCas op = do
  let mk_op arrs bucket = do
        let acc_params = take (length arrs) $ lambdaParams $ histOp op
            bind_acc_params =
              sComment "bind lhs" $
                forM_ (zip acc_params arrs) $ \(acc_p, arr) ->
                  copyDWIMFix (paramName acc_p) [] (Var arr) bucket
            op_body = sComment "execute operation" $
                      compileBody' acc_params $ lambdaBody $ histOp op
            do_hist =
              sComment "update result" $
                zipWithM_ (writeArray bucket) arrs $ map (Var . paramName) acc_params

        sComment "Start of body" $ do
          dLParams acc_params
          bind_acc_params
          op_body
          do_hist
  return mk_op
  where
    writeArray bucket arr val = copyDWIMFix arr bucket val []



nonsegmentedHist :: Pattern MCMem
                -> SegSpace
                -> [HistOp MCMem]
                -> KernelBody MCMem
                -> MulticoreGen Imp.Code
nonsegmentedHist pat space histops kbody = do
  emit $ Imp.DebugPrint "nonsegmented segHist" Nothing
  -- let ns = map snd $ unSegSpace space
  -- ns' <- mapM toExp ns

  -- num_threads <- getNumThreads
  -- num_threads' <- toExp $ Var num_threads
  -- hist_width <- toExp $ histWidth $ head histops
  -- TODO we should find a proper condition
  -- let use_small_dest_histogram =  (num_threads' * hist_width) .<=. product ns'
  -- histops' <- renameHistOpLambda histops

  collect $ do
    -- flat_idx <- dPrim "iter" int32
    -- sIf use_small_dest_histogram
    -- smallDestHistogram pat flat_idx space histops num_threads kbody
    casHistogram pat space histops kbody



-- |
-- A different version of segHist
-- Takes sequential version and simpley chunks is
-- Implementation does currently not ensure amotic updates
casHistogram :: Pattern MCMem
             -> SegSpace
             -> [HistOp MCMem]
             -> KernelBody MCMem
             -> MulticoreGen ()
casHistogram pat space histops kbody = do
  emit $ Imp.DebugPrint "largeDestHistogram segHist" Nothing

  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns
  let num_red_res = length histops + sum (map (length . histNeutral) histops)
      (all_red_pes, map_pes) = splitAt num_red_res $ patternValueElements pat

  atomicOps <- mapM onOpCas histops

  idx <- dPrim "iter" int32
  emit $ Imp.DebugPrint "CAS segHist body" Nothing
  body <- collect $ do
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 idx
    compileStms mempty (kernelBodyStms kbody) $ do
      let (red_res, map_res) = splitFromEnd (length map_pes) $ kernelBodyResult kbody
          (buckets, vs) = splitAt (length histops) red_res
          perOp = chunks $ map (length . histDest) histops

      let pes_per_op = chunks (map (length . histDest) histops) all_red_pes
      forM_ (zip5 histops (perOp vs) buckets atomicOps pes_per_op) $
         \(HistOp dest_w _ _ _ shape lam, vs', bucket, do_op, dest_res) -> do

           let (is_params, vs_params) = splitAt (length vs') $ lambdaParams lam
               bucket'   = toExp' int32 $ kernelResultSubExp bucket
               dest_w'   = toExp' int32 dest_w
               bucket_in_bounds = bucket' .<. dest_w' .&&. 0 .<=. bucket'

           sComment "save map-out results" $
             forM_ (zip map_pes map_res) $ \(pe, res) ->
               copyDWIMFix (patElemName pe) (map Imp.vi32 is) (kernelResultSubExp res) []

           sComment "perform updates" $
             sWhen bucket_in_bounds $ do
             dLParams $ lambdaParams lam
             sLoopNest shape $ \is' -> do
               forM_ (zip vs_params vs') $ \(p, res) ->
                 copyDWIMFix (paramName p) [] (kernelResultSubExp res) is'
               do_op (map patElemName dest_res) (bucket' : is')


           -- sComment "perform non-atomic updates" $
           --   sWhen bucket_in_bounds $ do
           --     dLParams $ lambdaParams lam
           --     let segment_dims =  map Imp.vi32 $ init is
           --     sLoopNest shape $ \is' -> do
           --       forM_ (zip vs_params vs') $ \(p, res) ->
           --         copyDWIMFix (paramName p) [] (kernelResultSubExp res) is'
           --       forM_ (zip is_params dest_res) $ \(acc_p, pe) ->
           --         copyDWIMFix (paramName acc_p) [] (Var $ patElemName pe) (segment_dims ++ bucket' : is')
           --       compileStms (freeIn $ bodyResult $ lambdaBody lam) (bodyStms $ lambdaBody lam) $
           --         forM_ (zip dest_res $ bodyResult $ lambdaBody lam) $
           --           \(pe, se) -> copyDWIMFix  (patElemName pe) (segment_dims ++ bucket' : is') se []

  free_params <- freeParams body (segFlat space : [idx])
  num_histos <- dPrim "num_histos" $ IntType Int32

  let sched = decideScheduling body
  emit $ Imp.Op $ Imp.MCFunc idx mempty body free_params $
      Imp.MulticoreInfo num_histos sched (segFlat space)


-- | A function for generating code for an atomic update.  Assumes
-- that the bucket is in-bounds.
type DoAtomicUpdate lore r =
  [VName] -> [Imp.Exp] -> MulticoreGen ()

-- | The mechanism that will be used for performing the atomic update.
-- Approximates how efficient it will be.  Ordered from most to least
-- efficient.
data AtomicUpdate lore r
  = AtomicPrim (DoAtomicUpdate lore r)
    -- ^ Supported directly by primitive.
  | AtomicCAS (DoAtomicUpdate lore r)
    -- ^ Can be done by efficient swaps.
  -- | AtomicLocking (Locking -> DoAtomicUpdate lore r)
  --   -- ^ Requires explicit locking.

atomicUpdateLocking :: Lambda MCMem
                    -> AtomicUpdate lore r
atomicUpdateLocking op
  | [Prim t] <- lambdaReturnType op,
    [xp, _] <- lambdaParams op,
    primBitSize t == 32 = AtomicCAS $ \[arr] bucket -> do
      old <- dPrim "old" t
      atomicUpdateCAS t arr old bucket (paramName xp) $
        compileBody' [xp] $ lambdaBody op

atomicUpdateLocking _ =
  error "Type of hist not suppoorted yet"

atomicUpdateCAS :: PrimType
                -> VName -> VName
                -> [Imp.Exp] -> VName
                -> MulticoreGen ()
                -> MulticoreGen ()
atomicUpdateCAS t arr old bucket x do_op = do
  -- Code generation target:
  --
  -- old = d_his[idx];
  -- do {
  --   assumed = old;
  --   x = do_op(assumed, y);
  --   old = atomicCAS(&d_his[idx], assumed, tmp);
  -- } while(assumed != old);
  assumed <- dPrim "assumed" t
  run_loop <- dPrimV "run_loop" 0

  -- XXX: CUDA may generate really bad code if this is not a volatile
  -- read.  Unclear why.  The later reads are volatile, so maybe
  -- that's it.
  everythingVolatile $ copyDWIMFix old [] (Var arr) bucket

  (arr', _a_space, bucket_offset) <- fullyIndexArray arr bucket

  -- While-loop: Try to insert your value
  let (toBits, fromBits) =
        case t of FloatType Float32 -> (\v -> Imp.FunExp "to_bits32" [v] int32,
                                        \v -> Imp.FunExp "from_bits32" [v] t)
                  _                 -> (id, id)
  sWhile (Imp.var run_loop int32 .==. 0) $ do
    assumed <-- Imp.var old t
    x <-- Imp.var assumed t
    do_op
    old_bits <- dPrim "old_bits" int32
    sOp $ Imp.Atomic $
      Imp.AtomicCmpXchg int32 old_bits arr' bucket_offset
      (run_loop) (toBits (Imp.var x t))
    old <-- fromBits (Imp.var old_bits int32)






segmentedHist :: Pattern MCMem
              -> SegSpace
              -> [HistOp MCMem]
              -> KernelBody MCMem
              -> MulticoreGen Imp.Code
segmentedHist pat space histops kbody = do
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
      (_all_red_pes, map_pes) = splitAt num_red_res pes

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
  let sched = Imp.Static

  -- How many subtasks was used by scheduler
  num_histos <- dPrim "num_histos" $ IntType Int32
  emit $ Imp.Op $ Imp.MCFunc flat_idx prebody body free_params $
      Imp.MulticoreInfo num_histos sched (segFlat space)


  emit $ Imp.DebugPrint "nonsegmented hist stage 2"  Nothing
  forM_ (zip3 per_red_pes histograms histops) $ \(red_pes, (hists,_,_),  op) -> do
    bucket_id <- newVName "bucket_id"
    subhistogram_id <- newVName "subhistogram_id"

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
            SegSpace (segFlat space) $
            segment_dims ++
            [(bucket_id, num_buckets)] ++
            [(subhistogram_id, Var num_histos)]

      let segred_op = SegBinOp Noncommutative (histOp op) (histNeutral op) (histShape op)
          ns_red = map snd $ unSegSpace segred_space
      ns_red' <- mapM toExp ns_red

      let iterations = case unSegSpace segred_space of
                         [_] -> product ns_red'
                         _   -> product $ init ns_red' -- Segmented reduction is over the inner most dimension
      let retvals = map patElemName red_pes
      retvals_ts <- mapM lookupType retvals
      retval_params <- zipWithM toParam retvals retvals_ts
      let retval_names = map Imp.paramName retval_params

      dPrimV_ (segFlat segred_space) 0
      red_code <- compileSegRed' (Pattern [] red_pes) segred_space [segred_op] $ \red_cont ->
        red_cont $ flip map hists $ \subhisto ->
              (Var subhisto, map Imp.vi32 $
                map fst segment_dims ++ [subhistogram_id, bucket_id])

      free_params_red <- freeParams red_code (segFlat space : retval_names)
      emit $ Imp.Op $ Imp.Task free_params_red iterations red_code Nothing (segFlat space) retval_params


   where segment_dims = init $ unSegSpace space


prepareIntermediateArrays :: VName
                          -> [HistOp MCMem]
                          -> MulticoreGen [InitLocalHistograms]
prepareIntermediateArrays num_subhistos_per_group =
  mapM (onOp num_subhistos_per_group)

compileSegHist  :: Pattern MCMem
                -> SegSpace
                -> [HistOp MCMem]
                -> KernelBody MCMem
                -> MulticoreGen Imp.Code
compileSegHist pat space histops kbody
  | [_] <- unSegSpace space =
      nonsegmentedHist pat space histops kbody
  | otherwise =
      segmentedHist pat space histops kbody

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
      (_all_red_pes, map_pes) = splitAt num_red_res $ patternValueElements pat
      per_red_pes = segHistOpChunks histops $ patternValueElements pat

  idx' <- toExp $ Var idx
  collect $ do
    emit $ Imp.DebugPrint "Segmented segHist" Nothing
    let inner_bound = last ns'

    sFor "i" inner_bound $ \i -> do
      zipWithM_ dPrimV_ (init is) $ unflattenIndex (init ns') idx'
      dPrimV_ (last is) i

      compileStms mempty (kernelBodyStms kbody) $ do
        let (_red_res, map_res) = splitFromEnd (length map_pes) $
                                 map kernelResultSubExp $ kernelBodyResult kbody
            (buckets, vs) = splitAt (length histops) _red_res
            perOp = chunks $ map (length . histDest) histops

        forM_ (zip4 per_red_pes histops (perOp vs) buckets) $
           \(red_res, HistOp dest_w _ _ _ shape lam, vs', bucket) -> do

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
                 forM_ (zip red_res is_params) $ \(pe, p) ->
                   copyDWIMFix (paramName p) [] (Var $ patElemName pe) (map Imp.vi32 (init is)  ++ [buck] ++ is')
                 -- Value at index
                 forM_ (zip vs_params vs') $ \(p, v) ->
                   copyDWIMFix (paramName p) [] v is'
                 compileStms mempty (bodyStms $ lambdaBody lam) $
                   forM_ (zip red_res  $ bodyResult $ lambdaBody lam) $
                   \(pe, se) -> copyDWIMFix (patElemName pe) (map Imp.vi32 (init is) ++ [buck] ++ is') se []

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
