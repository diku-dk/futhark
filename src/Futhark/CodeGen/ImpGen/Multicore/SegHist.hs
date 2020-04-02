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

prepareIntermediateArraysLocal :: VName
                               -> [MySegHistSlug]
                               -> MulticoreGen InitLocalHistograms
prepareIntermediateArraysLocal num_subhistos_per_group =
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
compileSegHist pat space histops kbody = do
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
    prepareIntermediateArraysLocal hist_M slugs

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

  prebody <- collect $
    forM_ (zip3 histograms hist_H_chks slugs) $ \((hists, _, _), hist_H_chk, slug) ->
      forM_ (zip3 (patternElements pat) hists (histNeutral $ mySlugOp slug)) $ \(pe, hist, ne) -> do
        hist_H_chk' <- toExp $ Var hist_H_chk
        let is_first_tid = tid_exp .==. 0
            set_dest_vals = sFor "i" hist_H_chk' $ \i ->
                             copyDWIMFix hist (tid_exp :[i]) (Var $ patElemName pe) [i]
            set_neutral = sFor "i" hist_H_chk' $ \i ->
                             copyDWIMFix hist (tid_exp :[i]) ne []
        sIf is_first_tid set_dest_vals set_neutral

  -- Generate body of parallel function
  body' <- collect $ do
     zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
     compileStms mempty (kernelBodyStms kbody) $ do
       let (red_res, map_res) = splitFromEnd (length map_pes) $
                            map kernelResultSubExp $ kernelBodyResult kbody


           (buckets, vs) = splitAt (length slugs) red_res
           perOp = chunks $ map (length . histDest . mySlugOp) slugs

       sComment "save map-out results" $
          forM_ (zip map_pes map_res) $ \(pe, se) ->
          copyDWIMFix (patElemName pe)
          (map Imp.vi32 is) se []

       forM_ (zip4 (map mySlugOp slugs) histograms buckets (perOp vs)) $
         \(HistOp dest_w _ _ _ shape lam,
           (_, _hist_H_chk, do_op), bucket, vs') -> do

           let bucket' = toExp' int32 bucket
               dest_w' = toExp' int32 dest_w
               bucket_in_bounds = bucket' .<. dest_w' .&&. 0 .<=. bucket'
               vs_params = takeLast (length vs') $ lambdaParams lam
               bucket_is = [tid_exp, bucket']

           sComment "perform updates" $
             sWhen bucket_in_bounds $ do
             dLParams $ lambdaParams lam
             sLoopNest shape $ \is' -> do
               forM_ (zip vs_params vs') $ \(p, v) ->
                 copyDWIMFix (paramName p) [] v is'
               do_op (bucket_is ++ is')


  let paramsNames = namesToList $ freeIn (prebody <> body') `namesSubtract`
                                  namesFromList (thread_id : [segFlat space])
  ts <- mapM lookupType paramsNames
  let params = zipWith toParam paramsNames ts

  -- How many subtasks was generated by scheduler
  num_histos <- dPrim "num_histos" $ IntType Int32


  emit $ Imp.Op $ Imp.ParLoop num_histos (segFlat space) (product ns')
                              (Imp.MulticoreFunc params prebody body' thread_id)


  let pes_per_op = chunks (map (length . histDest) histops) all_red_pes

  forM_ (zip3 pes_per_op histograms histops) $ \(red_pes, (hists,_,_),  op) -> do
    bucket_id <- newVName "bucket_id"
    subhistogram_id <- newVName "subhistogram_id"
    vector_ids <- mapM (const $ newVName "vector_id") $ shapeDims $ histShape op

    flat_gtid <- newVName "flat_gtid"

    let num_buckets = histWidth op

    let segred_space =
          SegSpace flat_gtid $
          segment_dims ++
          [(bucket_id, num_buckets)] ++
          zip vector_ids (shapeDims $ histShape op) ++
          [(subhistogram_id, Var num_histos)]

    let segred_op = SegRedOp Commutative (histOp op) (histNeutral op) mempty
    compileSegRed' (Pattern [] red_pes) segred_space [segred_op] $ \red_cont -> do
      let
      red_cont $ flip map hists $ \subhisto ->
            (Var subhisto, map Imp.vi32 $
              map fst segment_dims ++ [subhistogram_id, bucket_id] ++ vector_ids)
   where segment_dims = init $ unSegSpace space


    -- compileSegRed pat lvl space reds kbody

  -- -- second stage
  -- reduce_body <- collect $ do
  --   zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
  --   let (red_res, _map_res) = splitFromEnd (length map_pes) $
  --                         map kernelResultSubExp $ kernelBodyResult kbody
  --       (buckets, vs) = splitAt (length slugs) red_res
  --       perOp = chunks $ map (length . histDest . mySlugOp) slugs

  --   num_tasks' <- toExp $ Var num_tasks
  --   sFor "i" num_tasks' $ \i ->
  --     forM_ (zip4 (map mySlugOp slugs) histograms buckets (perOp vs)) $
  --         \(HistOp _dest_w _ _ _ shape lam,
  --          (hist, _hist_H_chk, _do_op), _bucket, vs') -> do

  --          -- let bucket' = toExp' int32 bucket
  --              -- dest_w' = toExp' int32 dest_w
  --              -- bucket_in_bounds = bucket' .<. dest_w' .&&. 0 .<=. bucket'
  --              -- Use splitAt
  --          let vs_params = takeLast (length vs') $ lambdaParams lam
  --              is_params = take (length vs') $ lambdaParams lam
  --              bucket_is = i : map Imp.vi32 is

  --          sComment "perform updates" $ do
  --            dLParams $ lambdaParams lam
  --            sLoopNest shape $ \is' -> do
  --              forM_ (zip vs_params hist) $ \(p, v) ->
  --                copyDWIMFix (paramName p) [] (Var v) (bucket_is ++ is')
  --              forM_ (zip (patternElements pat) is_params) $ \(pe, p) ->
  --                copyDWIMFix (paramName p) [] (Var $ patElemName pe) (map Imp.vi32 is)
  --              compileStms mempty (bodyStms $ lambdaBody lam) $
  --                forM_ (zip (patternElements pat) $ bodyResult $ lambdaBody lam) $
  --                \(pe, se) ->
  --                  copyDWIMFix (patElemName pe) (map Imp.vi32 is) se []

  -- let paramsNames' = namesToList $ freeIn reduce_body `namesSubtract` namesFromList [segFlat space]
  -- ts' <- mapM lookupType paramsNames'
  -- let params' = zipWith toParam paramsNames' ts'

  -- thread_id2 <- dPrim "thread_id" $ IntType Int32
  -- num_elem <- mapM (toExp . Var) hist_H_chks

  -- -- How many subtasks was generated by scheduler
  -- num_tasks2 <- dPrim "num_tasks" $ IntType Int32


  -- emit $ Imp.Op $ Imp.ParLoop num_tasks2 (segFlat space) (head num_elem)
  --                             (Imp.MulticoreFunc params' mempty reduce_body thread_id2)




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
