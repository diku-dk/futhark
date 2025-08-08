module Futhark.CodeGen.ImpGen.Multicore.SegHist
  ( compileSegHist,
  )
where

import Control.Monad
import Data.List (zip4)
import Data.Maybe (listToMaybe)
import Futhark.CodeGen.ImpCode.Multicore qualified as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.CodeGen.ImpGen.Multicore.SegRed (compileSegRed')
import Futhark.IR.MCMem
import Futhark.Transform.Rename (renameLambda)
import Futhark.Util (chunks, splitFromEnd, takeLast)
import Futhark.Util.IntegralExp (rem)
import Prelude hiding (quot, rem)

compileSegHist ::
  Pat LetDecMem ->
  SegSpace ->
  [HistOp MCMem] ->
  KernelBody MCMem ->
  TV Int32 ->
  MulticoreGen Imp.MCCode
compileSegHist pat space histops kbody nsubtasks
  | [_] <- unSegSpace space =
      nonsegmentedHist pat space histops kbody nsubtasks
  | otherwise =
      segmentedHist pat space histops kbody

-- | Split some list into chunks equal to the number of values
-- returned by each 'SegBinOp'
segHistOpChunks :: [HistOp rep] -> [a] -> [[a]]
segHistOpChunks = chunks . map (length . histNeutral)

histSize :: HistOp MCMem -> Imp.TExp Int64
histSize = product . map pe64 . shapeDims . histShape

genHistOpParams :: HistOp MCMem -> MulticoreGen ()
genHistOpParams histops =
  dScope Nothing $ scopeOfLParams $ lambdaParams $ histOp histops

renameHistop :: HistOp MCMem -> MulticoreGen (HistOp MCMem)
renameHistop histop = do
  let op = histOp histop
  lambda' <- renameLambda op
  pure histop {histOp = lambda'}

nonsegmentedHist ::
  Pat LetDecMem ->
  SegSpace ->
  [HistOp MCMem] ->
  KernelBody MCMem ->
  TV Int32 ->
  MulticoreGen Imp.MCCode
nonsegmentedHist pat space histops kbody num_histos = do
  let ns = map snd $ unSegSpace space
      ns_64 = map pe64 ns
      num_histos' = tvExp num_histos
      hist_width = maybe 0 histSize $ listToMaybe histops
      use_subhistogram = sExt64 num_histos' * hist_width .<=. product ns_64

  histops' <- renameHistOpLambda histops

  -- Only do something if there is actually input.
  collect $
    sUnless (product ns_64 .==. 0) $ do
      sIf
        use_subhistogram
        (subHistogram pat space histops num_histos kbody)
        (atomicHistogram pat space histops' kbody)

-- |
-- Atomic Histogram approach
-- The implementation has three sub-strategies depending on the
-- type of the operator
-- 1. If values are integral scalars, a direct-supported atomic update is used.
-- 2. If values are on one memory location, e.g. a float, then a
-- CAS operation is used to perform the update, where the float is
-- casted to an integral scalar.
-- 1. and 2. currently only works for 32-bit and 64-bit types,
-- but GCC has support for 8-, 16- and 128- bit types as well.
-- 3. Otherwise a locking based approach is used
onOpAtomic :: HistOp MCMem -> MulticoreGen ([VName] -> [Imp.TExp Int64] -> MulticoreGen ())
onOpAtomic op = do
  atomics <- hostAtomics <$> askEnv
  let lambda = histOp op
      do_op = atomicUpdateLocking atomics lambda
  case do_op of
    AtomicPrim f -> pure f
    AtomicCAS f -> pure f
    AtomicLocking f -> do
      -- Allocate a static array of locks
      -- as in the GPU backend
      let num_locks = 100151 -- This number is taken from the GPU backend
          dims = map pe64 $ shapeDims (histOpShape op <> histShape op)
      locks <-
        sStaticArray "hist_locks" int32 $
          Imp.ArrayZeros num_locks
      let l' = Locking locks 0 1 0 (pure . (`rem` fromIntegral num_locks) . flattenIndex dims)
      pure $ f l'

atomicHistogram ::
  Pat LetDecMem ->
  SegSpace ->
  [HistOp MCMem] ->
  KernelBody MCMem ->
  MulticoreGen ()
atomicHistogram pat space histops kbody = do
  let (is, ns) = unzip $ unSegSpace space
      ns_64 = map pe64 ns
  let num_red_res = length histops + sum (map (length . histNeutral) histops)
      (all_red_pes, map_pes) = splitAt num_red_res $ patElems pat

  atomicOps <- mapM onOpAtomic histops

  body <- collect $ do
    dPrim_ (segFlat space) int64
    sOp $ Imp.GetTaskId (segFlat space)
    generateChunkLoop "SegHist" Scalar $ \flat_idx -> do
      zipWithM_ dPrimV_ is $ unflattenIndex ns_64 flat_idx
      compileStms mempty (bodyStms kbody) $ do
        let (red_res, map_res) =
              splitFromEnd (length map_pes) $ bodyResult kbody
            red_res_split = splitHistResults histops $ map kernelResultSubExp red_res

        let pes_per_op = chunks (map (length . histDest) histops) all_red_pes
        forM_ (zip4 histops red_res_split atomicOps pes_per_op) $
          \(HistOp dest_shape _ _ _ shape lam, (bucket, vs'), do_op, dest_res) -> do
            let (_is_params, vs_params) = splitAt (length vs') $ lambdaParams lam
                dest_shape' = map pe64 $ shapeDims dest_shape
                bucket' = map pe64 bucket
                bucket_in_bounds = inBounds (Slice (map DimFix bucket')) dest_shape'

            sComment "save map-out results" $
              forM_ (zip map_pes map_res) $ \(pe, res) ->
                copyDWIMFix (patElemName pe) (map Imp.le64 is) (kernelResultSubExp res) []

            sComment "perform updates" $
              sWhen bucket_in_bounds $ do
                let bucket_is = map Imp.le64 (init is) ++ bucket'
                dLParams $ lambdaParams lam
                sLoopNest shape $ \is' -> do
                  forM_ (zip vs_params vs') $ \(p, res) ->
                    copyDWIMFix (paramName p) [] res is'
                  do_op (map patElemName dest_res) (bucket_is ++ is')

  free_params <- freeParams body
  emit $ Imp.Op $ Imp.ParLoop "atomic_seg_hist" body free_params

updateHisto ::
  HistOp MCMem ->
  [VName] ->
  [Imp.TExp Int64] ->
  Imp.TExp Int64 ->
  [Param LParamMem] ->
  MulticoreGen ()
updateHisto op arrs bucket j uni_acc = do
  let bind_acc_params =
        forM_ (zip uni_acc arrs) $ \(acc_u, arr) -> do
          copyDWIMFix (paramName acc_u) [] (Var arr) bucket

      op_body = compileBody' [] $ lambdaBody $ histOp op
      writeArray arr val = extractVectorLane j $ collect $ copyDWIMFix arr bucket val []
      do_hist = zipWithM_ writeArray arrs $ map resSubExp $ bodyResult $ lambdaBody $ histOp op

  sComment "Start of body" $ do
    bind_acc_params
    op_body
    do_hist

-- Generates num_histos sub-histograms of the size
-- of the destination histogram
-- Then for each chunk of the input each subhistogram
-- is computed and finally combined through a segmented reduction
-- across the histogram indicies.
-- This is expected to be fast if len(histDest) is small
subHistogram ::
  Pat LetDecMem ->
  SegSpace ->
  [HistOp MCMem] ->
  TV Int32 ->
  KernelBody MCMem ->
  MulticoreGen ()
subHistogram pat space histops num_histos kbody = do
  emit $ Imp.DebugPrint "subHistogram segHist" Nothing

  let (is, ns) = unzip $ unSegSpace space
      ns_64 = map pe64 ns

  let pes = patElems pat
      num_red_res = length histops + sum (map (length . histNeutral) histops)
      map_pes = drop num_red_res pes
      per_red_pes = segHistOpChunks histops $ patElems pat

  -- Allocate array of subhistograms in the calling thread.  Each
  -- tasks will work in its own private allocations (to avoid false
  -- sharing), but this is where they will ultimately copy their
  -- results.
  global_subhistograms <- forM histops $ \histop ->
    forM (histType histop) $ \t -> do
      let shape = Shape [tvSize num_histos] <> arrayShape t
      sAllocArray "subhistogram" (elemType t) shape DefaultSpace

  let tid' = Imp.le64 $ segFlat space

  -- Generate loop body of parallel function
  body <- collect $ do
    dPrim_ (segFlat space) int64
    sOp $ Imp.GetTaskId (segFlat space)

    local_subhistograms <- forM (zip per_red_pes histops) $ \(pes', histop) -> do
      op_local_subhistograms <- forM (histType histop) $ \t ->
        sAllocArray "subhistogram" (elemType t) (arrayShape t) DefaultSpace

      forM_ (zip3 pes' op_local_subhistograms (histNeutral histop)) $ \(pe, hist, ne) ->
        -- First thread initializes histogram with dest vals. Others
        -- initialize with neutral element
        sIf
          (tid' .==. 0)
          (copyDWIMFix hist [] (Var $ patElemName pe) [])
          ( sLoopNest (histShape histop) $ \shape_is ->
              sLoopNest (histOpShape histop) $ \vec_is ->
                copyDWIMFix hist (shape_is <> vec_is) ne []
          )

      pure op_local_subhistograms

    inISPC $
      generateChunkLoop "SegRed" Vectorized $ \i -> do
        zipWithM_ dPrimV_ is $ unflattenIndex ns_64 i
        compileStms mempty (bodyStms kbody) $ do
          let (red_res, map_res) =
                splitFromEnd (length map_pes) $
                  map kernelResultSubExp $
                    bodyResult kbody

          sComment "save map-out results" $
            forM_ (zip map_pes map_res) $ \(pe, res) ->
              copyDWIMFix (patElemName pe) (map Imp.le64 is) res []

          forM_ (zip3 histops local_subhistograms (splitHistResults histops red_res)) $
            \( histop@(HistOp dest_shape _ _ _ shape _),
               histop_subhistograms,
               (bucket, vs')
               ) -> do
                histop' <- renameHistop histop

                let bucket' = map pe64 bucket
                    dest_shape' = map pe64 $ shapeDims dest_shape
                    acc_params' = (lambdaParams . histOp) histop'
                    vs_params' = takeLast (length vs') $ lambdaParams $ histOp histop'

                generateUniformizeLoop $ \j ->
                  sComment "perform updates" $ do
                    -- Create new set of uniform buckets
                    -- That is extract each bucket from a SIMD vector lane
                    extract_buckets <- mapM (dPrimSV "extract_bucket" . (primExpType . untyped)) bucket'
                    forM_ (zip extract_buckets bucket') $ \(x, y) ->
                      emit $ Imp.Op $ Imp.ExtractLane (tvVar x) (untyped y) (untyped j)
                    let bucket'' = map tvExp extract_buckets
                        bucket_in_bounds =
                          inBounds (Slice (map DimFix bucket'')) dest_shape'
                    sWhen bucket_in_bounds $ do
                      genHistOpParams histop'
                      sLoopNest shape $ \is' -> do
                        -- read values vs and perform lambda writing result back to is
                        forM_ (zip vs_params' vs') $ \(p, res) ->
                          ifPrimType (paramType p) $ \pt -> do
                            -- Hack to copy varying load into uniform result variable
                            tmp <- dPrimS "tmp" pt
                            copyDWIMFix tmp [] res is'
                            extractVectorLane j . pure $
                              Imp.SetScalar (paramName p) (toExp' pt tmp)
                        updateHisto histop' histop_subhistograms (bucket'' ++ is') j acc_params'

    -- Copy the task-local subhistograms to the global subhistograms,
    -- where they will be combined.
    forM_ (zip (concat global_subhistograms) (concat local_subhistograms)) $
      \(global, local) -> copyDWIMFix global [tid'] (Var local) []

  free_params <- freeParams body
  emit $ Imp.Op $ Imp.ParLoop "seghist_stage_1" body free_params

  -- Perform a segmented reduction over the subhistograms
  forM_ (zip3 per_red_pes global_subhistograms histops) $ \(red_pes, hists, op) -> do
    bucket_ids <-
      replicateM (shapeRank (histShape op)) (newVName "bucket_id")
    subhistogram_id <- newVName "subhistogram_id"

    let segred_space =
          SegSpace (segFlat space) $
            segment_dims
              ++ zip bucket_ids (shapeDims (histShape op))
              ++ [(subhistogram_id, tvSize num_histos)]

        segred_op = SegBinOp Noncommutative (histOp op) (histNeutral op) (histOpShape op)

    red_code <- collect $ do
      nsubtasks <- dPrim "nsubtasks"
      sOp $ Imp.GetNumTasks $ tvVar nsubtasks
      emit <=< compileSegRed' (Pat red_pes) segred_space [segred_op] nsubtasks $ \red_cont ->
        red_cont $
          segBinOpChunks [segred_op] $
            flip map hists $ \subhisto ->
              ( Var subhisto,
                map Imp.le64 $
                  map fst segment_dims ++ [subhistogram_id] ++ bucket_ids
              )

    let ns_red = map (pe64 . snd) $ unSegSpace segred_space
        iterations = product $ init ns_red -- The segmented reduction is sequential over the inner most dimension
        scheduler_info = Imp.SchedulerInfo (untyped iterations) Imp.Static
        red_task = Imp.ParallelTask red_code
    free_params_red <- freeParams red_code
    emit $ Imp.Op $ Imp.SegOp "seghist_red" free_params_red red_task Nothing mempty scheduler_info
  where
    segment_dims = init $ unSegSpace space
    ifPrimType (Prim pt) f = f pt
    ifPrimType _ _ = pure ()

-- Note: This isn't currently used anywhere.
-- This implementation for a Segmented Hist only
-- parallelize over the segments,
-- where each segment is updated sequentially.
segmentedHist ::
  Pat LetDecMem ->
  SegSpace ->
  [HistOp MCMem] ->
  KernelBody MCMem ->
  MulticoreGen Imp.MCCode
segmentedHist pat space histops kbody = do
  emit $ Imp.DebugPrint "Segmented segHist" Nothing
  collect $ do
    body <- compileSegHistBody pat space histops kbody
    free_params <- freeParams body
    emit $ Imp.Op $ Imp.ParLoop "segmented_hist" body free_params

compileSegHistBody ::
  Pat LetDecMem ->
  SegSpace ->
  [HistOp MCMem] ->
  KernelBody MCMem ->
  MulticoreGen Imp.MCCode
compileSegHistBody pat space histops kbody = collect $ do
  let (is, ns) = unzip $ unSegSpace space
      ns_64 = map pe64 ns

  let num_red_res = length histops + sum (map (length . histNeutral) histops)
      map_pes = drop num_red_res $ patElems pat
      per_red_pes = segHistOpChunks histops $ patElems pat

  dPrim_ (segFlat space) int64
  sOp $ Imp.GetTaskId (segFlat space)

  generateChunkLoop "SegHist" Scalar $ \idx -> do
    let inner_bound = last ns_64
    sFor "i" inner_bound $ \i -> do
      zipWithM_ dPrimV_ (init is) $ unflattenIndex (init ns_64) idx
      dPrimV_ (last is) i

      compileStms mempty (bodyStms kbody) $ do
        let (red_res, map_res) =
              splitFromEnd (length map_pes) $
                map kernelResultSubExp $
                  bodyResult kbody
        forM_ (zip3 per_red_pes histops (splitHistResults histops red_res)) $
          \(red_pes, HistOp dest_shape _ _ _ shape lam, (bucket, vs')) -> do
            let (is_params, vs_params) = splitAt (length vs') $ lambdaParams lam
                bucket' = map pe64 bucket
                dest_shape' = map pe64 $ shapeDims dest_shape
                bucket_in_bounds = inBounds (Slice (map DimFix bucket')) dest_shape'

            sComment "save map-out results" $
              forM_ (zip map_pes map_res) $ \(pe, res) ->
                copyDWIMFix (patElemName pe) (map Imp.le64 is) res []

            sComment "perform updates" $
              sWhen bucket_in_bounds $ do
                dLParams $ lambdaParams lam
                sLoopNest shape $ \vec_is -> do
                  -- Index
                  forM_ (zip red_pes is_params) $ \(pe, p) ->
                    copyDWIMFix
                      (paramName p)
                      []
                      (Var $ patElemName pe)
                      (map Imp.le64 (init is) ++ bucket' ++ vec_is)
                  -- Value at index
                  forM_ (zip vs_params vs') $ \(p, v) ->
                    copyDWIMFix (paramName p) [] v vec_is
                  compileStms mempty (bodyStms $ lambdaBody lam) $
                    forM_ (zip red_pes $ map resSubExp $ bodyResult $ lambdaBody lam) $
                      \(pe, se) ->
                        copyDWIMFix
                          (patElemName pe)
                          (map Imp.le64 (init is) ++ bucket' ++ vec_is)
                          se
                          []
