{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Our compilation strategy for 'SegGenRed' is based around avoiding
-- bin conflicts.  We do this by splitting the input into chunks, and
-- for each chunk computing a single subhistogram.  Then we combine
-- the subhistograms using an ordinary segmented reduction ('SegRed').
--
-- There are some branches around to efficiently handle the case where
-- we use only a single subhistogram (because it's large), so that we
-- respect the asymptotics, and do not copy the destination array.
module Futhark.CodeGen.ImpGen.Kernels.SegGenRed
  ( compileSegGenRed )
  where

import Control.Monad.Except
import Data.Maybe
import Data.List

import Prelude hiding (quot, rem)

import Futhark.MonadFreshNames
import Futhark.Representation.ExplicitMemory
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import Futhark.Pass.ExplicitAllocations()
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import qualified Futhark.CodeGen.ImpGen as ImpGen
import Futhark.CodeGen.ImpGen ((<--),
                               sFor, sComment, sIf, sWhen, sArray,
                               dPrim_, dPrimV)
import Futhark.CodeGen.ImpGen.Kernels.SegRed (compileSegRed')
import Futhark.CodeGen.ImpGen.Kernels.Base
import Futhark.Util.IntegralExp (quotRoundingUp, quot, rem)
import Futhark.Util (chunks, mapAccumLM, splitFromEnd, takeLast)
import Futhark.Construct (fullSliceNum)

prepareAtomicUpdate :: Maybe Locking -> [VName] -> Lambda InKernel
                    -> CallKernelGen (Maybe Locking,
                                      [Imp.Exp] -> ImpGen.ImpM InKernel Imp.KernelOp ())
prepareAtomicUpdate l dests lam =
  -- We need a separate lock array if the opterators are not all of a
  -- particularly simple form that permits pure atomic operations.
  case (l, atomicUpdateLocking lam) of
    (_, Left f) -> return (l, f dests)
    (Just l', Right f) -> return (l, f l' dests)
    (Nothing, Right f) -> do
      -- The number of locks used here is too low, but since we are
      -- currently forced to inline a huge list, I'm keeping it down
      -- for now.  Some quick experiments suggested that it has little
      -- impact anyway (maybe the locking case is just too slow).
      --
      -- A fun solution would also be to use a simple hashing
      -- algorithm to ensure good distribution of locks.
      let num_locks = 10000
      locks <-
        ImpGen.sStaticArray "genred_locks" (Space "device") int32 $
        Imp.ArrayZeros num_locks
      let l' = Locking locks 0 1 0 ((`rem` fromIntegral num_locks) . sum)
      return (Just l', f l' dests)

prepareIntermediateArrays :: [SubExp] -> Imp.Exp -> [GenReduceOp InKernel]
                          -> CallKernelGen
                             [(VName,
                               [VName],
                               [Imp.Exp] -> ImpGen.ImpM InKernel Imp.KernelOp ())]
prepareIntermediateArrays segment_dims num_threads = fmap snd . mapAccumLM onOp Nothing
  where
    onOp l op = do
      -- Determining the degree of cooperation (heuristic):
      -- coop_lvl   := size of histogram (Cooperation level)
      -- num_histos := (threads / coop_lvl) (Number of histograms)
      -- threads    := min(physical_threads, segment_size)
      --
      -- Careful to avoid division by zero when genReduceWidth==0.
      num_histos <- dPrimV "num_histos" $ num_threads `quotRoundingUp`
                    BinOpExp (SMax Int32) 1 (ImpGen.compileSubExpOfType int32 (genReduceWidth op))

      ImpGen.emit $ Imp.DebugPrint "num_histograms" int32 $ Imp.var num_histos int32

      -- Initialise sub-histograms.
      --
      -- If num_histos is 1, then we just reuse the original
      -- destination.  The idea is to avoid a copy if we are writing a
      -- small number of values into a very large prior histogram.

      dests <- forM (zip (genReduceDest op) (genReduceNeutral op)) $ \(dest, ne) -> do
        dest_t <- lookupType dest
        dest_mem <- ImpGen.entryArrayLocation <$> ImpGen.lookupArray dest
        let num_elems = foldl' (*) (Imp.var num_histos int32) $
                        map (ImpGen.compileSubExpOfType int32) $
                        arrayDims dest_t
        let size = Imp.elements num_elems `Imp.withElemType` int32

        (sub_mem, size') <-
          ImpGen.sDeclareMem "subhistogram_mem" size $ Space "device"

        let num_segments = length segment_dims
            sub_shape = Shape (segment_dims++[Var num_histos]) <>
                        stripDims num_segments (arrayShape dest_t)
            sub_membind = ArrayIn sub_mem $ IxFun.iota $
                          map (primExpFromSubExp int32) $ shapeDims sub_shape
        subhisto <- sArray "genred_dest" (elemType dest_t) sub_shape sub_membind

        let unitHistoCase =
              ImpGen.emit $
              Imp.SetMem sub_mem (ImpGen.memLocationName dest_mem) $
              Space "device"

            multiHistoCase = do
              ImpGen.sAlloc_ sub_mem size' $ Space "device"
              sReplicate subhisto (Shape $ segment_dims ++ [Var num_histos, genReduceWidth op]) ne
              subhisto_t <- lookupType subhisto
              let slice = fullSliceNum (map (ImpGen.compileSubExpOfType int32) $ arrayDims subhisto_t) $
                          map (unitSlice 0 . ImpGen.compileSubExpOfType int32) segment_dims ++
                          [DimFix 0]
              ImpGen.sUpdate subhisto slice $ Var dest

        sIf (Imp.var num_histos int32 .==. 1) unitHistoCase multiHistoCase

        return subhisto

      (l', do_op) <- prepareAtomicUpdate l dests $ genReduceOp op

      return (l', (num_histos, dests, do_op))

genRedKernel :: [PatElem ExplicitMemory]
             -> KernelSpace
             -> [GenReduceOp InKernel]
             -> Body InKernel
             -> CallKernelGen [(VName, [VName])]
genRedKernel map_pes space ops body = do
  (base_constants, init_constants) <- kernelInitialisationSetSpace space $ return ()
  let constants = base_constants { kernelThreadActive = true }
      (space_is, space_sizes) = unzip $ spaceDimensions space
      i32_to_i64 = ConvOpExp (SExt Int32 Int64)
      space_sizes_64 = map (i32_to_i64 . ImpGen.compileSubExpOfType int32) space_sizes
      total_w_64 = product space_sizes_64

  histograms <- prepareIntermediateArrays (init space_sizes) (kernelNumThreads constants) ops

  elems_per_thread_64 <- dPrimV "elems_per_thread_64" $
                         total_w_64 `quotRoundingUp`
                         ConvOpExp (SExt Int32 Int64) (kernelNumThreads constants)

  sKernel constants "seggenred" $ allThreads constants $ do
    init_constants

    i <- newVName "i"

    -- Compute subhistogram index for each thread, per histogram.
    subhisto_inds <- forM histograms $ \(num_histograms, _, _) ->
      dPrimV "subhisto_ind" $
      kernelGlobalThreadId constants `quot`
      (kernelNumThreads constants `quotRoundingUp` Imp.var num_histograms int32)

    sFor i Int64 (Imp.var elems_per_thread_64 int64) $ do
      -- Compute the offset into the input and output.  To this a
      -- thread can add its local ID to figure out which element it is
      -- responsible for.  The calculation is done with 64-bit
      -- integers to avoid overflow, but the final segment indexes are
      -- 32 bit.
      offset <- dPrimV "offset" $
                (i32_to_i64 (kernelGroupId constants) *
                 (Imp.var elems_per_thread_64 int64 *
                  i32_to_i64 (kernelGroupSize constants)))
                + (Imp.var i int64 * i32_to_i64 (kernelGroupSize constants))

      j <- dPrimV "j" $ Imp.var offset int64 + i32_to_i64 (kernelLocalThreadId constants)

      -- Construct segment indices.
      let setIndex v e = do dPrim_ v int32
                            v <-- e
      zipWithM_ setIndex space_is $
        map (ConvOpExp (SExt Int64 Int32)) . unflattenIndex space_sizes_64 $ Imp.var j int64

      -- We execute the bucket function once and update each histogram serially.
      -- We apply the bucket function if j=offset+ltid is less than
      -- num_elements.  This also involves writing to the mapout
      -- arrays.
      let input_in_bounds = Imp.var j int32 .<. total_w_64

      sWhen input_in_bounds $ ImpGen.compileStms mempty (stmsToList $ bodyStms body) $ do
        let (red_res, map_res) = splitFromEnd (length map_pes) $ bodyResult body

        sComment "save map-out results" $
          forM_ (zip map_pes map_res) $ \(pe, se) ->
          ImpGen.copyDWIM (patElemName pe)
          (map ((`Imp.var` int32) . fst) $ kernelDimensions constants) se []

        let (buckets, vs) = splitAt (length ops) red_res
            perOp = chunks $ map (length . genReduceDest) ops

        sComment "perform atomic updates" $
          forM_ (zip5 ops histograms buckets (perOp vs) subhisto_inds) $
          \(GenReduceOp dest_w _ _ shape lam,
            (_, _, do_op), bucket, vs', subhisto_ind) -> do

            let bucket' = ImpGen.compileSubExpOfType int32 bucket
                dest_w' = ImpGen.compileSubExpOfType int32 dest_w
                bucket_in_bounds = 0 .<=. bucket' .&&. bucket' .<. dest_w'
                bucket_is = map (`Imp.var` int32) (init space_is) ++
                            [Imp.var subhisto_ind int32, bucket']
                vs_params = takeLast (length vs') $ lambdaParams lam

            sWhen bucket_in_bounds $ do
              ImpGen.dLParams $ lambdaParams lam
              vectorLoops [] (shapeDims shape) $ \is -> do
                forM_ (zip vs_params vs') $ \(p, v) ->
                  ImpGen.copyDWIM (paramName p) [] v is
                do_op (bucket_is ++ is)

  let histogramInfo (num_histos, dests, _) = (num_histos, dests)
  return $ map histogramInfo histograms

vectorLoops :: [Imp.Exp] -> [SubExp]
            -> ([Imp.Exp] -> ImpGen.ImpM lore op ())
            -> ImpGen.ImpM lore op ()
vectorLoops is [] f = f $ reverse is
vectorLoops is (d:ds) f = do
  i <- newVName "i"
  d' <- ImpGen.compileSubExp d
  ImpGen.sFor i Int32 d' $ vectorLoops (Imp.var i int32:is) ds f

compileSegGenRed :: Pattern ExplicitMemory
                 -> KernelSpace
                 -> [GenReduceOp InKernel]
                 -> Body InKernel
                 -> CallKernelGen ()
compileSegGenRed (Pattern _ pes) genred_space ops body = do
  let num_red_res = length ops + sum (map (length . genReduceNeutral) ops)
      (all_red_pes, map_pes) = splitAt num_red_res pes

  infos <- genRedKernel map_pes genred_space ops body
  let pes_per_op = chunks (map (length . genReduceDest) ops) all_red_pes

  forM_ (zip3 infos pes_per_op ops) $ \((num_histos, subhistos), red_pes, op) -> do
    let unitHistoCase =
          -- This is OK because the memory blocks are at least as
          -- large as the ones we are supposed to use for the result.
          forM_ (zip red_pes subhistos) $ \(pe, subhisto) -> do
            pe_mem <- ImpGen.memLocationName . ImpGen.entryArrayLocation <$>
                      ImpGen.lookupArray (patElemName pe)
            subhisto_mem <- ImpGen.memLocationName . ImpGen.entryArrayLocation <$>
                            ImpGen.lookupArray subhisto
            ImpGen.emit $ Imp.SetMem pe_mem subhisto_mem $ Space "device"

    sIf (Imp.var num_histos int32 .==. 1) unitHistoCase $ do
      -- For the segmented reduction, we keep the segment dimensions
      -- unchanged.  To this, we add two dimensions: one over the number
      -- of buckets, and one over the number of subhistograms.  This
      -- inner dimension is the one that is collapsed in the reduction.
      let segment_dims = init $ spaceDimensions genred_space
          num_buckets = genReduceWidth op

      bucket_id <- newVName "bucket_id"
      subhistogram_id <- newVName "subhistogram_id"
      vector_ids <- mapM (const $ newVName "vector_id") $
                    shapeDims $ genReduceShape op
      gtid <- newVName $ baseString $ spaceGlobalId genred_space
      let lam = genReduceOp op
          segred_space =
            genred_space
            { spaceStructure =
                FlatThreadSpace $
                segment_dims ++
                [(bucket_id, num_buckets)] ++
                zip vector_ids (shapeDims $ genReduceShape op) ++
                [(subhistogram_id, Var num_histos)]
            , spaceGlobalId = gtid
            }

      compileSegRed' (Pattern [] red_pes) segred_space
        Commutative lam (genReduceNeutral op) $ \red_dests _ ->
        forM_ (zip red_dests subhistos) $ \((d, is), subhisto) ->
          ImpGen.copyDWIM d is (Var subhisto) $ map (`Imp.var` int32) $
          map fst segment_dims ++ [subhistogram_id, bucket_id] ++ vector_ids
