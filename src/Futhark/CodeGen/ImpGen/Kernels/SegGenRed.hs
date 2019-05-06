{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Our compilation strategy for 'SegGenRed' is based around avoiding
-- bin conflicts.  We do this by splitting the input into chunks, and
-- for each chunk computing a single subhistogram.  Then we combine
-- the subhistograms using an ordinary segmented reduction ('SegRed').
--
-- There are some branches around to efficiently handle the case where
-- we use only a single subhistogram (because it's large), so that we
-- respect the asymptotics, and do not copy the destination array.
--
-- We also use a heuristic strategy for computing subhistograms in
-- local memory when possible.  Given:
--
-- H: total size of histograms in bytes, including any lock arrays.
--
-- G: group size
--
-- T: number of bytes of local memory each thread can be given without
-- impacting occupancy (determined experimentally, e.g. 32).
--
-- LMAX: maximum amount of local memory per workgroup (hard limit).
--
-- We wish to compute:
--
-- COOP: cooperation level (number of threads per subhistogram)
--
-- LH: number of local memory subhistograms
--
-- We do this as:
--
-- COOP = ceil(H / T)
-- LH = ceil((G*T)/H)
-- if COOP <= G && H <= LMAX then
--   use local memory
-- else
--   use global memory

module Futhark.CodeGen.ImpGen.Kernels.SegGenRed
  ( compileSegGenRed )
  where

import Control.Monad.Except
import Data.Either
import Data.Maybe
import Data.List

import Prelude hiding (quot, rem)

import Futhark.MonadFreshNames
import Futhark.Representation.ExplicitMemory
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import Futhark.Pass.ExplicitAllocations()
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Kernels.SegRed (compileSegRed')
import Futhark.CodeGen.ImpGen.Kernels.Base
import Futhark.Util.IntegralExp (quotRoundingUp, quot, rem)
import Futhark.Util (chunks, mapAccumLM, splitFromEnd, takeLast)
import Futhark.Construct (fullSliceNum)

vectorLoops :: [Imp.Exp] -> [SubExp]
            -> ([Imp.Exp] -> ImpM lore op ())
            -> ImpM lore op ()
vectorLoops is [] f = f $ reverse is
vectorLoops is (d:ds) f = do
  i <- newVName "vect_i"
  d' <- toExp d
  sFor i Int32 d' $ vectorLoops (Imp.var i int32:is) ds f

i32Toi64 :: PrimExp v -> PrimExp v
i32Toi64 = ConvOpExp (SExt Int32 Int64)

data SubhistosInfo = SubhistosInfo { subhistosArray :: VName
                                   , subhistosAlloc :: CallKernelGen ()
                                   }

data SegGenRedSlug = SegGenRedSlug
                     { slugOp :: GenReduceOp InKernel
                     , slugNumSubhistos :: VName
                     , slugSubhistos :: [SubhistosInfo]
                     }
-- | Figure out how much memory is needed per histogram, and compute
-- some other auxiliary information.
computeHistoUsage :: KernelSpace
                  -> GenReduceOp InKernel
                  -> CallKernelGen (Imp.Count Imp.Bytes, SegGenRedSlug)
computeHistoUsage space op = do
  let segment_dims = init $ spaceDimensions space
      num_segments = length segment_dims

  op_h <- fmap (sum . map typeSize) $ mapM lookupType $ genReduceDest op

  -- Create names for the intermediate array memory blocks,
  -- memory block sizes, arrays, and number of subhistograms.
  num_subhistos <- dPrim "num_subhistos" int32
  subhisto_infos <- forM (zip (genReduceDest op) (genReduceNeutral op)) $ \(dest, ne) -> do
    dest_t <- lookupType dest
    dest_mem <- entryArrayLocation <$> lookupArray dest

    subhistos_mem <-
      sDeclareMem (baseString dest ++ "_subhistos_mem") (Space "device")

    let subhistos_shape = Shape (map snd segment_dims++[Var num_subhistos]) <>
                          stripDims num_segments (arrayShape dest_t)
        subhistos_membind = ArrayIn subhistos_mem $ IxFun.iota $
                            map (primExpFromSubExp int32) $ shapeDims subhistos_shape
    subhistos <- sArray (baseString dest ++ "_subhistos")
                 (elemType dest_t) subhistos_shape subhistos_membind

    return $ SubhistosInfo subhistos $ do
      let unitHistoCase =
            emit $
            Imp.SetMem subhistos_mem (memLocationName dest_mem) $
            Space "device"

          multiHistoCase = do
            let num_elems = foldl' (*) (Imp.var num_subhistos int32) $
                            map (toExp' int32) $
                            arrayDims dest_t

            let subhistos_mem_size =
                  Imp.bytes $
                  Imp.innerExp (Imp.elements num_elems `Imp.withElemType` int32)

            sAlloc_ subhistos_mem subhistos_mem_size $ Space "device"
            sReplicate subhistos (Shape (map snd segment_dims ++
                                         [Var num_subhistos, genReduceWidth op]) <>
                                  genReduceShape op) ne
            subhistos_t <- lookupType subhistos
            let slice = fullSliceNum (map (toExp' int32) $ arrayDims subhistos_t) $
                        map (unitSlice 0 . toExp' int32 . snd) segment_dims ++
                        [DimFix 0]
            sUpdate subhistos slice $ Var dest

      sIf (Imp.var num_subhistos int32 .==. 1) unitHistoCase multiHistoCase

  return (op_h, SegGenRedSlug op num_subhistos subhisto_infos)

localMemLockArray :: KernelSpace -> Type
localMemLockArray space = Array int32 (Shape [spaceGroupSize space]) NoUniqueness

-- | How many bytes will be spent on lock arrays if we use a local
-- memory implementation?
localMemLockUsage :: KernelSpace -> [SegGenRedSlug] -> Imp.Count Imp.Bytes
localMemLockUsage space slugs =
  if any (isRight . atomicUpdateLocking . genReduceOp . slugOp) slugs
  then typeSize $ localMemLockArray space
  else 0

prepareAtomicUpdateGlobal :: Maybe Locking -> [VName] -> Lambda InKernel
                          -> CallKernelGen (Maybe Locking,
                                            [Imp.Exp] -> ImpM InKernel Imp.KernelOp ())
prepareAtomicUpdateGlobal l dests lam =
  -- We need a separate lock array if the operators are not all of a
  -- particularly simple form that permits pure atomic operations.
  case (l, atomicUpdateLocking lam) of
    (_, Left f) -> return (l, f (Space "global") dests)
    (Just l', Right f) -> return (l, f l' (Space "global") dests)
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
        sStaticArray "genred_locks" (Space "device") int32 $
        Imp.ArrayZeros num_locks
      let l' = Locking locks 0 1 0 ((`rem` fromIntegral num_locks) . sum)
      return (Just l', f l' (Space "global") dests)

prepareIntermediateArraysGlobal :: Imp.Exp -> [SegGenRedSlug]
                                -> CallKernelGen
                                   [(VName,
                                     [VName],
                                     [Imp.Exp] -> ImpM InKernel Imp.KernelOp ())]
prepareIntermediateArraysGlobal num_threads = fmap snd . mapAccumLM onOp Nothing
  where
    onOp l (SegGenRedSlug op num_subhistos subhisto_info) = do
      -- Determining the degree of cooperation (heuristic):
      -- coop_lvl   := size of histogram (Cooperation level)
      -- num_histos := (threads / coop_lvl) (Number of histograms)
      -- threads    := min(physical_threads, segment_size)
      --
      -- Careful to avoid division by zero when genReduceWidth==0.
      num_subhistos <--
        num_threads `quotRoundingUp`
        BinOpExp (SMax Int32) 1 (toExp' int32 (genReduceWidth op))

      emit $ Imp.DebugPrint "Number of subhistograms in global memory" $
        Just (int32, Imp.vi32 num_subhistos)

      -- Initialise sub-histograms.
      --
      -- If num_subhistos is 1, then we just reuse the original
      -- destination.  The idea is to avoid a copy if we are writing a
      -- small number of values into a very large prior histogram.
      dests <- forM (zip (genReduceDest op) subhisto_info) $ \(dest, info) -> do
        dest_mem <- entryArrayLocation <$> lookupArray dest

        sub_mem <- fmap memLocationName $
                   entryArrayLocation <$>
                   lookupArray (subhistosArray info)

        let unitHistoCase =
              emit $
              Imp.SetMem sub_mem (memLocationName dest_mem) $
              Space "device"

            multiHistoCase = subhistosAlloc info

        sIf (Imp.var num_subhistos int32 .==. 1) unitHistoCase multiHistoCase

        return $ subhistosArray info

      (l', do_op) <- prepareAtomicUpdateGlobal l dests $ genReduceOp op

      return (l', (num_subhistos, dests, do_op))

genRedKernelGlobal :: [PatElem ExplicitMemory]
                  -> KernelSpace
                  -> [SegGenRedSlug]
                  -> KernelBody InKernel
                  -> CallKernelGen ()
genRedKernelGlobal map_pes space slugs kbody = do
  (base_constants, init_constants) <- kernelInitialisationSetSpace space $ return ()
  let constants = base_constants { kernelThreadActive = true }
      (space_is, space_sizes) = unzip $ spaceDimensions space
      space_sizes_64 = map (i32Toi64 . toExp' int32) space_sizes
      total_w_64 = product space_sizes_64

  histograms <- prepareIntermediateArraysGlobal (kernelNumThreads constants) slugs

  elems_per_thread_64 <- dPrimV "elems_per_thread_64" $
                         total_w_64 `quotRoundingUp`
                         ConvOpExp (SExt Int32 Int64) (kernelNumThreads constants)

  sKernel constants "seggenred_global" $ allThreads constants $ do
    init_constants

    -- Compute subhistogram index for each thread, per histogram.
    subhisto_inds <- forM histograms $ \(num_histograms, _, _) ->
      dPrimV "subhisto_ind" $
      kernelGlobalThreadId constants `quot`
      (kernelNumThreads constants `quotRoundingUp` Imp.var num_histograms int32)

    flat_idx <- newVName "flat_idx"
    sFor flat_idx Int64 (Imp.var elems_per_thread_64 int64) $ do
      -- Compute the offset into the input and output.  To this a
      -- thread can add its local ID to figure out which element it is
      -- responsible for.  The calculation is done with 64-bit
      -- integers to avoid overflow, but the final segment indexes are
      -- 32 bit.
      offset <- dPrimV "offset" $
                (i32Toi64 (kernelGroupId constants) *
                 (Imp.var elems_per_thread_64 int64 *
                  i32Toi64 (kernelGroupSize constants)))
                + (Imp.var flat_idx int64 * i32Toi64 (kernelGroupSize constants))

      j <- dPrimV "j" $ Imp.var offset int64 + i32Toi64 (kernelLocalThreadId constants)

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

      sWhen input_in_bounds $ compileStms mempty (kernelBodyStms kbody) $ do
        let (red_res, map_res) = splitFromEnd (length map_pes) $ kernelBodyResult kbody

        sComment "save map-out results" $
          forM_ (zip map_pes map_res) $ \(pe, res) ->
          copyDWIM (patElemName pe)
          (map ((`Imp.var` int32) . fst) $ kernelDimensions constants)
          (kernelResultSubExp res) []

        let (buckets, vs) = splitAt (length slugs) red_res
            perOp = chunks $ map (length . genReduceDest . slugOp) slugs

        sComment "perform atomic updates" $
          forM_ (zip5 (map slugOp slugs) histograms buckets (perOp vs) subhisto_inds) $
          \(GenReduceOp dest_w _ _ shape lam,
            (_, _, do_op), bucket, vs', subhisto_ind) -> do

            let bucket' = toExp' int32 $ kernelResultSubExp bucket
                dest_w' = toExp' int32 dest_w
                bucket_in_bounds = 0 .<=. bucket' .&&. bucket' .<. dest_w'
                bucket_is = map (`Imp.var` int32) (init space_is) ++
                            [Imp.var subhisto_ind int32, bucket']
                vs_params = takeLast (length vs') $ lambdaParams lam

            sWhen bucket_in_bounds $ do
              dLParams $ lambdaParams lam
              vectorLoops [] (shapeDims shape) $ \is -> do
                forM_ (zip vs_params vs') $ \(p, res) ->
                  copyDWIM (paramName p) [] (kernelResultSubExp res) is
                do_op (bucket_is ++ is)

prepareIntermediateArraysLocal :: KernelSpace -> KernelConstants
                               -> VName -> [SegGenRedSlug]
                               -> CallKernelGen
                                  [([VName],
                                    InKernelGen ([VName],
                                                 [Imp.Exp] -> ImpM InKernel Imp.KernelOp ()))]
prepareIntermediateArraysLocal space constants num_subhistos_per_group =
  fmap snd . mapAccumLM onOp Nothing
  where
    onOp l (SegGenRedSlug op num_subhistos subhisto_info) = do

      num_subhistos <--
        toExp' int32 (spaceNumGroups space)

      emit $ Imp.DebugPrint "Number of subhistograms in global memory" $
        Just (int32, Imp.vi32 num_subhistos)

      -- Some trickery is afoot here because we need to construct a
      -- Locking structure in the CallKernelGen monad, but the actual
      -- initialisation of the locks array must happen on the device.
      -- Also, we want only one locks array, no matter how many
      -- operators need locking.
      (l', mk_op) <-
        case (l, atomicUpdateLocking $ genReduceOp op) of
          (_, Left f) -> return (l, return f)
          (Just l', Right f) -> return (l, return $ f l')
          (Nothing, Right f) -> do
            locks <- newVName "locks"
            num_locks <- toExp $ spaceGroupSize space
            let l' = Locking locks 0 1 0 ((`rem` num_locks) . sum)
                locks_t = localMemLockArray space

                mk_op = do
                  locks_mem <- sAlloc "locks_mem" (typeSize locks_t) $ Space "local"
                  dArray locks int32 (arrayShape locks_t) $
                    ArrayIn locks_mem $ IxFun.iota $
                    map (primExpFromSubExp int32) $ arrayDims locks_t

                  sComment "All locks start out unlocked" $
                    copyDWIM locks [kernelLocalThreadId constants] (intConst Int32 0) []

                  return $ f l'

            return (Just l', mk_op)

      -- Initialise local-memory sub-histograms.  These are
      -- represented as two-dimensional arrays.
      let init_local_subhistos = do
            local_subhistos <-
              forM (genReduceDest op) $ \dest -> do
                dest_t <- lookupType dest

                let sub_local_shape =
                      Shape [Var num_subhistos_per_group] <> arrayShape dest_t
                sAllocArray "subhistogram_local"
                  (elemType dest_t) sub_local_shape (Space "local")

            do_op <- mk_op

            return (local_subhistos, do_op (Space "local") local_subhistos)

      -- Initialise global-memory sub-histograms.
      glob_subhistos <- forM subhisto_info $ \info -> do
        subhistosAlloc info
        return $ subhistosArray info

      return (l', (glob_subhistos, init_local_subhistos))

genRedKernelLocal :: VName
                  -> [PatElem ExplicitMemory]
                  -> KernelSpace
                  -> [SegGenRedSlug]
                  -> KernelBody InKernel
                  -> CallKernelGen ()
genRedKernelLocal num_subhistos_per_group_var map_pes space slugs kbody = do
  (base_constants, init_constants) <- kernelInitialisationSetSpace space $ return ()
  let (space_is, space_sizes) = unzip $ spaceDimensions space
      segment_dims = init space_sizes
      num_segments = length segment_dims
      constants = base_constants { kernelThreadActive = true }
      space_sizes_64 = map (i32Toi64 . toExp' int32) space_sizes
      total_w_64 = product space_sizes_64
      num_subhistos_per_group = Imp.var num_subhistos_per_group_var int32

  emit $ Imp.DebugPrint "Number of local subhistograms per group" $ Just (int32, num_subhistos_per_group)

  init_histograms <- prepareIntermediateArraysLocal space constants num_subhistos_per_group_var slugs

  elems_per_thread_64 <- dPrimV "elems_per_thread_64" $
                         total_w_64 `quotRoundingUp`
                         ConvOpExp (SExt Int32 Int64) (kernelNumThreads constants)

  sKernel constants "seggenred_local" $ allThreads constants $ do
    init_constants

    histograms <- forM init_histograms $
                  \(glob_subhistos, init_local_subhistos) -> do
      (local_subhistos, do_op) <- init_local_subhistos
      return (zip glob_subhistos local_subhistos, do_op)

    -- Find index of local subhistograms updated by this thread.  We
    -- try to ensure, as much as possible, that threads in the same
    -- warp use different subhistograms, to avoid conflicts.
    thread_local_subhisto_i <-
      fmap (`Imp.var` int32) $ dPrimV "thread_local_subhisto_i" $
      kernelLocalThreadId constants `rem` num_subhistos_per_group

    let (red_res, map_res) = splitFromEnd (length map_pes) $
                             map kernelResultSubExp $ kernelBodyResult kbody
        (buckets, vs) = splitAt (length slugs) red_res
        perOp = chunks $ map (length . genReduceDest . slugOp) slugs

    let onSlugs f = forM_ (zip slugs histograms) $ \(slug, (dests, _)) -> do
          let histo_dims =
                map (toExp' int32) $
                segment_dims ++
                genReduceWidth (slugOp slug) : shapeDims (genReduceShape (slugOp slug))
          histo_size <- fmap (`Imp.var` int32) $ dPrimV "histo_size" $
                        product histo_dims
          f slug dests histo_dims histo_size

    let onAllHistograms f =
          onSlugs $ \slug dests histo_dims histo_size -> do
            let group_hists_size = num_subhistos_per_group * histo_size
            init_per_thread <- dPrimV "init_per_thread" $
                               group_hists_size `quotRoundingUp` kernelGroupSize constants

            forM_ (zip dests (genReduceNeutral $ slugOp slug)) $ \((dest_global, dest_local), ne) -> do
              i <- newVName "local_i"
              sFor i Int32 (Imp.var init_per_thread int32) $ do
                j <- fmap (`Imp.var` int32) $ dPrimV "j" $
                     Imp.var i int32 * kernelGroupSize constants +
                     kernelLocalThreadId constants
                j_offset <- fmap (`Imp.var` int32) $ dPrimV "j_offset" $
                            num_subhistos_per_group *
                            histo_size *
                            kernelGroupId constants + j

                local_subhisto_i <- dPrimV "local_subhisto_i" $ j `quot` histo_size
                let bucket_is = unflattenIndex histo_dims $ j `rem` histo_size
                global_subhisto_i <- dPrimV "global_subhisto_i" $ j_offset `quot` histo_size

                sWhen (j .<. group_hists_size) $
                  f dest_local dest_global (slugOp slug) ne
                  (Imp.var local_subhisto_i int32) (Imp.var global_subhisto_i int32)
                  bucket_is

    sComment "initialize histograms in local memory" $
      onAllHistograms $ \dest_local dest_global op ne local_subhisto_i global_subhisto_i bucket_is ->
      sComment "First subhistogram is initialised from global memory; others with neutral element." $ do
      let global_is = take num_segments bucket_is ++
                      [0] ++ drop num_segments bucket_is
          local_is = local_subhisto_i : bucket_is
      sIf (global_subhisto_i .==. 0)
        (copyDWIM dest_local local_is (Var dest_global) global_is)
        (vectorLoops [] (shapeDims $ genReduceShape op) $ \is ->
            copyDWIM dest_local (local_is++is) ne [])

    sOp Imp.LocalBarrier

    flat_idx <- newVName "flat_idx"
    sFor flat_idx Int64 (Imp.var elems_per_thread_64 int64) $ do
      -- Compute the offset into the input and output.  To this a
      -- thread can add its local ID to figure out which element it is
      -- responsible for.  The calculation is done with 64-bit
      -- integers to avoid overflow, but the final segment indexes are
      -- 32 bit.
      offset <- dPrimV "offset" $
                (i32Toi64 (kernelGroupId constants) *
                 (Imp.var elems_per_thread_64 int64 *
                  i32Toi64 (kernelGroupSize constants)))
                + (Imp.var flat_idx int64 * i32Toi64 (kernelGroupSize constants))

      j <- dPrimV "j" $ Imp.var offset int64 + i32Toi64 (kernelLocalThreadId constants)

      -- Construct segment indices.
      zipWithM_ dPrimV_ space_is $
        map (ConvOpExp (SExt Int64 Int32)) . unflattenIndex space_sizes_64 $ Imp.var j int64

      -- We execute the bucket function once and update each histogram serially.
      -- We apply the bucket function if j=offset+ltid is less than
      -- num_elements.  This also involves writing to the mapout
      -- arrays.
      let input_in_bounds = Imp.var j int32 .<. total_w_64

      sWhen input_in_bounds $ compileStms mempty (kernelBodyStms kbody) $ do

        sComment "save map-out results" $
          forM_ (zip map_pes map_res) $ \(pe, se) ->
          copyDWIM (patElemName pe)
          (map ((`Imp.var` int32) . fst) $ kernelDimensions constants) se []

        forM_ (zip4 (map slugOp slugs) histograms buckets (perOp vs)) $
          \(GenReduceOp dest_w _ _ shape lam,
            (_, do_op), bucket, vs') -> do

            let bucket' = toExp' int32 bucket
                dest_w' = toExp' int32 dest_w
                bucket_in_bounds = 0 .<=. bucket' .&&. bucket' .<. dest_w'
                bucket_is = thread_local_subhisto_i :
                            map (`Imp.var` int32) (init space_is) ++ [bucket']
                vs_params = takeLast (length vs') $ lambdaParams lam

            sComment "perform atomic updates" $
              sWhen bucket_in_bounds $ do
              dLParams $ lambdaParams lam
              vectorLoops [] (shapeDims shape) $ \is -> do
                forM_ (zip vs_params vs') $ \(p, v) ->
                  copyDWIM (paramName p) [] v is
                do_op (bucket_is ++ is)

    sOp Imp.LocalBarrier
    sOp Imp.GlobalBarrier

    sComment "Compact the multiple local memory subhistograms to a single subhistogram result" $
      onSlugs $ \slug dests histo_dims histo_size -> do
      bins_per_thread <- fmap (`Imp.var` int32) $ dPrimV "init_per_thread" $
                         histo_size `quotRoundingUp` kernelGroupSize constants

      i <- newVName "local_i"
      sFor i Int32 bins_per_thread $ do
        j <- fmap (`Imp.var` int32) $ dPrimV "j" $
             Imp.var i int32 * kernelGroupSize constants +
             kernelLocalThreadId constants
        sWhen (j .<. histo_size) $ do
          -- We are responsible for compacting the flat bin 'j', which
          -- we immediately unflatten.
          let bucket_is = unflattenIndex histo_dims j
          dLParams $ lambdaParams $ genReduceOp $ slugOp slug
          let (xparams, yparams) = splitAt (length local_dests) $
                                   lambdaParams $ genReduceOp $ slugOp slug
              local_dests = map snd dests

          sComment "Read values from subhistogram 0." $
            forM_ (zip xparams local_dests) $ \(xp, subhisto) ->
            copyDWIM
            (paramName xp) []
            (Var subhisto) (0:bucket_is)

          sComment "Accumulate based on values in other subhistograms." $ do
            subhisto_id <- newVName "subhisto_id"
            sFor subhisto_id Int32 (num_subhistos_per_group - 1) $ do
              forM_ (zip yparams local_dests) $ \(yp, subhisto) ->
                copyDWIM
                (paramName yp) []
                (Var subhisto) (Imp.var subhisto_id int32 + 1 : bucket_is)
              compileBody' xparams $ lambdaBody $ genReduceOp $ slugOp slug

          sComment "Put values back in subhistogram 0." $
            forM_ (zip xparams local_dests) $ \(xp, subhisto) ->
              copyDWIM
              subhisto (0:bucket_is)
              (Var $ paramName xp) []

    sComment "Copy the first local histogram to global memory." $
      onSlugs $ \_slug dests histo_dims histo_size -> do
      write_per_thread <- dPrimV "write_per_thread" $
                          histo_size `quotRoundingUp` kernelGroupSize constants

      forM_ dests $ \(dest_global, dest_local) -> do
        i <- newVName "local_i"
        sFor i Int32 (Imp.var write_per_thread int32) $ do
          j <- fmap (`Imp.var` int32) $ dPrimV "j" $
               Imp.var i int32 * kernelGroupSize constants +
               kernelLocalThreadId constants

          sWhen (j .<. histo_size) $ do
            let bucket_is = unflattenIndex histo_dims $ j `rem` histo_size
                global_is = take num_segments bucket_is ++
                            [kernelGroupId constants] ++
                            drop num_segments bucket_is
                local_is = 0 : bucket_is
            copyDWIM dest_global global_is (Var dest_local) local_is

-- Most of this function is not the histogram part itself, but rather
-- figuring out whether to use a local or global memory strategy, as
-- well as collapsing the subhistograms produced (which are always in
-- global memory, but their number may vary).
compileSegGenRed :: Pattern ExplicitMemory
                 -> KernelSpace
                 -> [GenReduceOp InKernel]
                 -> KernelBody InKernel
                 -> CallKernelGen ()
compileSegGenRed (Pattern _ pes) space ops kbody = do
  let num_red_res = length ops + sum (map (length . genReduceNeutral) ops)
      (all_red_pes, map_pes) = splitAt num_red_res pes

  let t = 8 * 4
  g <- toExp $ spaceGroupSize space
  lmax <- dPrim "lmax" int32
  sOp $ Imp.GetSizeMax lmax Imp.SizeLocalMemory

  (op_hs, slugs) <- unzip <$> mapM (computeHistoUsage space) ops
  h <- fmap (`Imp.var` int32) $
       dPrimV "h" $ Imp.innerExp $ sum op_hs + localMemLockUsage space slugs
  coop <- fmap (`Imp.var` int32) $
          dPrimV "coop" $ h `quotRoundingUp` t

  -- Check for emptyness to avoid division-by-zero.
  sUnless (h .==. 0) $ do
    lh <- dPrimV "lh" $ (g * t) `quotRoundingUp` h

    emit $ Imp.DebugPrint "\n# SegGenRed" Nothing
    emit $ Imp.DebugPrint "Cooperation level" $ Just (int32, coop)
    emit $ Imp.DebugPrint "Memory per set of subhistograms" $ Just (int32, h)
    emit $ Imp.DebugPrint "Desired group size" $ Just (int32, g)

    forM_ slugs $ \slug ->
      emit $ Imp.DebugPrint "Number of subhistograms" $
      Just (int32, Imp.vi32 $ slugNumSubhistos slug)

    sIf (h .<=. Imp.var lmax int32 .&&. coop .<=. g)
      (genRedKernelLocal lh map_pes space slugs kbody)
      (genRedKernelGlobal map_pes space slugs kbody)

    let pes_per_op = chunks (map (length . genReduceDest) ops) all_red_pes

    forM_ (zip3 slugs pes_per_op ops) $ \(slug, red_pes, op) -> do
      let num_histos = slugNumSubhistos slug
          subhistos = map subhistosArray $ slugSubhistos slug

      let unitHistoCase =
            -- This is OK because the memory blocks are at least as
            -- large as the ones we are supposed to use for the result.
            forM_ (zip red_pes subhistos) $ \(pe, subhisto) -> do
              pe_mem <- memLocationName . entryArrayLocation <$>
                        lookupArray (patElemName pe)
              subhisto_mem <- memLocationName . entryArrayLocation <$>
                              lookupArray subhisto
              emit $ Imp.SetMem pe_mem subhisto_mem $ Space "device"

      sIf (Imp.var num_histos int32 .==. 1) unitHistoCase $ do
        -- For the segmented reduction, we keep the segment dimensions
        -- unchanged.  To this, we add two dimensions: one over the number
        -- of buckets, and one over the number of subhistograms.  This
        -- inner dimension is the one that is collapsed in the reduction.
        let num_buckets = genReduceWidth op

        bucket_id <- newVName "bucket_id"
        subhistogram_id <- newVName "subhistogram_id"
        vector_ids <- mapM (const $ newVName "vector_id") $
                      shapeDims $ genReduceShape op
        gtid <- newVName $ baseString $ spaceGlobalId space
        let segred_space =
              space { spaceStructure =
                        FlatThreadSpace $
                        segment_dims ++
                        [(bucket_id, num_buckets)] ++
                        zip vector_ids (shapeDims $ genReduceShape op) ++
                        [(subhistogram_id, Var num_histos)]
                    , spaceGlobalId = gtid
                    }

        compileSegRed' (Pattern [] red_pes) segred_space
          Commutative (genReduceOp op) (genReduceNeutral op) $ \_ red_dests ->
          forM_ (zip red_dests subhistos) $ \((d, is), subhisto) ->
            copyDWIM d is (Var subhisto) $ map (`Imp.var` int32) $
            map fst segment_dims ++ [subhistogram_id, bucket_id] ++ vector_ids

  where segment_dims = init $ spaceDimensions space
