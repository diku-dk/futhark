{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Our compilation strategy for 'SegHist' is based around avoiding
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

module Futhark.CodeGen.ImpGen.Kernels.SegHist
  ( compileSegHist )
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
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Kernels.SegRed (compileSegRed')
import Futhark.CodeGen.ImpGen.Kernels.Base
import Futhark.Util.IntegralExp (quotRoundingUp, quot, rem)
import Futhark.Util (chunks, mapAccumLM, splitFromEnd, takeLast)
import Futhark.Construct (fullSliceNum)

i32Toi64 :: PrimExp v -> PrimExp v
i32Toi64 = ConvOpExp (SExt Int32 Int64)

data SubhistosInfo = SubhistosInfo { subhistosArray :: VName
                                   , subhistosAlloc :: CallKernelGen ()
                                   }

data SegHistSlug = SegHistSlug
                   { slugOp :: HistOp ExplicitMemory
                   , slugNumSubhistos :: VName
                   , slugSubhistos :: [SubhistosInfo]
                   , slugAtomicUpdate :: AtomicUpdate ExplicitMemory
                   }

histoSpaceUsage :: HistOp ExplicitMemory
                -> Imp.Count Imp.Bytes Imp.Exp
histoSpaceUsage op =
  sum $
  map (typeSize .
       (`arrayOfRow` histWidth op) .
       (`arrayOfShape` histShape op)) $
  lambdaReturnType $ histOp op

-- | Figure out how much memory is needed per histogram, both
-- segmented and unsegmented,, and compute some other auxiliary
-- information.
computeHistoUsage :: SegSpace
                  -> HistOp ExplicitMemory
                  -> CallKernelGen (Imp.Count Imp.Bytes Imp.Exp,
                                    Imp.Count Imp.Bytes Imp.Exp,
                                    SegHistSlug)
computeHistoUsage space op = do
  let segment_dims = init $ unSegSpace space
      num_segments = length segment_dims

  -- Create names for the intermediate array memory blocks,
  -- memory block sizes, arrays, and number of subhistograms.
  num_subhistos <- dPrim "num_subhistos" int32
  subhisto_infos <- forM (zip (histDest op) (histNeutral op)) $ \(dest, ne) -> do
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
                            map (toExp' int32) $ arrayDims dest_t

            let subhistos_mem_size =
                  Imp.bytes $
                  Imp.unCount (Imp.elements num_elems `Imp.withElemType` elemType dest_t)

            sAlloc_ subhistos_mem subhistos_mem_size $ Space "device"
            sReplicate subhistos (Shape (map snd segment_dims ++
                                         [Var num_subhistos, histWidth op]) <>
                                  histShape op) ne
            subhistos_t <- lookupType subhistos
            let slice = fullSliceNum (map (toExp' int32) $ arrayDims subhistos_t) $
                        map (unitSlice 0 . toExp' int32 . snd) segment_dims ++
                        [DimFix 0]
            sUpdate subhistos slice $ Var dest

      sIf (Imp.var num_subhistos int32 .==. 1) unitHistoCase multiHistoCase

  let h = histoSpaceUsage op
      segmented_h = h * product (map (Imp.bytes . toExp' int32) $ init $ segSpaceDims space)

  return (h,
          segmented_h,
          SegHistSlug op num_subhistos subhisto_infos $
          atomicUpdateLocking $ histOp op)

prepareAtomicUpdateGlobal :: Maybe Locking -> [VName] -> SegHistSlug
                          -> CallKernelGen (Maybe Locking,
                                            [Imp.Exp] -> InKernelGen ())
prepareAtomicUpdateGlobal l dests slug =
  -- We need a separate lock array if the operators are not all of a
  -- particularly simple form that permits pure atomic operations.
  case (l, slugAtomicUpdate slug) of
    (_, AtomicPrim f) -> return (l, f (Space "global") dests)
    (_, AtomicCAS f) -> return (l, f (Space "global") dests)
    (Just l', AtomicLocking f) -> return (l, f l' (Space "global") dests)
    (Nothing, AtomicLocking f) -> do
      -- The number of locks used here is too low, but since we are
      -- currently forced to inline a huge list, I'm keeping it down
      -- for now.  Some quick experiments suggested that it has little
      -- impact anyway (maybe the locking case is just too slow).
      --
      -- A fun solution would also be to use a simple hashing
      -- algorithm to ensure good distribution of locks.
      let num_locks = 10000
          dims = map (toExp' int32) $
                 shapeDims (histShape (slugOp slug)) ++
                 [ Var (slugNumSubhistos slug)
                 , histWidth (slugOp slug)]
      locks <-
        sStaticArray "hist_locks" (Space "device") int32 $
        Imp.ArrayZeros num_locks
      let l' = Locking locks 0 1 0 (pure . (`rem` fromIntegral num_locks) . flattenIndex dims)
      return (Just l', f l' (Space "global") dests)

infoPrints :: Imp.Exp -> VName -> Imp.Exp -> ImpM lore op ()
infoPrints hist_H hist_M hist_C = do
  emit $ Imp.DebugPrint "Histogram size (H)" $ Just hist_H
  emit $ Imp.DebugPrint "Multiplication degree (M)" $ Just $ Imp.vi32 hist_M
  emit $ Imp.DebugPrint "Cooperation level (C)" $ Just hist_C

prepareIntermediateArraysGlobal :: Imp.Exp -> Imp.Exp -> [SegHistSlug]
                                -> CallKernelGen
                                   [(VName,
                                     [VName],
                                     [Imp.Exp] -> InKernelGen ())]
prepareIntermediateArraysGlobal hist_T hist_N =
  fmap snd . mapAccumLM onOp Nothing
  where
    onOp l slug@(SegHistSlug op num_subhistos subhisto_info do_op) = do
      hist_H <- toExp $ histWidth op

      hist_u <- dPrimVE "hist_u" $
                case do_op of
                  AtomicPrim{} -> 1
                  _            -> 2

      hist_RF <- toExp $ histRaceFactor op

      let hist_k_RF = 0.75
          hist_L2 = 16384
          hist_L2_ln_sz = 64
          hist_F_L2 = 0.4

      let r64 = ConvOpExp (SIToFP Int32 Float64)
          t64 = ConvOpExp (FPToSI Float64 Int32)

      let hist_el_size =
            case do_op of
              AtomicLocking{} ->
                unCount
                (sum $ map (typeSize . (`arrayOfShape` histShape op)) $
                 Prim int32 : lambdaReturnType (histOp op))
                `quot` genericLength (lambdaReturnType (histOp op))
              _ ->
                unCount $ sum $
                map (typeSize . (`arrayOfShape` histShape op)) $
                lambdaReturnType (histOp op)

      hist_RACE_exp <- dPrimVE "hist_RACE_exp" $
        Imp.BinOpExp (FMax Float64) 1 $
        (hist_k_RF * r64 hist_RF) /
        Imp.BinOpExp (FMin Float64) 1 (r64 hist_L2_ln_sz  / r64 hist_el_size)

      -- Hardcode a single pass for now.
      let hist_S = 1

      hist_H_chk <- dPrimVE "hist_H_chk" $
                    hist_H `quotRoundingUp` hist_S


      hist_k_max <- dPrimVE "hist_k_max" $
        Imp.BinOpExp (SMin Int32)
        (t64 (hist_F_L2 * hist_L2 * hist_RACE_exp)) hist_N
        `quotRoundingUp` hist_T

      hist_C <- dPrimVE "hist_C" $
                Imp.BinOpExp (SMin Int32) hist_T $
                (hist_u * hist_H_chk) `quotRoundingUp` hist_k_max

      -- Minimal sequential chunking factor.
      let q_small = 2
      work_asymp_M_max <- dPrimVE "work_asymp_M_max" $
                          hist_N `quot` (q_small * hist_H)

      let glb_k_min = 2
      coop_min <- dPrimVE "coop_min" $
                  Imp.BinOpExp (SMin Int32) hist_T $ hist_H `quot` glb_k_min

      hist_M_min <- dPrimVE "hist_M_min" $
                    Imp.BinOpExp (SMax Int32) 1 $
                    Imp.BinOpExp (SMin Int32) work_asymp_M_max $
                    hist_T `quot` coop_min

      -- Number of subhistograms per result histogram.
      hist_M <- dPrimV "hist_M" $
                Imp.BinOpExp (SMin Int32) hist_M_min $
                Imp.BinOpExp (SMax Int32) 1 $ hist_T `quot` hist_C

      -- num_subhistos is the variable we use to communicate back.
      num_subhistos <-- Imp.vi32 hist_M

      -- Initialise sub-histograms.
      --
      -- If hist_M is 1, then we just reuse the original
      -- destination.  The idea is to avoid a copy if we are writing a
      -- small number of values into a very large prior histogram.
      dests <- forM (zip (histDest op) subhisto_info) $ \(dest, info) -> do
        dest_mem <- entryArrayLocation <$> lookupArray dest

        sub_mem <- fmap memLocationName $
                   entryArrayLocation <$>
                   lookupArray (subhistosArray info)

        let unitHistoCase =
              emit $
              Imp.SetMem sub_mem (memLocationName dest_mem) $
              Space "device"

            multiHistoCase = subhistosAlloc info

        sIf (Imp.var hist_M int32 .==. 1) unitHistoCase multiHistoCase

        return $ subhistosArray info

      (l', do_op') <- prepareAtomicUpdateGlobal l dests slug

      return (l', (hist_M, dests, do_op'))

histKernelGlobal :: [PatElem ExplicitMemory]
                 -> Count NumGroups SubExp -> Count GroupSize SubExp
                 -> SegSpace
                 -> [SegHistSlug]
                 -> KernelBody ExplicitMemory
                 -> CallKernelGen ()
histKernelGlobal map_pes num_groups group_size space slugs kbody = do
  num_groups' <- traverse toExp num_groups
  group_size' <- traverse toExp group_size
  let (space_is, space_sizes) = unzip $ unSegSpace space
      space_sizes_64 = map (i32Toi64 . toExp' int32) space_sizes
      total_w_64 = product space_sizes_64
      num_threads = unCount num_groups' * unCount group_size'

  emit $ Imp.DebugPrint "## Using global memory" Nothing

  histograms <-
    prepareIntermediateArraysGlobal num_threads
    (toExp' int32 $ last space_sizes) slugs

  elems_per_thread_64 <- dPrimVE "elems_per_thread_64" $
                         total_w_64 `quotRoundingUp`
                         ConvOpExp (SExt Int32 Int64) num_threads

  sKernelThread "seghist_global" num_groups' group_size' (segFlat space) $ \constants -> do
    -- Compute subhistogram index for each thread, per histogram.
    subhisto_inds <- forM histograms $ \(num_histograms, _, _) ->
      dPrimVE "subhisto_ind" $
      kernelGlobalThreadId constants `quot`
      (kernelNumThreads constants `quotRoundingUp` Imp.var num_histograms int32)

    sFor "flat_idx" elems_per_thread_64 $ \flat_idx -> do
      -- Compute the offset into the input and output.  To this a
      -- thread can add its local ID to figure out which element it is
      -- responsible for.  The calculation is done with 64-bit
      -- integers to avoid overflow, but the final segment indexes are
      -- 32 bit.
      offset <- dPrimVE "offset" $
                (i32Toi64 (kernelGroupId constants) *
                 (elems_per_thread_64 *
                  i32Toi64 (kernelGroupSize constants)))
                + (flat_idx * i32Toi64 (kernelGroupSize constants))

      j <- dPrimVE "j" $ offset + i32Toi64 (kernelLocalThreadId constants)

      -- Construct segment indices.
      let setIndex v e = do dPrim_ v int32
                            v <-- e
      zipWithM_ setIndex space_is $
        map (ConvOpExp (SExt Int64 Int32)) $ unflattenIndex space_sizes_64 j

      -- We execute the bucket function once and update each histogram serially.
      -- We apply the bucket function if j=offset+ltid is less than
      -- num_elements.  This also involves writing to the mapout
      -- arrays.
      let input_in_bounds = j .<. total_w_64

      sWhen input_in_bounds $ compileStms mempty (kernelBodyStms kbody) $ do
        let (red_res, map_res) = splitFromEnd (length map_pes) $ kernelBodyResult kbody

        sComment "save map-out results" $
          forM_ (zip map_pes map_res) $ \(pe, res) ->
          copyDWIM (patElemName pe)
          (map (Imp.vi32 . fst) $ unSegSpace space)
          (kernelResultSubExp res) []

        let (buckets, vs) = splitAt (length slugs) red_res
            perOp = chunks $ map (length . histDest . slugOp) slugs

        sComment "perform atomic updates" $
          forM_ (zip5 (map slugOp slugs) histograms buckets (perOp vs) subhisto_inds) $
          \(HistOp dest_w _ _ _ shape lam,
            (_, _, do_op), bucket, vs', subhisto_ind) -> do

            let bucket' = toExp' int32 $ kernelResultSubExp bucket
                dest_w' = toExp' int32 dest_w
                bucket_in_bounds = 0 .<=. bucket' .&&. bucket' .<. dest_w'
                bucket_is = map Imp.vi32 (init space_is) ++
                            [subhisto_ind, bucket']
                vs_params = takeLast (length vs') $ lambdaParams lam

            sWhen bucket_in_bounds $ do
              dLParams $ lambdaParams lam
              sLoopNest shape $ \is -> do
                forM_ (zip vs_params vs') $ \(p, res) ->
                  copyDWIM (paramName p) [] (kernelResultSubExp res) is
                do_op (bucket_is ++ is)

prepareIntermediateArraysLocal :: VName
                               -> Count NumGroups SubExp
                               -> SegSpace -> [SegHistSlug]
                               -> CallKernelGen
                                  [([VName],
                                    KernelConstants ->
                                    InKernelGen ([VName],
                                                 [Imp.Exp] -> InKernelGen ()))]
prepareIntermediateArraysLocal num_subhistos_per_group num_groups space =
  mapM onOp
  where
    onOp (SegHistSlug op num_subhistos subhisto_info do_op) = do

      -- For the segmented case we produce a single histogram per group.
      if length (unSegSpace space) > 1
        then num_subhistos <-- 1
        else num_subhistos <-- toExp' int32 (unCount num_groups)

      emit $ Imp.DebugPrint "Number of subhistograms in global memory" $
        Just $ Imp.vi32 num_subhistos

      mk_op <-
        case do_op of
          AtomicPrim f -> return $ const $ return f
          AtomicCAS f -> return $ const $ return f
          AtomicLocking f -> do

            let lock_shape =
                  Shape $ Var num_subhistos_per_group :
                  shapeDims (histShape op) ++
                  [histWidth op]

            dims <- mapM toExp $ shapeDims lock_shape

            return $ \constants -> do
              locks <- sAllocArray "locks" int32 lock_shape $ Space "local"

              sComment "All locks start out unlocked" $
                groupCoverSpace constants dims $ \is ->
                copyDWIM locks is (intConst Int32 0) []

              return $ f $ Locking locks 0 1 0 id

      -- Initialise local-memory sub-histograms.  These are
      -- represented as two-dimensional arrays.
      let init_local_subhistos constants = do
            local_subhistos <-
              forM (histType op) $ \t -> do
                let sub_local_shape =
                      Shape [Var num_subhistos_per_group] <> arrayShape t
                sAllocArray "subhistogram_local"
                  (elemType t) sub_local_shape (Space "local")

            do_op' <- mk_op constants

            return (local_subhistos, do_op' (Space "local") local_subhistos)

      -- Initialise global-memory sub-histograms.
      glob_subhistos <- forM subhisto_info $ \info -> do
        subhistosAlloc info
        return $ subhistosArray info

      return (glob_subhistos, init_local_subhistos)

histKernelLocal :: VName -> Count NumGroups Imp.Exp
                  -> [PatElem ExplicitMemory]
                  -> Count NumGroups SubExp -> Count GroupSize SubExp
                  -> SegSpace
                  -> [SegHistSlug]
                  -> KernelBody ExplicitMemory
                  -> CallKernelGen ()
histKernelLocal num_subhistos_per_group_var groups_per_segment map_pes num_groups group_size space slugs kbody = do
  num_groups' <- traverse toExp num_groups
  group_size' <- traverse toExp group_size
  let (space_is, space_sizes) = unzip $ unSegSpace space
      segment_is = init space_is
      segment_dims = init space_sizes
      (i_in_segment, segment_size) = last $ unSegSpace space
      num_subhistos_per_group = Imp.var num_subhistos_per_group_var int32

  segment_size' <- toExp segment_size

  emit $ Imp.DebugPrint "Number of local subhistograms per group" $ Just num_subhistos_per_group

  init_histograms <-
    prepareIntermediateArraysLocal num_subhistos_per_group_var num_groups space slugs

  num_segments <- dPrimVE "num_segments" $
                  product $ map (toExp' int32) segment_dims

  sKernelThread "seghist_local" num_groups' group_size' (segFlat space) $ \constants ->
    virtualiseGroups constants SegVirt (unCount groups_per_segment * num_segments) $ \group_id_var -> do

    let group_id = Imp.vi32 group_id_var

    flat_segment_id <- dPrimVE "flat_segment_id" $ group_id `quot` unCount groups_per_segment
    gid_in_segment <- dPrimVE "gid_in_segment" $ group_id `rem` unCount groups_per_segment
    -- This pgtid is kind of a "virtualised physical" gtid - not the
    -- same thing as the gtid used for the SegHist itself.
    pgtid_in_segment <- dPrimVE "pgtid_in_segment" $
      gid_in_segment * kernelGroupSize constants + kernelLocalThreadId constants
    threads_per_segment <- dPrimVE "threads_per_segment" $
      unCount groups_per_segment * kernelGroupSize constants

    -- Set segment indices.
    zipWithM_ dPrimV_ segment_is $
      unflattenIndex (map (toExp' int32) segment_dims) flat_segment_id

    histograms <- forM init_histograms $
                  \(glob_subhistos, init_local_subhistos) -> do
      (local_subhistos, do_op) <- init_local_subhistos constants
      return (zip glob_subhistos local_subhistos, do_op)

    -- Find index of local subhistograms updated by this thread.  We
    -- try to ensure, as much as possible, that threads in the same
    -- warp use different subhistograms, to avoid conflicts.
    thread_local_subhisto_i <-
      dPrimVE "thread_local_subhisto_i" $
      kernelLocalThreadId constants `rem` num_subhistos_per_group

    let (red_res, map_res) = splitFromEnd (length map_pes) $
                             map kernelResultSubExp $ kernelBodyResult kbody
        (buckets, vs) = splitAt (length slugs) red_res
        perOp = chunks $ map (length . histDest . slugOp) slugs

    let onSlugs f = forM_ (zip slugs histograms) $ \(slug, (dests, _)) -> do
          let histo_dims = map (toExp' int32) $ histWidth (slugOp slug) :
                           shapeDims (histShape (slugOp slug))
          histo_size <- dPrimVE "histo_size" $ product histo_dims
          f slug dests histo_dims histo_size

    let onAllHistograms f =
          onSlugs $ \slug dests histo_dims histo_size -> do
            let group_hists_size = num_subhistos_per_group * histo_size
            init_per_thread <- dPrimVE "init_per_thread" $
                               group_hists_size `quotRoundingUp`
                               kernelGroupSize constants

            forM_ (zip dests (histNeutral $ slugOp slug)) $
              \((dest_global, dest_local), ne) ->
                sFor "local_i" init_per_thread $ \i -> do
                  j <- dPrimVE "j" $
                       i * kernelGroupSize constants +
                       kernelLocalThreadId constants
                  j_offset <- dPrimVE "j_offset" $
                              num_subhistos_per_group * histo_size * gid_in_segment + j

                  local_subhisto_i <- dPrimVE "local_subhisto_i" $ j `quot` histo_size
                  let bucket_is = unflattenIndex histo_dims $ j `rem` histo_size
                  global_subhisto_i <- dPrimVE "global_subhisto_i" $ j_offset `quot` histo_size

                  sWhen (j .<. group_hists_size) $
                    f dest_local dest_global (slugOp slug) ne
                    local_subhisto_i global_subhisto_i
                    bucket_is

    sComment "initialize histograms in local memory" $
      onAllHistograms $ \dest_local dest_global op ne local_subhisto_i global_subhisto_i bucket_is ->
      sComment "First subhistogram is initialised from global memory; others with neutral element." $ do
      let global_is = map Imp.vi32 segment_is ++ [0] ++ bucket_is
          local_is = local_subhisto_i : bucket_is
      sIf (global_subhisto_i .==. 0)
        (copyDWIM dest_local local_is (Var dest_global) global_is)
        (sLoopNest (histShape op) $ \is ->
            copyDWIM dest_local (local_is++is) ne [])

    sOp Imp.LocalBarrier

    kernelLoop pgtid_in_segment threads_per_segment segment_size' $ \ie -> do
      dPrimV_ i_in_segment ie

      -- We execute the bucket function once and update each histogram
      -- serially.  This also involves writing to the mapout arrays.

      compileStms mempty (kernelBodyStms kbody) $ do

        sComment "save map-out results" $
          forM_ (zip map_pes map_res) $ \(pe, se) ->
          copyDWIM (patElemName pe)
          (map Imp.vi32 space_is) se []

        forM_ (zip4 (map slugOp slugs) histograms buckets (perOp vs)) $
          \(HistOp dest_w _ _ _ shape lam,
            (_, do_op), bucket, vs') -> do

            let bucket' = toExp' int32 bucket
                dest_w' = toExp' int32 dest_w
                bucket_in_bounds = 0 .<=. bucket' .&&. bucket' .<. dest_w'
                bucket_is = [thread_local_subhisto_i, bucket']
                vs_params = takeLast (length vs') $ lambdaParams lam

            sComment "perform atomic updates" $
              sWhen bucket_in_bounds $ do
              dLParams $ lambdaParams lam
              sLoopNest shape $ \is -> do
                forM_ (zip vs_params vs') $ \(p, v) ->
                  copyDWIM (paramName p) [] v is
                do_op (bucket_is ++ is)

    sOp Imp.LocalBarrier
    sOp Imp.GlobalBarrier

    sComment "Compact the multiple local memory subhistograms to result in global memory" $
      onSlugs $ \slug dests histo_dims histo_size -> do
      bins_per_thread <- dPrimVE "init_per_thread" $
                         histo_size `quotRoundingUp` kernelGroupSize constants

      sFor "local_i" bins_per_thread $ \i -> do
        j <- dPrimVE "j" $
             i * kernelGroupSize constants + kernelLocalThreadId constants
        sWhen (j .<. histo_size) $ do
          -- We are responsible for compacting the flat bin 'j', which
          -- we immediately unflatten.
          let bucket_is = unflattenIndex histo_dims j
          dLParams $ lambdaParams $ histOp $ slugOp slug
          let (xparams, yparams) = splitAt (length local_dests) $
                                   lambdaParams $ histOp $ slugOp slug
              (global_dests, local_dests) = unzip dests

          sComment "Read values from subhistogram 0." $
            forM_ (zip xparams local_dests) $ \(xp, subhisto) ->
            copyDWIM
            (paramName xp) []
            (Var subhisto) (0:bucket_is)

          sComment "Accumulate based on values in other subhistograms." $
            sFor "subhisto_id" (num_subhistos_per_group - 1) $ \subhisto_id -> do
              forM_ (zip yparams local_dests) $ \(yp, subhisto) ->
                copyDWIM
                (paramName yp) []
                (Var subhisto) (subhisto_id + 1 : bucket_is)
              compileBody' xparams $ lambdaBody $ histOp $ slugOp slug

          sComment "Put final bucket value in global memory." $ do
            let global_is = map Imp.vi32 segment_is ++
                            [group_id `rem` unCount groups_per_segment] ++
                            bucket_is
            forM_ (zip xparams global_dests) $ \(xp, global_dest) ->
              copyDWIM global_dest global_is (Var $ paramName xp) []

localMemoryCase :: [PatElem ExplicitMemory]
                -> Count NumGroups SubExp -> Count GroupSize SubExp
                -> SegSpace
                -> Imp.Exp -> Imp.Exp -> Imp.Exp -> Imp.Exp
                -> [SegHistSlug]
                -> KernelBody ExplicitMemory
                -> CallKernelGen (Imp.Exp, ImpM ExplicitMemory Imp.HostOp ())
localMemoryCase map_pes num_groups group_size space hist_H hist_el_size hist_N hist_RF slugs kbody = do
  num_groups' <- traverse toExp num_groups
  group_size' <- traverse toExp group_size

  -- Maximum group size (or actual, in this case).
  let hist_B = unCount group_size'
      space_sizes = segSpaceDims space
      segment_dims = init space_sizes
      segmented = not $ null segment_dims

  hist_L <- dPrim "hist_L" int32
  sOp $ Imp.GetSizeMax hist_L Imp.SizeLocalMemory

  let r64 = ConvOpExp (SIToFP Int32 Float64)
      t64 = ConvOpExp (FPToSI Float64 Int32)
      f64ceil x = t64 $ FunExp "round64" [x] $ FloatType Float64

  -- M approximation.
  hist_m' <- dPrimVE "hist_m_prime" $
             r64 (Imp.BinOpExp (SMin Int32)
                  (Imp.vi32 hist_L `quot` hist_el_size)
                  (hist_N `quotRoundingUp` unCount num_groups'))
             / r64 hist_H

  hist_m <- dPrimVE "hist_m" $
            Imp.BinOpExp (FMax Float64) (r64 1) hist_m'

  -- FIXME: query the lockstep width at runtime.
  hist_W <- dPrimVE "hist_W" 32

  hist_RFC <- dPrimVE "hist_RFC" $
              Imp.BinOpExp (FMin Float64) (r64 hist_RF) $
              r64 hist_W * Imp.BinOpExp (FPow Float64) (r64 hist_RF / r64 hist_W) (1.0 / 3.0)

  hist_f' <- dPrimVE "hist_f_prime" $
             (r64 hist_B * hist_RFC) /
             (hist_m * hist_m * r64 hist_H)

  let casOp AtomicCAS{} = True
      casOp _ = False
      lockOp AtomicLocking{} = True
      lockOp _ = False

      hwdCase =
        dPrimVE "hist_M0" $
        Imp.BinOpExp (SMax Int32) 1 $
        Imp.BinOpExp (SMin Int32) (t64 hist_m') hist_B

      casCase = do
        hist_f_cas <- dPrimVE "hist_f_cas" $ Imp.BinOpExp (SMax Int32) 1 $ f64ceil hist_f'
        dPrimVE "hist_M0" $
          Imp.BinOpExp (SMax Int32) 1 $
          Imp.BinOpExp (SMin Int32) (t64 $ hist_m * r64 hist_f_cas) hist_B

      lockCase = do
        hist_f_cas <- dPrimVE "hist_f_lock" $ Imp.BinOpExp (SMax Int32) 1 $ f64ceil hist_f'
        dPrimVE "hist_M0" $
          Imp.BinOpExp (SMax Int32) 1 $
          Imp.BinOpExp (SMin Int32) (t64 $ hist_m' * r64 hist_f_cas) hist_B

  -- M in the paper, but not adjusted for asymptotic efficiency.
  hist_M0 <-
    if any (lockOp . slugAtomicUpdate) slugs
    then lockCase
    else if any (casOp . slugAtomicUpdate) slugs
    then casCase
    else hwdCase

  -- Minimal sequential chunking factor.
  let q_small = 2

  hist_Nout <- dPrimVE "hist_Nout" $
               product $ map (toExp' int32) segment_dims

  hist_Nin <- dPrimVE "hist_Nin" $ toExp' int32 $ last space_sizes

  -- Maximum M for work efficiency.
  work_asymp_M_max <-
    if segmented then do

      hist_T <- dPrimVE "hist_T" $ unCount num_groups' * unCount group_size'

      hist_T_hist_min <- dPrimVE "hist_T_hist_min" $
                         Imp.BinOpExp (SMin Int32) (hist_Nin * hist_Nout) hist_T
                         `quotRoundingUp`
                         hist_Nout

      -- Number of groups, rounded up.
      let r = hist_T_hist_min `quotRoundingUp` hist_B

      dPrimVE "work_asymp_M_max" $ hist_Nin `quot` (r * hist_H)

    else dPrimVE "work_asymp_M_max" $
         (hist_Nout * hist_N) `quot`
         ((q_small * unCount num_groups' * hist_H)
          `quot` genericLength slugs)

  -- Number of subhistograms per result histogram.
  hist_M <- dPrimV "hist_M" $
            Imp.BinOpExp (SMin Int32) hist_M0 work_asymp_M_max

  -- hist_M may be zero (which we'll check for below), but we need it
  -- for some divisions first, so crudely make a nonzero form.
  let hist_M_nonzero = Imp.BinOpExp (SMax Int32) 1 $ Imp.vi32 hist_M

  -- "Cooperation factor" - the number of threads cooperatively
  -- working on the same (sub)histogram.
  hist_C <- dPrimVE "hist_C" $
            hist_B `quotRoundingUp` hist_M_nonzero

  emit $ Imp.DebugPrint "local hist_M0" $ Just hist_M0
  emit $ Imp.DebugPrint "local work asymp M max" $ Just work_asymp_M_max
  emit $ Imp.DebugPrint "local C" $ Just hist_C
  emit $ Imp.DebugPrint "local B" $ Just hist_B
  emit $ Imp.DebugPrint "local M" $ Just $ Imp.vi32 hist_M
  emit $ Imp.DebugPrint "local memory needed" $
    Just $ hist_H * hist_el_size * Imp.vi32 hist_M

  -- We only use local memory if the number of updates per histogram
  -- at least matches the histogram size, as otherwise it is not
  -- asymptotically efficient.  This mostly matters for the segmented
  -- case.
  let pick_local =
        hist_Nin .>=. hist_H
        .&&. (hist_H * hist_el_size * Imp.vi32 hist_M
              .<=. Imp.vi32 hist_L)
        .&&. hist_C .<=. hist_B
        .&&. Imp.vi32 hist_M .>. 0

      groups_per_segment
        | segmented = 1
        | otherwise = num_groups'

      run = do
        emit $ Imp.DebugPrint "## Using local memory" Nothing
        infoPrints hist_H hist_M hist_C
        histKernelLocal hist_M groups_per_segment map_pes num_groups group_size space slugs kbody

  return (pick_local, run)

-- Most of this function is not the histogram part itself, but rather
-- figuring out whether to use a local or global memory strategy, as
-- well as collapsing the subhistograms produced (which are always in
-- global memory, but their number may vary).
compileSegHist :: Pattern ExplicitMemory
                 -> Count NumGroups SubExp -> Count GroupSize SubExp
                 -> SegSpace
                 -> [HistOp ExplicitMemory]
                 -> KernelBody ExplicitMemory
                 -> CallKernelGen ()
compileSegHist (Pattern _ pes) num_groups group_size space ops kbody = do
  group_size' <- traverse toExp group_size

  dims <- mapM toExp $ segSpaceDims space

  let num_red_res = length ops + sum (map (length . histNeutral) ops)
      (all_red_pes, map_pes) = splitAt num_red_res pes
      segment_size = last dims

  (op_hs, op_seg_hs, slugs) <- unzip3 <$> mapM (computeHistoUsage space) ops
  h <- dPrimVE "h" $ Imp.unCount $ sum op_hs
  seg_h <- dPrimVE "seg_h" $ Imp.unCount $ sum op_seg_hs

  -- Maximum group size (or actual, in this case).
  let hist_B = unCount group_size'

  -- Size of a histogram.
  hist_H <- dPrimVE "hist_H" $ sum $ map (toExp' int32 . histWidth) ops

  -- Size of a single histogram element.  Actually the weighted
  -- average of histogram elements in cases where we have more than
  -- one histogram operation, plus any locks.
  let lockSize slug = case slugAtomicUpdate slug of
                        AtomicLocking{} -> Just $ primByteSize int32
                        _               -> Nothing
  hist_el_size <- dPrimVE "hist_el_size" $ foldl' (+) (h `quotRoundingUp` hist_H) $
                  mapMaybe lockSize slugs

  -- Input elements contributing to each histogram.
  hist_N <- dPrimVE "hist_N" segment_size

  -- Compute RF as the average RF over all the histograms.
  hist_RF <- dPrimVE "hist_RF" $
             sum (map (toExp' int32. histRaceFactor . slugOp) slugs)
             `quot`
             genericLength slugs

  -- Check for emptyness to avoid division-by-zero.
  sUnless (seg_h .==. 0) $ do

    emit $ Imp.DebugPrint "\n# SegHist" Nothing
    emit $ Imp.DebugPrint "Memory per set of subhistograms per segment" $ Just h
    emit $ Imp.DebugPrint "Memory per set of subhistograms times segments" $ Just seg_h
    emit $ Imp.DebugPrint "Input elements per histogram (N)" $ Just hist_N
    emit $ Imp.DebugPrint "Desired group size (B)" $ Just hist_B
    emit $ Imp.DebugPrint "Histogram size (H)" $ Just hist_H
    emit $ Imp.DebugPrint "Histogram element size (el_size)" $ Just hist_el_size
    emit $ Imp.DebugPrint "Race factor (RF)" $ Just hist_RF

    (use_local_memory, run_in_local_memory) <-
      localMemoryCase map_pes num_groups group_size space hist_H hist_el_size hist_N hist_RF slugs kbody

    sIf use_local_memory run_in_local_memory $
      histKernelGlobal map_pes num_groups group_size space slugs kbody

    let pes_per_op = chunks (map (length . histDest) ops) all_red_pes

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
        let num_buckets = histWidth op

        bucket_id <- newVName "bucket_id"
        subhistogram_id <- newVName "subhistogram_id"
        vector_ids <- mapM (const $ newVName "vector_id") $
                      shapeDims $ histShape op

        flat_gtid <- newVName "flat_gtid"

        let lvl = SegThread num_groups group_size SegVirt
            segred_space =
              SegSpace flat_gtid $
              segment_dims ++
              [(bucket_id, num_buckets)] ++
              zip vector_ids (shapeDims $ histShape op) ++
              [(subhistogram_id, Var num_histos)]

        let segred_op = SegRedOp Commutative (histOp op) (histNeutral op) mempty
        compileSegRed' (Pattern [] red_pes) lvl segred_space [segred_op] $ \_ red_cont ->
          red_cont $ flip map subhistos $ \subhisto ->
            (Var subhisto, map Imp.vi32 $
              map fst segment_dims ++ [subhistogram_id, bucket_id] ++ vector_ids)

  where segment_dims = init $ unSegSpace space
