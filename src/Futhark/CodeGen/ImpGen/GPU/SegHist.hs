{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

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
module Futhark.CodeGen.ImpGen.GPU.SegHist (compileSegHist) where

import Control.Monad.Except
import Data.List (foldl', genericLength, zip4, zip6)
import Data.Maybe
import qualified Futhark.CodeGen.ImpCode.GPU as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.GPU.Base
import Futhark.CodeGen.ImpGen.GPU.SegRed (compileSegRed')
import Futhark.Construct (fullSliceNum)
import Futhark.IR.GPUMem
import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.MonadFreshNames
import Futhark.Pass.ExplicitAllocations ()
import Futhark.Util (chunks, mapAccumLM, maxinum, splitFromEnd, takeLast)
import Futhark.Util.IntegralExp (divUp, quot, rem)
import Prelude hiding (quot, rem)

data SubhistosInfo = SubhistosInfo
  { subhistosArray :: VName,
    subhistosAlloc :: CallKernelGen ()
  }

data SegHistSlug = SegHistSlug
  { slugOp :: HistOp GPUMem,
    slugNumSubhistos :: TV Int64,
    slugSubhistos :: [SubhistosInfo],
    slugAtomicUpdate :: AtomicUpdate GPUMem KernelEnv
  }

histoSpaceUsage ::
  HistOp GPUMem ->
  Imp.Count Imp.Bytes (Imp.TExp Int64)
histoSpaceUsage op =
  sum $
    map
      ( typeSize
          . (`arrayOfRow` histWidth op)
          . (`arrayOfShape` histShape op)
      )
      $ lambdaReturnType $ histOp op

-- | Figure out how much memory is needed per histogram, both
-- segmented and unsegmented,, and compute some other auxiliary
-- information.
computeHistoUsage ::
  SegSpace ->
  HistOp GPUMem ->
  CallKernelGen
    ( Imp.Count Imp.Bytes (Imp.TExp Int64),
      Imp.Count Imp.Bytes (Imp.TExp Int64),
      SegHistSlug
    )
computeHistoUsage space op = do
  let segment_dims = init $ unSegSpace space
      num_segments = length segment_dims

  -- Create names for the intermediate array memory blocks,
  -- memory block sizes, arrays, and number of subhistograms.
  num_subhistos <- dPrim "num_subhistos" int32
  subhisto_infos <- forM (zip (histDest op) (histNeutral op)) $ \(dest, ne) -> do
    dest_t <- lookupType dest
    dest_mem <- entryArrayLoc <$> lookupArray dest

    subhistos_mem <-
      sDeclareMem (baseString dest ++ "_subhistos_mem") (Space "device")

    let subhistos_shape =
          Shape (map snd segment_dims ++ [tvSize num_subhistos])
            <> stripDims num_segments (arrayShape dest_t)
    subhistos <-
      sArray
        (baseString dest ++ "_subhistos")
        (elemType dest_t)
        subhistos_shape
        subhistos_mem
        $ IxFun.iota $ map pe64 $ shapeDims subhistos_shape

    return $
      SubhistosInfo subhistos $ do
        let unitHistoCase =
              emit $
                Imp.SetMem subhistos_mem (memLocName dest_mem) $
                  Space "device"

            multiHistoCase = do
              let num_elems =
                    foldl' (*) (sExt64 $ tvExp num_subhistos) $
                      map toInt64Exp $ arrayDims dest_t

              let subhistos_mem_size =
                    Imp.bytes $
                      Imp.unCount (Imp.elements num_elems `Imp.withElemType` elemType dest_t)

              sAlloc_ subhistos_mem subhistos_mem_size $ Space "device"
              sReplicate subhistos ne
              subhistos_t <- lookupType subhistos
              let slice =
                    fullSliceNum (map toInt64Exp $ arrayDims subhistos_t) $
                      map (unitSlice 0 . toInt64Exp . snd) segment_dims
                        ++ [DimFix 0]
              sUpdate subhistos slice $ Var dest

        sIf (tvExp num_subhistos .==. 1) unitHistoCase multiHistoCase

  let h = histoSpaceUsage op
      segmented_h = h * product (map (Imp.bytes . toInt64Exp) $ init $ segSpaceDims space)

  atomics <- hostAtomics <$> askEnv

  return
    ( h,
      segmented_h,
      SegHistSlug op num_subhistos subhisto_infos $
        atomicUpdateLocking atomics $ histOp op
    )

prepareAtomicUpdateGlobal ::
  Maybe Locking ->
  [VName] ->
  SegHistSlug ->
  CallKernelGen
    ( Maybe Locking,
      [Imp.TExp Int64] -> InKernelGen ()
    )
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
      let num_locks = 100151
          dims =
            map toInt64Exp $
              shapeDims (histShape (slugOp slug))
                ++ [ tvSize (slugNumSubhistos slug),
                     histWidth (slugOp slug)
                   ]
      locks <-
        sStaticArray "hist_locks" (Space "device") int32 $
          Imp.ArrayZeros num_locks
      let l' = Locking locks 0 1 0 (pure . (`rem` fromIntegral num_locks) . flattenIndex dims)
      return (Just l', f l' (Space "global") dests)

-- | Some kernel bodies are not safe (or efficient) to execute
-- multiple times.
data Passage = MustBeSinglePass | MayBeMultiPass deriving (Eq, Ord)

bodyPassage :: KernelBody GPUMem -> Passage
bodyPassage kbody
  | mempty == consumedInKernelBody (aliasAnalyseKernelBody mempty kbody) =
    MayBeMultiPass
  | otherwise =
    MustBeSinglePass

prepareIntermediateArraysGlobal ::
  Passage ->
  Imp.TExp Int32 ->
  Imp.TExp Int64 ->
  [SegHistSlug] ->
  CallKernelGen
    ( Imp.TExp Int32,
      [[Imp.TExp Int64] -> InKernelGen ()]
    )
prepareIntermediateArraysGlobal passage hist_T hist_N slugs = do
  -- The paper formulae assume there is only one histogram, but in our
  -- implementation there can be multiple that have been horisontally
  -- fused.  We do a bit of trickery with summings and averages to
  -- pretend there is really only one.  For the case of a single
  -- histogram, the actual calculations should be the same as in the
  -- paper.

  -- The sum of all Hs.
  hist_H <- dPrimVE "hist_H" $ sum $ map (toInt64Exp . histWidth . slugOp) slugs

  hist_RF <-
    dPrimVE "hist_RF" $
      sum (map (r64 . toInt64Exp . histRaceFactor . slugOp) slugs)
        / genericLength slugs

  hist_el_size <- dPrimVE "hist_el_size" $ sum $ map slugElAvgSize slugs

  hist_C_max <-
    dPrimVE "hist_C_max" $
      fMin64 (r64 hist_T) $ r64 hist_H / hist_k_ct_min

  hist_M_min <-
    dPrimVE "hist_M_min" $
      sMax32 1 $ sExt32 $ t64 $ r64 hist_T / hist_C_max

  -- Querying L2 cache size is not reliable.  Instead we provide a
  -- tunable knob with a hopefully sane default.
  let hist_L2_def = 4 * 1024 * 1024
  hist_L2 <- dPrim "L2_size" int32
  entry <- askFunction
  -- Equivalent to F_L2*L2 in paper.
  sOp $
    Imp.GetSize
      (tvVar hist_L2)
      (keyWithEntryPoint entry $ nameFromString (pretty (tvVar hist_L2)))
      $ Imp.SizeBespoke (nameFromString "L2_for_histogram") hist_L2_def

  let hist_L2_ln_sz = 16 * 4 -- L2 cache line size approximation
  hist_RACE_exp <-
    dPrimVE "hist_RACE_exp" $
      fMax64 1 $
        (hist_k_RF * hist_RF)
          / (hist_L2_ln_sz / r64 hist_el_size)

  hist_S <- dPrim "hist_S" int32

  -- For sparse histograms (H exceeds N) we only want a single chunk.
  sIf
    (hist_N .<. hist_H)
    (hist_S <-- (1 :: Imp.TExp Int32))
    $ hist_S
      <-- case passage of
        MayBeMultiPass ->
          sExt32 $
            (sExt64 hist_M_min * hist_H * sExt64 hist_el_size)
              `divUp` t64 (hist_F_L2 * r64 (tvExp hist_L2) * hist_RACE_exp)
        MustBeSinglePass ->
          1

  emit $ Imp.DebugPrint "Race expansion factor (RACE^exp)" $ Just $ untyped hist_RACE_exp
  emit $ Imp.DebugPrint "Number of chunks (S)" $ Just $ untyped $ tvExp hist_S

  histograms <-
    snd
      <$> mapAccumLM
        (onOp (tvExp hist_L2) hist_M_min (tvExp hist_S) hist_RACE_exp)
        Nothing
        slugs

  return (tvExp hist_S, histograms)
  where
    hist_k_ct_min = 2 -- Chosen experimentally
    hist_k_RF = 0.75 -- Chosen experimentally
    hist_F_L2 = 0.4 -- Chosen experimentally
    r64 = isF64 . ConvOpExp (SIToFP Int32 Float64) . untyped
    t64 = isInt64 . ConvOpExp (FPToSI Float64 Int64) . untyped

    -- "Average element size" as computed by a formula that also takes
    -- locking into account.
    slugElAvgSize slug@(SegHistSlug op _ _ do_op) =
      case do_op of
        AtomicLocking {} ->
          slugElSize slug `quot` (1 + genericLength (lambdaReturnType (histOp op)))
        _ ->
          slugElSize slug `quot` genericLength (lambdaReturnType (histOp op))

    -- "Average element size" as computed by a formula that also takes
    -- locking into account.
    slugElSize (SegHistSlug op _ _ do_op) =
      case do_op of
        AtomicLocking {} ->
          sExt32 $
            unCount $
              sum $
                map (typeSize . (`arrayOfShape` histShape op)) $
                  Prim int32 : lambdaReturnType (histOp op)
        _ ->
          sExt32 $
            unCount $
              sum $
                map (typeSize . (`arrayOfShape` histShape op)) $
                  lambdaReturnType (histOp op)

    onOp hist_L2 hist_M_min hist_S hist_RACE_exp l slug = do
      let SegHistSlug op num_subhistos subhisto_info do_op = slug
          hist_H = toInt64Exp $ histWidth op

      hist_H_chk <- dPrimVE "hist_H_chk" $ hist_H `divUp` sExt64 hist_S

      emit $ Imp.DebugPrint "Chunk size (H_chk)" $ Just $ untyped hist_H_chk

      hist_k_max <-
        dPrimVE "hist_k_max" $
          fMin64
            (hist_F_L2 * (r64 hist_L2 / r64 (slugElSize slug)) * hist_RACE_exp)
            (r64 hist_N)
            / r64 hist_T

      hist_u <- dPrimVE "hist_u" $
        case do_op of
          AtomicPrim {} -> 2
          _ -> 1

      hist_C <-
        dPrimVE "hist_C" $
          fMin64 (r64 hist_T) $ r64 (hist_u * hist_H_chk) / hist_k_max

      -- Number of subhistograms per result histogram.
      hist_M <- dPrimVE "hist_M" $
        case slugAtomicUpdate slug of
          AtomicPrim {} -> 1
          _ -> sMax32 hist_M_min $ sExt32 $ t64 $ r64 hist_T / hist_C

      emit $ Imp.DebugPrint "Elements/thread in L2 cache (k_max)" $ Just $ untyped hist_k_max
      emit $ Imp.DebugPrint "Multiplication degree (M)" $ Just $ untyped hist_M
      emit $ Imp.DebugPrint "Cooperation level (C)" $ Just $ untyped hist_C

      -- num_subhistos is the variable we use to communicate back.
      num_subhistos <-- sExt64 hist_M

      -- Initialise sub-histograms.
      --
      -- If hist_M is 1, then we just reuse the original
      -- destination.  The idea is to avoid a copy if we are writing a
      -- small number of values into a very large prior histogram.
      dests <- forM (zip (histDest op) subhisto_info) $ \(dest, info) -> do
        dest_mem <- entryArrayLoc <$> lookupArray dest

        sub_mem <-
          fmap memLocName $
            entryArrayLoc
              <$> lookupArray (subhistosArray info)

        let unitHistoCase =
              emit $
                Imp.SetMem sub_mem (memLocName dest_mem) $
                  Space "device"

            multiHistoCase = subhistosAlloc info

        sIf (hist_M .==. 1) unitHistoCase multiHistoCase

        return $ subhistosArray info

      (l', do_op') <- prepareAtomicUpdateGlobal l dests slug

      return (l', do_op')

histKernelGlobalPass ::
  [PatElem GPUMem] ->
  Count NumGroups (Imp.TExp Int64) ->
  Count GroupSize (Imp.TExp Int64) ->
  SegSpace ->
  [SegHistSlug] ->
  KernelBody GPUMem ->
  [[Imp.TExp Int64] -> InKernelGen ()] ->
  Imp.TExp Int32 ->
  Imp.TExp Int32 ->
  CallKernelGen ()
histKernelGlobalPass map_pes num_groups group_size space slugs kbody histograms hist_S chk_i = do
  let (space_is, space_sizes) = unzip $ unSegSpace space
      space_sizes_64 = map (sExt64 . toInt64Exp) space_sizes
      total_w_64 = product space_sizes_64

  hist_H_chks <- forM (map (histWidth . slugOp) slugs) $ \w ->
    dPrimVE "hist_H_chk" $ toInt64Exp w `divUp` sExt64 hist_S

  sKernelThread "seghist_global" num_groups group_size (segFlat space) $ do
    constants <- kernelConstants <$> askEnv

    -- Compute subhistogram index for each thread, per histogram.
    subhisto_inds <- forM slugs $ \slug ->
      dPrimVE "subhisto_ind" $
        kernelGlobalThreadId constants
          `quot` ( kernelNumThreads constants
                     `divUp` sExt32 (tvExp (slugNumSubhistos slug))
                 )

    -- Loop over flat offsets into the input and output.  The
    -- calculation is done with 64-bit integers to avoid overflow,
    -- but the final unflattened segment indexes are 32 bit.
    let gtid = sExt64 $ kernelGlobalThreadId constants
        num_threads = sExt64 $ kernelNumThreads constants
    kernelLoop gtid num_threads total_w_64 $ \offset -> do
      -- Construct segment indices.
      zipWithM_ dPrimV_ space_is $
        map sExt32 $ unflattenIndex space_sizes_64 offset

      -- We execute the bucket function once and update each histogram serially.
      -- We apply the bucket function if j=offset+ltid is less than
      -- num_elements.  This also involves writing to the mapout
      -- arrays.
      let input_in_bounds = offset .<. total_w_64

      sWhen input_in_bounds $
        compileStms mempty (kernelBodyStms kbody) $ do
          let (red_res, map_res) = splitFromEnd (length map_pes) $ kernelBodyResult kbody

          sComment "save map-out results" $
            forM_ (zip map_pes map_res) $ \(pe, res) ->
              copyDWIMFix
                (patElemName pe)
                (map (Imp.le64 . fst) $ unSegSpace space)
                (kernelResultSubExp res)
                []

          let (buckets, vs) = splitAt (length slugs) red_res
              perOp = chunks $ map (length . histDest . slugOp) slugs

          sComment "perform atomic updates" $
            forM_ (zip6 (map slugOp slugs) histograms buckets (perOp vs) subhisto_inds hist_H_chks) $
              \( HistOp dest_w _ _ _ shape lam,
                 do_op,
                 bucket,
                 vs',
                 subhisto_ind,
                 hist_H_chk
                 ) -> do
                  let chk_beg = sExt64 chk_i * hist_H_chk
                      bucket' = toInt64Exp $ kernelResultSubExp bucket
                      dest_w' = toInt64Exp dest_w
                      bucket_in_bounds =
                        chk_beg .<=. bucket'
                          .&&. bucket' .<. (chk_beg + hist_H_chk)
                          .&&. bucket' .<. dest_w'
                      vs_params = takeLast (length vs') $ lambdaParams lam

                  sWhen bucket_in_bounds $ do
                    let bucket_is =
                          map Imp.le64 (init space_is)
                            ++ [sExt64 subhisto_ind, bucket']
                    dLParams $ lambdaParams lam
                    sLoopNest shape $ \is -> do
                      forM_ (zip vs_params vs') $ \(p, res) ->
                        copyDWIMFix (paramName p) [] (kernelResultSubExp res) is
                      do_op (bucket_is ++ is)

histKernelGlobal ::
  [PatElem GPUMem] ->
  Count NumGroups SubExp ->
  Count GroupSize SubExp ->
  SegSpace ->
  [SegHistSlug] ->
  KernelBody GPUMem ->
  CallKernelGen ()
histKernelGlobal map_pes num_groups group_size space slugs kbody = do
  let num_groups' = fmap toInt64Exp num_groups
      group_size' = fmap toInt64Exp group_size
  let (_space_is, space_sizes) = unzip $ unSegSpace space
      num_threads = sExt32 $ unCount num_groups' * unCount group_size'

  emit $ Imp.DebugPrint "## Using global memory" Nothing

  (hist_S, histograms) <-
    prepareIntermediateArraysGlobal
      (bodyPassage kbody)
      num_threads
      (toInt64Exp $ last space_sizes)
      slugs

  sFor "chk_i" hist_S $ \chk_i ->
    histKernelGlobalPass
      map_pes
      num_groups'
      group_size'
      space
      slugs
      kbody
      histograms
      hist_S
      chk_i

type InitLocalHistograms =
  [ ( [VName],
      SubExp ->
      InKernelGen
        ( [VName],
          [Imp.TExp Int64] -> InKernelGen ()
        )
    )
  ]

prepareIntermediateArraysLocal ::
  TV Int32 ->
  Count NumGroups (Imp.TExp Int64) ->
  SegSpace ->
  [SegHistSlug] ->
  CallKernelGen InitLocalHistograms
prepareIntermediateArraysLocal num_subhistos_per_group groups_per_segment space slugs = do
  num_segments <-
    dPrimVE "num_segments" $
      product $ map (toInt64Exp . snd) $ init $ unSegSpace space
  mapM (onOp num_segments) slugs
  where
    onOp num_segments (SegHistSlug op num_subhistos subhisto_info do_op) = do
      num_subhistos <-- sExt64 (unCount groups_per_segment) * num_segments

      emit $
        Imp.DebugPrint "Number of subhistograms in global memory" $
          Just $ untyped $ tvExp num_subhistos

      mk_op <-
        case do_op of
          AtomicPrim f -> return $ const $ return f
          AtomicCAS f -> return $ const $ return f
          AtomicLocking f -> return $ \hist_H_chk -> do
            let lock_shape =
                  Shape $
                    tvSize num_subhistos_per_group :
                    shapeDims (histShape op)
                      ++ [hist_H_chk]

            let dims = map toInt64Exp $ shapeDims lock_shape

            locks <- sAllocArray "locks" int32 lock_shape $ Space "local"

            sComment "All locks start out unlocked" $
              groupCoverSpace dims $ \is ->
                copyDWIMFix locks is (intConst Int32 0) []

            return $ f $ Locking locks 0 1 0 id

      -- Initialise local-memory sub-histograms.  These are
      -- represented as two-dimensional arrays.
      let init_local_subhistos hist_H_chk = do
            local_subhistos <-
              forM (histType op) $ \t -> do
                let sub_local_shape =
                      Shape [tvSize num_subhistos_per_group]
                        <> (arrayShape t `setOuterDim` hist_H_chk)
                sAllocArray
                  "subhistogram_local"
                  (elemType t)
                  sub_local_shape
                  (Space "local")

            do_op' <- mk_op hist_H_chk

            return (local_subhistos, do_op' (Space "local") local_subhistos)

      -- Initialise global-memory sub-histograms.
      glob_subhistos <- forM subhisto_info $ \info -> do
        subhistosAlloc info
        return $ subhistosArray info

      return (glob_subhistos, init_local_subhistos)

histKernelLocalPass ::
  TV Int32 ->
  Count NumGroups (Imp.TExp Int64) ->
  [PatElem GPUMem] ->
  Count NumGroups (Imp.TExp Int64) ->
  Count GroupSize (Imp.TExp Int64) ->
  SegSpace ->
  [SegHistSlug] ->
  KernelBody GPUMem ->
  InitLocalHistograms ->
  Imp.TExp Int32 ->
  Imp.TExp Int32 ->
  CallKernelGen ()
histKernelLocalPass
  num_subhistos_per_group_var
  groups_per_segment
  map_pes
  num_groups
  group_size
  space
  slugs
  kbody
  init_histograms
  hist_S
  chk_i = do
    let (space_is, space_sizes) = unzip $ unSegSpace space
        segment_is = init space_is
        segment_dims = init space_sizes
        (i_in_segment, segment_size) = last $ unSegSpace space
        num_subhistos_per_group = tvExp num_subhistos_per_group_var
        segment_size' = toInt64Exp segment_size

    num_segments <-
      dPrimVE "num_segments" $
        product $ map toInt64Exp segment_dims

    hist_H_chks <- forM (map (histWidth . slugOp) slugs) $ \w ->
      dPrimV "hist_H_chk" $ toInt64Exp w `divUp` sExt64 hist_S

    histo_sizes <- forM (zip slugs hist_H_chks) $ \(slug, hist_H_chk) -> do
      let histo_dims =
            tvExp hist_H_chk :
            map toInt64Exp (shapeDims (histShape (slugOp slug)))
      histo_size <-
        dPrimVE "histo_size" $ product histo_dims
      let group_hists_size =
            sExt64 num_subhistos_per_group * histo_size
      init_per_thread <-
        dPrimVE "init_per_thread" $ sExt32 $ group_hists_size `divUp` unCount group_size
      return (histo_dims, histo_size, init_per_thread)

    sKernelThread "seghist_local" num_groups group_size (segFlat space) $
      virtualiseGroups SegVirt (sExt32 $ unCount groups_per_segment * num_segments) $ \group_id -> do
        constants <- kernelConstants <$> askEnv

        flat_segment_id <- dPrimVE "flat_segment_id" $ group_id `quot` sExt32 (unCount groups_per_segment)
        gid_in_segment <- dPrimVE "gid_in_segment" $ group_id `rem` sExt32 (unCount groups_per_segment)
        -- This pgtid is kind of a "virtualised physical" gtid - not the
        -- same thing as the gtid used for the SegHist itself.
        pgtid_in_segment <-
          dPrimVE "pgtid_in_segment" $
            gid_in_segment * sExt32 (kernelGroupSize constants)
              + kernelLocalThreadId constants
        threads_per_segment <-
          dPrimVE "threads_per_segment" $
            sExt32 $ unCount groups_per_segment * kernelGroupSize constants

        -- Set segment indices.
        zipWithM_ dPrimV_ segment_is $
          unflattenIndex (map toInt64Exp segment_dims) $ sExt64 flat_segment_id

        histograms <- forM (zip init_histograms hist_H_chks) $
          \((glob_subhistos, init_local_subhistos), hist_H_chk) -> do
            (local_subhistos, do_op) <- init_local_subhistos $ Var $ tvVar hist_H_chk
            return (zip glob_subhistos local_subhistos, hist_H_chk, do_op)

        -- Find index of local subhistograms updated by this thread.  We
        -- try to ensure, as much as possible, that threads in the same
        -- warp use different subhistograms, to avoid conflicts.
        thread_local_subhisto_i <-
          dPrimVE "thread_local_subhisto_i" $
            kernelLocalThreadId constants `rem` num_subhistos_per_group

        let onSlugs f =
              forM_ (zip3 slugs histograms histo_sizes) $
                \(slug, (dests, hist_H_chk, _), (histo_dims, histo_size, init_per_thread)) ->
                  f slug dests (tvExp hist_H_chk) histo_dims histo_size init_per_thread

        let onAllHistograms f =
              onSlugs $ \slug dests hist_H_chk histo_dims histo_size init_per_thread -> do
                let group_hists_size = num_subhistos_per_group * sExt32 histo_size

                forM_ (zip dests (histNeutral $ slugOp slug)) $
                  \((dest_global, dest_local), ne) ->
                    sFor "local_i" init_per_thread $ \i -> do
                      j <-
                        dPrimVE "j" $
                          i * sExt32 (kernelGroupSize constants)
                            + kernelLocalThreadId constants
                      j_offset <-
                        dPrimVE "j_offset" $
                          num_subhistos_per_group * sExt32 histo_size * gid_in_segment + j

                      local_subhisto_i <- dPrimVE "local_subhisto_i" $ j `quot` sExt32 histo_size
                      let local_bucket_is = unflattenIndex histo_dims $ sExt64 $ j `rem` sExt32 histo_size
                          global_bucket_is =
                            head local_bucket_is + sExt64 chk_i * hist_H_chk :
                            tail local_bucket_is
                      global_subhisto_i <- dPrimVE "global_subhisto_i" $ j_offset `quot` sExt32 histo_size

                      sWhen (j .<. group_hists_size) $
                        f
                          dest_local
                          dest_global
                          (slugOp slug)
                          ne
                          local_subhisto_i
                          global_subhisto_i
                          local_bucket_is
                          global_bucket_is

        sComment "initialize histograms in local memory" $
          onAllHistograms $ \dest_local dest_global op ne local_subhisto_i global_subhisto_i local_bucket_is global_bucket_is ->
            sComment "First subhistogram is initialised from global memory; others with neutral element." $ do
              let global_is = map Imp.le64 segment_is ++ [0] ++ global_bucket_is
                  local_is = sExt64 local_subhisto_i : local_bucket_is
              sIf
                (global_subhisto_i .==. 0)
                (copyDWIMFix dest_local local_is (Var dest_global) global_is)
                ( sLoopNest (histShape op) $ \is ->
                    copyDWIMFix dest_local (local_is ++ is) ne []
                )

        sOp $ Imp.Barrier Imp.FenceLocal

        kernelLoop pgtid_in_segment threads_per_segment (sExt32 segment_size') $ \ie -> do
          dPrimV_ i_in_segment ie

          -- We execute the bucket function once and update each histogram
          -- serially.  This also involves writing to the mapout arrays if
          -- this is the first chunk.

          compileStms mempty (kernelBodyStms kbody) $ do
            let (red_res, map_res) =
                  splitFromEnd (length map_pes) $
                    map kernelResultSubExp $ kernelBodyResult kbody
                (buckets, vs) = splitAt (length slugs) red_res
                perOp = chunks $ map (length . histDest . slugOp) slugs

            sWhen (chk_i .==. 0) $
              sComment "save map-out results" $
                forM_ (zip map_pes map_res) $ \(pe, se) ->
                  copyDWIMFix
                    (patElemName pe)
                    (map Imp.le64 space_is)
                    se
                    []

            forM_ (zip4 (map slugOp slugs) histograms buckets (perOp vs)) $
              \( HistOp dest_w _ _ _ shape lam,
                 (_, hist_H_chk, do_op),
                 bucket,
                 vs'
                 ) -> do
                  let chk_beg = sExt64 chk_i * tvExp hist_H_chk
                      bucket' = toInt64Exp bucket
                      dest_w' = toInt64Exp dest_w
                      bucket_in_bounds =
                        bucket' .<. dest_w'
                          .&&. chk_beg .<=. bucket'
                          .&&. bucket' .<. (chk_beg + tvExp hist_H_chk)
                      bucket_is = [sExt64 thread_local_subhisto_i, bucket' - chk_beg]
                      vs_params = takeLast (length vs') $ lambdaParams lam

                  sComment "perform atomic updates" $
                    sWhen bucket_in_bounds $ do
                      dLParams $ lambdaParams lam
                      sLoopNest shape $ \is -> do
                        forM_ (zip vs_params vs') $ \(p, v) ->
                          copyDWIMFix (paramName p) [] v is
                        do_op (bucket_is ++ is)

        sOp $ Imp.ErrorSync Imp.FenceGlobal

        sComment "Compact the multiple local memory subhistograms to result in global memory" $
          onSlugs $ \slug dests hist_H_chk histo_dims _histo_size bins_per_thread -> do
            trunc_H <-
              dPrimV "trunc_H" $
                sMin64 hist_H_chk $
                  toInt64Exp (histWidth (slugOp slug))
                    - sExt64 chk_i * head histo_dims
            let trunc_histo_dims =
                  tvExp trunc_H :
                  map toInt64Exp (shapeDims (histShape (slugOp slug)))
            trunc_histo_size <- dPrimVE "histo_size" $ sExt32 $ product trunc_histo_dims

            sFor "local_i" bins_per_thread $ \i -> do
              j <-
                dPrimVE "j" $
                  i * sExt32 (kernelGroupSize constants)
                    + kernelLocalThreadId constants
              sWhen (j .<. trunc_histo_size) $ do
                -- We are responsible for compacting the flat bin 'j', which
                -- we immediately unflatten.
                let local_bucket_is = unflattenIndex histo_dims $ sExt64 j
                    global_bucket_is =
                      head local_bucket_is + sExt64 chk_i * hist_H_chk :
                      tail local_bucket_is
                dLParams $ lambdaParams $ histOp $ slugOp slug
                let (global_dests, local_dests) = unzip dests
                    (xparams, yparams) =
                      splitAt (length local_dests) $
                        lambdaParams $ histOp $ slugOp slug

                sComment "Read values from subhistogram 0." $
                  forM_ (zip xparams local_dests) $ \(xp, subhisto) ->
                    copyDWIMFix
                      (paramName xp)
                      []
                      (Var subhisto)
                      (0 : local_bucket_is)

                sComment "Accumulate based on values in other subhistograms." $
                  sFor "subhisto_id" (num_subhistos_per_group - 1) $ \subhisto_id -> do
                    forM_ (zip yparams local_dests) $ \(yp, subhisto) ->
                      copyDWIMFix
                        (paramName yp)
                        []
                        (Var subhisto)
                        (sExt64 subhisto_id + 1 : local_bucket_is)
                    compileBody' xparams $ lambdaBody $ histOp $ slugOp slug

                sComment "Put final bucket value in global memory." $ do
                  let global_is =
                        map Imp.le64 segment_is
                          ++ [sExt64 group_id `rem` unCount groups_per_segment]
                          ++ global_bucket_is
                  forM_ (zip xparams global_dests) $ \(xp, global_dest) ->
                    copyDWIMFix global_dest global_is (Var $ paramName xp) []

histKernelLocal ::
  TV Int32 ->
  Count NumGroups (Imp.TExp Int64) ->
  [PatElem GPUMem] ->
  Count NumGroups SubExp ->
  Count GroupSize SubExp ->
  SegSpace ->
  Imp.TExp Int32 ->
  [SegHistSlug] ->
  KernelBody GPUMem ->
  CallKernelGen ()
histKernelLocal num_subhistos_per_group_var groups_per_segment map_pes num_groups group_size space hist_S slugs kbody = do
  let num_groups' = fmap toInt64Exp num_groups
      group_size' = fmap toInt64Exp group_size
      num_subhistos_per_group = tvExp num_subhistos_per_group_var

  emit $
    Imp.DebugPrint "Number of local subhistograms per group" $
      Just $ untyped num_subhistos_per_group

  init_histograms <-
    prepareIntermediateArraysLocal num_subhistos_per_group_var groups_per_segment space slugs

  sFor "chk_i" hist_S $ \chk_i ->
    histKernelLocalPass
      num_subhistos_per_group_var
      groups_per_segment
      map_pes
      num_groups'
      group_size'
      space
      slugs
      kbody
      init_histograms
      hist_S
      chk_i

-- | The maximum number of passes we are willing to accept for this
-- kind of atomic update.
slugMaxLocalMemPasses :: SegHistSlug -> Int
slugMaxLocalMemPasses slug =
  case slugAtomicUpdate slug of
    AtomicPrim _ -> 3
    AtomicCAS _ -> 4
    AtomicLocking _ -> 6

localMemoryCase ::
  [PatElem GPUMem] ->
  Imp.TExp Int32 ->
  SegSpace ->
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  Imp.TExp Int32 ->
  [SegHistSlug] ->
  KernelBody GPUMem ->
  CallKernelGen (Imp.TExp Bool, CallKernelGen ())
localMemoryCase map_pes hist_T space hist_H hist_el_size hist_N _ slugs kbody = do
  let space_sizes = segSpaceDims space
      segment_dims = init space_sizes
      segmented = not $ null segment_dims

  hist_L <- dPrim "hist_L" int32
  sOp $ Imp.GetSizeMax (tvVar hist_L) Imp.SizeLocalMemory

  max_group_size <- dPrim "max_group_size" int32
  sOp $ Imp.GetSizeMax (tvVar max_group_size) Imp.SizeGroup
  let group_size = Imp.Count $ Var $ tvVar max_group_size
  num_groups <-
    fmap (Imp.Count . tvSize) $
      dPrimV "num_groups" $
        sExt64 hist_T `divUp` toInt64Exp (unCount group_size)
  let num_groups' = toInt64Exp <$> num_groups
      group_size' = toInt64Exp <$> group_size

  let r64 = isF64 . ConvOpExp (SIToFP Int64 Float64) . untyped
      t64 = isInt64 . ConvOpExp (FPToSI Float64 Int64) . untyped

  -- M approximation.
  hist_m' <-
    dPrimVE "hist_m_prime" $
      r64
        ( sMin64
            (sExt64 (tvExp hist_L `quot` hist_el_size))
            (hist_N `divUp` sExt64 (unCount num_groups'))
        )
        / r64 hist_H

  let hist_B = unCount group_size'

  -- M in the paper, but not adjusted for asymptotic efficiency.
  hist_M0 <-
    dPrimVE "hist_M0" $
      sMax64 1 $ sMin64 (t64 hist_m') hist_B

  -- Minimal sequential chunking factor.
  let q_small = 2

  -- The number of segments/histograms produced..
  hist_Nout <- dPrimVE "hist_Nout" $ product $ map toInt64Exp segment_dims

  hist_Nin <- dPrimVE "hist_Nin" $ toInt64Exp $ last space_sizes

  -- Maximum M for work efficiency.
  work_asymp_M_max <-
    if segmented
      then do
        hist_T_hist_min <-
          dPrimVE "hist_T_hist_min" $
            sExt32 $
              sMin64 (sExt64 hist_Nin * sExt64 hist_Nout) (sExt64 hist_T)
                `divUp` sExt64 hist_Nout

        -- Number of groups, rounded up.
        let r = hist_T_hist_min `divUp` sExt32 hist_B

        dPrimVE "work_asymp_M_max" $ hist_Nin `quot` (sExt64 r * hist_H)
      else
        dPrimVE "work_asymp_M_max" $
          (hist_Nout * hist_N)
            `quot` ( (q_small * unCount num_groups' * hist_H)
                       `quot` genericLength slugs
                   )

  -- Number of subhistograms per result histogram.
  hist_M <- dPrimV "hist_M" $ sExt32 $ sMin64 hist_M0 work_asymp_M_max

  -- hist_M may be zero (which we'll check for below), but we need it
  -- for some divisions first, so crudely make a nonzero form.
  let hist_M_nonzero = sMax32 1 $ tvExp hist_M

  -- "Cooperation factor" - the number of threads cooperatively
  -- working on the same (sub)histogram.
  hist_C <-
    dPrimVE "hist_C" $
      hist_B `divUp` sExt64 hist_M_nonzero

  emit $ Imp.DebugPrint "local hist_M0" $ Just $ untyped hist_M0
  emit $ Imp.DebugPrint "local work asymp M max" $ Just $ untyped work_asymp_M_max
  emit $ Imp.DebugPrint "local C" $ Just $ untyped hist_C
  emit $ Imp.DebugPrint "local B" $ Just $ untyped hist_B
  emit $ Imp.DebugPrint "local M" $ Just $ untyped $ tvExp hist_M
  emit $
    Imp.DebugPrint "local memory needed" $
      Just $ untyped $ hist_H * hist_el_size * sExt64 (tvExp hist_M)

  -- local_mem_needed is what we need to keep a single bucket in local
  -- memory - this is an absolute minimum.  We can fit anything else
  -- by doing multiple passes, although more than a few is
  -- (heuristically) not efficient.
  local_mem_needed <-
    dPrimVE "local_mem_needed" $
      hist_el_size * sExt64 (tvExp hist_M)
  hist_S <-
    dPrimVE "hist_S" $
      sExt32 $
        (hist_H * local_mem_needed) `divUp` tvExp hist_L
  let max_S = case bodyPassage kbody of
        MustBeSinglePass -> 1
        MayBeMultiPass -> fromIntegral $ maxinum $ map slugMaxLocalMemPasses slugs

  groups_per_segment <-
    if segmented
      then
        fmap Count $
          dPrimVE "groups_per_segment" $ unCount num_groups' `divUp` hist_Nout
      else pure num_groups'

  -- We only use local memory if the number of updates per histogram
  -- at least matches the histogram size, as otherwise it is not
  -- asymptotically efficient.  This mostly matters for the segmented
  -- case.
  let pick_local =
        hist_Nin .>=. hist_H
          .&&. (local_mem_needed .<=. tvExp hist_L)
          .&&. (hist_S .<=. max_S)
          .&&. hist_C .<=. hist_B
          .&&. tvExp hist_M .>. 0

      run = do
        emit $ Imp.DebugPrint "## Using local memory" Nothing
        emit $ Imp.DebugPrint "Histogram size (H)" $ Just $ untyped hist_H
        emit $ Imp.DebugPrint "Multiplication degree (M)" $ Just $ untyped $ tvExp hist_M
        emit $ Imp.DebugPrint "Cooperation level (C)" $ Just $ untyped hist_C
        emit $ Imp.DebugPrint "Number of chunks (S)" $ Just $ untyped hist_S
        when segmented $
          emit $ Imp.DebugPrint "Groups per segment" $ Just $ untyped $ unCount groups_per_segment
        histKernelLocal
          hist_M
          groups_per_segment
          map_pes
          num_groups
          group_size
          space
          hist_S
          slugs
          kbody

  return (pick_local, run)

-- | Generate code for a segmented histogram called from the host.
compileSegHist ::
  Pat GPUMem ->
  Count NumGroups SubExp ->
  Count GroupSize SubExp ->
  SegSpace ->
  [HistOp GPUMem] ->
  KernelBody GPUMem ->
  CallKernelGen ()
compileSegHist (Pat pes) num_groups group_size space ops kbody = do
  -- Most of this function is not the histogram part itself, but
  -- rather figuring out whether to use a local or global memory
  -- strategy, as well as collapsing the subhistograms produced (which
  -- are always in global memory, but their number may vary).
  let num_groups' = fmap toInt64Exp num_groups
      group_size' = fmap toInt64Exp group_size
      dims = map toInt64Exp $ segSpaceDims space

      num_red_res = length ops + sum (map (length . histNeutral) ops)
      (all_red_pes, map_pes) = splitAt num_red_res pes
      segment_size = last dims

  (op_hs, op_seg_hs, slugs) <- unzip3 <$> mapM (computeHistoUsage space) ops
  h <- dPrimVE "h" $ Imp.unCount $ sum op_hs
  seg_h <- dPrimVE "seg_h" $ Imp.unCount $ sum op_seg_hs

  -- Check for emptyness to avoid division-by-zero.
  sUnless (seg_h .==. 0) $ do
    -- Maximum group size (or actual, in this case).
    let hist_B = unCount group_size'

    -- Size of a histogram.
    hist_H <- dPrimVE "hist_H" $ sum $ map (toInt64Exp . histWidth) ops

    -- Size of a single histogram element.  Actually the weighted
    -- average of histogram elements in cases where we have more than
    -- one histogram operation, plus any locks.
    let lockSize slug = case slugAtomicUpdate slug of
          AtomicLocking {} -> Just $ primByteSize int32
          _ -> Nothing
    hist_el_size <-
      dPrimVE "hist_el_size" $
        foldl' (+) (h `divUp` hist_H) $
          mapMaybe lockSize slugs

    -- Input elements contributing to each histogram.
    hist_N <- dPrimVE "hist_N" segment_size

    -- Compute RF as the average RF over all the histograms.
    hist_RF <-
      dPrimVE "hist_RF" $
        sExt32 $
          sum (map (toInt64Exp . histRaceFactor . slugOp) slugs)
            `quot` genericLength slugs

    let hist_T = sExt32 $ unCount num_groups' * unCount group_size'
    emit $ Imp.DebugPrint "\n# SegHist" Nothing
    emit $ Imp.DebugPrint "Number of threads (T)" $ Just $ untyped hist_T
    emit $ Imp.DebugPrint "Desired group size (B)" $ Just $ untyped hist_B
    emit $ Imp.DebugPrint "Histogram size (H)" $ Just $ untyped hist_H
    emit $ Imp.DebugPrint "Input elements per histogram (N)" $ Just $ untyped hist_N
    emit $
      Imp.DebugPrint "Number of segments" $
        Just $ untyped $ product $ map (toInt64Exp . snd) segment_dims
    emit $ Imp.DebugPrint "Histogram element size (el_size)" $ Just $ untyped hist_el_size
    emit $ Imp.DebugPrint "Race factor (RF)" $ Just $ untyped hist_RF
    emit $ Imp.DebugPrint "Memory per set of subhistograms per segment" $ Just $ untyped h
    emit $ Imp.DebugPrint "Memory per set of subhistograms times segments" $ Just $ untyped seg_h

    (use_local_memory, run_in_local_memory) <-
      localMemoryCase map_pes hist_T space hist_H hist_el_size hist_N hist_RF slugs kbody

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
              pe_mem <-
                memLocName . entryArrayLoc
                  <$> lookupArray (patElemName pe)
              subhisto_mem <-
                memLocName . entryArrayLoc
                  <$> lookupArray subhisto
              emit $ Imp.SetMem pe_mem subhisto_mem $ Space "device"

      sIf (tvExp num_histos .==. 1) unitHistoCase $ do
        -- For the segmented reduction, we keep the segment dimensions
        -- unchanged.  To this, we add two dimensions: one over the number
        -- of buckets, and one over the number of subhistograms.  This
        -- inner dimension is the one that is collapsed in the reduction.
        let num_buckets = histWidth op

        bucket_id <- newVName "bucket_id"
        subhistogram_id <- newVName "subhistogram_id"
        vector_ids <-
          mapM (const $ newVName "vector_id") $
            shapeDims $ histShape op

        flat_gtid <- newVName "flat_gtid"

        let lvl = SegThread num_groups group_size SegVirt
            segred_space =
              SegSpace flat_gtid $
                segment_dims
                  ++ [(bucket_id, num_buckets)]
                  ++ zip vector_ids (shapeDims $ histShape op)
                  ++ [(subhistogram_id, Var $ tvVar num_histos)]

        let segred_op = SegBinOp Commutative (histOp op) (histNeutral op) mempty
        compileSegRed' (Pat red_pes) lvl segred_space [segred_op] $ \red_cont ->
          red_cont $
            flip map subhistos $ \subhisto ->
              ( Var subhisto,
                map Imp.le64 $
                  map fst segment_dims ++ [subhistogram_id, bucket_id] ++ vector_ids
              )

  emit $ Imp.DebugPrint "" Nothing
  where
    segment_dims = init $ unSegSpace space
