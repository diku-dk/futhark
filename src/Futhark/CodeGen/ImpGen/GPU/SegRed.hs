{-# LANGUAGE TypeFamilies #-}

-- | We generate code for non-segmented/single-segment SegRed using
-- the basic approach outlined in the paper "Design and GPGPU
-- Performance of Futhark’s Redomap Construct" (ARRAY '16).  The main
-- deviations are:
--
-- * While we still use two-phase reduction, we use only a single
--   kernel, with the final workgroup to write a result (tracked via
--   an atomic counter) performing the final reduction as well.
--
-- * Instead of depending on storage layout transformations to handle
--   non-commutative reductions efficiently, we slide a
--   @groupsize@-sized window over the input, and perform a parallel
--   reduction for each window.  This sacrifices the notion of
--   efficient sequentialisation, but is sometimes faster and
--   definitely simpler and more predictable (and uses less auxiliary
--   storage).
--
-- For segmented reductions we use the approach from "Strategies for
-- Regular Segmented Reductions on GPU" (FHPC '17).  This involves
-- having two different strategies, and dynamically deciding which one
-- to use based on the number of segments and segment size. We use the
-- (static) @group_size@ to decide which of the following two
-- strategies to choose:
--
-- * Large: uses one or more groups to process a single segment. If
--   multiple groups are used per segment, the intermediate reduction
--   results must be recursively reduced, until there is only a single
--   value per segment.
--
--   Each thread /can/ read multiple elements, which will greatly
--   increase performance; however, if the reduction is
--   non-commutative we will have to use a less efficient traversal
--   (with interim group-wide reductions) to enable coalesced memory
--   accesses, just as in the non-segmented case.
--
-- * Small: is used to let each group process *multiple* segments
--   within a group. We will only use this approach when we can
--   process at least two segments within a single group. In those
--   cases, we would allocate a /whole/ group per segment with the
--   large strategy, but at most 50% of the threads in the group would
--   have any element to read, which becomes highly inefficient.
--
-- TODO: add description of the sequentialization optimization for
-- non-commutative prim-parameterized nonsegmented and large segments
-- reductions.
module Futhark.CodeGen.ImpGen.GPU.SegRed
  ( compileSegRed,
    compileSegRed',
    DoSegBody,
  )
where

import Control.Monad
import Data.List (genericLength, zip4)
import Data.Maybe
import Debug.Trace
import Futhark.CodeGen.ImpCode.GPU qualified as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.GPU.Base
import Futhark.Error
import Futhark.IR.GPUMem
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.Transform.Rename
import Futhark.Util (chunks, mapAccumLM)
import Futhark.Util.IntegralExp (divUp, nextMul, quot, rem)
import Prelude hiding (quot, rem)

forM2_ :: (Monad m) => [a] -> [b] -> (a -> b -> m c) -> m ()
forM2_ xs ys f = forM (zip xs ys) (uncurry f) >> pure ()

-- | The maximum number of operators we support in a single SegRed.
-- This limit arises out of the static allocation of counters.
maxNumOps :: Int
maxNumOps = 10

-- | Code generation for the body of the SegRed, taking a continuation
-- for saving the results of the body.  The results should be
-- represented as a pairing of a t'SubExp' along with a list of
-- indexes into that t'SubExp' for reading the result.
type DoSegBody = ([(SubExp, [Imp.TExp Int64])] -> InKernelGen ()) -> InKernelGen ()

-- | Datatype used to distinguish between the different types of reductions we
-- generate code for. Actually, we only distinguish between non-commutative
-- reductions with all-Prim parameters, and all other reductions. The former
-- requires a different (and larger) set of intermediate arrays (in local mem)
-- and a more elaborate stage one loop body.
--
-- TODO (1): ideally this choice would be made implicitly throughout the module,
-- somehow. Perhaps Troels has a good idea on how to do this elegantly (?).
data SegredKind = NoncommPrimSegred | GeneralSegred

-- | Datatype used to distinguish between and work with the different sets of
-- intermediate memory we need for the different ReduceKinds.
data SegRedIntermediateArrays
  = GeneralSegRedInterms
      { groupRedArrs :: [VName]
      }
  | NoncommPrimSegRedInterms
      { collCopyArrs :: [VName],
        groupRedArrs :: [VName],
        privateChunks :: [VName]
      }

-- | Compile 'SegRed' instance to host-level code with calls to
-- various kernels.
compileSegRed ::
  Pat LetDecMem ->
  SegLevel ->
  SegSpace ->
  [SegBinOp GPUMem] ->
  KernelBody GPUMem ->
  CallKernelGen ()
compileSegRed pat lvl space segbinops map_kbody = do
  emit $ Imp.DebugPrint "\n# SegRed" Nothing
  KernelAttrs _ _ num_groups group_size <- lvlKernelAttrs lvl
  let grid = KernelGrid num_groups group_size

  compileSegRed' pat grid space segbinops $ \red_cont ->
    sComment "apply map function" $
      compileStms mempty (kernelBodyStms map_kbody) $ do
        let (red_res, map_res) = splitAt (segBinOpResults segbinops) $ kernelBodyResult map_kbody

        let mapout_arrs = drop (segBinOpResults segbinops) $ patElems pat
        when (not $ null mapout_arrs) $
          sComment "write map-out result(s)" $ do
            zipWithM_ (compileThreadResult space) mapout_arrs map_res

        red_cont $ map ((,[]) . kernelResultSubExp) red_res
  emit $ Imp.DebugPrint "" Nothing

paramOf :: SegBinOp GPUMem -> [Param LParamMem]
paramOf (SegBinOp _ op ne _) = take (length ne) $ lambdaParams op

-- | Like 'compileSegRed', but where the body is a monadic action.
compileSegRed' ::
  Pat LetDecMem ->
  KernelGrid ->
  SegSpace ->
  [SegBinOp GPUMem] ->
  DoSegBody ->
  CallKernelGen ()
compileSegRed' pat grid space segbinops map_body_cont
  | genericLength segbinops > maxNumOps =
      compilerLimitationS $
        "compileSegRed': at most " ++ show maxNumOps ++ " reduction operators are supported."
  | (Constant (IntValue (Int64Value 1)) : _) <- segSpaceDims space =
      compileReduction nonsegmentedReduction
  | otherwise = do
      traceM $ "segspacedims: " ++ prettyString (segSpaceDims space)
      let segment_size = pe64 $ last $ segSpaceDims space
          use_small_segments = segment_size * 2 .<. group_size_E * chunk_E
      sIf
        use_small_segments
        (compileReduction smallSegmentsReduction)
        (compileReduction largeSegmentsReduction)
  where
    compileReduction f =
      f segred_kind pat num_groups group_size chunk space segbinops map_body_cont

    chunk
      | NoncommPrimSegred <- segred_kind =
          intConst Int64 $ getChunkSize param_types
      | otherwise = intConst Int64 1

    segred_kind
      | mconcat (map segBinOpComm segbinops) == Noncommutative,
        all primType param_types =
          NoncommPrimSegred
      | otherwise = GeneralSegred

    param_types = map paramType $ concatMap paramOf segbinops

    num_groups = gridNumGroups grid
    group_size = gridGroupSize grid
    group_size_E = pe64 $ unCount group_size
    chunk_E = pe64 chunk

-- | Prepare intermediate arrays for the reduction.  Prim-typed
-- arguments go in local memory (so we need to do the allocation of
-- those arrays inside the kernel), while array-typed arguments go in
-- global memory.  Allocations for the latter have already been
-- performed.  This policy is baked into how the allocations are done
-- in ExplicitAllocations.
--
-- For more info about the intermediate arrays used for the different reduction
-- kernels, see note [IntermArrays].
makeIntermArrays ::
  SegredKind ->
  SubExp ->
  SubExp ->
  SubExp ->
  [[Param LParamMem]] ->
  InKernelGen [SegRedIntermediateArrays]
makeIntermArrays NoncommPrimSegred group_size chunk _ params = do
  group_worksize <- tvSize <$> (dPrimV "group_worksize" group_worksize_E)

  -- compute total amount of lmem needed for the group reduction arrays as well
  -- as the offset into the total pool of lmem of each such array.

  -- TODO: we essentially compute `group_reds_lmem_requirement` twice, since it
  -- is also contained in the computation of `offsets`. However, trying to
  -- extract it, we get an "unknown variable" error, since the variable holding
  -- the lmem size exists only in kernel scope but needs to be accessed from
  -- host scope.
  -- Alternatively, we can compute it without the use of `dPrimVE` inside the
  -- below `forAccum`, but this means the individual offsets are not bound to
  -- names, which absolutely clutters the generated code.

  let sum_ x y = nextMul x y + group_size_E * y
      group_reds_lmem_requirement = foldl sum_ 0 $ concat elem_sizes

  (_, offsets) <-
    forAccumLM2D 0 elem_sizes $ \byte_offs elem_size ->
      (,byte_offs `quot` elem_size)
        <$> (dPrimVE "offset" $ sum_ byte_offs elem_size)

  let collcopy_lmem_requirement = group_worksize_E * max_elem_size
      lmem_total_size =
        Imp.bytes $
          collcopy_lmem_requirement `sMax64` group_reds_lmem_requirement

  -- total pool of local mem.
  lmem <- sAlloc "local_mem" lmem_total_size (Space "local")

  let arrInLMem ptype name len_se offset = do
        sArray
          (name ++ "_" ++ prettyString ptype)
          ptype
          (Shape [len_se])
          lmem
          $ LMAD.iota offset [pe64 len_se]

  forM (zipWith zip params offsets) $ \ps_and_offsets -> do
    (coll_copy_arrs, group_red_arrs, priv_chunks) <-
      fmap unzip3 $ forM ps_and_offsets $ \(p, offset) -> do
        let ptype = elemType $ paramType p
        (,,)
          <$> arrInLMem ptype "coll_copy_arr" group_worksize 0
          <*> arrInLMem ptype "group_red_arr" group_size offset
          <*> sAllocArray
            ("chunk_" ++ prettyString ptype)
            ptype
            (Shape [chunk])
            (ScalarSpace [chunk] ptype)
    pure $ NoncommPrimSegRedInterms coll_copy_arrs group_red_arrs priv_chunks
  where
    group_size_E = pe64 group_size
    group_worksize_E = group_size_E * pe64 chunk

    paramSize = (primByteSize . elemType . paramType)
    elem_sizes = map (map paramSize) params
    max_elem_size = maximum $ concat elem_sizes

    forAccumLM2D acc ls f = mapAccumLM (mapAccumLM f) acc ls
makeIntermArrays _ group_size _ num_threads params =
  fmap (map GeneralSegRedInterms) $
    forM params $
      mapM $ \p ->
        case paramDec p of
          MemArray pt shape _ (ArrayIn mem _) -> do
            let shape' = Shape [num_threads] <> shape
            sArray "red_arr" pt shape' mem $
              LMAD.iota 0 (map pe64 $ shapeDims shape')
          _ -> do
            let pt = elemType $ paramType p
                shape = Shape [group_size]
            sAllocArray ("red_arr_" ++ prettyString pt) pt shape $ Space "local"

-- | Arrays for storing group results.
--
-- The group-result arrays have an extra dimension because they are
-- also used for keeping vectorised accumulators for first-stage
-- reduction, if necessary.  If necessary, this dimension has size
-- group_size, and otherwise 1.  When actually storing group results,
-- the first index is set to 0.
groupResultArrays ::
  SubExp ->
  SubExp ->
  [SegBinOp GPUMem] ->
  CallKernelGen [[VName]]
groupResultArrays num_virtgroups group_size segbinops =
  forM segbinops $ \(SegBinOp _ lam _ shape) ->
    forM (lambdaReturnType lam) $ \t -> do
      let pt = elemType t
          extra_dim
            | primType t, shapeRank shape == 0 = intConst Int64 1
            | otherwise = group_size
          full_shape = Shape [extra_dim, num_virtgroups] <> shape <> arrayShape t
          -- Move the groupsize dimension last to ensure coalesced
          -- memory access.
          perm = [1 .. shapeRank full_shape - 1] ++ [0]
      sAllocArrayPerm "segred_tmp" pt full_shape (Space "device") perm

type DoCompileSegRed =
  SegredKind ->
  Pat LetDecMem ->
  Count NumGroups SubExp ->
  Count GroupSize SubExp ->
  SubExp ->
  SegSpace ->
  [SegBinOp GPUMem] ->
  DoSegBody ->
  CallKernelGen ()

nonsegmentedReduction :: DoCompileSegRed
nonsegmentedReduction segred_kind (Pat segred_pes) num_groups group_size chunk_se space segbinops map_body_cont = do
  let (gtids, dims) = unzip $ unSegSpace space
      chunk = pe64 chunk_se
      num_groups_se = unCount num_groups
      group_size_se = unCount group_size
      group_size' = pe64 group_size_se
      global_tid = Imp.le64 $ segFlat space
      n = pe64 $ last dims
      params = map paramOf segbinops

  counters <- genZeroes "counters" maxNumOps

  reds_group_res_arrs <- groupResultArrays num_groups_se group_size_se segbinops

  num_threads <-
    fmap tvSize $ dPrimV "num_threads" $ pe64 num_groups_se * group_size'

  sKernelThread "segred_nonseg" (segFlat space) (defKernelAttrs num_groups group_size) $ do
    constants <- kernelConstants <$> askEnv
    let ltid = kernelLocalThreadId constants
    let group_id = kernelGroupId constants

    interms <- makeIntermArrays segred_kind group_size_se chunk_se num_threads params
    sync_arr <- sAllocArray "sync_arr" Bool (Shape [intConst Int32 1]) $ Space "local"

    -- Since this is the nonsegmented case, all outer segment IDs must
    -- necessarily be 0.
    forM_ gtids $ \v -> dPrimV_ v (0 :: Imp.TExp Int64)

    q <- dPrimVE "q" $ n `divUp` (sExt64 (kernelNumThreads constants) * chunk)

    slugs <-
      mapM (segBinOpSlug ltid group_id) $
        zip3 segbinops interms reds_group_res_arrs
    new_lambdas <-
      reductionStageOne
        segred_kind
        gtids
        n
        global_tid
        q
        chunk
        (pe64 num_threads)
        slugs
        map_body_cont

    let segred_pess =
          chunks (map (length . segBinOpNeutral) segbinops) $
            segred_pes
    forM_ (zip4 segred_pess slugs new_lambdas [0 ..]) $
      \(pes, slug, new_lambda, i) ->
        reductionStageTwo
          pes
          group_id
          [0]
          0
          (sExt64 $ kernelNumGroups constants)
          slug
          new_lambda
          counters
          sync_arr
          (fromInteger i)

smallSegmentsReduction :: DoCompileSegRed
smallSegmentsReduction _ (Pat segred_pes) num_groups group_size _ space segbinops map_body_cont = do
  let (gtids, dims) = unzip $ unSegSpace space
      dims' = map pe64 dims
      segment_size = last dims'
      params = map paramOf segbinops

  -- Careful to avoid division by zero now.
  segment_size_nonzero <-
    dPrimVE "segment_size_nonzero" $ sMax64 1 segment_size

  let group_size_se = unCount group_size
      num_groups_se = unCount group_size
      num_groups' = pe64 num_groups_se
      group_size' = pe64 group_size_se
  num_threads <- fmap tvSize $ dPrimV "num_threads" $ num_groups' * group_size'
  let num_segments = product $ init dims'
      segments_per_group = group_size' `quot` segment_size_nonzero
      required_groups = sExt32 $ num_segments `divUp` segments_per_group

  emit $ Imp.DebugPrint "# SegRed-small" Nothing
  emit $ Imp.DebugPrint "num_segments" $ Just $ untyped num_segments
  emit $ Imp.DebugPrint "segment_size" $ Just $ untyped segment_size
  emit $ Imp.DebugPrint "segments_per_group" $ Just $ untyped segments_per_group
  emit $ Imp.DebugPrint "required_groups" $ Just $ untyped required_groups

  sKernelThread "segred_small" (segFlat space) (defKernelAttrs num_groups group_size) $ do
    constants <- kernelConstants <$> askEnv

    interms <- makeIntermArrays GeneralSegred group_size_se undefined num_threads params
    let reds_arrs = map groupRedArrs interms

    -- We probably do not have enough actual workgroups to cover the
    -- entire iteration space.  Some groups thus have to perform double
    -- duty; we put an outer loop to accomplish this.
    virtualiseGroups SegVirt required_groups $ \virt_group_id -> do
      -- Compute the 'n' input indices.  The outer 'n-1' correspond to
      -- the segment ID, and are computed from the group id.  The inner
      -- is computed from the local thread id, and may be out-of-bounds.
      let ltid = sExt64 $ kernelLocalThreadId constants
          segment_index =
            (ltid `quot` segment_size_nonzero)
              + (sExt64 virt_group_id * sExt64 segments_per_group)
          index_within_segment = ltid `rem` segment_size

      dIndexSpace (zip (init gtids) (init dims')) segment_index
      dPrimV_ (last gtids) index_within_segment

      let in_bounds =
            map_body_cont $ \red_res ->
              sComment "save results to be reduced" $ do
                let red_dests = map (,[ltid]) (concat reds_arrs)
                forM2_ red_dests red_res $ \(d, d_is) (res, res_is) ->
                  copyDWIMFix d d_is res res_is
          out_of_bounds =
            forM2_ segbinops reds_arrs $ \(SegBinOp _ _ nes _) red_arrs ->
              forM2_ red_arrs nes $ \arr ne ->
                copyDWIMFix arr [ltid] ne []

      sComment "apply map function if in bounds" $
        sIf
          ( segment_size
              .>. 0
              .&&. isActive (init $ zip gtids dims)
              .&&. ltid
              .<. segment_size
              * segments_per_group
          )
          in_bounds
          out_of_bounds

      sOp $ Imp.ErrorSync Imp.FenceLocal -- Also implicitly barrier.
      let crossesSegment from to =
            (sExt64 to - sExt64 from) .>. (sExt64 to `rem` segment_size)
      sWhen (segment_size .>. 0) $
        sComment "perform segmented scan to imitate reduction" $
          forM2_ segbinops reds_arrs $ \(SegBinOp _ red_op _ _) red_arrs ->
            groupScan
              (Just crossesSegment)
              (sExt64 $ pe64 num_threads)
              (segment_size * segments_per_group)
              red_op
              red_arrs

      sOp $ Imp.Barrier Imp.FenceLocal

      sComment "save final values of segments"
        $ sWhen
          ( sExt64 virt_group_id
              * segments_per_group
              + sExt64 ltid
                .<. num_segments
                .&&. ltid
                .<. segments_per_group
          )
        $ forM2_ segred_pes (concat reds_arrs)
        $ \pe arr -> do
          -- Figure out which segment result this thread should write...
          let flat_segment_index =
                sExt64 virt_group_id * segments_per_group + sExt64 ltid
              gtids' =
                unflattenIndex (init dims') flat_segment_index
          copyDWIMFix
            (patElemName pe)
            gtids'
            (Var arr)
            [(ltid + 1) * segment_size_nonzero - 1]

      -- Finally another barrier, because we will be writing to the
      -- local memory array first thing in the next iteration.
      sOp $ Imp.Barrier Imp.FenceLocal

largeSegmentsReduction :: DoCompileSegRed
largeSegmentsReduction segred_kind (Pat segred_pes) num_groups group_size chunk_se space segbinops map_body_cont = do
  let (gtids, dims) = unzip $ unSegSpace space
      dims' = map pe64 dims
      num_segments = product $ init dims'
      segment_size = last dims'
      num_groups' = pe64 $ unCount num_groups
      group_size_se = unCount group_size
      group_size' = pe64 group_size_se
      chunk = pe64 chunk_se
      params = map paramOf segbinops

  num_threads <-
    dPrimV "num_threads_total" $
      group_size' * num_groups'

  groups_per_segment <-
    dPrimVE "groups_per_segment" $
      num_groups' `divUp` sMax64 1 num_segments

  q <-
    dPrimVE "q" $
      segment_size `divUp` (group_size' * groups_per_segment * chunk)

  num_virtgroups <-
    dPrimV "num_virtgroups" $
      groups_per_segment * num_segments
  threads_per_segment <-
    dPrimVE "threads_per_segment" $
      groups_per_segment * group_size'

  emit $ Imp.DebugPrint "# SegRed-large" Nothing
  emit $ Imp.DebugPrint "num_segments" $ Just $ untyped num_segments
  emit $ Imp.DebugPrint "segment_size" $ Just $ untyped segment_size
  emit $ Imp.DebugPrint "num_virtgroups" $ Just $ untyped $ tvExp num_virtgroups
  emit $ Imp.DebugPrint "num_groups" $ Just $ untyped num_groups'
  emit $ Imp.DebugPrint "group_size" $ Just $ untyped group_size'
  emit $ Imp.DebugPrint "q" $ Just $ untyped q
  emit $ Imp.DebugPrint "groups_per_segment" $ Just $ untyped groups_per_segment

  reds_group_res_arrs <- groupResultArrays (tvSize num_virtgroups) group_size_se segbinops

  -- In principle we should have a counter for every segment.  Since
  -- the number of segments is a dynamic quantity, we would have to
  -- allocate and zero out an array here, which is expensive.
  -- However, we exploit the fact that the number of segments being
  -- reduced at any point in time is limited by the number of
  -- workgroups. If we bound the number of workgroups, we can get away
  -- with using that many counters.  FIXME: Is this limit checked
  -- anywhere?  There are other places in the compiler that will fail
  -- if the group count exceeds the maximum group size, which is at
  -- most 1024 anyway.
  let num_counters = maxNumOps * 1024
  counters <- genZeroes "counters" $ fromIntegral num_counters

  sKernelThread "segred_large" (segFlat space) (defKernelAttrs num_groups group_size) $ do
    constants <- kernelConstants <$> askEnv

    interms <- makeIntermArrays segred_kind group_size_se chunk_se (tvSize num_threads) params

    sync_arr <- sAllocArray "sync_arr" Bool (Shape [intConst Int32 1]) $ Space "local"

    -- We probably do not have enough actual workgroups to cover the
    -- entire iteration space.  Some groups thus have to perform double
    -- duty; we put an outer loop to accomplish this.
    virtualiseGroups SegVirt (sExt32 (tvExp num_virtgroups)) $ \virt_group_id -> do
      let segment_gtids = init gtids
          ltid = kernelLocalThreadId constants

      flat_segment_id <-
        dPrimVE "flat_segment_id" $
          sExt64 virt_group_id `quot` groups_per_segment

      global_tid <-
        dPrimVE "global_tid" $
          Imp.le64 (segFlat space)
            `rem` (sExt64 group_size' * groups_per_segment)

      let first_group_for_segment = flat_segment_id * groups_per_segment
      dIndexSpace (zip segment_gtids (init dims')) $ flat_segment_id
      dPrim_ (last gtids) int64
      let n = pe64 $ last dims

      slugs <-
        mapM (segBinOpSlug ltid virt_group_id) $
          zip3 segbinops interms reds_group_res_arrs
      new_lambdas <-
        reductionStageOne
          segred_kind
          gtids
          n
          global_tid
          q
          chunk
          threads_per_segment
          slugs
          map_body_cont

      let segred_pess =
            chunks (map (length . segBinOpNeutral) segbinops) $
              segred_pes

          multiple_groups_per_segment =
            forM_ (zip4 segred_pess slugs new_lambdas [0 ..]) $
              \(pes, slug, new_lambda, i) -> do
                let counter_idx =
                      fromIntegral (i * num_counters)
                        + flat_segment_id
                          `rem` fromIntegral num_counters
                reductionStageTwo
                  pes
                  virt_group_id
                  (map Imp.le64 segment_gtids)
                  first_group_for_segment
                  groups_per_segment
                  slug
                  new_lambda
                  counters
                  sync_arr
                  counter_idx

          one_group_per_segment =
            sComment "first thread in group saves final result to memory" $
              forM2_ slugs segred_pess $ \slug pes ->
                sWhen (ltid .==. 0) $
                  forM2_ pes (slugAccs slug) $ \v (acc, acc_is) ->
                    copyDWIMFix (patElemName v) (map Imp.le64 segment_gtids) (Var acc) acc_is

      sIf (groups_per_segment .==. 1) one_group_per_segment multiple_groups_per_segment

-- | Auxiliary information for a single reduction. A slug holds the `SegBinOp`
-- operator for a single reduction, the different arrays required throughout
-- stages one and two, and a global mem destination for the final result of the
-- particular reduction.
data SegBinOpSlug = SegBinOpSlug
  { slugOp :: SegBinOp GPUMem,
    -- | Intermediate arrays needed for the given reduction.
    slugInterms :: SegRedIntermediateArrays,
    -- | Place(s) to store group accumulator(s) in stage 1 reduction.
    slugAccs :: [(VName, [Imp.TExp Int64])],
    -- | Global memory destination(s) for the final result(s) for this
    -- particular reduction.
    groupResArrs :: [VName]
  }

segBinOpSlug ::
  Imp.TExp Int32 ->
  Imp.TExp Int32 ->
  (SegBinOp GPUMem, SegRedIntermediateArrays, [VName]) ->
  InKernelGen SegBinOpSlug
segBinOpSlug ltid group_id (op, interms, group_res_arrs) = do
  accs <- zipWithM mkAcc (lambdaParams (segBinOpLambda op)) group_res_arrs
  pure $ SegBinOpSlug op interms accs group_res_arrs
  where
    mkAcc p group_res_arr
      | Prim t <- paramType p,
        shapeRank (segBinOpShape op) == 0 = do
          group_res_acc <- dPrim (baseString (paramName p) <> "_group_res_acc") t
          pure (tvVar group_res_acc, [])
      -- if this is a non-primitive reduction, the global mem result array will
      -- double as accumulator.
      | otherwise =
          pure (group_res_arr, [sExt64 ltid, sExt64 group_id])

slugLambda :: SegBinOpSlug -> Lambda GPUMem
slugLambda = segBinOpLambda . slugOp

slugBody :: SegBinOpSlug -> Body GPUMem
slugBody = lambdaBody . slugLambda

slugParams :: SegBinOpSlug -> [LParam GPUMem]
slugParams = lambdaParams . slugLambda

slugNeutral :: SegBinOpSlug -> [SubExp]
slugNeutral = segBinOpNeutral . slugOp

slugShape :: SegBinOpSlug -> Shape
slugShape = segBinOpShape . slugOp

slugsComm :: [SegBinOpSlug] -> Commutativity
slugsComm = mconcat . map (segBinOpComm . slugOp)

slugSplitParams :: SegBinOpSlug -> ([LParam GPUMem], [LParam GPUMem])
slugSplitParams slug = splitAt (length (slugNeutral slug)) $ slugParams slug

slugSplitParams :: SegBinOpSlug -> ([LParam GPUMem], [LParam GPUMem])
slugSplitParams slug = splitAt (length (slugNeutral slug)) $ slugParams slug

slugGroupRedArrs :: SegBinOpSlug -> [VName]
slugGroupRedArrs = groupRedArrs . slugInterms

slugPrivChunks :: SegBinOpSlug -> [VName]
slugPrivChunks = privateChunks . slugInterms

slugCollCopyArrs :: SegBinOpSlug -> [VName]
slugCollCopyArrs = collCopyArrs . slugInterms

reductionStageOne ::
  SegredKind ->
  [VName] ->
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  [SegBinOpSlug] ->
  DoSegBody ->
  InKernelGen [Lambda GPUMem]
reductionStageOne segred_kind gtids n global_tid q chunk threads_per_segment slugs body_cont = do
  constants <- kernelConstants <$> askEnv
  let glb_ind_var = mkTV (last gtids) int64
      ltid = sExt64 $ kernelLocalThreadId constants

  dScope Nothing $ scopeOfLParams $ concatMap slugParams slugs

  sComment "ne-initialise the outer (per-group) accumulator(s)" $ do
    forM_ slugs $ \slug ->
      forM2_ (slugAccs slug) (slugNeutral slug) $ \(acc, acc_is) ne ->
        sLoopNest (slugShape slug) $ \vec_is ->
          copyDWIMFix acc (acc_is ++ vec_is) ne []

  new_lambdas <- mapM (renameLambda . slugLambda) slugs
  let group_size = sExt32 $ kernelGroupSize constants
  let doGroupReduce =
        forM2_ slugs new_lambdas $ \slug new_lambda -> do
          let accs = slugAccs slug
          let params = slugParams slug
          sLoopNest (slugShape slug) $ \vec_is -> do
            let group_red_arrs = slugGroupRedArrs slug
            sComment "store accs. prims go in lmem; non-prims in params (in global mem)" $
              forM_ (zip3 group_red_arrs accs params) $
                \(arr, (acc, acc_is), p) ->
                  if isPrimParam p
                    then copyDWIMFix arr [ltid] (Var acc) (acc_is ++ vec_is)
                    else copyDWIMFix (paramName p) [] (Var acc) (acc_is ++ vec_is)

            let new_lambda = segBinOpLambda $ slugOp slug
            sOp $ Imp.ErrorSync Imp.FenceLocal -- Also implicitly barrier.
            groupReduce group_size new_lambda group_red_arrs
            sOp $ Imp.Barrier Imp.FenceLocal

            sComment "thread 0 updates per-group acc(s); rest reset to ne" $ do
              sIf
                (ltid .==. 0)
                ( forM2_ accs (lambdaParams new_lambda) $
                    \(acc, acc_is) p ->
                      copyDWIMFix acc (acc_is ++ vec_is) (Var $ paramName p) []
                )
                ( forM2_ accs (slugNeutral slug) $
                    \(acc, acc_is) ne ->
                      copyDWIMFix acc (acc_is ++ vec_is) ne []
                )

  case segred_kind of
    NoncommPrimSegred ->
      noncommPrimParamsStageOneBody
        slugs
        body_cont
        glb_ind_var
        global_tid
        q
        n
        chunk
        doGroupReduce
    _ ->
      generalStageOneBody
        slugs
        body_cont
        glb_ind_var
        global_tid
        q
        n
        threads_per_segment
        doGroupReduce

  pure new_lambdas

generalStageOneBody ::
  [SegBinOpSlug] ->
  DoSegBody ->
  TV Int64 ->
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  InKernelGen () ->
  InKernelGen ()
generalStageOneBody slugs body_cont glb_ind_var global_tid q n threads_per_segment doGroupReduce = do
  let is_comm = slugsComm slugs == Commutative

  constants <- kernelConstants <$> askEnv
  let group_size = kernelGroupSize constants
      ltid = sExt64 $ kernelLocalThreadId constants

  -- this group's id within its designated segment; the stride made per group in
  -- the outer `i < q` loop; and this group's initial global offset.
  group_id_in_segment <- dPrimVE "group_id_in_segment" $ global_tid `quot` group_size
  group_base_offset <- dPrimVE "group_base_offset" $ group_id_in_segment * q * group_size

  sFor "i" q $ \i -> do
    group_offset <- dPrimVE "group_offset" $ group_base_offset + i * group_size
    glb_ind_var
      <-- if is_comm
        then global_tid + threads_per_segment * i
        else group_offset + ltid

    sWhen (tvExp glb_ind_var .<. n) $
      sComment "apply map function(s)" $
        body_cont $ \all_red_res -> do
          let maps_res = chunks (map (length . slugNeutral) slugs) all_red_res

          forM2_ slugs maps_res $ \slug map_res ->
            sLoopNest (slugShape slug) $ \vec_is -> do
              let (acc_params, next_params) = slugSplitParams slug
              sComment "load accumulator(s)" $
                forM2_ acc_params (slugAccs slug) $ \p (acc, acc_is) ->
                  copyDWIMFix (paramName p) [] (Var acc) (acc_is ++ vec_is)
              sComment "load next value(s)" $
                forM2_ next_params map_res $ \p (res, res_is) ->
                  copyDWIMFix (paramName p) [] res (res_is ++ vec_is)
              sComment "apply reduction operator(s)"
                $ compileStms mempty (bodyStms $ slugBody slug)
                $ sComment "store in accumulator(s)"
                $ forM2_
                  (slugAccs slug)
                  (map resSubExp $ bodyResult $ slugBody slug)
                $ \(acc, acc_is) se ->
                  copyDWIMFix acc (acc_is ++ vec_is) se []

    unless is_comm doGroupReduce
  sOp $ Imp.ErrorSync Imp.FenceLocal
  when is_comm doGroupReduce

noncommPrimParamsStageOneBody ::
  [SegBinOpSlug] ->
  DoSegBody ->
  TV Int64 ->
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  InKernelGen () ->
  InKernelGen ()
noncommPrimParamsStageOneBody slugs body_cont glb_ind_var global_tid q n chunk doLMemGroupReduce = do
  constants <- kernelConstants <$> askEnv
  let group_size = kernelGroupSize constants
      ltid = sExt64 $ kernelLocalThreadId constants

  -- this group's id within its designated segment; the stride made per group in
  -- the outer `i < q` loop; and this group's initial global offset.
  group_id_in_segment <- dPrimVE "group_offset_in_segment" $ global_tid `quot` group_size
  group_stride <- dPrimVE "group_stride" $ group_size * chunk
  group_base_offset <- dPrimVE "group_base_offset" $ group_id_in_segment * q * group_stride

  let chunkLoop = sFor "k" chunk

  sFor "i" q $ \i -> do
    -- group offset in this iteration.
    group_offset <- dPrimVE "group_offset" $ group_base_offset + i * group_stride
    chunkLoop $ \k -> do
      loc_ind <- dPrimVE "loc_ind" $ k * group_size + ltid
      glb_ind_var <-- group_offset + loc_ind

      sIf
        (tvExp glb_ind_var .<. n)
        ( body_cont $ \all_red_res -> do
            let slugs_res = chunks (map (length . slugNeutral) slugs) all_red_res
            forM2_ slugs slugs_res $ \slug slug_res -> do
              let priv_chunks = slugPrivChunks slug
              sComment "write map result(s) to private chunk(s)" $
                forM2_ priv_chunks slug_res $ \priv_chunk (res, res_is) ->
                  copyDWIMFix priv_chunk [k] res res_is
        )
        -- if out of bounds, fill chunk(s) with neutral element(s)
        ( forM_ slugs $ \slug ->
            forM2_ (slugPrivChunks slug) (slugNeutral slug) $
              \priv_chunk ne ->
                copyDWIMFix priv_chunk [k] ne []
        )

    -- TODO: is this (errorsync) barrier necessary? the above block of code only
    -- reads elements from global, maps them, and writes them to private mem.
    -- sOp $ Imp.ErrorSync Imp.FenceLocal

    sComment "effectualize collective copies in local memory" $ do
      forM_ slugs $ \slug -> do
        let coll_copy_arrs = slugCollCopyArrs slug
        let priv_chunks = slugPrivChunks slug

        forM2_ coll_copy_arrs priv_chunks $ \lmem_arr priv_chunk -> do
          chunkLoop $ \k -> do
            lmem_idx <- dPrimVE "lmem_idx" $ ltid + k * group_size
            copyDWIMFix lmem_arr [lmem_idx] (Var priv_chunk) [k]

          sOp $ Imp.Barrier Imp.FenceLocal

          chunkLoop $ \k -> do
            lmem_idx <- dPrimVE "lmem_idx" $ ltid * chunk + k
            copyDWIMFix priv_chunk [k] (Var lmem_arr) [lmem_idx]

          sOp $ Imp.Barrier Imp.FenceLocal

    sComment "per-thread sequential reduction of private chunk(s)" $ do
      chunkLoop $ \k ->
        forM_ slugs $ \slug -> do
          let accs = map fst $ slugAccs slug
          let (acc_ps, next_ps) = slugSplitParams slug
          let ps_accs_chunks = zip4 acc_ps next_ps accs (slugPrivChunks slug)

          sComment "load params for all reductions" $ do
            forM_ ps_accs_chunks $ \(acc_p, next_p, acc, priv_chunk) -> do
              copyDWIM (paramName acc_p) [] (Var acc) []
              copyDWIMFix (paramName next_p) [] (Var priv_chunk) [k]

          sComment "apply reduction operator(s)" $ do
            let binop_ress = map resSubExp $ bodyResult $ slugBody slug
            compileStms mempty (bodyStms $ slugBody slug) $
              forM2_ accs binop_ress $ \acc binop_res ->
                copyDWIM acc [] binop_res []
    doLMemGroupReduce
  sOp $ Imp.ErrorSync Imp.FenceLocal

reductionStageTwo ::
  [PatElem LetDecMem] ->
  Imp.TExp Int32 ->
  [Imp.TExp Int64] ->
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  SegBinOpSlug ->
  Lambda GPUMem ->
  VName ->
  VName ->
  Imp.TExp Int64 ->
  InKernelGen ()
reductionStageTwo segred_pes group_id segment_gtids first_group_for_segment groups_per_segment slug new_lambda counters sync_arr counter_idx = do
  constants <- kernelConstants <$> askEnv

  let ltid32 = kernelLocalThreadId constants
      ltid = sExt64 ltid32
      group_size = kernelGroupSize constants

  let (acc_params, next_params) = slugSplitParams slug
  let nes = slugNeutral slug
  let red_arrs = slugGroupRedArrs slug
  let group_res_arrs = groupResArrs slug

  old_counter <- dPrim "old_counter" int32
  (counter_mem, _, counter_offset) <-
    fullyIndexArray
      counters
      [counter_idx]
  sComment "first thread in group saves group result to global memory" $
    sWhen (ltid32 .==. 0) $ do
      forM_ (take (length nes) $ zip group_res_arrs (slugAccs slug)) $ \(v, (acc, acc_is)) ->
        copyDWIMFix v [0, sExt64 group_id] (Var acc) acc_is
      sOp $ Imp.MemFence Imp.FenceGlobal
      -- Increment the counter, thus stating that our result is
      -- available.
      sOp
        $ Imp.Atomic DefaultSpace
        $ Imp.AtomicAdd
          Int32
          (tvVar old_counter)
          counter_mem
          counter_offset
        $ untyped (1 :: Imp.TExp Int32)
      -- Now check if we were the last group to write our result.  If
      -- so, it is our responsibility to produce the final result.
      sWrite sync_arr [0] $ untyped $ tvExp old_counter .==. groups_per_segment - 1

  sOp $ Imp.Barrier Imp.FenceGlobal

  is_last_group <- dPrim "is_last_group" Bool
  copyDWIMFix (tvVar is_last_group) [] (Var sync_arr) [0]
  sWhen (tvExp is_last_group) $ do
    -- The final group has written its result (and it was
    -- us!), so read in all the group results and perform the
    -- final stage of the reduction.  But first, we reset the
    -- counter so it is ready for next time.  This is done
    -- with an atomic to avoid warnings about write/write
    -- races in oclgrind.
    sWhen (ltid32 .==. 0) $
      sOp $
        Imp.Atomic DefaultSpace $
          Imp.AtomicAdd Int32 (tvVar old_counter) counter_mem counter_offset $
            untyped $
              negate groups_per_segment

    sLoopNest (slugShape slug) $ \vec_is -> do
      unless (null $ slugShape slug) $
        sOp (Imp.Barrier Imp.FenceLocal)

      -- There is no guarantee that the number of workgroups for the
      -- segment is less than the workgroup size, so each thread may
      -- have to read multiple elements.  We do this in a sequential
      -- way that may induce non-coalesced accesses, but the total
      -- number of accesses should be tiny here.
      sComment "read in the per-group-results" $ do
        read_per_thread <-
          dPrimVE "read_per_thread" $
            groups_per_segment `divUp` sExt64 group_size

        forM2_ acc_params nes $ \p ne ->
          copyDWIM (paramName p) [] ne []

        sFor "i" read_per_thread $ \i -> do
          group_res_id <-
            dPrimVE "group_res_id" $
              ltid * read_per_thread + i
          index_of_group_res <-
            dPrimVE "index_of_group_res" $
              first_group_for_segment + group_res_id

          sWhen (group_res_id .<. groups_per_segment) $ do
            forM2_ next_params group_res_arrs $
              \p group_res_arr ->
                copyDWIMFix
                  (paramName p)
                  []
                  (Var group_res_arr)
                  ([0, index_of_group_res] ++ vec_is)

            compileStms mempty (bodyStms $ slugBody slug) $
              forM2_ acc_params (map resSubExp $ bodyResult $ slugBody slug) $ \p se ->
                copyDWIMFix (paramName p) [] se []

      forM2_ acc_params red_arrs $ \p arr ->
        when (isPrimParam p) $
          copyDWIMFix arr [ltid] (Var $ paramName p) []

      sOp $ Imp.ErrorSync Imp.FenceLocal

      sComment "reduce the per-group results" $ do
        groupReduce (sExt32 group_size) new_lambda red_arrs

        sComment "and back to memory with the final result" $
          sWhen (ltid32 .==. 0) $
            forM2_ segred_pes (lambdaParams new_lambda) $ \pe p ->
              copyDWIMFix
                (patElemName pe)
                (segment_gtids ++ vec_is)
                (Var $ paramName p)
                []

-- Note [IntermArrays]
--
-- Intermediate memory for the nonsegmented and large segments non-commutative
-- reductions with all primitive parameters:
--
--   These kernels need local memory for 1) the initial collective copy, 2) the
--   (virtualized) group reductions, and (TODO: this one not implemented yet!)
--   3) the final single-group collective copy. There are no dependencies
--   between these three stages, so we can reuse the same pool of local mem for
--   all three. These intermediates all go into local mem because of the
--   assumption of primitive parameter types.
--
--   Let `elem_sizes` be a list of element type sizes for the reduction
--   operators in a given redomap fusion. Then the amount of local mem needed
--   across the three steps are:
--
--   1) The initial collective copy from global to thread-private memory
--   requires `group_size * CHUNK * max elem_sizes`, since the collective copies
--   are performed in sequence (ie. inputs to different reduction operators need
--   not be held in local mem simultaneously).
--   2) The intra-group reductions of local memory held per-thread results
--   require `group_size * sum elem_sizes` bytes, since per-thread results for
--   all fused reductions are group-reduced simultaneously.
--   3) If group_size < num_groups, then after the final single-group collective
--   copy, a thread-sequential reduction reduces the number of per-group partial
--   results from num_groups down to group_size for each reduction array, such
--   that they will each fit in the final intra-group reduction. This requires
--   `num_groups * max elem_sizes`.
--
--   In summary, the total amount of local mem needed is the maximum between:
--   1) initial collective copy: group_size * CHUNK * max elem_sizes
--   2) intra-group reductions:  group_size * sum elem_sizes
--   3) final collective copy:   num_groups * max elem_sizes
--
--   The amount of local mem will most likely be decided by 1) in most cases,
--   unless the number of fused operators is very high *or* if we have a
--   `num_groups > group_size * CHUNK`, but this is unlikely, in which case 2)
--   and 3), respectively, will dominate.
--
--   Aside from local memory, these kernels also require a CHUNK-sized array of
--   thread-private register memory per reduction operator.
--
-- For all other reductions, ie. commutative reductions, reductions with at
-- least one non-primitive operator, and small segments reductions:
--
--   These kernels use local memory only for the intra-group reductions, and
--   since they do not use chunking or CHUNK, they all require onlly `group_size
--   * max elem_sizes` bytes of local memory and no thread-private register mem.
