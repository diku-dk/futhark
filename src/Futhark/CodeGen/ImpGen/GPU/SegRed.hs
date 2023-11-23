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
module Futhark.CodeGen.ImpGen.GPU.SegRed
  ( compileSegRed,
    compileSegRed',
    DoSegBody,
  )
where

import Control.Monad
import Data.List (genericLength, zip4, zip7)
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
maxNumOps :: Int32
maxNumOps = 10

-- | Code generation for the body of the SegRed, taking a continuation
-- for saving the results of the body.  The results should be
-- represented as a pairing of a t'SubExp' along with a list of
-- indexes into that t'SubExp' for reading the result.
type DoSegBody = ([(SubExp, [Imp.TExp Int64])] -> InKernelGen ()) -> InKernelGen ()

-- | Compile 'SegRed' instance to host-level code with calls to
-- various kernels.
compileSegRed ::
  Pat LetDecMem ->
  SegLevel ->
  SegSpace ->
  [SegBinOp GPUMem] ->
  KernelBody GPUMem ->
  CallKernelGen ()
compileSegRed pat lvl space reds body = do
  emit $ Imp.DebugPrint "\n# SegRed" Nothing
  KernelAttrs _ _ num_groups group_size <- lvlKernelAttrs lvl
  let grid = KernelGrid num_groups group_size

  compileSegRed' pat grid space reds $ \red_cont ->
    sComment "apply map function" $
      compileStms mempty (kernelBodyStms body) $ do
        let (red_res, map_res) = splitAt (segBinOpResults reds) $ kernelBodyResult body

        let mapout_arrs = drop (segBinOpResults reds) $ patElems pat
        when (not $ null mapout_arrs) $
          sComment "write map-out result(s)" $ do
            zipWithM_ (compileThreadResult space) mapout_arrs map_res

        red_cont $ map ((,[]) . kernelResultSubExp) red_res
  emit $ Imp.DebugPrint "" Nothing

-- | Like 'compileSegRed', but where the body is a monadic action.
compileSegRed' ::
  Pat LetDecMem ->
  KernelGrid ->
  SegSpace ->
  [SegBinOp GPUMem] ->
  DoSegBody ->
  CallKernelGen ()
compileSegRed' pat grid space segbinops body_cont
  | genericLength segbinops > maxNumOps =
      compilerLimitationS $
        "compileSegRed': at most " ++ show maxNumOps ++ " reduction operators are supported."
  | [(_, Constant (IntValue (Int64Value 1))), _] <- unSegSpace space =
      compileReduction nonsegmentedReduction
  | otherwise = do
      let segment_size = pe64 $ last $ segSpaceDims space
          use_small_segments = segment_size * 2 .<. pe64 group_size'
      sIf
        use_small_segments
        (compileReduction smallSegmentsReduction)
        (compileReduction largeSegmentsReduction)
  where
    compileReduction f =
      f pat num_groups group_size (pe64 chunk) space segbinops body_cont interms_cont

    num_groups = gridNumGroups grid
    group_size = gridGroupSize grid
    group_size' = unCount group_size
    chunk = intConst Int64 $ getChunkSize $ map paramType $ concat params

    paramOf (SegBinOp _ op ne _) = take (length ne) $ lambdaParams op
    params = map paramOf segbinops

    interms_cont = do
      let is_noncommprim_reduce =
            binopsComm segbinops == Noncommutative
              && all (all isPrimParam) params
      num_threads <-
        dPrimV "num_threads" $ (pe64 $ unCount num_groups) * pe64 group_size'
      makeIntermArrays is_noncommprim_reduce group_size' chunk num_threads params



data BinOpIntermediateArrays
  = CommInterms
      { groupRedArrs :: [VName]
      }
  | NoncommPrimInterms
      { collCopyArrs :: [VName],
        groupRedArrs :: [VName],
        privateChunks :: [VName]
      }

-- | Prepare intermediate arrays for the reduction.  Prim-typed
-- arguments go in local memory (so we need to do the allocation of
-- those arrays inside the kernel), while array-typed arguments go in
-- global memory.  Allocations for the former have already been
-- performed.  This policy is baked into how the allocations are done
-- in ExplicitAllocations.
makeIntermArrays ::
  Bool ->
  SubExp ->
  SubExp ->
  TV Int64 ->
  [[Param LParamMem]] ->
  InKernelGen [BinOpIntermediateArrays]
makeIntermArrays True group_size chunk _ params = do

  -- regarding local memory layout for the non-commutative reduction kernel:
  -- the kernel uses local memory for the initial collective copy, the
  -- (virtualized) group reductions, and the final single-group collective
  -- copy. there are no dependencies between these three stages, so we can
  -- reuse the same pool of local mem for all three.
  --
  -- after the final single-group collective copy, a thread-sequential
  -- reduction reduces the number of per-group partial results
  -- from num_groups down to group_size (unless group_size >= num_groups, in
  -- which case the loop runs for 0 iterations) for each reduction array,
  -- such that they each fit in the final intra-group reduction.
  --
  -- if for each partial result we copy and immediately reduce, then we can
  -- reuse the same lmem for all reduction arrays, rather than needing to
  -- hold all partial results in lmem at once.
  --
  -- hence the total amount of local mem needed is the maximum between:
  --   collective copy lmem       = group_size * chunk * max elem_sizes
  --   final collective copy lmem = num_groups * max elem_sizes
  --   group reduction lmem       = group_size * sum elem_sizes
  --
  -- as such, the amount of lmem will typically be dominated by the initial
  -- collective copy *unless* the number of fused reductions is large *or*
  -- we simultaneously have a large num_groups and small group_size and
  -- chunk.

  group_worksize <- tvSize <$> (dPrimV "group_worksize" group_worksize_E)

  -- compute total amount of lmem needed for the group reduction arrays as well
  -- as the offset into the total pool of lmem of each such array.

  -- TODO: we essentially compute `group_reds_lmem_requirement` twice, since it
  -- is also contained in the computation of `offsets`. However, trying to
  -- extract it, we get an "unknown variable" error, since the variable holding
  -- the lmem size exists only in kernel scope but needs to be accessed from
  -- host scope. To avoid computing it twice, we cannot bind the individual
  -- offsets (to names, using dPrimVE), which absolutely clutters the generated
  -- code.

  let f x y = nextMul x y + group_size_E * y
      group_reds_lmem_requirement = foldl f 0 $ concat elem_sizes

  (_, offsets) <-
    forAccumLM2D 0 elem_sizes $ \byte_offs elem_size -> do
      next_offs <- dPrimVE "offset" $ f byte_offs elem_size
      pure (next_offs, byte_offs `quot` elem_size)

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
    pure $ NoncommPrimInterms coll_copy_arrs group_red_arrs priv_chunks

  where
    group_size_E = pe64 group_size
    group_worksize_E = group_size_E * pe64 chunk

    paramSize = (primByteSize . elemType . paramType)
    elem_sizes = map (map paramSize) params
    max_elem_size = maximum $ concat elem_sizes

    forAccumLM2D acc ls f = mapAccumLM (mapAccumLM f) acc ls

makeIntermArrays _ group_size _ num_threads params =
  fmap (map CommInterms) $
    forM params $
      mapM $ \p ->
        case paramDec p of
          MemArray pt shape _ (ArrayIn mem _) -> do
            let shape' = Shape [tvSize num_threads] <> shape
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
groupResultArrays virt_num_groups group_size reds =
  forM reds $ \(SegBinOp _ lam _ shape) ->
    forM (lambdaReturnType lam) $ \t -> do
      let pt = elemType t
          extra_dim
            | primType t, shapeRank shape == 0 = intConst Int64 1
            | otherwise = group_size
          full_shape = Shape [extra_dim, virt_num_groups] <> shape <> arrayShape t
          -- Move the groupsize dimension last to ensure coalesced
          -- memory access.
          perm = [1 .. shapeRank full_shape - 1] ++ [0]
      sAllocArrayPerm "segred_tmp" pt full_shape (Space "device") perm

nonsegmentedReduction ::
  Pat LetDecMem ->
  Count NumGroups SubExp ->
  Count GroupSize SubExp ->
  Imp.TExp Int64 ->
  SegSpace ->
  [SegBinOp GPUMem] ->
  DoSegBody ->
  InKernelGen [BinOpIntermediateArrays] ->
  CallKernelGen ()
nonsegmentedReduction (Pat segred_pes) num_groups group_size chunk space reds body interms_cont = do
  let (gtids, dims) = unzip $ unSegSpace space
      num_groups' = unCount num_groups
      group_size' = unCount group_size
      global_tid = Imp.le64 $ segFlat space
      w = pe64 $ last dims

  counter <- genZeroes "counters" $ fromIntegral maxNumOps

  reds_group_res_arrs <- groupResultArrays num_groups' group_size' reds

  num_threads <-
    dPrimV "num_threads" $ pe64 num_groups' * pe64 group_size'

  sKernelThread "segred_nonseg" (segFlat space) (defKernelAttrs num_groups group_size) $ do
    constants <- kernelConstants <$> askEnv

    interms <- interms_cont
    let reds_arrs = map groupRedArrs interms
    sync_arr <- sAllocArray "sync_arr" Bool (Shape [intConst Int32 1]) $ Space "local"

    -- Since this is the nonsegmented case, all outer segment IDs must
    -- necessarily be 0.
    forM_ gtids $ \v -> dPrimV_ v (0 :: Imp.TExp Int64)

    q <- dPrimVE "q" $ w `divUp` (sExt64 (kernelNumThreads constants) * chunk)

    slugs <-
      mapM (segBinOpSlug (kernelLocalThreadId constants) (kernelGroupId constants)) $
        zip3 reds interms reds_group_res_arrs
    sComment_ "stage one BEGIN"
    red_ops_renamed <-
      reductionStageOne
        gtids
        w
        global_tid
        q
        chunk
        (tvExp num_threads)
        slugs
        body

    sComment_ "stage one END"
    let segred_pess =
          chunks (map (length . segBinOpNeutral) reds) $
            segred_pes
    forM_ (zip7 reds reds_arrs reds_group_res_arrs segred_pess slugs red_ops_renamed [0 ..]) $
      \(SegBinOp _ red_op nes _, red_arrs, group_res_arrs, pes, slug, red_op_renamed, i) -> do
        let (red_x_params, red_y_params) = splitAt (length nes) $ lambdaParams red_op
        sComment_ "stage two BEGIN"
        reductionStageTwo
          constants
          pes
          (kernelGroupId constants)
          0
          [0]
          0
          (sExt64 $ kernelNumGroups constants)
          slug
          red_x_params
          red_y_params
          red_op_renamed
          nes
          1
          counter
          (fromInteger i)
          sync_arr
          group_res_arrs
          red_arrs
        sComment_ "stage two END"

smallSegmentsReduction ::
  Pat LetDecMem ->
  Count NumGroups SubExp ->
  Count GroupSize SubExp ->
  Imp.TExp Int64 ->
  SegSpace ->
  [SegBinOp GPUMem] ->
  DoSegBody ->
  InKernelGen [BinOpIntermediateArrays] ->
  CallKernelGen ()
smallSegmentsReduction (Pat segred_pes) num_groups group_size _chunk space reds body make_interms = do
  let (gtids, dims) = unzip $ unSegSpace space
      dims' = map pe64 dims
      segment_size = last dims'

  -- Careful to avoid division by zero now.
  segment_size_nonzero <-
    dPrimVE "segment_size_nonzero" $ sMax64 1 segment_size

  let num_groups' = fmap pe64 num_groups
      group_size' = fmap pe64 group_size
  num_threads <- dPrimV "num_threads" $ unCount num_groups' * unCount group_size'
  let num_segments = product $ init dims'
      segments_per_group = unCount group_size' `quot` segment_size_nonzero
      required_groups = sExt32 $ num_segments `divUp` segments_per_group

  emit $ Imp.DebugPrint "# SegRed-small" Nothing
  emit $ Imp.DebugPrint "num_segments" $ Just $ untyped num_segments
  emit $ Imp.DebugPrint "segment_size" $ Just $ untyped segment_size
  emit $ Imp.DebugPrint "segments_per_group" $ Just $ untyped segments_per_group
  emit $ Imp.DebugPrint "required_groups" $ Just $ untyped required_groups

  sKernelThread "segred_small" (segFlat space) (defKernelAttrs num_groups group_size) $ do
    constants <- kernelConstants <$> askEnv

    interms <- make_interms
    let reds_arrs = map groupRedArrs interms

    -- We probably do not have enough actual workgroups to cover the
    -- entire iteration space.  Some groups thus have to perform double
    -- duty; we put an outer loop to accomplish this.
    virtualiseGroups SegVirt required_groups $ \group_id' -> do
      -- Compute the 'n' input indices.  The outer 'n-1' correspond to
      -- the segment ID, and are computed from the group id.  The inner
      -- is computed from the local thread id, and may be out-of-bounds.
      let ltid = sExt64 $ kernelLocalThreadId constants
          segment_index =
            (ltid `quot` segment_size_nonzero)
              + (sExt64 group_id' * sExt64 segments_per_group)
          index_within_segment = ltid `rem` segment_size

      dIndexSpace (zip (init gtids) (init dims')) segment_index
      dPrimV_ (last gtids) index_within_segment

      let out_of_bounds =
            forM2_ reds reds_arrs $ \(SegBinOp _ _ nes _) red_arrs ->
              forM2_ red_arrs nes $ \arr ne ->
                copyDWIMFix arr [ltid] ne []

          in_bounds =
            body $ \red_res ->
              sComment "save results to be reduced" $ do
                let red_dests = map (,[ltid]) (concat reds_arrs)
                forM2_ red_dests red_res $ \(d, d_is) (res, res_is) ->
                  copyDWIMFix d d_is res res_is

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
          forM2_ reds reds_arrs $ \(SegBinOp _ red_op _ _) red_arrs ->
            groupScan
              (Just crossesSegment)
              (sExt64 $ tvExp num_threads)
              (segment_size * segments_per_group)
              red_op
              red_arrs

      sOp $ Imp.Barrier Imp.FenceLocal

      sComment "save final values of segments"
        $ sWhen
          ( sExt64 group_id'
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
                sExt64 group_id' * segments_per_group + sExt64 ltid
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

largeSegmentsReduction ::
  Pat LetDecMem ->
  Count NumGroups SubExp ->
  Count GroupSize SubExp ->
  Imp.TExp Int64 ->
  SegSpace ->
  [SegBinOp GPUMem] ->
  DoSegBody ->
  InKernelGen [BinOpIntermediateArrays] ->
  CallKernelGen ()
largeSegmentsReduction segred_pat num_groups group_size chunk space reds body make_interms = do
  let (gtids, dims) = unzip $ unSegSpace space
      dims' = map pe64 dims
      num_segments = product $ init dims'
      segment_size = last dims'
      num_groups' = pe64 $ unCount num_groups
      group_size' = pe64 $ unCount group_size

  (groups_per_segment, q) <-
    groupsPerSegmentAndQ
      segment_size
      num_segments
      num_groups'
      group_size'
      chunk

  virt_num_groups <-
    dPrimV "virt_num_groups" $
      groups_per_segment * num_segments

  threads_per_segment <-
    dPrimV "threads_per_segment" $
      groups_per_segment * group_size'

  emit $ Imp.DebugPrint "# SegRed-large" Nothing
  emit $ Imp.DebugPrint "num_segments" $ Just $ untyped num_segments
  emit $ Imp.DebugPrint "segment_size" $ Just $ untyped segment_size
  emit $ Imp.DebugPrint "virt_num_groups" $ Just $ untyped $ tvExp virt_num_groups
  emit $ Imp.DebugPrint "num_groups" $ Just $ untyped num_groups'
  emit $ Imp.DebugPrint "group_size" $ Just $ untyped group_size'
  emit $ Imp.DebugPrint "q" $ Just $ untyped q
  emit $ Imp.DebugPrint "groups_per_segment" $ Just $ untyped groups_per_segment

  reds_group_res_arrs <- groupResultArrays (tvSize virt_num_groups) (unCount group_size) reds

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
  let num_counters = fromIntegral maxNumOps * 1024
  counter <- genZeroes "counters" num_counters

  sKernelThread "segred_large" (segFlat space) (defKernelAttrs num_groups group_size) $ do
    constants <- kernelConstants <$> askEnv

    interms <- make_interms
    let reds_arrs = map groupRedArrs interms
    sync_arr <- sAllocArray "sync_arr" Bool (Shape [intConst Int32 1]) $ Space "local"

    -- We probably do not have enough actual workgroups to cover the
    -- entire iteration space.  Some groups thus have to perform double
    -- duty; we put an outer loop to accomplish this.
    virtualiseGroups SegVirt (sExt32 (tvExp virt_num_groups)) $ \group_id -> do
      let segment_gtids = init gtids
          w = last dims
          local_tid = kernelLocalThreadId constants

      flat_segment_id <-
        dPrimVE "flat_segment_id" $
          group_id `quot` sExt32 groups_per_segment

      global_tid <-
        dPrimVE "global_tid" $
          (sExt64 group_id * sExt64 group_size' + sExt64 local_tid)
            `rem` (sExt64 group_size' * groups_per_segment)

      let first_group_for_segment = sExt64 flat_segment_id * groups_per_segment
      dIndexSpace (zip segment_gtids (init dims')) $ sExt64 flat_segment_id
      dPrim_ (last gtids) int64
      let num_elements = pe64 w

      slugs <-
        mapM (segBinOpSlug local_tid group_id) $
          zip3 reds interms reds_group_res_arrs
      red_ops_renamed <-
        reductionStageOne
          gtids
          num_elements
          global_tid
          q
          chunk
          (tvExp threads_per_segment)
          slugs
          body

      let segred_pes =
            chunks (map (length . segBinOpNeutral) reds) $
              patElems segred_pat

          multiple_groups_per_segment =
            forM_ (zip7 reds reds_arrs reds_group_res_arrs segred_pes slugs red_ops_renamed [0 ..]) $
              \(SegBinOp _ red_op nes _, red_arrs, group_res_arrs, pes, slug, red_op_renamed, i) -> do
                let (red_x_params, red_y_params) =
                      splitAt (length nes) $ lambdaParams red_op
                reductionStageTwo
                  constants
                  pes
                  group_id
                  flat_segment_id
                  (map Imp.le64 segment_gtids)
                  (sExt64 first_group_for_segment)
                  groups_per_segment
                  slug
                  red_x_params
                  red_y_params
                  red_op_renamed
                  nes
                  (fromIntegral num_counters)
                  counter
                  (fromInteger i)
                  sync_arr
                  group_res_arrs
                  red_arrs

          one_group_per_segment =
            sComment "first thread in group saves final result to memory" $
              forM2_ slugs segred_pes $ \slug pes ->
                sWhen (local_tid .==. 0) $
                  forM2_ pes (slugAccs slug) $ \v (acc, acc_is) ->
                    copyDWIMFix (patElemName v) (map Imp.le64 segment_gtids) (Var acc) acc_is

      sIf (groups_per_segment .==. 1) one_group_per_segment multiple_groups_per_segment

-- Careful to avoid division by zero here.  We have at least one group
-- per segment.
groupsPerSegmentAndQ ::
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  CallKernelGen
    ( Imp.TExp Int64,
      Imp.TExp Int64
    )
groupsPerSegmentAndQ segment_size num_segments num_groups_hint group_size chunk = do
  groups_per_segment <-
    dPrimVE "groups_per_segment" $
      num_groups_hint `divUp` sMax64 1 num_segments
  q <-
    dPrimVE "q" $
      segment_size `divUp` (group_size * groups_per_segment * chunk)
  pure (groups_per_segment, q)


-- | A SegBinOp with auxiliary information.
data SegBinOpSlug = SegBinOpSlug
  { slugOp :: SegBinOp GPUMem,
    -- | The arrays used for computing the intra-group reduction
    -- (either local or global memory).
    slugInterms :: BinOpIntermediateArrays,
    -- | Places to store accumulator in stage 1 reduction.
    slugAccs :: [(VName, [Imp.TExp Int64])]
  }

slugBody :: SegBinOpSlug -> Body GPUMem
slugBody = lambdaBody . segBinOpLambda . slugOp

slugParams :: SegBinOpSlug -> [LParam GPUMem]
slugParams = lambdaParams . segBinOpLambda . slugOp

slugNeutral :: SegBinOpSlug -> [SubExp]
slugNeutral = segBinOpNeutral . slugOp

slugShape :: SegBinOpSlug -> Shape
slugShape = segBinOpShape . slugOp

slugsComm :: [SegBinOpSlug] -> Commutativity
slugsComm = binopsComm . map slugOp

binopsComm :: [SegBinOp GPUMem] -> Commutativity
binopsComm = mconcat . map segBinOpComm

slugGroupRedArrs :: SegBinOpSlug -> [VName]
slugGroupRedArrs = groupRedArrs . slugInterms

slugPrivChunks :: SegBinOpSlug -> [VName]
slugPrivChunks = privateChunks . slugInterms

slugCollCopyArrs :: SegBinOpSlug -> [VName]
slugCollCopyArrs = collCopyArrs . slugInterms

slugSplitParams :: SegBinOpSlug -> ([LParam GPUMem], [LParam GPUMem])
slugSplitParams slug = splitAt (length (slugNeutral slug)) $ slugParams slug

segBinOpSlug ::
  Imp.TExp Int32 ->
  Imp.TExp Int32 ->
  (SegBinOp GPUMem, BinOpIntermediateArrays, [VName]) ->
  InKernelGen SegBinOpSlug
segBinOpSlug local_tid group_id (op, interms, param_arrs) =
  SegBinOpSlug op interms
    <$> zipWithM mkAcc (lambdaParams (segBinOpLambda op)) param_arrs
  where
    mkAcc p param_arr
      | Prim t <- paramType p,
        shapeRank (segBinOpShape op) == 0 = do
          group_res_acc <- dPrim (baseString (paramName p) <> "_group_res_acc") t
          pure (tvVar group_res_acc, [])
      | otherwise =
          pure (param_arr, [sExt64 local_tid, sExt64 group_id])

reductionStageOne ::
  [VName] ->
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  [SegBinOpSlug] ->
  DoSegBody ->
  InKernelGen [Lambda GPUMem]
reductionStageOne gtids num_elements global_tid q chunk threads_per_segment slugs body_cont = do
  constants <- kernelConstants <$> askEnv
  let glb_ind = mkTV (last gtids) int64
      ltid = sExt64 $ kernelLocalThreadId constants

  dScope Nothing $ scopeOfLParams $ concatMap slugParams slugs

  -- TODO: this/these accumulator/s should be used for the per-group
  -- accumulation in the prim-param case!
  sComment "ne-initialise the outer (per-group) accumulator(s)" $ do
    forM_ slugs $ \slug ->
      forM2_ (slugAccs slug) (slugNeutral slug) $ \(acc, acc_is) ne ->
        sLoopNest (slugShape slug) $ \vec_is ->
          copyDWIMFix acc (acc_is ++ vec_is) ne []

  slugs_op_renamed <- mapM (renameLambda . segBinOpLambda . slugOp) slugs

  let comm = slugsComm slugs
      is_comm = comm == Commutative
      group_size = kernelGroupSize constants

  let doLMemGroupRed =
        forM2_ slugs_op_renamed slugs $ \slug_op_renamed slug -> do
          let accs = slugAccs slug
          sLoopNest (slugShape slug) $ \vec_is -> do
            let group_red_arrs = slugGroupRedArrs slug
            sComment "before intra-group reduction, store accs in lmem" $ do
              forM2_ (slugParams slug) accs $ \p (acc, acc_is) ->
                copyDWIMFix (paramName p) [] (Var acc) (acc_is ++ vec_is)

              -- TODO: what is this?
              forM2_ group_red_arrs (slugParams slug) $ \arr p ->
                when (isPrimParam p) $
                  copyDWIMFix arr [ltid] (Var $ paramName p) []

            sOp $ Imp.ErrorSync Imp.FenceLocal -- Also implicitly barrier.
            groupReduce (sExt32 group_size) slug_op_renamed group_red_arrs
            -- TODO: I don't think this barrier is necessary, considering the
            -- below two loops only write private accumulator variables, and we
            -- have an error sync following the group reduction.
            -- sOp $ Imp.Barrier Imp.FenceLocal

            -- accumulate group results in per-group accumulators. these are
            -- only meaningful for thread 0, but no guard is necessary because
            -- only thread 0 will ever be reading this value later on.
            --
            -- TODO: for the non-primitive case, we actually *do* need to guard
            -- the write, since we are going to be using the global mem group
            -- result arrays as accumulators for the duration of stage one.
            -- see below commented-out block of code.
            sComment "accumulate in the per-group accumulator(s)" $ do
              forM2_ accs (lambdaParams slug_op_renamed) $
                \(acc, acc_is) p ->
                  copyDWIMFix acc (acc_is ++ vec_is) (Var $ paramName p) []

            sWhen (ltid .>. 0) $
              sComment "all but thread 0 reset to ne(s)" $
                forM2_ accs (slugNeutral slug) $
                  \(acc, acc_is) ne ->
                    copyDWIMFix acc (acc_is ++ vec_is) ne []



            -- if non_prim
            -- sComment "first thread saves result(s) in acc(s)" $
            --   -- TODO: is this guard necessary?
            --   -- in stageTwo, only the first thread uses this value anyway.
            --   -- the answer might be in more complex cases (eg. multiple
            --   -- reduction arrays)
            --   -- additionally: do we ever need to reset to NE? is the variable
            --   -- read before it is written somewhere earlier in the loop body?
            --   sIf (ltid .==. 0)
            --     ( forM_ (zip (slugAccs slug) (lambdaParams slug_op_renamed)) $
            --         \((acc, acc_is), p) ->
            --           copyDWIMFix acc (acc_is ++ vec_is) (Var $ paramName p) []
            --     )
            --     ( id $ -- when (not is_comm) $
            --         sComment "others reset to ne(s) (in case of non-comm reduction)" $
            --           forM_ (zip (slugAccs slug) (slugNeutral slug)) $
            --             \((acc, acc_is), ne) ->
            --               copyDWIMFix acc (acc_is ++ vec_is) ne []
            --     )


  -- If this is a non-commutative reduction, each thread must run the
  -- loop the same number of iterations, because we will be performing
  -- a group-wide reduction in there.
  -- TODO: reduce and reduce-segmented benchmarks indicate that there is no
  -- significant gain in performance from differentiating between commutative
  -- and noncommutative here (the `Noncommutative` case can be used for both to
  -- simplify code generation)

  -- this group's offset into its designated segment. for the non-segmented
  -- case, this simply reduces to group_id (ie. blockIdx.x).
  group_offs_in_segment <- dPrimVE "group_offset_in_segment" $ global_tid `quot` group_size
  -- the stride made per group in the outer `i < q` loop.
  group_stride <- dPrimVE "group_stride" $ group_size * chunk
  -- this group's initial global offset.
  group_offs <- dPrimVE "group_offset" $ group_offs_in_segment * q * group_stride


    -- TODO: for now we ignore the case of non-prim-parameterized non-comm
    -- operators. hopefully this case is as simple as setting chunk = 1, such
    -- that we don't have to interleave the comm and non-comm code generation
    -- too much, but we should probably consult Troels on this.
  sFor "i" q $ \i -> do
    glb_offset <- dPrimVE "global_offset" $ group_offs + i * group_stride
    if not is_comm then do
      let chunkLoop = sFor "k" chunk

      chunkLoop $ \k -> do
        loc_ind <- dPrimVE "loc_ind" $ k * group_size + sExt64 ltid
        glb_ind <-- glb_offset + loc_ind

        sIf
          (tvExp glb_ind .<. num_elements)
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

      -- TODO: is this barrier necessary? the above block of code only reads
      -- elements from global, maps them, and writes them to private mem.
      sOp $ Imp.ErrorSync Imp.FenceLocal 

      sComment "effectualize collective copies in local memory" $ do
        -- TODO: once again, do we need to take care about the slugShape here,
        -- in other words should we have a `sLoopNest (slugShape slug) ..`?
        forM_ slugs $ \slug -> do
          let coll_copy_arrs = slugCollCopyArrs slug
          let priv_chunks = slugPrivChunks slug
          let do_second_barrier = length coll_copy_arrs > 1
          forM2_ coll_copy_arrs priv_chunks $ \lmem_arr priv_chunk -> do
            chunkLoop $ \k -> do
              lmem_idx <- dPrimVE "lmem_idx" $ ltid + k * group_size
              copyDWIMFix lmem_arr [lmem_idx] (Var priv_chunk) [k]

            sOp $ Imp.Barrier Imp.FenceLocal

            chunkLoop $ \k -> do
              lmem_idx <- dPrimVE "lmem_idx" $ ltid * chunk + k
              copyDWIMFix priv_chunk [k] (Var lmem_arr) [lmem_idx]

            when do_second_barrier $ sOp $ Imp.Barrier Imp.FenceLocal

      sComment "per-thread sequential reduction of private chunk(s)" $ do
        chunkLoop $ \k ->
          forM_ slugs $ \slug -> do

            let accs = map fst $ slugAccs slug
            let (acc_ps, next_ps) = slugSplitParams slug
            let ps_accs_chunks = zip4 acc_ps next_ps accs (slugPrivChunks slug)

            sComment "load params for all reductions" $ do
              forM_ ps_accs_chunks $ \(acc_p, next_p, acc, priv_chunk) -> do
                copyDWIMFix (paramName acc_p) [] (Var acc) []
                copyDWIMFix (paramName next_p) [] (Var priv_chunk) [k]

            sComment "apply reduction operator(s)" $ do
              let binop_ress = map resSubExp $ bodyResult $ slugBody slug
              compileStms mempty (bodyStms $ slugBody slug) $
                forM2_ accs binop_ress $ \acc binop_res ->
                  copyDWIMFix acc [] binop_res []

        -- sComment "write chunk reduction(s) to outer accumulator(s)" $ do
        --   forM2_ slugs priv_accss $ \slug priv_accs -> do
        --     let accs = slugAccs slug
        --     forM2_ accs priv_accs $ \(acc, acc_is) priv_acc ->
        --       -- TODO: slugShape??
        --       copyDWIMFix acc acc_is (Var priv_acc) []

      sComment_ "noncomm doLMemGroupRed BEGIN"
      doLMemGroupRed
      sComment_ "noncomm doLMemGroupRed END"
      sOp $ Imp.ErrorSync Imp.FenceLocal

    else do
      glb_ind <-- case comm of
        Commutative -> global_tid + threads_per_segment * i
        Noncommutative -> glb_offset + sExt64 ltid
      sWhen (tvExp glb_ind .<. num_elements) $
        sComment "apply map function(s)" $
          body_cont $ \all_red_res -> do
            let maps_res = chunks (map (length . slugNeutral) slugs) all_red_res

            forM_ (zip slugs maps_res) $ \(slug, map_res) ->
              sLoopNest (slugShape slug) $ \vec_is -> do
                let (acc_params, next_params) = slugSplitParams slug
                sComment "load accumulator(s)" $
                  forM_ (zip acc_params (slugAccs slug)) $ \(p, (acc, acc_is)) ->
                    copyDWIMFix (paramName p) [] (Var acc) (acc_is ++ vec_is)
                sComment "load next value(s)" $
                  forM_ (zip next_params map_res) $ \(p, (res, res_is)) ->
                    copyDWIMFix (paramName p) [] res (res_is ++ vec_is)
                sComment "apply reduction operator(s)" $
                  compileStms mempty (bodyStms $ slugBody slug)
                    $ sComment "store in accumulator(s)"
                    $ forM_
                      ( zip
                          (slugAccs slug)
                          (map resSubExp $ bodyResult $ slugBody slug)
                      )
                    $ \((acc, acc_is), se) ->
                      copyDWIMFix acc (acc_is ++ vec_is) se []

  when is_comm $ do
    sOp $ Imp.ErrorSync Imp.FenceLocal
    sComment_ "comm doLMemGroupRed BEGIN"
    doLMemGroupRed
    sComment_ "comm doLMemGroupRed END"

  pure slugs_op_renamed

reductionStageTwo ::
  KernelConstants ->
  [PatElem LetDecMem] ->
  Imp.TExp Int32 ->
  Imp.TExp Int32 ->
  [Imp.TExp Int64] ->
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  SegBinOpSlug ->
  [LParam GPUMem] ->
  [LParam GPUMem] ->
  Lambda GPUMem ->
  [SubExp] ->
  Imp.TExp Int32 ->
  VName ->
  Imp.TExp Int32 ->
  VName ->
  [VName] ->
  [VName] ->
  InKernelGen ()
reductionStageTwo
  constants
  segred_pes
  group_id
  flat_segment_id
  segment_gtids
  first_group_for_segment
  groups_per_segment
  slug
  red_x_params
  red_y_params
  red_op_renamed
  nes
  num_counters
  counter
  counter_i
  sync_arr
  group_res_arrs
  red_arrs = do
    let ltid32 = kernelLocalThreadId constants
        ltid = sExt64 ltid32
        group_size = kernelGroupSize constants
    old_counter <- dPrim "old_counter" int32
    (counter_mem, _, counter_offset) <-
      fullyIndexArray
        counter
        [ sExt64 $
            counter_i * num_counters
              + flat_segment_id `rem` num_counters
        ]
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
              (groups_per_segment - ltid) `divUp` sExt64 group_size

          forM_ (zip red_x_params nes) $ \(p, ne) ->
            copyDWIMFix (paramName p) [] ne []

          sFor "i" read_per_thread $ \i -> do
            group_res_id <-
              dPrimVE "group_res_id" $
                ltid * read_per_thread + i
            index_of_group_res <-
              dPrimVE "index_of_group_res" $
                first_group_for_segment + group_res_id

            sWhen (group_res_id .<. groups_per_segment) $ do
              forM_ (zip red_y_params group_res_arrs) $
                \(p, group_res_arr) ->
                  copyDWIMFix
                    (paramName p)
                    []
                    (Var group_res_arr)
                    ([0, index_of_group_res] ++ vec_is)

              compileStms mempty (bodyStms $ slugBody slug) $
                forM_ (zip red_x_params $ map resSubExp $ bodyResult $ slugBody slug) $ \(p, se) ->
                  copyDWIMFix (paramName p) [] se []

        forM_ (zip red_x_params red_arrs) $ \(p, arr) ->
          when (isPrimParam p) $
            copyDWIMFix arr [ltid] (Var $ paramName p) []

        sOp $ Imp.ErrorSync Imp.FenceLocal

        sComment "reduce the per-group results" $ do
          groupReduce (sExt32 group_size) red_op_renamed red_arrs

          sComment "and back to memory with the final result" $
            sWhen (ltid32 .==. 0) $
              forM_ (zip segred_pes $ lambdaParams red_op_renamed) $ \(pe, p) ->
                copyDWIMFix
                  (patElemName pe)
                  (segment_gtids ++ vec_is)
                  (Var $ paramName p)
                  []
