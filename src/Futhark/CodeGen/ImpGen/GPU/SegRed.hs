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
import Data.List (genericLength, zip7)
import Data.Maybe
import Debug.Trace
import Futhark.CodeGen.ImpCode.GPU qualified as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.GPU.Base
import Futhark.Error
import Futhark.IR.GPUMem
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.Transform.Rename
import Futhark.Util (chunks, forAccumLM)
import Futhark.Util.IntegralExp (divUp, quot, rem, nextMul)
import Prelude hiding (quot, rem)

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
    compileStms mempty (kernelBodyStms body) $ do
      let (red_res, map_res) = splitAt (segBinOpResults reds) $ kernelBodyResult body

      sComment "save map-out results" $ do
        let map_arrs = drop (segBinOpResults reds) $ patElems pat
        zipWithM_ (compileThreadResult space) map_arrs map_res

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
compileSegRed' pat grid space reds body
  | genericLength reds > maxNumOps =
      compilerLimitationS $
        "compileSegRed': at most " ++ show maxNumOps ++ " reduction operators are supported."
  | [(_, Constant (IntValue (Int64Value 1))), _] <- unSegSpace space =
      nonsegmentedReduction pat num_groups group_size space reds body
  | otherwise = do
      let group_size' = pe64 $ unCount group_size
          segment_size = pe64 $ last $ segSpaceDims space
          use_small_segments = segment_size * 2 .<. group_size'
      sIf
        use_small_segments
        (smallSegmentsReduction pat num_groups group_size space reds body)
        (largeSegmentsReduction pat num_groups group_size space reds body)
  where
    num_groups = gridNumGroups grid
    group_size = gridGroupSize grid

-- | Prepare intermediate arrays for the reduction.  Prim-typed
-- arguments go in local memory (so we need to do the allocation of
-- those arrays inside the kernel), while array-typed arguments go in
-- global memory.  Allocations for the former have already been
-- performed.  This policy is baked into how the allocations are done
-- in ExplicitAllocations.

-- data ReduceType = NoncommPrimReduce | CommOrNonprimReduce
data ReduceIntermediateArrays =
    CommReduceInterms
      { groupRedLMem :: [[VName]]
      }
  | NoncommPrimReduceInterms
      { collCopyLMem :: [[VName]],
        groupRedLMem :: [[VName]]
      }


makeLocalArrays ::
  Count GroupSize SubExp ->
  TPrimExp Int64 VName ->
  TV Int64 ->
  [SegBinOp GPUMem] ->
  InKernelGen ReduceIntermediateArrays
makeLocalArrays (Count group_size) chunk num_threads segbinops
  | is_noncommutative_prim_red =
      localArraysNoncomm

  | otherwise =
    fmap CommReduceInterms $
      forM segbinops $ \segbinop ->
        forM (paramOf segbinop) $ \p ->
          case paramDec p of
            MemArray pt shape _ (ArrayIn mem _) -> do
              let shape' = Shape [tvSize num_threads] <> shape
              sArray "red_arr" pt shape' mem $
                LMAD.iota 0 (map pe64 $ shapeDims shape')
            _ -> do
              let pt = elemType $ paramType p
                  shape = Shape [group_size]
              sAllocArray ("red_arr_" ++ prettyString pt) pt shape $ Space "local"

  -- | otherwise =
  --   error $ "Non-commutative reduction with non-primitive reduction param(s) "
  --             <> "not yet handled! :')\nGot SegBinOp params: "
  --             <> (prettyString $ map paramOf segbinops)
  --             <> "\n\n"

  where

    -- TODO: this decision (of whether to declare interms for a commutative or a
    -- noncommutative and prim-parameterized reduction) should not be made here,
    -- but rather at the top level (compileSegRed or compileSegred').
    is_noncommutative_prim_red =
      binopsComm segbinops == Commutative
        && all (all (isPrimParam . paramDec) . paramOf) segbinops


    isPrimParam (MemArray _ _ _ _) = False
    isPrimParam _ = True

    paramOf (SegBinOp _ op ne _) = take (length ne) $ lambdaParams op

    localArraysNoncomm = do
      let group_size_E = pe64 group_size
          group_worksize_E = group_size_E * chunk

      group_worksize <- tvSize <$> (dPrimV "group_worksize" group_worksize_E)

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


      -- get param elem sizes of all the binops for which we need to store
      -- intermediate arrays in local mem.
      let paramSize = primByteSize . elemType . paramType
          elem_sizes = concatMap (map paramSize . paramOf) segbinops


          -- TODO: should we align to each elem_size or to 8?
          --
          -- TODO: additionally: should we align each array length individually
          -- before summing, or should each prefix sum be aligned to the value
          -- of the next element in a fold? because there is a difference when
          -- there exists an elem_size which does not divide group_size (but
          -- this may not be a problem, eg. if group_size is required to be a
          -- mult of 32).

          group_reds_lmem_req =
            foldl (\x y -> nextMul x y + y * group_size_E) 0 elem_sizes

          collcopy_lmem_req = group_worksize_E * maximum elem_sizes

          lmem_total_size =
            Imp.bytes $
              collcopy_lmem_req `sMax64`
              group_reds_lmem_req

      -- allocate total pool of local mem.
      lmem <- sAlloc "local_mem" lmem_total_size (Space "local")

      let arrInLMem p name len_se offset = do
            let ptype = elemType $ paramType p
            sArray
              (name ++ "_" ++ prettyString ptype)
              ptype
              (Shape [len_se])
              lmem
              $ LMAD.iota offset [pe64 len_se]

      (coll_copy_arrs, group_red_arrs) <-
        fmap (unzip . map unzip . snd) $
          forAccumLM 0 segbinops $ \offset_outer segbinop ->
            forAccumLM offset_outer (paramOf segbinop) $ \offset p -> do
              let elem_size = paramSize p
                  -- TODO: again, should these be aligned to elem_size instead?
                  offset_aligned = nextMul offset elem_size
                  offset' = offset_aligned + group_size_E * elem_size

              fmap (offset', ) $
                (,) <$> arrInLMem p "coll_copy_arr" group_worksize 0
                    <*> arrInLMem p "group_red_arr" group_size (offset `quot` elem_size)

      pure $
        NoncommPrimReduceInterms
          { collCopyLMem = coll_copy_arrs,
            groupRedLMem = group_red_arrs
          }


-- | Arrays for storing group results.
--
-- The group-result arrays have an extra dimension because they are
-- also used for keeping vectorised accumulators for first-stage
-- reduction, if necessary.  If necessary, this dimension has size
-- group_size, and otherwise 1.  When actually storing group results,
-- the first index is set to 0.
groupResultArrays ::
  Count NumGroups SubExp ->
  Count GroupSize SubExp ->
  [SegBinOp GPUMem] ->
  CallKernelGen [[VName]]
groupResultArrays (Count virt_num_groups) (Count group_size) reds =
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
  SegSpace ->
  [SegBinOp GPUMem] ->
  DoSegBody ->
  CallKernelGen ()
nonsegmentedReduction segred_pat num_groups group_size space reds body = do
  let (gtids, dims) = unzip $ unSegSpace space
      dims' = map pe64 dims
      num_groups' = fmap pe64 num_groups
      group_size' = fmap pe64 group_size
      global_tid = Imp.le64 $ segFlat space
      w = last dims'

  counter <- genZeroes "counters" $ fromIntegral maxNumOps

  reds_group_res_arrs <- groupResultArrays num_groups group_size reds

  num_threads <-
    dPrimV "num_threads" $
      unCount num_groups' * unCount group_size'

  sKernelThread "segred_nonseg" (segFlat space) (defKernelAttrs num_groups group_size) $ do
    constants <- kernelConstants <$> askEnv
    sync_arr <- sAllocArray "sync_arr" Bool (Shape [intConst Int32 1]) $ Space "local"
    local_arrs <- makeLocalArrays group_size 1 num_threads reds
    let reds_arrs = groupRedLMem local_arrs

    -- Since this is the nonsegmented case, all outer segment IDs must
    -- necessarily be 0.
    forM_ gtids $ \v -> dPrimV_ v (0 :: Imp.TExp Int64)

    let num_elements = Imp.elements w
        elems_per_thread =
          num_elements
            `divUp` Imp.elements (sExt64 (kernelNumThreads constants))

    slugs <-
      mapM (segBinOpSlug (kernelLocalThreadId constants) (kernelGroupId constants)) $
        zip3 reds reds_arrs reds_group_res_arrs
    sComment_ "stage one BEGIN"
    red_ops_renamed <-
      reductionStageOne
        constants
        (zip gtids dims')
        num_elements
        global_tid
        elems_per_thread
        (tvExp num_threads)
        slugs
        body

    sComment_ "stage one END"
    let segred_pes =
          chunks (map (length . segBinOpNeutral) reds) $
            patElems segred_pat
    forM_ (zip7 reds reds_arrs reds_group_res_arrs segred_pes slugs red_ops_renamed [0 ..]) $
      \(SegBinOp _ red_op nes _, red_arrs, group_res_arrs, pes, slug, red_op_renamed, i) -> do
        let (red_x_params, red_y_params) = splitAt (length nes) $ lambdaParams red_op
        sComment ("stage two BEGIN") $ pure ()
        reductionStageTwo
          constants                                -- constants
          pes                                      -- segred_pes
          (kernelGroupId constants)                -- group_id
          0                                        -- flat_segment_id
          [0]                                      -- segment_gtids
          0                                        -- first_group_for_segment
          (sExt64 $ kernelNumGroups constants)     -- groups_per_segment
          slug                                     -- slug
          red_x_params                             -- red_x_params
          red_y_params                             -- red_y_params
          red_op_renamed                           -- red_op_renamed
          nes                                      -- nes
          1                                        -- num_counters
          counter                                  -- counter
          (fromInteger i)                          -- counter_i
          sync_arr                                 -- sync_arr
          group_res_arrs                           -- group_res_arrs
          red_arrs                                 -- red_arrs
        sComment_ "stage two END"

smallSegmentsReduction ::
  Pat LetDecMem ->
  Count NumGroups SubExp ->
  Count GroupSize SubExp ->
  SegSpace ->
  [SegBinOp GPUMem] ->
  DoSegBody ->
  CallKernelGen ()
smallSegmentsReduction (Pat segred_pes) num_groups group_size space reds body = do
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
    tmp <- makeLocalArrays group_size 1 num_threads reds
    let reds_arrs = groupRedLMem tmp

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
            forM_ (zip reds reds_arrs) $ \(SegBinOp _ _ nes _, red_arrs) ->
              forM_ (zip red_arrs nes) $ \(arr, ne) ->
                copyDWIMFix arr [ltid] ne []

          in_bounds =
            body $ \red_res ->
              sComment "save results to be reduced" $ do
                let red_dests = map (,[ltid]) (concat reds_arrs)
                forM_ (zip red_dests red_res) $ \((d, d_is), (res, res_is)) ->
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
          forM_ (zip reds reds_arrs) $ \(SegBinOp _ red_op _ _, red_arrs) ->
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
        $ forM_ (zip segred_pes (concat reds_arrs))
        $ \(pe, arr) -> do
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
  SegSpace ->
  [SegBinOp GPUMem] ->
  DoSegBody ->
  CallKernelGen ()
largeSegmentsReduction segred_pat num_groups group_size space reds body = do
  let (gtids, dims) = unzip $ unSegSpace space
      dims' = map pe64 dims
      num_segments = product $ init dims'
      segment_size = last dims'
      num_groups' = fmap pe64 num_groups
      group_size' = fmap pe64 group_size

  (groups_per_segment, elems_per_thread) <-
    groupsPerSegmentAndElementsPerThread
      segment_size
      num_segments
      num_groups'
      group_size'
  virt_num_groups <-
    dPrimV "virt_num_groups" $
      groups_per_segment * num_segments

  num_threads <-
    dPrimV "num_threads" $
      unCount num_groups' * unCount group_size'

  threads_per_segment <-
    dPrimV "threads_per_segment" $
      groups_per_segment * unCount group_size'

  emit $ Imp.DebugPrint "# SegRed-large" Nothing
  emit $ Imp.DebugPrint "num_segments" $ Just $ untyped num_segments
  emit $ Imp.DebugPrint "segment_size" $ Just $ untyped segment_size
  emit $ Imp.DebugPrint "virt_num_groups" $ Just $ untyped $ tvExp virt_num_groups
  emit $ Imp.DebugPrint "num_groups" $ Just $ untyped $ Imp.unCount num_groups'
  emit $ Imp.DebugPrint "group_size" $ Just $ untyped $ Imp.unCount group_size'
  emit $ Imp.DebugPrint "elems_per_thread" $ Just $ untyped $ Imp.unCount elems_per_thread
  emit $ Imp.DebugPrint "groups_per_segment" $ Just $ untyped groups_per_segment

  reds_group_res_arrs <- groupResultArrays (Count (tvSize virt_num_groups)) group_size reds

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
    tmp <- makeLocalArrays group_size 1 num_threads reds
    let reds_arrs = groupRedLMem tmp
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
          (sExt64 group_id * sExt64 (unCount group_size') + sExt64 local_tid)
            `rem` (sExt64 (unCount group_size') * groups_per_segment)

      let first_group_for_segment = sExt64 flat_segment_id * groups_per_segment
      dIndexSpace (zip segment_gtids (init dims')) $ sExt64 flat_segment_id
      dPrim_ (last gtids) int64
      let num_elements = Imp.elements $ pe64 w

      slugs <-
        mapM (segBinOpSlug local_tid group_id) $
          zip3 reds reds_arrs reds_group_res_arrs
      red_ops_renamed <-
        reductionStageOne
          constants
          (zip gtids dims')
          num_elements
          global_tid
          elems_per_thread
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
                  constants                         -- constants
                  pes                               -- segred_pes
                  group_id                          -- group_id
                  flat_segment_id                   -- flat_segment_id
                  (map Imp.le64 segment_gtids)      -- segment_gtids
                  (sExt64 first_group_for_segment)  -- first_group_for_segment
                  groups_per_segment                -- groups_per_segment
                  slug                              -- slug
                  red_x_params                      -- red_x_params
                  red_y_params                      -- red_y_params
                  red_op_renamed                    -- red_op_renamed
                  nes                               -- nes
                  (fromIntegral num_counters)       -- num_counters
                  counter                           -- counter
                  (fromInteger i)                   -- counter_i
                  sync_arr                          -- sync_arr
                  group_res_arrs                    -- group_res_arrs
                  red_arrs                          -- red_arrs

          one_group_per_segment =
            sComment "first thread in group saves final result to memory" $
              forM_ (zip slugs segred_pes) $ \(slug, pes) ->
                sWhen (local_tid .==. 0) $
                  forM_ (zip pes (slugAccs slug)) $ \(v, (acc, acc_is)) ->
                    copyDWIMFix (patElemName v) (map Imp.le64 segment_gtids) (Var acc) acc_is

      sIf (groups_per_segment .==. 1) one_group_per_segment multiple_groups_per_segment

-- Careful to avoid division by zero here.  We have at least one group
-- per segment.
groupsPerSegmentAndElementsPerThread ::
  Imp.TExp Int64 ->
  Imp.TExp Int64 ->
  Count NumGroups (Imp.TExp Int64) ->
  Count GroupSize (Imp.TExp Int64) ->
  CallKernelGen
    ( Imp.TExp Int64,
      Imp.Count Imp.Elements (Imp.TExp Int64)
    )
groupsPerSegmentAndElementsPerThread segment_size num_segments num_groups_hint group_size = do
  groups_per_segment <-
    dPrimVE "groups_per_segment" $
      unCount num_groups_hint `divUp` sMax64 1 num_segments
  elements_per_thread <-
    dPrimVE "elements_per_thread" $
      segment_size `divUp` (unCount group_size * groups_per_segment)
  pure (groups_per_segment, Imp.elements elements_per_thread)

-- | A SegBinOp with auxiliary information.
data SegBinOpSlug = SegBinOpSlug
  { slugOp :: SegBinOp GPUMem,
    -- | The arrays used for computing the intra-group reduction
    -- (either local or global memory).
    slugArrs :: [VName],
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


accParams, nextParams :: SegBinOpSlug -> [LParam GPUMem]
accParams slug = take (length (slugNeutral slug)) $ slugParams slug
nextParams slug = drop (length (slugNeutral slug)) $ slugParams slug

segBinOpSlug :: Imp.TExp Int32 -> Imp.TExp Int32 -> (SegBinOp GPUMem, [VName], [VName]) -> InKernelGen SegBinOpSlug
segBinOpSlug local_tid group_id (op, group_res_arrs, param_arrs) =
  SegBinOpSlug op group_res_arrs
    <$> zipWithM mkAcc (lambdaParams (segBinOpLambda op)) param_arrs
  where
    mkAcc p param_arr
      | Prim t <- paramType p,
        shapeRank (segBinOpShape op) == 0 = do
          acc <- dPrim (baseString (paramName p) <> "_acc") t
          pure (tvVar acc, [])
      | otherwise =
          pure (param_arr, [sExt64 local_tid, sExt64 group_id])

reductionStageOne ::
  KernelConstants ->
  [(VName, Imp.TExp Int64)] ->
  Imp.Count Imp.Elements (Imp.TExp Int64) ->
  Imp.TExp Int64 ->
  Imp.Count Imp.Elements (Imp.TExp Int64) ->
  Imp.TExp Int64 ->
  [SegBinOpSlug] ->
  DoSegBody ->
  InKernelGen [Lambda GPUMem]
reductionStageOne constants ispace num_elements global_tid elems_per_thread threads_per_segment slugs body = do
  let (gtids, _dims) = unzip ispace
      gtid = mkTV (last gtids) int64
      local_tid = sExt64 $ kernelLocalThreadId constants

  dScope Nothing $ scopeOfLParams $ concatMap slugParams slugs

  sComment "neutral-initialise the accumulators" $
    forM_ slugs $ \slug ->
      forM_ (zip (slugAccs slug) (slugNeutral slug)) $ \((acc, acc_is), ne) ->
        sLoopNest (slugShape slug) $ \vec_is ->
          copyDWIMFix acc (acc_is ++ vec_is) ne []

  slugs_op_renamed <- mapM (renameLambda . segBinOpLambda . slugOp) slugs

  let doTheReduction = do
        sComment_ "doTheReduction INNER BEGIN"
        forM_ (zip slugs_op_renamed slugs) $ \(slug_op_renamed, slug) ->
          sLoopNest (slugShape slug) $ \vec_is -> do
            sComment "to reduce current chunk, first store our result in memory" $ do
              forM_ (zip (slugParams slug) (slugAccs slug)) $ \(p, (acc, acc_is)) ->
                copyDWIMFix (paramName p) [] (Var acc) (acc_is ++ vec_is)

              forM_ (zip (slugArrs slug) (slugParams slug)) $ \(arr, p) ->
                when (primType $ paramType p) $
                  copyDWIMFix arr [local_tid] (Var $ paramName p) []

            sOp $ Imp.ErrorSync Imp.FenceLocal -- Also implicitly barrier.
            groupReduce (sExt32 (kernelGroupSize constants)) slug_op_renamed (slugArrs slug)

            sOp $ Imp.Barrier Imp.FenceLocal

            sComment "first thread saves the result in accumulator" $
              sWhen (local_tid .==. 0) $
                forM_ (zip (slugAccs slug) (lambdaParams slug_op_renamed)) $ \((acc, acc_is), p) ->
                  copyDWIMFix acc (acc_is ++ vec_is) (Var $ paramName p) []

        sComment_ "doTheReduction INNER END"

  -- If this is a non-commutative reduction, each thread must run the
  -- loop the same number of iterations, because we will be performing
  -- a group-wide reduction in there.
  -- TODO: reduce and reduce-segmented benchmarks indicate that there is no
  -- significant gain in performance from differentiating between commutative
  -- and noncommutative here (the `Noncommutative` case can be used for both)
  let comm = slugsComm slugs
      is_comm = comm == Commutative
      (bound, check_bounds) =
        case comm of
          Commutative ->
            ( sMin64
                (Imp.unCount elems_per_thread)
                ((Imp.unCount num_elements - (sExt64 global_tid))
                  `divUp` threads_per_segment),
              id
            )
          Noncommutative ->
            ( Imp.unCount elems_per_thread,
              sWhen (tvExp gtid .<. Imp.unCount num_elements)
            )

  sFor "i" bound $ \i -> do
    gtid
      <-- case comm of
        Commutative ->
          global_tid + threads_per_segment * i
        Noncommutative ->
          let index_in_segment = global_tid `quot` kernelGroupSize constants
           in sExt64 local_tid
                + (index_in_segment * Imp.unCount elems_per_thread + i)
                  * kernelGroupSize constants

    check_bounds $
      sComment "apply map function" $
        body $ \all_red_res -> do
          let slugs_res = chunks (map (length . slugNeutral) slugs) all_red_res

          forM_ (zip slugs slugs_res) $ \(slug, red_res) ->
            sLoopNest (slugShape slug) $ \vec_is -> do
              sComment "load accumulator" $
                forM_ (zip (accParams slug) (slugAccs slug)) $ \(p, (acc, acc_is)) ->
                  copyDWIMFix (paramName p) [] (Var acc) (acc_is ++ vec_is)
              sComment "load new values" $
                forM_ (zip (nextParams slug) red_res) $ \(p, (res, res_is)) ->
                  copyDWIMFix (paramName p) [] res (res_is ++ vec_is)
              sComment "apply reduction operator"
                $ compileStms mempty (bodyStms $ slugBody slug)
                $ sComment "store in accumulator"
                $ forM_
                  ( zip
                      (slugAccs slug)
                      (map resSubExp $ bodyResult $ slugBody slug)
                  )
                $ \((acc, acc_is), se) ->
                  copyDWIMFix acc (acc_is ++ vec_is) se []

    when (not is_comm) $ do
      sComment_ "noncomm doTheReduction BEGIN"
      doTheReduction
      sComment "first thread keeps accumulator; others reset to neutral element" $ do
        let reset_to_neutral =
              forM_ slugs $ \slug ->
                forM_ (zip (slugAccs slug) (slugNeutral slug)) $ \((acc, acc_is), ne) ->
                  sLoopNest (slugShape slug) $ \vec_is ->
                    copyDWIMFix acc (acc_is ++ vec_is) ne []
        sWhen (local_tid .>. 0) reset_to_neutral
      sComment_ "noncomm doTheReduction END"

  sOp $ Imp.ErrorSync Imp.FenceLocal

  when is_comm doTheReduction

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
    let local_tid = kernelLocalThreadId constants
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
      sWhen (local_tid .==. 0) $ do
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
      sWhen (local_tid .==. 0) $
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

          forM_ (zip red_x_params nes) $ \(p, ne) ->
            copyDWIMFix (paramName p) [] ne []

          sFor "i" read_per_thread $ \i -> do
            group_res_id <-
              dPrimVE "group_res_id" $
                sExt64 local_tid * read_per_thread + i
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
          when (primType $ paramType p) $
            copyDWIMFix arr [sExt64 local_tid] (Var $ paramName p) []

        sOp $ Imp.ErrorSync Imp.FenceLocal

        sComment "reduce the per-group results" $ do
          groupReduce (sExt32 group_size) red_op_renamed red_arrs

          sComment "and back to memory with the final result" $
            sWhen (local_tid .==. 0) $
              forM_ (zip segred_pes $ lambdaParams red_op_renamed) $ \(pe, p) ->
                copyDWIMFix
                  (patElemName pe)
                  (segment_gtids ++ vec_is)
                  (Var $ paramName p)
                  []
