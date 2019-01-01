{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
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
--   'groupsize'-sized window over the input, and perform a parallel
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
module Futhark.CodeGen.ImpGen.Kernels.SegRed
  ( compileSegRed
  )
  where

import Control.Monad.Except
import Data.Maybe
import Data.Semigroup ((<>))
import qualified Data.Set as S
import Data.List

import Prelude hiding (quot, rem)

import Futhark.MonadFreshNames
import Futhark.Transform.Rename
import Futhark.Representation.ExplicitMemory
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import qualified Futhark.CodeGen.ImpGen as ImpGen
import Futhark.CodeGen.ImpGen ((<--),
                               sFor, sComment, sIf, sWhen,
                               sOp,
                               dPrim, dPrimV)
import Futhark.CodeGen.ImpGen.Kernels.Base
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import Futhark.Util.IntegralExp (quotRoundingUp, quot, rem)

-- | For many kernels, we may not have enough physical groups to cover
-- the logical iteration space.  Some groups thus have to perform
-- double duty; we put an outer loop to accomplish this.  The
-- advantage over just launching a bazillion threads is that the cost
-- of memory expansion should be proportional to the number of
-- *physical* threads (hardware parallelism), not the amount of
-- application parallelism.
virtualiseGroups :: KernelConstants
                 -> Imp.Exp
                 -> (Imp.Exp -> ImpGen.ImpM lore op ())
                 -> ImpGen.ImpM lore op ()
virtualiseGroups constants required_groups m = do
  let group_id = kernelGroupId constants
      iterations = (required_groups - group_id) `quotRoundingUp` kernelNumGroups constants
  i <- newVName "i"
  sFor i Int32 iterations $ m $ group_id + Imp.var i int32 * kernelNumGroups constants

-- Compile 'SegRed' instance to host-level code with calls to various
-- kernels.
compileSegRed :: Pattern ExplicitMemory
              -> KernelSpace
              -> Commutativity -> Lambda InKernel -> [SubExp]
              -> Body InKernel
              -> CallKernelGen ()
compileSegRed pat space comm red_op nes body
  | [(_, Constant (IntValue (Int32Value 1))), _] <- spaceDimensions space =
      nonsegmentedReduction pat space comm red_op nes body
  | otherwise = do
      segment_size <-
        ImpGen.compileSubExp $ last $ map snd $ spaceDimensions space
      group_size <- ImpGen.compileSubExp $ spaceGroupSize space
      let use_small_segments = segment_size * 2 .<. group_size
      sIf (segment_size .==. 1)
        (unitSegmentsReduction pat space nes body) $
        sIf use_small_segments
        (smallSegmentsReduction pat space red_op nes body)
        (largeSegmentsReduction pat space comm red_op nes body)

-- Handle degenerate case where segments are of size 1, meaning
-- that it is really just a 'map' in disguise.
unitSegmentsReduction :: Pattern ExplicitMemory
                      -> KernelSpace
                      -> [SubExp]
                      -> Body InKernel
                      -> CallKernelGen ()
unitSegmentsReduction (Pattern _ segred_pes) space nes body = do
  (constants, init_constants) <- kernelInitialisationSetSpace space $ return ()

  let (gtids, dims) = unzip $ spaceDimensions space
      (redout_pes, mapout_pes) = splitAt (length nes) segred_pes

  dims' <- mapM ImpGen.compileSubExp dims

  let num_segments = product $ init dims'
      required_groups = num_segments `quotRoundingUp` kernelGroupSize constants

  ImpGen.emit $ Imp.DebugPrint "num_segments" int32 num_segments
  ImpGen.emit $ Imp.DebugPrint "required_groups" int32 required_groups

  sKernel constants "segred_mapseg" $ do
    init_constants
    virtualiseGroups constants required_groups $ \group_id -> do
      setSpaceIndices (group_id * kernelGroupSize constants + kernelLocalThreadId constants) space
      ImpGen.compileStms mempty (stmsToList $ bodyStms body) $
        sWhen (kernelThreadActive constants) $ do
        let (redout_ses, mapout_ses) = splitAt (length nes) $ bodyResult body
        forM_ (zip redout_pes redout_ses) $ \(pe, se) ->
          ImpGen.copyDWIM (patElemName pe)
          (map (`Imp.var` int32) (init gtids)) se []

        forM_ (zip mapout_pes mapout_ses) $ \(pe, se) ->
          ImpGen.copyDWIM (patElemName pe)
          (map (`Imp.var` int32) gtids) se []

nonsegmentedReduction :: Pattern ExplicitMemory
                      -> KernelSpace
                      -> Commutativity -> Lambda InKernel -> [SubExp]
                      -> Body InKernel
                      -> CallKernelGen ()
nonsegmentedReduction segred_pat space comm red_op nes body = do
  (base_constants, init_constants) <- kernelInitialisationSetSpace space $ return ()
  let constants = base_constants { kernelThreadActive = true }
      global_tid = kernelGlobalThreadId constants
      (_, w) = last $ spaceDimensions space

  let red_op_params = lambdaParams red_op
      (red_acc_params, _) = splitAt (length nes) red_op_params
  red_arrs <- forM red_acc_params $ \p ->
    case paramAttr p of
      MemArray pt shape _ (ArrayIn mem _) -> do
        let shape' = Shape [spaceNumThreads space] <> shape
        ImpGen.sArray "red_arr" pt shape' $
          ArrayIn mem $ IxFun.iota $ map (primExpFromSubExp int32) $ shapeDims shape'
      _ -> do
        let pt = elemType $ paramType p
            shape = Shape [spaceGroupSize space]
        ImpGen.sAllocArray "red_arr" pt shape $ Space "local"

  counter <-
    ImpGen.sStaticArray "counter" (Space "device") int32 $
    replicate 1 $ IntValue $ Int32Value 0

  group_res_arrs <- forM (lambdaReturnType red_op) $ \t -> do
    let pt = elemType t
        shape = Shape [spaceNumGroups space] <> arrayShape t
    ImpGen.sAllocArray "group_res_arr" pt shape $ Space "device"

  sync_arr <- ImpGen.sAllocArray "sync_arr" Bool (Shape [intConst Int32 1]) $ Space "local"

  num_threads <- dPrimV "num_threads" $ kernelNumThreads constants

  sKernel constants "segred_nonseg" $ allThreads constants $ do
    init_constants

    -- Since this is the nonsegmented case, all outer segment IDs must
    -- necessarily be 0.
    let gtids = map fst $ spaceDimensions space
    forM_ (init gtids) $ \v ->
      v <-- 0

    num_elements <- Imp.elements <$> ImpGen.compileSubExp w
    let elems_per_thread = num_elements `quotRoundingUp` Imp.elements (kernelNumThreads constants)

    (group_result_params, red_op_renamed) <-
      reductionStageOne constants segred_pat num_elements
      global_tid elems_per_thread num_threads
      comm red_op nes red_arrs body

    reductionStageTwo constants segred_pat 0 [0] 0
      (kernelNumGroups constants) group_result_params red_acc_params red_op_renamed nes
      1 counter sync_arr group_res_arrs red_arrs

hasMemoryAccesses :: Body InKernel -> ImpGen.ImpM InKernel Imp.KernelOp Bool
hasMemoryAccesses body = or <$> mapM isArray (S.toList $ freeInBody body)
  where isArray = fmap (not . primType) . lookupType

smallSegmentsReduction :: Pattern ExplicitMemory
                       -> KernelSpace
                       -> Lambda InKernel -> [SubExp]
                       -> Body InKernel
                       -> CallKernelGen ()
smallSegmentsReduction (Pattern _ segred_pes) space red_op nes body = do
  (base_constants, init_constants) <- kernelInitialisationSetSpace space $ return ()
  let constants = base_constants { kernelThreadActive = true }

  let (gtids, dims) = unzip $ spaceDimensions space
  dims' <- mapM ImpGen.compileSubExp dims

  let segment_size = last dims'
      num_segments = product $ init dims'
      segments_per_group = kernelGroupSize constants `quot` segment_size
      required_groups = num_segments `quotRoundingUp` segments_per_group

  let red_op_params = lambdaParams red_op
      (red_acc_params, _red_next_params) = splitAt (length nes) red_op_params
  red_arrs <- forM red_acc_params $ \p ->
    case paramAttr p of
      MemArray pt shape _ (ArrayIn mem _) -> do
        let shape' = Shape [spaceNumThreads space] <> shape
        ImpGen.sArray "red_arr" pt shape' $
          ArrayIn mem $ IxFun.iota $ map (primExpFromSubExp int32) $ shapeDims shape'
      _ -> do
        let pt = elemType $ paramType p
            shape = Shape [spaceGroupSize space]
        ImpGen.sAllocArray "red_arr" pt shape $ Space "local"

  ImpGen.emit $ Imp.DebugPrint "num_segments" int32 num_segments
  ImpGen.emit $ Imp.DebugPrint "segment_size" int32 segment_size
  ImpGen.emit $ Imp.DebugPrint "segments_per_group" int32 segments_per_group
  ImpGen.emit $ Imp.DebugPrint "required_groups" int32 required_groups

  sKernel constants "segred_small" $ allThreads constants $ do
    init_constants

    -- We probably do not have enough actual workgroups to cover the
    -- entire iteration space.  Some groups thus have to perform double
    -- duty; we put an outer loop to accomplish this.
    virtualiseGroups constants required_groups $ \group_id' -> do
      -- Compute the 'n' input indices.  The outer 'n-1' correspond to
      -- the segment ID, and are computed from the group id.  The inner
      -- is computed from the local thread id, and may be out-of-bounds.
      let ltid = kernelLocalThreadId constants
          segment_index = (ltid `quot` segment_size) + (group_id' * segments_per_group)
          index_within_segment = ltid `rem` segment_size

      zipWithM_ (<--) (init gtids) $ unflattenIndex (init dims') segment_index
      last gtids <-- index_within_segment

      let toLocalMemory ses =
            forM_ (zip red_arrs ses) $ \(arr, se) -> do
            se_t <- subExpType se
            when (primType se_t) $
              ImpGen.copyDWIM arr [ltid] se []

          in_bounds =
            ImpGen.compileStms mempty (stmsToList $ bodyStms body) $ do
              let (red_res, map_res) = splitAt (length nes) $ bodyResult body

              sComment "save results to be reduced" $
                toLocalMemory red_res

              sComment "save map-out results" $
                forM_ (zip (drop (length nes) segred_pes) map_res) $ \(pe, se) ->
                ImpGen.copyDWIM (patElemName pe) (map (`Imp.var` int32) gtids) se []

      sComment "apply map function if in bounds" $
        sIf (isActive (init $ zip gtids dims) .&&.
             ltid .<. segment_size * segments_per_group) in_bounds (toLocalMemory nes)

      sOp Imp.LocalBarrier

      index_i <- newVName "index_i"
      index_j <- newVName "index_j"
      let crossesSegment from to =
            (to-from) .>. (to `rem` segment_size)
          red_op' = red_op { lambdaParams = Param index_i (MemPrim int32) :
                                            Param index_j (MemPrim int32) :
                                            lambdaParams red_op }

      sComment "perform segmented scan to imitate reduction" $
        groupScan constants (Just crossesSegment) (segment_size*segments_per_group) red_op' red_arrs

      sOp Imp.LocalBarrier

      sComment "save final values of segments" $
        sWhen (group_id' * segments_per_group + ltid .<. num_segments .&&.
               ltid .<. segments_per_group) $
        forM_ (zip segred_pes red_arrs) $ \(pe, arr) -> do
        -- Figure out which segment result this thread should write...
        let flat_segment_index = group_id' * segments_per_group + ltid
            gtids' = unflattenIndex (init dims') flat_segment_index
        ImpGen.copyDWIM (patElemName pe) gtids'
                        (Var arr) [(ltid+1) * segment_size - 1]

largeSegmentsReduction :: Pattern ExplicitMemory
                       -> KernelSpace
                       -> Commutativity -> Lambda InKernel -> [SubExp]
                       -> Body InKernel
                       -> CallKernelGen ()
largeSegmentsReduction segred_pat space comm red_op nes body = do
  (base_constants, init_constants) <- kernelInitialisationSetSpace space $ return ()
  let (gtids, dims) = unzip $ spaceDimensions space
  dims' <- mapM ImpGen.compileSubExp dims
  let segment_size = last dims'
      num_segments = product $ init dims'

  let (groups_per_segment, elems_per_thread) =
        groupsPerSegmentAndElementsPerThread segment_size num_segments
        (kernelNumGroups base_constants) (kernelGroupSize base_constants)
  num_groups <- dPrimV "num_groups" $
    groups_per_segment * num_segments

  num_threads <- dPrimV "num_threads" $
    Imp.var num_groups int32 * kernelGroupSize base_constants

  threads_per_segment <- dPrimV "thread_per_segment" $
    groups_per_segment * kernelGroupSize base_constants

  let constants = base_constants
                  { kernelThreadActive = true
                  , kernelNumGroups = Imp.var num_groups int32
                  , kernelNumThreads = Imp.var num_threads int32
                  }

  ImpGen.emit $ Imp.DebugPrint "num_segments" int32 num_segments
  ImpGen.emit $ Imp.DebugPrint "segment_size" int32 segment_size
  ImpGen.emit $ Imp.DebugPrint "num_groups" int32 (Imp.var num_groups int32)
  ImpGen.emit $ Imp.DebugPrint "group_size" int32 (kernelGroupSize constants)
  ImpGen.emit $ Imp.DebugPrint "elems_per_thread" int32 $ Imp.innerExp elems_per_thread
  ImpGen.emit $ Imp.DebugPrint "groups_per_segment" int32 groups_per_segment

  let red_op_params = lambdaParams red_op
      (red_acc_params, _) = splitAt (length nes) red_op_params
  red_arrs <- forM red_acc_params $ \p ->
    case paramAttr p of
      MemArray pt shape _ (ArrayIn mem _) -> do
        let shape' = Shape [Var num_threads] <> shape
        ImpGen.sArray "red_arr" pt shape' $
          ArrayIn mem $ IxFun.iota $ map (primExpFromSubExp int32) $ shapeDims shape'
      _ -> do
        let pt = elemType $ paramType p
            shape = Shape [spaceGroupSize space]
        ImpGen.sAllocArray "red_arr" pt shape $ Space "local"

  group_res_arrs <- forM (lambdaReturnType red_op) $ \t -> do
    let pt = elemType t
        shape = Shape [Var num_groups] <> arrayShape t
    ImpGen.sAllocArray "group_res_arr" pt shape $ Space "device"

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
  let num_counters = 1024
  counter <-
    ImpGen.sStaticArray "counter" (Space "device") int32 $
    replicate num_counters $ IntValue $ Int32Value 0

  sync_arr <- ImpGen.sAllocArray "sync_arr" Bool (Shape [intConst Int32 1]) $ Space "local"

  sKernel constants "segred_large" $ allThreads constants $ do
    init_constants
    let segment_gtids = init gtids
        group_id = kernelGroupId constants
        group_size = kernelGroupSize constants
        flat_segment_id = group_id `quot` groups_per_segment
        local_tid = kernelLocalThreadId constants

        global_tid = kernelGlobalThreadId constants
                     `rem` (group_size * groups_per_segment)
        w = last dims
        first_group_for_segment = flat_segment_id * groups_per_segment

    zipWithM_ (<--) segment_gtids $ unflattenIndex (init dims') flat_segment_id
    num_elements <- Imp.elements <$> ImpGen.compileSubExp w

    (group_result_params, red_op_renamed) <-
      reductionStageOne constants segred_pat num_elements
      global_tid elems_per_thread threads_per_segment
      comm red_op nes red_arrs body

    let multiple_groups_per_segment =
          reductionStageTwo constants segred_pat
          flat_segment_id (map (`Imp.var` int32) segment_gtids)
          first_group_for_segment groups_per_segment
          group_result_params red_acc_params red_op_renamed
          nes (fromIntegral num_counters) counter sync_arr group_res_arrs red_arrs

        one_group_per_segment =
          ImpGen.comment "first thread in group saves final result to memory" $
          sWhen (local_tid .==. 0) $
            forM_ (take (length nes) $ zip (patternNames segred_pat) group_result_params) $ \(v, p) ->
            ImpGen.copyDWIM v (map (`Imp.var` int32) segment_gtids) (Var $ paramName p) []

    sIf (groups_per_segment .==. 1) one_group_per_segment multiple_groups_per_segment

groupsPerSegmentAndElementsPerThread :: Imp.Exp -> Imp.Exp -> Imp.Exp -> Imp.Exp
                                     -> (Imp.Exp, Imp.Count Imp.Elements)
groupsPerSegmentAndElementsPerThread segment_size num_segments num_groups_hint group_size =
  let groups_per_segment =
        num_groups_hint `quotRoundingUp` num_segments
      elements_per_thread =
        segment_size `quotRoundingUp` (group_size * groups_per_segment)
  in (groups_per_segment, Imp.elements elements_per_thread)

reductionStageOne :: KernelConstants
                  -> Pattern ExplicitMemory
                  -> Imp.Count Imp.Elements
                  -> Imp.Exp
                  -> Imp.Count Imp.Elements
                  -> VName
                  -> Commutativity
                  -> LambdaT InKernel
                  -> [SubExp]
                  -> [VName]
                  -> Body InKernel
                  -> InKernelGen ([LParam InKernel], Lambda InKernel)
reductionStageOne constants (Pattern _ segred_pes) num_elements global_tid elems_per_thread threads_per_segment comm red_op nes red_arrs body = do

  let red_op_params = lambdaParams red_op
      (red_acc_params, red_next_params) = splitAt (length nes) red_op_params
      (gtids, _dims) = unzip $ kernelDimensions constants
      gtid = last gtids
      local_tid = kernelLocalThreadId constants
      index_in_segment = global_tid `quot` kernelGroupSize constants

  -- Figure out how many elements this thread should process.
  chunk_size <- dPrim "chunk_size" int32
  let ordering = case comm of Commutative -> SplitStrided $ Var threads_per_segment
                              Noncommutative -> SplitContiguous
  accesses_memory <- hasMemoryAccesses body
  computeThreadChunkSize ordering global_tid elems_per_thread num_elements chunk_size

  ImpGen.dScope Nothing $ scopeOfLParams $ lambdaParams red_op

  forM_ (zip red_acc_params nes) $ \(p, ne) ->
    ImpGen.copyDWIM (paramName p) [] ne []

  red_op_renamed <- renameLambda red_op

  let doTheReduction = do
        ImpGen.comment "to reduce current chunk, first store our result to memory" $
          forM_ (zip red_arrs red_acc_params) $ \(arr, p) ->
          when (primType $ paramType p) $
          ImpGen.copyDWIM arr [local_tid] (Var $ paramName p) []

        sOp Imp.LocalBarrier

        groupReduce constants (kernelGroupSize constants) red_op_renamed red_arrs

  i <- newVName "i"
  -- If this is a non-commutative reduction, each thread must run the
  -- loop the same number of iterations, because we will be performing
  -- a group-wide reduction in there.
  let (bound, check_bounds) =
        case comm of
          Commutative -> (Imp.var chunk_size int32, id)
          Noncommutative -> (Imp.innerExp elems_per_thread,
                             sWhen (Imp.var gtid int32 .<. Imp.innerExp num_elements))

  sFor i Int32 bound $ do
    gtid <--
      case comm of
        Commutative ->
          global_tid +
          Imp.var threads_per_segment int32 * Imp.var i int32
        Noncommutative | accesses_memory ->
          local_tid +
          (index_in_segment * Imp.innerExp elems_per_thread + Imp.var i int32) *
          kernelGroupSize constants
        Noncommutative ->
          Imp.var i int32 +
          global_tid * Imp.innerExp elems_per_thread

    check_bounds $ sComment "apply map function" $
      ImpGen.compileStms mempty (stmsToList $ bodyStms body) $ do
        let (red_res, map_res) = splitAt (length nes) $ bodyResult body

        sComment "save results to be reduced" $
          forM_ (zip red_next_params red_res) $ \(p, se) ->
          ImpGen.copyDWIM (paramName p) [] se []

        sComment "save map-out results" $
          forM_ (zip (drop (length nes) segred_pes) map_res) $ \(pe, se) ->
          ImpGen.copyDWIM (patElemName pe) (map (`Imp.var` int32) gtids) se []

        sComment "apply reduction operator" $
          ImpGen.compileBody' red_acc_params $ lambdaBody red_op

    case comm of
      Noncommutative | accesses_memory -> do
        doTheReduction
        sComment "first thread takes carry-out; others neutral element" $ do
          let carry_out =
                forM_ (zip red_acc_params $ lambdaParams red_op_renamed) $ \(p_to, p_from) ->
                ImpGen.copyDWIM (paramName p_to) [] (Var $ paramName p_from) []
              reset_to_neutral =
                forM_ (zip red_acc_params nes) $ \(p, ne) ->
                ImpGen.copyDWIM (paramName p) [] ne []
          sIf (local_tid .==. 0) carry_out reset_to_neutral
      _ ->
        return ()

  group_result_params <- case comm of
    Noncommutative | accesses_memory ->
      return red_acc_params

    _ -> do
      doTheReduction

      return $ lambdaParams red_op_renamed

  return (group_result_params, red_op_renamed)

reductionStageTwo :: KernelConstants
                  -> Pattern ExplicitMemory
                  -> Imp.Exp
                  -> [Imp.Exp]
                  -> Imp.Exp
                  -> PrimExp Imp.ExpLeaf
                  -> [LParam InKernel]
                  -> [LParam InKernel]
                  -> Lambda InKernel
                  -> [SubExp]
                  -> Imp.Exp
                  -> VName
                  -> VName
                  -> [VName]
                  -> [VName]
                  -> InKernelGen ()
reductionStageTwo constants segred_pat
                  flat_segment_id segment_gtids first_group_for_segment groups_per_segment
                  group_result_params red_acc_params
                  red_op_renamed nes
                  num_counters counter sync_arr group_res_arrs red_arrs = do
  let local_tid = kernelLocalThreadId constants
      group_id = kernelGroupId constants
      group_size = kernelGroupSize constants
  old_counter <- dPrim "old_counter" int32
  (counter_mem, _, counter_offset) <- ImpGen.fullyIndexArray counter [flat_segment_id `rem` num_counters]
  ImpGen.comment "first thread in group saves group result to memory" $
    sWhen (local_tid .==. 0) $ do
    forM_ (take (length nes) $ zip group_res_arrs group_result_params) $ \(v, p) ->
      ImpGen.copyDWIM v [group_id] (Var $ paramName p) []
    sOp Imp.MemFence
    -- Increment the counter, thus stating that our result is
    -- available.
    sOp $ Imp.Atomic $ Imp.AtomicAdd old_counter counter_mem counter_offset 1
    -- Now check if we were the last group to write our result.  If
    -- so, it is our responsibility to produce the final result.
    ImpGen.sWrite sync_arr [0] $ Imp.var old_counter int32 .==. groups_per_segment - 1

  sOp Imp.LocalBarrier

  is_last_group <- dPrim "is_last_group" Bool
  ImpGen.copyDWIM is_last_group [] (Var sync_arr) [0]
  sWhen (Imp.var is_last_group Bool) $ do
    -- The final group has written its result (and it was
    -- us!), so read in all the group results and perform the
    -- final stage of the reduction.  But first, we reset the
    -- counter so it is ready for next time.  This is done
    -- with an atomic to avoid warnings about write/write
    -- races in oclgrind.
    sWhen (local_tid .==. 0) $
      sOp $ Imp.Atomic $ Imp.AtomicAdd old_counter counter_mem counter_offset $
      negate groups_per_segment
    ImpGen.comment "read in the per-group-results" $
      forM_ (zip4 red_acc_params red_arrs nes group_res_arrs) $
      \(p, arr, ne, group_res_arr) -> do
        let load_group_result =
              ImpGen.copyDWIM (paramName p) []
              (Var group_res_arr) [first_group_for_segment + local_tid]
            load_neutral_element =
              ImpGen.copyDWIM (paramName p) [] ne []
        ImpGen.sIf (local_tid .<. groups_per_segment)
          load_group_result load_neutral_element
        when (primType $ paramType p) $
          ImpGen.copyDWIM arr [local_tid] (Var $ paramName p) []

    sComment "reduce the per-group results" $ do
      groupReduce constants group_size red_op_renamed red_arrs

      sComment "and back to memory with the final result" $
        sWhen (local_tid .==. 0) $
        forM_ (take (length nes) $ zip (patternNames segred_pat) $
               lambdaParams red_op_renamed) $ \(v, p) ->
        ImpGen.copyDWIM v segment_gtids (Var $ paramName p) []
