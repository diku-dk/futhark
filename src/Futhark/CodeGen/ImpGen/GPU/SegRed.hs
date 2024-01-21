{-# LANGUAGE TypeFamilies #-}

-- | We generate code for non-segmented/single-segment SegRed using
-- the basic approach outlined in the paper "Design and GPGPU
-- Performance of Futhark’s Redomap Construct" (ARRAY '16).  The main
-- deviations are:
--
-- * While we still use two-phase reduction, we use only a single
--   kernel, with the final threadblock to write a result (tracked via
--   an atomic counter) performing the final reduction as well.
--
-- * Instead of depending on storage layout transformations to handle
--   non-commutative reductions efficiently, we slide a
--   @tblocksize@-sized window over the input, and perform a parallel
--   reduction for each window.  This sacrifices the notion of
--   efficient sequentialisation, but is sometimes faster and
--   definitely simpler and more predictable (and uses less auxiliary
--   storage).
--
-- For segmented reductions we use the approach from "Strategies for
-- Regular Segmented Reductions on GPU" (FHPC '17).  This involves
-- having two different strategies, and dynamically deciding which one
-- to use based on the number of segments and segment size. We use the
-- (static) @tblock_size@ to decide which of the following two
-- strategies to choose:
--
-- * Large: uses one or more blocks to process a single segment. If
--   multiple blocks are used per segment, the intermediate reduction
--   results must be recursively reduced, until there is only a single
--   value per segment.
--
--   Each thread /can/ read multiple elements, which will greatly
--   increase performance; however, if the reduction is
--   non-commutative we will have to use a less efficient traversal
--   (with interim block-wide reductions) to enable coalesced memory
--   accesses, just as in the non-segmented case.
--
-- * Small: is used to let each block process *multiple* segments
--   within a block. We will only use this approach when we can
--   process at least two segments within a single block. In those
--   cases, we would allocate a /whole/ block per segment with the
--   large strategy, but at most 50% of the threads in the block would
--   have any element to read, which becomes highly inefficient.
--
-- An optimization specfically targeted at non-segmented and large-segments
-- segmented reductions with non-commutative is made: The stage one main loop is
-- essentially stripmined by a factor *chunk*, inserting collective copies via
-- shared memory of each reduction parameter going into the intra-block (partial)
-- reductions. This saves a factor *chunk* number of intra-block reductions at
-- the cost of some overhead in collective copies.
module Futhark.CodeGen.ImpGen.GPU.SegRed
  ( compileSegRed,
    compileSegRed',
    DoSegBody,
  )
where

import Control.Monad
import Data.List (genericLength, zip4)
import Data.Map qualified as M
import Data.Maybe
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
forM2_ xs ys f = forM_ (zip xs ys) (uncurry f)

-- | The maximum number of operators we support in a single SegRed.
-- This limit arises out of the static allocation of counters.
maxNumOps :: Int
maxNumOps = 20

-- | Code generation for the body of the SegRed, taking a continuation
-- for saving the results of the body.  The results should be
-- represented as a pairing of a t'SubExp' along with a list of
-- indexes into that t'SubExp' for reading the result.
type DoSegBody = ([(SubExp, [Imp.TExp Int64])] -> InKernelGen ()) -> InKernelGen ()

-- | Datatype used to distinguish between and work with the different sets of
-- intermediate memory we need for the different ReduceKinds.
data SegRedIntermediateArrays
  = GeneralSegRedInterms
      { blockRedArrs :: [VName]
      }
  | NoncommPrimSegRedInterms
      { collCopyArrs :: [VName],
        blockRedArrs :: [VName],
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
  KernelAttrs {kAttrNumBlocks = num_tblocks, kAttrBlockSize = tblock_size} <-
    lvlKernelAttrs lvl
  let grid = KernelGrid num_tblocks tblock_size

  compileSegRed' pat grid space segbinops $ \red_cont ->
    sComment "apply map function" $
      compileStms mempty (kernelBodyStms map_kbody) $ do
        let (red_res, map_res) = splitAt (segBinOpResults segbinops) $ kernelBodyResult map_kbody

        let mapout_arrs = drop (segBinOpResults segbinops) $ patElems pat
        unless (null mapout_arrs) $
          sComment "write map-out result(s)" $ do
            zipWithM_ (compileThreadResult space) mapout_arrs map_res

        red_cont $ map ((,[]) . kernelResultSubExp) red_res
  emit $ Imp.DebugPrint "" Nothing

paramOf :: SegBinOp GPUMem -> [Param LParamMem]
paramOf (SegBinOp _ op ne _) = take (length ne) $ lambdaParams op

isPrimSegBinOp :: SegBinOp GPUMem -> Bool
isPrimSegBinOp segbinop =
  all primType (lambdaReturnType $ segBinOpLambda segbinop)
    && shapeRank (segBinOpShape segbinop) == 0

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
        ("compileSegRed': at most " <> show maxNumOps <> " reduction operators are supported.\n")
          <> ("Pattern: " <> prettyString pat)
  | otherwise = do
      chunk_v <- dPrimV "chunk_size" . isInt64 =<< kernelConstToExp chunk_const
      case unSegSpace space of
        [(_, Constant (IntValue (Int64Value 1))), _] ->
          compileReduction (chunk_v, chunk_const) nonsegmentedReduction
        _ -> do
          let segment_size = pe64 $ last $ segSpaceDims space
              use_small_segments = segment_size * 2 .<. pe64 (unCount tblock_size) * tvExp chunk_v
          sIf
            use_small_segments
            (compileReduction (chunk_v, chunk_const) smallSegmentsReduction)
            (compileReduction (chunk_v, chunk_const) largeSegmentsReduction)
  where
    compileReduction chunk f =
      f pat num_tblocks tblock_size chunk space segbinops map_body_cont

    param_types = map paramType $ concatMap paramOf segbinops

    num_tblocks = gridNumBlocks grid
    tblock_size = gridBlockSize grid

    chunk_const =
      if Noncommutative `elem` map segBinOpComm segbinops
        && all isPrimSegBinOp segbinops
        then getChunkSize param_types
        else Imp.ValueExp $ IntValue $ intValue Int64 (1 :: Int64)

-- | Prepare intermediate arrays for the reduction.  Prim-typed
-- arguments go in shared memory (so we need to do the allocation of
-- those arrays inside the kernel), while array-typed arguments go in
-- global memory.  Allocations for the latter have already been
-- performed.  This policy is baked into how the allocations are done
-- in ExplicitAllocations.
--
-- For more info about the intermediate arrays used for the different reduction
-- kernels, see note [IntermArrays].
makeIntermArrays ::
  Imp.TExp Int64 ->
  SubExp ->
  SubExp ->
  [SegBinOp GPUMem] ->
  InKernelGen [SegRedIntermediateArrays]
makeIntermArrays tblock_id tblock_size chunk segbinops
  | Noncommutative <- mconcat (map segBinOpComm segbinops),
    all isPrimSegBinOp segbinops =
      noncommPrimSegRedInterms
  | otherwise =
      generalSegRedInterms tblock_id tblock_size segbinops
  where
    params = map paramOf segbinops

    noncommPrimSegRedInterms = do
      block_worksize <- tvSize <$> dPrimV "block_worksize" block_worksize_E

      -- compute total amount of lmem.
      let sum_ x y = nextMul x y + tblock_size_E * y
          block_reds_lmem_requirement = foldl sum_ 0 $ concat elem_sizes
          collcopy_lmem_requirement = block_worksize_E * max_elem_size
          lmem_total_size =
            Imp.bytes $
              collcopy_lmem_requirement `sMax64` block_reds_lmem_requirement

      -- offsets into the total pool of lmem for each block reduction array.
      (_, offsets) <-
        forAccumLM2D 0 elem_sizes $ \byte_offs elem_size ->
          (,byte_offs `quot` elem_size)
            <$> dPrimVE "offset" (sum_ byte_offs elem_size)

      -- total pool of local mem.
      lmem <- sAlloc "local_mem" lmem_total_size (Space "shared")
      let arrInLMem ptype name len_se offset =
            sArray
              (name ++ "_" ++ prettyString ptype)
              ptype
              (Shape [len_se])
              lmem
              $ LMAD.iota offset [pe64 len_se]

      forM (zipWith zip params offsets) $ \ps_and_offsets -> do
        (coll_copy_arrs, block_red_arrs, priv_chunks) <-
          fmap unzip3 $ forM ps_and_offsets $ \(p, offset) -> do
            let ptype = elemType $ paramType p
            (,,)
              <$> arrInLMem ptype "coll_copy_arr" block_worksize 0
              <*> arrInLMem ptype "block_red_arr" tblock_size offset
              <*> sAllocArray
                ("chunk_" ++ prettyString ptype)
                ptype
                (Shape [chunk])
                (ScalarSpace [chunk] ptype)
        pure $ NoncommPrimSegRedInterms coll_copy_arrs block_red_arrs priv_chunks
    tblock_size_E = pe64 tblock_size
    block_worksize_E = tblock_size_E * pe64 chunk

    paramSize = primByteSize . elemType . paramType
    elem_sizes = map (map paramSize) params
    max_elem_size = maximum $ concat elem_sizes

    forAccumLM2D acc ls f = mapAccumLM (mapAccumLM f) acc ls

generalSegRedInterms ::
  Imp.TExp Int64 ->
  SubExp ->
  [SegBinOp GPUMem] ->
  InKernelGen [SegRedIntermediateArrays]
generalSegRedInterms tblock_id tblock_size segbinops =
  fmap (map GeneralSegRedInterms) $
    forM (map paramOf segbinops) $
      mapM $ \p ->
        case paramDec p of
          MemArray pt shape _ (ArrayIn mem _) -> do
            let shape' = Shape [tblock_size] <> shape
            let shape_E = map pe64 $ shapeDims shape'
            sArray ("red_arr_" ++ prettyString pt) pt shape' mem $
              LMAD.iota (tblock_id * product shape_E) shape_E
          _ -> do
            let pt = elemType $ paramType p
                shape = Shape [tblock_size]
            sAllocArray ("red_arr_" ++ prettyString pt) pt shape $ Space "shared"

-- | Arrays for storing block results.
--
-- The block-result arrays have an extra dimension because they are
-- also used for keeping vectorised accumulators for first-stage
-- reduction, if necessary.  If necessary, this dimension has size
-- tblock_size, and otherwise 1.  When actually storing block results,
-- the first index is set to 0.
groupResultArrays ::
  SubExp ->
  SubExp ->
  [SegBinOp GPUMem] ->
  CallKernelGen [[VName]]
groupResultArrays num_virtblocks tblock_size segbinops =
  forM segbinops $ \(SegBinOp _ lam _ shape) ->
    forM (lambdaReturnType lam) $ \t -> do
      let pt = elemType t
          extra_dim
            | primType t, shapeRank shape == 0 = intConst Int64 1
            | otherwise = tblock_size
          full_shape = Shape [extra_dim, num_virtblocks] <> shape <> arrayShape t
          -- Move the tblocksize dimension last to ensure coalesced
          -- memory access.
          perm = [1 .. shapeRank full_shape - 1] ++ [0]
      sAllocArrayPerm "segred_tmp" pt full_shape (Space "device") perm

type DoCompileSegRed =
  Pat LetDecMem ->
  Count NumBlocks SubExp ->
  Count BlockSize SubExp ->
  (TV Int64, Imp.KernelConstExp) ->
  SegSpace ->
  [SegBinOp GPUMem] ->
  DoSegBody ->
  CallKernelGen ()

nonsegmentedReduction :: DoCompileSegRed
nonsegmentedReduction (Pat segred_pes) num_tblocks tblock_size (chunk_v, chunk_const) space segbinops map_body_cont = do
  let (gtids, dims) = unzip $ unSegSpace space
      chunk = tvExp chunk_v
      num_tblocks_se = unCount num_tblocks
      tblock_size_se = unCount tblock_size
      tblock_size' = pe64 tblock_size_se
      global_tid = Imp.le64 $ segFlat space
      n = pe64 $ last dims

  counters <- genZeroes "counters" maxNumOps

  reds_block_res_arrs <- groupResultArrays num_tblocks_se tblock_size_se segbinops

  num_threads <-
    fmap tvSize $ dPrimV "num_threads" $ pe64 num_tblocks_se * tblock_size'

  let attrs =
        (defKernelAttrs num_tblocks tblock_size)
          { kAttrConstExps = M.singleton (tvVar chunk_v) chunk_const
          }

  sKernelThread "segred_nonseg" (segFlat space) attrs $ do
    constants <- kernelConstants <$> askEnv
    let ltid = kernelLocalThreadId constants
    let tblock_id = kernelBlockId constants

    interms <- makeIntermArrays (sExt64 tblock_id) tblock_size_se (tvSize chunk_v) segbinops
    sync_arr <- sAllocArray "sync_arr" Bool (Shape [intConst Int32 1]) $ Space "shared"

    -- Since this is the nonsegmented case, all outer segment IDs must
    -- necessarily be 0.
    forM_ gtids $ \v -> dPrimV_ v (0 :: Imp.TExp Int64)

    q <- dPrimVE "q" $ n `divUp` (sExt64 (kernelNumThreads constants) * chunk)

    slugs <-
      mapM (segBinOpSlug ltid tblock_id) $
        zip3 segbinops interms reds_block_res_arrs
    new_lambdas <-
      reductionStageOne
        gtids
        n
        global_tid
        q
        chunk
        (pe64 num_threads)
        slugs
        map_body_cont

    let segred_pess =
          chunks
            (map (length . segBinOpNeutral) segbinops)
            segred_pes

    forM_ (zip4 segred_pess slugs new_lambdas [0 ..]) $
      \(pes, slug, new_lambda, i) ->
        reductionStageTwo
          pes
          tblock_id
          [0]
          0
          (sExt64 $ kernelNumBlocks constants)
          slug
          new_lambda
          counters
          sync_arr
          (fromInteger i)

smallSegmentsReduction :: DoCompileSegRed
smallSegmentsReduction (Pat segred_pes) num_tblocks tblock_size _ space segbinops map_body_cont = do
  let (gtids, dims) = unzip $ unSegSpace space
      dims' = map pe64 dims
      segment_size = last dims'

  -- Careful to avoid division by zero now.
  segment_size_nonzero <-
    dPrimVE "segment_size_nonzero" $ sMax64 1 segment_size

  let tblock_size_se = unCount tblock_size
      num_tblocks_se = unCount tblock_size
      num_tblocks' = pe64 num_tblocks_se
      tblock_size' = pe64 tblock_size_se
  num_threads <- fmap tvSize $ dPrimV "num_threads" $ num_tblocks' * tblock_size'
  let num_segments = product $ init dims'
      segments_per_block = tblock_size' `quot` segment_size_nonzero
      required_blocks = sExt32 $ num_segments `divUp` segments_per_block

  emit $ Imp.DebugPrint "# SegRed-small" Nothing
  emit $ Imp.DebugPrint "num_segments" $ Just $ untyped num_segments
  emit $ Imp.DebugPrint "segment_size" $ Just $ untyped segment_size
  emit $ Imp.DebugPrint "segments_per_block" $ Just $ untyped segments_per_block
  emit $ Imp.DebugPrint "required_blocks" $ Just $ untyped required_blocks

  sKernelThread "segred_small" (segFlat space) (defKernelAttrs num_tblocks tblock_size) $ do
    constants <- kernelConstants <$> askEnv
    let tblock_id = kernelBlockSize constants
        ltid = sExt64 $ kernelLocalThreadId constants

    interms <- generalSegRedInterms tblock_id tblock_size_se segbinops
    let reds_arrs = map blockRedArrs interms

    -- We probably do not have enough actual threadblocks to cover the
    -- entire iteration space.  Some blocks thus have to perform double
    -- duty; we put an outer loop to accomplish this.
    virtualiseBlocks SegVirt required_blocks $ \virttblock_id -> do
      -- Compute the 'n' input indices.  The outer 'n-1' correspond to
      -- the segment ID, and are computed from the block id.  The inner
      -- is computed from the local thread id, and may be out-of-bounds.
      let segment_index =
            (ltid `quot` segment_size_nonzero)
              + (sExt64 virttblock_id * sExt64 segments_per_block)
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
              * segments_per_block
          )
          in_bounds
          out_of_bounds

      sOp $ Imp.ErrorSync Imp.FenceLocal -- Also implicitly barrier.
      let crossesSegment from to =
            (sExt64 to - sExt64 from) .>. (sExt64 to `rem` segment_size)
      sWhen (segment_size .>. 0) $
        sComment "perform segmented scan to imitate reduction" $
          forM2_ segbinops reds_arrs $ \(SegBinOp _ red_op _ _) red_arrs ->
            blockScan
              (Just crossesSegment)
              (sExt64 $ pe64 num_threads)
              (segment_size * segments_per_block)
              red_op
              red_arrs

      sOp $ Imp.Barrier Imp.FenceLocal

      sComment "save final values of segments"
        $ sWhen
          ( sExt64 virttblock_id
              * segments_per_block
              + sExt64 ltid
                .<. num_segments
                .&&. ltid
                .<. segments_per_block
          )
        $ forM2_ segred_pes (concat reds_arrs)
        $ \pe arr -> do
          -- Figure out which segment result this thread should write...
          let flat_segment_index =
                sExt64 virttblock_id * segments_per_block + sExt64 ltid
              gtids' =
                unflattenIndex (init dims') flat_segment_index
          copyDWIMFix
            (patElemName pe)
            gtids'
            (Var arr)
            [(ltid + 1) * segment_size_nonzero - 1]

      -- Finally another barrier, because we will be writing to the
      -- shared memory array first thing in the next iteration.
      sOp $ Imp.Barrier Imp.FenceLocal

largeSegmentsReduction :: DoCompileSegRed
largeSegmentsReduction (Pat segred_pes) num_tblocks tblock_size (chunk_v, chunk_const) space segbinops map_body_cont = do
  let (gtids, dims) = unzip $ unSegSpace space
      dims' = map pe64 dims
      num_segments = product $ init dims'
      segment_size = last dims'
      num_tblocks' = pe64 $ unCount num_tblocks
      tblock_size_se = unCount tblock_size
      tblock_size' = pe64 tblock_size_se
      chunk = tvExp chunk_v

  blocks_per_segment <-
    dPrimVE "blocks_per_segment" $
      num_tblocks' `divUp` sMax64 1 num_segments

  q <-
    dPrimVE "q" $
      segment_size `divUp` (tblock_size' * blocks_per_segment * chunk)

  num_virtblocks <-
    dPrimV "num_virtblocks" $
      blocks_per_segment * num_segments
  threads_per_segment <-
    dPrimVE "threads_per_segment" $
      blocks_per_segment * tblock_size'

  emit $ Imp.DebugPrint "# SegRed-large" Nothing
  emit $ Imp.DebugPrint "num_segments" $ Just $ untyped num_segments
  emit $ Imp.DebugPrint "segment_size" $ Just $ untyped segment_size
  emit $ Imp.DebugPrint "num_virtblocks" $ Just $ untyped $ tvExp num_virtblocks
  emit $ Imp.DebugPrint "num_tblocks" $ Just $ untyped num_tblocks'
  emit $ Imp.DebugPrint "tblock_size" $ Just $ untyped tblock_size'
  emit $ Imp.DebugPrint "q" $ Just $ untyped q
  emit $ Imp.DebugPrint "blocks_per_segment" $ Just $ untyped blocks_per_segment

  reds_block_res_arrs <- groupResultArrays (tvSize num_virtblocks) tblock_size_se segbinops

  -- In principle we should have a counter for every segment.  Since
  -- the number of segments is a dynamic quantity, we would have to
  -- allocate and zero out an array here, which is expensive.
  -- However, we exploit the fact that the number of segments being
  -- reduced at any point in time is limited by the number of
  -- threadblocks. If we bound the number of threadblocks, we can get away
  -- with using that many counters.  FIXME: Is this limit checked
  -- anywhere?  There are other places in the compiler that will fail
  -- if the block count exceeds the maximum block size, which is at
  -- most 1024 anyway.
  let num_counters = maxNumOps * 1024
  counters <- genZeroes "counters" $ fromIntegral num_counters

  let attrs =
        (defKernelAttrs num_tblocks tblock_size)
          { kAttrConstExps = M.singleton (tvVar chunk_v) chunk_const
          }

  sKernelThread "segred_large" (segFlat space) attrs $ do
    constants <- kernelConstants <$> askEnv
    let tblock_id = sExt64 $ kernelBlockId constants
        ltid = kernelLocalThreadId constants

    interms <- makeIntermArrays tblock_id tblock_size_se (tvSize chunk_v) segbinops
    sync_arr <- sAllocArray "sync_arr" Bool (Shape [intConst Int32 1]) $ Space "shared"

    -- We probably do not have enough actual threadblocks to cover the
    -- entire iteration space.  Some blocks thus have to perform double
    -- duty; we put an outer loop to accomplish this.
    virtualiseBlocks SegVirt (sExt32 (tvExp num_virtblocks)) $ \virttblock_id -> do
      let segment_gtids = init gtids

      flat_segment_id <-
        dPrimVE "flat_segment_id" $
          sExt64 virttblock_id `quot` blocks_per_segment

      global_tid <-
        dPrimVE "global_tid" $
          (sExt64 virttblock_id * sExt64 tblock_size' + sExt64 ltid)
            `rem` threads_per_segment

      let first_block_for_segment = flat_segment_id * blocks_per_segment
      dIndexSpace (zip segment_gtids (init dims')) flat_segment_id
      dPrim_ (last gtids) int64
      let n = pe64 $ last dims

      slugs <-
        mapM (segBinOpSlug ltid virttblock_id) $
          zip3 segbinops interms reds_block_res_arrs
      new_lambdas <-
        reductionStageOne
          gtids
          n
          global_tid
          q
          chunk
          threads_per_segment
          slugs
          map_body_cont

      let segred_pess =
            chunks
              (map (length . segBinOpNeutral) segbinops)
              segred_pes

          multiple_blocks_per_segment =
            forM_ (zip4 segred_pess slugs new_lambdas [0 ..]) $
              \(pes, slug, new_lambda, i) -> do
                let counter_idx =
                      fromIntegral (i * num_counters)
                        + flat_segment_id
                          `rem` fromIntegral num_counters
                reductionStageTwo
                  pes
                  virttblock_id
                  (map Imp.le64 segment_gtids)
                  first_block_for_segment
                  blocks_per_segment
                  slug
                  new_lambda
                  counters
                  sync_arr
                  counter_idx

          one_block_per_segment =
            sComment "first thread in block saves final result to memory" $
              forM2_ slugs segred_pess $ \slug pes ->
                sWhen (ltid .==. 0) $
                  forM2_ pes (slugAccs slug) $ \v (acc, acc_is) ->
                    copyDWIMFix (patElemName v) (map Imp.le64 segment_gtids) (Var acc) acc_is

      sIf (blocks_per_segment .==. 1) one_block_per_segment multiple_blocks_per_segment

-- | Auxiliary information for a single reduction. A slug holds the `SegBinOp`
-- operator for a single reduction, the different arrays required throughout
-- stages one and two, and a global mem destination for the final result of the
-- particular reduction.
data SegBinOpSlug = SegBinOpSlug
  { slugOp :: SegBinOp GPUMem,
    -- | Intermediate arrays needed for the given reduction.
    slugInterms :: SegRedIntermediateArrays,
    -- | Place(s) to store block accumulator(s) in stage 1 reduction.
    slugAccs :: [(VName, [Imp.TExp Int64])],
    -- | Global memory destination(s) for the final result(s) for this
    -- particular reduction.
    blockResArrs :: [VName]
  }

segBinOpSlug ::
  Imp.TExp Int32 ->
  Imp.TExp Int32 ->
  (SegBinOp GPUMem, SegRedIntermediateArrays, [VName]) ->
  InKernelGen SegBinOpSlug
segBinOpSlug ltid tblock_id (op, interms, block_res_arrs) = do
  accs <- zipWithM mkAcc (lambdaParams (segBinOpLambda op)) block_res_arrs
  pure $ SegBinOpSlug op interms accs block_res_arrs
  where
    mkAcc p block_res_arr
      | Prim t <- paramType p,
        shapeRank (segBinOpShape op) == 0 = do
          block_res_acc <- dPrim (baseString (paramName p) <> "_block_res_acc") t
          pure (tvVar block_res_acc, [])
      -- if this is a non-primitive reduction, the global mem result array will
      -- double as accumulator.
      | otherwise =
          pure (block_res_arr, [sExt64 ltid, sExt64 tblock_id])

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

slugBlockRedArrs :: SegBinOpSlug -> [VName]
slugBlockRedArrs = blockRedArrs . slugInterms

slugPrivChunks :: SegBinOpSlug -> [VName]
slugPrivChunks = privateChunks . slugInterms

slugCollCopyArrs :: SegBinOpSlug -> [VName]
slugCollCopyArrs = collCopyArrs . slugInterms

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
reductionStageOne gtids n global_tid q chunk threads_per_segment slugs body_cont = do
  constants <- kernelConstants <$> askEnv
  let glb_ind_var = mkTV (last gtids) int64
      ltid = sExt64 $ kernelLocalThreadId constants

  dScope Nothing $ scopeOfLParams $ concatMap slugParams slugs

  sComment "ne-initialise the outer (per-block) accumulator(s)" $ do
    forM_ slugs $ \slug ->
      forM2_ (slugAccs slug) (slugNeutral slug) $ \(acc, acc_is) ne ->
        sLoopNest (slugShape slug) $ \vec_is ->
          copyDWIMFix acc (acc_is ++ vec_is) ne []

  new_lambdas <- mapM (renameLambda . slugLambda) slugs
  let tblock_size = sExt32 $ kernelBlockSize constants
  let doBlockReduce =
        forM2_ slugs new_lambdas $ \slug new_lambda -> do
          let accs = slugAccs slug
          let params = slugParams slug
          sLoopNest (slugShape slug) $ \vec_is -> do
            let block_red_arrs = slugBlockRedArrs slug
            sComment "store accs. prims go in lmem; non-prims in params (in global mem)" $
              forM_ (zip3 block_red_arrs accs params) $
                \(arr, (acc, acc_is), p) ->
                  if isPrimParam p
                    then copyDWIMFix arr [ltid] (Var acc) (acc_is ++ vec_is)
                    else copyDWIMFix (paramName p) [] (Var acc) (acc_is ++ vec_is)

            sOp $ Imp.ErrorSync Imp.FenceLocal -- Also implicitly barrier.
            blockReduce tblock_size new_lambda block_red_arrs
            sOp $ Imp.Barrier Imp.FenceLocal

            sComment "thread 0 updates per-block acc(s); rest reset to ne" $ do
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

  case (slugsComm slugs, all (isPrimSegBinOp . slugOp) slugs) of
    (Noncommutative, True) ->
      noncommPrimParamsStageOneBody
        slugs
        body_cont
        glb_ind_var
        global_tid
        q
        n
        chunk
        doBlockReduce
    _ ->
      generalStageOneBody
        slugs
        body_cont
        glb_ind_var
        global_tid
        q
        n
        threads_per_segment
        doBlockReduce

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
generalStageOneBody slugs body_cont glb_ind_var global_tid q n threads_per_segment doBlockReduce = do
  let is_comm = slugsComm slugs == Commutative

  constants <- kernelConstants <$> askEnv
  let tblock_size = kernelBlockSize constants
      ltid = sExt64 $ kernelLocalThreadId constants

  -- this block's id within its designated segment, and this block's initial
  -- global offset.
  tblock_id_in_segment <- dPrimVE "tblock_id_in_segment" $ global_tid `quot` tblock_size
  block_base_offset <- dPrimVE "block_base_offset" $ tblock_id_in_segment * q * tblock_size

  sFor "i" q $ \i -> do
    block_offset <- dPrimVE "block_offset" $ block_base_offset + i * tblock_size
    glb_ind_var
      <-- if is_comm
        then global_tid + threads_per_segment * i
        else block_offset + ltid

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

    unless is_comm doBlockReduce
  sOp $ Imp.ErrorSync Imp.FenceLocal
  when is_comm doBlockReduce

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
noncommPrimParamsStageOneBody slugs body_cont glb_ind_var global_tid q n chunk doLMemBlockReduce = do
  constants <- kernelConstants <$> askEnv
  let tblock_size = kernelBlockSize constants
      ltid = sExt64 $ kernelLocalThreadId constants

  -- this block's id within its designated segment; the stride made per block in
  -- the outer `i < q` loop; and this block's initial global offset.
  tblock_id_in_segment <- dPrimVE "block_offset_in_segment" $ global_tid `quot` tblock_size
  block_stride <- dPrimVE "block_stride" $ tblock_size * chunk
  block_base_offset <- dPrimVE "block_base_offset" $ tblock_id_in_segment * q * block_stride

  let chunkLoop = sFor "k" chunk

  sFor "i" q $ \i -> do
    -- block offset in this iteration.
    block_offset <- dPrimVE "block_offset" $ block_base_offset + i * block_stride
    chunkLoop $ \k -> do
      loc_ind <- dPrimVE "loc_ind" $ k * tblock_size + ltid
      glb_ind_var <-- block_offset + loc_ind

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

    sOp $ Imp.ErrorSync Imp.FenceLocal

    sComment "effectualize collective copies in shared memory" $ do
      forM_ slugs $ \slug -> do
        let coll_copy_arrs = slugCollCopyArrs slug
        let priv_chunks = slugPrivChunks slug

        forM2_ coll_copy_arrs priv_chunks $ \lmem_arr priv_chunk -> do
          chunkLoop $ \k -> do
            lmem_idx <- dPrimVE "lmem_idx" $ ltid + k * tblock_size
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
    doLMemBlockReduce
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
reductionStageTwo segred_pes tblock_id segment_gtids first_block_for_segment blocks_per_segment slug new_lambda counters sync_arr counter_idx = do
  constants <- kernelConstants <$> askEnv

  let ltid32 = kernelLocalThreadId constants
      ltid = sExt64 ltid32
      tblock_size = kernelBlockSize constants

  let (acc_params, next_params) = slugSplitParams slug
      nes = slugNeutral slug
      red_arrs = slugBlockRedArrs slug
      block_res_arrs = blockResArrs slug

  old_counter <- dPrim "old_counter" int32
  (counter_mem, _, counter_offset) <-
    fullyIndexArray
      counters
      [counter_idx]
  sComment "first thread in block saves block result to global memory" $
    sWhen (ltid32 .==. 0) $ do
      forM_ (take (length nes) $ zip block_res_arrs (slugAccs slug)) $ \(v, (acc, acc_is)) ->
        copyDWIMFix v [0, sExt64 tblock_id] (Var acc) acc_is
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
      -- Now check if we were the last block to write our result.  If
      -- so, it is our responsibility to produce the final result.
      sWrite sync_arr [0] $ untyped $ tvExp old_counter .==. blocks_per_segment - 1

  sOp $ Imp.Barrier Imp.FenceGlobal

  is_last_block <- dPrim "is_last_block" Bool
  copyDWIMFix (tvVar is_last_block) [] (Var sync_arr) [0]
  sWhen (tvExp is_last_block) $ do
    -- The final block has written its result (and it was
    -- us!), so read in all the block results and perform the
    -- final stage of the reduction.  But first, we reset the
    -- counter so it is ready for next time.  This is done
    -- with an atomic to avoid warnings about write/write
    -- races in oclgrind.
    sWhen (ltid32 .==. 0) $
      sOp $
        Imp.Atomic DefaultSpace $
          Imp.AtomicAdd Int32 (tvVar old_counter) counter_mem counter_offset $
            untyped $
              negate blocks_per_segment

    sLoopNest (slugShape slug) $ \vec_is -> do
      unless (null $ slugShape slug) $
        sOp (Imp.Barrier Imp.FenceLocal)

      -- There is no guarantee that the number of threadblocks for the
      -- segment is less than the threadblock size, so each thread may
      -- have to read multiple elements.  We do this in a sequential
      -- way that may induce non-coalesced accesses, but the total
      -- number of accesses should be tiny here.
      --
      -- TODO: here we *could* insert a collective copy of the num_tblocks
      -- per-block results. However, it may not be beneficial, since num_tblocks
      -- is not necessarily larger than tblock_size, meaning the number of
      -- uncoalesced reads here may be insignificant. In fact, if we happen to
      -- have a num_tblocks < tblock_size, then the collective copy would add
      -- unnecessary overhead. Also, this code is only executed by a single
      -- block.
      sComment "read in the per-block-results" $ do
        read_per_thread <-
          dPrimVE "read_per_thread" $
            blocks_per_segment `divUp` sExt64 tblock_size

        forM2_ acc_params nes $ \p ne ->
          copyDWIM (paramName p) [] ne []

        sFor "i" read_per_thread $ \i -> do
          block_res_id <-
            dPrimVE "block_res_id" $
              ltid * read_per_thread + i
          index_of_block_res <-
            dPrimVE "index_of_block_res" $
              first_block_for_segment + block_res_id

          sWhen (block_res_id .<. blocks_per_segment) $ do
            forM2_ next_params block_res_arrs $
              \p block_res_arr ->
                copyDWIMFix
                  (paramName p)
                  []
                  (Var block_res_arr)
                  ([0, index_of_block_res] ++ vec_is)

            compileStms mempty (bodyStms $ slugBody slug) $
              forM2_ acc_params (map resSubExp $ bodyResult $ slugBody slug) $ \p se ->
                copyDWIMFix (paramName p) [] se []

      forM2_ acc_params red_arrs $ \p arr ->
        when (isPrimParam p) $
          copyDWIMFix arr [ltid] (Var $ paramName p) []

      sOp $ Imp.ErrorSync Imp.FenceLocal

      sComment "reduce the per-block results" $ do
        blockReduce (sExt32 tblock_size) new_lambda red_arrs

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
--   These kernels need shared memory for 1) the initial collective copy, 2) the
--   (virtualized) block reductions, and (TODO: this one not implemented yet!)
--   3) the final single-block collective copy. There are no dependencies
--   between these three stages, so we can reuse the same pool of local mem for
--   all three. These intermediates all go into local mem because of the
--   assumption of primitive parameter types.
--
--   Let `elem_sizes` be a list of element type sizes for the reduction
--   operators in a given redomap fusion. Then the amount of local mem needed
--   across the three steps are:
--
--   1) The initial collective copy from global to thread-private memory
--   requires `tblock_size * CHUNK * max elem_sizes`, since the collective copies
--   are performed in sequence (ie. inputs to different reduction operators need
--   not be held in local mem simultaneously).
--   2) The intra-block reductions of shared memory held per-thread results
--   require `tblock_size * sum elem_sizes` bytes, since per-thread results for
--   all fused reductions are block-reduced simultaneously.
--   3) If tblock_size < num_tblocks, then after the final single-block collective
--   copy, a thread-sequential reduction reduces the number of per-block partial
--   results from num_tblocks down to tblock_size for each reduction array, such
--   that they will each fit in the final intra-block reduction. This requires
--   `num_tblocks * max elem_sizes`.
--
--   In summary, the total amount of local mem needed is the maximum between:
--   1) initial collective copy: tblock_size * CHUNK * max elem_sizes
--   2) intra-block reductions:  tblock_size * sum elem_sizes
--   3) final collective copy:   num_tblocks * max elem_sizes
--
--   The amount of local mem will most likely be decided by 1) in most cases,
--   unless the number of fused operators is very high *or* if we have a
--   `num_tblocks > tblock_size * CHUNK`, but this is unlikely, in which case 2)
--   and 3), respectively, will dominate.
--
--   Aside from shared memory, these kernels also require a CHUNK-sized array of
--   thread-private register memory per reduction operator.
--
-- For all other reductions, ie. commutative reductions, reductions with at
-- least one non-primitive operator, and small segments reductions:
--
--   These kernels use shared memory only for the intra-block reductions, and
--   since they do not use chunking or CHUNK, they all require onlly `tblock_size
--   * max elem_sizes` bytes of shared memory and no thread-private register mem.
