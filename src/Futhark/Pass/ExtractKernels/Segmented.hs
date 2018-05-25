{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Multiversion segmented reduction.
module Futhark.Pass.ExtractKernels.Segmented
       ( regularSegmentedRedomap
       , regularSegmentedScan
       )
       where

import Control.Monad
import qualified Data.Map.Strict as M
import Data.Semigroup ((<>))

import Futhark.Transform.Rename
import Futhark.Representation.Kernels
import Futhark.Representation.SOACS.SOAC (nilFn)
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Pass.ExtractKernels.BlockedKernel

data SegmentedVersion = OneGroupOneSegment
                      | ManyGroupsOneSegment
                      deriving (Eq, Ord, Show)

-- | @regularSegmentedRedomap@ will generate code for a segmented redomap using
-- two different strategies, and dynamically deciding which one to use based on
-- the number of segments and segment size. We use the (static) @group_size@ to
-- decide which of the following two strategies to choose:
--
-- * Large: uses one or more groups to process a single segment. If multiple
--   groups are used per segment, the intermediate reduction results must be
--   recursively reduced, until there is only a single value per segment.
--
--       Each thread /can/ read multiple elements, which will greatly increase
--   performance; however, if the reduction is non-commutative the input array
--   will be transposed (by the KernelBabysitter) to enable memory coalesced
--   accesses. Currently we will always make each thread read as many elements
--   as it can, but this /could/ be unfavorable because of the transpose: in
--   the case where each thread can only read 2 elements, the cost of the
--   transpose might not be worth the performance gained by letting each thread
--   read multiple elements. This could be investigated more in depth in the
--   future (TODO)
--
-- * Small: is used to let each group process *multiple* segments within a
--   group. We will only use this approach when we can process at least two
--   segments within a single group. In those cases, we would normally allocate
--   a /whole/ group per segment with the large strategy, but at most 50% of the
--   threads in the group would have any element to read, which becomes highly
--   inefficient.
regularSegmentedRedomap :: (HasScope Kernels m, MonadBinder m, Lore m ~ Kernels) =>
                           SubExp            -- segment_size
                        -> SubExp            -- num_segments
                        -> [SubExp]          -- nest_sizes = the sizes of the maps on "top" of this redomap
                        -> Pattern Kernels   -- flat_pat ... pat where each type is array with dim [w]
                        -> Pattern Kernels   -- pat
                        -> SubExp            -- w = total_num_elements
                        -> Commutativity     -- comm
                        -> Lambda InKernel   -- reduce_lam
                        -> Lambda InKernel   -- fold_lam = this lambda performs both the map-part and
                                             -- reduce-part of a redomap (described in redomap paper)
                        -> [(VName, SubExp)] -- ispace = pair of (gtid, size) for the maps on "top" of this redomap
                        -> [KernelInput]     -- inps = inputs that can be looked up by using the gtids from ispace
                        -> [SubExp]          -- nes
                        -> [VName]           -- arrs_flat
                        -> m ()
regularSegmentedRedomap segment_size num_segments nest_sizes flat_pat
                        pat w comm reduce_lam fold_lam ispace inps nes arrs_flat = do
  unless (null $ patternContextElements pat) $ fail "regularSegmentedRedomap result pattern contains context elements, and Rasmus did not think this would ever happen."

  -- the result of the "map" part of a redomap has to be stored somewhere within
  -- the chunking loop of a kernel. The current way to do this is to make some
  -- scratch space initially, and each thread will get a part of this by
  -- splitting it. Finally it is returned as a result of the kernel (to not
  -- break functional semantics).
  map_out_arrs <- forM (drop num_redres $ patternIdents pat) $ \(Ident name t) -> do
    tmp <- letExp (baseString name <> "_out_in") $
           BasicOp $ Scratch (elemType t) (arrayDims t)
    -- This reshape will not always work.
    letExp (baseString name ++ "_out_in") $
      BasicOp $ Reshape (reshapeOuter [DimNew w] (length nest_sizes+1) $ arrayShape t) tmp

  -- Check that we're only dealing with arrays with dimension [w]
  forM_ arrs_flat $ \arr -> do
    tp <- lookupType arr
    case tp of
      -- TODO: this won't work if the reduction operator works on lists... but
      -- they seem to be handled in some other way (which makes sense)
      Array _primtp (Shape (flatsize:_)) _uniqness ->
        when (flatsize /= w) $
          fail$ "regularSegmentedRedomap: first dimension of array has incorrect size " ++ pretty arr ++ ":" ++ pretty tp
      _ ->
        fail $ "regularSegmentedRedomap: non array encountered " ++ pretty arr ++ ":" ++ pretty tp

  -- The pattern passed to chunkLambda must have exactly *one* array dimension,
  -- to get the correct size of [chunk_size]type.
  --
  -- TODO: not sure if this will work when result of map is multidimensional,
  -- or if reduction operator uses lists... must check
  chunk_pat <- fmap (Pattern []) $ forM (patternValueElements pat) $ \pat_e ->
    case patElemType pat_e of
      Array ty (Shape (dim0:_)) u -> do
          vn' <- newName $ patElemName pat_e
          return $ PatElem vn' $ Array ty (Shape [dim0]) u
      _ -> fail $ "segmentedRedomap: result pattern is not array " ++ pretty pat_e

  chunk_fold_lam <- chunkLambda chunk_pat nes fold_lam

  kern_chunk_fold_lam <- kerneliseLambda nes chunk_fold_lam

  let chunk_red_pat = Pattern [] $ take num_redres $ patternValueElements chunk_pat
  kern_chunk_reduce_lam <- kerneliseLambda nes =<< chunkLambda chunk_red_pat nes reduce_lam

  -- the lambda for a GroupReduce needs these two extra parameters
  my_index <- newVName "my_index"
  other_offset <- newVName "other_offset"
  let my_index_param = Param my_index (Prim int32)
  let other_offset_param = Param other_offset (Prim int32)
  let reduce_lam' = reduce_lam { lambdaParams = my_index_param :
                                                other_offset_param :
                                                lambdaParams reduce_lam
                               }
  flag_reduce_lam <- addFlagToLambda nes reduce_lam
  let flag_reduce_lam' = flag_reduce_lam { lambdaParams = my_index_param :
                                                          other_offset_param :
                                                          lambdaParams flag_reduce_lam
                                         }


  -- TODO: 'blockedReductionStream' in BlockedKernel.hs which is very similar
  -- performs a copy here... however, I have not seen a need for it yet.

  group_size <- getSize "group_size" SizeGroup
  num_groups_hint <- getSize "num_groups_hint" SizeNumGroups

  -- Here we make a small optimization: if we will use the large kernel, and
  -- only one group per segment, we can simplify the calcualtions within the
  -- kernel for the indexes of which segment is it working on; therefore we
  -- create two different kernels (this will increase the final code size a bit
  -- though). TODO: test how much we win by doing this.

  (num_groups_per_segment, _) <-
    calcGroupsPerSegmentAndElementsPerThread
    segment_size num_segments num_groups_hint group_size ManyGroupsOneSegment

  let all_arrs = arrs_flat ++ map_out_arrs
  (large_1_ses, large_1_stms) <- runBinder $
    useLargeOnePerSeg group_size all_arrs reduce_lam' kern_chunk_fold_lam
  (large_m_ses, large_m_stms) <- runBinder $
    useLargeMultiRecursiveReduce group_size all_arrs reduce_lam' kern_chunk_fold_lam
    kern_chunk_reduce_lam flag_reduce_lam'

  let e_large_seg = eIf (eCmpOp (CmpEq $ IntType Int32) (eSubExp num_groups_per_segment)
                                                        (eSubExp one))
                        (mkBodyM large_1_stms large_1_ses)
                        (mkBodyM large_m_stms large_m_ses)


  (small_ses, small_stms) <- runBinder $ useSmallKernel group_size map_out_arrs flag_reduce_lam'

  -- if (group_size/2) < segment_size, means that we will not be able to fit two
  -- segments into one group, and therefore we should not use the kernel that
  -- relies on this.
  e <- eIf (eCmpOp (CmpSlt Int32) (eBinOp (SQuot Int32) (eSubExp group_size) (eSubExp two))
                                  (eSubExp segment_size))
         (eBody [e_large_seg])
         (mkBodyM small_stms small_ses)

  redres_pes <- forM (take num_redres (patternValueElements pat)) $ \pe -> do
    vn' <- newName $ patElemName pe
    return $ PatElem vn' $ patElemType pe `setArrayDims` [num_segments]
  let mapres_pes = drop num_redres $ patternValueElements flat_pat
  let unreshaped_pat = Pattern [] $ redres_pes ++ mapres_pes

  letBind_ unreshaped_pat e

  forM_ (zip (patternValueElements unreshaped_pat)
             (patternValueElements pat)) $ \(kpe, pe) ->
    letBind_ (Pattern [] [pe]) $
    BasicOp $ Reshape [DimNew se | se <- arrayDims $ patElemAttr pe]
    (patElemName kpe)

  where
    one = constant (1 :: Int32)
    two = constant (2 :: Int32)

    -- number of reduction results (tuple size for reduction operator)
    num_redres = length nes

    ----------------------------------------------------------------------------
    -- The functions below generate all the needed code for the two different
    -- version of segmented-redomap (one group per segment, and many groups per
    -- segment).
    --
    -- We rename statements before adding them because the same lambdas
    -- (reduce/fold) are used multiple times, and we do not want to bind the
    -- same VName twice (as this is a type error)
    ----------------------------------------------------------------------------
    useLargeOnePerSeg group_size all_arrs reduce_lam' kern_chunk_fold_lam = do
      mapres_pes <- forM (drop num_redres $ patternValueElements flat_pat) $ \pe -> do
        vn' <- newName $ patElemName pe
        return $ PatElem vn' $ patElemType pe

      (kernel, _, _) <-
        largeKernel group_size segment_size num_segments nest_sizes
        all_arrs comm reduce_lam' kern_chunk_fold_lam
        nes w OneGroupOneSegment
        ispace inps

      kernel_redres_pes <- forM (take num_redres (patternValueElements pat)) $ \pe -> do
        vn' <- newName $ patElemName pe
        return $ PatElem vn' $ patElemType pe `setArrayDims` [num_segments]

      let kernel_pat = Pattern [] $ kernel_redres_pes ++ mapres_pes

      addStm =<< renameStm (Let kernel_pat (defAux ()) $ Op kernel)
      return $ map (Var . patElemName) $ patternValueElements kernel_pat

    ----------------------------------------------------------------------------
    useLargeMultiRecursiveReduce group_size all_arrs reduce_lam' kern_chunk_fold_lam kern_chunk_reduce_lam flag_reduce_lam' = do
      mapres_pes <- forM (drop num_redres $ patternValueElements flat_pat) $ \pe -> do
        vn' <- newName $ patElemName pe
        return $ PatElem vn' $ patElemType pe

      (firstkernel, num_groups_used, num_groups_per_segment) <-
        largeKernel group_size segment_size num_segments nest_sizes
        all_arrs comm reduce_lam' kern_chunk_fold_lam
        nes w ManyGroupsOneSegment
        ispace inps

      firstkernel_redres_pes <- forM (take num_redres (patternValueElements pat)) $ \pe -> do
        vn' <- newName $ patElemName pe
        return $ PatElem vn' $ patElemType pe `setArrayDims` [num_groups_used]

      let first_pat = Pattern [] $ firstkernel_redres_pes ++ mapres_pes
      addStm =<< renameStm (Let first_pat (defAux ()) $ Op firstkernel)

      let new_segment_size = num_groups_per_segment
      let new_total_elems = num_groups_used
      let tmp_redres = map patElemName firstkernel_redres_pes

      (finalredres, part_two_stms) <- runBinder $ performFinalReduction
        new_segment_size new_total_elems tmp_redres
        reduce_lam' kern_chunk_reduce_lam flag_reduce_lam'

      mapM_ (addStm <=< renameStm) part_two_stms

      return $ finalredres ++ map (Var . patElemName) mapres_pes

    ----------------------------------------------------------------------------
    -- The "recursive" reduction step. However, will always do this using
    -- exactly one extra step. Either by using the small kernel, or by using the
    -- large kernel with one group per segment.
    performFinalReduction new_segment_size new_total_elems tmp_redres
                          reduce_lam' kern_chunk_reduce_lam flag_reduce_lam' = do
      group_size <- getSize "group_size" SizeGroup

      -- Large kernel, using one group per segment (ogps)
      (large_ses, large_stms) <- runBinder $ do
        (large_kernel, _, _) <- largeKernel group_size new_segment_size num_segments nest_sizes
          tmp_redres comm reduce_lam' kern_chunk_reduce_lam
          nes new_total_elems OneGroupOneSegment
          ispace inps
        letTupExp' "kernel_result" $ Op large_kernel

      -- Small kernel, using one group many segments (ogms)
      (small_ses, small_stms) <- runBinder $ do
        red_scratch_arrs <- forM (take num_redres $ patternIdents pat) $ \(Ident name t) -> do
          tmp <- letExp (baseString name <> "_redres_scratch") $
                 BasicOp $ Scratch (elemType t) (arrayDims t)
          letExp (baseString name ++ "_redres_scratch") $
                  BasicOp $ Reshape [DimNew num_segments] tmp
        kernel <- smallKernel group_size new_segment_size num_segments
                              tmp_redres red_scratch_arrs
                              comm flag_reduce_lam' reduce_lam
                              nes new_total_elems ispace inps
        letTupExp' "kernel_result" $ Op kernel

      e <- eIf (eCmpOp (CmpSlt Int32)
                 (eBinOp (SQuot Int32) (eSubExp group_size) (eSubExp two))
                 (eSubExp new_segment_size))
         (mkBodyM large_stms large_ses)
         (mkBodyM small_stms small_ses)

      letTupExp' "step_two_kernel_result" e

    ----------------------------------------------------------------------------
    useSmallKernel group_size map_out_arrs flag_reduce_lam' = do
      red_scratch_arrs <-
        forM (take num_redres $ patternIdents pat) $ \(Ident name t) -> do
        tmp <- letExp (baseString name <> "_redres_scratch") $
               BasicOp $ Scratch (elemType t) (arrayDims t)
        let shape_change = reshapeOuter [DimNew num_segments]
                           (length nest_sizes) (arrayShape t)
        letExp (baseString name ++ "_redres_scratch") $
          BasicOp $ Reshape shape_change tmp

      let scratch_arrays = red_scratch_arrs ++ map_out_arrs

      kernel <- smallKernel group_size segment_size num_segments
                            arrs_flat scratch_arrays
                            comm flag_reduce_lam' fold_lam
                            nes w ispace inps
      letTupExp' "kernel_result" $ Op kernel

largeKernel :: (MonadBinder m, Lore m ~ Kernels) =>
          SubExp            -- group_size
       -> SubExp            -- segment_size
       -> SubExp            -- num_segments
       -> [SubExp]          -- nest sizes
       -> [VName]           -- all_arrs: flat arrays (also the "map_out" ones)
       -> Commutativity     -- comm
       -> Lambda InKernel   -- reduce_lam
       -> Lambda InKernel   -- kern_chunk_fold_lam
       -> [SubExp]          -- nes
       -> SubExp            -- w = total_num_elements
       -> SegmentedVersion  -- segver
       -> [(VName, SubExp)] -- ispace = pair of (gtid, size) for the maps on "top" of this redomap
       -> [KernelInput]     -- inps = inputs that can be looked up by using the gtids from ispace
       -> m (Kernel InKernel, SubExp, SubExp)
largeKernel group_size segment_size num_segments nest_sizes all_arrs comm
            reduce_lam' kern_chunk_fold_lam
            nes w segver ispace inps = do
  let num_redres = length nes -- number of reduction results (tuple size for
                              -- reduction operator)

  num_groups_hint <- getSize "num_groups_hint" SizeNumGroups

  (num_groups_per_segment, elements_per_thread) <-
    calcGroupsPerSegmentAndElementsPerThread segment_size num_segments num_groups_hint group_size segver

  num_groups <- letSubExp "num_groups" $
    case segver of
      OneGroupOneSegment -> BasicOp $ SubExp num_segments
      ManyGroupsOneSegment -> BasicOp $ BinOp (Mul Int32) num_segments num_groups_per_segment

  num_threads <- letSubExp "num_threads" $
    BasicOp $ BinOp (Mul Int32) num_groups group_size

  threads_within_segment <- letSubExp "threads_within_segment" $
    BasicOp $ BinOp (Mul Int32) group_size num_groups_per_segment

  gtid_vn <- newVName "gtid"
  gtid_ln <- newVName "gtid"

  -- the array passed here is the structure for how to layout the kernel space
  space <- newKernelSpace (num_groups, group_size, num_threads) $
    FlatThreadSpace $ ispace ++ [(gtid_vn, num_groups_per_segment),(gtid_ln,group_size)]

  let red_ts = take num_redres $ lambdaReturnType kern_chunk_fold_lam
  let map_ts = map rowType $ drop num_redres $ lambdaReturnType kern_chunk_fold_lam
  let kernel_return_types = red_ts ++ map_ts

  let ordering = case comm of Commutative -> SplitStrided threads_within_segment
                              Noncommutative -> SplitContiguous

  let stride = case ordering of SplitStrided s -> s
                                SplitContiguous -> one

  let each_thread = do
        segment_index <- letSubExp "segment_index" $
          BasicOp $ BinOp (SQuot Int32) (Var $ spaceGroupId space) num_groups_per_segment

        -- localId + (group_size * (groupId % num_groups_per_segment))
        index_within_segment <- letSubExp "index_within_segment" =<<
          eBinOp (Add Int32)
              (eSubExp $ Var gtid_ln)
              (eBinOp (Mul Int32)
                 (eSubExp group_size)
                 (eBinOp (SRem Int32) (eSubExp $ Var $ spaceGroupId space) (eSubExp num_groups_per_segment))
              )

        (in_segment_offset,offset) <-
          makeOffsetExp ordering index_within_segment elements_per_thread segment_index

        let (_, chunksize, [], arr_params) =
              partitionChunkedKernelFoldParameters 0 $ lambdaParams kern_chunk_fold_lam
        let chunksize_se = Var $ paramName chunksize

        patelems_res_of_split <- forM arr_params $ \arr_param -> do
          let chunk_t = paramType arr_param `setOuterSize` Var (paramName chunksize)
          return $ PatElem (paramName arr_param) chunk_t

        letBind_ (Pattern [] [PatElem (paramName chunksize) $ paramType chunksize]) $
          Op $ SplitSpace ordering segment_size index_within_segment elements_per_thread

        addKernelInputStms inps

        forM_ (zip all_arrs patelems_res_of_split) $ \(arr, pe) -> do
          let pe_t = patElemType pe
              segment_dims = nest_sizes ++ arrayDims (pe_t `setOuterSize` segment_size)
          arr_nested <- letExp (baseString arr ++ "_nested") $
            BasicOp $ Reshape (map DimNew segment_dims) arr
          arr_nested_t <- lookupType arr_nested
          let slice = fullSlice arr_nested_t $ map (DimFix . Var . fst) ispace ++
                      [DimSlice in_segment_offset chunksize_se stride]
          letBind_ (Pattern [] [pe]) $ BasicOp $ Index arr_nested slice

        red_pes <- forM red_ts $ \red_t -> do
          pe_name <- newVName "chunk_fold_red"
          return $ PatElem pe_name red_t
        map_pes <- forM map_ts $ \map_t -> do
          pe_name <- newVName "chunk_fold_map"
          return $ PatElem pe_name $ map_t `arrayOfRow` chunksize_se

        -- we add the lets here, as we practially don't know if the resulting subexp
        -- is a Constant or a Var, so better be safe (?)
        addStms $ bodyStms (lambdaBody kern_chunk_fold_lam)
        addStms $ stmsFromList
          [ Let (Pattern [] [pe]) (defAux ()) $ BasicOp $ SubExp se
          | (pe,se) <- zip (red_pes ++ map_pes)
                       (bodyResult $ lambdaBody kern_chunk_fold_lam) ]

        -- Combine the reduction results from each thread. This will put results in
        -- local memory, so a GroupReduce can be performed on them
        combine_red_pes <- forM red_ts $ \red_t -> do
          pe_name <- newVName "chunk_fold_red"
          return $ PatElem pe_name $ red_t `arrayOfRow` group_size
        cids <- replicateM (length red_pes) $ newVName "cid"
        addStms $ stmsFromList
          [ Let (Pattern [] [pe']) (defAux ()) $
            Op $ Combine [(cid, group_size)] [patElemType pe] [] $
            Body () mempty [Var $ patElemName pe]
          | (cid, pe', pe) <- zip3 cids combine_red_pes red_pes ]

        final_red_pes <- forM (lambdaReturnType reduce_lam') $ \t -> do
          pe_name <- newVName "final_result"
          return $ PatElem pe_name t
        letBind_ (Pattern [] final_red_pes) $
          Op $ GroupReduce group_size reduce_lam' $
          zip nes (map patElemName combine_red_pes)

        return (final_red_pes, map_pes, offset)


  ((final_red_pes, map_pes, offset), stms) <- runBinder each_thread

  red_returns <- forM final_red_pes $ \pe ->
    return $ ThreadsReturn OneResultPerGroup $ Var $ patElemName pe
  map_returns <- forM map_pes $ \pe ->
    return $ ConcatReturns ordering w elements_per_thread
                           (Just offset) $
                           patElemName pe
  let kernel_returns = red_returns ++ map_returns

  let kerneldebughints = KernelDebugHints kernelname
                         [ ("num_segment", num_segments)
                         , ("segment_size", segment_size)
                         , ("num_groups", num_groups)
                         , ("group_size", group_size)
                         , ("elements_per_thread", elements_per_thread)
                         , ("num_groups_per_segment", num_groups_per_segment)
                         ]

  let kernel = Kernel kerneldebughints space kernel_return_types $
                  KernelBody () stms kernel_returns

  return (kernel, num_groups, num_groups_per_segment)

  where
    one = constant (1 :: Int32)

    commname = case comm of Commutative -> "comm"
                            Noncommutative -> "nocomm"

    kernelname = case segver of
      OneGroupOneSegment -> "segmented_redomap__large_" ++ commname ++ "_one"
      ManyGroupsOneSegment -> "segmented_redomap__large_"  ++ commname ++ "_many"

    makeOffsetExp SplitContiguous index_within_segment elements_per_thread segment_index = do
      in_segment_offset <- letSubExp "in_segment_offset" $
        BasicOp $ BinOp (Mul Int32) elements_per_thread index_within_segment
      offset <- letSubExp "offset" =<< eBinOp (Add Int32) (eSubExp in_segment_offset)
                (eBinOp (Mul Int32) (eSubExp segment_size) (eSubExp segment_index))
      return (in_segment_offset, offset)
    makeOffsetExp (SplitStrided _) index_within_segment _elements_per_thread segment_index = do
      offset <- letSubExp "offset" =<< eBinOp (Add Int32) (eSubExp index_within_segment)
                (eBinOp (Mul Int32) (eSubExp segment_size) (eSubExp segment_index))
      return (index_within_segment, offset)

calcGroupsPerSegmentAndElementsPerThread :: (MonadBinder m, Lore m ~ Kernels) =>
                        SubExp
                     -> SubExp
                     -> SubExp
                     -> SubExp
                     -> SegmentedVersion
                     -> m (SubExp, SubExp)
calcGroupsPerSegmentAndElementsPerThread segment_size num_segments
                                         num_groups_hint group_size segver = do
  num_groups_per_segment_hint <-
    letSubExp "num_groups_per_segment_hint" =<<
    case segver of
      OneGroupOneSegment -> eSubExp one
      ManyGroupsOneSegment -> eDivRoundingUp Int32 (eSubExp num_groups_hint)
                                                   (eSubExp num_segments)
  elements_per_thread <-
    letSubExp "elements_per_thread" =<<
    eDivRoundingUp Int32 (eSubExp segment_size)
                         (eBinOp (Mul Int32) (eSubExp group_size)
                                             (eSubExp num_groups_per_segment_hint))

  -- if we are using 1 element per thread, we might be launching too many
  -- groups. This expression will remedy this.
  --
  -- For example, if there are 3 segments of size 512, we are using group size
  -- 128, and @num_groups_hint@ is 256; then we would use 1 element per thread,
  -- and launch 256 groups. However, we only need 4 groups per segment to
  -- process all elements.
  num_groups_per_segment <-
    letSubExp "num_groups_per_segment" =<<
    case segver of
      OneGroupOneSegment -> eSubExp one
      ManyGroupsOneSegment ->
        eIf (eCmpOp (CmpEq $ IntType Int32) (eSubExp elements_per_thread) (eSubExp one))
          (eBody [eDivRoundingUp Int32 (eSubExp segment_size) (eSubExp group_size)])
          (mkBodyM mempty [num_groups_per_segment_hint])

  return (num_groups_per_segment, elements_per_thread)

  where
    one = constant (1 :: Int32)

smallKernel :: (MonadBinder m, Lore m ~ Kernels) =>
          SubExp            -- group_size
       -> SubExp            -- segment_size
       -> SubExp            -- num_segments
       -> [VName]           -- in_arrs: flat arrays (containing input to fold_lam)
       -> [VName]           -- scratch_arrs: Preallocated space that we can write into
       -> Commutativity     -- comm
       -> Lambda InKernel   -- flag_reduce_lam'
       -> Lambda InKernel   -- fold_lam
       -> [SubExp]          -- nes
       -> SubExp            -- w = total_num_elements
       -> [(VName, SubExp)] -- ispace = pair of (gtid, size) for the maps on "top" of this redomap
       -> [KernelInput]     -- inps = inputs that can be looked up by using the gtids from ispace
       -> m (Kernel InKernel)
smallKernel group_size segment_size num_segments in_arrs scratch_arrs
            comm flag_reduce_lam' fold_lam_unrenamed
            nes w ispace inps = do
  let num_redres = length nes -- number of reduction results (tuple size for
                              -- reduction operator)

  fold_lam <- renameLambda fold_lam_unrenamed

  num_segments_per_group <- letSubExp "num_segments_per_group" $
    BasicOp $ BinOp (SQuot Int32) group_size segment_size

  num_groups <- letSubExp "num_groups" =<<
    eDivRoundingUp Int32 (eSubExp num_segments) (eSubExp num_segments_per_group)

  num_threads <- letSubExp "num_threads" $
    BasicOp $ BinOp (Mul Int32) num_groups group_size

  active_threads_per_group <- letSubExp "active_threads_per_group" $
    BasicOp $ BinOp (Mul Int32) segment_size num_segments_per_group

  let remainder_last_group = eBinOp (SRem Int32) (eSubExp num_segments) (eSubExp num_segments_per_group)

  segments_in_last_group <- letSubExp "seg_in_last_group" =<<
    eIf (eCmpOp (CmpEq $ IntType Int32) remainder_last_group
                                        (eSubExp zero))
        (eBody [eSubExp num_segments_per_group])
        (eBody [remainder_last_group])

  active_threads_in_last_group <- letSubExp "active_threads_last_group" $
    BasicOp $ BinOp (Mul Int32) segment_size segments_in_last_group

  -- the array passed here is the structure for how to layout the kernel space
  space <- newKernelSpace (num_groups, group_size, num_threads) $
    FlatThreadSpace []

  ------------------------------------------------------------------------------
  -- What follows is the statements used in the kernel
  ------------------------------------------------------------------------------

  let lid = Var $ spaceLocalId space

  let (red_ts, map_ts) = splitAt num_redres $ lambdaReturnType fold_lam
  let kernel_return_types = red_ts ++ map_ts

  let wasted_thread_part1 = do
        let create_dummy_val (Prim ty) = return $ Constant $ blankPrimValue ty
            create_dummy_val (Array ty sh _) = letSubExp "dummy" $ BasicOp $ Scratch ty (shapeDims sh)
            create_dummy_val Mem{} = fail "segredomap, 'Mem' used as result type"
        dummy_vals <- mapM create_dummy_val kernel_return_types
        return (negone : dummy_vals)

  let normal_thread_part1 = do
        segment_index <- letSubExp "segment_index" =<<
          eBinOp (Add Int32)
            (eBinOp (SQuot Int32) (eSubExp $ Var $ spaceLocalId space) (eSubExp segment_size))
            (eBinOp (Mul Int32) (eSubExp $ Var $ spaceGroupId space) (eSubExp num_segments_per_group))

        index_within_segment <- letSubExp "index_within_segment" =<<
          eBinOp (SRem Int32) (eSubExp $ Var $ spaceLocalId space) (eSubExp segment_size)

        offset <- makeOffsetExp index_within_segment segment_index

        red_pes <- forM red_ts $ \red_t -> do
          pe_name <- newVName "fold_red"
          return $ PatElem pe_name red_t
        map_pes <- forM map_ts $ \map_t -> do
          pe_name <- newVName "fold_map"
          return $ PatElem pe_name map_t

        addManualIspaceCalcStms segment_index ispace

        addKernelInputStms inps

        -- Index input array to get arguments to fold_lam
        let arr_params = drop num_redres $ lambdaParams fold_lam
        let nonred_lamparam_pes = map
              (\p -> PatElem (paramName p) (paramType p)) arr_params
        forM_ (zip in_arrs nonred_lamparam_pes) $ \(arr, pe) -> do
          tp <- lookupType arr
          let slice = fullSlice tp [DimFix offset]
          letBind_ (Pattern [] [pe]) $ BasicOp $ Index arr slice

        -- Bind neutral element (serves as the reduction arguments to fold_lam)
        forM_ (zip nes (take num_redres $ lambdaParams fold_lam)) $ \(ne,param) -> do
          let pe = PatElem (paramName param) (paramType param)
          letBind_ (Pattern [] [pe]) $ BasicOp $ SubExp ne

        addStms $ bodyStms $ lambdaBody fold_lam

        -- we add the lets here, as we practially don't know if the resulting subexp
        -- is a Constant or a Var, so better be safe (?)
        addStms $ stmsFromList
          [ Let (Pattern [] [pe]) (defAux ()) $ BasicOp $ SubExp se
          | (pe,se) <- zip (red_pes ++ map_pes) (bodyResult $ lambdaBody fold_lam) ]

        let mapoffset = offset
        let mapret_elems = map (Var . patElemName) map_pes
        let redres_elems = map (Var . patElemName) red_pes
        return (mapoffset : redres_elems ++ mapret_elems)

  let all_threads red_pes = do
        isfirstinsegment <- letExp "isfirstinsegment" =<<
          eCmpOp (CmpEq $ IntType Int32)
            (eBinOp (SRem Int32) (eSubExp lid) (eSubExp segment_size))
            (eSubExp zero)

        -- We will perform a segmented-scan, so all the prime variables here
        -- include the flag, which is the first argument to flag_reduce_lam
        let red_pes_wflag = PatElem isfirstinsegment (Prim Bool) : red_pes
        let red_ts_wflag = Prim Bool : red_ts

        -- Combine the reduction results from each thread. This will put results in
        -- local memory, so a GroupReduce/GroupScan can be performed on them
        combine_red_pes' <- forM red_ts_wflag $ \red_t -> do
          pe_name <- newVName "chunk_fold_red"
          return $ PatElem pe_name $ red_t `arrayOfRow` group_size
        cids <- replicateM (length red_pes_wflag) $ newVName "cid"
        addStms $ stmsFromList [ Let (Pattern [] [pe']) (defAux ()) $ Op $
                                 Combine [(cid, group_size)] [patElemType pe] [] $
                                 Body () mempty [Var $ patElemName pe]
                               | (cid, pe', pe) <- zip3 cids combine_red_pes' red_pes_wflag ]

        scan_red_pes_wflag <- forM red_ts_wflag $ \red_t -> do
          pe_name <- newVName "scanned"
          return $ PatElem pe_name $ red_t `arrayOfRow` group_size
        let scan_red_pes = drop 1 scan_red_pes_wflag
        letBind_ (Pattern [] scan_red_pes_wflag) $ Op $
          GroupScan group_size flag_reduce_lam' $
          zip (false:nes) (map patElemName combine_red_pes')

        return scan_red_pes

  let normal_thread_part2 scan_red_pes = do
        segment_index <- letSubExp "segment_index" =<<
          eBinOp (Add Int32)
            (eBinOp (SQuot Int32) (eSubExp $ Var $ spaceLocalId space) (eSubExp segment_size))
            (eBinOp (Mul Int32) (eSubExp $ Var $ spaceGroupId space) (eSubExp num_segments_per_group))

        islastinsegment <- letExp "islastinseg" =<< eCmpOp (CmpEq $ IntType Int32)
            (eBinOp (SRem Int32) (eSubExp lid) (eSubExp segment_size))
            (eBinOp (Sub Int32) (eSubExp segment_size) (eSubExp one))

        redoffset <- letSubExp "redoffset" =<<
            eIf (eSubExp $ Var islastinsegment)
              (eBody [eSubExp segment_index])
              (mkBodyM mempty [negone])

        redret_elems <- fmap (map Var) $ letTupExp "red_return_elem" =<<
          eIf (eSubExp $ Var islastinsegment)
            (eBody [return $ BasicOp $ Index (patElemName pe) (fullSlice (patElemType pe) [DimFix lid])
                   | pe <- scan_red_pes])
            (mkBodyM mempty nes)

        return (redoffset : redret_elems)


  let picknchoose = do
        is_last_group <- letSubExp "islastgroup" =<<
            eCmpOp (CmpEq $ IntType Int32)
                (eSubExp $ Var $ spaceGroupId space)
                (eBinOp (Sub Int32) (eSubExp num_groups) (eSubExp one))

        active_threads_this_group <- letSubExp "active_thread_this_group" =<<
            eIf (eSubExp is_last_group)
               (eBody [eSubExp active_threads_in_last_group])
               (eBody [eSubExp active_threads_per_group])

        isactive <- letSubExp "isactive" =<<
          eCmpOp (CmpSlt Int32) (eSubExp lid) (eSubExp active_threads_this_group)

        -- Part 1: All active threads reads element from input array and applies
        -- folding function. "wasted" threads will just create dummy values
        (normal_res1, normal_stms1) <- runBinder normal_thread_part1
        (wasted_res1, wasted_stms1) <- runBinder wasted_thread_part1

        -- we could just have used letTupExp, but this would not give as nice
        -- names in the generated code
        mapoffset_pe <- (`PatElem` i32) <$> newVName "mapoffset"
        redtmp_pes <- forM red_ts $ \red_t -> do
          pe_name <- newVName "redtmp_res"
          return $ PatElem pe_name red_t
        map_pes <- forM map_ts $ \map_t -> do
          pe_name <- newVName "map_res"
          return $ PatElem pe_name map_t

        e1 <- eIf (eSubExp isactive)
            (mkBodyM normal_stms1 normal_res1)
            (mkBodyM wasted_stms1 wasted_res1)
        letBind_ (Pattern [] (mapoffset_pe:redtmp_pes++map_pes)) e1

        -- Part 2: All threads participate in Comine & GroupScan
        scan_red_pes <- all_threads redtmp_pes

        -- Part 3: Active thread that are the last element in segment, should
        -- write the element from local memory to the output array
        (normal_res2, normal_stms2) <- runBinder $ normal_thread_part2 scan_red_pes

        redoffset_pe <- (`PatElem` i32) <$> newVName "redoffset"
        red_pes <- forM red_ts $ \red_t -> do
          pe_name <- newVName "red_res"
          return $ PatElem pe_name red_t

        e2 <- eIf (eSubExp isactive)
            (mkBodyM normal_stms2 normal_res2)
            (mkBodyM mempty (negone : nes))
        letBind_ (Pattern [] (redoffset_pe:red_pes)) e2

        return $ map (Var . patElemName) $ redoffset_pe:mapoffset_pe:red_pes++map_pes

  (redoffset:mapoffset:redmapres, stms) <- runBinder picknchoose
  let (finalredvals, finalmapvals) = splitAt num_redres redmapres

  -- To be able to only return elements from some threads, we exploit the fact
  -- that WriteReturn with offset=-1, won't do anything.
  red_returns <- forM (zip finalredvals $ take num_redres scratch_arrs) $ \(se, scarr) ->
    return $ WriteReturn [num_segments] scarr [([redoffset], se)]
  map_returns <- forM (zip finalmapvals $ drop num_redres scratch_arrs) $ \(se, scarr) ->
    return $ WriteReturn [w] scarr [([mapoffset], se)]
  let kernel_returns = red_returns ++ map_returns

  let kerneldebughints = KernelDebugHints kernelname
                         [ ("num_segment", num_segments)
                         , ("segment_size", segment_size)
                         , ("num_groups", num_groups)
                         , ("group_size", group_size)
                         , ("num_segments_per_group", num_segments_per_group)
                         , ("active_threads_per_group", active_threads_per_group)
                         ]

  let kernel = Kernel kerneldebughints space kernel_return_types $
                  KernelBody () stms kernel_returns

  return kernel

  where
    i32 = Prim $ IntType Int32
    zero = constant (0 :: Int32)
    one = constant (1 :: Int32)
    negone = constant (-1 :: Int32)
    false = constant False


    commname = case comm of Commutative -> "comm"
                            Noncommutative -> "nocomm"
    kernelname = "segmented_redomap__small_" ++ commname

    makeOffsetExp index_within_segment segment_index = do
      e <- eBinOp (Add Int32)
             (eSubExp index_within_segment)
             (eBinOp (Mul Int32) (eSubExp segment_size) (eSubExp segment_index))
      letSubExp "offset" e

addKernelInputStms :: (MonadBinder m, Lore m ~ InKernel) =>
                      [KernelInput]
                   -> m ()
addKernelInputStms = mapM_ $ \kin -> do
        let pe = PatElem (kernelInputName kin) (kernelInputType kin)
        let arr = kernelInputArray kin
        arrtp <- lookupType arr
        let slice = fullSlice arrtp [DimFix se | se <- kernelInputIndices kin]
        letBind (Pattern [] [pe]) $ BasicOp $ Index arr slice

-- | Manually calculate the values for the ispace identifiers, when the
-- 'SpaceStructure' won't do. ispace is the dimensions of the overlaying maps.
--
-- If the input is @i [(a_vn, a), (b_vn, b), (c_vn, c)]@ then @i@ should hit all
-- the values [0,a*b*c). We can calculate the indexes for the other dimensions:
--
-- >  c_vn = i % c
-- >  b_vn = (i/c) % b
-- >  a_vn = ((i/c)/b) % a
addManualIspaceCalcStms :: (MonadBinder m, Lore m ~ InKernel) =>
                           SubExp
                        -> [(VName, SubExp)]
                        -> m ()
addManualIspaceCalcStms outer_index ispace = do
        -- TODO: The ispace index is calculated in a bit different way than it
        -- would have been done if the ThreadSpace was used. However, this
        -- works. Maybe ask Troels if doing it the other way has some benefit?
        let calc_ispace_index prev_val (vn,size) = do
              let pe = PatElem vn (Prim $ IntType Int32)
              letBind_ (Pattern [] [pe]) $ BasicOp $ BinOp (SRem Int32) prev_val size
              letSubExp "tmp_val" $ BasicOp $ BinOp (SQuot Int32) prev_val size
        foldM_ calc_ispace_index outer_index (reverse ispace)

addFlagToLambda :: (MonadBinder m, Lore m ~ Kernels) =>
                   [SubExp] -> Lambda InKernel -> m (Lambda InKernel)
addFlagToLambda nes lam = do
  let num_accs = length nes
  x_flag <- newVName "x_flag"
  y_flag <- newVName "y_flag"
  let x_flag_param = Param x_flag $ Prim Bool
      y_flag_param = Param y_flag $ Prim Bool
      (x_params, y_params) = splitAt num_accs $ lambdaParams lam
      params = [x_flag_param] ++ x_params ++ [y_flag_param] ++ y_params

  body <- runBodyBinder $ localScope (scopeOfLParams params) $ do
    new_flag <- letSubExp "new_flag" $
                BasicOp $ BinOp LogOr (Var x_flag) (Var y_flag)
    lhs <- fmap (map Var) $ letTupExp "seg_lhs" $ If (Var y_flag)
           (resultBody nes)
           (resultBody $ map (Var . paramName) x_params) $
           ifCommon $ map paramType x_params
    let rhs = map (Var . paramName) y_params

    lam' <- renameLambda lam -- avoid shadowing
    res <- eLambda lam' $ map eSubExp $ lhs ++ rhs

    return $ resultBody $ new_flag : res

  return Lambda { lambdaParams = params
                , lambdaBody = body
                , lambdaReturnType = Prim Bool : lambdaReturnType lam
                }

regularSegmentedScan :: (MonadBinder m, Lore m ~ Kernels) =>
                        SubExp
                     -> Pattern Kernels
                     -> SubExp
                     -> Lambda InKernel
                     -> Lambda InKernel
                     -> [(VName, SubExp)] -> [KernelInput]
                     -> [SubExp] -> [VName]
                     -> m ()
regularSegmentedScan segment_size pat w lam map_lam ispace inps nes arrs = do
  flags_i <- newVName "flags_i"

  unused_flag_array <- newVName "unused_flag_array"
  flags_body <-
    runBodyBinder $ localScope (M.singleton flags_i $ IndexInfo Int32) $ do
      segment_index <- letSubExp "segment_index" $
                       BasicOp $ BinOp (SRem Int32) (Var flags_i) segment_size
      start_of_segment <- letSubExp "start_of_segment" $
                          BasicOp $ CmpOp (CmpEq int32) segment_index zero
      let flag = start_of_segment
      return $ resultBody [flag]
  (mapk_bnds, mapk) <- mapKernelFromBody w (FlatThreadSpace [(flags_i, w)]) [] [Prim Bool] flags_body
  addStms mapk_bnds
  flags <- letExp "flags" $ Op mapk

  lam' <- addFlagToLambda nes lam

  flag_p <- newParam "flag" $ Prim Bool
  let map_lam' = map_lam { lambdaParams = flag_p : lambdaParams map_lam
                         , lambdaBody = (lambdaBody map_lam)
                           { bodyResult = Var (paramName flag_p) : bodyResult (lambdaBody map_lam) }
                         , lambdaReturnType = Prim Bool : lambdaReturnType map_lam
                         }

  let pat' = pat { patternValueElements = PatElem unused_flag_array
                                          (arrayOf (Prim Bool) (Shape [w]) NoUniqueness) :
                                          patternValueElements pat
                 }
  void $ blockedScan pat' w (lam', false:nes) (Commutative, nilFn, mempty) map_lam' segment_size ispace inps (flags:arrs)
  where zero = constant (0 :: Int32)
        false = constant False
