{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Multiversion segmented reduction.
module Futhark.Pass.ExtractKernels.Segmented
       ( regularSegmentedRedomapAsScan
       , regularSegmentedRedomap
       , regularSegmentedScan
       )
       where

import Control.Monad
import qualified Data.HashMap.Lazy as HM
import Data.Monoid

import Prelude

import Futhark.Transform.Rename
import qualified Futhark.Analysis.ScalExp as SE
import Futhark.Representation.Kernels
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Pass.ExtractKernels.BlockedKernel

data SegmentedVersion = OneGroupOneSegment
                      | ManyGroupsOneSegment
                      deriving (Eq, Ord, Show)

regularSegmentedRedomap :: (HasScope Kernels m, MonadBinder m, Lore m ~ Kernels) =>
                           SubExp            -- segment_size
                        -> SubExp            -- num_segments
                        -> [SubExp]          -- nest_sizes = the sizes of the maps on "top" of this redomap
                        -> Pattern Kernels   -- flat_pat ... pat where each type is array with dim [w]
                        -> Pattern Kernels   -- pat
                        -> Certificates      -- cs
                        -> SubExp            -- w = total_num_elements
                        -> Commutativity     -- comm
                        -> Lambda InKernel   -- reduce_lam
                        -> Lambda InKernel   -- fold_lam = this lambda performs both the map-part and
                                             -- reduce-part of a redomap (described in redomap paper)
                        -> [(VName, SubExp)] -- ispace = pair of (gtid, size) for the maps on "top" of this redomap
                        -> [KernelInput]     -- inps = more detailed information about the arguments for the maps on "top" of this redomap
                        -> [SubExp]          -- nes
                        -> [VName]           -- arrs_flat
                        -> m ()
regularSegmentedRedomap segment_size num_segments _nest_sizes flat_pat
                        pat cs w comm reduce_lam fold_lam _ispace _inps nes arrs_flat = do
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
    -- For example if the "map" part takes an input a 1D array and produces a 2D
    -- array, this is clearly wrong. See ex3.fut
    letExp (baseString name ++ "_out_in") $
            BasicOp $ Reshape cs [DimNew w] tmp

  -- Check that we're only dealing with arrays with dimension [w]
  forM_ arrs_flat $ \arr -> do
    tp <- lookupType arr
    case tp of
      -- FIXME: this won't work if the reduction operator works on lists... but
      -- they seem to be handled in some other way (which makes sense). Talk
      -- with troels if I should worry about this.
      Array _primtp (Shape [flatsize]) _uniqness ->
        when (flatsize /= w) $
          fail$ "regularSegmentedRedomap: flat array, with incorrect size encountered " ++ pretty arr
      _ -> fail $ "regularSegmentedRedomap: non-flat array encountered " ++ pretty arr

  -- The pattern passed to chunkLambda must have exactly *one* array dimension,
  -- to get the correct size of [chunk_size]type.
  --
  -- FIXME: not sure if this will work when result of map is multidimensional,
  -- or if reduction operator uses lists... must check
  chunk_pat <- fmap (Pattern []) $ forM (patternValueElements pat) $ \pat_e ->
    case patElemType pat_e of
      (Array ty (Shape (dim0:_)) u) -> do
          vn' <- newName $ patElemName pat_e
          return $ PatElem vn' BindVar $ Array ty (Shape [dim0]) u
      _ -> fail $ "segmentedRedomap: result pattern is not array " ++ pretty pat_e

  chunk_fold_lam <- chunkLambda chunk_pat nes fold_lam

  -- kernliseLambda intializes the value of the merge pattern for the reduction
  -- to the neutral element.
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

  -- FIXME: do we need to copy arrays here? :S
  -- see 'blockedReductionStream' in BlockedKernel.hs

  let all_arrs = arrs_flat ++ map_out_arrs
  (ogps_ses, ogps_stms) <- runBinder $ oneGroupPerSeg all_arrs reduce_lam' kern_chunk_fold_lam
  (mgps_ses, mgps_stms) <- runBinder $ manyGroupPerSeg all_arrs reduce_lam' kern_chunk_fold_lam kern_chunk_reduce_lam

  -- TODO: should unify this calculation somewhere, with the one taking place in
  -- 'groupPerSegmentKernel'
  num_groups_hint <- letSubExp "num_groups_hint" $ Op NumGroups
  num_groups_per_segment <- letSubExp "num_groups_per_segment" =<<
    eDivRoundingUp Int32 (eSubExp num_groups_hint) (eSubExp num_segments)

  e <- eIf (eCmpOp (CmpEq $ IntType Int32) (eSubExp num_groups_per_segment)
                                           (eSubExp one))
           (mkBodyM ogps_stms ogps_ses)
           (mkBodyM mgps_stms mgps_ses)

  redres_pes <- forM (take num_redres (patternValueElements pat)) $ \pe -> do
    vn' <- newName $ patElemName pe
    return $ PatElem vn' BindVar $ patElemType pe `setArrayDims` [num_segments]
  let mapres_pes = drop num_redres $ patternValueElements flat_pat
  let unreshaped_pat = Pattern [] $ redres_pes ++ mapres_pes

  addStm $ Let unreshaped_pat () e

  forM_ (zip (patternValueElements unreshaped_pat)
             (patternValueElements pat)) $ \(kpe, pe) ->
    addStm $ Let (Pattern [] [pe]) () $
             BasicOp $ Reshape cs [DimNew se | se <- arrayDims $ patElemAttr pe]
                               (patElemName kpe)

  where
    one = constant (1 :: Int32)

    -- number of reduction results (tuple size for reduction operator)
    num_redres = length nes

    ----------------------------------------------------------------------------
    -- The functions below generate all the needed code for the two different
    -- version of segmented-redomap.
    --
    -- We rename statements before adding them because the same lambdas
    -- (reduce/fold) are used multiple times, and we do not want to bind the
    -- same VName twice (as this is a type error)
    ----------------------------------------------------------------------------
    oneGroupPerSeg all_arrs reduce_lam' kern_chunk_fold_lam = do
      mapres_pes <- forM (drop num_redres $ patternValueElements flat_pat) $ \pe -> do
        vn' <- newName $ patElemName pe
        return $ PatElem vn' BindVar $ patElemType pe

      (kernel, _, _) <- groupPerSegmentKernel segment_size num_segments cs
        all_arrs comm reduce_lam' kern_chunk_fold_lam
        nes w OneGroupOneSegment

      kernel_redres_pes <- forM (take num_redres (patternValueElements pat)) $ \pe -> do
        vn' <- newName $ patElemName pe
        return $ PatElem vn' BindVar $ patElemType pe `setArrayDims` [num_segments]

      let kernel_pat = Pattern [] $ kernel_redres_pes ++ mapres_pes

      addStm =<< renameStm (Let kernel_pat () $ Op kernel)
      return $ map (Var . patElemName) $ patternValueElements kernel_pat

    ----------------------------------------------------------------------------
    manyGroupPerSeg all_arrs reduce_lam' kern_chunk_fold_lam kern_chunk_reduce_lam= do
      mapres_pes <- forM (drop num_redres $ patternValueElements flat_pat) $ \pe -> do
        vn' <- newName $ patElemName pe
        return $ PatElem vn' BindVar $ patElemType pe

      (firstkenel, num_groups_used, num_groups_per_segment) <- groupPerSegmentKernel segment_size num_segments cs
        all_arrs comm reduce_lam' kern_chunk_fold_lam
        nes w ManyGroupsOneSegment

      firstkernel_redres_pes <- forM (take num_redres (patternValueElements pat)) $ \pe -> do
        vn' <- newName $ patElemName pe
        return $ PatElem vn' BindVar $ patElemType pe `setArrayDims` [num_groups_used]

      let first_pat = Pattern [] $ firstkernel_redres_pes ++ mapres_pes
      addStm =<< renameStm (Let first_pat () $ Op firstkenel)

      (secondkernel, _, _) <- groupPerSegmentKernel num_groups_per_segment num_segments cs
        (map patElemName firstkernel_redres_pes) comm reduce_lam' kern_chunk_reduce_lam
        nes num_groups_used OneGroupOneSegment

      second_redres_pes <- forM (take num_redres (patternValueElements pat)) $ \pe -> do
        vn' <- newName $ patElemName pe
        return $ PatElem vn' BindVar $ patElemType pe `setArrayDims` [num_segments]

      let second_pat = Pattern [] second_redres_pes
      addStm =<< renameStm (Let second_pat () $ Op secondkernel)

      let result_pes = second_redres_pes ++ mapres_pes
      return $ map (Var . patElemName) result_pes

groupPerSegmentKernel :: (MonadBinder m, Lore m ~ Kernels) =>
          SubExp            -- segment_size
       -> SubExp            -- num_segments
       -> Certificates      -- cs
       -> [VName]           -- all_arrs: flat arrays (also the "map_out" ones)
       -> Commutativity     -- comm
       -> Lambda InKernel   -- reduce_lam
       -> Lambda InKernel   -- kern_chunk_fold_lam
       -> [SubExp]          -- nes
       -> SubExp            -- w = total_num_elements
       -> SegmentedVersion  -- segver
       -> m (Kernel InKernel, SubExp, SubExp)
groupPerSegmentKernel segment_size num_segments cs all_arrs comm
                      reduce_lam' kern_chunk_fold_lam
                      nes w segver = do
  let num_redres = length nes -- number of reduction results (tuple size for
                              -- reduction operator)

  group_size <- letSubExp "group_size" $ Op GroupSize
  num_groups_hint <- letSubExp "num_groups_hint" $ Op NumGroups

  num_groups_per_segment <-
    letSubExp "num_groups_per_segment" =<<
    case segver of
      OneGroupOneSegment -> eSubExp one
      ManyGroupsOneSegment -> eDivRoundingUp Int32 (eSubExp num_groups_hint)
                                                   (eSubExp num_segments)
  num_groups <- letSubExp "num_groups" $
    case segver of
      OneGroupOneSegment -> BasicOp $ SubExp num_segments
      ManyGroupsOneSegment -> BasicOp $ BinOp (Mul Int32) num_segments num_groups_per_segment

  num_threads <- letSubExp "num_threads" $
    BasicOp $ BinOp (Mul Int32) num_groups group_size

  elements_per_thread <-
    letSubExp "elements_per_thread" =<<
    eDivRoundingUp Int32 (eSubExp segment_size)
                         (eBinOp (Mul Int32) (eSubExp group_size)
                                             (eSubExp num_groups_per_segment))

  threads_within_segment <- letSubExp "threads_within_segment" $
    BasicOp $ BinOp (Mul Int32) group_size num_groups_per_segment

  -- the array passed here is the structure for how to layout the kernel space
  space <- newKernelSpace (num_groups, group_size, num_threads) $ FlatThreadSpace []

  ((segment_index, index_within_segment), calc_segindex_stms) <- runBinder $ do
    segment_index <- letSubExp "segment_index" $
      BasicOp $ BinOp (SDiv Int32) (Var $ spaceGroupId space) num_groups_per_segment

    -- localId + (group_size * (groupId % num_groups_per_segment))
    index_within_segment <- letSubExp "index_within_segment" =<<
      eBinOp (Add Int32)
          (eSubExp $ Var $ spaceLocalId space)
          (eBinOp (Mul Int32)
             (eSubExp group_size)
             (eBinOp (SMod Int32) (eSubExp $ Var $ spaceGroupId space) (eSubExp num_groups_per_segment))
          )
    return (segment_index, index_within_segment)

  let ordering = case comm of Commutative -> SplitStrided threads_within_segment
                              Noncommutative -> SplitContiguous

  let (_, chunksize, [], arr_params) =
        partitionChunkedKernelFoldParameters 0 $ lambdaParams kern_chunk_fold_lam
  let chunksize_se = Var $ paramName chunksize

  patelems_res_of_split <- forM arr_params $ \arr_param -> do
    let chunk_t = paramType arr_param `setOuterSize` Var (paramName chunksize)
    return $ PatElem (paramName arr_param) BindVar chunk_t

  let chunksize_stm =
        Let (Pattern [] [PatElem (paramName chunksize) BindVar $ paramType chunksize])
            () $
            Op $ SplitSpace ordering segment_size index_within_segment elements_per_thread

  let stride = case ordering of SplitStrided s -> s
                                SplitContiguous -> one

  (offset, offset_stms) <- runBinder $
    makeOffsetExp ordering index_within_segment elements_per_thread segment_index

  index_stms <- forM (zip all_arrs patelems_res_of_split) $ \(arr, pe) -> do
    tp <- lookupType arr
    let slice = fullSlice tp [DimSlice offset chunksize_se stride]
    return $ Let (Pattern [] [pe]) () $ BasicOp $ Index cs arr slice

  let red_ts = take num_redres $ lambdaReturnType kern_chunk_fold_lam
  let map_ts = map rowType $ drop num_redres $ lambdaReturnType kern_chunk_fold_lam
  let kernel_return_types = red_ts ++ map_ts

  red_pes <- forM red_ts $ \red_t -> do
    pe_name <- newVName "chunk_fold_red"
    return $ PatElem pe_name BindVar red_t
  map_pes <- forM map_ts $ \map_t -> do
    pe_name <- newVName "chunk_fold_map"
    return $ PatElem pe_name BindVar $ map_t `arrayOfRow` chunksize_se

  -- we add the lets here, as we practially don't know if the resulting subexp
  -- is a Constant or a Var, so better be safe (?)
  let fold_chunk_stms = bodyStms (lambdaBody kern_chunk_fold_lam) ++
        [ Let (Pattern [] [pe]) () $ BasicOp $ SubExp se
          | (pe,se) <- zip (red_pes ++ map_pes) (bodyResult $ lambdaBody kern_chunk_fold_lam) ]

  -- Combine the reduction results from each thread. This will put results in
  -- local memory, so a GroupReduce can be performed on them
  combine_red_pes <- forM red_ts $ \red_t -> do
    pe_name <- newVName "chunk_fold_red"
    return $ PatElem pe_name BindVar $ red_t `arrayOfRow` group_size
  let combine_stms = [ Let (Pattern [] [pe']) () $ Op $
                       Combine [(spaceLocalId space, group_size)] [patElemType pe]
                       (constant True) $
                       Body () [] [Var $ patElemName pe]
                     | (pe', pe) <- zip combine_red_pes red_pes ]

  final_red_pes <- forM (lambdaReturnType reduce_lam') $ \t -> do
    pe_name <- newVName "final_result"
    return $ PatElem pe_name BindVar t
  let group_reduce_stm = Let (Pattern [] final_red_pes) () $ Op $
                         GroupReduce group_size reduce_lam' $
                         zip nes $ map patElemName combine_red_pes

  red_returns <- forM final_red_pes $ \pe ->
    return $ ThreadsReturn (OneThreadPerGroup (constant (0::Int32))) $
                           Var $ patElemName pe
  map_returns <- forM map_pes $ \pe ->
    return $ ConcatReturns ordering w elements_per_thread
                           (Just offset) $
                           patElemName pe
  let kernel_returns = red_returns ++ map_returns

  let kernel = Kernel kernelname cs space kernel_return_types $
                  KernelBody () (calc_segindex_stms ++ [chunksize_stm] ++ offset_stms ++ index_stms ++
                                 fold_chunk_stms ++ combine_stms ++ [group_reduce_stm])
                  kernel_returns

  return (kernel, num_groups, num_groups_per_segment)

  where
    one = constant (1 :: Int32)

    kernelname = case segver of
      OneGroupOneSegment -> "segmented_redomap__one_group_one_segment"
      ManyGroupsOneSegment -> "segmented_redomap__many_groups_one_segment"

    makeOffsetExp SplitContiguous index_within_segment elements_per_thread segment_index = do
      e <- eBinOp (Add Int32)
             (eBinOp (Mul Int32) (eSubExp elements_per_thread) (eSubExp index_within_segment))
             (eBinOp (Mul Int32) (eSubExp segment_size) (eSubExp segment_index))
      letSubExp "offset" e
    makeOffsetExp (SplitStrided _) index_within_segment _elements_per_thread segment_index = do
      e <- eBinOp (Add Int32) (eSubExp index_within_segment)
             (eBinOp (Mul Int32) (eSubExp segment_size) (eSubExp segment_index))
      letSubExp "offset" e

regularSegmentedRedomapAsScan :: (HasScope Kernels m, MonadBinder m, Lore m ~ Kernels) =>
                                SubExp
                             -> SubExp
                             -> [SubExp]
                             -> Pattern Kernels
                             -> Pattern Kernels
                             -> Certificates
                             -> SubExp
                             -> Commutativity
                             -> Lambda InKernel
                             -> Lambda InKernel
                             -> [(VName, SubExp)]
                             -> [KernelInput]
                             -> [SubExp] -> [VName]
                             -> m ()
regularSegmentedRedomapAsScan segment_size num_segments nest_sizes flat_pat
                              pat cs w _comm lam fold_lam ispace inps nes arrs = do
  regularSegmentedScan segment_size flat_pat cs w lam fold_lam ispace inps nes arrs

  let (acc_arrs, map_arrs) = splitAt (length nes) $ patternValueIdents flat_pat
      (acc_pes, map_pes) = splitAt (length nes) $ patternValueElements pat
      acc_ts = lambdaReturnType lam
      acc_pat = Pattern [] acc_pes

  is <- replicateM (length nest_sizes) $ newVName "i"

  body <- runBodyBinder $ localScope (HM.fromList $ zip is $ repeat $ IndexInfo Int32) $ do
    let segment_id = flattenIndex
                     (map SE.intSubExpToScalExp nest_sizes)
                     (map (SE.intSubExpToScalExp . Var) is)
        offset = (segment_id + 1) * SE.intSubExpToScalExp segment_size - 1
    j <- letSubExp "j" =<< SE.fromScalExp offset
    vals <- forM acc_arrs $ \arr ->
      letSubExp "v" $ BasicOp $ Index [] (identName arr) $
      fullSlice (identType arr) [DimFix j]
    return $ resultBody vals

  (mapk_bnds, mapk) <-
    mapKernelFromBody [] num_segments (FlatThreadSpace $ zip is nest_sizes) [] acc_ts body
  mapM_ addStm mapk_bnds
  letBind_ acc_pat $ Op mapk

  forM_ (zip map_pes map_arrs) $ \(pe,arr) ->
    letBind_ (Pattern [] [pe]) $
    BasicOp $ Reshape [] (map DimNew $ arrayDims $ typeOf pe) $ identName arr

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
      (resultBody $ map (Var . paramName) x_params)
      (staticShapes $ map paramType x_params)
    let rhs = map (Var . paramName) y_params

    lam' <- renameLambda lam -- avoid shadowing
    res <- eLambda lam' $ lhs ++ rhs

    return $ resultBody $ new_flag : res

  return Lambda { lambdaParams = params
                , lambdaBody = body
                , lambdaReturnType = Prim Bool : lambdaReturnType lam
                }

regularSegmentedScan :: (MonadBinder m, Lore m ~ Kernels) =>
                        SubExp
                     -> Pattern Kernels
                     -> Certificates
                     -> SubExp
                     -> Lambda InKernel
                     -> Lambda InKernel
                     -> [(VName, SubExp)] -> [KernelInput]
                     -> [SubExp] -> [VName]
                     -> m ()
regularSegmentedScan segment_size pat cs w lam fold_lam ispace inps nes arrs = do
  flags_i <- newVName "flags_i"

  unused_flag_array <- newVName "unused_flag_array"
  flags_body <-
    runBodyBinder $ localScope (HM.singleton flags_i $ IndexInfo Int32) $ do
      segment_index <- letSubExp "segment_index" $
                       BasicOp $ BinOp (SRem Int32) (Var flags_i) segment_size
      start_of_segment <- letSubExp "start_of_segment" $
                          BasicOp $ CmpOp (CmpEq int32) segment_index zero
      flag <- letSubExp "flag" $
              If start_of_segment (resultBody [true]) (resultBody [false]) [Prim Bool]
      return $ resultBody [flag]
  (mapk_bnds, mapk) <- mapKernelFromBody [] w (FlatThreadSpace [(flags_i, w)]) [] [Prim Bool] flags_body
  mapM_ addStm mapk_bnds
  flags <- letExp "flags" $ Op mapk

  lam' <- addFlagToLambda nes lam
  fold_lam' <- addFlagToLambda nes fold_lam

  let pat' = pat { patternValueElements = PatElem unused_flag_array BindVar
                                          (arrayOf (Prim Bool) (Shape [w]) NoUniqueness) :
                                          patternValueElements pat
                 }
  blockedScan pat' cs w lam' fold_lam' segment_size ispace inps (false:nes) (flags:arrs)
  where zero = constant (0 :: Int32)
        true = constant True
        false = constant False
