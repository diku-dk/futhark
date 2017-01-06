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

regularSegmentedRedomap :: (HasScope Kernels m, MonadBinder m, Lore m ~ Kernels) =>
                           SubExp            -- segment_size
                        -> SubExp            -- num_segments
                        -> [SubExp]          -- nest_sizes -- FIXME: what is this?
                        -> Pattern Kernels   -- flat_pat
                        -> Pattern Kernels   -- pat
                        -> Certificates      -- cs
                        -> SubExp            -- w = total_num_elements
                        -> Commutativity     -- comm
                        -> Lambda InKernel   -- reduce_lam
                        -> Lambda InKernel   -- fold_lam = this lambda performs both the map-part and
                                             -- reduce-part of a redomap (described in redomap paper)
                        -> [(VName, SubExp)] -- ispace -- FIXME: what is this?
                        -> [KernelInput]     -- inps -- FIXME: what is this?
                        -> [SubExp]          -- nes
                        -> [VName]           -- arrs_flat
                        -> m ()
regularSegmentedRedomap segment_size num_segments _nest_sizes flat_pat
                        pat cs w comm reduce_lam fold_lam _ispace _inps nes arrs_flat = do
  let num_redres = length nes -- number of reduction results

  unless (null $ patternContextElements pat) $ fail "regularSegmentedRedomap result pattern contains context elements, and Rasmus did not think this would ever happen."

  -- the result of the "map" part of a redomap has to be stored somewhere within
  -- the chunking loop of a kernel. The current way to do this is to make some
  -- scratch space initially, and each thread will get a part of this by
  -- splitting it. Finally it is returned as a result of the kernel (to not
  -- break functional semantics).
  let arr_idents = drop num_redres $ patternIdents pat
  map_out_arrs <- forM arr_idents $ \(Ident name t) ->
    letExp (baseString name <> "_out_in") $
           BasicOp $ Scratch (elemType t) (arrayDims t)

  arrs_non_flat <- forM arrs_flat $ \arr -> do
    tp <- lookupType arr
    case tp of
      -- FIXME: this won't work if the reduction operator works on lists
      Array _primtp (Shape [flatsize]) _uniqness ->
        if flatsize /= w
        then fail $ "regularSegmentedRedomap: flat array, with incorrect size encountered " ++ pretty arr
        else
          letExp (baseString arr ++ "_non_flat") $
            BasicOp $ Reshape cs [DimNew num_segments, DimNew segment_size] arr
      _ -> fail $ "regularSegmentedRedomap: non-flat array encountered " ++ pretty arr

  let num_groups = num_segments
  group_size <- letSubExp "group_size" $ Op GroupSize
  num_threads <- letSubExp "num_threads" $ BasicOp $ BinOp (Mul Int32) num_groups group_size
  elements_per_thread <-
    letSubExp "elements_per_thread" =<<
    eDivRoundingUp Int32 (eSubExp segment_size) (eSubExp group_size)

  -- TODO: what does the last array do? ==> see FIXME in Representation/Kernels/Kernel.hs
  space <- newKernelSpace (num_groups, group_size, num_threads) []

  -- The result we want for each segment (or group?) -- right now it is the same
  pat' <- fmap (Pattern []) $ forM (patternValueElements pat) $ \pat_e ->
    case patElemType pat_e of
      (Array ty (Shape (dim0:dims)) u) ->
        if dim0 /= num_segments
        then fail "outer size of result pattern is not == num_segments"
        else do
          vn' <- newName $ patElemName pat_e
          return $ PatElem vn' BindVar (Array ty (Shape dims) u)
      _ -> fail "result pattern is not just arrays"

  chunk_fold_lam <- chunkLambda pat' nes fold_lam
  -- kernliseLambda intializes the value of the merge pattern for the reduction
  -- to the neutral element.
  kern_chunk_fold_lam <- kerneliseLambda nes chunk_fold_lam

  -- the lambda for a GroupReduce needs these two extra parameters
  my_index <- newVName "my_index"
  other_offset <- newVName "other_offset"
  let my_index_param = Param my_index (Prim int32)
  let other_offset_param = Param other_offset (Prim int32)
  let reduce_lam' = reduce_lam { lambdaParams = my_index_param :
                                                other_offset_param :
                                                lambdaParams reduce_lam
                               }

  -- FIXME: do we need to copy arrays? :S
  -- see 'blockedReductionStream' in BlockedKernel.hs

  the_kernel <- groupPerSegmentKernel segment_size num_segments cs
                  (arrs_non_flat ++ map_out_arrs) space
                  comm reduce_lam' kern_chunk_fold_lam nes
                  w elements_per_thread

  let kernel_pat = Pattern [] $ take num_redres (patternValueElements pat) ++
                                drop num_redres (patternValueElements flat_pat)

  -- TODO: BlockeKernel renames of one of these. I'm not sure if I should do it
  -- too
  addStm $ Let kernel_pat () $ Op the_kernel

  forM_ (zip (drop num_redres $ patternValueElements kernel_pat)
             (drop num_redres $ patternValueElements pat)) $ \(kpe, pe) ->
    addStm $ Let (Pattern [] [pe]) () $
             BasicOp $ Reshape cs [DimNew se | se <- arrayDims $ patElemAttr pe]
                               (patElemName kpe)

groupPerSegmentKernel :: (MonadBinder m, Lore m ~ Kernels) =>
          SubExp            -- segment_size
       -> SubExp            -- num_segments
       -> Certificates      -- cs
       -> [VName]           -- all_arrs: non_flat arrays (also the "map_out" ones)
       -> KernelSpace       -- space
       -> Commutativity     -- comm
       -> Lambda InKernel   -- reduce_lam
       -> Lambda InKernel   -- kern_chunk_fold_lam
       -> [SubExp]          -- nes
       -> SubExp            -- w = total_num_elements
       -> SubExp            -- elements_per_thread
       -> m (Kernel InKernel)
groupPerSegmentKernel segment_size _num_segments cs all_arrs space comm
                      reduce_lam' kern_chunk_fold_lam
                      nes w elements_per_thread = do
  let ordering = case comm of Commutative -> InOrder -- TODO: should be Disorder
                                                     -- see below
                              Noncommutative -> InOrder
  let num_redres = length nes -- number of reduction results

  let group_size = spaceGroupSize space
  let segment_index = spaceGroupId space
  let index_within_segment = spaceLocalId space
  let threads_within_segment = spaceGroupSize space

  let (_, chunk_size, [], arr_params) =
        partitionChunkedKernelFoldParameters 0 $ lambdaParams kern_chunk_fold_lam

  patelems_res_of_split <- forM arr_params $ \arr_param -> do
    let chunk_t = paramType arr_param `setOuterSize` Var (paramName chunk_size)
    return $ PatElem (paramName arr_param) BindVar chunk_t

  (all_arrs_indexed, index_stms) <- runBinder $ forM all_arrs $ \arr -> do
    tp <- lookupType arr
    let slice = fullSlice tp [DimFix $ Var segment_index]
    letExp (baseString arr <> "_indexed") $
           BasicOp $ Index cs arr slice

  -- a SplitArray on no arrays is invalid, so we need a special case if all_arrs
  -- is empty. Rasmus does not beleive this will happen, but better be safe
  let split_stm =
        if null all_arrs
        then Let (Pattern []
                  [PatElem (paramName chunk_size) BindVar $ paramType chunk_size])
                 () $
                 Op $ SplitSpace ordering segment_size (Var index_within_segment)
                                 threads_within_segment elements_per_thread
        else Let (Pattern [PatElem (paramName chunk_size) BindVar $ paramType chunk_size]
                          patelems_res_of_split)
                 () $
                 Op $ SplitArray ordering segment_size (Var index_within_segment)
                                 threads_within_segment elements_per_thread
                                 all_arrs_indexed

  let red_ts = take num_redres $ lambdaReturnType kern_chunk_fold_lam
  let map_ts = map rowType $ drop num_redres $ lambdaReturnType kern_chunk_fold_lam
  let kernel_return_types = red_ts ++ map_ts

  red_pes <- forM red_ts $ \red_t -> do
    pe_name <- newVName "chunk_fold_red"
    return $ PatElem pe_name BindVar red_t
  map_pes <- forM map_ts $ \map_t -> do
    pe_name <- newVName "chunk_fold_map"
    return $ PatElem pe_name BindVar $ map_t `arrayOfRow` Var (paramName chunk_size)

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


  (offset_for_first_value, offset_stms) <- runBinder $
    makeOffsetExp ordering index_within_segment segment_index
  red_returns <- forM final_red_pes $ \pe ->
    return $ ThreadsReturn (OneThreadPerGroup (constant (0::Int32))) $
                           Var $ patElemName pe
  map_returns <- forM map_pes $ \pe ->
    return $ ConcatReturns ordering w elements_per_thread
                           (Just offset_for_first_value) $
                           patElemName pe
  let kernel_returns = red_returns ++ map_returns

  return $ Kernel "segmented_redomap__one_group_per_segment" cs space kernel_return_types $
                  KernelBody () (index_stms ++ [split_stm] ++ fold_chunk_stms ++
                                 combine_stms ++ [group_reduce_stm] ++
                                 offset_stms)
                  kernel_returns
  where
    makeOffsetExp InOrder index_within_segment segment_index = do
      e <- eBinOp (Add Int32) (eSubExp $ Var index_within_segment) $
             eBinOp (Mul Int32) (eSubExp segment_size) (eSubExp $ Var segment_index)
      letSubExp "offset" e
    makeOffsetExp Disorder _ _ =
      fail "TODO: disorder not implemented, must change stride in CodeGen/ImpGen/Kernels.hs"

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
