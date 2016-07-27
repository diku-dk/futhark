{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.Pass.ExtractKernels.BlockedKernel
       ( blockedReduction
       , blockedReductionStream
       , blockedMap
       , blockedScan
       , blockedSegmentedScan
       , blockedKernelSize

       , chunkLambda

       , mapKernel
       , mapKernelFromBody
       , KernelInput(..)
       , mapKernelSkeleton

       , newKernelSpace
       )
       where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import Prelude

import Futhark.Representation.Kernels
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Transform.FirstOrderTransform (doLoopMapAccumL)
import Futhark.Representation.AST.Attributes.Aliases
import qualified Futhark.Analysis.Alias as Alias
import Futhark.Util

blockedReductionStream :: (MonadFreshNames m, HasScope Kernels m) =>
                          Pattern
                       -> Certificates -> SubExp
                       -> Commutativity
                       -> Lambda -> Lambda
                       -> [SubExp]
                       -> [VName]
                       -> m [Binding]
blockedReductionStream pat cs w comm reduce_lam fold_lam nes arrs = runBinder_ $ do
  step_one_size <- blockedKernelSize w

  let one = constant (1 :: Int32)
      num_chunks = kernelWorkgroups step_one_size

  let (acc_idents, arr_idents) = splitAt (length nes) $ patternIdents pat
  step_one_pat <- basicPattern' [] <$>
                  ((++) <$>
                   mapM (mkIntermediateIdent num_chunks) acc_idents <*>
                   pure arr_idents)
  let (_fold_chunk_param, _fold_acc_params, _fold_inp_params) =
        partitionChunkedFoldParameters (length nes) $ lambdaParams fold_lam

  fold_lam' <- kerneliseLambda nes fold_lam

  my_index <- newVName "my_index"
  other_offset <- newVName "other_offset"
  let my_index_param = Param my_index (Prim int32)
      other_offset_param = Param other_offset (Prim int32)
      reduce_lam' = reduce_lam { lambdaParams = my_index_param :
                                                other_offset_param :
                                                lambdaParams reduce_lam
                               }
      params_to_arrs = zip (map paramName $ drop 1 $ lambdaParams fold_lam') arrs
      consumedArray v = fromMaybe v $ lookup v params_to_arrs
      consumed_in_fold =
        HS.map consumedArray $ consumedByLambda $ Alias.analyseLambda fold_lam

  arrs_copies <- forM arrs $ \arr ->
    if arr `HS.member` consumed_in_fold then
      letExp (baseString arr <> "_copy") $ PrimOp $ Copy arr
    else return arr

  step_one <- chunkedReduceKernel cs w step_one_size comm reduce_lam' fold_lam' nes arrs_copies
  addBinding =<< renameBinding
    (Let step_one_pat () $ Op step_one)

  step_two_pat <- basicPattern' [] <$>
                  mapM (mkIntermediateIdent $ constant (1 :: Int32)) acc_idents

  let step_two_size = KernelSize one num_chunks one num_chunks one num_chunks

  step_two <- reduceKernel [] step_two_size reduce_lam' nes $ take (length nes) $ patternNames step_one_pat

  addBinding $ Let step_two_pat () $ Op step_two

  forM_ (zip (patternNames step_two_pat) (patternIdents pat)) $ \(arr, x) ->
    addBinding $ mkLet' [] [x] $ PrimOp $ Index [] arr [constant (0 :: Int32)]
  where mkIntermediateIdent chunk_size ident =
          newIdent (baseString $ identName ident) $
          arrayOfRow (identType ident) chunk_size

chunkedReduceKernel :: (MonadBinder m, Lore m ~ Kernels) =>
                       Certificates
                    -> SubExp
                    -> KernelSize
                    -> Commutativity
                    -> Lambda
                    -> Lambda
                    -> [SubExp]
                    -> [VName]
                    -> m (Kernel Kernels)
chunkedReduceKernel cs w step_one_size comm reduce_lam' fold_lam' nes arrs = do
  let ordering = case comm of Commutative -> Disorder
                              Noncommutative -> InOrder
      group_size = kernelWorkgroupSize step_one_size
      num_nonconcat = length nes

  (chunk_red_pes, chunk_map_pes, chunk_and_fold) <-
    blockedPerThread w step_one_size ordering fold_lam' num_nonconcat arrs
  space <- newKernelSpace (kernelWorkgroups step_one_size, group_size, kernelNumThreads step_one_size) []
  let red_ts = map patElemType chunk_red_pes
      map_ts = map (rowType . patElemType) chunk_map_pes
      ts = red_ts ++ map_ts

  chunk_red_pes' <- forM red_ts $ \red_t -> do
    pe_name <- newVName "chunk_fold_red"
    return $ PatElem pe_name BindVar $ red_t `arrayOfRow` group_size
  let combine_reds = [ Combine pe' [(spaceLocalId space, group_size)] $
                       Var $ patElemName pe
                     | (pe', pe) <- zip chunk_red_pes' chunk_red_pes ]

  final_red_pes <- forM (lambdaReturnType reduce_lam') $ \t -> do
    pe_name <- newVName "final_result"
    return $ PatElem pe_name BindVar t
  let reduce_chunk = GroupReduce final_red_pes group_size reduce_lam' $
                     zip nes $ map patElemName chunk_red_pes'

  red_rets <- forM final_red_pes $ \pe ->
    return $ ThreadsReturn (OneThreadPerGroup (constant (0::Int32))) $ Var $ patElemName pe
  map_rets <- forM chunk_map_pes $ \pe ->
    return $ ConcatReturns ordering w (kernelElementsPerThread step_one_size) $ patElemName pe
  let rets = red_rets ++ map_rets

  return $ Kernel cs space ts $
    KernelBody (chunk_and_fold++combine_reds++[reduce_chunk]) rets

reduceKernel :: (MonadBinder m, Lore m ~ Kernels) =>
                Certificates
             -> KernelSize
             -> Lambda
             -> [SubExp]
             -> [VName]
             -> m (Kernel Kernels)
reduceKernel cs step_two_size reduce_lam' nes arrs = do
  let group_size = kernelWorkgroupSize step_two_size
      red_ts = lambdaReturnType reduce_lam'
  space <- newKernelSpace (kernelWorkgroups step_two_size, group_size, kernelNumThreads step_two_size) []
  let thread_id = spaceGlobalId space

  (copy_input, arrs_index) <-
    fmap unzip $ forM (zip red_ts arrs) $ \(t, arr) -> do
      arr_index <- newVName (baseString arr ++ "_index")
      return (Thread AllThreads $
               Let (Pattern [] [PatElem arr_index BindVar t]) () $
               PrimOp $ Index [] arr [Var thread_id]
             , arr_index)

  (combine_arrs, arrs') <-
    fmap unzip $ forM (zip red_ts arrs_index) $ \(red_t, arr_index) -> do
      arr' <- newVName $ baseString arr_index ++ "_combined"
      let pe = PatElem arr' BindVar $ red_t `arrayOfRow` group_size
      return (Combine pe [(spaceLocalId space, group_size)] (Var arr_index),
              arr')

  final_res_pes <- forM (lambdaReturnType reduce_lam') $ \t -> do
    pe_name <- newVName "final_result"
    return $ PatElem pe_name BindVar t
  let reduce = GroupReduce final_res_pes group_size reduce_lam' $ zip nes arrs'

  rets <- forM final_res_pes $ \pe ->
    return $ ThreadsReturn (OneThreadPerGroup (constant (0::Int32))) $ Var $ patElemName pe

  return $ Kernel cs space (lambdaReturnType reduce_lam')  $
    KernelBody (copy_input++combine_arrs++[reduce]) rets

chunkLambda :: (MonadFreshNames m, HasScope Kernels m) =>
               Pattern -> [SubExp] -> Lambda -> m Lambda
chunkLambda pat nes fold_lam = do
  chunk_size <- newVName "chunk_size"

  let arr_idents = drop (length nes) $ patternIdents pat
      (fold_acc_params, fold_arr_params) =
        splitAt (length nes) $ lambdaParams fold_lam
      chunk_size_param = Param chunk_size (Prim int32)
  arr_chunk_params <- mapM (mkArrChunkParam $ Var chunk_size) fold_arr_params

  map_arr_params <- forM arr_idents $ \arr ->
    newParam (baseString (identName arr) <> "_in") $
    setOuterSize (identType arr) (Var chunk_size)

  fold_acc_params' <- forM fold_acc_params $ \p ->
    newParam (baseString $ paramName p) $ paramType p

  let param_scope =
        scopeOfLParams $ fold_acc_params' ++ arr_chunk_params ++ map_arr_params
  (seq_loop, seq_loop_prologue) <-
    runBinder $ localScope param_scope $
    doLoopMapAccumL [] (Var chunk_size) (Alias.analyseLambda fold_lam)
    (map (Var . paramName) fold_acc_params')
    (map paramName arr_chunk_params) (map paramName map_arr_params)

  dummys <- mapM (newIdent "dummy" . paramType) arr_chunk_params

  let seq_rt =
        let (acc_ts, arr_ts) =
              splitAt (length nes) $ lambdaReturnType fold_lam
        in acc_ts ++ map (`arrayOfRow` Var chunk_size) arr_ts

      res_idents = zipWith Ident (patternValueNames pat) seq_rt

      seq_loop_bnd = mkLet' [] (dummys++res_idents) seq_loop
      seq_body = mkBody (seq_loop_prologue++[seq_loop_bnd]) $ map (Var . identName) res_idents

  return Lambda { lambdaParams = chunk_size_param :
                                 fold_acc_params' ++
                                 arr_chunk_params ++
                                 map_arr_params
                , lambdaReturnType = seq_rt
                , lambdaBody = seq_body
                }
  where mkArrChunkParam chunk_size arr_param =
          newParam (baseString (paramName arr_param) <> "_chunk") $
            arrayOfRow (paramType arr_param) chunk_size

-- | Given a chunked fold lambda that takes its initial accumulator
-- value as parameters, bind those parameters to the neutral element
-- instead.  Also fix the index computation.
kerneliseLambda :: MonadFreshNames m =>
                   [SubExp] -> Lambda -> m Lambda
kerneliseLambda nes lam = do
  thread_index <- newVName "thread_index"
  let thread_index_param = Param thread_index $ Prim int32
      (fold_chunk_param, fold_acc_params, fold_inp_params) =
        partitionChunkedFoldParameters (length nes) $ lambdaParams lam

      mkAccInit p (Var v)
        | not $ primType $ paramType p =
            mkLet' [] [paramIdent p] $ PrimOp $ Copy v
      mkAccInit p x = mkLet' [] [paramIdent p] $ PrimOp $ SubExp x
      acc_init_bnds = zipWith mkAccInit fold_acc_params nes
  return lam { lambdaBody = insertBindings acc_init_bnds $
                            lambdaBody lam
             , lambdaParams = thread_index_param :
                              fold_chunk_param :
                              fold_inp_params
             }

blockedReduction :: (MonadFreshNames m, HasScope Kernels m) =>
                    Pattern
                 -> Certificates -> SubExp
                 -> Commutativity
                 -> Lambda -> Lambda
                 -> [SubExp]
                 -> [VName]
                 -> m [Binding]
blockedReduction pat cs w comm reduce_lam fold_lam nes arrs = runBinder_ $ do
  fold_lam' <- chunkLambda pat nes fold_lam

  let arr_idents = drop (length nes) $ patternIdents pat
  map_out_arrs <- forM arr_idents $ \(Ident name t) ->
    letExp (baseString name <> "_out_in") $
    PrimOp $ Scratch (elemType t) (arrayDims t)

  mapM_ addBinding =<<
    blockedReductionStream pat cs w comm reduce_lam fold_lam' nes
    (arrs ++ map_out_arrs)

blockedMap :: (MonadFreshNames m, HasScope Kernels m) =>
              Pattern -> Certificates -> SubExp
           -> StreamOrd -> Lambda -> [SubExp] -> [VName]
           -> m (Binding, [Binding])
blockedMap concat_pat cs w ordering lam nes arrs = runBinder $ do
  kernel_size <- blockedKernelSize w
  let num_nonconcat = length (lambdaReturnType lam) - patternSize concat_pat
      num_groups = kernelWorkgroups kernel_size
      group_size = kernelWorkgroupSize kernel_size
      num_threads = kernelNumThreads kernel_size

  lam' <- kerneliseLambda nes lam
  (chunk_red_pes, chunk_map_pes, chunk_and_fold) <-
    blockedPerThread w kernel_size ordering lam' num_nonconcat arrs

  nonconcat_pat <-
    fmap (Pattern []) $ forM (take num_nonconcat $ lambdaReturnType lam) $ \t -> do
      name <- newVName "nonconcat"
      return $ PatElem name BindVar $ t `arrayOfRow` num_threads

  let pat = nonconcat_pat <> concat_pat
      ts = map patElemType chunk_red_pes ++
           map (rowType . patElemType) chunk_map_pes

  nonconcat_rets <- forM chunk_red_pes $ \pe ->
    return $ ThreadsReturn AllThreads $ Var $ patElemName pe
  concat_rets <- forM chunk_map_pes $ \pe ->
    return $ ConcatReturns ordering w (kernelElementsPerThread kernel_size) $ patElemName pe

  space <- newKernelSpace (num_groups, group_size, num_threads) []
  return $ Let pat () $ Op $ Kernel cs space ts $
    KernelBody chunk_and_fold $ nonconcat_rets ++ concat_rets

blockedPerThread :: MonadFreshNames m =>
                    SubExp -> KernelSize -> StreamOrd -> Lambda
                 -> Int -> [VName]
                 -> m ([PatElem], [PatElem], [KernelStm Kernels])
blockedPerThread w kernel_size ordering lam num_nonconcat arrs = do
  let (_, chunk_size, [], arr_params) =
        partitionChunkedKernelFoldParameters 0 $ lambdaParams lam

  split_bound <- forM arr_params $ \arr_param -> do
    let chunk_t = paramType arr_param `setOuterSize` Var (paramName chunk_size)
    return $ PatElem (paramName arr_param) BindVar chunk_t
  let chunk_stm = SplitArray (paramName chunk_size, split_bound)
                  ordering w (kernelElementsPerThread kernel_size) arrs
      red_ts = take num_nonconcat $ lambdaReturnType lam
      map_ts = map rowType $ drop num_nonconcat $ lambdaReturnType lam

  chunk_red_pes <- forM red_ts $ \red_t -> do
    pe_name <- newVName "chunk_fold_red"
    return $ PatElem pe_name BindVar red_t
  chunk_map_pes <- forM map_ts $ \map_t -> do
    pe_name <- newVName "chunk_fold_map"
    return $ PatElem pe_name BindVar $ map_t `arrayOfRow` Var (paramName chunk_size)
  let (chunk_red_ses, chunk_map_ses) =
        splitAt num_nonconcat $ bodyResult $ lambdaBody lam
      fold_chunk = map (Thread AllThreads) (bodyBindings (lambdaBody lam)) ++
                   [ Thread AllThreads $ Let (Pattern [] [pe]) () $ PrimOp $ SubExp se
                   | (pe,se) <- zip chunk_red_pes chunk_red_ses ] ++
                   [ Thread AllThreads $ Let (Pattern [] [pe]) () $ PrimOp $ SubExp se
                   | (pe,se) <- zip chunk_map_pes chunk_map_ses ]

  return (chunk_red_pes, chunk_map_pes, chunk_stm : fold_chunk)

blockedKernelSize :: (MonadBinder m, Lore m ~ Kernels) =>
                     SubExp -> m KernelSize
blockedKernelSize w = do
  num_groups <- letSubExp "num_groups" $ Op NumGroups
  group_size <- letSubExp "group_size" $ Op GroupSize

  num_threads <-
    letSubExp "num_threads" $ PrimOp $ BinOp (Mul Int32) num_groups group_size

  per_thread_elements <-
    letSubExp "per_thread_elements" =<<
    eDivRoundingUp Int32 (eSubExp w) (eSubExp num_threads)

  return $ KernelSize num_groups group_size per_thread_elements w per_thread_elements num_threads

blockedScan :: (MonadBinder m, Lore m ~ Kernels) =>
               Pattern
            -> Certificates -> SubExp
            -> Lambda -> Lambda
            -> [SubExp] -> [VName]
            -> m ()
blockedScan pat cs w lam foldlam nes arrs = do
  first_scan_size <- blockedKernelSize w
  my_index <- newVName "my_index"
  other_index <- newVName "other_index"
  let num_groups = kernelWorkgroups first_scan_size
      group_size = kernelWorkgroupSize first_scan_size
      num_threads = kernelNumThreads first_scan_size
      my_index_param = Param my_index (Prim int32)
      other_index_param = Param other_index (Prim int32)

  first_scan_foldlam <- renameLambda
    foldlam { lambdaParams = my_index_param :
                             other_index_param :
                             lambdaParams foldlam
            }
  first_scan_lam <- renameLambda
    lam { lambdaParams = my_index_param :
                         other_index_param :
                         lambdaParams lam
        }

  let (scan_idents, arr_idents) = splitAt (length nes) $ patternIdents pat
      final_res_pat = Pattern [] $ take (length nes) $ patternValueElements pat
  first_scan_pat <- basicPattern' [] <$>
    (((.).(.)) (++) (++) <$> -- Dammit Haskell
     mapM (mkIntermediateIdent "seq_scanned" [w]) scan_idents <*>
     mapM (mkIntermediateIdent "group_sums" [num_groups]) scan_idents <*>
     pure arr_idents)

  addBinding $ Let first_scan_pat () $
    Op $ ScanKernel cs w first_scan_size
    first_scan_lam first_scan_foldlam nes arrs

  let (sequentially_scanned, group_carry_out, _) =
        splitAt3 (length nes) (length nes) $ patternNames first_scan_pat

  let second_scan_size = KernelSize one num_groups one num_groups one num_groups
  second_scan_lam <- renameLambda first_scan_lam
  second_scan_lam_renamed <- renameLambda first_scan_lam

  group_carry_out_scanned <-
    letTupExp "group_carry_out_scanned" $
    Op $ ScanKernel cs num_groups second_scan_size
    second_scan_lam second_scan_lam_renamed
    nes group_carry_out

  lam''' <- renameLambda lam
  j <- newVName "j"
  let (acc_params, arr_params) =
        splitAt (length nes) $ lambdaParams lam'''
      result_map_input =
        zipWith (mkKernelInput [Var j]) arr_params sequentially_scanned

  chunks_per_group <- letSubExp "chunks_per_group" =<<
    eDivRoundingUp Int32 (eSubExp w) (eSubExp num_threads)
  elems_per_group <- letSubExp "elements_per_group" $
    PrimOp $ BinOp (Mul Int32) chunks_per_group group_size

  result_map_body <- runBodyBinder $ localScope (scopeOfLParams $ map kernelInputParam result_map_input) $ do
    group_id <-
      letSubExp "group_id" $
      PrimOp $ BinOp (SQuot Int32) (Var j) elems_per_group
    let do_nothing =
          pure $ resultBody $ map (Var . paramName) arr_params
        add_carry_in = runBodyBinder $ do
          forM_ (zip acc_params group_carry_out_scanned) $ \(p, arr) -> do
            carry_in_index <-
              letSubExp "carry_in_index" $
              PrimOp $ BinOp (Sub Int32) group_id one
            letBindNames'_ [paramName p] $
              PrimOp $ Index [] arr [carry_in_index]
          return $ lambdaBody lam'''
    group_lasts <-
      letTupExp "final_result" =<<
        eIf (eCmpOp (CmpEq int32) (eSubExp zero) (eSubExp group_id))
        do_nothing
        add_carry_in
    return $ resultBody $ map Var group_lasts
  (mapk_bnds, mapk) <- mapKernelFromBody [] w [(j, w)] result_map_input
                       (lambdaReturnType lam) result_map_body
  mapM_ addBinding mapk_bnds
  letBind_ final_res_pat $ Op mapk
  where one = constant (1 :: Int32)
        zero = constant (0 :: Int32)

        mkIntermediateIdent desc shape ident =
          newIdent (baseString (identName ident) ++ "_" ++ desc) $
          arrayOf (rowType $ identType ident) (Shape shape) NoUniqueness

        mkKernelInput indices p arr = KernelInput { kernelInputName = paramName p
                                                  , kernelInputType = paramType p
                                                  , kernelInputArray = arr
                                                  , kernelInputIndices = indices
                                                  }

blockedSegmentedScan :: (MonadBinder m, Lore m ~ Kernels) =>
                        SubExp
                     -> Pattern
                     -> Certificates
                     -> SubExp
                     -> Lambda
                     -> [(SubExp, VName)]
                     -> m ()
blockedSegmentedScan segment_size pat cs w lam input = do
  x_flag <- newVName "x_flag"
  y_flag <- newVName "y_flag"
  let x_flag_param = Param x_flag $ Prim Bool
      y_flag_param = Param y_flag $ Prim Bool
      (x_params, y_params) = splitAt (length input) $ lambdaParams lam
      params = [x_flag_param] ++ x_params ++ [y_flag_param] ++ y_params

  body <- runBodyBinder $ localScope (scopeOfLParams params) $ do
    new_flag <- letSubExp "new_flag" $
                PrimOp $ BinOp LogOr (Var x_flag) (Var y_flag)
    seg_res <- letTupExp "seg_res" $ If (Var y_flag)
      (resultBody $ map (Var . paramName) y_params)
      (lambdaBody lam)
      (staticShapes $ lambdaReturnType lam)
    return $ resultBody $ new_flag : map Var seg_res

  flags_i <- newVName "flags_i"
  flags_body <-
    runBodyBinder $ localScope (HM.singleton flags_i IndexInfo) $ do
      segment_index <- letSubExp "segment_index" $
                       PrimOp $ BinOp (SRem Int32) (Var flags_i) segment_size
      start_of_segment <- letSubExp "start_of_segment" $
                          PrimOp $ CmpOp (CmpEq int32) segment_index zero
      flag <- letSubExp "flag" $
              If start_of_segment (resultBody [true]) (resultBody [false]) [Prim Bool]
      return $ resultBody [flag]
  (mapk_bnds, mapk) <- mapKernelFromBody [] w [(flags_i, w)] [] [Prim Bool] flags_body
  mapM_ addBinding mapk_bnds
  flags <-
    letExp "flags" $ Op mapk

  unused_flag_array <- newVName "unused_flag_array"
  let lam' = Lambda { lambdaParams = params
                    , lambdaBody = body
                    , lambdaReturnType = Prim Bool : lambdaReturnType lam
                    }
      pat' = pat { patternValueElements = PatElem unused_flag_array BindVar
                                          (arrayOf (Prim Bool) (Shape [w]) NoUniqueness) :
                                          patternValueElements pat
                 }
      (nes, arrs) = unzip input
  lam_renamed <- renameLambda lam'
  blockedScan pat' cs w lam' lam_renamed (false:nes) (flags:arrs)
  where zero = constant (0 :: Int32)
        true = constant True
        false = constant False

mapKernelSkeleton :: (HasScope Kernels m, MonadFreshNames m) =>
                     SubExp -> [KernelInput]
                  -> m ([Binding], (SubExp,SubExp,SubExp), [Binding])
mapKernelSkeleton w inputs = do
  group_size_v <- newVName "group_size"

  ((num_threads, num_groups), ksize_bnds) <- runBinder $ do
    letBindNames'_ [group_size_v] $ Op GroupSize
    numThreadsAndGroups w $ Var group_size_v

  read_input_bnds <- forM inputs $ \inp -> do
    let pe = PatElem (kernelInputName inp) BindVar $ kernelInputType inp
    return $ Let (Pattern [] [pe]) () $
      PrimOp $ Index [] (kernelInputArray inp) (kernelInputIndices inp)

  let ksize = (num_groups, Var group_size_v, num_threads)
  return (ksize_bnds, ksize, read_input_bnds)

-- Given the desired minium number of threads and the number of
-- threads per group, compute the number of groups and total number of
-- threads.
numThreadsAndGroups :: MonadBinder m => SubExp -> SubExp -> m (SubExp, SubExp)
numThreadsAndGroups w group_size = do
  num_groups <- letSubExp "num_groups" =<< eDivRoundingUp Int32
    (eSubExp w) (eSubExp group_size)
  num_threads <- letSubExp "num_threads" $
    PrimOp $ BinOp (Mul Int32) num_groups group_size
  return (num_threads, num_groups)

mapKernel :: (HasScope Kernels m, MonadFreshNames m) =>
             Certificates -> SubExp -> [(VName, SubExp)] -> [KernelInput]
          -> [Type] -> KernelBody Kernels
          -> m ([Binding], Kernel Kernels)
mapKernel cs w ispace inputs rts (KernelBody kstms krets) = do
  (ksize_bnds, ksize, read_input_bnds) <- mapKernelSkeleton w inputs

  space <- newKernelSpace ksize ispace

  let kstms' = map (Thread ThreadsInSpace) read_input_bnds ++ kstms
      kbody' = KernelBody kstms' krets
  return (ksize_bnds, Kernel cs space rts kbody')

mapKernelFromBody :: (HasScope Kernels m, MonadFreshNames m) =>
             Certificates -> SubExp -> [(VName, SubExp)] -> [KernelInput]
          -> [Type] -> Body
          -> m ([Binding], Kernel Kernels)
mapKernelFromBody cs w ispace inputs rts body =
  mapKernel cs w ispace inputs rts kbody
  where kbody = KernelBody kstms krets
        kstms = map (Thread ThreadsInSpace) $ bodyBindings body
        krets = map (ThreadsReturn ThreadsInSpace) $ bodyResult body

data KernelInput = KernelInput { kernelInputName :: VName
                               , kernelInputType :: Type
                               , kernelInputArray :: VName
                               , kernelInputIndices :: [SubExp]
                               }

kernelInputParam :: KernelInput -> Param Type
kernelInputParam p = Param (kernelInputName p) (kernelInputType p)

newKernelSpace :: MonadFreshNames m =>
                  (SubExp,SubExp,SubExp) -> [(VName, SubExp)] -> m KernelSpace
newKernelSpace (num_groups, group_size, num_threads) dims =
  KernelSpace
  <$> newVName "global_tid"
  <*> newVName "local_tid"
  <*> newVName "group_id"
  <*> pure num_threads
  <*> pure num_groups
  <*> pure group_size
  <*> pure (FlatSpace dims)
