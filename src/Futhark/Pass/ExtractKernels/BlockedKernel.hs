{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.Pass.ExtractKernels.BlockedKernel
       ( blockedReduction
       , blockedReductionStream
       , blockedMap
       , blockedScan

       , mapKernel
       , mapKernelFromBody
       , KernelInput(..)
       , readKernelInput

       -- Helper functions shared with at least Segmented.hs
       , kerneliseLambda
       , newKernelSpace
       , chunkLambda
       , splitArrays
       , getSize
       , cmpSizeLe
       )
       where

import Control.Monad
import Data.Maybe
import Data.Semigroup ((<>))
import qualified Data.Set as S

import Prelude hiding (quot)

import Futhark.Analysis.PrimExp
import Futhark.Representation.AST
import Futhark.Representation.Kernels
       hiding (Prog, Body, Stm, Pattern, PatElem,
               BasicOp, Exp, Lambda, FunDef, FParam, LParam, RetType)
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Transform.Rename
import qualified Futhark.Pass.ExtractKernels.Kernelise as Kernelise
import Futhark.Representation.AST.Attributes.Aliases
import qualified Futhark.Analysis.Alias as Alias
import Futhark.Representation.SOACS.SOAC (composeLambda, Scan, Reduce, nilFn)
import Futhark.Util
import Futhark.Util.IntegralExp

getSize :: (MonadBinder m, Op (Lore m) ~ Kernel innerlore) =>
           String -> SizeClass -> m SubExp
getSize desc size_class = do
  size_key <- newVName desc
  letSubExp desc $ Op $ GetSize size_key size_class

cmpSizeLe :: (MonadBinder m, Op (Lore m) ~ Kernel innerlore) =>
           String -> SizeClass -> SubExp -> m (SubExp, VName)
cmpSizeLe desc size_class to_what = do
  size_key <- newVName desc
  cmp_res <- letSubExp desc $ Op $ CmpSizeLe size_key size_class to_what
  return (cmp_res, size_key)

blockedReductionStream :: (MonadFreshNames m, HasScope Kernels m) =>
                          Pattern Kernels
                       -> SubExp
                       -> Commutativity
                       -> Lambda InKernel -> Lambda InKernel
                       -> [(VName, SubExp)] -> [SubExp] -> [VName]
                       -> m (Stms Kernels)
blockedReductionStream pat w comm reduce_lam fold_lam ispace nes arrs = runBinder_ $ do
  (max_step_one_num_groups, step_one_size) <- blockedKernelSize w

  let one = constant (1 :: Int32)
      num_chunks = kernelWorkgroups step_one_size

  let (acc_idents, arr_idents) = splitAt (length nes) $ patternIdents pat
  step_one_pat <- basicPattern [] <$>
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
        S.map consumedArray $ consumedByLambda $ Alias.analyseLambda fold_lam

  arrs_copies <- forM arrs $ \arr ->
    if arr `S.member` consumed_in_fold then
      letExp (baseString arr <> "_copy") $ BasicOp $ Copy arr
    else return arr

  step_one <- chunkedReduceKernel w step_one_size comm reduce_lam' fold_lam'
              ispace nes arrs_copies
  addStm =<< renameStm (Let step_one_pat (defAux ()) $ Op step_one)

  step_two_pat <- basicPattern [] <$>
                  mapM (mkIntermediateIdent $ constant (1 :: Int32)) acc_idents

  let step_two_size = KernelSize one max_step_one_num_groups one num_chunks max_step_one_num_groups

  step_two <- reduceKernel step_two_size reduce_lam' nes $ take (length nes) $ patternNames step_one_pat

  addStm $ Let step_two_pat (defAux ()) $ Op step_two

  forM_ (zip (patternIdents step_two_pat) (patternIdents pat)) $ \(arr, x) ->
    addStm $ mkLet [] [x] $ BasicOp $ Index (identName arr) $
    fullSlice (identType arr) [DimFix $ constant (0 :: Int32)]
  where mkIntermediateIdent chunk_size ident =
          newIdent (baseString $ identName ident) $
          arrayOfRow (identType ident) chunk_size

chunkedReduceKernel :: (MonadBinder m, Lore m ~ Kernels) =>
                       SubExp
                    -> KernelSize
                    -> Commutativity
                    -> Lambda InKernel -> Lambda InKernel
                    -> [(VName, SubExp)] -> [SubExp] -> [VName]
                    -> m (Kernel InKernel)
chunkedReduceKernel w step_one_size comm reduce_lam' fold_lam' ispace nes arrs = do
  let ordering = case comm of Commutative -> Disorder
                              Noncommutative -> InOrder
      group_size = kernelWorkgroupSize step_one_size
      num_nonconcat = length nes

  space <- newKernelSpace (kernelWorkgroups step_one_size, group_size, kernelNumThreads step_one_size) $ FlatThreadSpace ispace
  ((chunk_red_pes, chunk_map_pes), chunk_and_fold) <-
    runBinder $ blockedPerThread (spaceGlobalId space)
    w step_one_size ordering fold_lam' num_nonconcat arrs
  let red_ts = map patElemType chunk_red_pes
      map_ts = map (rowType . patElemType) chunk_map_pes
      ts = red_ts ++ map_ts
      ordering' =
        case ordering of InOrder -> SplitContiguous
                         Disorder -> SplitStrided $ kernelNumThreads step_one_size

  chunk_red_pes' <- forM red_ts $ \red_t -> do
    pe_name <- newVName "chunk_fold_red"
    return $ PatElem pe_name $ red_t `arrayOfRow` group_size
  let combine_reds = [ Let (Pattern [] [pe']) (defAux ()) $ Op $
                       Combine [(spaceLocalId space, group_size)] [patElemType pe] [] $
                       Body () mempty [Var $ patElemName pe]
                     | (pe', pe) <- zip chunk_red_pes' chunk_red_pes ]

  final_red_pes <- forM (lambdaReturnType reduce_lam') $ \t -> do
    pe_name <- newVName "final_result"
    return $ PatElem pe_name t
  let reduce_chunk = Let (Pattern [] final_red_pes) (defAux ()) $ Op $
                     GroupReduce group_size reduce_lam' $
                     zip nes $ map patElemName chunk_red_pes'

  red_rets <- forM final_red_pes $ \pe ->
    return $ ThreadsReturn OneResultPerGroup $ Var $ patElemName pe
  map_rets <- forM chunk_map_pes $ \pe ->
    return $ ConcatReturns ordering' w (kernelElementsPerThread step_one_size) Nothing $ patElemName pe
  let rets = red_rets ++ map_rets

  return $ Kernel (KernelDebugHints "chunked_reduce" [("input size", w)]) space ts $
    KernelBody () (chunk_and_fold<>stmsFromList combine_reds<>oneStm reduce_chunk) rets

reduceKernel :: (MonadBinder m, Lore m ~ Kernels) =>
                KernelSize
             -> Lambda InKernel
             -> [SubExp]
             -> [VName]
             -> m (Kernel InKernel)
reduceKernel step_two_size reduce_lam' nes arrs = do
  let group_size = kernelWorkgroupSize step_two_size
      red_ts = lambdaReturnType reduce_lam'
  space <- newKernelSpace (kernelWorkgroups step_two_size, group_size, kernelNumThreads step_two_size) $
           FlatThreadSpace []
  let thread_id = spaceGlobalId space

  (rets, kstms) <- runBinder $ localScope (scopeOfKernelSpace space) $ do
    in_bounds <- letSubExp "in_bounds" $ BasicOp $ CmpOp (CmpSlt Int32)
                 (Var $ spaceLocalId space)
                 (kernelTotalElements step_two_size)

    combine_body <- runBodyBinder $
      fmap resultBody $ forM (zip arrs nes) $ \(arr, ne) -> do
        arr_t <- lookupType arr
        letSubExp "elem" =<<
          eIf (eSubExp in_bounds)
          (eBody [pure $ BasicOp $ Index arr $
                  fullSlice arr_t [DimFix (Var thread_id)]])
          (resultBodyM [ne])

    combine_pat <- fmap (Pattern []) $ forM (zip arrs red_ts) $ \(arr, red_t) -> do
      arr' <- newVName $ baseString arr ++ "_combined"
      return $ PatElem arr' $ red_t `arrayOfRow` group_size

    letBind_ combine_pat $
      Op $ Combine [(spaceLocalId space, group_size)]
      (map rowType $ patternTypes combine_pat) [] combine_body

    let arrs' = patternNames combine_pat

    final_res_pes <- forM (lambdaReturnType reduce_lam') $ \t -> do
      pe_name <- newVName "final_result"
      return $ PatElem pe_name t
    letBind_ (Pattern [] final_res_pes) $
      Op $ GroupReduce group_size reduce_lam' $ zip nes arrs'

    forM final_res_pes $ \pe ->
      return $ ThreadsReturn OneResultPerGroup $ Var $ patElemName pe

  return $ Kernel (KernelDebugHints "reduce" []) space (lambdaReturnType reduce_lam')  $
    KernelBody () kstms rets

-- | Requires a fold lambda that includes accumulator parameters.
chunkLambda :: (MonadFreshNames m, HasScope Kernels m) =>
               Pattern Kernels -> [SubExp] -> Lambda InKernel -> m (Lambda InKernel)
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

  let seq_rt =
        let (acc_ts, arr_ts) =
              splitAt (length nes) $ lambdaReturnType fold_lam
        in acc_ts ++ map (`arrayOfRow` Var chunk_size) arr_ts

      res_idents = zipWith Ident (patternValueNames pat) seq_rt

      param_scope =
        scopeOfLParams $ fold_acc_params' ++ arr_chunk_params ++ map_arr_params

  seq_loop_stms <-
    runBinder_ $ localScope param_scope $
    Kernelise.groupStreamMapAccumL
    (patternElements (basicPattern [] res_idents))
    (Var chunk_size) fold_lam (map (Var . paramName) fold_acc_params')
    (map paramName arr_chunk_params)

  let seq_body = mkBody seq_loop_stms $ map (Var . identName) res_idents

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
-- instead.
kerneliseLambda :: MonadFreshNames m =>
                   [SubExp] -> Lambda InKernel -> m (Lambda InKernel)
kerneliseLambda nes lam = do
  thread_index <- newVName "thread_index"
  let thread_index_param = Param thread_index $ Prim int32
      (fold_chunk_param, fold_acc_params, fold_inp_params) =
        partitionChunkedFoldParameters (length nes) $ lambdaParams lam

      mkAccInit p (Var v)
        | not $ primType $ paramType p =
            mkLet [] [paramIdent p] $ BasicOp $ Copy v
      mkAccInit p x = mkLet [] [paramIdent p] $ BasicOp $ SubExp x
      acc_init_bnds = stmsFromList $ zipWith mkAccInit fold_acc_params nes
  return lam { lambdaBody = insertStms acc_init_bnds $
                            lambdaBody lam
             , lambdaParams = thread_index_param :
                              fold_chunk_param :
                              fold_inp_params
             }

blockedReduction :: (MonadFreshNames m, HasScope Kernels m) =>
                    Pattern Kernels
                 -> SubExp
                 -> Commutativity
                 -> Lambda InKernel -> Lambda InKernel
                 -> [(VName, SubExp)] -> [SubExp] -> [VName]
                 -> m (Stms Kernels)
blockedReduction pat w comm reduce_lam map_lam ispace nes arrs = runBinder_ $ do
  fold_lam <- composeLambda nilFn reduce_lam map_lam
  fold_lam' <- chunkLambda pat nes fold_lam

  let arr_idents = drop (length nes) $ patternIdents pat
  map_out_arrs <- forM arr_idents $ \(Ident name t) ->
    letExp (baseString name <> "_out_in") $
    BasicOp $ Scratch (elemType t) (arrayDims t)

  addStms =<<
    blockedReductionStream pat w comm reduce_lam fold_lam'
    ispace nes (arrs ++ map_out_arrs)

blockedMap :: (MonadFreshNames m, HasScope Kernels m) =>
              Pattern Kernels -> SubExp
           -> StreamOrd -> Lambda InKernel -> [SubExp] -> [VName]
           -> m (Stm Kernels, Stms Kernels)
blockedMap concat_pat w ordering lam nes arrs = runBinder $ do
  (_, kernel_size) <- blockedKernelSize w
  let num_nonconcat = length (lambdaReturnType lam) - patternSize concat_pat
      num_groups = kernelWorkgroups kernel_size
      group_size = kernelWorkgroupSize kernel_size
      num_threads = kernelNumThreads kernel_size
      ordering' =
        case ordering of InOrder -> SplitContiguous
                         Disorder -> SplitStrided $ kernelNumThreads kernel_size

  space <- newKernelSpace (num_groups, group_size, num_threads) (FlatThreadSpace [])
  lam' <- kerneliseLambda nes lam
  ((chunk_red_pes, chunk_map_pes), chunk_and_fold) <- runBinder $
    blockedPerThread (spaceGlobalId space) w kernel_size ordering lam' num_nonconcat arrs

  nonconcat_pat <-
    fmap (Pattern []) $ forM (take num_nonconcat $ lambdaReturnType lam) $ \t -> do
      name <- newVName "nonconcat"
      return $ PatElem name $ t `arrayOfRow` num_threads

  let pat = nonconcat_pat <> concat_pat
      ts = map patElemType chunk_red_pes ++
           map (rowType . patElemType) chunk_map_pes

  nonconcat_rets <- forM chunk_red_pes $ \pe ->
    return $ ThreadsReturn AllThreads $ Var $ patElemName pe
  concat_rets <- forM chunk_map_pes $ \pe ->
    return $ ConcatReturns ordering' w (kernelElementsPerThread kernel_size) Nothing $ patElemName pe

  return $ Let pat (defAux ()) $ Op $ Kernel (KernelDebugHints "chunked_map" []) space ts $
    KernelBody () chunk_and_fold $ nonconcat_rets ++ concat_rets

blockedPerThread :: (MonadBinder m, Lore m ~ InKernel) =>
                    VName -> SubExp -> KernelSize -> StreamOrd -> Lambda InKernel
                 -> Int -> [VName]
                 -> m ([PatElem InKernel], [PatElem InKernel])
blockedPerThread thread_gtid w kernel_size ordering lam num_nonconcat arrs = do
  let (_, chunk_size, [], arr_params) =
        partitionChunkedKernelFoldParameters 0 $ lambdaParams lam

      ordering' =
        case ordering of InOrder -> SplitContiguous
                         Disorder -> SplitStrided $ kernelNumThreads kernel_size
      red_ts = take num_nonconcat $ lambdaReturnType lam
      map_ts = map rowType $ drop num_nonconcat $ lambdaReturnType lam

  splitArrays (paramName chunk_size) (map paramName arr_params) ordering' w
    (Var thread_gtid) (kernelElementsPerThread kernel_size) arrs

  chunk_red_pes <- forM red_ts $ \red_t -> do
    pe_name <- newVName "chunk_fold_red"
    return $ PatElem pe_name red_t
  chunk_map_pes <- forM map_ts $ \map_t -> do
    pe_name <- newVName "chunk_fold_map"
    return $ PatElem pe_name $ map_t `arrayOfRow` Var (paramName chunk_size)

  let (chunk_red_ses, chunk_map_ses) =
        splitAt num_nonconcat $ bodyResult $ lambdaBody lam

  addStms $
    bodyStms (lambdaBody lam) <>
    stmsFromList
    [ Let (Pattern [] [pe]) (defAux ()) $ BasicOp $ SubExp se
    | (pe,se) <- zip chunk_red_pes chunk_red_ses ] <>
    stmsFromList
    [ Let (Pattern [] [pe]) (defAux ()) $ BasicOp $ SubExp se
    | (pe,se) <- zip chunk_map_pes chunk_map_ses ]

  return (chunk_red_pes, chunk_map_pes)

splitArrays :: (MonadBinder m, Lore m ~ InKernel) =>
               VName -> [VName]
            -> SplitOrdering -> SubExp -> SubExp -> SubExp -> [VName]
            -> m ()
splitArrays chunk_size split_bound ordering w i elems_per_i arrs = do
  letBindNames_ [chunk_size] $ Op $ SplitSpace ordering w i elems_per_i
  case ordering of
    SplitContiguous     -> do
      offset <- letSubExp "slice_offset" $ BasicOp $ BinOp (Mul Int32) i elems_per_i
      zipWithM_ (contiguousSlice offset) split_bound arrs
    SplitStrided stride -> zipWithM_ (stridedSlice stride) split_bound arrs
  where contiguousSlice offset slice_name arr = do
          arr_t <- lookupType arr
          let slice = fullSlice arr_t [DimSlice offset (Var chunk_size) (constant (1::Int32))]
          letBindNames_ [slice_name] $ BasicOp $ Index arr slice

        stridedSlice stride slice_name arr = do
          arr_t <- lookupType arr
          let slice = fullSlice arr_t [DimSlice i (Var chunk_size) stride]
          letBindNames_ [slice_name] $ BasicOp $ Index arr slice

data KernelSize = KernelSize { kernelWorkgroups :: SubExp
                             , kernelWorkgroupSize :: SubExp
                             , kernelElementsPerThread :: SubExp
                             , kernelTotalElements :: SubExp
                             , kernelNumThreads :: SubExp
                             }
                deriving (Eq, Ord, Show)

numberOfGroups :: MonadBinder m => SubExp -> SubExp -> SubExp -> m (SubExp, SubExp)
numberOfGroups w group_size max_num_groups = do
  -- If 'w' is small, we launch fewer groups than we normally would.
  -- We don't want any idle groups.
  w_div_group_size <- letSubExp "w_div_group_size" =<<
    eDivRoundingUp Int32 (eSubExp w) (eSubExp group_size)
  -- We also don't want zero groups.
  num_groups_maybe_zero <- letSubExp "num_groups_maybe_zero" $ BasicOp $ BinOp (SMin Int32)
                           w_div_group_size max_num_groups
  num_groups <- letSubExp "num_groups" $
                BasicOp $ BinOp (SMax Int32) (constant (1::Int32))
                num_groups_maybe_zero
  num_threads <-
    letSubExp "num_threads" $ BasicOp $ BinOp (Mul Int32) num_groups group_size
  return (num_groups, num_threads)

blockedKernelSize :: (MonadBinder m, Lore m ~ Kernels) =>
                     SubExp -> m (SubExp, KernelSize)
blockedKernelSize w = do
  group_size <- getSize "group_size" SizeGroup
  max_num_groups <- getSize "max_num_groups" SizeNumGroups

  (num_groups, num_threads) <- numberOfGroups w group_size max_num_groups

  per_thread_elements <-
    letSubExp "per_thread_elements" =<<
    eDivRoundingUp Int32 (eSubExp w) (eSubExp num_threads)

  return (max_num_groups,
          KernelSize num_groups group_size per_thread_elements w num_threads)

-- First stage scan kernel.
scanKernel1 :: (MonadBinder m, Lore m ~ Kernels) =>
               SubExp -> KernelSize
            -> Scan InKernel
            -> Reduce InKernel
            -> Lambda InKernel -> [VName]
            -> m (Kernel InKernel)
scanKernel1 w scan_sizes (scan_lam, scan_nes) (_comm, red_lam, red_nes) foldlam arrs = do
  let (scan_ts, red_ts, map_ts) =
        splitAt3 (length scan_nes) (length red_nes) $ lambdaReturnType foldlam
      (_, foldlam_acc_params, _) =
        partitionChunkedFoldParameters (length scan_nes + length red_nes) $ lambdaParams foldlam

  -- Scratch arrays for scanout and mapout parts.
  (scanout_arrs, scanout_arr_params, scanout_arr_ts) <-
    unzip3 <$> mapM (mkOutArray "scanout") scan_ts
  (mapout_arrs, mapout_arr_params, mapout_arr_ts) <-
    unzip3 <$> mapM (mkOutArray "scanout") map_ts

  last_thread <- letSubExp "last_thread" $ BasicOp $
                 BinOp (Sub Int32) group_size (constant (1::Int32))
  kspace <- newKernelSpace (num_groups, group_size, num_threads) $ FlatThreadSpace []
  let lid = spaceLocalId kspace

  (res, stms) <- runBinder $ localScope (scopeOfKernelSpace kspace) $ do
    -- We create a loop that moves in group_size chunks over the input.
    num_iterations <- letSubExp "num_iterations" =<<
                      eDivRoundingUp Int32 (eSubExp w) (eSubExp num_threads)

    -- The merge parameters are the scanout arrays, the reduction
    -- results, the mapout arrays, and the (renamed) scan accumulator
    -- parameters of foldlam (which function as carries).  We do not
    -- need to keep accumulator parameters/carries for the reduction,
    -- because the reduction result suffices.
    (acc_params, nes') <- unzip <$> zipWithM mkAccMergeParam foldlam_acc_params
                          (scan_nes ++ red_nes)
    let (scan_acc_params, red_acc_params) =
          splitAt (length scan_nes) acc_params
        (scan_nes', red_nes') =
          splitAt (length scan_nes) nes'
    let merge = zip scanout_arr_params (map Var scanout_arrs) ++
                zip red_acc_params red_nes' ++
                zip mapout_arr_params (map Var mapout_arrs) ++
                zip scan_acc_params scan_nes'
    i <- newVName "i"
    let form = ForLoop i Int32 num_iterations []

    loop_body <- runBodyBinder $ localScope (scopeOfFParams (map fst merge) <>
                                             scopeOf form) $ do
      -- Compute the offset into the input and output.  To this a
      -- thread can add its local ID to figure out which element it is
      -- responsible for.
      offset <- letSubExp "offset" =<<
                eBinOp (Add Int32)
                (eBinOp (Mul Int32)
                 (eSubExp $ Var $ spaceGroupId kspace)
                 (pure $ BasicOp $ BinOp (Mul Int32) num_iterations group_size))
                (pure $ BasicOp $ BinOp (Mul Int32) (Var i) group_size)

      -- Now we apply the fold function if j=offset+lid is less than
      -- num_elements.  This also involves writing to the mapout
      -- arrays.
      j <- letSubExp "j" $ BasicOp $ BinOp (Add Int32) offset (Var lid)
      let in_bounds = pure $ BasicOp $ CmpOp (CmpSlt Int32) j num_elements

          in_bounds_fold_branch = do
            -- Read array input.
            arr_elems <- forM arrs $ \arr -> do
              arr_t <- lookupType arr
              let slice = fullSlice arr_t [DimFix j]
              letSubExp (baseString arr ++ "_elem") $ BasicOp $ Index arr slice

            -- Apply the body of the fold function.
            fold_res <-
              eLambda foldlam $ map eSubExp $ j : map (Var . paramName) acc_params ++ arr_elems

            -- Scatter the to_map parts to the mapout arrays using
            -- in-place updates, and return the to_scan parts.
            let (to_scan, to_red, to_map) = splitAt3 (length scan_nes) (length red_nes) fold_res
            mapout_arrs' <- forM (zip to_map mapout_arr_params) $ \(se,arr) -> do
              let slice = fullSlice (paramType arr) [DimFix j]
              letInPlace "mapout" (paramName arr) slice $ BasicOp $ SubExp se
            return $ resultBody $ to_scan ++ to_red ++ map Var mapout_arrs'

          not_in_bounds_fold_branch = return $ resultBody $ map (Var . paramName) $
                                      scan_acc_params ++ red_acc_params ++ mapout_arr_params

      (to_scan_res, to_red_res, mapout_arrs') <-
        fmap (splitAt3 (length scan_nes) (length red_nes)) . letTupExp "foldres" =<<
        eIf in_bounds in_bounds_fold_branch not_in_bounds_fold_branch

      (scanned_arrs, scanout_arrs') <-
        doScan j kspace in_bounds scanout_arr_params to_scan_res

      new_scan_carries <-
        resetCarries "scan" lid scan_acc_params scan_nes' $ runBodyBinder $ do
          carries <- forM scanned_arrs $ \arr -> do
            arr_t <- lookupType arr
            let slice = fullSlice arr_t [DimFix last_thread]
            letSubExp "carry" $ BasicOp $ Index arr slice
          return $ resultBody carries

      red_res <- doReduce kspace to_red_res

      new_red_carries <- resetCarries "red" lid red_acc_params red_nes' $
                         return $ resultBody $ map Var red_res

      -- HACK
      new_scan_carries' <- letTupExp "new_carry_sync" $ Op $ Barrier $ map Var new_scan_carries
      return $ resultBody $ map Var $
        scanout_arrs' ++ new_red_carries ++ mapout_arrs' ++ new_scan_carries'

    result <- letTupExp "result" $ DoLoop [] merge form loop_body
    let (scanout_result, red_result, mapout_result, scan_carry_result) =
          splitAt4 (length scan_ts) (length red_ts) (length mapout_arrs) result
    return (map KernelInPlaceReturn scanout_result ++
            map (ThreadsReturn OneResultPerGroup . Var) scan_carry_result ++
            map (ThreadsReturn OneResultPerGroup . Var) red_result ++
            map KernelInPlaceReturn mapout_result)

  let kts = scanout_arr_ts ++ scan_ts ++ red_ts ++ mapout_arr_ts
      kbody = KernelBody () stms res

  return $ Kernel (KernelDebugHints "scan1" []) kspace kts kbody
  where num_groups = kernelWorkgroups scan_sizes
        group_size = kernelWorkgroupSize scan_sizes
        num_threads = kernelNumThreads scan_sizes
        num_elements = kernelTotalElements scan_sizes
        consumed_in_foldlam = consumedInBody $ lambdaBody $ Alias.analyseLambda foldlam

        mkOutArray desc t = do
          let arr_t = t `arrayOfRow` w
          arr <- letExp desc $ BasicOp $ Scratch (elemType arr_t) (arrayDims arr_t)
          pname <- newVName $ desc++"param"
          return (arr, Param pname $ toDecl arr_t Unique, arr_t)

        mkAccMergeParam (Param pname ptype) se = do
          pname' <- newVName $ baseString pname ++ "_merge"
          -- We have to copy the initial merge parameter (the neutral
          -- element) if it is consumed inside the lambda.
          case se of
            Var v | pname `S.member` consumed_in_foldlam -> do
                      se' <- letSubExp "scan_ne_copy" $ BasicOp $ Copy v
                      return (Param pname' $ toDecl ptype Unique,
                              se')
            _ -> return (Param pname' $ toDecl ptype Nonunique,
                         se)

        doScan j kspace in_bounds scanout_arr_params to_scan_res = do
          let lid = spaceLocalId kspace
              scan_ts = map (rowType . paramType) scanout_arr_params
          -- Create an array of per-thread fold results and scan it.
          to_scan_arrs <- letTupExp "combined" $
                          Op $ Combine [(spaceLocalId kspace, group_size)] scan_ts [] $
                          Body () mempty $ map Var to_scan_res
          scanned_arrs <- letTupExp "scanned" $
                          Op $ GroupScan group_size scan_lam $ zip scan_nes to_scan_arrs

          -- If we are in bounds, we write scanned_arrs[lid] to scanout[j].
          let in_bounds_scan_branch = do
                -- Read scanned_arrs[j].
                arr_elems <- forM scanned_arrs $ \arr -> do
                  arr_t <- lookupType arr
                  let slice = fullSlice arr_t [DimFix $ Var lid]
                  letSubExp (baseString arr ++ "_elem") $ BasicOp $ Index arr slice

                -- Scatter the to_map parts to the scanout arrays using
                -- in-place updates.
                scanout_arrs' <- forM (zip arr_elems scanout_arr_params) $ \(se,p) -> do
                  let slice = fullSlice (paramType p) [DimFix j]
                  letInPlace "mapout" (paramName p) slice $ BasicOp $ SubExp se
                return $ resultBody $ map Var scanout_arrs'

              not_in_bounds_scan_branch =
                return $ resultBody $ map (Var . paramName) scanout_arr_params

          scanres <- letTupExp "scanres" =<<
                     eIf in_bounds in_bounds_scan_branch not_in_bounds_scan_branch
          return (scanned_arrs, scanres)

        doReduce kspace to_red_res = do
          red_ts <- mapM lookupType to_red_res

          -- Create an array of per-thread fold results and reduce it.
          to_red_arrs <- letTupExp "combined" $
                         Op $ Combine [(spaceLocalId kspace, group_size)] red_ts [] $
                         Body () mempty $ map Var to_red_res
          letTupExp "reduced" $
            Op $ GroupReduce group_size red_lam $ zip red_nes to_red_arrs

        resetCarries what lid acc_params nes mk_read_res = do
          -- All threads but the first in the group reset the accumulator
          -- to the neutral element.  The first resets it to the carry-out
          -- of the scan or reduction.
          is_first_thread <- letSubExp "is_first_thread" $ BasicOp $
                             CmpOp (CmpEq int32) (Var lid) (constant (0::Int32))

          read_res <- mk_read_res

          reset_carry_outs <- runBodyBinder $ do
            carries <- forM (zip acc_params nes) $ \(p, se) ->
              case se of
                Var v | unique $ declTypeOf p ->
                        letSubExp "reset_acc_copy" $ BasicOp $ Copy v
                _ -> return se
            return $ resultBody carries

          letTupExp ("new_" ++ what ++ "_carry") $
            If is_first_thread read_res reset_carry_outs $
            ifCommon $ map paramType acc_params

-- Second stage scan kernel with no fold part.
scanKernel2 :: (MonadBinder m, Lore m ~ Kernels) =>
               KernelSize
            -> Lambda InKernel
            -> [(SubExp,VName)]
            -> m (Kernel InKernel)
scanKernel2 scan_sizes lam input = do
  let (nes, arrs) = unzip input
      scan_ts = lambdaReturnType lam

  kspace <- newKernelSpace (kernelWorkgroups scan_sizes,
                            group_size,
                            kernelNumThreads scan_sizes) (FlatThreadSpace [])
  let lid = spaceLocalId kspace

  (res, stms) <- runBinder $ localScope (scopeOfKernelSpace kspace) $ do
    -- Create an array of the elements we are to scan.
    let indexMine arr = do
          arr_t <- lookupType arr
          let slice = fullSlice arr_t [DimFix $ Var lid]
          letSubExp (baseString arr <> "_elem") $ BasicOp $ Index arr slice
    read_elements <- runBodyBinder $ resultBody <$> mapM indexMine arrs
    to_scan_arrs <- letTupExp "combined" $
                    Op $ Combine [(lid, group_size)] scan_ts [] read_elements
    scanned_arrs <- letTupExp "scanned" $
                    Op $ GroupScan group_size lam $ zip nes to_scan_arrs

    -- Each thread returns scanned_arrs[i].
    res_elems <- mapM indexMine scanned_arrs
    return $ map (ThreadsReturn AllThreads) res_elems

  return $ Kernel (KernelDebugHints "scan2" []) kspace (lambdaReturnType lam) $ KernelBody () stms res
  where group_size = kernelWorkgroupSize scan_sizes

-- | The 'VName's returned are the names of variables bound to the
-- carry-out of the last thread.  You can ignore them if you don't
-- need them.
blockedScan :: (MonadBinder m, Lore m ~ Kernels) =>
               Pattern Kernels -> SubExp
            -> Scan InKernel
            -> Reduce InKernel
            -> Lambda InKernel -> SubExp -> [(VName, SubExp)] -> [KernelInput]
            -> [VName]
            -> m [VName]
blockedScan pat w (scan_lam, scan_nes) (comm, red_lam, red_nes) map_lam segment_size ispace inps arrs = do
  foldlam <- composeLambda scan_lam red_lam map_lam

  (_, first_scan_size) <- blockedKernelSize w
  my_index <- newVName "my_index"
  other_index <- newVName "other_index"
  let num_groups = kernelWorkgroups first_scan_size
      group_size = kernelWorkgroupSize first_scan_size
      num_threads = kernelNumThreads first_scan_size
      my_index_param = Param my_index (Prim int32)
      other_index_param = Param other_index (Prim int32)

  let foldlam_scope = scopeOfLParams $ my_index_param : lambdaParams foldlam
      bindIndex i v = letBindNames_ [i] =<< toExp v
  compute_segments <- runBinder_ $ localScope foldlam_scope $
                      zipWithM_ bindIndex (map fst ispace) $
                      unflattenIndex (map (primExpFromSubExp int32 . snd) ispace)
                      (LeafExp (paramName my_index_param) int32 `quot`
                       primExpFromSubExp int32 segment_size)
  read_inps <- stmsFromList <$> mapM readKernelInput inps
  first_scan_foldlam <- renameLambda
    foldlam { lambdaParams = my_index_param :
                             lambdaParams foldlam
            , lambdaBody = insertStms (compute_segments<>read_inps) $
                           lambdaBody foldlam
            }
  first_scan_lam <- renameLambda
    scan_lam { lambdaParams = my_index_param :
                              other_index_param :
                              lambdaParams scan_lam
        }
  first_scan_red_lam <- renameLambda
    red_lam { lambdaParams = my_index_param :
                             other_index_param :
                             lambdaParams red_lam
            }

  let (scan_idents, red_idents, arr_idents) =
        splitAt3 (length scan_nes) (length red_nes) $ patternIdents pat
      final_res_pat = Pattern [] $ take (length scan_nes) $ patternValueElements pat
  first_scan_pat <- basicPattern [] . concat <$>
    sequence [mapM (mkIntermediateIdent "seq_scanned" [w]) scan_idents,
              mapM (mkIntermediateIdent "scan_carry_out" [num_groups]) scan_idents,
              mapM (mkIntermediateIdent "red_carry_out" [num_groups]) red_idents,
              pure arr_idents]

  addStm . Let first_scan_pat (defAux ()) . Op =<< scanKernel1 w first_scan_size
    (first_scan_lam, scan_nes)
    (comm, first_scan_red_lam, red_nes)
    first_scan_foldlam arrs

  let (sequentially_scanned, group_carry_out, group_red_res, _) =
        splitAt4 (length scan_nes) (length scan_nes) (length red_nes) $ patternNames first_scan_pat

  let second_scan_size = KernelSize one num_groups one num_groups num_groups
  unless (null group_red_res) $ do
    second_stage_red_lam <- renameLambda first_scan_red_lam
    red_res <- letTupExp "red_res" . Op =<<
               reduceKernel second_scan_size second_stage_red_lam red_nes group_red_res
    forM_ (zip red_idents red_res) $ \(dest, arr) -> do
      arr_t <- lookupType arr
      addStm $ mkLet [] [dest] $ BasicOp $ Index arr $
        fullSlice arr_t [DimFix $ constant (0 :: Int32)]

  second_scan_lam <- renameLambda first_scan_lam

  group_carry_out_scanned <-
    letTupExp "group_carry_out_scanned" . Op =<<
    scanKernel2 second_scan_size
    second_scan_lam (zip scan_nes group_carry_out)

  last_group <- letSubExp "last_group" $ BasicOp $ BinOp (Sub Int32) num_groups one
  carries <- forM group_carry_out_scanned $ \carry_outs -> do
    arr_t <- lookupType carry_outs
    letExp "carry_out" $ BasicOp $ Index carry_outs $ fullSlice arr_t [DimFix last_group]

  scan_lam''' <- renameLambda scan_lam
  j <- newVName "j"
  let (acc_params, arr_params) =
        splitAt (length scan_nes) $ lambdaParams scan_lam'''
      result_map_input =
        zipWith (mkKernelInput [Var j]) arr_params sequentially_scanned

  chunks_per_group <- letSubExp "chunks_per_group" =<<
    eDivRoundingUp Int32 (eSubExp w) (eSubExp num_threads)
  elems_per_group <- letSubExp "elements_per_group" $
    BasicOp $ BinOp (Mul Int32) chunks_per_group group_size

  result_map_body <- runBodyBinder $ localScope (scopeOfLParams $ map kernelInputParam result_map_input) $ do
    group_id <-
      letSubExp "group_id" $
      BasicOp $ BinOp (SQuot Int32) (Var j) elems_per_group
    let do_nothing =
          pure $ resultBody $ map (Var . paramName) arr_params
        add_carry_in = runBodyBinder $ do
          forM_ (zip acc_params group_carry_out_scanned) $ \(p, arr) -> do
            carry_in_index <-
              letSubExp "carry_in_index" $
              BasicOp $ BinOp (Sub Int32) group_id one
            arr_t <- lookupType arr
            letBindNames_ [paramName p] $
              BasicOp $ Index arr $ fullSlice arr_t [DimFix carry_in_index]
          return $ lambdaBody scan_lam'''
    group_lasts <-
      letTupExp "final_result" =<<
        eIf (eCmpOp (CmpEq int32) (eSubExp zero) (eSubExp group_id))
        do_nothing
        add_carry_in
    return $ resultBody $ map Var group_lasts

  (mapk_bnds, mapk) <- mapKernelFromBody w (FlatThreadSpace [(j, w)]) result_map_input
                       (lambdaReturnType scan_lam) result_map_body
  addStms mapk_bnds
  letBind_ final_res_pat $ Op mapk

  return carries
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

mapKernelSkeleton :: (HasScope Kernels m, MonadFreshNames m) =>
                     SubExp -> SpaceStructure -> [KernelInput]
                  -> m (KernelSpace,
                        Stms Kernels,
                        Stms InKernel)
mapKernelSkeleton w ispace inputs = do
  ((group_size, num_threads, num_groups), ksize_bnds) <-
    runBinder $ numThreadsAndGroups w

  read_input_bnds <- stmsFromList <$> mapM readKernelInput inputs

  let ksize = (num_groups, group_size, num_threads)

  space <- newKernelSpace ksize ispace
  return (space, ksize_bnds, read_input_bnds)

-- Given the desired minium number of threads, compute the group size,
-- number of groups and total number of threads.
numThreadsAndGroups :: (MonadBinder m, Op (Lore m) ~ Kernel innerlore) =>
                       SubExp -> m (SubExp, SubExp, SubExp)
numThreadsAndGroups w = do
  group_size <- getSize "group_size" SizeGroup
  num_groups <- letSubExp "num_groups" =<< eDivRoundingUp Int32
    (eSubExp w) (eSubExp group_size)
  num_threads <- letSubExp "num_threads" $
    BasicOp $ BinOp (Mul Int32) num_groups group_size
  return (group_size, num_threads, num_groups)

mapKernel :: (HasScope Kernels m, MonadFreshNames m) =>
             SubExp -> SpaceStructure -> [KernelInput]
          -> [Type] -> KernelBody InKernel
          -> m (Stms Kernels, Kernel InKernel)
mapKernel w ispace inputs rts (KernelBody () kstms krets) = do
  (space, ksize_bnds, read_input_bnds) <- mapKernelSkeleton w ispace inputs

  let kbody' = KernelBody () (read_input_bnds <> kstms) krets
  return (ksize_bnds, Kernel (KernelDebugHints "map" []) space rts kbody')

mapKernelFromBody :: (HasScope Kernels m, MonadFreshNames m) =>
                     SubExp -> SpaceStructure -> [KernelInput]
                  -> [Type] -> Body InKernel
                  -> m (Stms Kernels, Kernel InKernel)
mapKernelFromBody w ispace inputs rts body =
  mapKernel w ispace inputs rts kbody
  where kbody = KernelBody () (bodyStms body) krets
        krets = map (ThreadsReturn ThreadsInSpace) $ bodyResult body

data KernelInput = KernelInput { kernelInputName :: VName
                               , kernelInputType :: Type
                               , kernelInputArray :: VName
                               , kernelInputIndices :: [SubExp]
                               }
                 deriving (Show)

kernelInputParam :: KernelInput -> Param Type
kernelInputParam p = Param (kernelInputName p) (kernelInputType p)

readKernelInput :: (HasScope Kernels m, Monad m) =>
                   KernelInput -> m (Stm InKernel)
readKernelInput inp = do
  let pe = PatElem (kernelInputName inp) $ kernelInputType inp
  arr_t <- lookupType $ kernelInputArray inp
  return $ Let (Pattern [] [pe]) (defAux ()) $
    BasicOp $ Index (kernelInputArray inp) $
    fullSlice arr_t $ map DimFix $ kernelInputIndices inp

newKernelSpace :: MonadFreshNames m =>
                  (SubExp,SubExp,SubExp) -> SpaceStructure -> m KernelSpace
newKernelSpace (num_groups, group_size, num_threads) ispace =
  KernelSpace
  <$> newVName "global_tid"
  <*> newVName "local_tid"
  <*> newVName "group_id"
  <*> pure num_threads
  <*> pure num_groups
  <*> pure group_size
  <*> pure ispace
