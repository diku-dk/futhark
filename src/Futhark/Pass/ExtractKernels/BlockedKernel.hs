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
       , mapKernelSkeleton
       )
       where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid
import qualified Data.HashSet as HS

import Prelude hiding (quot)

import Futhark.Analysis.PrimExp
import Futhark.Representation.AST
import Futhark.Representation.Kernels
       hiding (Prog, Body, Stm, Pattern, PatElem,
               BasicOp, Exp, Lambda, ExtLambda, FunDef, FParam, LParam, RetType)
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Transform.FirstOrderTransform (doLoopMapAccumL)
import Futhark.Representation.AST.Attributes.Aliases
import qualified Futhark.Analysis.Alias as Alias
import Futhark.Util
import Futhark.Util.IntegralExp

blockedReductionStream :: (MonadFreshNames m, HasScope Kernels m) =>
                          Pattern Kernels
                       -> Certificates -> SubExp
                       -> Commutativity
                       -> Lambda InKernel -> Lambda InKernel
                       -> [SubExp]
                       -> [VName]
                       -> m [Stm Kernels]
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
      letExp (baseString arr <> "_copy") $ BasicOp $ Copy arr
    else return arr

  step_one <- chunkedReduceKernel cs w step_one_size comm reduce_lam' fold_lam' nes arrs_copies
  addStm =<< renameStm
    (Let step_one_pat () $ Op step_one)

  step_two_pat <- basicPattern' [] <$>
                  mapM (mkIntermediateIdent $ constant (1 :: Int32)) acc_idents

  let step_two_size = KernelSize one num_chunks one num_chunks num_chunks

  step_two <- reduceKernel [] step_two_size reduce_lam' nes $ take (length nes) $ patternNames step_one_pat

  addStm $ Let step_two_pat () $ Op step_two

  forM_ (zip (patternIdents step_two_pat) (patternIdents pat)) $ \(arr, x) ->
    addStm $ mkLet' [] [x] $ BasicOp $ Index [] (identName arr) $
    fullSlice (identType arr) [DimFix $ constant (0 :: Int32)]
  where mkIntermediateIdent chunk_size ident =
          newIdent (baseString $ identName ident) $
          arrayOfRow (identType ident) chunk_size

chunkedReduceKernel :: (MonadBinder m, Lore m ~ Kernels) =>
                       Certificates
                    -> SubExp
                    -> KernelSize
                    -> Commutativity
                    -> Lambda InKernel
                    -> Lambda InKernel
                    -> [SubExp]
                    -> [VName]
                    -> m (Kernel InKernel)
chunkedReduceKernel cs w step_one_size comm reduce_lam' fold_lam' nes arrs = do
  let ordering = case comm of Commutative -> Disorder
                              Noncommutative -> InOrder
      group_size = kernelWorkgroupSize step_one_size
      num_nonconcat = length nes

  space <- newKernelSpace (kernelWorkgroups step_one_size, group_size, kernelNumThreads step_one_size) $
           FlatThreadSpace []
  (chunk_red_pes, chunk_map_pes, chunk_and_fold) <-
    blockedPerThread (spaceGlobalId space)
    w step_one_size ordering fold_lam' num_nonconcat arrs
  let red_ts = map patElemType chunk_red_pes
      map_ts = map (rowType . patElemType) chunk_map_pes
      ts = red_ts ++ map_ts

  chunk_red_pes' <- forM red_ts $ \red_t -> do
    pe_name <- newVName "chunk_fold_red"
    return $ PatElem pe_name BindVar $ red_t `arrayOfRow` group_size
  let combine_reds = [ Let (Pattern [] [pe']) () $ Op $
                       Combine [(spaceLocalId space, group_size)] [patElemType pe]
                       (constant True) $
                       Body () [] [Var $ patElemName pe]
                     | (pe', pe) <- zip chunk_red_pes' chunk_red_pes ]

  final_red_pes <- forM (lambdaReturnType reduce_lam') $ \t -> do
    pe_name <- newVName "final_result"
    return $ PatElem pe_name BindVar t
  let reduce_chunk = Let (Pattern [] final_red_pes) () $ Op $
                     GroupReduce group_size reduce_lam' $
                     zip nes $ map patElemName chunk_red_pes'

  red_rets <- forM final_red_pes $ \pe ->
    return $ ThreadsReturn (OneThreadPerGroup (constant (0::Int32))) $ Var $ patElemName pe
  map_rets <- forM chunk_map_pes $ \pe ->
    return $ ConcatReturns ordering w (kernelElementsPerThread step_one_size) $ patElemName pe
  let rets = red_rets ++ map_rets

  return $ Kernel "chunked_reduce" cs space ts $
    KernelBody () (chunk_and_fold++combine_reds++[reduce_chunk]) rets

reduceKernel :: (MonadBinder m, Lore m ~ Kernels) =>
                Certificates
             -> KernelSize
             -> Lambda InKernel
             -> [SubExp]
             -> [VName]
             -> m (Kernel InKernel)
reduceKernel cs step_two_size reduce_lam' nes arrs = do
  let group_size = kernelWorkgroupSize step_two_size
      red_ts = lambdaReturnType reduce_lam'
  space <- newKernelSpace (kernelWorkgroups step_two_size, group_size, kernelNumThreads step_two_size) $
           FlatThreadSpace []
  let thread_id = spaceGlobalId space

  (copy_input, arrs_index) <-
    fmap unzip $ forM (zip red_ts arrs) $ \(t, arr) -> do
      arr_index <- newVName (baseString arr ++ "_index")
      return (Let (Pattern [] [PatElem arr_index BindVar t]) () $
              BasicOp $ Index [] arr $
              fullSlice (t `arrayOfRow` group_size) [DimFix (Var thread_id)]
             , arr_index)

  (combine_arrs, arrs') <-
    fmap unzip $ forM (zip red_ts arrs_index) $ \(red_t, arr_index) -> do
      arr' <- newVName $ baseString arr_index ++ "_combined"
      let pe = PatElem arr' BindVar $ red_t `arrayOfRow` group_size
      return (Let (Pattern [] [pe]) () $
              Op $ Combine [(spaceLocalId space, group_size)] [red_t] (constant True) $
              Body () [] [Var arr_index],
              arr')

  final_res_pes <- forM (lambdaReturnType reduce_lam') $ \t -> do
    pe_name <- newVName "final_result"
    return $ PatElem pe_name BindVar t
  let reduce = Let (Pattern [] final_res_pes) () $ Op $
               GroupReduce group_size reduce_lam' $ zip nes arrs'

  rets <- forM final_res_pes $ \pe ->
    return $ ThreadsReturn (OneThreadPerGroup (constant (0::Int32))) $ Var $ patElemName pe

  return $ Kernel "reduce" cs space (lambdaReturnType reduce_lam')  $
    KernelBody () (copy_input++combine_arrs++[reduce]) rets

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
            mkLet' [] [paramIdent p] $ BasicOp $ Copy v
      mkAccInit p x = mkLet' [] [paramIdent p] $ BasicOp $ SubExp x
      acc_init_bnds = zipWith mkAccInit fold_acc_params nes
  return lam { lambdaBody = insertStms acc_init_bnds $
                            lambdaBody lam
             , lambdaParams = thread_index_param :
                              fold_chunk_param :
                              fold_inp_params
             }

blockedReduction :: (MonadFreshNames m, HasScope Kernels m) =>
                    Pattern Kernels
                 -> Certificates -> SubExp
                 -> Commutativity
                 -> Lambda InKernel -> Lambda InKernel
                 -> [SubExp]
                 -> [VName]
                 -> m [Stm Kernels]
blockedReduction pat cs w comm reduce_lam fold_lam nes arrs = runBinder_ $ do
  fold_lam' <- chunkLambda pat nes fold_lam

  let arr_idents = drop (length nes) $ patternIdents pat
  map_out_arrs <- forM arr_idents $ \(Ident name t) ->
    letExp (baseString name <> "_out_in") $
    BasicOp $ Scratch (elemType t) (arrayDims t)

  mapM_ addStm =<<
    blockedReductionStream pat cs w comm reduce_lam fold_lam' nes
    (arrs ++ map_out_arrs)

blockedMap :: (MonadFreshNames m, HasScope Kernels m) =>
              Pattern Kernels -> Certificates -> SubExp
           -> StreamOrd -> Lambda InKernel -> [SubExp] -> [VName]
           -> m (Stm Kernels, [Stm Kernels])
blockedMap concat_pat cs w ordering lam nes arrs = runBinder $ do
  kernel_size <- blockedKernelSize w
  let num_nonconcat = length (lambdaReturnType lam) - patternSize concat_pat
      num_groups = kernelWorkgroups kernel_size
      group_size = kernelWorkgroupSize kernel_size
      num_threads = kernelNumThreads kernel_size

  space <- newKernelSpace (num_groups, group_size, num_threads) (FlatThreadSpace [])
  lam' <- kerneliseLambda nes lam
  (chunk_red_pes, chunk_map_pes, chunk_and_fold) <-
    blockedPerThread (spaceGlobalId space) w kernel_size ordering lam' num_nonconcat arrs

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

  return $ Let pat () $ Op $ Kernel "chunked_map" cs space ts $
    KernelBody () chunk_and_fold $ nonconcat_rets ++ concat_rets

blockedPerThread :: MonadFreshNames m =>
                    VName -> SubExp -> KernelSize -> StreamOrd -> Lambda InKernel
                 -> Int -> [VName]
                 -> m ([PatElem InKernel], [PatElem InKernel], [Stm InKernel])
blockedPerThread thread_gtid w kernel_size ordering lam num_nonconcat arrs = do
  let (_, chunk_size, [], arr_params) =
        partitionChunkedKernelFoldParameters 0 $ lambdaParams lam

  split_bound <- forM arr_params $ \arr_param -> do
    let chunk_t = paramType arr_param `setOuterSize` Var (paramName chunk_size)
    return $ PatElem (paramName arr_param) BindVar chunk_t
  let chunk_stm =
        -- Zero-array SplitArrays are invalid, so make sure we don't create one.
        if null arrs
        then [Let (Pattern []
                   [PatElem (paramName chunk_size) BindVar $ paramType chunk_size]) () $ Op $
              SplitSpace ordering w (Var thread_gtid)
              (kernelNumThreads kernel_size) (kernelElementsPerThread kernel_size)]
        else [Let
               (Pattern
                 [PatElem (paramName chunk_size) BindVar $ paramType chunk_size]
                 split_bound) () $ Op $
               SplitArray ordering w (Var thread_gtid)
               (kernelNumThreads kernel_size) (kernelElementsPerThread kernel_size) arrs]
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
      fold_chunk = bodyStms (lambdaBody lam) ++
                   [ Let (Pattern [] [pe]) () $ BasicOp $ SubExp se
                   | (pe,se) <- zip chunk_red_pes chunk_red_ses ] ++
                   [ Let (Pattern [] [pe]) () $ BasicOp $ SubExp se
                   | (pe,se) <- zip chunk_map_pes chunk_map_ses ]

  return (chunk_red_pes, chunk_map_pes, chunk_stm ++ fold_chunk)

data KernelSize = KernelSize { kernelWorkgroups :: SubExp
                             , kernelWorkgroupSize :: SubExp
                             , kernelElementsPerThread :: SubExp
                             , kernelTotalElements :: SubExp
                             , kernelNumThreads :: SubExp
                             }
                deriving (Eq, Ord, Show)

blockedKernelSize :: (MonadBinder m, Lore m ~ Kernels) =>
                     SubExp -> m KernelSize
blockedKernelSize w = do
  num_groups <- letSubExp "num_groups" $ Op NumGroups
  group_size <- letSubExp "group_size" $ Op GroupSize

  num_threads <-
    letSubExp "num_threads" $ BasicOp $ BinOp (Mul Int32) num_groups group_size

  per_thread_elements <-
    letSubExp "per_thread_elements" =<<
    eDivRoundingUp Int32 (eSubExp w) (eSubExp num_threads)

  return $ KernelSize num_groups group_size per_thread_elements w num_threads

-- First stage scan kernel.
scanKernel1 :: (MonadBinder m, Lore m ~ Kernels) =>
               Certificates -> SubExp -> KernelSize
            -> Lambda InKernel -> Lambda InKernel
            -> [SubExp] -> [VName]
            -> m (Kernel InKernel)
scanKernel1 cs w scan_sizes lam foldlam nes arrs = do
  let (scan_ts, map_ts) =
        splitAt (length nes) $ lambdaReturnType foldlam
      (_, foldlam_acc_params, _) =
        partitionChunkedFoldParameters (length nes) $ lambdaParams foldlam

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

    -- The merge parameters are the scanout arrays, the mapout arrays,
    -- and the (renamed) accumulator parameters of foldlam.
    (acc_params, nes') <- unzip <$> zipWithM mkAccMergeParam foldlam_acc_params nes
    let merge = zip scanout_arr_params (map Var scanout_arrs) ++
                zip mapout_arr_params (map Var mapout_arrs) ++
                zip acc_params nes'
    i <- newVName "i"
    let form = ForLoop i Int32 num_iterations

    loop_body <- runBodyBinder $ localScope (scopeOfFParams (map fst merge) <>
                                             scopeOfLoopForm form) $ do
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
              letSubExp (baseString arr ++ "_elem") $ BasicOp $ Index [] arr slice

            -- Apply the body of the fold function.
            fold_res <- eLambda foldlam $ j : map (Var . paramName) acc_params ++ arr_elems

            -- Write the to_map parts to the mapout arrays using
            -- in-place updates, and return the to_scan parts.
            let (to_scan, to_map) = splitAt (length nes) fold_res
            mapout_arrs' <- forM (zip to_map mapout_arr_params) $ \(se,arr) -> do
              let slice = fullSlice (paramType arr) [DimFix j]
              letInPlace "mapout" [] (paramName arr) slice $ BasicOp $ SubExp se
            return $ resultBody $ to_scan ++ map Var mapout_arrs'

          not_in_bounds_fold_branch = return $ resultBody $ map (Var . paramName) $
                                      acc_params ++ mapout_arr_params

      (fold_accs, mapout_arrs') <-
        fmap (splitAt (length nes)) . letTupExp "foldres" =<<
        eIf in_bounds in_bounds_fold_branch not_in_bounds_fold_branch

      -- Create an array of per-thread fold results and scan it.
      to_scan_arrs <- letTupExp "combined" $
                      Op $ Combine [(spaceLocalId kspace, group_size)] scan_ts (constant True) $
                      Body () [] $ map Var fold_accs
      scanned_arrs <- letTupExp "scanned" $
                      Op $ GroupScan group_size lam $ zip nes to_scan_arrs

      -- If we are in bounds, we write scanned_arrs[lid] to scanout[j].
      let in_bounds_scan_branch = do
            -- Read scanned_arrs[j].
            arr_elems <- forM scanned_arrs $ \arr -> do
              arr_t <- lookupType arr
              let slice = fullSlice arr_t [DimFix $ Var lid]
              letSubExp (baseString arr ++ "_elem") $ BasicOp $ Index [] arr slice

            -- Write the to_map parts to the scanout arrays using
            -- in-place updates.
            scanout_arrs' <- forM (zip arr_elems scanout_arr_params) $ \(se,p) -> do
              let slice = fullSlice (paramType p) [DimFix j]
              letInPlace "mapout" [] (paramName p) slice $ BasicOp $ SubExp se
            return $ resultBody $ map Var scanout_arrs'

          not_in_bounds_scan_branch =
            return $ resultBody $ map (Var . paramName) scanout_arr_params

      scanout_arrs' <- letTupExp "scanres" =<<
                       eIf in_bounds in_bounds_scan_branch not_in_bounds_scan_branch


      -- All threads but the first in the group reset the accumulator
      -- to the neutral element.  The first resets it to the carry-out
      -- of the scan.
      is_first_thread <- letSubExp "is_first_thread" $ BasicOp $
                         CmpOp (CmpEq int32) (Var lid) (constant (0::Int32))

      read_carry_outs <- runBodyBinder $ do
        carries <- forM scanned_arrs $ \arr -> do
          arr_t <- lookupType arr
          let slice = fullSlice arr_t [DimFix last_thread]
          letSubExp "carry" $ BasicOp $ Index [] arr slice
        return $ resultBody carries

      reset_carry_outs <- runBodyBinder $ do
        carries <- forM (zip acc_params nes) $ \(p, se) ->
          case se of
            Var v | unique $ declTypeOf p ->
                      letSubExp "reset_acc_copy" $ BasicOp $ Copy v
            _ -> return se
        return $ resultBody carries

      new_carries <- letTupExp "new_carry" $ If is_first_thread
        read_carry_outs
        reset_carry_outs
        (staticShapes $ map paramType acc_params)


      return $ resultBody $ map Var $ scanout_arrs' ++ mapout_arrs' ++ new_carries

    result <- letTupExp "result" $ DoLoop [] merge form loop_body
    let (scanout_result, mapout_result, carry_result) =
          splitAt3 (length scanout_arrs) (length mapout_arrs) result
    return (map KernelInPlaceReturn scanout_result ++
            map (ThreadsReturn (OneThreadPerGroup (constant (0::Int32))) . Var) carry_result ++
            map KernelInPlaceReturn mapout_result)

  let kts = scanout_arr_ts ++ scan_ts ++ mapout_arr_ts
      kbody = KernelBody () stms res

  return $ Kernel "scan1" cs kspace kts kbody
  where num_groups = kernelWorkgroups scan_sizes
        group_size = kernelWorkgroupSize scan_sizes
        num_threads = kernelNumThreads scan_sizes
        num_elements = kernelTotalElements scan_sizes

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
            Var v | pname `HS.member` consumed_in_foldlam -> do
                      se' <- letSubExp "scan_ne_copy" $ BasicOp $ Copy v
                      return (Param pname' $ toDecl ptype Unique,
                              se')
            _ -> return (Param pname' $ toDecl ptype Unique,
                         se)

        consumed_in_foldlam = consumedInBody $ lambdaBody $ Alias.analyseLambda foldlam

-- Second stage scan kernel with no fold part.
scanKernel2 :: (MonadBinder m, Lore m ~ Kernels) =>
               Certificates -> KernelSize
            -> Lambda InKernel
            -> [(SubExp,VName)]
            -> m (Kernel InKernel)
scanKernel2 cs scan_sizes lam input = do
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
          letSubExp (baseString arr <> "_elem") $ BasicOp $ Index [] arr slice
    read_elements <- runBodyBinder $ resultBody <$> mapM indexMine arrs
    to_scan_arrs <- letTupExp "combined" $
                    Op $ Combine [(lid, group_size)] scan_ts (constant True)
                    read_elements
    scanned_arrs <- letTupExp "scanned" $
                    Op $ GroupScan group_size lam $ zip nes to_scan_arrs

    -- Each thread returns scanned_arrs[i].
    res_elems <- mapM indexMine scanned_arrs
    return $ map (ThreadsReturn AllThreads) res_elems

  return $ Kernel "scan2" cs kspace (lambdaReturnType lam) $ KernelBody () stms res
  where group_size = kernelWorkgroupSize scan_sizes

blockedScan :: (MonadBinder m, Lore m ~ Kernels) =>
               Pattern Kernels
            -> Certificates -> SubExp
            -> Lambda InKernel -> Lambda InKernel
            -> SubExp -> [(VName, SubExp)] -> [KernelInput]
            -> [SubExp] -> [VName]
            -> m ()
blockedScan pat cs w lam foldlam segment_size ispace inps nes arrs = do
  first_scan_size <- blockedKernelSize w
  my_index <- newVName "my_index"
  other_index <- newVName "other_index"
  let num_groups = kernelWorkgroups first_scan_size
      group_size = kernelWorkgroupSize first_scan_size
      num_threads = kernelNumThreads first_scan_size
      my_index_param = Param my_index (Prim int32)
      other_index_param = Param other_index (Prim int32)

  let foldlam_scope = scopeOfLParams $ my_index_param : lambdaParams foldlam
      bindIndex i v = letBindNames'_ [i] =<< toExp v
  compute_segments <- runBinder_ $ localScope foldlam_scope $
                      zipWithM_ bindIndex (map fst ispace) $
                      unflattenIndex (map (primExpFromSubExp int32 . snd) ispace)
                      (LeafExp (paramName my_index_param) int32 `quot`
                       primExpFromSubExp int32 segment_size)
  read_inps <- mapM readKernelInput inps
  first_scan_foldlam <- renameLambda
    foldlam { lambdaParams = my_index_param :
                             lambdaParams foldlam
            , lambdaBody = insertStms (compute_segments++read_inps) $
                           lambdaBody foldlam
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

  addStm . Let first_scan_pat () . Op =<< scanKernel1 cs w first_scan_size
    first_scan_lam first_scan_foldlam nes arrs

  let (sequentially_scanned, group_carry_out, _) =
        splitAt3 (length nes) (length nes) $ patternNames first_scan_pat

  let second_scan_size = KernelSize one num_groups one num_groups num_groups
  second_scan_lam <- renameLambda first_scan_lam

  group_carry_out_scanned <-
    letTupExp "group_carry_out_scanned" . Op =<<
    scanKernel2 cs second_scan_size
    second_scan_lam (zip nes group_carry_out)

  lam''' <- renameLambda lam
  j <- newVName "j"
  let (acc_params, arr_params) =
        splitAt (length nes) $ lambdaParams lam'''
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
            letBindNames'_ [paramName p] $
              BasicOp $ Index [] arr $ fullSlice arr_t [DimFix carry_in_index]
          return $ lambdaBody lam'''
    group_lasts <-
      letTupExp "final_result" =<<
        eIf (eCmpOp (CmpEq int32) (eSubExp zero) (eSubExp group_id))
        do_nothing
        add_carry_in
    return $ resultBody $ map Var group_lasts
  (mapk_bnds, mapk) <- mapKernelFromBody [] w (FlatThreadSpace [(j, w)]) result_map_input
                       (lambdaReturnType lam) result_map_body
  mapM_ addStm mapk_bnds
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

mapKernelSkeleton :: (HasScope Kernels m, MonadFreshNames m) =>
                     SubExp -> SpaceStructure -> [KernelInput]
                  -> m (KernelSpace,
                        [Stm Kernels],
                        [Stm InKernel])
mapKernelSkeleton w ispace inputs = do
  group_size_v <- newVName "group_size"

  ((num_threads, num_groups), ksize_bnds) <- runBinder $ do
    letBindNames'_ [group_size_v] $ Op GroupSize
    numThreadsAndGroups w $ Var group_size_v

  read_input_bnds <- mapM readKernelInput inputs

  let ksize = (num_groups, Var group_size_v, num_threads)

  space <- newKernelSpace ksize ispace
  return (space, ksize_bnds, read_input_bnds)


-- Given the desired minium number of threads and the number of
-- threads per group, compute the number of groups and total number of
-- threads.
numThreadsAndGroups :: MonadBinder m => SubExp -> SubExp -> m (SubExp, SubExp)
numThreadsAndGroups w group_size = do
  num_groups <- letSubExp "num_groups" =<< eDivRoundingUp Int32
    (eSubExp w) (eSubExp group_size)
  num_threads <- letSubExp "num_threads" $
    BasicOp $ BinOp (Mul Int32) num_groups group_size
  return (num_threads, num_groups)

mapKernel :: (HasScope Kernels m, MonadFreshNames m) =>
             Certificates -> SubExp -> SpaceStructure -> [KernelInput]
          -> [Type] -> KernelBody InKernel
          -> m ([Stm Kernels], Kernel InKernel)
mapKernel cs w ispace inputs rts (KernelBody () kstms krets) = do
  group_size_v <- newVName "group_size"

  group_size_bnds <- runBinder_ $
    letBindNames'_ [group_size_v] $ Op GroupSize

  (space, ksize_bnds, read_input_bnds) <- mapKernelSkeleton w ispace inputs

  let kbody' = KernelBody () (read_input_bnds ++ kstms) krets
  return (group_size_bnds ++ ksize_bnds, Kernel "map" cs space rts kbody')

mapKernelFromBody :: (HasScope Kernels m, MonadFreshNames m) =>
                     Certificates -> SubExp -> SpaceStructure -> [KernelInput]
                  -> [Type] -> Body InKernel
                  -> m ([Stm Kernels], Kernel InKernel)
mapKernelFromBody cs w ispace inputs rts body =
  mapKernel cs w ispace inputs rts kbody
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
  let pe = PatElem (kernelInputName inp) BindVar $ kernelInputType inp
  arr_t <- lookupType $ kernelInputArray inp
  return $ Let (Pattern [] [pe]) () $
    BasicOp $ Index [] (kernelInputArray inp) $
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
