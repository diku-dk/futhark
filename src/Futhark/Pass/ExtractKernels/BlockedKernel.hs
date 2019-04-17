{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.Pass.ExtractKernels.BlockedKernel
       ( blockedReductionStream
       , blockedMap

       , segRed
       , nonSegRed
       , segScan
       , segGenRed

       , mapKernel
       , mapKernelFromBody
       , KernelInput(..)
       , readKernelInput

       , newKernelSpace
       , getSize
       )
       where

import Control.Monad
import Data.Maybe
import Data.List
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
import Futhark.Representation.AST.Attributes.Aliases
import qualified Futhark.Analysis.Alias as Alias

getSize :: (MonadBinder m, Op (Lore m) ~ HostOp (Lore m) inner) =>
           String -> SizeClass -> m SubExp
getSize desc size_class = do
  size_key <- nameFromString . pretty <$> newVName desc
  letSubExp desc $ Op $ GetSize size_key size_class

blockedReductionStream :: (MonadFreshNames m, HasScope Kernels m) =>
                          Pattern Kernels
                       -> SubExp
                       -> Commutativity
                       -> Lambda InKernel -> Lambda InKernel
                       -> [SubExp] -> [VName]
                       -> m (Stms Kernels)
blockedReductionStream pat w comm reduce_lam fold_lam nes arrs = runBinder_ $ do
  (max_step_one_num_groups, step_one_size) <- blockedKernelSize =<< asIntS Int64 w

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

  let params_to_arrs = zip (map paramName $ drop 1 $ lambdaParams fold_lam') arrs
      consumedArray v = fromMaybe v $ lookup v params_to_arrs
      consumed_in_fold =
        S.map consumedArray $ consumedByLambda $ Alias.analyseLambda fold_lam

  arrs_copies <- forM arrs $ \arr ->
    if arr `S.member` consumed_in_fold then
      letExp (baseString arr <> "_copy") $ BasicOp $ Copy arr
    else return arr

  step_one <- chunkedReduceKernel w step_one_size comm reduce_lam fold_lam'
              nes arrs_copies
  addStm =<< renameStm (Let step_one_pat (defAux ()) $ Op $ HostOp step_one)

  step_two_pat <- basicPattern [] <$>
                  mapM (mkIntermediateIdent $ constant (1 :: Int32)) acc_idents

  let step_two_size = KernelSize one max_step_one_num_groups one num_chunks max_step_one_num_groups

  step_two <- reduceKernel step_two_size reduce_lam nes $ take (length nes) $ patternNames step_one_pat

  addStm $ Let step_two_pat (defAux ()) $ Op $ HostOp step_two

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
                    -> [SubExp] -> [VName]
                    -> m (Kernel InKernel)
chunkedReduceKernel w step_one_size comm reduce_lam fold_lam' nes arrs = do
  let ordering = case comm of Commutative -> Disorder
                              Noncommutative -> InOrder
      group_size = kernelWorkgroupSize step_one_size
      num_nonconcat = length nes

  space <- newKernelSpace (kernelWorkgroups step_one_size, group_size, kernelNumThreads step_one_size) $
           FlatThreadSpace []
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
  combine_reds <- forM (zip chunk_red_pes' chunk_red_pes) $ \(pe', pe) -> do
    combine_id <- newVName "combine_id"
    return $ Let (Pattern [] [pe']) (defAux ()) $ Op $
      Combine (combineSpace [(combine_id, group_size)]) [patElemType pe] [] $
      Body () mempty [Var $ patElemName pe]

  final_red_pes <- forM (lambdaReturnType reduce_lam) $ \t -> do
    pe_name <- newVName "final_result"
    return $ PatElem pe_name t
  let reduce_chunk = Let (Pattern [] final_red_pes) (defAux ()) $ Op $
                     GroupReduce group_size reduce_lam $
                     zip nes $ map patElemName chunk_red_pes'

  red_rets <- forM final_red_pes $ \pe ->
    return $ GroupsReturn $ Var $ patElemName pe
  elems_per_thread <- asIntS Int32 $ kernelElementsPerThread step_one_size
  map_rets <- forM chunk_map_pes $ \pe ->
    return $ ConcatReturns ordering' w elems_per_thread Nothing $ patElemName pe
  let rets = red_rets ++ map_rets

  return $ Kernel (KernelDebugHints "chunked_reduce" [("input size", w)]) space ts $
    KernelBody () (chunk_and_fold<>stmsFromList combine_reds<>oneStm reduce_chunk) rets

reduceKernel :: (MonadBinder m, Lore m ~ Kernels) =>
                KernelSize
             -> Lambda InKernel
             -> [SubExp]
             -> [VName]
             -> m (Kernel InKernel)
reduceKernel step_two_size reduce_lam nes arrs = do
  let group_size = kernelWorkgroupSize step_two_size
      red_ts = lambdaReturnType reduce_lam
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

    combine_id <- newVName "combine_id"
    letBind_ combine_pat $
      Op $ Combine (combineSpace [(combine_id, group_size)])
      (map rowType $ patternTypes combine_pat) [] combine_body

    let arrs' = patternNames combine_pat

    final_res_pes <- forM (lambdaReturnType reduce_lam) $ \t -> do
      pe_name <- newVName "final_result"
      return $ PatElem pe_name t
    letBind_ (Pattern [] final_res_pes) $
      Op $ GroupReduce group_size reduce_lam $ zip nes arrs'

    forM final_res_pes $ \pe ->
      return $ GroupsReturn $ Var $ patElemName pe

  return $ Kernel (KernelDebugHints "reduce" []) space (lambdaReturnType reduce_lam)  $
    KernelBody () kstms rets

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

prepareRedOrScan :: (MonadBinder m, Lore m ~ Kernels) =>
                    SubExp -> SubExp
                 -> LambdaT InKernel
                 -> [VName] -> [(VName, SubExp)] -> [KernelInput]
                 -> m (KernelSpace, Body InKernel)
prepareRedOrScan total_num_elements w map_lam arrs ispace inps = do
  (_, KernelSize num_groups group_size _ _ num_threads) <- blockedKernelSize =<< asIntS Int64 total_num_elements
  gtid <- newVName "gtid"
  kspace <- newKernelSpace (num_groups, group_size, num_threads) $ FlatThreadSpace $
            ispace ++ [(gtid, w)]
  body <- runBodyBinder $ localScope (scopeOfKernelSpace kspace) $ do
    mapM_ (addStm <=< readKernelInput) inps
    forM_ (zip (lambdaParams map_lam) arrs) $ \(p, arr) -> do
      arr_t <- lookupType arr
      letBindNames_ [paramName p] $
        BasicOp $ Index arr $ fullSlice arr_t [DimFix $ Var gtid]
    return $ lambdaBody map_lam

  return (kspace, body)

segRed :: (MonadFreshNames m, HasScope Kernels m) =>
          Pattern Kernels
       -> SubExp
       -> SubExp -- segment size
       -> Commutativity
       -> Lambda InKernel -> Lambda InKernel
       -> [SubExp] -> [VName]
       -> [(VName, SubExp)] -- ispace = pair of (gtid, size) for the maps on "top" of this reduction
       -> [KernelInput]     -- inps = inputs that can be looked up by using the gtids from ispace
       -> m (Stms Kernels)
segRed pat total_num_elements w comm reduce_lam map_lam nes arrs ispace inps = runBinder_ $ do
  (kspace, body) <- prepareRedOrScan total_num_elements w map_lam arrs ispace inps
  letBind_ pat $ Op $ HostOp $
    SegRed kspace comm reduce_lam nes (lambdaReturnType map_lam) body

segScan :: (MonadFreshNames m, HasScope Kernels m) =>
           Pattern Kernels
        -> SubExp
        -> SubExp -- segment size
        -> Lambda InKernel -> Lambda InKernel
        -> [SubExp] -> [VName]
        -> [(VName, SubExp)] -- ispace = pair of (gtid, size) for the maps on "top" of this scan
        -> [KernelInput]     -- inps = inputs that can be looked up by using the gtids from ispace
        -> m (Stms Kernels)
segScan pat total_num_elements w scan_lam map_lam nes arrs ispace inps = runBinder_ $ do
  (kspace, body) <- prepareRedOrScan total_num_elements w map_lam arrs ispace inps
  letBind_ pat $ Op $ HostOp $
    SegScan kspace scan_lam nes (lambdaReturnType map_lam) body

nonSegRed :: (MonadFreshNames m, HasScope Kernels m) =>
             Pattern Kernels
          -> SubExp
          -> Commutativity
          -> Lambda InKernel
          -> Lambda InKernel
          -> [SubExp]
          -> [VName]
          -> m (Stms Kernels)
nonSegRed pat w comm red_lam map_lam nes arrs = runBinder_ $ do
  -- We add a unit-size segment on top to ensure that the result
  -- of the SegRed is an array, which we then immediately index.
  -- This is useful in the case that the value is used on the
  -- device afterwards, as this may save an expensive
  -- host-device copy (scalars are kept on the host, but arrays
  -- may be on the device).
  let addDummyDim t = t `arrayOfRow` intConst Int32 1
  pat' <- fmap addDummyDim <$> renamePattern pat
  dummy <- newVName "dummy"
  addStms =<<
    segRed pat' w w comm red_lam map_lam nes arrs [(dummy, intConst Int32 1)] []

  forM_ (zip (patternNames pat') (patternNames pat)) $ \(from, to) -> do
    from_t <- lookupType from
    letBindNames_ [to] $ BasicOp $ Index from $ fullSlice from_t [DimFix $ intConst Int32 0]

segGenRed :: (MonadFreshNames m, HasScope Kernels m) =>
             Pattern Kernels
          -> SubExp
          -> [(VName,SubExp)] -- ^ Segment indexes and sizes.
          -> [KernelInput]
          -> [GenReduceOp InKernel]
          -> Lambda InKernel -> [VName]
          -> m (Stms Kernels)
segGenRed pat arr_w ispace inps ops lam arrs = runBinder_ $ do
  let (_, segment_sizes) = unzip ispace
  arr_w_64 <- letSubExp "arr_w_64" =<< eConvOp (SExt Int32 Int64) (toExp arr_w)
  segment_sizes_64 <- mapM (letSubExp "segment_size_64" <=< eConvOp (SExt Int32 Int64) . toExp) segment_sizes
  total_w <- letSubExp "genreduce_elems" =<< foldBinOp (Mul Int64) arr_w_64 segment_sizes_64
  (_, KernelSize num_groups group_size _ _ num_threads) <-
    blockedKernelSize total_w

  gtid <- newVName "gtid"
  kspace <- newKernelSpace (num_groups, group_size, num_threads) $
            FlatThreadSpace $ ispace ++ [(gtid, arr_w)]

  body <- runBodyBinder $ localScope (scopeOfKernelSpace kspace) $ do
    mapM_ (addStm <=< readKernelInput) inps
    forM_ (zip (lambdaParams lam) arrs) $ \(p, arr) -> do
      arr_t <- lookupType arr
      letBindNames_ [paramName p] $
        BasicOp $ Index arr $ fullSlice arr_t [DimFix $ Var gtid]
    return $ lambdaBody lam

  letBind_ pat $ Op $ HostOp $ SegGenRed kspace ops (lambdaReturnType lam) body

blockedMap :: (MonadFreshNames m, HasScope Kernels m) =>
              Pattern Kernels -> SubExp
           -> StreamOrd -> Lambda InKernel -> [SubExp] -> [VName]
           -> m (Stm Kernels, Stms Kernels)
blockedMap concat_pat w ordering lam nes arrs = runBinder $ do
  (_, kernel_size) <- blockedKernelSize =<< asIntS Int64 w
  let num_nonconcat = length (lambdaReturnType lam) - patternSize concat_pat
      num_groups = kernelWorkgroups kernel_size
      group_size = kernelWorkgroupSize kernel_size
      num_threads = kernelNumThreads kernel_size
      ordering' =
        case ordering of InOrder -> SplitContiguous
                         Disorder -> SplitStrided $ kernelNumThreads kernel_size

  unused_gtid <- newVName "unused_gtid"
  space <- newKernelSpace (num_groups, group_size, num_threads)
                          (FlatThreadSpace [(unused_gtid, kernelNumThreads kernel_size)])
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
    return $ ThreadsReturn $ Var $ patElemName pe
  elems_per_thread <- asIntS Int32 $ kernelElementsPerThread kernel_size
  concat_rets <- forM chunk_map_pes $ \pe ->
    return $ ConcatReturns ordering' w elems_per_thread Nothing $ patElemName pe

  return $ Let pat (defAux ()) $ Op $ HostOp $ Kernel (KernelDebugHints "chunked_map" []) space ts $
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

  per_thread <- asIntS Int32 $ kernelElementsPerThread kernel_size
  splitArrays (paramName chunk_size) (map paramName arr_params) ordering' w
    (Var thread_gtid) per_thread arrs

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
                               -- ^ Int32
                             , kernelWorkgroupSize :: SubExp
                               -- ^ Int32
                             , kernelElementsPerThread :: SubExp
                               -- ^ Int64
                             , kernelTotalElements :: SubExp
                               -- ^ Int64
                             , kernelNumThreads :: SubExp
                               -- ^ Int32
                             }
                deriving (Eq, Ord, Show)

numberOfGroups :: MonadBinder m => SubExp -> SubExp -> SubExp -> m (SubExp, SubExp)
numberOfGroups w group_size max_num_groups = do
  -- If 'w' is small, we launch fewer groups than we normally would.
  -- We don't want any idle groups.
  w_div_group_size <- letSubExp "w_div_group_size" =<<
    eDivRoundingUp Int64 (eSubExp w) (eSubExp group_size)
  -- We also don't want zero groups.
  num_groups_maybe_zero <- letSubExp "num_groups_maybe_zero" $ BasicOp $ BinOp (SMin Int64)
                           w_div_group_size max_num_groups
  num_groups <- letSubExp "num_groups" $
                BasicOp $ BinOp (SMax Int64) (intConst Int64 1)
                num_groups_maybe_zero
  num_threads <-
    letSubExp "num_threads" $ BasicOp $ BinOp (Mul Int64) num_groups group_size
  return (num_groups, num_threads)

blockedKernelSize :: (MonadBinder m, Lore m ~ Kernels) =>
                     SubExp -> m (SubExp, KernelSize)
blockedKernelSize w = do
  group_size <- getSize "group_size" SizeGroup
  max_num_groups <- getSize "max_num_groups" SizeNumGroups

  group_size' <- asIntS Int64 group_size
  max_num_groups' <- asIntS Int64 max_num_groups
  (num_groups, num_threads) <- numberOfGroups w group_size' max_num_groups'
  num_groups' <- asIntS Int32 num_groups
  num_threads' <- asIntS Int32 num_threads

  per_thread_elements <-
    letSubExp "per_thread_elements" =<<
    eDivRoundingUp Int64 (toExp =<< asIntS Int64 w) (toExp =<< asIntS Int64 num_threads)

  return (max_num_groups,
          KernelSize num_groups' group_size per_thread_elements w num_threads')

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
numThreadsAndGroups :: (MonadBinder m, Op (Lore m) ~ HostOp (Lore m) inner) =>
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
        krets = map ThreadsReturn $ bodyResult body

data KernelInput = KernelInput { kernelInputName :: VName
                               , kernelInputType :: Type
                               , kernelInputArray :: VName
                               , kernelInputIndices :: [SubExp]
                               }
                 deriving (Show)

readKernelInput :: (HasScope scope m, Monad m) =>
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
