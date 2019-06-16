{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.Pass.ExtractKernels.BlockedKernel
       ( segRed
       , nonSegRed
       , huskedNonSegRed
       , segScan
       , segGenRed

       , streamRed
       , huskedStreamRed
       , streamMap

       , mapKernel
       , KernelInput(..)
       , readKernelInput

       , newKernelSpace
       , getSize
       )
       where

import Control.Monad
import Control.Monad.Writer
import Data.Maybe
import Data.List

import Prelude hiding (quot)

import Futhark.Analysis.PrimExp
import Futhark.Representation.AST
import Futhark.Representation.Kernels
       hiding (Prog, Body, Stm, Pattern, PatElem,
               BasicOp, Exp, Lambda, FunDef, FParam, LParam, RetType)
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Transform.Rename

getSize :: (MonadBinder m, Op (Lore m) ~ HostOp (Lore m) inner) =>
           String -> SizeClass -> m SubExp
getSize desc size_class = do
  size_key <- nameFromString . pretty <$> newVName desc
  letSubExp desc $ Op $ GetSize size_key size_class

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
                 -> m (KernelSpace, KernelBody InKernel)
prepareRedOrScan total_num_elements w map_lam arrs ispace inps = do
  (_, KernelSize num_groups group_size _ _ num_threads) <- blockedKernelSize =<< asIntS Int64 total_num_elements
  gtid <- newVName "gtid"
  kspace <- newKernelSpace (num_groups, group_size, num_threads, num_groups) $
            FlatThreadSpace $ ispace ++ [(gtid, w)]
  body <- fmap (uncurry (flip (KernelBody ()))) $ runBinder $
          localScope (scopeOfKernelSpace kspace) $ do
    mapM_ (addStm <=< readKernelInput) inps
    forM_ (zip (lambdaParams map_lam) arrs) $ \(p, arr) -> do
      arr_t <- lookupType arr
      letBindNames_ [paramName p] $
        BasicOp $ Index arr $ fullSlice arr_t [DimFix $ Var gtid]
    map ThreadsReturn <$> bodyBind (lambdaBody map_lam)

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
  (kspace, kbody) <- prepareRedOrScan total_num_elements w map_lam arrs ispace inps
  letBind_ pat $ Op $ HostOp $
    SegRed kspace comm reduce_lam nes (lambdaReturnType map_lam) kbody

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
  (kspace, kbody) <- prepareRedOrScan total_num_elements w map_lam arrs ispace inps
  letBind_ pat $ Op $ HostOp $
    SegScan kspace scan_lam nes (lambdaReturnType map_lam) kbody

dummyDim :: (MonadFreshNames m, MonadBinder m) =>
            Pattern Kernels
         -> m (Pattern Kernels, [(VName, SubExp)], m ())
dummyDim pat = do
  -- We add a unit-size segment on top to ensure that the result
  -- of the SegRed is an array, which we then immediately index.
  -- This is useful in the case that the value is used on the
  -- device afterwards, as this may save an expensive
  -- host-device copy (scalars are kept on the host, but arrays
  -- may be on the device).
  let addDummyDim t = t `arrayOfRow` intConst Int32 1
  pat' <- fmap addDummyDim <$> renamePattern pat
  dummy <- newVName "dummy"
  let ispace = [(dummy, intConst Int32 1)]

  return (pat', ispace,
          forM_ (zip (patternNames pat') (patternNames pat)) $ \(from, to) -> do
             from_t <- lookupType from
             letBindNames_ [to] $ BasicOp $ Index from $
               fullSlice from_t [DimFix $ intConst Int32 0])

huskedOp :: (MonadFreshNames m, HasScope Kernels m) =>
              Pattern Kernels
          -> SubExp
          -> [Type]
          -> Lambda Kernels
          -> [SubExp]
          -> [VName]
          -> (Pattern Kernels -> SubExp -> [VName] -> Binder Kernels (Stms Kernels))
          -> m (Stms Kernels)
huskedOp pat w ret_ts red_lam_fot nes arrs red = runBinder_ $ do
  red_lam_fot' <- renameLambda red_lam_fot
  hspace@(HuskSpace _ _ parts parts_elems _ _ _) <- constructHuskSpace arrs w
  let hscope = scopeOfHuskSpace hspace
      parts_names = map paramName parts
      parts_elems_v = Var parts_elems
  (Pattern pcs pes) <- renamePattern pat
  node_res <- replicateM (length pes) $ newVName "node_res"
  let (red_ts, map_ts) = splitAt (length nes) $ map patElemAttr pes
      body_pat_ts = red_ts ++ map (`setOuterSize` Var parts_elems) map_ts
      body_pat = Pattern pcs $ zipWith PatElem node_res body_pat_ts
  body_stms <- localScope hscope $ red body_pat parts_elems_v parts_names
  letBind_ pat $ Op $ Husk hspace red_lam_fot' nes ret_ts $ mkBody body_stms $ map Var node_res

huskedNonSegRed :: (MonadFreshNames m, HasScope Kernels m) =>
             Pattern Kernels
          -> SubExp
          -> Commutativity
          -> Lambda InKernel
          -> Lambda Kernels
          -> Lambda InKernel
          -> [SubExp]
          -> [VName]
          -> m (Stms Kernels)
huskedNonSegRed pat w comm red_lam_seq red_lam_fot map_lam nes arrs =
  let (red_ts, map_ts) = splitAt (length nes) $ lambdaReturnType map_lam
      ret_ts = red_ts ++ map (`arrayOfShape` Shape [w]) map_ts
  in huskedOp pat w ret_ts red_lam_fot nes arrs $
      \pat' w' arrs' -> nonSegRed pat' w' comm red_lam_seq map_lam nes arrs'

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
  (pat', ispace, read_dummy) <- dummyDim pat
  addStms =<< segRed pat' w w comm red_lam map_lam nes arrs ispace []
  read_dummy

prepareStream :: (MonadBinder m, Lore m ~ Kernels) =>
                 KernelSize
              -> [(VName, SubExp)]
              -> SubExp
              -> Commutativity
              -> Lambda InKernel
              -> [SubExp]
              -> [VName]
              -> m (KernelSpace, [Type], KernelBody InKernel)
prepareStream size ispace w comm fold_lam nes arrs = do
  let (KernelSize num_groups group_size elems_per_thread _ num_threads) = size
  let (ordering, split_ordering) =
        case comm of Commutative -> (Disorder, SplitStrided num_threads)
                     Noncommutative -> (InOrder, SplitContiguous)

  fold_lam' <- kerneliseLambda nes fold_lam

  elems_per_thread_32 <- asIntS Int32 elems_per_thread

  gtid <- newVName "gtid"
  kspace <- newKernelSpace (num_groups, group_size, num_threads, num_groups) $
            FlatThreadSpace $ ispace ++ [(gtid, num_threads)]
  kbody <- fmap (uncurry (flip (KernelBody ()))) $ runBinder $
           localScope (scopeOfKernelSpace kspace) $ do
    (chunk_red_pes, chunk_map_pes) <-
      blockedPerThread gtid w size ordering fold_lam' (length nes) arrs
    let concatReturns pe =
          ConcatReturns split_ordering w elems_per_thread_32 Nothing $ patElemName pe
    return (map (ThreadsReturn . Var . patElemName) chunk_red_pes ++
            map concatReturns chunk_map_pes)

  let (redout_ts, mapout_ts) = splitAt (length nes) $ lambdaReturnType fold_lam
      ts = redout_ts ++ map rowType mapout_ts

  return (kspace, ts, kbody)

huskedStreamRed :: (MonadFreshNames m, HasScope Kernels m) =>
                  Pattern Kernels
                -> SubExp
                -> Commutativity
                -> Lambda InKernel -> Lambda Kernels -> Lambda InKernel
                -> [SubExp] -> [VName]
                -> m (Stms Kernels)
huskedStreamRed pat w comm red_lam_seq red_lam_fot fold_lam nes arrs =
  let (red_ts, map_ts) = splitAt (length nes) $ lambdaReturnType fold_lam
      ret_ts = red_ts ++ map (`setOuterSize` w) map_ts
  in huskedOp pat w ret_ts red_lam_fot nes arrs $
      \pat' w' arrs' -> streamRed pat' w' comm red_lam_seq fold_lam nes arrs'

streamRed :: (MonadFreshNames m, HasScope Kernels m) =>
             Pattern Kernels
          -> SubExp
          -> Commutativity
          -> Lambda InKernel -> Lambda InKernel
          -> [SubExp] -> [VName]
          -> m (Stms Kernels)
streamRed pat w comm red_lam fold_lam nes arrs = runBinder_ $ do
  -- The strategy here is to rephrase the stream reduction as a
  -- non-segmented SegRed that does explicit chunking within its body.
  -- First, figure out how many threads to use for this.
  (_, size) <- blockedKernelSize =<< asIntS Int64 w

  let (redout_pes, mapout_pes) = splitAt (length nes) $ patternElements pat
  (redout_pat, ispace, read_dummy) <- dummyDim $ Pattern [] redout_pes
  let pat' = Pattern [] $ patternElements redout_pat ++ mapout_pes

  (kspace, ts, kbody) <- prepareStream size ispace w comm fold_lam nes arrs

  letBind_ pat' $ Op $ HostOp $ SegRed kspace comm red_lam nes ts kbody

  read_dummy

-- Similar to streamRed, but without the last reduction.
streamMap :: (MonadFreshNames m, HasScope Kernels m) =>
              [String] -> [PatElem Kernels] -> SubExp
           -> Commutativity -> Lambda InKernel -> [SubExp] -> [VName]
           -> m ((SubExp, [VName]), Stms Kernels)
streamMap out_desc mapout_pes w comm fold_lam nes arrs = runBinder $ do
  (_, size) <- blockedKernelSize =<< asIntS Int64 w

  (kspace, ts, kbody) <- prepareStream size [] w comm fold_lam nes arrs

  let redout_ts = take (length nes) ts

  redout_pes <- forM (zip out_desc redout_ts) $ \(desc, t) ->
    PatElem <$> newVName desc <*> pure (t `arrayOfRow` spaceNumThreads kspace)

  let pat = Pattern [] $ redout_pes ++ mapout_pes
  letBind_ pat $ Op $ HostOp $ SegMap kspace ts kbody

  return (spaceNumThreads kspace, map patElemName redout_pes)

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
  kspace <- newKernelSpace (num_groups, group_size, num_threads, num_groups) $
            FlatThreadSpace $ ispace ++ [(gtid, arr_w)]

  kbody <- fmap (uncurry (flip $ KernelBody ())) $ runBinder $
          localScope (scopeOfKernelSpace kspace) $ do
    mapM_ (addStm <=< readKernelInput) inps
    forM_ (zip (lambdaParams lam) arrs) $ \(p, arr) -> do
      arr_t <- lookupType arr
      letBindNames_ [paramName p] $
        BasicOp $ Index arr $ fullSlice arr_t [DimFix $ Var gtid]
    map ThreadsReturn <$> bodyBind (lambdaBody lam)

  letBind_ pat $ Op $ HostOp $ SegGenRed kspace ops (lambdaReturnType lam) kbody

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

createsArrays :: KernelBody InKernel -> Bool
createsArrays = getAny . execWriter . mapM_ onStm . kernelBodyStms
  where onStm stm = do
          when (any (not . primType) $ patternTypes $ stmPattern stm) $ tell $ Any True
          walkExpM walker $ stmExp stm
        walker = identityWalker { walkOnBody = mapM_ onStm . bodyStms }

mapKernelSkeleton :: (HasScope Kernels m, MonadFreshNames m) =>
                     SubExp -> SpaceStructure -> [KernelInput] -> Bool
                  -> m (KernelSpace,
                        Stms Kernels,
                        Stms InKernel)
mapKernelSkeleton w ispace inputs creates_arrays = do
  ((group_size, num_threads, num_groups, virt_groups), ksize_bnds) <- runBinder $
    -- If the kernel creates arrays internally (meaning it will
    -- require memory expansion), we want to truncate the amount of
    -- threads.  Otherwise, have at it!  This is a bit of a hack - in
    -- principle, we should make this decision later, when we have a
    -- clearer idea of what is happening inside the kernel.
    if not creates_arrays then do
      group_size <- getSize "group_size" SizeGroup
      num_groups <- letSubExp "num_groups" =<< eDivRoundingUp Int32
                    (eSubExp w) (eSubExp group_size)
      num_threads <- letSubExp "num_threads" $
        BasicOp $ BinOp (Mul Int32) num_groups group_size
      return (group_size, num_threads, num_groups, num_groups)

      else do
      (_, ksize) <- blockedKernelSize =<< asIntS Int64 w
      virt_groups <- letSubExp "virt_groups" =<< eDivRoundingUp Int32
                     (eSubExp w) (eSubExp (kernelWorkgroupSize ksize))
      return (kernelWorkgroupSize ksize, kernelNumThreads ksize,
              kernelWorkgroups ksize, virt_groups)

  read_input_bnds <- stmsFromList <$> mapM readKernelInput inputs

  let ksize = (num_groups, group_size, num_threads, virt_groups)

  space <- newKernelSpace ksize ispace
  return (space, ksize_bnds, read_input_bnds)

mapKernel :: (HasScope Kernels m, MonadFreshNames m) =>
             SubExp -> SpaceStructure -> [KernelInput]
          -> [Type] -> KernelBody InKernel
          -> m (Stms Kernels, Kernel InKernel)
mapKernel w ispace inputs rts kbody@(KernelBody () kstms krets) = do
  (space, ksize_bnds, read_input_bnds) <- mapKernelSkeleton w ispace inputs $
                                          createsArrays kbody

  let kbody' = KernelBody () (read_input_bnds <> kstms) krets
  return (ksize_bnds, Kernel (KernelDebugHints "map" []) space rts kbody')

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
                  (SubExp,SubExp,SubExp,SubExp) -> SpaceStructure -> m KernelSpace
newKernelSpace (num_groups, group_size, num_threads, virt_groups) ispace =
  KernelSpace
  <$> newVName "global_tid"
  <*> newVName "local_tid"
  <*> newVName "group_id"
  <*> pure num_threads
  <*> pure num_groups
  <*> pure group_size
  <*> pure virt_groups
  <*> pure ispace
