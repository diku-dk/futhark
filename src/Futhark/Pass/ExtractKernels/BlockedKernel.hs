{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.Pass.ExtractKernels.BlockedKernel
       ( MkSegLevel
       , ThreadRecommendation(..)
       , segRed
       , nonSegRed
       , segScan
       , segHist
       , segMap

       , streamRed
       , streamMap

       , mapKernel
       , KernelInput(..)
       , readKernelInput

       , soacsLambdaToKernels
       , soacsStmToKernels
       , scopeForKernels
       , scopeForSOACs

       , getSize
       , segThread
       , segThreadCapped
       , mkSegSpace
       )
       where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Identity
import Data.List

import Prelude hiding (quot)

import Futhark.Analysis.PrimExp
import Futhark.Analysis.Rephrase
import Futhark.Representation.AST
import Futhark.Representation.SOACS (SOACS)
import qualified Futhark.Representation.SOACS.SOAC as SOAC
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
  letSubExp desc $ Op $ SizeOp $ GetSize size_key size_class

numberOfGroups :: (MonadBinder m, Op (Lore m) ~ HostOp (Lore m) inner) =>
                  String -> SubExp -> SubExp -> m (SubExp, SubExp)
numberOfGroups desc w64 group_size = do
  max_num_groups_key <- nameFromString . pretty <$> newVName (desc ++ "_num_groups")
  num_groups <- letSubExp "num_groups" $
                Op $ SizeOp $ CalcNumGroups w64 max_num_groups_key group_size
  num_threads <- letSubExp "num_threads" $ BasicOp $ BinOp (Mul Int32) num_groups group_size
  return (num_groups, num_threads)

segThread :: (MonadBinder m, Op (Lore m) ~ HostOp (Lore m) inner) =>
             String -> m SegLevel
segThread desc =
  SegThread
    <$> (Count <$> getSize (desc ++ "_num_groups") SizeNumGroups)
    <*> (Count <$> getSize (desc ++ "_group_size") SizeGroup)
    <*> pure SegVirt

data ThreadRecommendation = ManyThreads | NoRecommendation SegVirt

type MkSegLevel m =
  [SubExp] -> String -> ThreadRecommendation -> BinderT Kernels m SegLevel

-- | Like 'segThread', but cap the thread count to the input size.
-- This is more efficient for small kernels, e.g. summing a small
-- array.
segThreadCapped :: MonadFreshNames m => MkSegLevel m
segThreadCapped ws desc r = do
  w64 <- letSubExp "nest_size" =<<
         foldBinOp (Mul Int64) (intConst Int64 1) =<<
         mapM (asIntS Int64) ws
  group_size <- getSize (desc ++ "_group_size") SizeGroup

  case r of
    ManyThreads -> do
      usable_groups <- letSubExp "segmap_usable_groups" .
                       BasicOp . ConvOp (SExt Int64 Int32) =<<
                       letSubExp "segmap_usable_groups_64" =<<
                       eDivRoundingUp Int64 (eSubExp w64)
                       (eSubExp =<< asIntS Int64 group_size)
      return $ SegThread (Count usable_groups) (Count group_size) SegNoVirt
    NoRecommendation v -> do
      (num_groups, _) <- numberOfGroups desc w64 group_size
      return $ SegThread (Count num_groups) (Count group_size) v

mkSegSpace :: MonadFreshNames m => [(VName, SubExp)] -> m SegSpace
mkSegSpace dims = SegSpace <$> newVName "phys_tid" <*> pure dims

-- | Given a chunked fold lambda that takes its initial accumulator
-- value as parameters, bind those parameters to the neutral element
-- instead.
kerneliseLambda :: MonadFreshNames m =>
                   [SubExp] -> Lambda Kernels -> m (Lambda Kernels)
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
                    SubExp
                 -> Lambda Kernels
                 -> [VName] -> [(VName, SubExp)] -> [KernelInput]
                 -> m (SegSpace, KernelBody Kernels)
prepareRedOrScan w map_lam arrs ispace inps = do
  gtid <- newVName "gtid"
  space <- mkSegSpace $ ispace ++ [(gtid, w)]
  kbody <- fmap (uncurry (flip (KernelBody ()))) $ runBinder $
           localScope (scopeOfSegSpace space) $ do
    mapM_ readKernelInput inps
    forM_ (zip (lambdaParams map_lam) arrs) $ \(p, arr) -> do
      arr_t <- lookupType arr
      letBindNames_ [paramName p] $
        BasicOp $ Index arr $ fullSlice arr_t [DimFix $ Var gtid]
    map Returns <$> bodyBind (lambdaBody map_lam)

  return (space, kbody)

segRed :: (MonadFreshNames m, HasScope Kernels m) =>
          SegLevel
       -> Pattern Kernels
       -> SubExp -- segment size
       -> [SegRedOp Kernels]
       -> Lambda Kernels
       -> [VName]
       -> [(VName, SubExp)] -- ispace = pair of (gtid, size) for the maps on "top" of this reduction
       -> [KernelInput]     -- inps = inputs that can be looked up by using the gtids from ispace
       -> m (Stms Kernels)
segRed lvl pat w ops map_lam arrs ispace inps = runBinder_ $ do
  (kspace, kbody) <- prepareRedOrScan w map_lam arrs ispace inps
  letBind_ pat $ Op $ SegOp $
    SegRed lvl kspace ops (lambdaReturnType map_lam) kbody

segScan :: (MonadFreshNames m, HasScope Kernels m) =>
           SegLevel
        -> Pattern Kernels
        -> SubExp -- segment size
        -> Lambda Kernels -> Lambda Kernels
        -> [SubExp] -> [VName]
        -> [(VName, SubExp)] -- ispace = pair of (gtid, size) for the maps on "top" of this scan
        -> [KernelInput]     -- inps = inputs that can be looked up by using the gtids from ispace
        -> m (Stms Kernels)
segScan lvl pat w scan_lam map_lam nes arrs ispace inps = runBinder_ $ do
  (kspace, kbody) <- prepareRedOrScan w map_lam arrs ispace inps
  letBind_ pat $ Op $ SegOp $
    SegScan lvl kspace scan_lam nes (lambdaReturnType map_lam) kbody

segMap :: (MonadFreshNames m, HasScope Kernels m) =>
          SegLevel
       -> Pattern Kernels
       -> SubExp -- segment size
       -> Lambda Kernels
       -> [VName]
       -> [(VName, SubExp)] -- ispace = pair of (gtid, size) for the maps on "top" of this map
       -> [KernelInput]     -- inps = inputs that can be looked up by using the gtids from ispace
       -> m (Stms Kernels)
segMap lvl pat w map_lam arrs ispace inps = runBinder_ $ do
  (kspace, kbody) <- prepareRedOrScan w map_lam arrs ispace inps
  letBind_ pat $ Op $ SegOp $
    SegMap lvl kspace (lambdaReturnType map_lam) kbody

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

nonSegRed :: (MonadFreshNames m, HasScope Kernels m) =>
             SegLevel
          -> Pattern Kernels
          -> SubExp
          -> [SegRedOp Kernels]
          -> Lambda Kernels
          -> [VName]
          -> m (Stms Kernels)
nonSegRed lvl pat w ops map_lam arrs = runBinder_ $ do
  (pat', ispace, read_dummy) <- dummyDim pat
  addStms =<< segRed lvl pat' w ops map_lam arrs ispace []
  read_dummy

prepareStream :: (MonadBinder m, Lore m ~ Kernels) =>
                 KernelSize
              -> [(VName, SubExp)]
              -> SubExp
              -> Commutativity
              -> Lambda Kernels
              -> [SubExp]
              -> [VName]
              -> m (SubExp, SegSpace, [Type], KernelBody Kernels)
prepareStream size ispace w comm fold_lam nes arrs = do
  let (KernelSize elems_per_thread num_threads) = size
  let (ordering, split_ordering) =
        case comm of Commutative -> (Disorder, SplitStrided num_threads)
                     Noncommutative -> (InOrder, SplitContiguous)

  fold_lam' <- kerneliseLambda nes fold_lam

  elems_per_thread_32 <- asIntS Int32 elems_per_thread

  gtid <- newVName "gtid"
  space <- mkSegSpace $ ispace ++ [(gtid, num_threads)]
  kbody <- fmap (uncurry (flip (KernelBody ()))) $ runBinder $
           localScope (scopeOfSegSpace space) $ do
    (chunk_red_pes, chunk_map_pes) <-
      blockedPerThread gtid w size ordering fold_lam' (length nes) arrs
    let concatReturns pe =
          ConcatReturns split_ordering w elems_per_thread_32 $ patElemName pe
    return (map (Returns . Var . patElemName) chunk_red_pes ++
            map concatReturns chunk_map_pes)

  let (redout_ts, mapout_ts) = splitAt (length nes) $ lambdaReturnType fold_lam
      ts = redout_ts ++ map rowType mapout_ts

  return (num_threads, space, ts, kbody)

streamRed :: (MonadFreshNames m, HasScope Kernels m) =>
             Pattern Kernels
          -> SubExp
          -> Commutativity
          -> Lambda Kernels -> Lambda Kernels
          -> [SubExp] -> [VName]
          -> m (Stms Kernels)
streamRed pat w comm red_lam fold_lam nes arrs = runBinder_ $ do
  -- The strategy here is to rephrase the stream reduction as a
  -- non-segmented SegRed that does explicit chunking within its body.
  -- First, figure out how many threads to use for this.
  size <- blockedKernelSize "stream_red" w

  let (redout_pes, mapout_pes) = splitAt (length nes) $ patternElements pat
  (redout_pat, ispace, read_dummy) <- dummyDim $ Pattern [] redout_pes
  let pat' = Pattern [] $ patternElements redout_pat ++ mapout_pes

  (_, kspace, ts, kbody) <- prepareStream size ispace w comm fold_lam nes arrs

  lvl <- segThreadCapped [w] "stream_red" $ NoRecommendation SegNoVirt
  letBind_ pat' $ Op $ SegOp $ SegRed lvl kspace
    [SegRedOp comm red_lam nes mempty] ts kbody

  read_dummy

-- Similar to streamRed, but without the last reduction.
streamMap :: (MonadFreshNames m, HasScope Kernels m) =>
              [String] -> [PatElem Kernels] -> SubExp
           -> Commutativity -> Lambda Kernels -> [SubExp] -> [VName]
           -> m ((SubExp, [VName]), Stms Kernels)
streamMap out_desc mapout_pes w comm fold_lam nes arrs = runBinder $ do
  size <- blockedKernelSize "stream_map" w

  (threads, kspace, ts, kbody) <- prepareStream size [] w comm fold_lam nes arrs

  let redout_ts = take (length nes) ts

  redout_pes <- forM (zip out_desc redout_ts) $ \(desc, t) ->
    PatElem <$> newVName desc <*> pure (t `arrayOfRow` threads)

  let pat = Pattern [] $ redout_pes ++ mapout_pes
  lvl <- segThreadCapped [w] "stream_map" $ NoRecommendation SegNoVirt
  letBind_ pat $ Op $ SegOp $ SegMap lvl kspace ts kbody

  return (threads, map patElemName redout_pes)

segHist :: (MonadFreshNames m, HasScope Kernels m) =>
             SegLevel
          -> Pattern Kernels
          -> SubExp
          -> [(VName,SubExp)] -- ^ Segment indexes and sizes.
          -> [KernelInput]
          -> [HistOp Kernels]
          -> Lambda Kernels -> [VName]
          -> m (Stms Kernels)
segHist lvl pat arr_w ispace inps ops lam arrs = runBinder_ $ do
  gtid <- newVName "gtid"
  space <- mkSegSpace $ ispace ++ [(gtid, arr_w)]

  kbody <- fmap (uncurry (flip $ KernelBody ())) $ runBinder $
           localScope (scopeOfSegSpace space) $ do
    mapM_ readKernelInput inps
    forM_ (zip (lambdaParams lam) arrs) $ \(p, arr) -> do
      arr_t <- lookupType arr
      letBindNames_ [paramName p] $
        BasicOp $ Index arr $ fullSlice arr_t [DimFix $ Var gtid]
    map Returns <$> bodyBind (lambdaBody lam)

  letBind_ pat $ Op $ SegOp $ SegHist lvl space ops (lambdaReturnType lam) kbody

blockedPerThread :: (MonadBinder m, Lore m ~ Kernels) =>
                    VName -> SubExp -> KernelSize -> StreamOrd -> Lambda Kernels
                 -> Int -> [VName]
                 -> m ([PatElem Kernels], [PatElem Kernels])
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

splitArrays :: (MonadBinder m, Lore m ~ Kernels) =>
               VName -> [VName]
            -> SplitOrdering -> SubExp -> SubExp -> SubExp -> [VName]
            -> m ()
splitArrays chunk_size split_bound ordering w i elems_per_i arrs = do
  letBindNames_ [chunk_size] $ Op $ SizeOp $ SplitSpace ordering w i elems_per_i
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

data KernelSize = KernelSize { kernelElementsPerThread :: SubExp
                               -- ^ Int64
                             , kernelNumThreads :: SubExp
                               -- ^ Int32
                             }
                deriving (Eq, Ord, Show)

blockedKernelSize :: (MonadBinder m, Lore m ~ Kernels) =>
                     String -> SubExp -> m KernelSize
blockedKernelSize desc w = do
  group_size <- getSize (desc ++ "_group_size") SizeGroup

  w64 <- letSubExp "w64" $ BasicOp $ ConvOp (SExt Int32 Int64) w
  (_, num_threads) <- numberOfGroups desc w64 group_size

  per_thread_elements <-
    letSubExp "per_thread_elements" =<<
    eDivRoundingUp Int64 (eSubExp w64) (toExp =<< asIntS Int64 num_threads)

  return $ KernelSize per_thread_elements num_threads

mapKernelSkeleton :: (HasScope Kernels m, MonadFreshNames m) =>
                     [(VName, SubExp)] -> [KernelInput]
                  -> m (SegSpace, Stms Kernels)
mapKernelSkeleton ispace inputs = do
  read_input_bnds <- runBinder_ $ mapM readKernelInput inputs

  space <- mkSegSpace ispace
  return (space, read_input_bnds)

mapKernel :: (HasScope Kernels m, MonadFreshNames m) =>
             MkSegLevel m
          -> [(VName, SubExp)] -> [KernelInput]
          -> [Type] -> KernelBody Kernels
          -> m (SegOp Kernels, Stms Kernels)
mapKernel mk_lvl ispace inputs rts (KernelBody () kstms krets) = runBinderT' $ do
  (space, read_input_stms) <- mapKernelSkeleton ispace inputs

  let kbody' = KernelBody () (read_input_stms <> kstms) krets

  -- If the kernel creates arrays (meaning it will require memory
  -- expansion), we want to truncate the amount of threads.
  -- Otherwise, have at it!  This is a bit of a hack - in principle,
  -- we should make this decision later, when we have a clearer idea
  -- of what is happening inside the kernel.
  let r = if all primType rts then ManyThreads else NoRecommendation SegVirt

  lvl <- mk_lvl (map snd ispace) "segmap" r

  return $ SegMap lvl space rts kbody'

data KernelInput = KernelInput { kernelInputName :: VName
                               , kernelInputType :: Type
                               , kernelInputArray :: VName
                               , kernelInputIndices :: [SubExp]
                               }
                 deriving (Show)

readKernelInput :: (MonadBinder m, Lore m ~ Kernels) =>
                   KernelInput -> m ()
readKernelInput inp = do
  let pe = PatElem (kernelInputName inp) $ kernelInputType inp
  arr_t <- lookupType $ kernelInputArray inp
  letBind_ (Pattern [] [pe]) $
    BasicOp $ Index (kernelInputArray inp) $
    fullSlice arr_t $ map DimFix $ kernelInputIndices inp

injectSOACS :: (Monad m,
                SameScope from to,
                ExpAttr from ~ ExpAttr to,
                BodyAttr from ~ BodyAttr to,
                RetType from ~ RetType to,
                BranchType from ~ BranchType to,
                Op from ~ SOAC from) =>
               (SOAC to -> Op to) -> Rephraser m from to
injectSOACS f = Rephraser { rephraseExpLore = return
                          , rephraseBodyLore = return
                          , rephraseLetBoundLore = return
                          , rephraseFParamLore = return
                          , rephraseLParamLore = return
                          , rephraseOp = fmap f . onSOAC
                          , rephraseRetType = return
                          , rephraseBranchType = return
                          }
  where onSOAC = SOAC.mapSOACM mapper
        mapper = SOAC.SOACMapper { SOAC.mapOnSOACSubExp = return
                                 , SOAC.mapOnSOACVName = return
                                 , SOAC.mapOnSOACLambda = rephraseLambda $ injectSOACS f
                                 }

soacsStmToKernels :: Stm SOACS -> Stm Kernels
soacsStmToKernels = runIdentity . rephraseStm (injectSOACS OtherOp)

soacsLambdaToKernels :: Lambda SOACS -> Lambda Kernels
soacsLambdaToKernels = runIdentity . rephraseLambda (injectSOACS OtherOp)

scopeForSOACs :: Scope Kernels -> Scope SOACS
scopeForSOACs = castScope

scopeForKernels :: Scope SOACS -> Scope Kernels
scopeForKernels = castScope
