{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.Pass.ExtractKernels.StreamKernel
  ( segThreadCapped
  , streamRed
  , streamMap
  )
  where

import Control.Monad
import Control.Monad.Writer
import Data.List ()

import Prelude hiding (quot)

import Futhark.Analysis.PrimExp
import Futhark.Representation.AST
import Futhark.Representation.Kernels
       hiding (Prog, Body, Stm, Pattern, PatElem,
               BasicOp, Exp, Lambda, FunDef, FParam, LParam, RetType)
import Futhark.Pass.ExtractKernels.BlockedKernel
import Futhark.Pass.ExtractKernels.ToKernels
import Futhark.MonadFreshNames
import Futhark.Tools

data KernelSize = KernelSize { kernelElementsPerThread :: SubExp
                               -- ^ Int64
                             , kernelNumThreads :: SubExp
                               -- ^ Int32
                             }
                deriving (Eq, Ord, Show)

numberOfGroups :: (MonadBinder m, Op (Lore m) ~ HostOp (Lore m) inner) =>
                  String -> SubExp -> SubExp -> m (SubExp, SubExp)
numberOfGroups desc w64 group_size = do
  max_num_groups_key <- nameFromString . pretty <$> newVName (desc ++ "_num_groups")
  num_groups <- letSubExp "num_groups" $
                Op $ SizeOp $ CalcNumGroups w64 max_num_groups_key group_size
  num_threads <- letSubExp "num_threads" $ BasicOp $ BinOp (Mul Int32) num_groups group_size
  return (num_groups, num_threads)

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


blockedPerThread :: (MonadBinder m, Lore m ~ Kernels) =>
                    VName -> SubExp -> KernelSize -> StreamOrd -> Lambda (Lore m)
                 -> Int -> [VName]
                 -> m ([PatElemT Type], [PatElemT Type])
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
    return (map (Returns ResultMaySimplify . Var . patElemName) chunk_red_pes ++
            map concatReturns chunk_map_pes)

  let (redout_ts, mapout_ts) = splitAt (length nes) $ lambdaReturnType fold_lam
      ts = redout_ts ++ map rowType mapout_ts

  return (num_threads, space, ts, kbody)

streamRed :: (MonadFreshNames m, HasScope Kernels m) =>
             MkSegLevel SegLevel Kernels m
          -> Pattern Kernels
          -> SubExp
          -> Commutativity
          -> Lambda Kernels -> Lambda Kernels
          -> [SubExp] -> [VName]
          -> m (Stms Kernels)
streamRed mk_lvl pat w comm red_lam fold_lam nes arrs = runBinderT'_ $ do
  -- The strategy here is to rephrase the stream reduction as a
  -- non-segmented SegRed that does explicit chunking within its body.
  -- First, figure out how many threads to use for this.
  size <- blockedKernelSize "stream_red" w

  let (redout_pes, mapout_pes) = splitAt (length nes) $ patternElements pat
  (redout_pat, ispace, read_dummy) <- dummyDim $ Pattern [] redout_pes
  let pat' = Pattern [] $ patternElements redout_pat ++ mapout_pes

  (_, kspace, ts, kbody) <- prepareStream size ispace w comm fold_lam nes arrs

  lvl <- mk_lvl [w] "stream_red" $ NoRecommendation SegNoVirt
  letBind_ pat' $ Op $ SegOp $ SegRed lvl kspace
    [SegRedOp comm red_lam nes mempty] ts kbody

  read_dummy

-- Similar to streamRed, but without the last reduction.
streamMap :: (MonadFreshNames m, HasScope Kernels m) =>
              MkSegLevel SegLevel Kernels m
          -> [String] -> [PatElem Kernels] -> SubExp
           -> Commutativity -> Lambda Kernels -> [SubExp] -> [VName]
           -> m ((SubExp, [VName]), Stms Kernels)
streamMap mk_lvl out_desc mapout_pes w comm fold_lam nes arrs = runBinderT' $ do
  size <- blockedKernelSize "stream_map" w

  (threads, kspace, ts, kbody) <- prepareStream size [] w comm fold_lam nes arrs

  let redout_ts = take (length nes) ts

  redout_pes <- forM (zip out_desc redout_ts) $ \(desc, t) ->
    PatElem <$> newVName desc <*> pure (t `arrayOfRow` threads)

  let pat = Pattern [] $ redout_pes ++ mapout_pes
  lvl <- mk_lvl [w] "stream_map" $ NoRecommendation SegNoVirt
  letBind_ pat $ Op $ SegOp $ SegMap lvl kspace ts kbody

  return (threads, map patElemName redout_pes)

-- | Like 'segThread', but cap the thread count to the input size.
-- This is more efficient for small kernels, e.g. summing a small
-- array.
segThreadCapped :: MonadFreshNames m => MkSegLevel SegLevel Kernels m
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
