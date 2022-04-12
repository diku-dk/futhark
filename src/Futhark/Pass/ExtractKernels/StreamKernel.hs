{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.Pass.ExtractKernels.StreamKernel
  ( segThreadCapped,
    streamRed,
    streamMap,
  )
where

import Control.Monad
import Control.Monad.Writer
import Data.List ()
import Futhark.Analysis.PrimExp
import Futhark.IR
import Futhark.IR.GPU hiding
  ( BasicOp,
    Body,
    Exp,
    FParam,
    FunDef,
    LParam,
    Lambda,
    Pat,
    PatElem,
    Prog,
    RetType,
    Stm,
  )
import Futhark.MonadFreshNames
import Futhark.Pass.ExtractKernels.BlockedKernel
import Futhark.Pass.ExtractKernels.ToGPU
import Futhark.Tools
import Prelude hiding (quot)

data KernelSize = KernelSize
  { -- | Int64
    kernelElementsPerThread :: SubExp,
    -- | Int32
    kernelNumThreads :: SubExp
  }
  deriving (Eq, Ord, Show)

numberOfGroups ::
  (MonadBuilder m, Op (Rep m) ~ HostOp (Rep m) inner) =>
  String ->
  SubExp ->
  SubExp ->
  m (SubExp, SubExp)
numberOfGroups desc w group_size = do
  max_num_groups_key <- nameFromString . pretty <$> newVName (desc ++ "_num_groups")
  num_groups <-
    letSubExp "num_groups" $
      Op $ SizeOp $ CalcNumGroups w max_num_groups_key group_size
  num_threads <-
    letSubExp "num_threads" $
      BasicOp $ BinOp (Mul Int64 OverflowUndef) num_groups group_size
  pure (num_groups, num_threads)

blockedKernelSize ::
  (MonadBuilder m, Rep m ~ GPU) =>
  String ->
  SubExp ->
  m KernelSize
blockedKernelSize desc w = do
  group_size <- getSize (desc ++ "_group_size") SizeGroup

  (_, num_threads) <- numberOfGroups desc w group_size

  per_thread_elements <-
    letSubExp "per_thread_elements"
      =<< eBinOp (SDivUp Int64 Unsafe) (eSubExp w) (eSubExp num_threads)

  pure $ KernelSize per_thread_elements num_threads

splitArrays ::
  (MonadBuilder m, Rep m ~ GPU) =>
  VName ->
  [VName] ->
  SplitOrdering ->
  SubExp ->
  SubExp ->
  SubExp ->
  [VName] ->
  m ()
splitArrays chunk_size split_bound ordering w i elems_per_i arrs = do
  letBindNames [chunk_size] $ Op $ SizeOp $ SplitSpace ordering w i elems_per_i
  case ordering of
    SplitContiguous -> do
      offset <- letSubExp "slice_offset" $ BasicOp $ BinOp (Mul Int64 OverflowUndef) i elems_per_i
      zipWithM_ (contiguousSlice offset) split_bound arrs
    SplitStrided stride -> zipWithM_ (stridedSlice stride) split_bound arrs
  where
    contiguousSlice offset slice_name arr = do
      arr_t <- lookupType arr
      let slice = fullSlice arr_t [DimSlice offset (Var chunk_size) (constant (1 :: Int64))]
      letBindNames [slice_name] $ BasicOp $ Index arr slice

    stridedSlice stride slice_name arr = do
      arr_t <- lookupType arr
      let slice = fullSlice arr_t [DimSlice i (Var chunk_size) stride]
      letBindNames [slice_name] $ BasicOp $ Index arr slice

partitionChunkedKernelFoldParameters ::
  Int ->
  [Param dec] ->
  (VName, Param dec, [Param dec], [Param dec])
partitionChunkedKernelFoldParameters num_accs (i_param : chunk_param : params) =
  let (acc_params, arr_params) = splitAt num_accs params
   in (paramName i_param, chunk_param, acc_params, arr_params)
partitionChunkedKernelFoldParameters _ _ =
  error "partitionChunkedKernelFoldParameters: lambda takes too few parameters"

blockedPerThread ::
  (MonadBuilder m, Rep m ~ GPU) =>
  VName ->
  SubExp ->
  KernelSize ->
  StreamOrd ->
  Lambda (Rep m) ->
  Int ->
  [VName] ->
  m ([PatElem Type], [PatElem Type])
blockedPerThread thread_gtid w kernel_size ordering lam num_nonconcat arrs = do
  let (_, chunk_size, [], arr_params) =
        partitionChunkedKernelFoldParameters 0 $ lambdaParams lam

      ordering' =
        case ordering of
          InOrder -> SplitContiguous
          Disorder -> SplitStrided $ kernelNumThreads kernel_size
      red_ts = take num_nonconcat $ lambdaReturnType lam
      map_ts = map rowType $ drop num_nonconcat $ lambdaReturnType lam

  per_thread <- asIntS Int64 $ kernelElementsPerThread kernel_size
  splitArrays
    (paramName chunk_size)
    (map paramName arr_params)
    ordering'
    w
    (Var thread_gtid)
    per_thread
    arrs

  chunk_red_pes <- forM red_ts $ \red_t -> do
    pe_name <- newVName "chunk_fold_red"
    pure $ PatElem pe_name red_t
  chunk_map_pes <- forM map_ts $ \map_t -> do
    pe_name <- newVName "chunk_fold_map"
    pure $ PatElem pe_name $ map_t `arrayOfRow` Var (paramName chunk_size)

  let (chunk_red_ses, chunk_map_ses) =
        splitAt num_nonconcat $ bodyResult $ lambdaBody lam

  addStms $
    bodyStms (lambdaBody lam)
      <> stmsFromList
        [ certify cs $ Let (Pat [pe]) (defAux ()) $ BasicOp $ SubExp se
          | (pe, SubExpRes cs se) <- zip chunk_red_pes chunk_red_ses
        ]
      <> stmsFromList
        [ certify cs $ Let (Pat [pe]) (defAux ()) $ BasicOp $ SubExp se
          | (pe, SubExpRes cs se) <- zip chunk_map_pes chunk_map_ses
        ]

  pure (chunk_red_pes, chunk_map_pes)

-- | Given a chunked fold lambda that takes its initial accumulator
-- value as parameters, bind those parameters to the neutral element
-- instead.
kerneliseLambda ::
  MonadFreshNames m =>
  [SubExp] ->
  Lambda GPU ->
  m (Lambda GPU)
kerneliseLambda nes lam = do
  thread_index_param <- newParam "thread_index" $ Prim int64
  let (fold_chunk_param, fold_acc_params, fold_inp_params) =
        partitionChunkedFoldParameters (length nes) $ lambdaParams lam

      mkAccInit p (Var v)
        | not $ primType $ paramType p =
            mkLet [paramIdent p] $ BasicOp $ Copy v
      mkAccInit p x = mkLet [paramIdent p] $ BasicOp $ SubExp x
      acc_init_stms = stmsFromList $ zipWith mkAccInit fold_acc_params nes
  pure
    lam
      { lambdaBody = insertStms acc_init_stms $ lambdaBody lam,
        lambdaParams = thread_index_param : fold_chunk_param : fold_inp_params
      }

prepareStream ::
  (MonadBuilder m, Rep m ~ GPU) =>
  KernelSize ->
  [(VName, SubExp)] ->
  SubExp ->
  Commutativity ->
  Lambda GPU ->
  [SubExp] ->
  [VName] ->
  m (SubExp, SegSpace, [Type], KernelBody GPU)
prepareStream size ispace w comm fold_lam nes arrs = do
  let (KernelSize elems_per_thread num_threads) = size
  let (ordering, split_ordering) =
        case comm of
          Commutative -> (Disorder, SplitStrided num_threads)
          Noncommutative -> (InOrder, SplitContiguous)

  fold_lam' <- kerneliseLambda nes fold_lam

  gtid <- newVName "gtid"
  space <- mkSegSpace $ ispace ++ [(gtid, num_threads)]
  kbody <- fmap (uncurry (flip (KernelBody ()))) $
    runBuilder $
      localScope (scopeOfSegSpace space) $ do
        (chunk_red_pes, chunk_map_pes) <-
          blockedPerThread gtid w size ordering fold_lam' (length nes) arrs
        let concatReturns pe =
              ConcatReturns mempty split_ordering w elems_per_thread $ patElemName pe
        pure
          ( map (Returns ResultMaySimplify mempty . Var . patElemName) chunk_red_pes
              ++ map concatReturns chunk_map_pes
          )

  let (redout_ts, mapout_ts) = splitAt (length nes) $ lambdaReturnType fold_lam
      ts = redout_ts ++ map rowType mapout_ts

  pure (num_threads, space, ts, kbody)

streamRed ::
  (MonadFreshNames m, HasScope GPU m) =>
  MkSegLevel GPU m ->
  Pat Type ->
  SubExp ->
  Commutativity ->
  Lambda GPU ->
  Lambda GPU ->
  [SubExp] ->
  [VName] ->
  m (Stms GPU)
streamRed mk_lvl pat w comm red_lam fold_lam nes arrs = runBuilderT'_ $ do
  -- The strategy here is to rephrase the stream reduction as a
  -- non-segmented SegRed that does explicit chunking within its body.
  -- First, figure out how many threads to use for this.
  size <- blockedKernelSize "stream_red" w

  let (redout_pes, mapout_pes) = splitAt (length nes) $ patElems pat
  (redout_pat, ispace, read_dummy) <- dummyDim $ Pat redout_pes
  let pat' = Pat $ patElems redout_pat ++ mapout_pes

  (_, kspace, ts, kbody) <- prepareStream size ispace w comm fold_lam nes arrs

  lvl <- mk_lvl [w] "stream_red" $ NoRecommendation SegNoVirt
  letBind pat' . Op . SegOp $
    SegRed lvl kspace [SegBinOp comm red_lam nes mempty] ts kbody

  read_dummy

-- Similar to streamRed, but without the last reduction.
streamMap ::
  (MonadFreshNames m, HasScope GPU m) =>
  MkSegLevel GPU m ->
  [String] ->
  [PatElem Type] ->
  SubExp ->
  Commutativity ->
  Lambda GPU ->
  [SubExp] ->
  [VName] ->
  m ((SubExp, [VName]), Stms GPU)
streamMap mk_lvl out_desc mapout_pes w comm fold_lam nes arrs = runBuilderT' $ do
  size <- blockedKernelSize "stream_map" w

  (threads, kspace, ts, kbody) <- prepareStream size [] w comm fold_lam nes arrs

  let redout_ts = take (length nes) ts

  redout_pes <- forM (zip out_desc redout_ts) $ \(desc, t) ->
    PatElem <$> newVName desc <*> pure (t `arrayOfRow` threads)

  let pat = Pat $ redout_pes ++ mapout_pes
  lvl <- mk_lvl [w] "stream_map" $ NoRecommendation SegNoVirt
  letBind pat $ Op $ SegOp $ SegMap lvl kspace ts kbody

  pure (threads, map patElemName redout_pes)

-- | Like 'segThread', but cap the thread count to the input size.
-- This is more efficient for small kernels, e.g. summing a small
-- array.
segThreadCapped :: MonadFreshNames m => MkSegLevel GPU m
segThreadCapped ws desc r = do
  w <-
    letSubExp "nest_size"
      =<< foldBinOp (Mul Int64 OverflowUndef) (intConst Int64 1) ws
  group_size <- getSize (desc ++ "_group_size") SizeGroup

  case r of
    ManyThreads -> do
      usable_groups <-
        letSubExp "segmap_usable_groups"
          =<< eBinOp
            (SDivUp Int64 Unsafe)
            (eSubExp w)
            (eSubExp =<< asIntS Int64 group_size)
      pure $ SegThread (Count usable_groups) (Count group_size) SegNoVirt
    NoRecommendation v -> do
      (num_groups, _) <- numberOfGroups desc w group_size
      pure $ SegThread (Count num_groups) (Count group_size) v
