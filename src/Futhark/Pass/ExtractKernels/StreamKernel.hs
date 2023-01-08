{-# LANGUAGE TypeFamilies #-}

module Futhark.Pass.ExtractKernels.StreamKernel
  ( segThreadCapped,
  )
where

import Control.Monad
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
  (MonadBuilder m, Op (Rep m) ~ HostOp inner (Rep m)) =>
  String ->
  SubExp ->
  SubExp ->
  m (SubExp, SubExp)
numberOfGroups desc w group_size = do
  max_num_groups_key <- nameFromString . prettyString <$> newVName (desc ++ "_num_groups")
  num_groups <-
    letSubExp "num_groups" $
      Op $
        SizeOp $
          CalcNumGroups w max_num_groups_key group_size
  num_threads <-
    letSubExp "num_threads" $
      BasicOp $
        BinOp (Mul Int64 OverflowUndef) num_groups group_size
  pure (num_groups, num_threads)

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
      let grid = KernelGrid (Count usable_groups) (Count group_size)
      pure $ SegThread SegNoVirt (Just grid)
    NoRecommendation v -> do
      (num_groups, _) <- numberOfGroups desc w group_size
      let grid = KernelGrid (Count num_groups) (Count group_size)
      pure $ SegThread v (Just grid)
