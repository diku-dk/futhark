{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.Pass.ExplicitAllocations.SegOp
  ( allocInKernelBody,
    allocInBinOpLambda,
  )
where

import Futhark.IR.GPUMem
import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.Pass.ExplicitAllocations

instance SizeSubst (SegOp lvl rep) where
  opSizeSubst _ _ = mempty

allocInKernelBody ::
  Allocable fromrep torep inner =>
  KernelBody fromrep ->
  AllocM fromrep torep (KernelBody torep)
allocInKernelBody (KernelBody () stms res) =
  uncurry (flip (KernelBody ()))
    <$> collectStms (allocInStms stms (pure res))

allocInLambda ::
  Allocable fromrep torep inner =>
  [LParam torep] ->
  Body fromrep ->
  AllocM fromrep torep (Lambda torep)
allocInLambda params body =
  mkLambda params . allocInStms (bodyStms body) $
    pure $ bodyResult body

allocInBinOpParams ::
  Allocable fromrep torep inner =>
  SubExp ->
  TPrimExp Int64 VName ->
  TPrimExp Int64 VName ->
  [LParam fromrep] ->
  [LParam fromrep] ->
  AllocM fromrep torep ([LParam torep], [LParam torep])
allocInBinOpParams num_threads my_id other_id xs ys = unzip <$> zipWithM alloc xs ys
  where
    alloc x y =
      case paramType x of
        Array pt shape u -> do
          twice_num_threads <-
            letSubExp "twice_num_threads" $
              BasicOp $ BinOp (Mul Int64 OverflowUndef) num_threads $ intConst Int64 2
          let t = paramType x `arrayOfRow` twice_num_threads
          mem <- allocForArray t DefaultSpace
          -- XXX: this iota ixfun is a bit inefficient; leading to
          -- uncoalesced access.
          let base_dims = map pe64 $ arrayDims t
              ixfun_base = IxFun.iota base_dims
              ixfun_x =
                IxFun.slice ixfun_base $
                  fullSliceNum base_dims [DimFix my_id]
              ixfun_y =
                IxFun.slice ixfun_base $
                  fullSliceNum base_dims [DimFix other_id]
          pure
            ( x {paramDec = MemArray pt shape u $ ArrayIn mem ixfun_x},
              y {paramDec = MemArray pt shape u $ ArrayIn mem ixfun_y}
            )
        Prim bt ->
          pure
            ( x {paramDec = MemPrim bt},
              y {paramDec = MemPrim bt}
            )
        Mem space ->
          pure
            ( x {paramDec = MemMem space},
              y {paramDec = MemMem space}
            )
        -- This next case will never happen.
        Acc acc ispace ts u ->
          pure
            ( x {paramDec = MemAcc acc ispace ts u},
              y {paramDec = MemAcc acc ispace ts u}
            )

allocInBinOpLambda ::
  Allocable fromrep torep inner =>
  SubExp ->
  SegSpace ->
  Lambda fromrep ->
  AllocM fromrep torep (Lambda torep)
allocInBinOpLambda num_threads (SegSpace flat _) lam = do
  let (acc_params, arr_params) =
        splitAt (length (lambdaParams lam) `div` 2) $ lambdaParams lam
      index_x = TPrimExp $ LeafExp flat int64
      index_y = index_x + pe64 num_threads
  (acc_params', arr_params') <-
    allocInBinOpParams num_threads index_x index_y acc_params arr_params

  allocInLambda (acc_params' ++ arr_params') (lambdaBody lam)
