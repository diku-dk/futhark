{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.Pass.ExplicitAllocations.SegOp
  ( allocInKernelBody,
    allocInBinOpLambda,
  )
where

import Futhark.IR.KernelsMem
import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.Pass.ExplicitAllocations

instance SizeSubst (SegOp lvl lore) where
  opSizeSubst _ _ = mempty

allocInKernelBody ::
  Allocable fromlore tolore =>
  KernelBody fromlore ->
  AllocM fromlore tolore (KernelBody tolore)
allocInKernelBody (KernelBody () stms res) =
  allocInStms stms $ \stms' -> return $ KernelBody () stms' res

allocInLambda ::
  Allocable fromlore tolore =>
  [LParam tolore] ->
  Body fromlore ->
  [Type] ->
  AllocM fromlore tolore (Lambda tolore)
allocInLambda params body rettype = do
  body' <- localScope (scopeOfLParams params) $
    allocInStms (bodyStms body) $ \bnds' ->
      return $ Body () bnds' $ bodyResult body
  return $ Lambda params body' rettype

allocInBinOpParams ::
  Allocable fromlore tolore =>
  SubExp ->
  TPrimExp Int64 VName ->
  TPrimExp Int64 VName ->
  [LParam fromlore] ->
  [LParam fromlore] ->
  AllocM fromlore tolore ([LParam tolore], [LParam tolore])
allocInBinOpParams num_threads my_id other_id xs ys = unzip <$> zipWithM alloc xs ys
  where
    alloc x y =
      case paramType x of
        Array (ElemPrim pt) shape u -> do
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
          return
            ( x {paramDec = MemArray pt shape u $ ArrayIn mem ixfun_x},
              y {paramDec = MemArray pt shape u $ ArrayIn mem ixfun_y}
            )
        Prim bt ->
          return
            ( x {paramDec = MemPrim bt},
              y {paramDec = MemPrim bt}
            )
        Mem space ->
          return
            ( x {paramDec = MemMem space},
              y {paramDec = MemMem space}
            )
        -- This next two cases will never happen.

        Array (ElemAcc arrs) shape _ ->
          return
            ( x {paramDec = MemAcc arrs shape},
              y {paramDec = MemAcc arrs shape}
            )
        Acc arrs ->
          return
            ( x {paramDec = MemAcc arrs mempty},
              y {paramDec = MemAcc arrs mempty}
            )

allocInBinOpLambda ::
  Allocable fromlore tolore =>
  SubExp ->
  SegSpace ->
  Lambda fromlore ->
  AllocM fromlore tolore (Lambda tolore)
allocInBinOpLambda num_threads (SegSpace flat _) lam = do
  let (acc_params, arr_params) =
        splitAt (length (lambdaParams lam) `div` 2) $ lambdaParams lam
      index_x = TPrimExp $ LeafExp flat int64
      index_y = index_x + pe64 num_threads
  (acc_params', arr_params') <-
    allocInBinOpParams num_threads index_x index_y acc_params arr_params

  allocInLambda
    (acc_params' ++ arr_params')
    (lambdaBody lam)
    (lambdaReturnType lam)
