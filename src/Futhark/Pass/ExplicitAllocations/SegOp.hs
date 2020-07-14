{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.Pass.ExplicitAllocations.SegOp
       ( allocInKernelBody
       , allocInBinOpLambda
       )
where

import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.IR.KernelsMem
import Futhark.Pass.ExplicitAllocations

allocInKernelBody :: Allocable fromlore tolore =>
                     KernelBody fromlore
                  -> AllocM fromlore tolore (KernelBody tolore)
allocInKernelBody (KernelBody () stms res) =
  allocInStms stms $ \stms' -> return $ KernelBody () stms' res

allocInLambda :: Allocable fromlore tolore =>
                 [LParam tolore] -> Body fromlore -> [Type]
              -> AllocM fromlore tolore (Lambda tolore)
allocInLambda params body rettype = do
  body' <- localScope (scopeOfLParams params) $
           allocInStms (bodyStms body) $ \bnds' ->
           return $ Body () bnds' $ bodyResult body
  return $ Lambda params body' rettype

allocInBinOpParams :: Allocable fromlore tolore =>
                      SubExp
                   -> PrimExp VName -> PrimExp VName
                   -> [LParam fromlore]
                   -> [LParam fromlore]
                   -> AllocM fromlore tolore ([LParam tolore], [LParam tolore])
allocInBinOpParams num_threads my_id other_id xs ys = unzip <$> zipWithM alloc xs ys
  where alloc x y =
          case paramType x of
            Array (ElemPrim pt) shape u -> do
              twice_num_threads <-
                letSubExp "twice_num_threads" $
                BasicOp $ BinOp (Mul Int32 OverflowUndef) num_threads $ intConst Int32 2
              let t = paramType x `arrayOfRow` twice_num_threads
              mem <- allocForArray t DefaultSpace
              -- XXX: this iota ixfun is a bit inefficient; leading to
              -- uncoalesced access.
              let base_dims = map (primExpFromSubExp int32) (arrayDims t)
                  ixfun_base = IxFun.iota base_dims
                  ixfun_x = IxFun.slice ixfun_base $
                            fullSliceNum base_dims [DimFix my_id]
                  ixfun_y = IxFun.slice ixfun_base $
                            fullSliceNum base_dims [DimFix other_id]
              return (x { paramDec = MemArray pt shape u $ ArrayIn mem ixfun_x },
                      y { paramDec = MemArray pt shape u $ ArrayIn mem ixfun_y })
            Prim pt ->
              return (x { paramDec = MemPrim pt },
                      y { paramDec = MemPrim pt })
            Mem space ->
              return (x { paramDec = MemMem space },
                      y { paramDec = MemMem space })

allocInBinOpLambda :: Allocable fromlore tolore =>
                      SubExp -> SegSpace -> Lambda fromlore
                   -> AllocM fromlore tolore (Lambda tolore)
allocInBinOpLambda num_threads (SegSpace flat _) lam = do
  let (acc_params, arr_params) =
        splitAt (length (lambdaParams lam) `div` 2) $ lambdaParams lam
      index_x = LeafExp flat int32
      index_y = index_x + primExpFromSubExp int32 num_threads
  (acc_params', arr_params') <-
    allocInBinOpParams num_threads index_x index_y acc_params arr_params

  allocInLambda (acc_params' ++ arr_params')
    (lambdaBody lam) (lambdaReturnType lam)
