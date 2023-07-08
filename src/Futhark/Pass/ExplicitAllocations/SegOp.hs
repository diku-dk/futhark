{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.Pass.ExplicitAllocations.SegOp
  ( allocInKernelBody,
    allocInBinOpLambda,
  )
where

import Control.Monad
import Futhark.IR.GPUMem
import Futhark.IR.Mem.IxFun qualified as IxFun
import Futhark.Pass.ExplicitAllocations

instance SizeSubst (SegOp lvl rep)

allocInKernelBody ::
  Allocable fromrep torep inner =>
  KernelBody fromrep ->
  AllocM fromrep torep (KernelBody torep)
allocInKernelBody (KernelBody () stms res) =
  uncurry (flip (KernelBody ()))
    <$> collectStms (allocInStms stms (pure res))

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
              BasicOp $
                BinOp (Mul Int64 OverflowUndef) num_threads $
                  intConst Int64 2
          let t = paramType x `arrayOfRow` twice_num_threads
          mem <- allocForArray t =<< askDefaultSpace
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
  let (xs_params, ys_params) =
        splitAt (length (lambdaParams lam) `div` 2) $ lambdaParams lam
      index_x = TPrimExp $ LeafExp flat int64
      index_y = index_x + pe64 num_threads

  params <-
    uncurry (++) <$> allocInBinOpParams num_threads index_x index_y xs_params ys_params

  -- FIXME: the index functions we construct for any array-typed
  -- params are very confusing to the rest of the compiler because
  -- they are slices of larger allocations, so we copy them into
  -- distinct memory blocks with a simpler index function and let the
  -- rest of the function use those instead.  The main problem is the
  -- unExistentialiseMemory simplification rule, which is necessary to
  -- make memory expansion work out.  Ultimately memory expansion
  -- should be completely rethought, and then we can get rid of hacks
  -- such as these.
  (params', mk_copies) <- mapAndUnzipM fixupArrParam params

  mkLambda params' $ do
    sequence_ mk_copies
    allocInStms (bodyStms (lambdaBody lam)) $ pure $ bodyResult (lambdaBody lam)
  where
    fixupArrParam (Param attr v dec@(MemArray pt shape u (ArrayIn mem _))) = do
      p <- Param attr <$> newVName (baseString v <> "_indirect") <*> pure dec
      space <- lookupMemSpace mem
      pure
        ( p,
          do
            let t = typeOf dec
            mem' <- allocForArray t space
            let info =
                  MemArray pt shape u . ArrayIn mem' $
                    IxFun.iota (map pe64 (arrayDims t))
            addStm $
              Let (Pat [PatElem v info]) (defAux ()) $
                BasicOp (Replicate mempty (Var (paramName p)))
        )
    fixupArrParam p = pure (p, pure ())
