{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.Pass.ExplicitAllocations.MC (explicitAllocations) where

import Futhark.IR.MC
import Futhark.IR.MCMem
import Futhark.Pass.ExplicitAllocations
import Futhark.Pass.ExplicitAllocations.SegOp

instance SizeSubst (MCOp lore op) where
  opSizeSubst _ _ = mempty

handleSegOp :: SegOp () MC -> AllocM MC MCMem (SegOp () MCMem)
handleSegOp segop = do
  let num_threads = intConst Int64 256 -- FIXME
  case segop of
    SegStencil lvl space op ts body -> do
      op' <- handleStencilOp num_threads op
      SegStencil lvl space op' ts
        <$> localScope scope (allocInKernelBody body)
    _ -> mapSegOpM (mapper num_threads) segop
  where
    scope = scopeOfSegSpace $ segSpace segop
    mapper num_threads =
      identitySegOpMapper
        { mapOnSegOpBody =
            localScope scope . allocInKernelBody,
          mapOnSegOpLambda =
            allocInBinOpLambda num_threads (segSpace segop)
        }

    handleStencilOp num_threads (StencilOp is arrs lam) =
      StencilOp is arrs <$> allocInStencilOpLambda num_threads (segSpace segop) lam

handleMCOp :: Op MC -> AllocM MC MCMem (Op MCMem)
handleMCOp (ParOp par_op op) =
  Inner <$> (ParOp <$> traverse handleSegOp par_op <*> handleSegOp op)
handleMCOp (OtherOp soac) =
  error $ "Cannot allocate memory in SOAC: " ++ pretty soac

explicitAllocations :: Pass MC MCMem
explicitAllocations = explicitAllocationsGeneric handleMCOp defaultExpHints
