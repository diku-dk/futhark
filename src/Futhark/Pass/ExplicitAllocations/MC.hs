{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Converting 'MC' programs to 'MCMem'.
module Futhark.Pass.ExplicitAllocations.MC (explicitAllocations) where

import Futhark.IR.MC
import Futhark.IR.MCMem
import Futhark.Pass.ExplicitAllocations
import Futhark.Pass.ExplicitAllocations.SegOp

instance SizeSubst (MCOp rep op)

handleSegOp :: SegOp () MC -> AllocM MC MCMem (SegOp () MCMem)
handleSegOp op = do
  let num_threads = intConst Int64 256 -- FIXME
  mapSegOpM (mapper num_threads) op
  where
    scope = scopeOfSegSpace $ segSpace op
    mapper num_threads =
      identitySegOpMapper
        { mapOnSegOpBody =
            localScope scope . allocInKernelBody,
          mapOnSegBinOpLambda =
            allocInBinOpLambda num_threads (segSpace op),
          mapOnSegPostOpLambda =
            allocInPostOpLambda num_threads (segSpace op)
        }

handleMCOp :: Op MC -> AllocM MC MCMem (Op MCMem)
handleMCOp (ParOp par_op op) =
  Inner <$> (ParOp <$> traverse handleSegOp par_op <*> handleSegOp op)
handleMCOp (OtherOp soac) =
  error $ "Cannot allocate memory in SOAC: " ++ prettyString soac

-- | The pass from 'MC' to 'MCMem'.
explicitAllocations :: Pass MC MCMem
explicitAllocations = explicitAllocationsGeneric DefaultSpace handleMCOp defaultExpHints
