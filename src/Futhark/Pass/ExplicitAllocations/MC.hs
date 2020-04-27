{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.Pass.ExplicitAllocations.MC
  ( explicitAllocations )
where

import Futhark.Representation.MCMem
import Futhark.Representation.MC
import Futhark.Pass.ExplicitAllocations
import Futhark.Pass.ExplicitAllocations.SegOp

handleSegOp :: SegOp () MC -> AllocM MC MCMem (MemOp (SegOp () MCMem))
handleSegOp op = do
  let num_threads = intConst Int32 256 -- FIXME
  Inner <$> mapSegOpM (mapper num_threads) op
  where scope = scopeOfSegSpace $ segSpace op
        mapper num_threads =
          identitySegOpMapper
          { mapOnSegOpBody =
              localScope scope . allocInKernelBody
          , mapOnSegOpLambda =
              allocInBinOpLambda num_threads (segSpace op)
          }

explicitAllocations :: Pass MC MCMem
explicitAllocations = explicitAllocationsGeneric handleSegOp defaultExpHints
