{-# LANGUAGE TypeFamilies #-}

-- | Do various kernel optimisations - mostly related to coalescing.
module Futhark.Pass.CoalesceAccess (coalesceAccess) where

import Futhark.IR.GPU
import Futhark.Pass

import Debug.Pretty.Simple
-- | The pass definition.
coalesceAccess :: Pass GPU GPU
coalesceAccess =
  Pass
    "coalesce access"
    "Transform kernel input arrays for better performance."
    $ return . pTraceShowId
