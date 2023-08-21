{-# LANGUAGE TypeFamilies #-}

-- | Transform any SOACs to @for@-loops.
--
-- Example:
--
-- @
-- let ys = map (\x -> x + 2) xs
-- @
--
-- becomes something like:
--
-- @
-- let out = scratch n i32
-- let ys =
--   loop (ys' = out) for i < n do
--     let x = xs[i]
--     let y = x + 2
--     let ys'[i] = y
--     in ys'
-- @
module Futhark.Pass.FirstOrderTransform (firstOrderTransform) where

import Futhark.IR.SOACS (SOACS, scopeOf)
import Futhark.Pass
import Futhark.Transform.FirstOrderTransform (FirstOrderRep, transformConsts, transformFunDef)

-- | The first-order transformation pass.
firstOrderTransform :: (FirstOrderRep rep) => Pass SOACS rep
firstOrderTransform =
  Pass
    "first order transform"
    "Transform all SOACs to for-loops."
    $ intraproceduralTransformationWithConsts
      transformConsts
      (transformFunDef . scopeOf)
