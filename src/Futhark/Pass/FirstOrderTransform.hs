{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
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
module Futhark.Pass.FirstOrderTransform
  ( firstOrderTransform )
  where

import Futhark.Transform.FirstOrderTransform (FirstOrderLore, transformFunDef, transformConsts)
import Futhark.IR.SOACS (SOACS, scopeOf)
import Futhark.Pass

-- | The first-order transformation pass.
firstOrderTransform :: FirstOrderLore lore => Pass SOACS lore
firstOrderTransform =
  Pass
  "first order transform"
  "Transform all SOACs to for-loops." $
  intraproceduralTransformationWithConsts
  transformConsts (transformFunDef . scopeOf)
