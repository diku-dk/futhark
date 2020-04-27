{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Futhark.Pass.FirstOrderTransform
  ( firstOrderTransform )
  where

import Futhark.Transform.FirstOrderTransform (FirstOrderLore, transformFunDef, transformStms)
import Futhark.Representation.SOACS (SOACS, scopeOf)
import Futhark.Pass

firstOrderTransform :: FirstOrderLore lore => Pass SOACS lore
firstOrderTransform =
  Pass
  "first order transform"
  "Transform all SOACs to for-loops." $
  intraproceduralTransformationWithConsts
  transformStms (transformFunDef . scopeOf)
