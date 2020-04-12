module Futhark.Pass.FirstOrderTransform
  ( firstOrderTransform )
  where

import Futhark.Transform.FirstOrderTransform (transformFunDef, transformStms)
import Futhark.Representation.SOACS (SOACS, scopeOf)
import Futhark.Representation.Kernels (Kernels)
import Futhark.Pass

firstOrderTransform :: Pass SOACS Kernels
firstOrderTransform =
  Pass
  "first order transform"
  "Transform all second-order array combinators to for-loops." $
  intraproceduralTransformationWithConsts
  transformStms (transformFunDef . scopeOf)
