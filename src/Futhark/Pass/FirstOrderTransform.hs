module Futhark.Pass.FirstOrderTransform
  ( firstOrderTransform
  )
  where

import Futhark.Transform.FirstOrderTransform (transformFunDef)
import Futhark.Representation.SOACS (SOACS)
import Futhark.Representation.Kernels (Kernels)
import Futhark.Pass

firstOrderTransform :: Pass SOACS Kernels
firstOrderTransform = Pass
                      "first order transform"
                      "Transform all second-order array combinators to for-loops." $
                      intraproceduralTransformation transformFunDef
