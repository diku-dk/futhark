module Futhark.Pass.FirstOrderTransform
  ( firstOrderTransform
  )
  where

import Futhark.Transform.FirstOrderTransform (transformProg)
import Futhark.Representation.SOACS
import Futhark.Pass

firstOrderTransform :: Pass SOACS SOACS
firstOrderTransform = simplePass
                      "first order transform"
                      "Transform all second-order array combinators to for-loops."
                      transformProg
