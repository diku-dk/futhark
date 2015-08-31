module Futhark.Pass.FirstOrderTransform
  ( firstOrderTransform
  )
  where

import Futhark.Transform.FirstOrderTransform (transformProg)
import Futhark.Representation.Basic
import Futhark.Pass

firstOrderTransform :: Pass Basic Basic
firstOrderTransform = simplePass
                      "first order transform"
                      "Transform all second-order array combinators to for-loops."
                      transformProg
