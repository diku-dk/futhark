module Futhark.Optimise.FusionTests (tests) where

import Futhark.Optimise.Fusion.ScremaTests qualified
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "OptimiseFusionTests"
    [Futhark.Optimise.Fusion.ScremaTests.tests]
