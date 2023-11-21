module Futhark.Pass.OptimizeArrayLayoutTests (tests) where

import Futhark.Pass.OptimizeArrayLayout.AnalyzePrimExpTests qualified
import Futhark.Pass.OptimizeArrayLayout.AnalyzeTests qualified
import Futhark.Pass.OptimizeArrayLayout.LayoutTests qualified
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "OptimizeArrayLayoutTests"
    [ Futhark.Pass.OptimizeArrayLayout.AnalyzeTests.tests,
      Futhark.Pass.OptimizeArrayLayout.LayoutTests.tests,
      Futhark.Pass.OptimizeArrayLayout.AnalyzePrimExpTests.tests
    ]
