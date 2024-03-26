module Futhark.Pass.OptimiseArrayLayoutTests (tests) where

import Futhark.Pass.OptimiseArrayLayout.AnalysePrimExpTests qualified
import Futhark.Pass.OptimiseArrayLayout.AnalyseTests qualified
import Futhark.Pass.OptimiseArrayLayout.LayoutTests qualified
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "OptimizeArrayLayoutTests"
    [ Futhark.Pass.OptimiseArrayLayout.AnalyseTests.tests,
      Futhark.Pass.OptimiseArrayLayout.LayoutTests.tests,
      Futhark.Pass.OptimiseArrayLayout.AnalysePrimExpTests.tests
    ]
