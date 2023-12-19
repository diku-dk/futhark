module Futhark.Pass.GALOPTests (tests) where

import Futhark.Pass.GALOP.AnalysePrimExpTests qualified
import Futhark.Pass.GALOP.AnalyseTests qualified
import Futhark.Pass.GALOP.LayoutTests qualified
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "GALOPTests"
    [ Futhark.Pass.GALOP.AnalyseTests.tests,
      Futhark.Pass.GALOP.LayoutTests.tests,
      Futhark.Pass.GALOP.AnalysePrimExpTests.tests
    ]
