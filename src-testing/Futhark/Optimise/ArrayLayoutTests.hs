module Futhark.Optimise.ArrayLayoutTests (tests) where

import Futhark.Analysis.PrimExp.TableTests qualified
import Futhark.Optimise.ArrayLayout.AnalyseTests qualified
import Futhark.Optimise.ArrayLayout.LayoutTests qualified
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "OptimizeArrayLayoutTests"
    [ Futhark.Optimise.ArrayLayout.AnalyseTests.tests,
      Futhark.Optimise.ArrayLayout.LayoutTests.tests,
      Futhark.Analysis.PrimExp.TableTests.tests
    ]
