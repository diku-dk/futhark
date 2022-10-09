module Main (main) where

import Futhark.AD.DerivativesTests qualified
import Futhark.Analysis.AlgSimplifyTests qualified
import Futhark.BenchTests qualified
import Futhark.IR.Mem.IntervalTests qualified
import Futhark.IR.Mem.IxFunTests qualified
import Futhark.IR.PropTests qualified
import Futhark.IR.Syntax.CoreTests qualified
import Futhark.Optimise.MemoryBlockMerging.GreedyColoringTests qualified
import Futhark.Pkg.SolveTests qualified
import Language.Futhark.PrimitiveTests qualified
import Language.Futhark.SyntaxTests qualified
import Language.Futhark.TypeCheckerTests qualified
import Test.Tasty

allTests :: TestTree
allTests =
  testGroup
    ""
    [ Language.Futhark.SyntaxTests.tests,
      Futhark.AD.DerivativesTests.tests,
      Futhark.BenchTests.tests,
      Futhark.IR.PropTests.tests,
      Futhark.IR.Syntax.CoreTests.tests,
      Futhark.Pkg.SolveTests.tests,
      Futhark.IR.Mem.IntervalTests.tests,
      Futhark.IR.Mem.IxFunTests.tests,
      Language.Futhark.PrimitiveTests.tests,
      Futhark.Optimise.MemoryBlockMerging.GreedyColoringTests.tests,
      Futhark.Analysis.AlgSimplifyTests.tests,
      Language.Futhark.TypeCheckerTests.tests
    ]

main :: IO ()
main = defaultMain allTests
