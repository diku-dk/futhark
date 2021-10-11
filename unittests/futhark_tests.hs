module Main (main) where

import qualified Futhark.Analysis.AlgSimplify2Tests
import qualified Futhark.Analysis.AlgSimplifyTests
import qualified Futhark.BenchTests
import qualified Futhark.IR.Mem.IxFunTests
import qualified Futhark.IR.PrimitiveTests
import qualified Futhark.IR.PropTests
import qualified Futhark.IR.Syntax.CoreTests
import qualified Futhark.Optimise.MemoryBlockMerging.GreedyColoringTests
import qualified Futhark.Pkg.SolveTests
import qualified Language.Futhark.SyntaxTests
import Test.Tasty

allTests :: TestTree
allTests =
  testGroup
    ""
    [ Language.Futhark.SyntaxTests.tests,
      Futhark.BenchTests.tests,
      Futhark.IR.PropTests.tests,
      Futhark.IR.Syntax.CoreTests.tests,
      Futhark.Pkg.SolveTests.tests,
      Futhark.IR.Mem.IxFunTests.tests,
      Futhark.IR.PrimitiveTests.tests,
      Futhark.Optimise.MemoryBlockMerging.GreedyColoringTests.tests,
      Futhark.Analysis.AlgSimplifyTests.tests,
      Futhark.Analysis.AlgSimplify2Tests.tests
    ]

main :: IO ()
main = defaultMain allTests
