module Main (main) where

import Futhark.AD.DerivativesTests qualified
import Futhark.Analysis.AlgSimplifyTests qualified
import Futhark.Analysis.DataDependenciesTests qualified
import Futhark.BenchTests qualified
import Futhark.IR.Mem.IntervalTests qualified
import Futhark.IR.Mem.IxFunTests qualified
import Futhark.IR.PropTests qualified
import Futhark.IR.Syntax.CoreTests qualified
import Futhark.Internalise.TypesValuesTests qualified
import Futhark.LspTests qualified
import Futhark.Optimise.ArrayLayoutTests qualified
import Futhark.Optimise.FusionTests qualified
import Futhark.Optimise.MemoryBlockMerging.GreedyColoringTests qualified
import Futhark.Pkg.SolveTests qualified
import Futhark.Solve.BranchAndBoundTests qualified
import Futhark.Solve.SimplexTests qualified
import Language.Futhark.PrettyTests qualified
import Language.Futhark.PrimitiveTests qualified
import Language.Futhark.SemanticTests qualified
import Language.Futhark.SyntaxTests qualified
import Language.Futhark.TypeCheckerTests qualified
import Test.Tasty

allTests :: TestTree
allTests =
  testGroup
    ""
    [ Futhark.AD.DerivativesTests.tests,
      Futhark.Analysis.AlgSimplifyTests.tests,
      Futhark.Analysis.DataDependenciesTests.tests,
      Futhark.BenchTests.tests,
      Futhark.IR.Mem.IntervalTests.tests,
      Futhark.IR.Mem.IxFunTests.tests,
      Futhark.IR.PropTests.tests,
      Futhark.IR.Syntax.CoreTests.tests,
      Futhark.Internalise.TypesValuesTests.tests,
      Futhark.LspTests.tests,
      Futhark.Optimise.ArrayLayoutTests.tests,
      Futhark.Optimise.FusionTests.tests,
      Futhark.Optimise.MemoryBlockMerging.GreedyColoringTests.tests,
      Futhark.Pkg.SolveTests.tests,
      Futhark.Solve.BranchAndBoundTests.tests,
      Futhark.Solve.SimplexTests.tests,
      Language.Futhark.PrettyTests.tests,
      Language.Futhark.PrimitiveTests.tests,
      Language.Futhark.SemanticTests.tests,
      Language.Futhark.TypeCheckerTests.tests,
      Language.Futhark.SyntaxTests.tests
    ]

main :: IO ()
main = defaultMain allTests
