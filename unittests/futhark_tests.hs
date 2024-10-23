module Main (main) where

import Futhark.AD.DerivativesTests qualified
import Futhark.Analysis.AlgSimplifyTests qualified
import Futhark.Analysis.Proofs.IndexFnTests qualified
import Futhark.Analysis.Proofs.QueryTests qualified
import Futhark.Analysis.Proofs.RewriteTests qualified
import Futhark.Analysis.Proofs.UnifyTests qualified
import Futhark.Analysis.Proofs.AlgebraPC.SolveTests qualified
import Futhark.BenchTests qualified
import Futhark.IR.Mem.IntervalTests qualified
import Futhark.IR.Mem.IxFunTests qualified
import Futhark.IR.PropTests qualified
import Futhark.IR.Syntax.CoreTests qualified
import Futhark.Internalise.TypesValuesTests qualified
import Futhark.Optimise.ArrayLayoutTests qualified
import Futhark.Optimise.MemoryBlockMerging.GreedyColoringTests qualified
import Futhark.Pkg.SolveTests qualified
import Futhark.SoP.FourierMotzkinTests qualified
import Futhark.SoP.RefineTests qualified
import Futhark.SoP.SoPTests qualified
import Language.Futhark.PrimitiveTests qualified
import Language.Futhark.SemanticTests qualified
import Language.Futhark.SyntaxTests qualified
import Language.Futhark.TypeCheckerTests qualified
import Test.Tasty

allTests :: TestTree
allTests =
  testGroup
    ""
    [ Language.Futhark.SyntaxTests.tests,
      Futhark.AD.DerivativesTests.tests,
      Futhark.Analysis.Proofs.IndexFnTests.tests,
      Futhark.Analysis.Proofs.QueryTests.tests,
      Futhark.Analysis.Proofs.RewriteTests.tests,
      Futhark.Analysis.Proofs.UnifyTests.tests,
      Futhark.Analysis.Proofs.AlgebraPC.SolveTests.tests,
      Futhark.BenchTests.tests,
      Futhark.IR.PropTests.tests,
      Futhark.IR.Syntax.CoreTests.tests,
      Futhark.Pkg.SolveTests.tests,
      Futhark.Internalise.TypesValuesTests.tests,
      Futhark.IR.Mem.IntervalTests.tests,
      Futhark.IR.Mem.IxFunTests.tests,
      Language.Futhark.PrimitiveTests.tests,
      Futhark.Optimise.MemoryBlockMerging.GreedyColoringTests.tests,
      Futhark.Analysis.AlgSimplifyTests.tests,
      Language.Futhark.TypeCheckerTests.tests,
      Language.Futhark.SemanticTests.tests,
      Futhark.Optimise.ArrayLayoutTests.tests,
      Futhark.SoP.SoPTests.tests,
      Futhark.SoP.FourierMotzkinTests.tests,
      Futhark.SoP.RefineTests.tests
    ]

main :: IO ()
main = defaultMain allTests
