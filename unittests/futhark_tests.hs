module Main (main) where

import qualified Language.Futhark.SyntaxTests
import qualified Futhark.Representation.AST.Syntax.CoreTests
import qualified Futhark.Representation.AST.AttributesTests
import qualified Futhark.Representation.ExplicitMemory.IndexFunctionTests
import qualified Futhark.Optimise.AlgSimplifyTests
import qualified Futhark.Pkg.SolveTests

import Test.Tasty

allTests :: TestTree
allTests =
  testGroup ""
  [ Language.Futhark.SyntaxTests.tests
  , Futhark.Representation.AST.AttributesTests.tests
  , Futhark.Optimise.AlgSimplifyTests.tests
  , Futhark.Representation.AST.Syntax.CoreTests.tests
  , Futhark.Pkg.SolveTests.tests
  , Futhark.Representation.ExplicitMemory.IndexFunctionTests.tests
  ]

main :: IO ()
main = defaultMain allTests
