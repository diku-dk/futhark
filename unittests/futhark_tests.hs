module Main (main) where

import qualified Language.Futhark.SyntaxTests
import qualified Futhark.Representation.AST.Syntax.CoreTests
import qualified Futhark.Representation.AST.AttributesTests
import qualified Futhark.Optimise.AlgSimplifyTests

import Test.Framework (defaultMain, testGroup, Test)

allTests :: [Test]
allTests =
  [ testGroup "external SyntaxTests" Language.Futhark.SyntaxTests.tests
  , testGroup "AttributesTests" Futhark.Representation.AST.AttributesTests.tests
  , testGroup "AlgSimplifyTests" Futhark.Optimise.AlgSimplifyTests.tests
  , testGroup "internal CoreTests" Futhark.Representation.AST.Syntax.CoreTests.tests
  ]

main :: IO ()
main = defaultMain allTests
