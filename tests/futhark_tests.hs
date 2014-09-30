module Main (main) where

import qualified Language.Futhark.CoreTests
import qualified Futhark.Representation.AST.AttributesTests
import qualified Futhark.Optimise.AlgSimplifyTests

import Test.Framework (defaultMain, testGroup, Test)

allTests :: [Test]
allTests =
  [ testGroup "CoreTests" Language.Futhark.CoreTests.tests
  , testGroup "AttributesTests" Futhark.Representation.AST.AttributesTests.tests
  , testGroup "AlgSimplifyTests" Futhark.Optimise.AlgSimplifyTests.tests
  ]

main :: IO ()
main = defaultMain allTests
