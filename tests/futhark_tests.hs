module Main (main) where

import qualified Language.Futhark.CoreTests
import qualified Futhark.InternalRep.AttributesTests

import Test.Framework (defaultMain, testGroup, Test)


allTests :: [Test]
allTests =
  [ testGroup "CoreTests" Language.Futhark.CoreTests.tests
  , testGroup "AttributesTests" Futhark.InternalRep.AttributesTests.tests
  ]

main :: IO ()
main = defaultMain allTests
