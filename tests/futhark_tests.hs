module Main (main) where

import qualified Language.Futhark.CoreTests

import Test.Framework (defaultMain, testGroup, Test)


allTests :: [Test]
allTests = [testGroup "CoreTests"  Language.Futhark.CoreTests.tests]

main :: IO ()
main = defaultMain allTests
