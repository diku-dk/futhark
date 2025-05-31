module Language.Futhark.TypeCheckerTests (tests) where

import Language.Futhark.TypeChecker.TypesTests qualified
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Source type checker tests"
    [ Language.Futhark.TypeChecker.TypesTests.tests
    ]
