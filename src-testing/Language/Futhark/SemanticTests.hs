module Language.Futhark.SemanticTests (tests) where

import Language.Futhark (ImportName (..))
import Language.Futhark.Semantic
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Semantic objects"
    [ testCase "a" $
        mkInitialImport "a" @?= ImportName "a",
      testCase "./a" $
        mkInitialImport "./a" @?= ImportName "a",
      testCase "a/b -> ../c" $
        mkImportFrom (mkInitialImport "a/b") "../c" @?= ImportName "c",
      testCase "a/b -> ../../c" $
        mkImportFrom (mkInitialImport "a/b") "../../c" @?= ImportName "../c",
      testCase "../a -> b" $
        mkImportFrom (mkInitialImport "../a") "b" @?= ImportName "../b",
      testCase "../a -> ../b" $
        mkImportFrom (mkInitialImport "../a") "../b" @?= ImportName "../../b"
    ]
