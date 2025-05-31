module Language.Futhark.PrettyTests (tests) where

import Data.Text qualified as T
import Language.Futhark
import Language.Futhark.SyntaxTests ()
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

var :: QualName Name -> UncheckedExp
var x = Var x NoInfo mempty

binOp :: QualName Name -> UncheckedExp -> UncheckedExp -> UncheckedExp
binOp op x y = AppExp (BinOp (op, mempty) NoInfo (x, NoInfo) (y, NoInfo) mempty) NoInfo

tests :: TestTree
tests =
  testGroup
    "Language.Futhark.Pretty"
    [ testCase "No outer parens" $
        p (binOp "+" (var "x") (var "y"))
          @?= "x + y",
      testCase "No redundant parens" $
        p (binOp "+" "x+y" (var "z"))
          @?= "x + y + z",
      testCase "Necessary parens" $
        p (binOp "+" (var "x") "y+z")
          @?= "x + (y + z)",
      testCase "Explicit but redundant parens" $
        p "(x+y)+z"
          @?= "(x + y) + z"
    ]
  where
    p :: UncheckedExp -> T.Text
    p = prettyText
