module Futhark.Representation.AST.Attributes.ValuesTests
  ( tests ) where

import Test.Tasty
import Test.Tasty.HUnit

import Futhark.Representation.AST.Attributes.Values
import Futhark.Representation.AST.Syntax

tests :: TestTree
tests = testGroup "ValuesTests" blankValueHasRightType

blankValueHasRightType :: [TestTree]
blankValueHasRightType = [ testCase (show t ++ " has blank of right type") $
                           valueType (PrimVal (blankPrimValue t)) @?= Prim t
                         | t <- [minBound..maxBound]
                         ]
