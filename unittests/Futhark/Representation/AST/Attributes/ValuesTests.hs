module Futhark.Representation.AST.Attributes.ValuesTests
  ( tests
  )
where

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

import Futhark.Representation.AST.Attributes.Values
import Futhark.Representation.AST.Syntax

tests :: [Test]
tests = blankValueHasRightType

blankValueHasRightType :: [Test]
blankValueHasRightType = [ testCase (show t ++ " has blank of right type") $
                           valueType (PrimVal (blankPrimValue t)) @?= Prim t
                         | t <- [minBound..maxBound]
                         ]
