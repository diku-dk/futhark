module Futhark.Representation.AST.Attributes.RearrangeTests
       ( tests )
       where

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

import Prelude

import Futhark.Representation.AST.Attributes.Rearrange

tests :: [Test]
tests = isMapTransposeTests

isMapTransposeTests :: [Test]
isMapTransposeTests =
  [ testCase (unwords ["isMapTranspose ", show perm, show dres]) $
    isMapTranspose perm @?= dres
  | (perm, dres) <- [ ([0,1,4,5,2,3], Just (2,2,2))
                    , ([1,0,4,5,2,3], Nothing)
                    , ([1,0], Just (0, 1, 1))
                    , ([0,2,1], Just (1, 1, 1))
                    ]
  ]
