{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futhark.Representation.AST.AttributesTests
  ( tests
  )
where

import Test.Tasty

import qualified Futhark.Representation.AST.Attributes.ReshapeTests
import qualified Futhark.Representation.AST.Attributes.RearrangeTests

tests :: TestTree
tests = testGroup "AttributesTests"
        [Futhark.Representation.AST.Attributes.ReshapeTests.tests,
         Futhark.Representation.AST.Attributes.RearrangeTests.tests]
