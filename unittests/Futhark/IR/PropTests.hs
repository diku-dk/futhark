{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futhark.IR.PropTests
  ( tests
  )
where

import Test.Tasty

import qualified Futhark.IR.Prop.ReshapeTests
import qualified Futhark.IR.Prop.RearrangeTests

tests :: TestTree
tests = testGroup "PropTests"
        [Futhark.IR.Prop.ReshapeTests.tests,
         Futhark.IR.Prop.RearrangeTests.tests]
