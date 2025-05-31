{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.IR.PropTests
  ( tests,
  )
where

import Futhark.IR.Prop.RearrangeTests qualified
import Futhark.IR.Prop.ReshapeTests qualified
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "PropTests"
    [ Futhark.IR.Prop.ReshapeTests.tests,
      Futhark.IR.Prop.RearrangeTests.tests
    ]
