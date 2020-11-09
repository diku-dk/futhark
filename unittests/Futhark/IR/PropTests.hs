{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.IR.PropTests
  ( tests,
  )
where

import qualified Futhark.IR.Prop.RearrangeTests
import qualified Futhark.IR.Prop.ReshapeTests
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "PropTests"
    [ Futhark.IR.Prop.ReshapeTests.tests,
      Futhark.IR.Prop.RearrangeTests.tests
    ]
