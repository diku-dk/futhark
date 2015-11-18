module Futhark.Representation.AST.Attributes.StripeTests
       ( tests
       )
       where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Prelude

import Futhark.Representation.AST.Attributes.Stripe

tests :: [Test]
tests = [ stripeAndInverseTest ]

stripeAndInverseTest :: Test
stripeAndInverseTest =
  testProperty "stripeIndex and stripeIndexInverse are inverses" prop
    where prop n num_blocks =
            (n >= 0 && num_blocks >= 1) ==>
            let striped = stripeIndices n num_blocks
            in [0..n-1] == map (striped!!) (stripeIndicesInverse n num_blocks)
