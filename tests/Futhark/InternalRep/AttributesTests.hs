{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futhark.InternalRep.AttributesTests
  ( tests
  )
where

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Futhark.InternalRep.SyntaxTests ()
import Futhark.InternalRep.Attributes

tests :: [Test]
tests = [ toParamFromParamIsIdent ]

toParamFromParamIsIdent :: Test
toParamFromParamIsIdent =
  testProperty "toParam . isParam == id" $ \var ->
  fromParam (toParam var) == var
