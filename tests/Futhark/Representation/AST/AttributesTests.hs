{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futhark.Representation.AST.AttributesTests
  ( tests
  )
where

import Test.Framework

import Futhark.Representation.AST.SyntaxTests ()
import qualified Futhark.Representation.AST.Attributes.ValuesTests

tests :: [Test]
tests = Futhark.Representation.AST.Attributes.ValuesTests.tests
