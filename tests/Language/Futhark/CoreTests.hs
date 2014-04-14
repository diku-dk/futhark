module Language.Futhark.CoreTests
  (tests)
where

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Data.HashMap.Lazy as HM

import Language.Futhark.Core

tests :: [Test]
tests = testBuiltins

testBuiltins :: [Test]
testBuiltins = [ testCase (nameToString f ++ " is builtin") $
                 isBuiltInFunction f @?= True
                 | f <- HM.keys builtInFunctions ]
