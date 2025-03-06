module Language.Futhark.TypeChecker.TySolveTests (tests) where

import Data.Map qualified as M
import Futhark.Util.Pretty (docString)
import Language.Futhark.Syntax (Liftedness (..))
import Language.Futhark.SyntaxTests ()
import Language.Futhark.TypeChecker.Constraints
  ( CtTy (..),
    Reason (..),
    TyParams,
    TyVarInfo (..),
    TyVars,
  )
import Language.Futhark.TypeChecker.Monad (prettyTypeError)
import Language.Futhark.TypeChecker.TySolve
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

testSolve ::
  [CtTy ()] ->
  TyParams ->
  TyVars () ->
  ([UnconTyVar], Solution) ->
  Assertion
testSolve constraints typarams tyvars expected =
  case solve constraints typarams tyvars of
    Right s -> s @?= expected
    Left e -> assertFailure $ docString $ prettyTypeError e

-- When writing type variables/names here (a_0, b_1), make *sure* that
-- the numbers are distinct. These are all that actually matter for
-- determining identify.

tests :: TestTree
tests =
  testGroup
    "Unsized constraint solver"
    [ testCase "empty" $
        testSolve [] mempty mempty ([], mempty),
      testCase "a_0 ~ b_1" $
        testSolve
          [CtEq (Reason mempty) "a_0" "b_1"]
          mempty
          (M.fromList [("a_0", (0, TyVarFree mempty Unlifted))])
          ([], M.fromList [("a_0", Right "b_1")])
    ]
