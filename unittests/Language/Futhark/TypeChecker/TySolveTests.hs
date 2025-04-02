module Language.Futhark.TypeChecker.TySolveTests (tests) where

import Data.Map qualified as M
import Futhark.Util.Pretty (docString)
import Language.Futhark.Syntax (Liftedness (..), NoUniqueness, TypeBase, VName)
import Language.Futhark.SyntaxTests ()
import Language.Futhark.TypeChecker.Constraints
  ( CtTy (..),
    Level,
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
-- determining identity.

(~) :: TypeBase () NoUniqueness -> TypeBase () NoUniqueness -> CtTy ()
t1 ~ t2 = CtEq (Reason mempty) t1 t2

tv :: VName -> Level -> (VName, (Level, TyVarInfo ()))
tv v lvl = (v, (lvl, TyVarFree mempty Unlifted))

tests :: TestTree
tests =
  testGroup
    "Unsized constraint solver"
    [ testCase "empty" $
        testSolve [] mempty mempty ([], mempty),

      testCase "a_0 ~ b_1" $
        testSolve
          ["a_0" ~ "b_1"]
          mempty
          (M.fromList [tv "a_0" 0])
          ([], M.fromList [("a_0", Right "b_1")]),

      testCase "a_0 ~ i32" $
        testSolve
          ["a_0" ~ "i32"]
          mempty
          (M.fromList [tv "a_0" 0])
          ([], M.fromList [("a_0", Right "i32")]),

      testCase "a_0 ~ a_0" $
        testSolve
          ["a_0" ~ "a_0"]
          mempty
          (M.fromList [tv "a_0" 0])
          ([("a_0", Unlifted)], mempty),

      testCase "unification fail" $
        let res = solve 
                  ["a_0" ~ "i32", "a_0" ~ "bool"] 
                  mempty 
                  (M.fromList [tv "a_0" 0]) 
        in 
          case res of
            Left _ -> pure ()
            Right _ -> assertFailure "Expected type error, but got a solution",
            
      testCase "infinite type" $
        let res = solve
                  ["a_0" ~ "a_0 -> b_1"]
                  mempty
                  (M.fromList [tv "a_0" 0])
        in
          case res of
            Left _ -> pure ()
            Right _ -> assertFailure "Expected type error, but got a solution"
    ]
