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

testSolveFail ::
  [CtTy ()] ->
  TyParams ->
  TyVars () ->
  Assertion
testSolveFail constraints typarams tyvars =
  case solve constraints typarams tyvars of
    Left _ -> pure ()
    Right _ -> assertFailure "Expected type error, but got a solution"

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
      testCase "Two variables" $
        testSolve 
          ["a_0" ~ "b_1", "c_2" ~ "d_3"]
          mempty
          (M.fromList [tv "a_0" 0, tv "c_2" 0])
          ([], M.fromList [("a_0", Right "b_1"), ("c_2", Right "d_3")]),
      testCase "i32 + (i32 + i32)" $
        testSolve
          ["i32 -> i32 -> a_0" ~ "i32 -> i32 -> i32",
           "i32 -> a_0 -> b_1" ~ "i32 -> i32 -> i32"]
          mempty
          (M.fromList [tv "a_0" 0, tv "b_1" 0])
          ([], M.fromList [("a_0", Right "i32"), ("b_1", Right "i32")]),
      testCase "((λx -> λy -> x * y) i32) i32" $
        testSolve
          ["a_0 -> b_1 -> c_2" ~ "i32 -> i32 -> i32",
           "a_0 -> b_1 -> c_2" ~ "i32 -> d_3",
           "d_3" ~ "i32 -> e_4"]
          mempty
          (M.fromList [tv "a_0" 0, tv "b_1" 0, tv "c_2" 0, tv "d_3" 0, tv "e_4" 0])
          ([], M.fromList [
                           ("a_0", Right "i32"),
                           ("b_1", Right "i32"),
                           ("c_2", Right "i32"),
                           ("d_3", Right "i32 -> i32"),
                           ("e_4", Right "i32")
                          ]),
      testCase "rec λf -> λn -> if n == 0 then 1 else n * (f (n - 1))" $
        testSolve
          ["b_1 -> i32 -> c_2" ~ "i32 -> i32 -> bool",
           "b_1 -> i32 -> d_3" ~ "i32 -> i32 -> i32",
           "a_0" ~ "d_3 -> e_4",
           "b_1 -> e_4 -> f_5" ~ "i32 -> i32 -> i32",
           "c_2" ~ "bool",
           "i32" ~ "f_5",
           "g_6 -> g_6" ~ "a_0 -> b_1 -> i32"] 
          mempty
          (M.fromList [tv "a_0" 0, tv "b_1" 0, tv "c_2" 0, tv "d_3" 0, tv "e_4" 0, tv "f_5" 0, tv "g_6" 0])
          ([], M.fromList [
                  ("a_0", Right "i32 -> i32"),
                  ("b_1", Right "i32"),
                  ("c_2", Right "bool"),
                  ("d_3", Right "i32"),
                  ("e_4", Right "i32"),
                  ("f_5", Right "i32"),
                  ("g_6", Right "i32 -> i32")
                ]),
      testCase "let id = λx -> x in id id" $
        testSolve
          ["b_1 -> b_1" ~ "(c_2 -> c_2) -> d_3"]
          mempty
          (M.fromList [tv "b_1" 0, tv "c_2" 0, tv "d_3" 0])
          ([("c_2", Unlifted)], M.fromList [
            ("b_1", Right "c_2 -> c_2"),
            ("d_3", Right "c_2 -> c_2")
          ]),

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
        testSolveFail
          ["a_0" ~ "i32", "a_0" ~ "bool"] 
          mempty 
          (M.fromList [tv "a_0" 0]),
      
      testCase "infinite type" $
        testSolveFail
          ["a_0" ~ "a_0 -> b_1"]
          mempty
          (M.fromList [tv "a_0" 0])
    ]
