module Language.Futhark.TypeChecker.TySolveTests (tests) where

import Data.Map qualified as M
import Data.Loc ( Loc, noLoc )
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
import Language.Futhark.TypeChecker.Monad (prettyTypeError, TypeError(TypeError))
import Language.Futhark.TypeChecker.TySolve
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Text.Regex.TDFA

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
  String ->
  Assertion
testSolveFail constraints typarams tyvars expectedMsg =
  case solve constraints typarams tyvars of
    Left (TypeError _ _ actualMsg) -> docString actualMsg @?= expectedMsg
    Right _ -> assertFailure "Expected type error, but got a solution"

-- When writing type variables/names here (a_0, b_1), make *sure* that
-- the numbers are distinct. These are all that actually matter for
-- determining identity.

(~) :: TypeBase () NoUniqueness -> TypeBase () NoUniqueness -> CtTy ()
t1 ~ t2 = CtEq (Reason mempty) t1 t2

tv :: VName -> Level -> (VName, (Level, TyVarInfo ()))
tv v lvl = (v, (lvl, TyVarFree mempty Unlifted))

typaram :: VName -> Level -> Liftedness -> (VName, (Level, Liftedness, Loc))
typaram v lvl liftedness = (v, (lvl, liftedness, noLoc))

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

      testCase "non-unifiable types" $
        testSolveFail
          ["a_0" ~ "i32", "a_0" ~ "bool"] 
          mempty 
          (M.fromList [tv "a_0" 0])
          "Cannot unify\n  i32\nwith\n  bool",
      
      testCase "infinite type (function)" $
        testSolveFail
          ["a_0" ~ "a_0 -> b_1"]
          mempty
          (M.fromList [tv "a_0" 0])
          "Occurs check: cannot instantiate a with a -> b.",

      testCase "infinite type (list)" $
        testSolveFail
          ["a_0" ~ "[]a_0"]
          mempty
          (M.fromList [tv "a_0" 0])
          "Occurs check: cannot instantiate a with []a.",

      testCase "infinite type (tuple)" $
        testSolveFail
          ["a_0" ~ "(a_0, bool)"]
          mempty
          (M.fromList [tv "a_0" 0])
          "Occurs check: cannot instantiate a with (a, bool).",

      testCase "infinite type (record)" $
        testSolveFail
          ["a_0" ~ "{foo: a_0, bar: f32}"]
          mempty
          (M.fromList [tv "a_0" 0])
          "Occurs check: cannot instantiate a with {bar: f32, foo: a}.",

      -- testCase "infinite type (sum type)" $
      --   testSolveFail
      --     ["a_0" ~ "#foo: a_0"]
      --     mempty
      --     (M.fromList [tv "a_0" 0])
      --     "Occurs check: cannot instantiate a with #foo: a.",

      testCase "infinite type (consuming array param)" $
        testSolveFail
          ["a_0" ~ "*[]a_0"]
          mempty
          (M.fromList [tv "a_0" 0])
          "Occurs check: cannot instantiate a with []a.",

      testCase "vector and 2D matrix" $
        testSolveFail
          ["a_0" ~ "[]i32", "a_0" ~ "[][]i32"]
          mempty
          (M.fromList [tv "a_0" 0])
          "Cannot unify\n  i32\nwith\n  []i32",

      testCase "different array types" $
        testSolveFail
          ["a_0" ~ "[]f64", "a_0" ~ "[]i64"]
          mempty
          (M.fromList [tv "a_0" 0])
          "Cannot unify\n  f64\nwith\n  i64",

      testCase "simple record" $
        testSolve
          ["a_0" ~ "{foo: i32, bar: bool}"]
          mempty
          (M.fromList [tv "a_0" 0])
          ([], M.fromList [("a_0", Right "{foo: i32, bar: bool}")]),

      testCase "record 2" $
        testSolve
          ["a_0" ~ "{foo: b_1, bar: c_2}", "b_1" ~ "c_2", "c_2" ~ "i64"]
          mempty
          (M.fromList [tv "a_0" 0, tv "b_1" 0, tv "c_2" 0])
          ([], M.fromList 
                [("a_0", Right "{foo: i64, bar: i64}"), 
                 ("b_1", Right "i64"),
                 ("c_2", Right "i64")
                ]
          ),

      testCase "record 3" $
        testSolve
          ["a_0" ~ "{foo: b_1, bar: c_2}", "b_1" ~ "c_2"]
          (M.fromList [typaram "c_2" 0 Lifted])
          (M.fromList [tv "a_0" 0, tv "b_1" 0])
          ([], M.fromList 
                [("a_0", Right "{foo: c_2, bar: c_2}"), 
                 ("b_1", Right "c_2")
                ]
          ),

      testCase "tuple" $
        testSolve
          ["a_0" ~ "(b_1, c_2, d_3)", "c_2" ~ "d_3"]
          mempty
          (M.fromList [tv "a_0" 0, tv "b_1" 0, tv "c_2" 0, tv "d_3" 0])
          ([("b_1", Unlifted), ("d_3", Unlifted)], 
           M.fromList 
            [("a_0", Right "(b_1, c_2, d_3)"),
             ("c_2", Right "d_3")
            ]
          ),

      testCase "compatible levels" $
        testSolve
          ["a_0" ~ "b_1"]
          (M.fromList [typaram "a_0" 0 Unlifted])
          (M.fromList [tv "b_1" 1])
          ([], M.fromList [("b_1", Right "a_0")]),

      testCase "incompatible levels" $
        testSolveFail
          ["a_0" ~ "b_1"]
          (M.fromList [typaram "b_1" 1 Unlifted])
          (M.fromList [tv "a_0" 0])
          "Cannot unify type\n  b\nwith \"a\" (scope violation).\nThis is because \"b\" is rigidly bound in a deeper scope.",

      testCase "differently sized tuples" $
        testSolveFail
          ["a_0" ~ "(i32, c_2)", "b_1" ~ "(i32, c_2, bool)", "a_0" ~ "b_1"]
          mempty
          (M.fromList [tv "a_0" 0, tv "b_1" 0])
          "Cannot unify\n  (i32, c)\nwith\n  (i32, c, bool)"

      -- testCase "different array sizes" $
      --   testSolveFail
      --     ["a_0" ~ "[n]i32", "a_0" ~ "[n]i32"]
      --     mempty
      --     (M.fromList [tv "a_0" 0])
          -- "Cannot unify\n  [1]i32\nwith\n  [2]i32"
    ]
