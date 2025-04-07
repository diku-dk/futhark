module Language.Futhark.TypeChecker.TySolveTests (tests) where

import Data.Map qualified as M
import Futhark.Util.Pretty (docString)
import Language.Futhark.Syntax
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
      --
      testCase "a_0 ~ b_1" $
        testSolve
          ["a_0" ~ "b_1"]
          mempty
          (M.fromList [tv "a_0" 0])
          ([], M.fromList [("a_0", Right "b_1")]),
      --
      testCase "infer unlifted" $
        testSolve
          [ CtEq (ReasonBranches noLoc "t_9895" "t_9896") "t_9895" "if_t_9897",
            CtEq (ReasonBranches noLoc "t_9895" "t_9896") (Scalar "t_9896") "if_t_9897",
            "if_t_9897" ~ "res_42"
          ]
          mempty
          ( M.fromList
              [ ("t_9895", (2, TyVarFree noLoc Lifted)),
                ("t_9896", (3, TyVarFree noLoc Lifted)),
                ("if_t_9897", (4, TyVarFree noLoc SizeLifted)),
                ("res_42", (1, TyVarFree noLoc Lifted))
              ]
          )
          ( [("res_42", SizeLifted)],
            M.fromList
              [ ("t_9895", Right "res_42"),
                ("t_9896", Right "res_42"),
                ("if_t_9897", Right "res_42")
              ]
          )
    ]
