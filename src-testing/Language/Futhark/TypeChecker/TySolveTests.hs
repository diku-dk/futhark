module Language.Futhark.TypeChecker.TySolveTests (tests) where

import Data.Loc (Loc (NoLoc))
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
          [ "t\8320_9896" ~ "if_t\8322_9898",
            "t\8321_9897" ~ "if_t\8322_9898",
            "t\8323_9899" ~ "if_t\8322_9898"
          ]
          mempty
          ( M.fromList
              [ ("t\8320_9896", (2, TyVarFree NoLoc Lifted)),
                ("t\8321_9897", (3, TyVarFree NoLoc Lifted)),
                ("if_t\8322_9898", (4, TyVarFree NoLoc SizeLifted)),
                ("t\8323_9899", (5, TyVarFree NoLoc Lifted))
              ]
          )
          ( [("if_t\8322_9898", SizeLifted)],
            M.fromList
              [ ("t\8320_9896", Right "if_t\8322_9898"),
                ("t\8321_9897", Right "if_t\8322_9898"),
                ("t\8323_9899", Right "if_t\8322_9898")
              ]
          )
    ]
