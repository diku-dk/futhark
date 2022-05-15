{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Futhark.TypeChecker.TypesTests (tests) where

import Data.Bifunctor (first)
import Data.List (isInfixOf)
import qualified Data.Map as M
import Futhark.FreshNames
import Futhark.Util.Pretty (prettyOneLine)
import Language.Futhark
import Language.Futhark.Semantic
import Language.Futhark.SyntaxTests ()
import Language.Futhark.TypeChecker (initialEnv)
import Language.Futhark.TypeChecker.Monad
import Language.Futhark.TypeChecker.Types
import Test.Tasty
import Test.Tasty.HUnit

evalTest :: TypeExp Name -> Either String ([VName], StructRetType) -> TestTree
evalTest te expected =
  testCase (pretty te) $
    case (fmap (extract . fst) (run (checkTypeExp te)), expected) of
      (Left got_e, Left expected_e) ->
        (expected_e `isInfixOf` pretty got_e) @? pretty got_e
      (Left got_e, Right _) ->
        assertFailure $ "Failed: " <> pretty got_e
      (Right actual_t, Right expected_t) ->
        actual_t @?= expected_t
      (Right actual_t, Left _) ->
        assertFailure $ "Expected error, got: " <> show actual_t
  where
    extract (_, svars, t, _) = (svars, t)
    run = snd . runTypeM env mempty (mkInitialImport "") blankNameSource
    -- We hack up an environment with some predefined type
    -- abbreviations for testing.  This is all pretty sensitive to the
    -- specific unique names, so we have to be careful!
    env =
      initialEnv
        { envTypeTable =
            M.fromList
              [ ( "square_1000",
                  TypeAbbr
                    Unlifted
                    [TypeParamDim "n_1001" mempty]
                    "[n_1001][n_1001]i32"
                ),
                ( "fun_1100",
                  TypeAbbr
                    Lifted
                    [ TypeParamType Lifted "a_1101" mempty,
                      TypeParamType Lifted "b_1102" mempty
                    ]
                    "a_1101 -> b_1102"
                ),
                ( "pair_1200",
                  TypeAbbr
                    SizeLifted
                    []
                    "?[n_1201][m_1202].([n_1201]i64, [m_1202]i64)"
                )
              ]
              <> envTypeTable initialEnv,
          envNameMap =
            M.fromList
              [ ((Type, "square"), "square_1000"),
                ((Type, "fun"), "fun_1100"),
                ((Type, "pair"), "pair_1200")
              ]
              <> envNameMap initialEnv
        }

evalTests :: TestTree
evalTests =
  testGroup
    "Type expression elaboration"
    [ testGroup "Positive tests" (map mkPos pos),
      testGroup "Negative tests" (map mkNeg neg)
    ]
  where
    mkPos (x, y) = evalTest x (Right y)
    mkNeg (x, y) = evalTest x (Left y)
    pos =
      [ ( "[]i32",
          ([], "?[d_0].[d_0]i32")
        ),
        ( "[][]i32",
          ([], "?[d_0][d_1].[d_0][d_1]i32")
        ),
        ( "bool -> []i32",
          ([], "bool -> ?[d_0].[d_0]i32")
        ),
        ( "bool -> []f32 -> []i32",
          (["d_0"], "bool -> [d_0]f32 -> ?[d_1].[d_1]i32")
        ),
        ( "([]i32,[]i32)",
          ([], "?[d_0][d_1].([d_0]i32, [d_1]i32)")
        ),
        ( "{a:[]i32,b:[]i32}",
          ([], "?[d_0][d_1].{a:[d_0]i32, b:[d_1]i32}")
        ),
        ( "?[n].[n][n]bool",
          ([], "?[n_0].[n_0][n_0]bool")
        ),
        ( "([]i32 -> []i32) -> bool -> []i32",
          (["d_0"], "([d_0]i32 -> ?[d_1].[d_1]i32) -> bool -> ?[d_2].[d_2]i32")
        ),
        ( "((k: i64) -> [k]i32 -> [k]i32) -> []i32 -> bool",
          (["d_1"], "((k_0: i64) -> [k_0]i32 -> [k_0]i32) -> [d_1]i32 -> bool")
        ),
        ( "square [10]",
          ([], "[10][10]i32")
        ),
        ( "square []",
          ([], "?[d_0].[d_0][d_0]i32")
        ),
        ( "bool -> square []",
          ([], "bool -> ?[d_0].[d_0][d_0]i32")
        ),
        ( "(k: i64) -> square [k]",
          ([], "(k_0: i64) -> [k_0][k_0]i32")
        ),
        ( "fun i32 bool",
          ([], "i32 -> bool")
        ),
        ( "fun ([]i32) bool",
          ([], "?[d_0].[d_0]i32 -> bool")
        ),
        ( "fun bool ([]i32)",
          ([], "?[d_0].bool -> [d_0]i32")
        ),
        ( "bool -> fun ([]i32) bool",
          ([], "bool -> ?[d_0].[d_0]i32 -> bool")
        ),
        ( "bool -> fun bool ([]i32)",
          ([], "bool -> ?[d_0].bool -> [d_0]i32")
        ),
        ( "pair",
          ([], "?[n_0][m_1].([n_0]i64, [m_1]i64)")
        ),
        ( "(pair,pair)",
          ([], "?[n_0][m_1][n_2][m_3].(([n_0]i64, [m_1]i64), ([n_2]i64, [m_3]i64))")
        )
      ]
    neg =
      [ ("?[n].bool", "Existential size \"n\""),
        ("?[n].bool -> [n]bool", "Existential size \"n\""),
        ("?[n].[n]bool -> [n]bool", "Existential size \"n\""),
        ("?[n].[n]bool -> bool", "Existential size \"n\"")
      ]

substTest :: M.Map VName (Subst StructRetType) -> StructRetType -> StructRetType -> TestTree
substTest m t expected =
  testCase (pretty_m <> ": " <> prettyOneLine t) $
    applySubst (`M.lookup` m) t @?= expected
  where
    pretty_m = prettyOneLine $ map (first prettyName) $ M.toList m

-- Some of these tests may be a bit fragile, in that they depend on
-- internal renumbering, which can be arbitrary.
substTests :: TestTree
substTests =
  testGroup
    "Type substitution"
    [ substTest m0 "t_0" "i64",
      substTest m0 "[1]t_0" "[1]i64",
      substTest m0 "?[n_10].[n_10]t_0" "?[n_10].[n_10]i64",
      --
      substTest m1 "t_0" "?[n_1].[n_1]bool",
      substTest m1 "f32 -> t_0" "f32 -> ?[n_1].[n_1]bool",
      substTest m1 "f32 -> f64 -> t_0" "f32 -> f64 -> ?[n_1].[n_1]bool",
      substTest m1 "f32 -> t_0 -> bool" "?[n_1].f32 -> [n_1]bool -> bool",
      substTest m1 "f32 -> t_0 -> t_0" "?[n_1].f32 -> [n_1]bool -> ?[n_2].[n_2]bool"
    ]
  where
    m0 =
      M.fromList [("t_0", Subst [] "i64")]

    m1 =
      M.fromList [("t_0", Subst [] "?[n_1].[n_1]bool")]

tests :: TestTree
tests = testGroup "Basic type operations" [evalTests, substTests]
