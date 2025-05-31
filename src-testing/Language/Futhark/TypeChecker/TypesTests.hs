module Language.Futhark.TypeChecker.TypesTests (tests) where

import Data.Bifunctor
import Data.List (isInfixOf)
import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.FreshNames
import Futhark.Util.Pretty (docText, prettyTextOneLine)
import Language.Futhark
import Language.Futhark.Semantic
import Language.Futhark.SyntaxTests ()
import Language.Futhark.TypeChecker (initialEnv)
import Language.Futhark.TypeChecker.Monad
import Language.Futhark.TypeChecker.Names (resolveTypeExp)
import Language.Futhark.TypeChecker.Terms
import Language.Futhark.TypeChecker.Types
import Test.Tasty
import Test.Tasty.HUnit

evalTest :: TypeExp (ExpBase NoInfo Name) Name -> Either String ([VName], ResRetType) -> TestTree
evalTest te expected =
  testCase (prettyString te) $
    case (fmap (extract . fst) (run (checkTypeExp checkSizeExp =<< resolveTypeExp te)), expected) of
      (Left got_e, Left expected_e) ->
        let got_e_s = T.unpack $ docText $ prettyTypeError got_e
         in (expected_e `isInfixOf` got_e_s) @? got_e_s
      (Left got_e, Right _) ->
        let got_e_s = T.unpack $ docText $ prettyTypeError got_e
         in assertFailure $ "Failed: " <> got_e_s
      (Right actual_t, Right expected_t) ->
        actual_t @?= expected_t
      (Right actual_t, Left _) ->
        assertFailure $ "Expected error, got: " <> show actual_t
  where
    extract (_, svars, t, _) = (svars, t)
    run = snd . runTypeM env mempty (mkInitialImport "") (newNameSource 100)
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
          ([], "?[d_100].[d_100]i32")
        ),
        ( "[][]i32",
          ([], "?[d_100][d_101].[d_100][d_101]i32")
        ),
        ( "bool -> []i32",
          ([], "bool -> ?[d_100].[d_100]i32")
        ),
        ( "bool -> []f32 -> []i32",
          (["d_100"], "bool -> [d_100]f32 -> ?[d_101].[d_101]i32")
        ),
        ( "([]i32,[]i32)",
          ([], "?[d_100][d_101].([d_100]i32, [d_101]i32)")
        ),
        ( "{a:[]i32,b:[]i32}",
          ([], "?[d_100][d_101].{a:[d_100]i32, b:[d_101]i32}")
        ),
        ( "?[n].[n][n]bool",
          ([], "?[n_100].[n_100][n_100]bool")
        ),
        ( "([]i32 -> []i32) -> bool -> []i32",
          (["d_100"], "([d_100]i32 -> ?[d_101].[d_101]i32) -> bool -> ?[d_102].[d_102]i32")
        ),
        ( "((k: i64) -> [k]i32 -> [k]i32) -> []i32 -> bool",
          (["d_101"], "((k_100: i64) -> [k_100]i32 -> [k_100]i32) -> [d_101]i32 -> bool")
        ),
        ( "square [10]",
          ([], "[10][10]i32")
        ),
        ( "square []",
          ([], "?[d_100].[d_100][d_100]i32")
        ),
        ( "bool -> square []",
          ([], "bool -> ?[d_100].[d_100][d_100]i32")
        ),
        ( "(k: i64) -> square [k]",
          ([], "(k_100: i64) -> [k_100][k_100]i32")
        ),
        ( "fun i32 bool",
          ([], "i32 -> bool")
        ),
        ( "fun ([]i32) bool",
          ([], "?[d_100].[d_100]i32 -> bool")
        ),
        ( "fun bool ([]i32)",
          ([], "?[d_100].bool -> [d_100]i32")
        ),
        ( "bool -> fun ([]i32) bool",
          ([], "bool -> ?[d_100].[d_100]i32 -> bool")
        ),
        ( "bool -> fun bool ([]i32)",
          ([], "bool -> ?[d_100].bool -> [d_100]i32")
        ),
        ( "pair",
          ([], "?[n_100][m_101].([n_100]i64, [m_101]i64)")
        ),
        ( "(pair,pair)",
          ([], "?[n_100][m_101][n_102][m_103].(([n_100]i64, [m_101]i64), ([n_102]i64, [m_103]i64))")
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
  testCase (pretty_m <> ": " <> T.unpack (prettyTextOneLine t)) $
    applySubst (`M.lookup` m) t @?= expected
  where
    pretty_m = T.unpack $ prettyText $ map (first toName) $ M.toList m

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
