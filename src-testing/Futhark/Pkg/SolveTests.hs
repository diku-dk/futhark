module Futhark.Pkg.SolveTests (tests) where

import Data.Map qualified as M
import Data.Monoid
import Data.Text qualified as T
import Futhark.Pkg.Solve
import Futhark.Pkg.Types
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

semverE :: T.Text -> SemVer
semverE s = case parseVersion s of
  Left err ->
    error $
      T.unpack s
        <> " is not a valid version number: "
        <> errorBundlePretty err
  Right x -> x

-- | A world of packages and interdependencies for testing the solver
-- without touching the outside world.
testEnv :: PkgRevDepInfo
testEnv =
  M.fromList $
    concatMap
      frob
      [ ( "athas",
          [ ( "foo",
              [ ("0.1.0", []),
                ("0.2.0", [("athas/bar", "1.0.0")]),
                ("0.3.0", [])
              ]
            ),
            ("foo@v2", [("2.0.0", [("athas/quux", "0.1.0")])]),
            ("bar", [("1.0.0", [])]),
            ("baz", [("0.1.0", [("athas/foo", "0.3.0")])]),
            ( "quux",
              [ ( "0.1.0",
                  [ ("athas/foo", "0.2.0"),
                    ("athas/baz", "0.1.0")
                  ]
                )
              ]
            ),
            ( "quux_perm",
              [ ( "0.1.0",
                  [ ("athas/baz", "0.1.0"),
                    ("athas/foo", "0.2.0")
                  ]
                )
              ]
            ),
            ("x_bar", [("1.0.0", [("athas/bar", "1.0.0")])]),
            ("x_foo", [("1.0.0", [("athas/foo", "0.3.0")])]),
            ( "tricky",
              [ ( "1.0.0",
                  [ ("athas/foo", "0.2.0"),
                    ("athas/x_foo", "1.0.0")
                  ]
                )
              ]
            )
          ]
        ),
        -- Some mutually recursive packages.
        ( "nasty",
          [ ("foo", [("1.0.0", [("nasty/bar", "1.0.0")])]),
            ("bar", [("1.0.0", [("nasty/foo", "1.0.0")])])
          ]
        )
      ]
  where
    frob (user, repos) = do
      (repo, repo_revs) <- repos
      (rev, deps) <- repo_revs
      let rev' = semverE rev
          onDep (dp, dv) = (dp, (semverE dv, Nothing))
          deps' = PkgRevDeps $ M.fromList $ map onDep deps
      pure ((user <> "/" <> repo, rev'), deps')

newtype SolverRes = SolverRes BuildList
  deriving (Eq)

instance Show SolverRes where
  show (SolverRes bl) = T.unpack $ prettyBuildList bl

solverTest :: PkgPath -> T.Text -> Either T.Text [(PkgPath, T.Text)] -> TestTree
solverTest p v expected =
  testCase (T.unpack $ p <> "-" <> prettySemVer v') $
    fmap SolverRes (solveDepsPure testEnv target)
      @?= expected'
  where
    target = PkgRevDeps $ M.singleton p (v', Nothing)
    v' = semverE v
    expected' = SolverRes . BuildList . M.fromList . map onRes <$> expected
    onRes (dp, dv) = (dp, semverE dv)

tests :: TestTree
tests =
  testGroup
    "SolveTests"
    [ solverTest "athas/foo" "0.1.0" $
        Right [("athas/foo", "0.1.0")],
      solverTest "athas/foo" "0.2.0" $
        Right
          [ ("athas/foo", "0.2.0"),
            ("athas/bar", "1.0.0")
          ],
      solverTest "athas/quux" "0.1.0" $
        Right
          [ ("athas/quux", "0.1.0"),
            ("athas/foo", "0.3.0"),
            ("athas/baz", "0.1.0")
          ],
      solverTest "athas/quux_perm" "0.1.0" $
        Right
          [ ("athas/quux_perm", "0.1.0"),
            ("athas/foo", "0.3.0"),
            ("athas/baz", "0.1.0")
          ],
      solverTest "athas/foo@v2" "2.0.0" $
        Right
          [ ("athas/foo@v2", "2.0.0"),
            ("athas/quux", "0.1.0"),
            ("athas/foo", "0.3.0"),
            ("athas/baz", "0.1.0")
          ],
      solverTest "athas/foo@v3" "3.0.0" $
        Left "Unknown package/version: athas/foo@v3-3.0.0",
      solverTest "nasty/foo" "1.0.0" $
        Right
          [ ("nasty/foo", "1.0.0"),
            ("nasty/bar", "1.0.0")
          ],
      solverTest "athas/tricky" "1.0.0" $
        Right
          [ ("athas/tricky", "1.0.0"),
            ("athas/foo", "0.3.0"),
            ("athas/x_foo", "1.0.0")
          ]
    ]
