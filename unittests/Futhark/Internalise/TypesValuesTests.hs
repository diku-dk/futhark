module Futhark.Internalise.TypesValuesTests (tests) where

import Control.Monad.Free (Free (..))
import Data.Map qualified as M
import Data.String (fromString)
import Futhark.IR.Syntax hiding (Free)
import Futhark.IR.SyntaxTests ()
import Futhark.Internalise.TypesValues
import Language.Futhark.SyntaxTests ()
import Test.Tasty
import Test.Tasty.HUnit

internaliseTypeTests :: TestTree
internaliseTypeTests =
  testGroup
    "internaliseType"
    [ mkTest
        "[0]()"
        [["[0i64]unit"]],
      mkTest
        "{a: [t_7447][n_7448](f32, f32), b: i64, c: i64}"
        [["[t_7447][n_7448]f32", "[t_7447][n_7448]f32"], ["i64"], ["i64"]],
      mkTest
        "([0]i32, {a: f32, b: f32, c: f32, d: [0]((f32, f32), (f32, f32))})"
        [ ["[0i64]i32"],
          ["f32"],
          ["f32"],
          ["f32"],
          ["[0i64]f32", "[0i64]f32", "[0i64]f32", "[0i64]f32"]
        ],
      mkTest
        "[0]([1]i32, f32)"
        [["[0i64][1i64]i32", "[0i64]f32"]]
    ]
  where
    mkTest x y =
      testCase (prettyString x) $ internaliseType x @?= y

sumTypeTests :: TestTree
sumTypeTests =
  testGroup
    "internaliseConstructors"
    [ testCase "Dedup of primitives" $
        internaliseConstructors
          ( M.fromList
              [ ("foo", [Pure "i64"]),
                ("bar", [Pure "i64"])
              ]
          )
          @?= ( [Pure "i64"],
                M.fromList
                  [ ("foo", (1, [0])),
                    ("bar", (0, [0]))
                  ]
              ),
      testCase "Dedup of array" $
        internaliseConstructors
          ( M.fromList
              [ ("foo", [Pure "[?0]i64"]),
                ("bar", [Pure "[?0]i64"])
              ]
          )
          @?= ( [Pure "[?0]i64"],
                M.fromList
                  [ ("foo", (1, [0])),
                    ("bar", (0, [0]))
                  ]
              ),
      testCase
        "Dedup of array of tuple"
        $ internaliseConstructors
          ( M.fromList
              [ ("foo", [Free [Pure "[?0]i64", Pure "[?0]i64"]]),
                ("bar", [Pure "[?0]i64"])
              ]
          )
          @?= ( [Pure "[?0]i64", Free [Pure "[?0]i64", Pure "[?0]i64"]],
                M.fromList
                  [ ("foo", (1, [1, 2])),
                    ("bar", (0, [0]))
                  ]
              )
    ]

-- Be aware that some of these tests simply reinforce current
-- behaviour - it may be that we want to restrict aliasing even
-- further in the future; these tests would have to be updated in such
-- cases.
inferAliasesTests :: TestTree
inferAliasesTests =
  testGroup
    "inferAliases"
    [ mkTest
        [Free [Pure "[0i64]i32"]]
        [Free [Pure "[?0]i32"]]
        [[("[?0]i32", RetAls [0] [0])]],
      mkTest
        [Free [Pure "[0i64]i32", Pure "[0i64]i32"]]
        [Free [Pure "[0i64]i32", Pure "[0i64]i32"]]
        [ [ ("[0i64]i32", RetAls [0] [0]),
            ("[0i64]i32", RetAls [1] [1])
          ]
        ],
      -- Basically zip.
      mkTest
        [Free [Pure "[n_0]i32"], Free [Pure "[n_0]i32"]]
        [Free [Pure "[n_0]i32", Pure "[n_0]i32"]]
        [ [ ("[n_0]i32", RetAls [] [0]),
            ("[n_0]i32", RetAls [] [1])
          ]
        ],
      mkTest
        [Free [Pure "[0i64]i32"], Free [Pure "[0i64]i32", Pure "[0i64]i32"]]
        [Free [Pure "[?0]i32", Pure "[?0]i32"]]
        [ [ ("[?0]i32", RetAls [1] [0]),
            ("[?0]i32", RetAls [2] [1])
          ]
        ],
      mkTest
        [Free [Free [Pure "[0i64][1i64]i32", Pure "[0i64][1i64]i32"]]]
        [Free [Pure "[?0]i32", Pure "[?0]i32"]]
        [ [ ("[?0]i32", RetAls [0] [0]),
            ("[?0]i32", RetAls [1] [1])
          ]
        ],
      mkTest
        [Free [Free [Pure "[n_0][n_1]i32"], Free [Pure "[n_0][n_1]i32"]]]
        [Free [Pure "[?0]i32"], Free [Pure "[?0]i32"]]
        [ [("[?0]i32", RetAls [0, 1] [0, 1])],
          [("[?0]i32", RetAls [0, 1] [0, 1])]
        ],
      mkTest
        [ Free [Free [Pure "*[n_0][n_1]i32"]],
          Free [Pure "[n_2]i64"],
          Free [Pure "[n_3]i64"]
        ]
        [Free [Free [Pure "*[n_0][n_1]i32"]]]
        [[("*[n_0][n_1]i32", RetAls [] [])]],
      mkTest
        [Free [Pure "[n_0]i32", Free [Free [Pure "[n_0][n_1]i32"]]]]
        [Free [Pure "[n_0]i32"]]
        [[("[n_0]i32", RetAls [1] [0])]],
      mkTest
        []
        [ Free [Pure "[n_0]i32", Free [Pure "[n_0][n_1]i32"]],
          Free [Pure "[n_0]i32"]
        ]
        [ [("[n_0]i32", RetAls [] [0]), ("[n_0][n_1]i32", RetAls [] [1, 2])],
          [("[n_0]i32", RetAls [] [1, 2])]
        ]
    ]
  where
    mkTest all_param_ts all_res_ts expected =
      testCase (show all_param_ts <> " " <> show all_res_ts) $
        inferAliases
          (map (fmap fromString) all_param_ts)
          (map (fmap fromString) all_res_ts)
          @?= expected

tests :: TestTree
tests =
  testGroup
    "Futhark.Internalise.TypesValuesTests"
    [ internaliseTypeTests,
      sumTypeTests,
      inferAliasesTests
    ]
