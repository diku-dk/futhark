module Futhark.Internalise.TypesValuesTests (tests) where

import Data.Map qualified as M
import Futhark.IR.Syntax
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
              [ ("foo", [["i64"]]),
                ("bar", [["i64"]])
              ]
          )
          @?= ( [["i64"]],
                M.fromList
                  [ ("foo", (1, [0])),
                    ("bar", (0, [0]))
                  ]
              ),
      testCase "Dedup of array" $
        internaliseConstructors
          ( M.fromList
              [ ("foo", [["[?0]i64"]]),
                ("bar", [["[?0]i64"]])
              ]
          )
          @?= ( [["[?0]i64"]],
                M.fromList
                  [ ("foo", (1, [0])),
                    ("bar", (0, [0]))
                  ]
              ),
      testCase
        "Dedup of array of tuple"
        $ internaliseConstructors
          ( M.fromList
              [ ("foo", [["[?0]i64", "[?0]i64"]]),
                ("bar", [["[?0]i64"]])
              ]
          )
          @?= ( [["[?0]i64"], ["[?0]i64", "[?0]i64"]],
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
        [["[0i64]i32"]]
        [[("[?0]i32", RetAls [0] [0])]],
      mkTest
        [["[0i64]i32", "[0i64]i32"]]
        [ [ ("[?0]i32", RetAls [0] [0]),
            ("[?0]i32", RetAls [1] [1])
          ]
        ],
      mkTest
        [["[0i64]i32"], ["[0i64]i32", "[0i64]i32"]]
        [ [ ("[?0]i32", RetAls [1] [0]),
            ("[?0]i32", RetAls [2] [1])
          ]
        ],
      mkTest
        [["[0i64][1i64]i32", "[0i64][1i64]i32"]]
        [ [ ("[?0]i32", RetAls [0] [0]),
            ("[?0]i32", RetAls [1] [1])
          ]
        ],
      mkTest
        [["[0i64][1i64]i32", "[0i64]i32"]]
        [ [ ("[?0]i32", RetAls [] [0]),
            ("[?0]i32", RetAls [] [1])
          ]
        ],
      mkTest
        [["[n_0][n_1]i32", "[n_0][n_1]i32"]]
        [ [("[?0]i32", RetAls [0, 1] [0, 1])],
          [("[?0]i32", RetAls [0, 1] [0, 1])]
        ],
      mkTest
        [["*[n_0][n_1]i32"], ["[n_2]i64"], ["[n_3]i64"]]
        [[("*[n_0][n_1]i32", RetAls [] [])]],
      mkTest
        [["[n_0]i32", "[n_0][n_1]i32"]]
        [[("[n_0]i32", RetAls [0, 1] [0])]],
      mkTest
        []
        [ [("[n_0]i32", RetAls [] [0, 2]), ("[n_0][n_1]i32", RetAls [] [1, 2])],
          [("[n_0]i32", RetAls [] [0, 1, 2])]
        ]
    ]
  where
    mkTest all_param_ts expected =
      testCase (prettyString (all_param_ts, all_res_ts)) $
        inferAliases all_param_ts all_res_ts @?= expected
      where
        all_res_ts = map (map fst) expected

tests :: TestTree
tests =
  testGroup
    "Futhark.Internalise.TypesValuesTests"
    [ internaliseTypeTests,
      sumTypeTests,
      inferAliasesTests
    ]
