module Futhark.Internalise.TypesValuesTests (tests) where

import Data.Map qualified as M
import Futhark.IR.SyntaxTests ()
import Futhark.Internalise.TypesValues
import Test.Tasty
import Test.Tasty.HUnit

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

tests :: TestTree
tests =
  testGroup
    "Futhark.Internalise.TypesValuesTests"
    [ sumTypeTests
    ]
