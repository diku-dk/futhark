{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.IR.Prop.ReshapeTests
  ( tests,
  )
where

import Futhark.IR.Prop.Constants
import Futhark.IR.Prop.Reshape
import Futhark.IR.Syntax
import Futhark.IR.SyntaxTests ()
import Test.Tasty
import Test.Tasty.HUnit

intShape :: [Int] -> Shape
intShape = Shape . map (intConst Int32 . toInteger)

reshapeOuterTests :: [TestTree]
reshapeOuterTests =
  [ testCase (unwords ["reshapeOuter", show sc, show n, show shape, "==", show sc_res]) $
      reshapeOuter (intShape sc) n (intShape shape) @?= intShape sc_res
    | (sc, n, shape, sc_res) <-
        [ ([1], 1, [4, 3], [1, 3]),
          ([1], 2, [4, 3], [1]),
          ([2, 2], 1, [4, 3], [2, 2, 3]),
          ([2, 2], 2, [4, 3], [2, 2])
        ]
  ]

reshapeInnerTests :: [TestTree]
reshapeInnerTests =
  [ testCase (unwords ["reshapeInner", show sc, show n, show shape, "==", show sc_res]) $
      reshapeInner (intShape sc) n (intShape shape) @?= intShape sc_res
    | (sc, n, shape, sc_res) <-
        [ ([1], 1, [4, 3], [4, 1]),
          ([1], 0, [4, 3], [1]),
          ([2, 2], 1, [4, 3], [4, 2, 2]),
          ([2, 2], 0, [4, 3], [2, 2])
        ]
  ]

flipTests :: [TestTree]
flipTests =
  [ testCase
      ( unwords
          [ "flipReshapeRearrange",
            show v0_shape,
            show v1_shape,
            show perm
          ]
      )
      $ flipReshapeRearrange v0_shape v1_shape perm @?= res
    | (v0_shape :: [String], v1_shape, perm, res) <-
        [ ( ["A", "B", "C"],
            ["A", "BC"],
            [1, 0],
            Just [1, 2, 0]
          ),
          ( ["A", "B", "C", "D"],
            ["A", "BCD"],
            [1, 0],
            Just [1, 2, 3, 0]
          ),
          ( ["A"],
            ["B", "C"],
            [1, 0],
            Nothing
          ),
          ( ["A", "B", "C"],
            ["AB", "C"],
            [1, 0],
            Just [2, 0, 1]
          ),
          ( ["A", "B", "C", "D"],
            ["ABC", "D"],
            [1, 0],
            Just [3, 0, 1, 2]
          )
        ]
  ]

fuseTests :: [TestTree]
fuseTests =
  [ testCase (unwords ["fuseReshape", show old, show new]) $
      fuseReshape (uncurry (NewShape . Shape) old) (uncurry (NewShape . Shape) new)
        @?= uncurry (NewShape . Shape) <$> expected
    | ( old :: ([String], [DimSplice String]),
        new,
        expected
        ) <-
        [ ( (["AB"], [dimJoin 1 1 "AB"]),
            (["A", "B"], [dimSplit 1 ["A", "B"]]),
            Just
              ( ["A", "B"],
                [ dimJoin 1 1 "AB",
                  dimSplit 1 ["A", "B"]
                ]
              )
          ),
          ( (["A", "BC"], [dimSplit 1 ["A", "BC"]]),
            (["A", "B", "C"], [dimSplit 2 ["B", "C"]]),
            Just
              ( ["A", "B", "C"],
                [ dimSplit 1 ["A", "BC"],
                  dimSplit 2 ["B", "C"]
                ]
              )
          )
        ]
  ]
  where
    dimJoin i k w = DimSplice i k (Shape [w])
    dimSplit i ws = DimSplice i 1 (Shape ws)

tests :: TestTree
tests =
  testGroup "ReshapeTests" . mconcat $
    [ reshapeOuterTests,
      reshapeInnerTests,
      flipTests,
      fuseTests
    ]
