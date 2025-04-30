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

simplifyTests :: [TestTree]
simplifyTests =
  [ testCase (unwords ["simplifyNewShape", prettyString input]) $
      simplifyNewShape (Shape orig_shape) (uncurry (NewShape . Shape) input)
        @?= uncurry (NewShape . Shape) . (fst input,) <$> expected
    | (orig_shape :: [String], input, expected) <-
        [ -- Inverse flatten and unflatten - simple case.
          ( ["A", "B"],
            ( ["A", "B"],
              [dimJoin 0 2 "AB", dimSplit 0 ["A", "B"]]
            ),
            Just []
          ),
          -- Non-inverse flatten and unflatten - simple case.
          ( ["A", "B"],
            ( ["C", "D"],
              [dimJoin 0 2 "AB", dimSplit 0 ["C", "D"]]
            ),
            Nothing
          ),
          -- Inverse flatten and unflatten - separated by coercion.
          ( ["A", "B"],
            ( ["C", "D"],
              [ dimJoin 0 2 "AB",
                dimCoerce 0 "CD",
                dimSplit 0 ["C", "D"]
              ]
            ),
            Just
              [ dimJoin 0 2 "AB",
                dimSplit 0 ["C", "D"]
              ]
          ),
          -- Two unflattens - simple case.
          ( ["ABC"],
            (["A", "B", "C"], [dimSplit 0 ["A", "BC"], dimSplit 1 ["B", "C"]]),
            Just [dimSplit 0 ["A", "B", "C"]]
          ),
          -- Identity coerce (with non-identity stuff afterwards)
          ( ["A", "CD"],
            ( ["B", "C", "D"],
              [dimCoerce 0 "B", dimSplit 1 ["C", "D"]]
            ),
            Just [dimSplit 1 ["C", "D"]]
          ),
          -- Get rid of a coerce.
          ( ["CD"],
            ( ["A", "B"],
              [dimCoerce 0 "AB", dimSplit 0 ["A", "B"]]
            ),
            Just [dimSplit 0 ["A", "B"]]
          )
        ]
  ]
  where
    dimJoin i k w = DimSplice i k (Shape [w])
    dimSplit i ws = DimSplice i 1 (Shape ws)
    dimCoerce i w = DimSplice i 1 (Shape [w])

tests :: TestTree
tests =
  testGroup "ReshapeTests" . mconcat $
    [ reshapeOuterTests,
      reshapeInnerTests,
      flipTests,
      simplifyTests
    ]
