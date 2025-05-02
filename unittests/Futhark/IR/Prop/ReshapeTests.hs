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

dimJoin :: Int -> Int -> d -> DimSplice d
dimJoin i k w = DimSplice i k (Shape [w])

dimSplit :: Int -> [d] -> DimSplice d
dimSplit i ws = DimSplice i 1 (Shape ws)

dimCoerce :: Int -> d -> DimSplice d
dimCoerce i w = DimSplice i 1 (Shape [w])

flipReshapeRearrangeTests :: [TestTree]
flipReshapeRearrangeTests =
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

flipRearrangeReshapeTests :: [TestTree]
flipRearrangeReshapeTests =
  [ testCase
      ( unwords
          [ "flipRearrangeReshape",
            show perm,
            prettyString newshape
          ]
      )
      $ flipRearrangeReshape perm newshape @?= res
    | (perm, newshape :: NewShape String, res) <-
        [ ( [1, 0],
            NewShape
              [dimSplit 1 ["B", "C"]]
              (Shape ["A", "B", "C"]),
            Just
              ( NewShape
                  [dimSplit 0 ["B", "C"]]
                  (Shape ["B", "C", "A"]),
                [2, 0, 1]
              )
          ),
          ( [1, 0],
            NewShape
              [dimJoin 0 2 "AB"]
              (Shape ["AB"]),
            Nothing
          )
        ]
  ]

simplifyTests :: [TestTree]
simplifyTests =
  [ testCase
      ( unwords
          [ "simplifyNewShape",
            prettyString orig_shape,
            prettyString input
          ]
      )
      $ simplifyNewShape (Shape orig_shape) (NewShape (fst input) (Shape (snd input)))
        @?= uncurry NewShape . (,Shape (snd input)) <$> expected
    | (orig_shape :: [String], input, expected) <-
        [ -- Inverse flatten and unflatten - simple case.
          ( ["A", "B"],
            ( [dimJoin 0 2 "AB", dimSplit 0 ["A", "B"]],
              ["A", "B"]
            ),
            Just []
          ),
          -- Non-inverse flatten and unflatten - simple case.
          ( ["A", "B"],
            ( [dimJoin 0 2 "AB", dimSplit 0 ["C", "D"]],
              ["C", "D"]
            ),
            Nothing
          ),
          -- Inverse flatten and unflatten - separated by coercion.
          ( ["A", "B"],
            ( [ dimJoin 0 2 "AB",
                dimCoerce 0 "CD",
                dimSplit 0 ["C", "D"]
              ],
              ["C", "D"]
            ),
            Just
              [ dimJoin 0 2 "AB",
                dimSplit 0 ["C", "D"]
              ]
          ),
          -- Two unflattens - simple case.
          ( ["ABC"],
            ( [dimSplit 0 ["A", "BC"], dimSplit 1 ["B", "C"]],
              ["A", "B", "C"]
            ),
            Just [dimSplit 0 ["A", "B", "C"]]
          ),
          -- Identity coerce (with non-identity stuff afterwards)
          ( ["B", "CD"],
            ( [dimCoerce 0 "B", dimSplit 1 ["C", "D"]],
              ["B", "C", "D"]
            ),
            Just [dimSplit 1 ["C", "D"]]
          ),
          -- Get rid of a coerce.
          ( ["CD"],
            ( [dimCoerce 0 "AB", dimSplit 0 ["A", "B"]],
              ["A", "B"]
            ),
            Just [dimSplit 0 ["A", "B"]]
          ),
          -- Don't get rid of anything here.
          ( ["A", "B"],
            ( [dimCoerce 0 "C", dimCoerce 1 "D"],
              ["C", "D"]
            ),
            Nothing
          )
        ]
  ]

tests :: TestTree
tests =
  testGroup "ReshapeTests" . mconcat $
    [ reshapeOuterTests,
      reshapeInnerTests,
      flipReshapeRearrangeTests,
      flipRearrangeReshapeTests,
      simplifyTests
    ]
