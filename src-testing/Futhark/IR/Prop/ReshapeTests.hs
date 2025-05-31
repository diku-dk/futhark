{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.IR.Prop.ReshapeTests
  ( tests,
  )
where

import Data.List qualified as L
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

dimFlatten :: Int -> Int -> d -> DimSplice d
dimFlatten i k w = DimSplice i k (Shape [w])

dimUnflatten :: Int -> [d] -> DimSplice d
dimUnflatten i ws = DimSplice i 1 (Shape ws)

dimCoerce :: Int -> d -> DimSplice d
dimCoerce i w = DimSplice i 1 (Shape [w])

dimSplice :: Int -> Int -> [d] -> DimSplice d
dimSplice i n s = DimSplice i n $ Shape s

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
            prettyStringOneLine newshape
          ]
      )
      $ flipRearrangeReshape perm newshape @?= res
    | (perm, newshape :: NewShape String, res) <-
        [ ( [1, 0],
            NewShape
              [dimUnflatten 1 ["B", "C"]]
              (Shape ["A", "B", "C"]),
            Just
              ( NewShape
                  [dimUnflatten 0 ["B", "C"]]
                  (Shape ["B", "C", "A"]),
                [2, 0, 1]
              )
          ),
          ( [1, 0],
            NewShape
              [dimFlatten 0 2 "AB"]
              (Shape ["AB"]),
            Nothing
          )
        ]
  ]

simplifyTests :: TestTree
simplifyTests =
  testGroup
    "simplifyNewShape"
    [ testCase "Inverse flatten and unflatten - simple case" $
        lhs
          ["A", "B"]
          [dimFlatten 0 2 "AB", dimUnflatten 0 ["A", "B"]]
          @?= Just [],
      testCase "Non-inverse flatten and unflatten - simple case" $
        lhs
          ["A", "B"]
          [dimFlatten 0 2 "AB", dimUnflatten 0 ["C", "D"]]
          @?= Just [dimSplice 0 2 ["C", "D"]],
      testCase "Inverse flatten and unflatten - separated by coercion" $
        lhs
          ["A", "B"]
          [ dimFlatten 0 2 "AB",
            dimCoerce 0 "CD",
            dimUnflatten 0 ["C", "D"]
          ]
          @?= Just [dimSplice 0 2 ["C", "D"]],
      testCase "Two unflattens - simple case" $
        lhs
          ["ABC"]
          [dimUnflatten 0 ["A", "BC"], dimUnflatten 1 ["B", "C"]]
          @?= Just [dimUnflatten 0 ["A", "B", "C"]],
      testCase "Two unflattens with unchanged prefix" $
        lhs
          ["A", "B", "C", "D", "E"]
          [ DimSplice 3 2 $ Shape ["DE"],
            DimSplice 2 2 $ Shape ["CDE"]
          ]
          @?= Just [dimFlatten 2 3 "CDE"],
      testCase "Identity coerce" $
        lhs
          ["A", "B", "C"]
          [dimCoerce 1 "B", dimCoerce 2 "C"]
          @?= Just [],
      testCase "Identity coerce (multiple dimensions)" $
        lhs
          ["A", "B", "C"]
          [DimSplice 1 2 (Shape ["B", "C"])]
          @?= Just [],
      testCase "Identity coerce (with non-identity stuff afterwards)" $
        lhs
          ["B", "CD"]
          [dimCoerce 0 "B", dimUnflatten 1 ["C", "D"]]
          @?= Just [dimUnflatten 1 ["C", "D"]],
      testCase "Get rid of a coerce before an unflatten" $
        lhs
          ["CD"]
          [dimCoerce 0 "AB", dimUnflatten 0 ["A", "B"]]
          @?= Just [dimUnflatten 0 ["A", "B"]],
      testCase "Get rid of a coerce after a flatten" $
        lhs
          ["A", "B", "C"]
          [dimFlatten 0 2 "ABC", dimCoerce 0 "K"]
          @?= Just [dimFlatten 0 2 "K"],
      testCase "Flatten and unflatten (invariant suffix)" $
        lhs
          ["A", "B", "C"]
          [dimFlatten 0 3 "ABC", dimUnflatten 0 ["D", "E", "C"]]
          @?= Just [dimSplice 0 2 ["D", "E"]],
      testCase "Flatten and unflatten (invariant prefix)" $
        lhs
          ["A", "B", "C"]
          [dimFlatten 0 3 "ABC", dimUnflatten 0 ["A", "D", "E"]]
          @?= Just [dimSplice 1 2 ["D", "E"]],
      testCase "Invariant part of splice" $
        lhs
          ["A", "B", "C", "D"]
          [DimSplice 1 3 $ Shape ["BC", "D"]]
          @?= Just [DimSplice 1 2 $ Shape ["BC"]],
      testCase "Necessary coercion" $
        lhs
          ["A", "B"]
          [dimCoerce 0 "C", dimCoerce 1 "D"]
          @?= Nothing,
      testCase "Another necessary coercion" $
        lhs
          ["A", "B", "C"]
          [dimCoerce 0 "A'", dimCoerce 1 "A'", dimCoerce 2 "A'"]
          @?= Nothing,
      testCase "Long with redundancies" $
        lhs
          ["A", "B", "C", "D"]
          [ DimSplice 1 3 $ Shape ["BC", "D"],
            dimCoerce 1 "BC",
            dimCoerce 2 "D",
            dimFlatten 1 2 "BCD",
            dimFlatten 0 2 "ABCD"
          ]
          @?= Just [dimFlatten 0 4 "ABCD"]
    ]
  where
    lhs orig_shape ss =
      let res_shape :: ShapeBase String =
            L.foldl' applySplice (Shape orig_shape) ss
       in dimSplices
            <$> simplifyNewShape (Shape orig_shape) (NewShape ss res_shape)

tests :: TestTree
tests =
  testGroup "ReshapeTests" . mconcat $
    [ reshapeOuterTests,
      reshapeInnerTests,
      flipReshapeRearrangeTests,
      flipRearrangeReshapeTests,
      [simplifyTests]
    ]
