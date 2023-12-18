module Futhark.Optimise.MemoryBlockMerging.GreedyColoringTests
  ( tests,
  )
where

import Control.Arrow ((***))
import Data.Function ((&))
import Data.Map qualified as M
import Data.Set qualified as S
import Futhark.Optimise.MemoryBlockMerging.GreedyColoring qualified as GreedyColoring
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "GreedyColoringTests"
    [psumTest, allIntersect, emptyGraph, noIntersections, differentSpaces]

psumTest :: TestTree
psumTest =
  testCase "psumTest"
    $ assertEqual
      "Color simple 1-2-3 using two colors"
      ( [(0, "shared"), (1, "shared")] :: [(Int, String)],
        [(1 :: Int, 0), (2, 1), (3, 0)]
      )
    $ (M.toList *** M.toList)
    $ GreedyColoring.colorGraph
      (M.fromList [(1, "shared"), (2, "shared"), (3, "shared")])
    $ S.fromList [(1, 2), (2, 3)]

allIntersect :: TestTree
allIntersect =
  testCase "allIntersect"
    $ assertEqual
      "Color a graph where all values intersect"
      ( [(0, "shared"), (1, "shared"), (2, "shared")] :: [(Int, String)],
        [(1 :: Int, 2), (2, 1), (3, 0)]
      )
    $ (M.toList *** M.toList)
    $ GreedyColoring.colorGraph
      (M.fromList [(1, "shared"), (2, "shared"), (3, "shared")])
    $ S.fromList [(1, 2), (2, 3), (1, 3)]

emptyGraph :: TestTree
emptyGraph =
  testCase "emptyGraph"
    $ assertEqual
      "Color an empty graph"
      ([] :: [(Int, Char)], [] :: [(Int, Int)])
    $ (M.toList *** M.toList)
    $ GreedyColoring.colorGraph M.empty
    $ S.fromList []

noIntersections :: TestTree
noIntersections =
  GreedyColoring.colorGraph
    (M.fromList [(1, "shared"), (2, "shared"), (3, "shared")])
    (S.fromList [])
    & M.toList *** M.toList
    & assertEqual
      "Color nodes with no intersections"
      ( [(0, "shared")] :: [(Int, String)],
        [(1, 0), (2, 0), (3, 0)] :: [(Int, Int)]
      )
    & testCase "noIntersections"

differentSpaces :: TestTree
differentSpaces =
  GreedyColoring.colorGraph
    (M.fromList [(1, "a"), (2, "b"), (3, "c")])
    (S.fromList [])
    & M.toList *** M.toList
    & assertEqual
      "Color nodes with no intersections but in different spaces"
      ( [(0, "c"), (1, "b"), (2, "a")] :: [(Int, String)],
        [(1, 2), (2, 1), (3, 0)] :: [(Int, Int)]
      )
    & testCase "differentSpaces"
