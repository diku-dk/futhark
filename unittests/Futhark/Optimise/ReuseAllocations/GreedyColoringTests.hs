module Futhark.Optimise.ReuseAllocations.GreedyColoringTests
  ( tests,
  )
where

import Control.Arrow ((***))
import Data.Function ((&))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Futhark.Optimise.ReuseAllocations.GreedyColoring as GreedyColoring
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "GreedyColoringTests"
    [psumTest, allIntersect, emptyGraph, noIntersections, differentSpaces]

psumTest :: TestTree
psumTest =
  testCase "psumTest" $
    assertEqual
      "Color simple 1-2-3 using two colors"
      ([(0, "local"), (1, "local")], [(1 :: Int, 0), (2, 1), (3, 0)])
      $ (Map.toList *** Map.toList) $
        GreedyColoring.colorGraph
          (Map.fromList [(1, "local"), (2, "local"), (3, "local")])
          $ Set.fromList [(1, 2), (2, 3)]

allIntersect :: TestTree
allIntersect =
  testCase "allIntersect" $
    assertEqual
      "Color a graph where all values intersect"
      ([(0, "local"), (1, "local"), (2, "local")], [(1 :: Int, 2), (2, 1), (3, 0)])
      $ (Map.toList *** Map.toList) $
        GreedyColoring.colorGraph
          (Map.fromList [(1, "local"), (2, "local"), (3, "local")])
          $ Set.fromList [(1, 2), (2, 3), (1, 3)]

emptyGraph :: TestTree
emptyGraph =
  testCase "emptyGraph" $
    assertEqual
      "Color an empty graph"
      ([] :: [(Int, Char)], [] :: [(Int, Int)])
      $ (Map.toList *** Map.toList) $ GreedyColoring.colorGraph (Map.fromList []) $ Set.fromList []

noIntersections :: TestTree
noIntersections =
  GreedyColoring.colorGraph
    (Map.fromList [(1, "local"), (2, "local"), (3, "local")])
    (Set.fromList [])
    & Map.toList *** Map.toList
    & assertEqual
      "Color nodes with no intersections"
      ([(0, "local")], [(1, 0), (2, 0), (3, 0)] :: [(Int, Int)])
    & testCase "noIntersections"

differentSpaces :: TestTree
differentSpaces =
  GreedyColoring.colorGraph
    (Map.fromList [(1, "a"), (2, "b"), (3, "c")])
    (Set.fromList [])
    & Map.toList *** Map.toList
    & assertEqual
      "Color nodes with no intersections but in different spaces"
      ([(0, "c"), (1, "b"), (2, "a")], [(1, 2), (2, 1), (3, 0)] :: [(Int, Int)])
    & testCase "differentSpaces"
