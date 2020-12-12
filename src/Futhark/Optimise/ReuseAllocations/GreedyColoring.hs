-- | Provides a greedy graph-coloring algorithm.
module Futhark.Optimise.ReuseAllocations.GreedyColoring (colorGraph, Coloring) where

import Data.Function ((&))
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Futhark.Analysis.Interference as Interference

-- | A map of values to their color, identified by an integer.
type Coloring a = Map a Int

-- | A map of values to the set "neighbors" in the graph
type Neighbors a = Map a (Set a)

-- | Computes the neighbor map of a graph.
neighbors :: Ord a => Interference.Graph a -> Neighbors a
neighbors =
  Set.foldr
    ( \(x, y) acc ->
        acc
          & Map.insertWith Set.union x (Set.singleton y)
          & Map.insertWith Set.union y (Set.singleton x)
    )
    Map.empty

firstAvailable :: Eq space => Map Int space -> Set Int -> Int -> space -> (Map Int space, Int)
firstAvailable spaces xs i sp =
  case (i `Set.member` xs, spaces !? i) of
    (False, Just sp') | sp' == sp -> (spaces, i)
    (False, Nothing) -> (Map.insert i sp spaces, i)
    _ -> firstAvailable spaces xs (i + 1) sp

colorNode ::
  (Ord a, Eq space) =>
  Neighbors a ->
  (a, space) ->
  (Map Int space, Coloring a) ->
  (Map Int space, Coloring a)
colorNode nbs (x, sp) (spaces, coloring) =
  let nb_colors =
        foldMap (maybe Set.empty Set.singleton . (coloring !?)) $
          fromMaybe mempty (nbs !? x)
      (spaces', color) = firstAvailable spaces nb_colors 0 sp
   in (spaces', Map.insert x color coloring)

-- | Graph coloring that takes into account the `space` of values. Two values
-- can only share the same color if they live in the same space. The result is
-- map from each color to a space and a map from each value in the input graph
-- to it's new color.
colorGraph ::
  (Ord a, Ord space) =>
  Map a space ->
  Interference.Graph a ->
  (Map Int space, Coloring a)
colorGraph spaces graph =
  let nodes = Set.fromList $ Map.toList spaces
      nbs = neighbors graph
   in Set.foldr (colorNode nbs) mempty nodes
