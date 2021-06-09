-- | Provides a greedy graph-coloring algorithm.
module Futhark.Optimise.ReuseAllocations.GreedyColoring (colorGraph, Coloring) where

import Data.Function ((&))
import Data.Map (Map, (!?))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Futhark.Analysis.Interference as Interference

-- | A map of values to their color, identified by an integer.
type Coloring a = Map a Int

-- | A map of values to the set "neighbors" in the graph
type Neighbors a = Map a (Set a)

-- | Computes the neighbor map of a graph.
neighbors :: Ord a => Interference.Graph a -> Neighbors a
neighbors =
  S.foldr
    ( \(x, y) acc ->
        acc
          & M.insertWith S.union x (S.singleton y)
          & M.insertWith S.union y (S.singleton x)
    )
    M.empty

firstAvailable :: Eq space => Map Int space -> Set Int -> Int -> space -> (Map Int space, Int)
firstAvailable spaces xs i sp =
  case (i `S.member` xs, spaces !? i) of
    (False, Just sp') | sp' == sp -> (spaces, i)
    (False, Nothing) -> (M.insert i sp spaces, i)
    _ -> firstAvailable spaces xs (i + 1) sp

colorNode ::
  (Ord a, Eq space) =>
  Neighbors a ->
  (a, space) ->
  (Map Int space, Coloring a) ->
  (Map Int space, Coloring a)
colorNode nbs (x, sp) (spaces, coloring) =
  let nb_colors =
        foldMap (maybe S.empty S.singleton . (coloring !?)) $
          fromMaybe mempty (nbs !? x)
      (spaces', color) = firstAvailable spaces nb_colors 0 sp
   in (spaces', M.insert x color coloring)

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
  let nodes = S.fromList $ M.toList spaces
      nbs = neighbors graph
   in S.foldr (colorNode nbs) mempty nodes
