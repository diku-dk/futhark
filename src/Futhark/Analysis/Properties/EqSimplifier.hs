module Futhark.Analysis.Properties.EqSimplifier
  ( groupEqualities,
    equivClasses,
  )
where

import Data.Graph (Tree (Node), buildG, components)
import Data.Map qualified as M
import Futhark.Util (nubOrd)
import qualified Data.Set as S
import Futhark.Util.Pretty
import Futhark.Analysis.Properties.Unify

type Equality a = (a, a)

-- | Partitions a list of equalities into equivalence classes.
groupEqualities :: (Ord a) => [Equality a] -> [S.Set a]
groupEqualities eqs =
  -- Get all connected components using DFS on undirected graph.
  let edges = map encoder eqs
      graph = buildG (0, length nodes - 1) edges
   in map (S.fromList . decoder) (components graph)
  where
    nodes = nubOrd (map fst eqs ++ map snd eqs) -- Unique elements in eqs.
    -- Encode/decode nodes as integers.
    enc = M.fromList $ zip nodes [0 ..]
    encoder (x, y) = (enc M.! x, enc M.! y)
    dec = M.fromList $ zip [0 ..] nodes
    decoder (Node v tree) = dec M.! v : concatMap decoder tree


-- | Partitions a list of equalities into equivalence classes.
-- 
-- Each equivalence class is represented as a minimum spanning tree;
-- if the tree is traversed top-down, at each level applying
-- replacements (parent |-> child) to the original expression
-- that gave these equalities, the number of equalities is
-- minimised.
equivClasses :: (Ord a) => [Equality a] -> [[Equality a]]
equivClasses eqs =
  -- Get all connected components using DFS on undirected graph.
  let edges = map encoder eqs
      graph = buildG (0, length nodes - 1) edges
   in map decoder (components graph)
  where
    nodes = nubOrd (map fst eqs ++ map snd eqs) -- Unique elements in eqs.
    -- Encode/decode nodes as integers.
    enc = M.fromList $ zip nodes [0 ..]
    encoder (x, y) = (enc M.! x, enc M.! y)
    dec = M.fromList $ zip [0 ..] nodes
    decoder (Node v trees) =
      [(dec M.! v, dec M.! child) | Node child _ <- trees] ++ concatMap decoder trees

-- [(a,b), (b,c), (c,d), (b,e)]
-- 
-- [(a,b), (b,c), (c,d) | (b -> e)]
-- [(a,e), (e,c), (c,d) | (b -> e)]
-- [(a,e), (e,d) | (b -> e), (c -> d)]
--
-- b = b, b = c, b = d
-- c = c, c = c, c = d
--
-- want to collapse the tree s.t. for each leaf,
-- root = leaf
--
-- {a, b, c}
-- 
-- a = b, a = c
--
-- pick (a,b):  c -> a
--
--
-- (edgeIds[i], H[edges[i]]), (H[edges[i]], edgeIds[j])
-- 
--
-- b -> a, b->c, b->d
-- a = c, a = d
-- at the start node, want to reverse the order of the subst
eqsToRep :: [Equality a] -> [Replacement a]
eqsToRep eqs = go (reverse eqs)
  where
    go ((a,b) : eqs') = 

-- instance Pretty a => Pretty (Tree a) where
--   pretty (Node v trees) = pretty v <+> "→" <+> pretty trees
-- 
-- equivClasses :: (Ord a) => [Equality a] -> [Tree a]
-- equivClasses eqs =
--   -- Get all connected components using DFS on undirected graph.
--   let edges = map encoder eqs
--       graph = buildG (0, length nodes - 1) edges
--    in map decoder (components graph)
--   where
--     nodes = nubOrd (map fst eqs ++ map snd eqs) -- Unique elements in eqs.
--     -- Encode/decode nodes as integers.
--     enc = M.fromList $ zip nodes [0 ..]
--     encoder (x, y) = (enc M.! x, enc M.! y)
--     dec = M.fromList $ zip [0 ..] nodes
--     decoder (Node v trees) = Node (dec M.! v) (map decoder trees)
