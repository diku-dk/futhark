module Futhark.Analysis.Properties.EqSimplifier
  ( equivST,
    transitiveEqs,
    eqSolver,
  )
where

import Control.Monad (join)
import Data.Graph (Tree (Node), buildG, components)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Futhark.Analysis.Properties.AlgebraBridge
import Futhark.Analysis.Properties.AlgebraPC.Symbol qualified as Algebra
import Futhark.Analysis.Properties.Monad
import Futhark.Analysis.Properties.Property
import Futhark.Analysis.Properties.Rule
import Futhark.Analysis.Properties.Symbol
import Futhark.Analysis.Properties.Traversals
import Futhark.Analysis.Properties.Unify
import Futhark.MonadFreshNames (newVName)
import Futhark.SoP.SoP
import Futhark.Util (nubOrd)

type Equality a = (a, a)

-- | Partitions a list of equalities into equivalence classes.
-- Each equivalence class is represented by a spanning tree.
equivST :: (Ord a) => [Equality a] -> [Tree a]
equivST eqs =
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
    decoder (Node v trees) = Node (dec M.! v) (map decoder trees)

-- Returns the transitive equalities defined by the edges in the Tree.
transitiveEqs :: Tree a -> [Equality a]
transitiveEqs (Node root trees) = concatMap go (skipDirectSucc trees)
  where
    -- Skip equalities that are already in the expression that produced the Tree.
    skipDirectSucc = concatMap (\(Node _ ts) -> ts)
    go (Node v []) = [(root, v)]
    go (Node v ts) = (root, v) : concatMap go ts ++ concatMap transitiveEqs ts

{-
              Solver for Symbol.
-}

eqSolver :: Symbol -> IndexFnM Symbol
eqSolver p = do
  -- TODO change this to also include the equalities transformed by astMap m?
  -- For example, x[i] = x[j] => i = j, returns only i = j, replacing x[i] = x[j].
  fixPointM (astMap rules) (foldl (:&&) p (getTransitiveEqs p))
  where
    getTransitiveEqs = map fromEquality . concatMap transitiveEqs . equivST . getEqualities

    getEqualities = mapMaybe toEquality . conjToList . toCNF

    toEquality :: Symbol -> Maybe (Equality (SoP Symbol))
    toEquality (x :== y) = Just (x, y)
    toEquality _ = Nothing

    fromEquality :: Equality (SoP Symbol) -> Symbol
    fromEquality (x, y) = x :== y

    rules :: ASTMapper Symbol IndexFnM =
      ASTMapper
        { mapOnSymbol = applyRuleBook eqSolverRules,
          mapOnSoP = pure
        }

eqSolverRules :: IndexFnM [Rule Symbol Symbol IndexFnM]
eqSolverRules = do
  x <- newVName "h"
  i <- newVName "h"
  j <- newVName "h"
  pure
    [ Rule
        { name = "For injective x: x[i] = x[j] => i = j",
          from = idx x i :== idx x j,
          to = \s -> do
            e_i <- sub s (hole i)
            e_j <- sub s (hole j)
            pure (e_i :== e_j),
          sideCondition = \s -> do
            e_x <- sub s (hole x)
            e_i <- sub s (hole i)
            e_j <- sub s (hole j)
            alg_inj <- join <$> traverse askInjectiveRCD (toAlgVar e_x)
            case alg_inj of
              Just (Injective _ (Just rcd)) -> do
                rcd' <- fromAlgebra rcd
                isYes <$> (e_i `inRange` rcd') `andM` (e_j `inRange` rcd')
              Just (Injective _ Nothing) ->
                pure True
              _ -> pure False
        }
    ]
  where
    hole = sym2SoP . Hole

    idx x i = sym2SoP (Idx (Hole x) (hole i))

    toAlgVar e | Just (Var vn) <- justSym e = Just (Algebra.Var vn)
    toAlgVar _ = Nothing

    inRange e (a, b) = isTrue (a :<= e :&& e :<= b)

fixPointM :: (Renameable t, Eq t) => (t -> IndexFnM t) -> t -> IndexFnM t
fixPointM f x = do
  x' <- f x
  if x' == x then pure x else fixPointM f x'
