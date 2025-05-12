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
import Futhark.Util.Pretty

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

-- Returns the transitive equalities defined by the edges in the Tree (via
-- a preorder traversal that connects each node to all previously visited nodes).
--
-- TODO If tree is output by equivST, aren't direct ancestors in the original equality
-- that constructed the tree? In that case we could discard those.
transitiveEqs :: Tree a -> [Equality a]
transitiveEqs (Node root trees) = go [root] trees
  where
    go _ [] = []
    go visited (Node v vs : ts) =
      [(v,v') | v' <- visited] ++ go (v:visited) (vs ++ ts)

instance Pretty a => Pretty (Tree a) where
  pretty (Node root trees) = "Node" <+> pretty root <+> stack (map (\t -> "->" <+> pretty t) trees)

{-
              Solver for Symbol.
-}

-- The first argument "guides" the solving by making a syntactical
-- substitution {a |-> b} in p and then enriching p with a == b afterwards.
eqSolver :: Symbol -> Symbol -> IndexFnM Symbol
eqSolver (a :== b) p = do
  printM 10 $ "eqSolver " <> prettyStr (a,b) <> " " <> prettyStr p
  -- Assume a == b by syntactical substitution in p.
  p' <-
    sop2Symbol
      <$> astMap
        ( identityMapper
            { mapOnSoP = \sym ->
                if sym == (b :: SoP Symbol)
                  then pure a
                  else pure sym
            }
        )
        (sym2SoP p)
  -- Make sure we don't lose the original equality. (E.g., if p = (a == b).)
  let p'' = p' :&& a :== b
  printM 10 $ "     --> " <> prettyStr p''
  printM 10 $ "     --> EQS   " <> prettyStr (getEqualities p'')
  printM 10 $ "     --> EQST  " <> prettyStr (equivST $ getEqualities p'')
  printM 10 $ "     --> TRANS " <> prettyStr (getTransitiveEqs p'')
  -- TODO change this to also include the equalities transformed by astMap m?
  -- For example, x[i] = x[j] => i = j, returns only i = j, replacing x[i] = x[j].
  fixPointM (astMap rules) (foldl (:&&) p'' (getTransitiveEqs p''))
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
eqSolver _ _ = error "eqSolver not implemented for this guide"

eqSolverRules :: IndexFnM [Rule Symbol Symbol IndexFnM]
eqSolverRules = do
  x <- newVName "h"
  i <- newVName "h"
  j <- newVName "h"
  pure
    [ Rule
        { name = "For injective x: x[i] = x[j] => i = j",
          from = sym2SoP (Apply (Hole x) [hole i]) :== sym2SoP (Apply (Hole x) [hole j]),
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

    toAlgVar e | Just (Var vn) <- justSym e = Just (Algebra.Var vn)
    toAlgVar _ = Nothing

    inRange e (a, b) = isTrue (a :<= e :&& e :<= b)

fixPointM :: (Renameable t, Eq t) => (t -> IndexFnM t) -> t -> IndexFnM t
fixPointM f x = do
  x' <- f x
  if x' == x then pure x else fixPointM f x'
