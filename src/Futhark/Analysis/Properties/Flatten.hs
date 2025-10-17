{-# LANGUAGE LambdaCase #-}

module Futhark.Analysis.Properties.Flatten (lookupII, from1Dto2D, unflatten) where

import Data.List (tails)
import Data.Map qualified as M
import Data.Set qualified as S
import Futhark.Analysis.Properties.IndexFn
import Futhark.Analysis.Properties.IndexFnPlus ()
import Futhark.Analysis.Properties.Monad
import Futhark.Analysis.Properties.Symbol
import Futhark.Analysis.Properties.Unify
import Futhark.MonadFreshNames (newVName)
import Futhark.SoP.SoP (SoP, int2SoP, sym2SoP, (.*.), (.+.), (.-.))
import Futhark.Util.Pretty (Pretty)
import Language.Futhark (VName)

from1Dto2D :: Quantified Domain -> Quantified Domain -> SoP Symbol -> [(VName, SoP Symbol)]
from1Dto2D (Forall i (Iota n)) (Forall j (Iota m)) e_idx
  | i `S.notMember` fv m =
      let idx = sym2SoP (Ix n m e_idx)
       in [(i, idx), (j, e_idx .-. idx .*. m)]
  | otherwise = error "Not implemented yet."
from1Dto2D _ _ _ = undefined

flatIndices :: [[Quantified Domain]] -> [SoP Symbol]
flatIndices = map flattenIndex

flattenIndex :: [Quantified Domain] -> SoP Symbol
flattenIndex dim
  | S.fromList (map boundVar dim) `S.disjoint` mconcat (map fv dim),
    all (\case (Forall _ (Iota {})) -> True; _ -> False) dim =
      let nss = drop 1 (tails [n | Forall _ (Iota n) <- dim])
       in foldl
            (.+.)
            (int2SoP 0)
            [multiply i ns | (i, ns) <- zip (map boundVar dim) nss]
  where
    multiply i [] = sym2SoP (Var i)
    multiply i ns = sym2SoP (Var i) .*. foldl1 (.*.) ns
flattenIndex _ = undefined

unflatten :: IndexFn -> IndexFn
unflatten f = f {shape = map (: []) (concat (shape f))}

lookupII :: Domain -> IndexFn -> IndexFnM (VName, IndexFn)
lookupII dom def = do
  v <- unisearch dom =<< getII
  case v of
    Just res -> pure res
    Nothing -> do
      vn <- newVName "II"
      insertII dom (vn, def)
      pure (vn, def)

-- Search a mapping using unification for equality checks.
unisearch :: (Ord v, Unify v Symbol, Pretty v) => v -> M.Map v a -> IndexFnM (Maybe a)
unisearch x mapping = do
  case mapping M.!? x of
    Just v ->
      -- Exact match.
      pure (Just v)
    Nothing -> do
      -- Search for matches using unification.
      matches :: [(a, Maybe (Substitution Symbol))] <-
        mapM (\(k, v) -> (v,) <$> unify k x) (M.toList mapping)
      case matches of
        [] -> pure Nothing
        [(v, _)] ->
          pure (Just v)
        _ -> error "unisearch: multiple matches"
