module Futhark.Analysis.Properties.Flatten (flatten2d, lookupII, from1Dto2D) where

import Data.Map qualified as M
import Data.Set qualified as S
import Futhark.Analysis.Properties.IndexFn
import Futhark.Analysis.Properties.IndexFnPlus ()
import Futhark.Analysis.Properties.Monad
import Futhark.Analysis.Properties.Symbol
import Futhark.Analysis.Properties.Unify
import Futhark.MonadFreshNames (newVName)
import Futhark.SoP.SoP (SoP, sym2SoP, (.*.), (.-.))
import Futhark.Util.Pretty (Pretty)
import Language.Futhark (VName)

from1Dto2D :: Quantified Domain -> Quantified Domain -> SoP Symbol -> [(VName, SoP Symbol)]
from1Dto2D (Forall i (Iota n)) (Forall j (Iota m)) e_idx
  | i `S.notMember` fv m =
      let idx = sym2SoP (Ix n m e_idx)
       in [(i, idx), (j, e_idx .-. idx .*. m)]
  | otherwise = error "Not implemented yet."
from1Dto2D _ _ _ = undefined

flatten2d :: VName -> SoP Symbol -> SoP Symbol -> IndexFnM (VName, Iterator)
flatten2d k m n =
  undefined

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
