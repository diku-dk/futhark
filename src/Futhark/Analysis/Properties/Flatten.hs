module Futhark.Analysis.Properties.Flatten (flatten2d, lookupII) where

import Data.Map qualified as M
import Futhark.Analysis.Properties.IndexFn
import Futhark.Analysis.Properties.IndexFnPlus ()
import Futhark.Analysis.Properties.Monad
import Futhark.Analysis.Properties.Symbol
import Futhark.Analysis.Properties.Unify
import Futhark.MonadFreshNames (newVName)
import Futhark.SoP.SoP (SoP, sym2SoP, (.*.))
import Futhark.Util.Pretty (Pretty)
import Language.Futhark (VName)

flatten2d :: VName -> SoP Symbol -> SoP Symbol -> IndexFnM (VName, Iterator)
flatten2d k m n = do
  i <- newVName "i"
  let d = Cat k m (sym2SoP (Var k) .*. n)
  (vn_II, f_II) <-
    lookupII d $ IndexFn [Forall i d] (cases [(Bool True, sym2SoP (Var k))])
  insertIndexFn vn_II [f_II]
  pure (vn_II, Forall i d)

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
