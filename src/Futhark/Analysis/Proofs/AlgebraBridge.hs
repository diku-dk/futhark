-- Utilities for using the Algebra layer from the IndexFn layer.
module Futhark.Analysis.Proofs.AlgebraBridge where

import Control.Monad (unless, void, (<=<))
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromJust)
import Data.Set qualified as S
import Futhark.Analysis.Proofs.AlgebraPC.Symbol qualified as Algebra
import Futhark.Analysis.Proofs.IndexFn (IndexFnM)
import Futhark.Analysis.Proofs.Symbol (Symbol (..))
import Futhark.Analysis.Proofs.SymbolPlus ()
import Futhark.Analysis.Proofs.Traversals (ASTMappable, ASTMapper (..), astMap)
import Futhark.Analysis.Proofs.Unify (Substitution (mapping), rep, unify)
import Futhark.MonadFreshNames (newVName)
import Futhark.SoP.Convert (ToSoP (toSoPNum))
import Futhark.SoP.Monad (addProperty, addRange, getUntrans, inv, lookupUntransPE, lookupUntransSym, mkRange)
import Futhark.SoP.SoP (SoP, int2SoP, justSym, mapSymSoP2M, mapSymSoP2M_, sym2SoP, (.+.), (~-~))
import Futhark.Util.Pretty (prettyString)
import Language.Futhark (VName)

-----------------------------------------------------------------------------
-- Translation from Algebra to IndexFn layer.
------------------------------------------------------------------------------
fromAlgebraSoP :: SoP Algebra.Symbol -> IndexFnM (SoP Symbol)
fromAlgebraSoP = mapSymSoP2M fromAlgebra

fromAlgebra :: Algebra.Symbol -> IndexFnM (SoP Symbol)
fromAlgebra (Algebra.Var vn) = do
  x <- lookupUntransSym (Algebra.Var vn)
  case x of
    Just x' -> pure . sym2SoP $ x'
    Nothing -> pure . sym2SoP $ Var vn
fromAlgebra (Algebra.Idx (Algebra.One vn) i) = do
  x <- lookupUntransSym (Algebra.Var vn)
  idx <- fromAlgebraSoP i
  case x of
    Just x' -> sym2SoP <$> repHoles x' idx
    Nothing -> pure . sym2SoP $ Idx (Var vn) idx
fromAlgebra (Algebra.Idx (Algebra.POR {}) _) = undefined
fromAlgebra (Algebra.Mdf _dir vn i j) = do
  -- TODO add monotonicity property to environment?
  a <- fromAlgebraSoP i
  b <- fromAlgebraSoP j
  x <- lookupUntransSymUnsafe vn
  xa <- repHoles x a
  xb <- repHoles x b
  pure $ xa ~-~ xb
fromAlgebra (Algebra.Sum (Algebra.One vn) lb ub) = do
  a <- fromAlgebraSoP lb
  b <- fromAlgebraSoP ub
  x <- lookupUntransSymUnsafe vn
  j <- newVName "j"
  xj <- repHoles x (sym2SoP $ Var j)
  pure . sym2SoP $ LinComb j a b xj
fromAlgebra (Algebra.Sum (Algebra.POR vns) lb ub) = do
  -- Sum (POR {x,y}) a b = Sum x a b + Sum y a b
  foldr1 (.+.)
    <$> mapM
      (\vn -> fromAlgebra $ Algebra.Sum (Algebra.One vn) lb ub)
      (S.toList vns)
fromAlgebra (Algebra.Pow {}) = undefined

lookupUntransSymUnsafe :: VName -> IndexFnM Symbol
lookupUntransSymUnsafe = fmap fromJust . lookupUntransSym . Algebra.Var

-- Replace holes in `x` by `replacement`.
repHoles :: (Monad m) => Symbol -> SoP Symbol -> m Symbol
repHoles x replacement =
  astMap mapper x
  where
    -- Change how we are replacing depending on if replacement is really a SoP.
    mapper
      | Just replacement' <- justSym replacement =
          ASTMapper
            { mapOnSymbol = \sym -> case sym of
                Hole _ -> pure replacement'
                _ -> pure sym,
              mapOnSoP = pure
            }
      | otherwise =
          ASTMapper
            { mapOnSymbol = pure,
              mapOnSoP = \sop -> case justSym sop of
                Just (Hole _) -> pure replacement
                _ -> pure sop
            }

instance ToSoP Algebra.Symbol Symbol where
  -- Convert from IndexFn Symbol to Algebra Symbol.
  -- toSoPNum symbol = (1,) . sym2SoP <$> toAlgebra symbol
  toSoPNum symbol = error $ "toSoPNum used on " <> prettyString symbol

-----------------------------------------------------------------------------
-- Translation from IndexFn to Algebra layer.
------------------------------------------------------------------------------
toAlgebraSoP :: SoP Symbol -> IndexFnM (SoP Algebra.Symbol)
toAlgebraSoP = mapSymSoP2M_ toAlgebra_ <=< handleQuantifiers

toAlgebra :: Symbol -> IndexFnM Algebra.Symbol
toAlgebra = toAlgebra_ <=< handleQuantifiers

-- Add quantified symbols to the untranslatable environement
-- with quantifiers replaced by holes. Uses unification on symbols
-- to ensure deduplication.
handleQuantifiers :: (ASTMappable Symbol b) => b -> IndexFnM b
handleQuantifiers = astMap m
  where
    m = ASTMapper {mapOnSymbol = handleQuant, mapOnSoP = pure}
    handleQuant p@(LinComb j _ _ x) = do
      res <- search x
      case res of
        Just _ -> pure p
        Nothing -> do
          hole <- sym2SoP . Hole <$> newVName "h"
          let x_holed = fromJust . justSym $ rep (M.insert j hole mempty) x
          addUntrans x_holed
          pure p
      where
        addUntrans = void . lookupUntransPE
    handleQuant x = pure x

-- Search for hole-less symbol in untranslatable environment, matching
-- any symbol in the environment that is syntactically identical up to one hole.
-- For example, `search x[0]` in environment `{y : x[hole]}`,
-- returns `(y, (hole, 0)`.
search :: Symbol -> IndexFnM (Maybe (VName, Maybe (VName, SoP Symbol)))
search x = do
  inv_map <- inv <$> getUntrans
  case inv_map M.!? x of
    Just algsym ->
      -- Symbol is a key in untranslatable env.
      pure $ Just (Algebra.getVName algsym, Nothing)
    Nothing -> do
      -- Search for symbol in untranslatable environment; if x unifies
      -- with some key in the environment, return that key.
      -- Otherwise create a new entry in the environment.
      let syms = M.toList inv_map
      matches <- catMaybes <$> mapM (\(sym, algsym) -> fmap (algsym,) <$> unify sym x) syms
      case matches of
        [] -> pure Nothing
        [(algsym, sub)] -> do
          unless (M.size (mapping sub) == 1) $ error "search: multiple holes"
          pure $
            Just (Algebra.getVName algsym, Just . head $ M.toList (mapping sub))
        _ -> error "search: symbol unifies with multiple symbols"

-- Translate IndexFn.Symbol to Algebra.Symbol.
-- Fresh names are created for untranslatable symbols such as indicators
-- and quantified symbols in sums. Indexing is preserved on untranslatable
-- symbols. For example, ⟦x[0] + 1⟧ + ∑j∈(1 .. b) ⟦x[j] + 1⟧ will be translated
-- as y[0] + Sum y[1:b] with fresh name y mapped to ⟦x[hole] + 1⟧.
-- This is done so because the algebra layer needs to know about indexing.
toAlgebra_ :: Symbol -> IndexFnM Algebra.Symbol
toAlgebra_ (Var x) = pure $ Algebra.Var x
toAlgebra_ (Hole _) = undefined
toAlgebra_ (LinComb _ lb ub x) = do
  res <- search x
  case res of
    Just (vn, _) -> do
      a <- mapSymSoP2M_ toAlgebra_ lb
      b <- mapSymSoP2M_ toAlgebra_ ub
      pure $ Algebra.Sum (Algebra.One vn) a b
    Nothing -> error "mkUntrans hasn't been run on sum"
toAlgebra_ sym@(Idx _ i) = do
  j <- mapSymSoP2M_ toAlgebra_ i
  (vn, _) <- lookupOrAdd sym
  pure $ Algebra.Idx (Algebra.One vn) j
toAlgebra_ sym@(Indicator _) = do
  (vn, sub) <- lookupOrAdd sym
  addRange (Algebra.Var vn) (mkRange (int2SoP 0) (int2SoP 1))
  addProperty (Algebra.Var vn) Algebra.Indicator
  case sub of
    Just (_hole, idx) -> do
      idx' <- mapSymSoP2M_ toAlgebra_ idx
      pure $ Algebra.Idx (Algebra.One vn) idx'
    Nothing -> pure $ Algebra.Var vn
toAlgebra_ x = lookupUntransPE x

-- Like search, but adds symbol to the untranslatable environment,
-- if it does not exist.
lookupOrAdd :: Symbol -> IndexFnM (VName, Maybe (VName, SoP Symbol))
lookupOrAdd sym = do
  res <- search sym
  case res of
    Just (vn, sub) -> pure (vn, sub)
    Nothing -> do
      vn <- Algebra.getVName <$> lookupUntransPE sym
      pure (vn, Nothing)
