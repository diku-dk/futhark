-- Utilities for using the Algebra layer from the IndexFn layer.
module Futhark.Analysis.Proofs.AlgebraBridge where

import Control.Monad (unless, (<=<), void)
import Control.Monad.RWS (gets)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromJust)
import Data.Set qualified as S
import Futhark.Analysis.Proofs.AlgebraPC.Algebra qualified as Algebra
import Futhark.Analysis.Proofs.IndexFn (IndexFnM, VEnv (..))
import Futhark.Analysis.Proofs.Symbol (Symbol (..))
import Futhark.Analysis.Proofs.SymbolPlus ()
import Futhark.Analysis.Proofs.Traversals (ASTFolder (..), ASTMapper (..), astFold, astMap, ASTMappable)
import Futhark.Analysis.Proofs.Unify (Substitution (mapping), rename, rep, unify)
import Futhark.MonadFreshNames (getNameSource, newVName)
import Futhark.SoP.Convert (ToSoP (toSoPNum))
import Futhark.SoP.Monad (MonadSoP, addProperty, addRange, getUntrans, inv, lookupUntransPE, lookupUntransSym, mkRange)
import Futhark.SoP.SoP (SoP, int2SoP, justSym, mapSymSoP2M, mapSymSoP2M_, sym2SoP, (.+.), (.-.))
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
    Just x' -> repExactlyOneHole x' idx
    Nothing -> pure . sym2SoP $ Idx (Var vn) idx
fromAlgebra (Algebra.Idx (Algebra.POR {}) _) = undefined
fromAlgebra (Algebra.Mdf _dir vn i j) = do
  -- TODO add monotonicity property to environment?
  a <- fromAlgebraSoP i
  b <- fromAlgebraSoP j
  x <- lookupUntransSymUnsafe vn
  xa <- repExactlyOneHole x a
  xb <- repExactlyOneHole x b
  pure $ xa .-. xb
fromAlgebra (Algebra.Sum (Algebra.One vn) lb ub) = do
  a <- fromAlgebraSoP lb
  b <- fromAlgebraSoP ub
  x <- lookupUntransSymUnsafe vn
  j <- newVName "j"
  xj <- repExactlyOneHole x (sym2SoP $ Var j)
  case justSym xj of
    Just xj' -> pure . sym2SoP $ LinComb j a b xj'
    Nothing -> error "fromAlgebra: Inconsistent untranslatable environment"
fromAlgebra (Algebra.Sum (Algebra.POR vns) lb ub) = do
  -- Sum (POR {x,y}) a b = Sum x a b + Sum y a b
  foldr1 (.+.)
    <$> mapM
      (\vn -> fromAlgebra $ Algebra.Sum (Algebra.One vn) lb ub)
      (S.toList vns)
fromAlgebra (Algebra.Pow {}) = undefined

lookupUntransSymUnsafe :: VName -> IndexFnM Symbol
lookupUntransSymUnsafe = fmap fromJust . lookupUntransSym . Algebra.Var

repExactlyOneHole :: (Monad m) => Symbol -> SoP Symbol -> m (SoP Symbol)
repExactlyOneHole x replacement = do
  hole <- findHole x
  case hole of
    Just h -> pure $ rep (M.insert h replacement mempty) x
    Nothing -> pure . sym2SoP $ x

-- TODO add the hole to the env as well like (hole, holed expression) to avoid this mockery
findHole :: Monad m => Symbol -> m (Maybe VName)
findHole sym = do
  holes <- findHoles sym
  case holes of
    [] -> pure Nothing
    [h] -> pure (Just h)
    _ -> error "findHole: Inconsistent untranslatable environment"
  where
    -- TODO make this not stupid
    findHoles = astFold m []
      where
        m = ASTFolder { foldOnSymbol = getHole }
        getHole acc (Hole vn) = pure $ vn : acc
        getHole acc _ = pure acc

instance ToSoP Algebra.Symbol Symbol where
  -- Convert from IndexFn Symbol to Algebra Symbol.
  -- toSoPNum symbol = (1,) . sym2SoP <$> toAlgebra symbol
  toSoPNum symbol = error $ "toSoPNum used on " <> prettyString symbol

-----------------------------------------------------------------------------
-- Translation from IndexFn to Algebra layer.
------------------------------------------------------------------------------
toAlgebraSoP :: SoP Symbol -> IndexFnM (SoP Algebra.Symbol)
toAlgebraSoP = mapSymSoP2M_ toAlgebra_ <=< mkUntrans

toAlgebra :: Symbol -> IndexFnM Algebra.Symbol
toAlgebra = toAlgebra_ <=< mkUntrans

mkUntrans :: ASTMappable Symbol b => b -> IndexFnM b
mkUntrans = astMap m
  where
    -- Add quantified symbols to the untranslatable environement
    -- with quantifiers replaced by holes. Uses unification on symbols
    -- to ensure deduplication.
    m = ASTMapper {mapOnSymbol = handleQuantified, mapOnSoP = pure}
    handleQuantified p@(LinComb j _ _ x) = do
      res <- search x
      case res of
        Just _ -> pure p
        Nothing -> do
          hole <- sym2SoP . Hole <$> newVName "h"
          let x_holed = fromJust . justSym $ rep (M.insert j hole mempty) x
          addUntrans x_holed
          pure p
    handleQuantified x = pure x

lookupOrAdd :: Symbol -> IndexFnM (VName, Maybe (VName, SoP Symbol))
lookupOrAdd sym = do
  res <- search sym
  case res of
    Just (vn, sub) -> pure (vn, sub)
    Nothing -> do
      vn <- lookupUntrans sym
      pure (vn, Nothing)

addUntrans :: (MonadSoP Algebra.Symbol e p f) => e -> f ()
addUntrans = void . lookupUntransPE

lookupUntrans :: (MonadSoP Algebra.Symbol e p f) => e -> f VName
lookupUntrans x = getVName <$> lookupUntransPE x

getVName :: Algebra.Symbol -> VName
getVName (Algebra.Var vn) = vn
getVName _ = undefined

search :: Symbol -> IndexFnM (Maybe (VName, Maybe (VName, SoP Symbol)))
search x = do
  inv_map <- inv <$> getUntrans
  algenv <- gets algenv
  case inv_map M.!? x of
    Just algsym ->
      -- Symbol is a key in untranslatable env.
      pure $ Just (getVName algsym, Nothing)
    Nothing -> do
      -- Search for symbol in untranslatable environment; if x unifies
      -- with some key in the environment, return that key.
      -- Otherwise create a new entry in the environment.
      let syms = M.toList inv_map
      matches <- catMaybes <$> mapM (\(sym, algsym) -> fmap (algsym,) <$> unify sym x) syms
      case matches of
        [] -> pure Nothing
        [(algsym, sub)] | M.size (mapping sub) == 1 ->
          pure $ Just (getVName algsym, Just . head $ M.toList (mapping sub))
        _ -> do
          error $ "unifies with multiple symbols in untrans: " <> prettyString x <> "\n" <> prettyString algenv

-- Translate IndexFn.Symbol to Algebra.Symbol.
-- Fresh names are created for untranslatable symbols such as indicators
-- and quantified symbols in sums. Indexing is preserved on untranslatable
-- symbols. For example, ⟦x[0] + 1⟧ + ∑j∈(1 .. b) ⟦x[j] + 1⟧ will be translated
-- as y[0] + Sum y[1:b] with fresh name y mapped to ⟦x[hole] + 1⟧.
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
