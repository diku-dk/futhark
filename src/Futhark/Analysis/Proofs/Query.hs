-- Answer queries on index functions using algebraic solver.
-- Defines transformations between Algebra and IndexFn layers.
{-# OPTIONS_GHC -Wno-orphans #-}

module Futhark.Analysis.Proofs.Query where

import Control.Monad (when)
import Control.Monad.RWS (gets, modify)
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Futhark.Analysis.Proofs.AlgebraPC.Algebra qualified as Algebra
import Futhark.Analysis.Proofs.IndexFn (Domain (..), IndexFn (..), IndexFnM, Iterator (..), VEnv (..), getCase)
import Futhark.Analysis.Proofs.IndexFnPlus (domainEnd, domainStart, intervalEnd)
import Futhark.Analysis.Proofs.Rewrite (rewrite)
import Futhark.Analysis.Proofs.Symbol (Symbol (..))
import Futhark.Analysis.Proofs.Traversals (ASTMapper (..), astMap)
import Futhark.MonadFreshNames (newVName)
import Futhark.SoP.FourierMotzkin (($<$), ($>$))
import Futhark.SoP.Monad (MonadSoP, addRange, lookupUntransPE, lookupUntransSym)
import Futhark.SoP.Refine (addRel)
import Futhark.SoP.SoP (Range (Range), Rel (..), SoP, int2SoP, justAffine, justSym, mapSymSoP2M, mapSymSoP2M_, sym2SoP, (.-.), (~-~))
import Futhark.SoP.SoP qualified as SoP

data Query
  = CaseIsMonotonic
  | -- Apply transform to case value, then check whether it simplifies to true.
    CaseTransform (SoP Symbol -> SoP Symbol)

data Answer = Yes | Unknown

simplify :: SoP Symbol -> IndexFnM (SoP Symbol)
simplify = rewrite

-- Answers the query for each case in an index function.
ask :: Query -> IndexFn -> Int -> IndexFnM Answer
ask query (IndexFn it cs) case_idx = rollbackAlgEnv $ do
  let (p, q) = getCase case_idx cs
  addIterator it
  addPred p
  case query of
    CaseTransform transf -> do
      x <- simplify $ transf q
      case justSym x of
        Just (Bool True) -> pure Yes
        _ -> pure Unknown
    _ -> undefined

rollbackAlgEnv :: IndexFnM a -> IndexFnM a
rollbackAlgEnv computation = do
  alg <- gets algenv
  res <- computation
  modify (\env -> env {algenv = alg})
  pure res

addPred :: Symbol -> IndexFnM ()
addPred (x :/= y) = do
  -- Express not equal as one is greater than the other.
  -- Figure out which one.
  x_is_greater <- x $>$ y
  if x_is_greater
    then addRel (x :>: y)
    else do
      y_is_greater <- x $<$ y
      when y_is_greater $ addRel (x :<: y)
addPred p | Just rel <- toRel p = addRel rel
addPred _ = pure ()

mkRange :: SoP u -> SoP u -> Range u
mkRange lb ub = Range (S.singleton lb) 1 (S.singleton ub)

addIterator :: Iterator -> IndexFnM ()
addIterator (Forall i dom) = case dom of
  Iota n -> do
    addRange (Var i) (mkRange (domainStart dom) (domainEnd dom))
    boundIndexValues n
  Cat k m _ -> do
    addRange (Var k) (mkRange (int 0) (m .-. int 1))
    addRange (Var i) (mkRange (domainStart dom) (domainEnd dom))
    addRange (Var i) (mkRangeUB (intervalEnd dom .-. int 1))
    boundIndexValues m
  where
    mkRangeUB x = Range mempty 1 (S.singleton x)

    int :: Int -> SoP Symbol
    int n = int2SoP (toInteger n)

    boundIndexValues e = do
      -- The type of Range restricts us to bound affine n. Lower bound is 1
      -- since otherwise Iterator would be a single point (and hence Empty).
      case justAffine (SoP.normalize e) of
        Just (c, x, b) ->
          let ub = int2SoP $ toInteger (maxBound :: Int) - b
           in addRange x (Range (S.singleton $ int 1) c (S.singleton ub))
        Nothing -> pure ()
addIterator _ = pure ()

-- Argument should be in NNF to work properly.
toRel :: Symbol -> Maybe (Rel Symbol)
toRel (x :<= y) = Just $ x :<=: y
toRel (x :< y) = Just $ x :<: y
toRel (x :> y) = Just $ x :>: y
toRel (x :>= y) = Just $ x :>=: y
toRel (x :== y) = Just $ x :==: y
toRel (x :&& y) = (:&&:) <$> toRel x <*> toRel y
toRel (x :|| y) = (:||:) <$> toRel x <*> toRel y
toRel _ = Nothing

------------------------------------------------------------------------------
-- Translation between Algebra and IndexFn layers.
------------------------------------------------------------------------------
lookupUntransSymUnsafe :: (MonadSoP Algebra.Symbol Symbol p m) => Algebra.Symbol -> m Symbol
lookupUntransSymUnsafe = fmap fromJust . lookupUntransSym

fromAlgebra :: (MonadSoP Algebra.Symbol Symbol p m) => SoP Algebra.Symbol -> m (SoP Symbol)
fromAlgebra = mapSymSoP2M fromAlgebraSymbol
  where
    fromAlgebraSymbol :: (MonadSoP Algebra.Symbol Symbol p m) => Algebra.Symbol -> m (SoP Symbol)
    fromAlgebraSymbol (Algebra.Var x) = pure . sym2SoP $ Var x
    fromAlgebraSymbol (Algebra.Idx (Algebra.One vn) i) = do
      x <- lookupUntransSymUnsafe (Algebra.Var vn)
      sym2SoP . Idx x <$> fromAlgebra i
    fromAlgebraSymbol (Algebra.Idx (Algebra.POR _) _) =
      undefined
    fromAlgebraSymbol (Algebra.Mdf _dir vn i j) = do
      -- TODO add monotonicity property to environment?
      a <- fromAlgebra i
      b <- fromAlgebra j
      x <- lookupUntransSymUnsafe (Algebra.Var vn)
      pure $ Idx x a ~-~ Idx x b
    fromAlgebraSymbol (Algebra.Sum (Algebra.One vn) lb ub) = do
      j <- newVName "j"
      a <- fromAlgebra lb
      b <- fromAlgebra ub
      x <- lookupUntransSymUnsafe (Algebra.Var vn)
      pure . sym2SoP $ LinComb j a b x
    fromAlgebraSymbol (Algebra.Sum (Algebra.POR _) _ _) = undefined
    fromAlgebraSymbol (Algebra.Pow {}) = undefined
    -- fromAlgebraSymbol (Algebra.Hole {}) = undefined

toAlgebra :: (MonadSoP Algebra.Symbol Symbol p m) => SoP Symbol -> m (SoP Algebra.Symbol)
toAlgebra = mapSymSoP2M_ toAlgebraSymbol
  where
    toAlgebraSymbol :: (MonadSoP Algebra.Symbol Symbol p m) => Symbol -> m Algebra.Symbol
    toAlgebraSymbol symbol = toAlgebra_ =<< astMap m symbol
      where
        getVName (Algebra.Var vn) = vn
        getVName _ = undefined

        -- TODO change untranslatable env to have VNames as keys?
        lookupUntrans x = getVName <$> lookupUntransPE x

        m = ASTMapper {mapOnSymbol = boundSymbol, mapOnSoP = pure}

        boundSymbol (Indicator p) = do
          vn <- lookupUntrans p
          addRange (Algebra.Var vn) (mkRange (int2SoP 0) (int2SoP 1))
          pure $ Indicator p
        boundSymbol x = pure x

        toAlgebra_ (Var x) = pure $ Algebra.Var x
        -- toAlgebra_ (Hole x) = pure $ Algebra.Hole x
        toAlgebra_ (LinComb _ lb ub x) = do
          vn <- lookupUntrans x
          a <- mapSymSoP2M_ toAlgebra_ lb
          b <- mapSymSoP2M_ toAlgebra_ ub
          pure $ Algebra.Sum (Algebra.One vn) a b
        toAlgebra_ (Idx xs i) = do
          vn <- lookupUntrans xs
          j <- mapSymSoP2M_ toAlgebra_ i
          pure $ Algebra.Idx (Algebra.One vn) j
        toAlgebra_ (Indicator p) = do
          vn <- lookupUntrans p
          pure $ Algebra.Var vn
        toAlgebra_ x = lookupUntransPE x
