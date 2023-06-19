{-# LANGUAGE DataKinds #-}

-- | Top-Level functionality for adding info to the
--   algebraic environment, which is in principle
--   constructed during program traversal, and used
--   to solve symbolically algebraic inequations.
module Futhark.SoP.Refine
  ( addConstraint,
    addConstraints,
  )
where

import Data.Set (Set)
import Data.Set qualified as S
import Futhark.Analysis.PrimExp
import Futhark.SoP.Constraint
import Futhark.SoP.Convert
import Futhark.SoP.Expression
import Futhark.SoP.Monad
import Futhark.SoP.RefineEquivs
import Futhark.SoP.RefineRanges
import Futhark.SoP.SoP
import Futhark.SoP.Util
import Futhark.Util.Pretty

constraintToSoP :: (Ord u, MonadSoP u e m) => Constraint u -> m (Set (SoP u == 0), Set (SoP u >= 0))
constraintToSoP (x :<=: y) = pure (mempty, S.singleton $ x .-. y)
constraintToSoP (x :>: y) = pure (mempty, S.singleton $ x .-. (y .+. int2SoP 1))
constraintToSoP (x :>=: y) = pure (mempty, S.singleton $ x .-. y)
constraintToSoP (x :==: y) = pure (S.singleton $ x .-. y, mempty)
constraintToSoP c = error $ "constraintToSoP: " <> prettyString c

addConstraint :: (ToSoP u e, MonadSoP u e m) => Constraint u -> m ()
addConstraint c = do
  (eqZs, ineqZs) <- constraintToSoP c
  extra_ineqZs <- addEqZeros eqZs
  addIneqZeros $ ineqZs <> extra_ineqZs

addConstraints :: (FromSoP u e, ToSoP u e, MonadSoP u e m) => Set (Constraint u) -> m ()
addConstraints cs = do
  -- Split candidates into equality and inequality sets.
  (eqZs, ineqZs) <- mconcat <$> mapM constraintToSoP (S.toList cs)
  -- Refine the environment with the equality set.
  extra_ineqZs <- addEqZeros eqZs
  -- Refine the environment with the extended inequality set.
  addIneqZeros $ ineqZs <> extra_ineqZs
