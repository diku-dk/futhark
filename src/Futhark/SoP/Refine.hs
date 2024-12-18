{-# LANGUAGE DataKinds #-}

-- | Top-Level functionality for adding info to the
--   algebraic environment, which is in principle
--   constructed during program traversal, and used
--   to solve symbolically algebraic inequations.
module Futhark.SoP.Refine
  ( addRel,
    addRels,
  )
where

import Data.Set (Set)
import Data.Set qualified as S
import Futhark.SoP.Convert
import Futhark.SoP.Monad
import Futhark.SoP.RefineEquivs
import Futhark.SoP.RefineRanges
import Futhark.SoP.SoP
import Futhark.SoP.Util
import Futhark.Util.Pretty

-- | The result is a tuple of sets:
--   the first  set contains equal-to-zero constraints,
--   the second set contains inequality (`>= 0`) constraints
constraintToSoP :: (MonadSoP u e p m) => Rel u -> m (Set (SoP u == 0), Set (SoP u >= 0))
constraintToSoP (x :<=: y) = pure (mempty, S.singleton $ y .-. x)
constraintToSoP (x :<: y) = pure (mempty, S.singleton $ y .-. (x .+. int2SoP 1))
constraintToSoP (x :>: y) = pure (mempty, S.singleton $ x .-. (y .+. int2SoP 1))
constraintToSoP (x :>=: y) = pure (mempty, S.singleton $ x .-. y)
constraintToSoP (x :==: y) = pure (S.singleton $ x .-. y, mempty)
constraintToSoP (x :&&: y) = (<>) <$> constraintToSoP x <*> constraintToSoP y
constraintToSoP c = error $ "constraintToSoP: " <> prettyString c

addRel :: (ToSoP u e, MonadSoP u e p m) => Rel u -> m ()
addRel c = do
  (eqZs, ineqZs) <- constraintToSoP c
  extra_ineqZs <- addEqZeros eqZs
  addIneqZeros $ ineqZs <> extra_ineqZs

addRels :: (ToSoP u e, MonadSoP u e p m) => Set (Rel u) -> m ()
addRels cs = do
  -- Split candidates into equality and inequality sets.
  (eqZs, ineqZs) <- mconcat <$> mapM constraintToSoP (S.toList cs)
  -- Refine the environment with the equality set.
  extra_ineqZs <- addEqZeros eqZs
  -- Refine the environment with the extended inequality set.
  addIneqZeros $ ineqZs <> extra_ineqZs
