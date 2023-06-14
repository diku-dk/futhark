-- | Top-Level functionality for adding info to the
--   algebraic environment, which is in principle
--   constructed during program traversal, and used
--   to solve symbolically algebraic inequations.
module Futhark.SoP.Refine
  ( refineAlgEnv,
  )
where

import Data.Set (Set)
import Data.Set qualified as S
import Futhark.Analysis.PrimExp
import Futhark.SoP.Convert
import Futhark.SoP.Expression
import Futhark.SoP.Monad
import Futhark.SoP.RefineEquivs
import Futhark.SoP.RefineRanges

refineAlgEnv :: (FromSoP u e, ToSoP u e, MonadSoP u e m) => Set e -> m ()
refineAlgEnv candidates = do
  -- Split candidates into equality and inequality sets.
  let (eqZs, ineqZs) = processExps candidates

  -- Refine the environment with the equality set.
  extra_ineqZs <- addEqZeroPEs eqZs

  -- Refine the environment with the extended inequality set.
  addIneqZeroPEs $ ineqZs <> S.map fromSoP extra_ineqZs
