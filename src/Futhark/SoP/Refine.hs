-- | Top-Level functionality for adding info to the
--   algebraic environment, which is in principle
--   constructed during program traversal, and used
--   to solve symbolically algebraic inequations.
module Futhark.SoP.Refine
  ( refineAlgEnv,
  )
where

import Data.Set (Set)
import Futhark.Analysis.PrimExp
import Futhark.SoP.Monad
import Futhark.SoP.PrimExp
import Futhark.SoP.RefineEquivs
import Futhark.SoP.RefineRanges
import Futhark.SoP.ToFromSoP

refineAlgEnv ::
  MonadSoP u m =>
  Set (PrimExp u) ->
  m ()
refineAlgEnv candidates = do
  -- Split candidates into equality and inequality sets.
  let (eqZs, ineqZs) = processPEs candidates

  -- Refine the environment with the equality set.
  extra_ineqZs <- addEqZeroPEs eqZs

  -- Refine the environment with the extended inequality set.
  addIneqZeroPEs $ ineqZs <> fromSoP extra_ineqZs
