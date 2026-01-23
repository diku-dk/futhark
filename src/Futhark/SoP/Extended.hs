module Futhark.SoP.SoP.Extended () where

import Futhark.SoP.SoP

-- data BuriedExpr u
--  = SoPExp (SoP u)
--  | Slice (SoP u) (SoP u) (SoP u)
--  deriving (Eq, Ord, Show)

class
  ( Ord u,
    Ord e,
    Nameable u,
    Show u, -- To be removed
    Pretty u, -- To be removed
    MonadFreshNames m,
    Expression e
  ) =>
  MonadSoPExtended u e m
    | m -> u,
      m -> e
  where
  getUntrans :: m (UntransEnv u e)
  getRanges :: m (RangeEnv u)
  getEquivs :: m (EquivEnv u)
  getProps :: m (PropEnv u)
  modifyEnv :: (AlgEnv u e -> AlgEnv u e) -> m ()

-- i < j
-- x --> monotonic
-- ==> x[i] - x[j] < 0
--
