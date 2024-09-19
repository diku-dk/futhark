module Futhark.Analysis.Proofs.Rule where

import Futhark.Analysis.Proofs.Unify (Substitution)

data Rule a b m = Rule
  { name :: String,
    from :: a,
    to :: Substitution b -> m a,
    sideCondition :: Substitution b -> m Bool
  }

type RuleBook a b m = [Rule a b m]
