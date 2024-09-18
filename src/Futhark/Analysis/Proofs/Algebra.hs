module Futhark.Analysis.Proofs.Algebra where

import Control.Monad.RWS.Strict
import Futhark.Analysis.Proofs.Rule
import Futhark.Analysis.Proofs.Symbol (Symbol)
import Futhark.SoP.SoP (SoP)

data AlgExp u
  = Sum
  | Pow2 (SoP u)
  | Leaf (SoP u)
  deriving (Show, Eq, Ord)

type AlgSoP = SoP (AlgExp Symbol)

data Env

newtype AlgM a = AlgM {runAlgM :: RWS () () Env a}

rules :: RuleBook AlgSoP Symbol AlgM
rules = undefined
