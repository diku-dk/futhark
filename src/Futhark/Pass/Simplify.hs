module Futhark.Pass.Simplify
  ( simplify
  , simplifyBasic
  , simplifyExplicitMemory

  , Simplifiable
  )
  where

import Control.Monad

import qualified Futhark.Representation.Basic as R
import qualified Futhark.Representation.ExplicitMemory as R

import qualified Futhark.Pass.ExplicitAllocations
import Futhark.Optimise.Simplifier
import Futhark.Optimise.Simplifier.Simple
import Futhark.Optimise.DeadVarElim
import Futhark.Pass

simplify :: Simplifiable lore =>
            SimpleOps (SimpleM lore)
         -> RuleBook (SimpleM lore)
         -> Pass lore lore
simplify simpl rules =
  simplePass
  "simplify"
  "Perform simple enabling optimisations." $
  -- XXX: A given simplification rule may leave the program in a form
  -- that is technically type-incorrect, but which will be correct
  -- after copy-propagation.  Right now, we just run the simplifier a
  -- number of times and hope that it is enough.  Will be fixed later;
  -- promise.
  foldl (<=<) return (replicate num_passes pass)
  where pass = liftM deadCodeElim . simplifyProgWithRules simpl rules
        num_passes = 5

simplifyBasic :: Pass R.Basic R.Basic
simplifyBasic = simplify bindableSimpleOps basicRules

simplifyExplicitMemory :: Pass R.ExplicitMemory R.ExplicitMemory
simplifyExplicitMemory = simplify Futhark.Pass.ExplicitAllocations.simplifiable standardRules
