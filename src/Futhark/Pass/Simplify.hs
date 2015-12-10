{-# LANGUAGE FlexibleContexts #-}
module Futhark.Pass.Simplify
  ( simplify
  , simplifySOACS
  , simplifyExplicitMemory
  )
  where

import Control.Monad
import Control.Monad.State

import qualified Futhark.Representation.SOACS as R
import qualified Futhark.Representation.SOACS.Simplify as R
import qualified Futhark.Representation.ExplicitMemory as R
import qualified Futhark.Representation.ExplicitMemory.Simplify as R

import Futhark.Optimise.DeadVarElim
import Futhark.Pass
import Futhark.MonadFreshNames
import Futhark.Representation.AST.Syntax
import Futhark.Binder.Class (Proper)

simplify :: Proper lore =>
            (Prog lore -> State VNameSource (Prog lore))
         -> Pass lore lore
simplify f =
  simplePass
  "simplify"
  "Perform simple enabling optimisations." $
  -- XXX: A given simplification rule may leave the program in a form
  -- that is technically type-incorrect, but which will be correct
  -- after copy-propagation.  Right now, we just run the simplifier a
  -- number of times and hope that it is enough.  Will be fixed later;
  -- promise.
  foldl (<=<) return (replicate num_passes pass)
  where pass = liftM deadCodeElim . f
        num_passes = 5

simplifySOACS :: Pass R.SOACS R.SOACS
simplifySOACS = simplify R.simplifySOACS

simplifyExplicitMemory :: Pass R.ExplicitMemory R.ExplicitMemory
simplifyExplicitMemory = simplify R.simplifyExplicitMemory
