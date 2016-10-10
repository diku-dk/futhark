{-# LANGUAGE FlexibleContexts #-}
module Futhark.Pass.Simplify
  ( simplify
  , simplifySOACS
  , simplifyKernels
  , simplifyExplicitMemory
  )
  where

import Control.Monad.State

import qualified Futhark.Representation.SOACS as R
import qualified Futhark.Representation.SOACS.Simplify as R
import qualified Futhark.Representation.Kernels as R
import qualified Futhark.Representation.Kernels.Simplify as R
import qualified Futhark.Representation.ExplicitMemory as R
import qualified Futhark.Representation.ExplicitMemory.Simplify as R

import Futhark.Optimise.DeadVarElim
import Futhark.Pass
import Futhark.MonadFreshNames
import Futhark.Representation.AST.Syntax

simplify :: R.Attributes lore =>
            (Prog lore -> State VNameSource (Prog lore))
         -> Pass lore lore
simplify f =
  simplePass
  "simplify"
  "Perform simple enabling optimisations." $
  fmap deadCodeElim . f

simplifySOACS :: Pass R.SOACS R.SOACS
simplifySOACS = simplify R.simplifySOACS

simplifyKernels :: Pass R.Kernels R.Kernels
simplifyKernels = simplify R.simplifyKernels

simplifyExplicitMemory :: Pass R.ExplicitMemory R.ExplicitMemory
simplifyExplicitMemory = simplify R.simplifyExplicitMemory
