{-# LANGUAGE FlexibleContexts #-}
module Futhark.Pass.Simplify
  ( simplify
  , simplifySOACS
  , simplifyKernels
  , simplifyExplicitMemory
  , simplifyExplicitMemory'
  )
  where

import qualified Futhark.Representation.SOACS as R
import qualified Futhark.Representation.SOACS.Simplify as R
import qualified Futhark.Representation.Kernels as R
import qualified Futhark.Representation.Kernels.Simplify as R
import qualified Futhark.Representation.ExplicitMemory as R
import qualified Futhark.Representation.ExplicitMemory.Simplify as R

import Futhark.Pass
import Futhark.Representation.AST.Syntax

simplify :: (Prog lore -> PassM (Prog lore))
         -> Pass lore lore
simplify = Pass "simplify" "Perform simple enabling optimisations."

simplifySOACS :: Pass R.SOACS R.SOACS
simplifySOACS = simplify R.simplifySOACS

simplifyKernels :: Pass R.Kernels R.Kernels
simplifyKernels = simplify R.simplifyKernels

simplifyExplicitMemory :: Pass R.ExplicitMemory R.ExplicitMemory
simplifyExplicitMemory = simplify R.simplifyExplicitMemory

-- FIXME: Only used after memory block reuse.
simplifyExplicitMemory' :: Pass R.ExplicitMemory R.ExplicitMemory
simplifyExplicitMemory' = simplify R.simplifyExplicitMemory'
