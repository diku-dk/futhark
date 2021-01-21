-- | Algorithmic differentiation.
module Futhark.Pass.AD (revADEntryPoints) where

import Futhark.IR.SOACS
import Futhark.Pass

revADEntryPoints :: Pass SOACS SOACS
revADEntryPoints =
  Pass
    "ad"
    "Apply reverse-mode automatic differentiation on all entry points"
    $ intraproceduralTransformationWithConsts pure transformFunDef

transformFunDef :: Stms SOACS -> FunDef SOACS -> PassM (FunDef SOACS)
transformFunDef constants fundef = do
  let constants_scope = scopeOf constants
  -- ...do whatever.
  undefined
