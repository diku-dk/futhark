module Futhark.Passes
  ( CompileError(..)
  , FutharkM
  , compileError
  , Pass(..)
  , canFail
  , fotransform
  , uttransform
  , eotransform
  , hotransform
  , inlinetransform
  , removeDeadFunctions
  , optimisePredicates
  , optimiseShapes
  , explicitMemory
  )
where

import Control.Applicative

import Futhark.Optimise.SimpleOpts
import Futhark.Optimise.Fusion
import qualified Futhark.FirstOrderTransform as FOT
import Futhark.Untrace
import Futhark.Pipeline
import Futhark.Optimise.InliningDeadFun
import qualified Futhark.Optimise.SuffCond
import qualified Futhark.Optimise.SplitShapes
import qualified Futhark.ExplicitAllocations

fotransform :: Pass
fotransform = unfailableBasicPass "first-order transform"
              FOT.transformProg

uttransform :: Pass
uttransform = unfailableBasicPass "debugging annotation removal"
              untraceProg

eotransform :: Pass
eotransform = polyPass "enabling optimations" op
  where op (Basic prog)          = canFail "" (Just $ Basic prog) $
                                   Basic <$> simpleOpts bindableSimplifiable prog
        op (ExplicitMemory prog) = canFail "" (Just $ ExplicitMemory prog) $
                                   ExplicitMemory <$> simpleOpts Futhark.ExplicitAllocations.simplifiable prog

hotransform :: Pass
hotransform = basicPass "higher-order optimisations"
              fuseProg

inlinetransform :: Pass
inlinetransform = basicPass "inline functions"
                  aggInlineDriver

removeDeadFunctions :: Pass
removeDeadFunctions = basicPass "Remove dead functions"
                      deadFunElim

optimisePredicates :: Pass
optimisePredicates = unfailableBasicPass "optimise predicates"
                     Futhark.Optimise.SuffCond.optimiseProg

optimiseShapes :: Pass
optimiseShapes = unfailableBasicPass "optimise shape analysis"
                 Futhark.Optimise.SplitShapes.splitShapes

explicitMemory :: Pass
explicitMemory = polyPass "insert explicit allocations" op
  where op s = do prog <- basicProg s
                  return $ ExplicitMemory $
                    Futhark.ExplicitAllocations.explicitAllocations prog
