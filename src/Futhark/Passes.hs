-- | A standard optimisation pipeline.
module Futhark.Passes
  ( standardPipeline
  )
where

import Futhark.Representation.SOACS (SOACS)
import Futhark.Pipeline
import Futhark.Pass.Untrace
import Futhark.Pass.Simplify
import Futhark.Optimise.InliningDeadFun
import Futhark.Optimise.CSE
import Futhark.Optimise.Fusion

standardPipeline :: Pipeline SOACS SOACS
standardPipeline =
  passes [ untraceProg
         , simplifySOACS
         , inlineAggressively
         , removeDeadFunctions
         , performCSE
         , simplifySOACS
           -- We run fusion twice
         , fuseSOACs
         , performCSE
         , simplifySOACS
         , fuseSOACs
         , performCSE
         , simplifySOACS
         , removeDeadFunctions
         ]
