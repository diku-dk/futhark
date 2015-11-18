-- | A standard optimisation pipeline.
module Futhark.Passes
  ( standardPipeline
  )
where

import Futhark.Representation.Basic (Basic)
import Futhark.Pipeline
import Futhark.Pass.Untrace
import Futhark.Pass.Simplify
import Futhark.Optimise.InliningDeadFun
import Futhark.Optimise.CSE
import Futhark.Optimise.Fusion

standardPipeline :: Pipeline Basic Basic
standardPipeline =
  passes [ untraceProg
         , simplifyBasic
         , inlineAggressively
         , removeDeadFunctions
         , performCSE
         , simplifyBasic
           -- We run fusion twice
         , fuseSOACs
         , performCSE
         , simplifyBasic
         , fuseSOACs
         , performCSE
         , simplifyBasic
         , removeDeadFunctions
         ]
