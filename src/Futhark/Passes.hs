{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Optimisation pipelines.
module Futhark.Passes
  ( standardPipeline
  , sequentialPipeline
  , gpuPipeline

  , CompilationMode (..)
  )
where

import Control.Category ((>>>), id)
import Control.Monad.Except

import Prelude hiding (id)

import Futhark.Optimise.CSE
import Futhark.Optimise.Fusion
import Futhark.Optimise.InPlaceLowering
import Futhark.Optimise.InliningDeadFun
import Futhark.Optimise.TileLoops
import Futhark.Optimise.DoubleBuffer
import Futhark.Pass.ExpandAllocations
import Futhark.Pass.ExplicitAllocations
import Futhark.Pass.ExtractKernels
import Futhark.Pass.FirstOrderTransform
import Futhark.Pass.KernelBabysitting
import Futhark.Pass.Simplify
import Futhark.Pass
import Futhark.Pipeline
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import Futhark.Representation.SOACS (SOACS)
import Futhark.Representation.AST.Syntax

-- | Are we compiling the Futhark program as an executable or a
-- library?  This affects which functions are considered as roots for
-- dead code elimination and ultimately exist in generated code.
data CompilationMode = Executable
                     -- ^ Only the top-level function named @main@ is
                       -- alive.
                     | Library
                       -- ^ Only top-level functions marked @entry@
                       -- are alive.

standardPipeline :: CompilationMode -> Pipeline SOACS SOACS
standardPipeline mode =
  markEntryPoints mode >>>
  passes [ simplifySOACS
         , inlineAggressively
         , removeDeadFunctions
         , performCSE True
         , simplifySOACS
           -- We run fusion twice
         , fuseSOACs
         , performCSE True
         , simplifySOACS
         , fuseSOACs
         , performCSE True
         , simplifySOACS
         , removeDeadFunctions
         ]
  where markEntryPoints :: CompilationMode -> Pipeline SOACS SOACS
        markEntryPoints Library = id
        markEntryPoints Executable =
          onePass Pass { passName = "Mark main function"
                       , passDescription = "Mark the main function as entry point"
                       , passFunction = \(Prog ps) -> do checkForMain ps
                                                         return $ Prog $ map setEntry ps
                       }
        setEntry fd = fd { funDefEntryPoint = funDefName fd == defaultEntryPoint }

        checkForMain ps
          | not $ any ((==defaultEntryPoint) . funDefName) ps =
              throwError "No main function defined."
          | otherwise =
              return ()

sequentialPipeline :: CompilationMode -> Pipeline SOACS ExplicitMemory
sequentialPipeline mode =
  standardPipeline mode >>>
  onePass firstOrderTransform >>>
  passes [ simplifyKernels
         , inPlaceLowering
         ] >>>
  onePass explicitAllocations >>>
  passes [ simplifyExplicitMemory
         , performCSE False
         , simplifyExplicitMemory
         , doubleBuffer
         , simplifyExplicitMemory
         ]

gpuPipeline :: CompilationMode -> Pipeline SOACS ExplicitMemory
gpuPipeline mode =
  standardPipeline mode >>>
  onePass extractKernels >>>
  passes [ simplifyKernels
         , babysitKernels
         , simplifyKernels
         , tileLoops
         , performCSE True
         , simplifyKernels
         , inPlaceLowering
         ] >>>
  onePass explicitAllocations >>>
  passes [ simplifyExplicitMemory
         , performCSE False
         , simplifyExplicitMemory
         , doubleBuffer
         , simplifyExplicitMemory
         , expandAllocations
         , simplifyExplicitMemory
         ]
