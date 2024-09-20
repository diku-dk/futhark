module Futhark.Optimise.IntraMMM (intraMMM) where

import Futhark.Pass 
  ( intraproceduralTransformation, 
    Pass (..),
    PassM
  )
import Futhark.IR.GPU.Simplify (simplifyGPU)
import Futhark.IR.GPU (GPU)
import Futhark.IR.Prop.Scope (Scope)
import Futhark.IR.Syntax (Stms)
import Control.Monad ((>=>))


intraMMM :: Pass GPU GPU
intraMMM = 
  Pass
  "extractTensorCoreOps"
  "Extracts NVIDIA tensor core MMA operations" $
  transformation >=> simplifyGPU
  where transformation = intraproceduralTransformation onStmts 


onStmts :: Scope GPU -> Stms GPU -> PassM (Stms GPU)
onStmts = error "foo"

transformStmts :: Stms GPU -> String
transformStmts stms = ""