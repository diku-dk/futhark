module Futhark.Optimise.IntraMMM (intraMMM) where

import Futhark.Pass
  ( intraproceduralTransformation,
    Pass (..),
    PassM
  )
-- TODO: add specific imports
import Futhark.IR.GPU.Simplify (simplifyGPU)
import Futhark.IR.GPU
import Futhark.IR.GPU.Op
import Futhark.IR.Prop.Scope (Scope)
import Futhark.IR.Syntax
import Control.Monad ((>=>))
import Debug.Trace


intraMMM :: Pass GPU GPU
intraMMM =
  Pass
  "extractTensorCoreOps"
  "Extracts NVIDIA tensor core MMA operations" $
  transformation >=> simplifyGPU
  where transformation = intraproceduralTransformation onStmts


onStmts :: Scope GPU -> Stms GPU -> PassM (Stms GPU)
onStmts scope stms = pure $ transformStmts stms

-- TODO: use PassM below?
transformStmts :: Stms GPU -> Stms GPU
transformStmts stms = fmap transformStmt stms

transformStmt :: Stm GPU -> Stm GPU
transformStmt (Let pat aux e) = Let pat aux (transformExp e)

transformExp :: Exp GPU -> Exp GPU
transformExp (Op op) = Op (transformOp op)
transformExp e = e

transformOp :: Op GPU -> Op GPU
transformOp (SegOp sOp) = SegOp (transformSegOp sOp)
transformOp op = op

transformSegOp :: SegOp SegLevel GPU -> SegOp SegLevel GPU
-- TODO: transform here
transformSegOp (SegMap level space ts body) = trace (show body) $ SegMap level space ts body
--transformSegOp (SegMap level space ts body) = SegMap level space ts (transformBody body)
transformSegOp (SegRed level space ops ts body) = SegRed level space ops ts (transformBody body)
transformSegOp (SegScan level space ops ts body) = SegScan level space ops ts (transformBody body)
transformSegOp (SegHist level space ops hist body) = SegHist level space ops hist (transformBody body)

transformBody :: KernelBody GPU -> KernelBody GPU
transformBody (KernelBody desc stms res) = KernelBody desc (transformStmts stms) res