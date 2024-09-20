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


traceHelper :: Show a => a -> a
traceHelper x = trace (show x ++ "\n") $ x


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
--transformExp (BasicOp op) = BasicOp op
--transformExp (Apply name args rets info) = Apply name args rets info
transformExp (Match subExps cases body matchDec) = Match subExps (fmap transformCase cases) (transformBody body) matchDec
transformExp (Loop params form body) = Loop params form (transformBody body)
transformExp (Op op) = Op (transformOp op)
transformExp e = e

transformCase :: Case (Body GPU) -> Case (Body GPU)
transformCase (Case pat body) = Case pat (transformBody body)

transformBody :: Body GPU -> Body GPU
transformBody (Body dec stms res) = Body dec (transformStmts stms) res

transformOp :: Op GPU -> Op GPU
transformOp (SegOp sOp) = SegOp (transformSegOp sOp)
-- TODO: handle these separately?
transformOp op = op

transformSegOp :: SegOp SegLevel GPU -> SegOp SegLevel GPU
-- TODO: may be able to transform here, also match level?
transformSegOp (SegMap level space ts body) = traceHelper $ SegMap level space ts (transformKernelBody body)
--transformSegOp (SegMap level space ts body) = SegMap level space ts (transformKernelBody body)
transformSegOp (SegRed level space ops ts body) = SegRed level space ops ts (transformKernelBody body)
transformSegOp (SegScan level space ops ts body) = SegScan level space ops ts (transformKernelBody body)
transformSegOp (SegHist level space ops hist body) = SegHist level space ops hist (transformKernelBody body)

transformKernelBody :: KernelBody GPU -> KernelBody GPU
transformKernelBody (KernelBody desc stms res) = KernelBody desc (transformStmts stms) res