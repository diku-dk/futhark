{-# LANGUAGE TypeFamilies #-}

module Futhark.IR.MCMem
  ( MCMem,

    -- * Simplification
    simplifyProg,

    -- * Module re-exports
    module Futhark.IR.Mem,
    module Futhark.IR.SegOp,
    module Futhark.IR.MC.Op,
  )
where

import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.MC.Op
import Futhark.IR.Mem
import Futhark.IR.Mem.Simplify
import Futhark.IR.SegOp
import Futhark.IR.TypeCheck qualified as TC
import Futhark.Optimise.Simplify.Engine qualified as Engine
import Futhark.Pass
import Futhark.Pass.ExplicitAllocations (BuilderOps (..), mkLetNamesB', mkLetNamesB'')

data MCMem

instance RepTypes MCMem where
  type LetDec MCMem = LetDecMem
  type FParamInfo MCMem = FParamMem
  type LParamInfo MCMem = LParamMem
  type RetType MCMem = RetTypeMem
  type BranchType MCMem = BranchTypeMem
  type OpC MCMem = MemOp (MCOp NoOp)

instance ASTRep MCMem where
  expTypesFromPat = pure . map snd . bodyReturnsFromPat

instance PrettyRep MCMem

instance TC.Checkable MCMem where
  checkOp = typeCheckMemoryOp
    where
      typeCheckMemoryOp (Alloc size _) =
        TC.require [Prim int64] size
      typeCheckMemoryOp (Inner op) =
        typeCheckMCOp (const $ pure ()) op
  checkFParamDec = checkMemInfo
  checkLParamDec = checkMemInfo
  checkLetBoundDec = checkMemInfo
  checkRetType = mapM_ (TC.checkExtType . declExtTypeOf)
  primFParam name t = pure $ Param mempty name (MemPrim t)
  matchPat = matchPatToExp
  matchReturnType = matchFunctionReturnType
  matchBranchType = matchBranchReturnType
  matchLoopResult = matchLoopResultMem

instance BuilderOps MCMem where
  mkExpDecB _ _ = pure ()
  mkBodyB stms res = pure $ Body () stms res
  mkLetNamesB = mkLetNamesB' DefaultSpace ()

instance BuilderOps (Engine.Wise MCMem) where
  mkExpDecB pat e = pure $ Engine.mkWiseExpDec pat () e
  mkBodyB stms res = pure $ Engine.mkWiseBody () stms res
  mkLetNamesB = mkLetNamesB'' DefaultSpace

instance TraverseOpStms (Engine.Wise MCMem) where
  traverseOpStms = traverseMemOpStms (traverseMCOpStms (const pure))

simplifyProg :: Prog MCMem -> PassM (Prog MCMem)
simplifyProg = simplifyProgGeneric memRuleBook simpleMCMem

simpleMCMem :: Engine.SimpleOps MCMem
simpleMCMem =
  simpleGeneric (const mempty) $ simplifyMCOp $ const $ pure (NoOp, mempty)
