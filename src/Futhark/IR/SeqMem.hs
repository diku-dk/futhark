{-# LANGUAGE TypeFamilies #-}

module Futhark.IR.SeqMem
  ( SeqMem,

    -- * Simplification
    simplifyProg,
    simpleSeqMem,

    -- * Module re-exports
    module Futhark.IR.Mem,
  )
where

import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Aliases (Aliases)
import Futhark.IR.Mem
import Futhark.IR.Mem.Simplify
import Futhark.IR.TypeCheck qualified as TC
import Futhark.Optimise.Simplify.Engine qualified as Engine
import Futhark.Pass
import Futhark.Pass.ExplicitAllocations (BuilderOps (..), mkLetNamesB', mkLetNamesB'')

data SeqMem

instance RepTypes SeqMem where
  type LetDec SeqMem = LetDecMem
  type FParamInfo SeqMem = FParamMem
  type LParamInfo SeqMem = LParamMem
  type RetType SeqMem = RetTypeMem
  type BranchType SeqMem = BranchTypeMem
  type OpC SeqMem = MemOp NoOp

instance ASTRep SeqMem where
  expTypesFromPat = pure . map snd . bodyReturnsFromPat

instance PrettyRep SeqMem

instance TC.Checkable (Aliases SeqMem) where
  checkOp (Alloc size _) = TC.require [Prim int64] size
  checkOp (Inner NoOp) = pure ()
  checkFParamDec = checkMemInfo
  checkLParamDec = checkMemInfo
  checkLetBoundDec v (_, dec) = checkMemInfo v dec
  checkRetType = mapM_ (TC.checkExtType . declExtTypeOf)
  primFParam name t = pure $ Param mempty name (MemPrim t)
  matchPat = matchPatToExp
  matchReturnType = matchFunctionReturnType
  matchBranchType = matchBranchReturnType
  matchLoopResult = matchLoopResultMem

instance BuilderOps SeqMem where
  mkExpDecB _ _ = pure ()
  mkBodyB stms res = pure $ Body () stms res
  mkLetNamesB = mkLetNamesB' DefaultSpace ()

instance TraverseOpStms SeqMem where
  traverseOpStms _ = pure

instance BuilderOps (Engine.Wise SeqMem) where
  mkExpDecB pat e = pure $ Engine.mkWiseExpDec pat () e
  mkBodyB stms res = pure $ Engine.mkWiseBody () stms res
  mkLetNamesB = mkLetNamesB'' DefaultSpace

instance TraverseOpStms (Engine.Wise SeqMem) where
  traverseOpStms _ = pure

simplifyProg :: Prog SeqMem -> PassM (Prog SeqMem)
simplifyProg = simplifyProgGeneric memRuleBook simpleSeqMem

simpleSeqMem :: Engine.SimpleOps SeqMem
simpleSeqMem =
  simpleGeneric (const mempty) $ const $ pure (NoOp, mempty)
