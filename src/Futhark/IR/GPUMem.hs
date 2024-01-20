{-# LANGUAGE TypeFamilies #-}

module Futhark.IR.GPUMem
  ( GPUMem,

    -- * Simplification
    simplifyProg,
    simplifyStms,
    simpleGPUMem,

    -- * Module re-exports
    module Futhark.IR.Mem,
    module Futhark.IR.GPU.Op,
  )
where

import Futhark.Analysis.PrimExp.Convert
import Futhark.Analysis.UsageTable qualified as UT
import Futhark.IR.Aliases (Aliases)
import Futhark.IR.GPU.Op
import Futhark.IR.GPU.Simplify (simplifyKernelOp)
import Futhark.IR.Mem
import Futhark.IR.Mem.Simplify
import Futhark.IR.TypeCheck qualified as TC
import Futhark.MonadFreshNames
import Futhark.Optimise.Simplify.Engine qualified as Engine
import Futhark.Pass
import Futhark.Pass.ExplicitAllocations (BuilderOps (..), mkLetNamesB', mkLetNamesB'')

data GPUMem

instance RepTypes GPUMem where
  type LetDec GPUMem = LetDecMem
  type FParamInfo GPUMem = FParamMem
  type LParamInfo GPUMem = LParamMem
  type RetType GPUMem = RetTypeMem
  type BranchType GPUMem = BranchTypeMem
  type OpC GPUMem = MemOp (HostOp NoOp)

instance ASTRep GPUMem where
  expTypesFromPat = pure . map snd . bodyReturnsFromPat

instance OpReturns (HostOp NoOp GPUMem) where
  opReturns (SegOp op) = segOpReturns op
  opReturns k = extReturns <$> opType k

instance OpReturns (HostOp NoOp (Aliases GPUMem)) where
  opReturns (SegOp op) = segOpReturns op
  opReturns k = extReturns <$> opType k

instance OpReturns (HostOp NoOp (Engine.Wise GPUMem)) where
  opReturns (SegOp op) = segOpReturns op
  opReturns k = extReturns <$> opType k

instance PrettyRep GPUMem

instance TC.Checkable GPUMem where
  checkOp = typeCheckMemoryOp Nothing
    where
      -- GHC 9.2 goes into an infinite loop without the type annotation.
      typeCheckMemoryOp ::
        Maybe SegLevel ->
        MemOp (HostOp NoOp) (Aliases GPUMem) ->
        TC.TypeM GPUMem ()
      typeCheckMemoryOp _ (Alloc size _) =
        TC.require [Prim int64] size
      typeCheckMemoryOp lvl (Inner op) =
        typeCheckHostOp (typeCheckMemoryOp . Just) lvl (const $ pure ()) op
  checkFParamDec = checkMemInfo
  checkLParamDec = checkMemInfo
  checkLetBoundDec = checkMemInfo
  checkRetType = mapM_ $ TC.checkExtType . declExtTypeOf
  primFParam name t = pure $ Param mempty name (MemPrim t)
  matchPat = matchPatToExp
  matchReturnType = matchFunctionReturnType
  matchBranchType = matchBranchReturnType
  matchLoopResult = matchLoopResultMem

instance BuilderOps GPUMem where
  mkExpDecB _ _ = pure ()
  mkBodyB stms res = pure $ Body () stms res
  mkLetNamesB = mkLetNamesB' (Space "device") ()

instance BuilderOps (Engine.Wise GPUMem) where
  mkExpDecB pat e = pure $ Engine.mkWiseExpDec pat () e
  mkBodyB stms res = pure $ Engine.mkWiseBody () stms res
  mkLetNamesB = mkLetNamesB'' (Space "device")

instance TraverseOpStms (Engine.Wise GPUMem) where
  traverseOpStms = traverseMemOpStms (traverseHostOpStms (const pure))

simplifyProg :: Prog GPUMem -> PassM (Prog GPUMem)
simplifyProg = simplifyProgGeneric memRuleBook simpleGPUMem

simplifyStms ::
  (HasScope GPUMem m, MonadFreshNames m) => Stms GPUMem -> m (Stms GPUMem)
simplifyStms = simplifyStmsGeneric memRuleBook simpleGPUMem

simpleGPUMem :: Engine.SimpleOps GPUMem
simpleGPUMem =
  simpleGeneric usage $ simplifyKernelOp $ const $ pure (NoOp, mempty)
  where
    -- Slightly hackily and very inefficiently, we look at the inside
    -- of SegOps to figure out the sizes of shared memory allocations,
    -- and add usages for those sizes.  This is necessary so the
    -- simplifier will hoist those sizes out as far as possible (most
    -- importantly, past the versioning If, but see also #1569).
    usage (SegOp (SegMap _ _ _ kbody)) = localAllocs kbody
    usage _ = mempty
    localAllocs = foldMap stmSharedAlloc . kernelBodyStms
    stmSharedAlloc = expSharedAlloc . stmExp
    expSharedAlloc (Op (Alloc (Var v) _)) =
      UT.sizeUsage v
    expSharedAlloc (Op (Inner (SegOp (SegMap _ _ _ kbody)))) =
      localAllocs kbody
    expSharedAlloc _ =
      mempty
