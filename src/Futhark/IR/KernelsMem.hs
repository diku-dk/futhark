{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.IR.KernelsMem
  ( KernelsMem,

    -- * Simplification
    simplifyProg,
    simplifyStms,
    simpleKernelsMem,

    -- * Module re-exports
    module Futhark.IR.Mem,
    module Futhark.IR.Kernels.Kernel,
  )
where

import Futhark.Analysis.PrimExp.Convert
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.IR.Kernels.Kernel
import Futhark.IR.Kernels.Simplify (simplifyKernelOp)
import Futhark.IR.Mem
import Futhark.IR.Mem.Simplify
import Futhark.MonadFreshNames
import qualified Futhark.Optimise.Simplify.Engine as Engine
import Futhark.Pass
import Futhark.Pass.ExplicitAllocations (BinderOps (..), mkLetNamesB', mkLetNamesB'')
import qualified Futhark.TypeCheck as TC

data KernelsMem

instance RepTypes KernelsMem where
  type LetDec KernelsMem = LetDecMem
  type FParamInfo KernelsMem = FParamMem
  type LParamInfo KernelsMem = LParamMem
  type RetType KernelsMem = RetTypeMem
  type BranchType KernelsMem = BranchTypeMem
  type Op KernelsMem = MemOp (HostOp KernelsMem ())

instance ASTRep KernelsMem where
  expTypesFromPattern = return . map snd . snd . bodyReturnsFromPattern

instance OpReturns KernelsMem where
  opReturns (Alloc _ space) =
    return [MemMem space]
  opReturns (Inner (SegOp op)) = segOpReturns op
  opReturns k = extReturns <$> opType k

instance PrettyRep KernelsMem

instance TC.CheckableOp KernelsMem where
  checkOp = typeCheckMemoryOp Nothing
    where
      typeCheckMemoryOp _ (Alloc size _) =
        TC.require [Prim int64] size
      typeCheckMemoryOp lvl (Inner op) =
        typeCheckHostOp (typeCheckMemoryOp . Just) lvl (const $ return ()) op

instance TC.Checkable KernelsMem where
  checkFParamDec = checkMemInfo
  checkLParamDec = checkMemInfo
  checkLetBoundDec = checkMemInfo
  checkRetType = mapM_ $ TC.checkExtType . declExtTypeOf
  primFParam name t = return $ Param name (MemPrim t)
  matchPattern = matchPatternToExp
  matchReturnType = matchFunctionReturnType
  matchBranchType = matchBranchReturnType
  matchLoopResult = matchLoopResultMem

instance BinderOps KernelsMem where
  mkExpDecB _ _ = return ()
  mkBodyB stms res = return $ Body () stms res
  mkLetNamesB = mkLetNamesB' ()

instance BinderOps (Engine.Wise KernelsMem) where
  mkExpDecB pat e = return $ Engine.mkWiseExpDec pat () e
  mkBodyB stms res = return $ Engine.mkWiseBody () stms res
  mkLetNamesB = mkLetNamesB''

simplifyProg :: Prog KernelsMem -> PassM (Prog KernelsMem)
simplifyProg = simplifyProgGeneric simpleKernelsMem

simplifyStms ::
  (HasScope KernelsMem m, MonadFreshNames m) =>
  Stms KernelsMem ->
  m
    ( Engine.SymbolTable (Engine.Wise KernelsMem),
      Stms KernelsMem
    )
simplifyStms = simplifyStmsGeneric simpleKernelsMem

simpleKernelsMem :: Engine.SimpleOps KernelsMem
simpleKernelsMem =
  simpleGeneric usage $ simplifyKernelOp $ const $ return ((), mempty)
  where
    -- Slightly hackily, we look at the inside of SegGroup operations
    -- to figure out the sizes of local memory allocations, and add
    -- usages for those sizes.  This is necessary so the simplifier
    -- will hoist those sizes out as far as possible (most
    -- importantly, past the versioning If).
    usage (SegOp (SegMap SegGroup {} _ _ kbody)) = localAllocs kbody
    usage _ = mempty
    localAllocs = foldMap stmLocalAlloc . kernelBodyStms
    stmLocalAlloc = expLocalAlloc . stmExp
    expLocalAlloc (Op (Alloc (Var v) (Space "local"))) =
      UT.sizeUsage v
    expLocalAlloc _ =
      mempty
