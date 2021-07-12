{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.IR.GPUMem
  ( GPUMem,

    -- * Simplification
    simplifyProg,
    simplifyStms,
    simpleGPUMem,

    -- * Module re-exports
    module Futhark.IR.Mem,
    module Futhark.IR.GPU.Kernel,
  )
where

import Futhark.Analysis.PrimExp.Convert
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.IR.GPU.Kernel
import Futhark.IR.GPU.Simplify (simplifyKernelOp)
import Futhark.IR.Mem
import Futhark.IR.Mem.Simplify
import Futhark.MonadFreshNames
import qualified Futhark.Optimise.Simplify.Engine as Engine
import Futhark.Pass
import Futhark.Pass.ExplicitAllocations (BuilderOps (..), mkLetNamesB', mkLetNamesB'')
import qualified Futhark.TypeCheck as TC

data GPUMem

instance RepTypes GPUMem where
  type LetDec GPUMem = LetDecMem
  type FParamInfo GPUMem = FParamMem
  type LParamInfo GPUMem = LParamMem
  type RetType GPUMem = RetTypeMem
  type BranchType GPUMem = BranchTypeMem
  type Op GPUMem = MemOp (HostOp GPUMem ())

instance ASTRep GPUMem where
  expTypesFromPat = return . map snd . bodyReturnsFromPat

instance OpReturns GPUMem where
  opReturns (Alloc _ space) =
    return [MemMem space]
  opReturns (Inner (SegOp op)) = segOpReturns op
  opReturns k = extReturns <$> opType k

instance PrettyRep GPUMem

instance TC.CheckableOp GPUMem where
  checkOp = typeCheckMemoryOp Nothing
    where
      typeCheckMemoryOp _ (Alloc size _) =
        TC.require [Prim int64] size
      typeCheckMemoryOp lvl (Inner op) =
        typeCheckHostOp (typeCheckMemoryOp . Just) lvl (const $ return ()) op

instance TC.Checkable GPUMem where
  checkFParamDec = checkMemInfo
  checkLParamDec = checkMemInfo
  checkLetBoundDec = checkMemInfo
  checkRetType = mapM_ $ TC.checkExtType . declExtTypeOf
  primFParam name t = return $ Param name (MemPrim t)
  matchPat = matchPatToExp
  matchReturnType = matchFunctionReturnType
  matchBranchType = matchBranchReturnType
  matchLoopResult = matchLoopResultMem

instance BuilderOps GPUMem where
  mkExpDecB _ _ = return ()
  mkBodyB stms res = return $ Body () stms res
  mkLetNamesB = mkLetNamesB' ()

instance BuilderOps (Engine.Wise GPUMem) where
  mkExpDecB pat e = return $ Engine.mkWiseExpDec pat () e
  mkBodyB stms res = return $ Engine.mkWiseBody () stms res
  mkLetNamesB = mkLetNamesB''

simplifyProg :: Prog GPUMem -> PassM (Prog GPUMem)
simplifyProg = simplifyProgGeneric simpleGPUMem

simplifyStms ::
  (HasScope GPUMem m, MonadFreshNames m) =>
  Stms GPUMem ->
  m
    ( Engine.SymbolTable (Engine.Wise GPUMem),
      Stms GPUMem
    )
simplifyStms = simplifyStmsGeneric simpleGPUMem

simpleGPUMem :: Engine.SimpleOps GPUMem
simpleGPUMem =
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
