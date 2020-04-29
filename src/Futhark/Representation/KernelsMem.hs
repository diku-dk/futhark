{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.Representation.KernelsMem
  ( KernelsMem

  -- * Simplification
  , simplifyProg
  , simplifyStms
  , simpleKernelsMem

    -- * Module re-exports
  , module Futhark.Representation.Mem
  , module Futhark.Representation.Kernels.Kernel
  )
  where

import Futhark.Analysis.PrimExp.Convert
import Futhark.MonadFreshNames
import Futhark.Pass
import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
import Futhark.Representation.Kernels.Kernel
import Futhark.Representation.Kernels.Simplify (simplifyKernelOp)
import qualified Futhark.TypeCheck as TC
import Futhark.Representation.Mem
import Futhark.Representation.Mem.Simplify
import Futhark.Pass.ExplicitAllocations (BinderOps(..), mkLetNamesB', mkLetNamesB'')
import qualified Futhark.Optimise.Simplify.Engine as Engine

data KernelsMem

instance Annotations KernelsMem where
  type LetAttr    KernelsMem = LetAttrMem
  type FParamAttr KernelsMem = FParamMem
  type LParamAttr KernelsMem = LParamMem
  type RetType    KernelsMem = RetTypeMem
  type BranchType KernelsMem = BranchTypeMem
  type Op         KernelsMem = MemOp (HostOp KernelsMem ())

instance Attributes KernelsMem where
  expTypesFromPattern = return . map snd . snd . bodyReturnsFromPattern

instance OpReturns KernelsMem where
  opReturns (Alloc _ space) =
    return [MemMem space]
  opReturns (Inner (SegOp op)) = segOpReturns op
  opReturns k = extReturns <$> opType k

instance PrettyLore KernelsMem where

instance TC.CheckableOp KernelsMem where
  checkOp = typeCheckMemoryOp Nothing
    where typeCheckMemoryOp _ (Alloc size _) =
            TC.require [Prim int64] size
          typeCheckMemoryOp lvl (Inner op) =
            typeCheckHostOp (typeCheckMemoryOp . Just) lvl (const $ return ()) op

instance TC.Checkable KernelsMem where
  checkFParamLore = checkMemInfo
  checkLParamLore = checkMemInfo
  checkLetBoundLore = checkMemInfo
  checkRetType = mapM_ TC.checkExtType . retTypeValues
  primFParam name t = return $ Param name (MemPrim t)
  matchPattern = matchPatternToExp
  matchReturnType = matchFunctionReturnType
  matchBranchType = matchBranchReturnType

instance BinderOps KernelsMem where
  mkExpAttrB _ _ = return ()
  mkBodyB stms res = return $ Body () stms res
  mkLetNamesB = mkLetNamesB' ()

instance BinderOps (Engine.Wise KernelsMem) where
  mkExpAttrB pat e = return $ Engine.mkWiseExpAttr pat () e
  mkBodyB stms res = return $ Engine.mkWiseBody () stms res
  mkLetNamesB = mkLetNamesB''

simplifyProg :: Prog KernelsMem -> PassM (Prog KernelsMem)
simplifyProg =
  simplifyProgGeneric $ simplifyKernelOp $ const $ return ((), mempty)

simplifyStms :: (HasScope KernelsMem m, MonadFreshNames m) =>
                 Stms KernelsMem
             -> m (Engine.SymbolTable (Engine.Wise KernelsMem),
                   Stms KernelsMem)
simplifyStms =
  simplifyStmsGeneric $ simplifyKernelOp $ const $ return ((), mempty)

simpleKernelsMem :: Engine.SimpleOps KernelsMem
simpleKernelsMem =
  simpleGeneric $ simplifyKernelOp $ const $ return ((), mempty)
