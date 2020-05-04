{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.Representation.SeqMem
  ( SeqMem

  -- * Simplification
  , simplifyProg
  , simplifyStms
  , simpleSeqMem

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
import qualified Futhark.TypeCheck as TC
import Futhark.Representation.Mem
import Futhark.Representation.Mem.Simplify
import Futhark.Pass.ExplicitAllocations (BinderOps(..), mkLetNamesB', mkLetNamesB'')
import qualified Futhark.Optimise.Simplify.Engine as Engine

data SeqMem

instance Annotations SeqMem where
  type LetAttr    SeqMem = LetAttrMem
  type FParamAttr SeqMem = FParamMem
  type LParamAttr SeqMem = LParamMem
  type RetType    SeqMem = RetTypeMem
  type BranchType SeqMem = BranchTypeMem
  type Op         SeqMem = MemOp ()

instance Attributes SeqMem where
  expTypesFromPattern = return . map snd . snd . bodyReturnsFromPattern

instance OpReturns SeqMem where
  opReturns (Alloc _ space) = return [MemMem space]
  opReturns (Inner ()) = pure []

instance PrettyLore SeqMem where

instance TC.CheckableOp SeqMem where
  checkOp (Alloc size _) =
    TC.require [Prim int64] size
  checkOp (Inner ()) =
    pure ()

instance TC.Checkable SeqMem where
  checkFParamLore = checkMemInfo
  checkLParamLore = checkMemInfo
  checkLetBoundLore = checkMemInfo
  checkRetType = mapM_ TC.checkExtType . retTypeValues
  primFParam name t = return $ Param name (MemPrim t)
  matchPattern = matchPatternToExp
  matchReturnType = matchFunctionReturnType
  matchBranchType = matchBranchReturnType

instance BinderOps SeqMem where
  mkExpAttrB _ _ = return ()
  mkBodyB stms res = return $ Body () stms res
  mkLetNamesB = mkLetNamesB' ()

instance BinderOps (Engine.Wise SeqMem) where
  mkExpAttrB pat e = return $ Engine.mkWiseExpAttr pat () e
  mkBodyB stms res = return $ Engine.mkWiseBody () stms res
  mkLetNamesB = mkLetNamesB''

simplifyProg :: Prog SeqMem -> PassM (Prog SeqMem)
simplifyProg =
  simplifyProgGeneric $ const $ return ((), mempty)

simplifyStms :: (HasScope SeqMem m, MonadFreshNames m) =>
                 Stms SeqMem
             -> m (Engine.SymbolTable (Engine.Wise SeqMem),
                   Stms SeqMem)
simplifyStms =
  simplifyStmsGeneric $ const $ return ((), mempty)

simpleSeqMem :: Engine.SimpleOps SeqMem
simpleSeqMem =
  simpleGeneric $ const $ return ((), mempty)
