{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.IR.SeqMem
  ( SeqMem

  -- * Simplification
  , simplifyProg
  , simpleSeqMem

    -- * Module re-exports
  , module Futhark.IR.Mem
  , module Futhark.IR.Kernels.Kernel
  )
  where

import Futhark.Analysis.PrimExp.Convert
import Futhark.Pass
import Futhark.IR.Syntax
import Futhark.IR.Prop
import Futhark.IR.Traversals
import Futhark.IR.Pretty
import Futhark.IR.Kernels.Kernel
import qualified Futhark.TypeCheck as TC
import Futhark.IR.Mem
import Futhark.IR.Mem.Simplify
import Futhark.Pass.ExplicitAllocations (BinderOps(..), mkLetNamesB', mkLetNamesB'')
import qualified Futhark.Optimise.Simplify.Engine as Engine

data SeqMem

instance Decorations SeqMem where
  type LetDec     SeqMem = LetDecMem
  type FParamInfo SeqMem = FParamMem
  type LParamInfo SeqMem = LParamMem
  type RetType    SeqMem = RetTypeMem
  type BranchType SeqMem = BranchTypeMem
  type Op         SeqMem = MemOp ()

instance ASTLore SeqMem where
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
  checkRetType = mapM_ (TC.checkExtType . declExtTypeOf)
  primFParam name t = return $ Param name (MemPrim t)
  matchPattern = matchPatternToExp
  matchReturnType = matchFunctionReturnType
  matchBranchType = matchBranchReturnType
  matchLoopResult = matchLoopResultMem

instance BinderOps SeqMem where
  mkExpDecB _ _ = return ()
  mkBodyB stms res = return $ Body () stms res
  mkLetNamesB = mkLetNamesB' ()

instance BinderOps (Engine.Wise SeqMem) where
  mkExpDecB pat e = return $ Engine.mkWiseExpDec pat () e
  mkBodyB stms res = return $ Engine.mkWiseBody () stms res
  mkLetNamesB = mkLetNamesB''

simplifyProg :: Prog SeqMem -> PassM (Prog SeqMem)
simplifyProg =
  simplifyProgGeneric $ const $ return ((), mempty)

simpleSeqMem :: Engine.SimpleOps SeqMem
simpleSeqMem =
  simpleGeneric $ const $ return ((), mempty)
