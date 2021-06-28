{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Futhark.IR.Mem
import Futhark.IR.Mem.Simplify
import qualified Futhark.Optimise.Simplify.Engine as Engine
import Futhark.Pass
import Futhark.Pass.ExplicitAllocations (BinderOps (..), mkLetNamesB', mkLetNamesB'')
import qualified Futhark.TypeCheck as TC

data SeqMem

instance RepTypes SeqMem where
  type LetDec SeqMem = LetDecMem
  type FParamInfo SeqMem = FParamMem
  type LParamInfo SeqMem = LParamMem
  type RetType SeqMem = RetTypeMem
  type BranchType SeqMem = BranchTypeMem
  type Op SeqMem = MemOp ()

instance ASTRep SeqMem where
  expTypesFromPattern = return . map snd . snd . bodyReturnsFromPattern

instance OpReturns SeqMem where
  opReturns (Alloc _ space) = return [MemMem space]
  opReturns (Inner ()) = pure []

instance PrettyRep SeqMem

instance TC.CheckableOp SeqMem where
  checkOp (Alloc size _) =
    TC.require [Prim int64] size
  checkOp (Inner ()) =
    pure ()

instance TC.Checkable SeqMem where
  checkFParamDec = checkMemInfo
  checkLParamDec = checkMemInfo
  checkLetBoundDec = checkMemInfo
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
simplifyProg = simplifyProgGeneric simpleSeqMem

simpleSeqMem :: Engine.SimpleOps SeqMem
simpleSeqMem =
  simpleGeneric (const mempty) $ const $ return ((), mempty)
