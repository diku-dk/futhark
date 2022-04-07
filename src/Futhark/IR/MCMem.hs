{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import qualified Futhark.IR.TypeCheck as TC
import qualified Futhark.Optimise.Simplify.Engine as Engine
import Futhark.Pass
import Futhark.Pass.ExplicitAllocations (BuilderOps (..), mkLetNamesB', mkLetNamesB'')

data MCMem

instance RepTypes MCMem where
  type LetDec MCMem = LetDecMem
  type FParamInfo MCMem = FParamMem
  type LParamInfo MCMem = LParamMem
  type RetType MCMem = RetTypeMem
  type BranchType MCMem = BranchTypeMem
  type Op MCMem = MemOp (MCOp MCMem ())

instance ASTRep MCMem where
  expTypesFromPat = pure . map snd . bodyReturnsFromPat

instance OpReturns (MCOp MCMem ()) where
  opReturns (ParOp _ op) = segOpReturns op
  opReturns (OtherOp ()) = pure []

instance OpReturns (MCOp (Engine.Wise MCMem) ()) where
  opReturns (ParOp _ op) = segOpReturns op
  opReturns k = extReturns <$> opType k

instance PrettyRep MCMem

instance TC.CheckableOp MCMem where
  checkOp = typeCheckMemoryOp
    where
      typeCheckMemoryOp (Alloc size _) =
        TC.require [Prim int64] size
      typeCheckMemoryOp (Inner op) =
        typeCheckMCOp pure op

instance TC.Checkable MCMem where
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
  mkLetNamesB = mkLetNamesB' ()

instance BuilderOps (Engine.Wise MCMem) where
  mkExpDecB pat e = pure $ Engine.mkWiseExpDec pat () e
  mkBodyB stms res = pure $ Engine.mkWiseBody () stms res
  mkLetNamesB = mkLetNamesB''

instance TraverseOpStms (Engine.Wise MCMem) where
  traverseOpStms = traverseMemOpStms (traverseMCOpStms (const pure))

simplifyProg :: Prog MCMem -> PassM (Prog MCMem)
simplifyProg = simplifyProgGeneric simpleMCMem

simpleMCMem :: Engine.SimpleOps MCMem
simpleMCMem =
  simpleGeneric (const mempty) $ simplifyMCOp $ const $ pure ((), mempty)
