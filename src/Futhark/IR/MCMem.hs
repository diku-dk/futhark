{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.IR.MCMem
  ( MCMem

  -- * Simplification
  , simplifyProg

    -- * Module re-exports
  , module Futhark.IR.Mem
  , module Futhark.IR.SegOp
  )
  where

import Futhark.Analysis.PrimExp.Convert
import Futhark.Pass
import Futhark.IR.Syntax
import Futhark.IR.Prop
import Futhark.IR.Traversals
import Futhark.IR.Pretty
import Futhark.IR.SegOp
import qualified Futhark.TypeCheck as TC
import Futhark.IR.Mem
import Futhark.IR.Mem.Simplify
import Futhark.Pass.ExplicitAllocations (BinderOps(..), mkLetNamesB', mkLetNamesB'')
import qualified Futhark.Optimise.Simplify.Engine as Engine

data MCMem

instance Decorations MCMem where
  type LetDec      MCMem = LetDecMem
  type FParamInfo  MCMem = FParamMem
  type LParamInfo  MCMem = LParamMem
  type RetType     MCMem = RetTypeMem
  type BranchType  MCMem = BranchTypeMem
  type Op          MCMem = MemOp (SegOp () MCMem)

instance ASTLore MCMem where
  expTypesFromPattern = return . map snd . snd . bodyReturnsFromPattern

instance OpReturns MCMem where
  opReturns (Alloc _ space) = return [MemMem space]
  opReturns (Inner op) = segOpReturns op

instance PrettyLore MCMem where

instance TC.CheckableOp MCMem where
  checkOp = typeCheckMemoryOp
    where typeCheckMemoryOp (Alloc size _) =
            TC.require [Prim int64] size
          typeCheckMemoryOp (Inner op) =
            typeCheckSegOp pure op

instance TC.Checkable MCMem where
  checkFParamLore = checkMemInfo
  checkLParamLore = checkMemInfo
  checkLetBoundLore = checkMemInfo
  checkRetType = mapM_ TC.checkExtType . retTypeValues
  primFParam name t = return $ Param name (MemPrim t)
  matchPattern = matchPatternToExp
  matchReturnType = matchFunctionReturnType
  matchBranchType = matchBranchReturnType

instance BinderOps MCMem where
  mkExpDecB _ _ = return ()
  mkBodyB stms res = return $ Body () stms res
  mkLetNamesB = mkLetNamesB' ()

instance BinderOps (Engine.Wise MCMem) where
  mkExpDecB pat e = return $ Engine.mkWiseExpDec pat () e
  mkBodyB stms res = return $ Engine.mkWiseBody () stms res
  mkLetNamesB = mkLetNamesB''

simplifyProg :: Prog MCMem -> PassM (Prog MCMem)
simplifyProg = simplifyProgGeneric simplifySegOp
