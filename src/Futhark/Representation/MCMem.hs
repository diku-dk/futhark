{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.Representation.MCMem
  ( MCMem

  -- * Simplification
  , simplifyProg

    -- * Module re-exports
  , module Futhark.Representation.Mem
  , module Futhark.Representation.SegOp
  )
  where

import Futhark.Analysis.PrimExp.Convert
import Futhark.Pass
import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
import Futhark.Representation.SegOp
import qualified Futhark.TypeCheck as TC
import Futhark.Representation.Mem
import Futhark.Representation.Mem.Simplify
import Futhark.Pass.ExplicitAllocations (BinderOps(..), mkLetNamesB', mkLetNamesB'')
import qualified Futhark.Optimise.Simplify.Engine as Engine

data MCMem

instance Annotations MCMem where
  type LetAttr    MCMem = LetAttrMem
  type FParamAttr MCMem = FParamMem
  type LParamAttr MCMem = LParamMem
  type RetType    MCMem = RetTypeMem
  type BranchType MCMem = BranchTypeMem
  type Op         MCMem = MemOp (SegOp () MCMem)

instance Attributes MCMem where
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
  mkExpAttrB _ _ = return ()
  mkBodyB stms res = return $ Body () stms res
  mkLetNamesB = mkLetNamesB' ()

instance BinderOps (Engine.Wise MCMem) where
  mkExpAttrB pat e = return $ Engine.mkWiseExpAttr pat () e
  mkBodyB stms res = return $ Engine.mkWiseBody () stms res
  mkLetNamesB = mkLetNamesB''

simplifyProg :: Prog MCMem -> PassM (Prog MCMem)
simplifyProg = simplifyProgGeneric simplifySegOp
