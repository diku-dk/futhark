{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
-- | A representation with flat parallelism via GPU-oriented kernels.
module Futhark.Representation.Kernels
       ( -- * The Lore definition
         Kernels
         -- * Syntax types
       , Prog
       , Body
       , Binding
       , Pattern
       , PrimOp
       , Exp
       , Lambda
       , ExtLambda
       , FunDef
       , FParam
       , LParam
       , RetType
       , PatElem
         -- * Module re-exports
       , module Futhark.Representation.AST.Attributes
       , module Futhark.Representation.AST.Traversals
       , module Futhark.Representation.AST.Pretty
       , module Futhark.Representation.AST.Syntax
       , module Futhark.Representation.Kernels.Kernel
       , AST.LambdaT(Lambda)
       , AST.ExtLambdaT(ExtLambda)
       , AST.BodyT(Body)
       , AST.PatternT(Pattern)
       , AST.PatElemT(PatElem)
       , AST.ProgT(Prog)
       , AST.ExpT(PrimOp)
       , AST.FunDefT(FunDef)
       , AST.ParamT(Param)
         -- Removing lore
       , removeProgLore
       , removeFunDefLore
       , removeBodyLore
       )
where

import Control.Monad

import qualified Futhark.Representation.AST.Syntax as AST
import Futhark.Representation.AST.Syntax
  hiding (Prog, PrimOp, Exp, Body, Binding,
          Pattern, Lambda, ExtLambda, FunDef, FParam, LParam,
          RetType, PatElem)
import Futhark.Representation.Kernels.Kernel
import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
import Futhark.Binder
import Futhark.Construct
import qualified Futhark.TypeCheck as TypeCheck
import Futhark.Analysis.Rephrase

-- This module could be written much nicer if Haskell had functors
-- like Standard ML.  Instead, we have to abuse the namespace/module
-- system.

-- | The lore for the basic representation.
data Kernels

instance Annotations Kernels where
  type Op Kernels = Kernel Kernels

instance Attributes Kernels where

type Prog = AST.Prog Kernels
type PrimOp = AST.PrimOp Kernels
type Exp = AST.Exp Kernels
type Body = AST.Body Kernels
type Binding = AST.Binding Kernels
type Pattern = AST.Pattern Kernels
type Lambda = AST.Lambda Kernels
type ExtLambda = AST.ExtLambda Kernels
type FunDef = AST.FunDefT Kernels
type FParam = AST.FParam Kernels
type LParam = AST.LParam Kernels
type RetType = AST.RetType Kernels
type PatElem = AST.PatElem Type

instance TypeCheck.Checkable Kernels where
  checkExpLore = return
  checkBodyLore = return
  checkFParamLore _ = TypeCheck.checkType
  checkLParamLore _ = TypeCheck.checkType
  checkLetBoundLore _ = TypeCheck.checkType
  checkRetType = mapM_ TypeCheck.checkExtType . retTypeValues
  checkOp = typeCheckKernel
  matchPattern pat e = do
    et <- expExtType e
    TypeCheck.matchExtPattern (patternElements pat) et
  primFParam name t =
    return $ AST.Param name (AST.Prim t)
  primLParam name t =
    return $ AST.Param name (AST.Prim t)
  matchReturnType name (ExtRetType ts) =
    TypeCheck.matchExtReturnType name $ map fromDecl ts

instance Bindable Kernels where
  mkBody = AST.Body ()
  mkLet context values =
    AST.Let (basicPattern context values) ()
  mkLetNames names e = do
    et <- expExtType e
    (ts, shapes) <- instantiateShapes' et
    let shapeElems = [ AST.PatElem shape BindVar shapet
                     | Ident shape shapet <- shapes
                     ]
        mkValElem (name, BindVar) t =
          return $ AST.PatElem name BindVar t
        mkValElem (name, bindage@(BindInPlace _ src _)) _ = do
          srct <- lookupType src
          return $ AST.PatElem name bindage srct
    valElems <- zipWithM mkValElem names ts
    return $ AST.Let (AST.Pattern shapeElems valElems) () e

instance PrettyLore Kernels where

removeLore :: (Attributes lore, Op lore ~ Op Kernels) => Rephraser lore Kernels
removeLore =
  Rephraser { rephraseExpLore = const ()
            , rephraseLetBoundLore = typeOf
            , rephraseBodyLore = const ()
            , rephraseFParamLore = declTypeOf
            , rephraseLParamLore = typeOf
            , rephraseRetType = removeRetTypeLore
            , rephraseOp = id
            }

removeProgLore :: (Attributes lore, Op lore ~ Op Kernels) => AST.Prog lore -> Prog
removeProgLore = rephraseProg removeLore

removeFunDefLore :: (Attributes lore, Op lore ~ Op Kernels) => AST.FunDef lore -> FunDef
removeFunDefLore = rephraseFunDef removeLore

removeBodyLore :: (Attributes lore, Op lore ~ Op Kernels) => AST.Body lore -> Body
removeBodyLore = rephraseBody removeLore

removeRetTypeLore :: IsRetType rt => rt -> RetType
removeRetTypeLore = ExtRetType . retTypeValues
