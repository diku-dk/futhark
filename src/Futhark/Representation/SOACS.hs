{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
-- | A simple representation with SOACs and nested parallelism.
module Futhark.Representation.SOACS
       ( -- * The Lore definition
         SOACS
         -- * Syntax types
       , Prog
       , Body
       , Stm
       , Pattern
       , BasicOp
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
       , module Futhark.Representation.SOACS.SOAC
       , AST.LambdaT(Lambda)
       , AST.ExtLambdaT(ExtLambda)
       , AST.BodyT(Body)
       , AST.PatternT(Pattern)
       , AST.PatElemT(PatElem)
       , AST.ProgT(Prog)
       , AST.ExpT(BasicOp)
       , AST.FunDefT(FunDef)
       , AST.ParamT(Param)
       )
where

import Prelude

import qualified Futhark.Representation.AST.Syntax as AST
import Futhark.Representation.AST.Syntax
  hiding (Prog, BasicOp, Exp, Body, Stm,
          Pattern, Lambda, ExtLambda, FunDef, FParam, LParam,
          RetType, PatElem)
import Futhark.Representation.SOACS.SOAC
import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
import Futhark.Binder
import Futhark.Construct
import qualified Futhark.TypeCheck as TypeCheck

-- This module could be written much nicer if Haskell had functors
-- like Standard ML.  Instead, we have to abuse the namespace/module
-- system.

-- | The lore for the basic representation.
data SOACS

instance Annotations SOACS where
  type Op SOACS = SOAC SOACS

instance Attributes SOACS where

type Prog = AST.Prog SOACS
type BasicOp = AST.BasicOp SOACS
type Exp = AST.Exp SOACS
type Body = AST.Body SOACS
type Stm = AST.Stm SOACS
type Pattern = AST.Pattern SOACS
type Lambda = AST.Lambda SOACS
type ExtLambda = AST.ExtLambda SOACS
type FunDef = AST.FunDefT SOACS
type FParam = AST.FParam SOACS
type LParam = AST.LParam SOACS
type RetType = AST.RetType SOACS
type PatElem = AST.PatElem SOACS

instance TypeCheck.Checkable SOACS where
  checkExpLore = return
  checkBodyLore = return
  checkFParamLore _ = TypeCheck.checkType
  checkLParamLore _ = TypeCheck.checkType
  checkLetBoundLore _ = TypeCheck.checkType
  checkRetType = mapM_ TypeCheck.checkExtType . retTypeValues
  checkOp = typeCheckSOAC
  matchPattern pat e = do
    et <- expExtType e
    TypeCheck.matchExtPattern (patternElements pat) et
  primFParam name t =
    return $ AST.Param name (AST.Prim t)
  primLParam name t =
    return $ AST.Param name (AST.Prim t)
  matchReturnType name (ExtRetType ts) =
    TypeCheck.matchExtReturnType name $ map fromDecl ts

instance Bindable SOACS where
  mkBody = AST.Body ()
  mkExpPat ctx val _ = basicPattern ctx val
  mkExpAttr _ _ = ()
  mkLetNames = simpleMkLetNames

instance BinderOps SOACS where
  mkExpAttrB = bindableMkExpAttrB
  mkBodyB = bindableMkBodyB
  mkLetNamesB = bindableMkLetNamesB

instance PrettyLore SOACS where
