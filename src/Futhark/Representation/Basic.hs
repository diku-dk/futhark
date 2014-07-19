{-# LANGUAGE TypeFamilies #-}
-- | A simple representation with known shapes, but no other
-- particular information.
module Futhark.Representation.Basic
       ( -- * The Lore definition
         Basic
         -- * Syntax types
       , Prog
       , Body
       , Binding
       , Exp
       , SubExp
       , Lambda
       , Ident
       , RetType
       , ResType
       , ConstType
       , Type
       , Shape
       , ExtShape
       , FunDec
       , Param
       , Certificates
       , ExtDimSize
       , Result
         -- * Module re-exports
       , module Futhark.Representation.AST.Attributes
       , module Futhark.Representation.AST.Traversals
       , module Futhark.Representation.AST.Pretty
       , module Futhark.Representation.AST.Syntax
       , AST.LambdaT(Lambda)
       , AST.IdentBase(Ident)
       , AST.BodyT(Body)
       , AST.ProgT(Prog)
       , AST.ExpT(SubExp)
       , AST.ShapeT(Shape)
       , AST.ExtShapeT(ExtShape)
       , AST.ResultT(Result)
       )
where

import Data.Monoid

import qualified Futhark.Representation.AST.Lore as Lore
import qualified Futhark.Representation.AST.Syntax as AST
import Futhark.Representation.AST.Syntax
  hiding (Prog, Exp, SubExp, Body, Binding, Ident, Lambda,
          Type, RetType, ResType, ConstType, ExtDimSize,
          Shape, ExtShape, FunDec, Param, Certificates, Result)
import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
import Futhark.Renamer
import qualified Futhark.TypeCheck as TypeCheck

-- This module could be written much nicer if Haskell had functors
-- like Standard ML.  Instead, we have to abuse the namespace/module
-- system.

-- | The lore for the basic representation.
data Basic

type instance Lore.Binding Basic = ()

instance Lore.Proper Basic where

type Prog = AST.Prog Basic
type Exp = AST.Exp Basic
type SubExp = AST.SubExpT Basic
type Body = AST.Body Basic
type Binding = AST.Binding Basic
type Ident = AST.Ident Basic
type Lambda = AST.Lambda Basic
type RetType = AST.RetType Basic
type ResType = AST.ResType Basic
type ConstType = AST.ConstType Basic
type Type = AST.Type Basic
type Shape = AST.Shape Basic
type ExtShape = AST.ExtShape Basic
type ExtDimSize = AST.ExtDimSize Basic
type FunDec = AST.FunDec Basic
type Param = AST.Param Basic
type Certificates = AST.Certificates Basic
type Result = AST.Result Basic

instance Renameable Basic where

instance FreeIn Basic where
  freeInBindingLore = mempty

instance TypeCheck.Checkable Basic where
  checkBindingLore = return
