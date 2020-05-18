{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
-- | A simple representation with SOACs and nested parallelism.
module Futhark.Representation.SOACS
       ( -- * The Lore definition
         SOACS
         -- * Syntax types
       , Body
       , Stm
       , Pattern
       , Exp
       , Lambda
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
       , AST.BodyT(Body)
       , AST.PatternT(Pattern)
       , AST.PatElemT(PatElem)
       )
where

import qualified Futhark.Representation.AST.Syntax as AST
import Futhark.Representation.AST.Syntax
  hiding (Exp, Body, Stm,
          Pattern, Lambda, FParam, LParam, RetType, PatElem)
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

instance Decorations SOACS where
  type Op SOACS = SOAC SOACS

instance Attributes SOACS where
  expTypesFromPattern = return . expExtTypesFromPattern

type Exp = AST.Exp SOACS
type Body = AST.Body SOACS
type Stm = AST.Stm SOACS
type Pattern = AST.Pattern SOACS
type Lambda = AST.Lambda SOACS
type FParam = AST.FParam SOACS
type LParam = AST.LParam SOACS
type RetType = AST.RetType SOACS
type PatElem = AST.PatElem SOACS

instance TypeCheck.CheckableOp SOACS where
  checkOp = typeCheckSOAC

instance TypeCheck.Checkable SOACS where

instance Bindable SOACS where
  mkBody = AST.Body ()
  mkExpPat ctx val _ = basicPattern ctx val
  mkExpDec _ _ = ()
  mkLetNames = simpleMkLetNames

instance BinderOps SOACS where
  mkExpDecB = bindableMkExpDecB
  mkBodyB = bindableMkBodyB
  mkLetNamesB = bindableMkLetNamesB

instance PrettyLore SOACS where
