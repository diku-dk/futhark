{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | A simple representation with SOACs and nested parallelism.
module Futhark.IR.SOACS
  ( SOACS,

    -- * Syntax types
    Body,
    Stm,
    Pat,
    Exp,
    Lambda,
    FParam,
    LParam,
    RetType,
    PatElem,

    -- * Module re-exports
    module Futhark.IR.Prop,
    module Futhark.IR.Traversals,
    module Futhark.IR.Pretty,
    module Futhark.IR.Syntax,
    module Futhark.IR.SOACS.SOAC,
    AST.LambdaT (Lambda),
    AST.BodyT (Body),
    AST.PatT (Pat),
    AST.PatElemT (PatElem),
  )
where

import Futhark.Builder
import Futhark.Construct
import Futhark.IR.Pretty
import Futhark.IR.Prop
import Futhark.IR.SOACS.SOAC
import Futhark.IR.Syntax hiding
  ( Body,
    Exp,
    FParam,
    LParam,
    Lambda,
    Pat,
    PatElem,
    RetType,
    Stm,
  )
import qualified Futhark.IR.Syntax as AST
import Futhark.IR.Traversals
import qualified Futhark.IR.TypeCheck as TC

-- This module could be written much nicer if Haskell had functors
-- like Standard ML.  Instead, we have to abuse the namespace/module
-- system.

-- | The rep for the basic representation.
data SOACS

instance RepTypes SOACS where
  type Op SOACS = SOAC SOACS

instance ASTRep SOACS where
  expTypesFromPat = return . expExtTypesFromPat

type Exp = AST.Exp SOACS

type Body = AST.Body SOACS

type Stm = AST.Stm SOACS

type Pat = AST.Pat SOACS

type Lambda = AST.Lambda SOACS

type FParam = AST.FParam SOACS

type LParam = AST.LParam SOACS

type RetType = AST.RetType SOACS

type PatElem = AST.PatElem SOACS

instance TC.CheckableOp SOACS where
  checkOp = typeCheckSOAC

instance TC.Checkable SOACS

instance Buildable SOACS where
  mkBody = AST.Body ()
  mkExpPat merge _ = basicPat merge
  mkExpDec _ _ = ()
  mkLetNames = simpleMkLetNames

instance BuilderOps SOACS

instance PrettyRep SOACS
