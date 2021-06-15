{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | The core Futhark AST is parameterised by a @rep@ type parameter,
-- which is then used to invoke the type families defined here.
module Futhark.IR.Rep
  ( RepTypes (..),
    module Futhark.IR.RetType,
  )
where

import qualified Data.Kind
import Futhark.IR.Prop.Types
import Futhark.IR.RetType
import Futhark.IR.Syntax.Core

-- | A collection of type families giving various common types for a
-- representation, along with constraints specifying that the types
-- they map to should satisfy some minimal requirements.
class
  ( Show (LetDec l),
    Show (ExpDec l),
    Show (BodyDec l),
    Show (FParamInfo l),
    Show (LParamInfo l),
    Show (RetType l),
    Show (BranchType l),
    Show (Op l),
    Eq (LetDec l),
    Eq (ExpDec l),
    Eq (BodyDec l),
    Eq (FParamInfo l),
    Eq (LParamInfo l),
    Eq (RetType l),
    Eq (BranchType l),
    Eq (Op l),
    Ord (LetDec l),
    Ord (ExpDec l),
    Ord (BodyDec l),
    Ord (FParamInfo l),
    Ord (LParamInfo l),
    Ord (RetType l),
    Ord (BranchType l),
    Ord (Op l),
    IsRetType (RetType l),
    IsBodyType (BranchType l),
    Typed (FParamInfo l),
    Typed (LParamInfo l),
    Typed (LetDec l),
    DeclTyped (FParamInfo l)
  ) =>
  RepTypes l
  where
  -- | Decoration for every let-pattern element.
  type LetDec l :: Data.Kind.Type

  type LetDec l = Type

  -- | Decoration for every expression.
  type ExpDec l :: Data.Kind.Type

  type ExpDec l = ()

  -- | Decoration for every body.
  type BodyDec l :: Data.Kind.Type

  type BodyDec l = ()

  -- | Decoration for every (non-lambda) function parameter.
  type FParamInfo l :: Data.Kind.Type

  type FParamInfo l = DeclType

  -- | Decoration for every lambda function parameter.
  type LParamInfo l :: Data.Kind.Type

  type LParamInfo l = Type

  -- | The return type decoration of function calls.
  type RetType l :: Data.Kind.Type

  type RetType l = DeclExtType

  -- | The return type decoration of branches.
  type BranchType l :: Data.Kind.Type

  type BranchType l = ExtType

  -- | Extensible operation.
  type Op l :: Data.Kind.Type

  type Op l = ()
