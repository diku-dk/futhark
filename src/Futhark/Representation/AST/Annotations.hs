{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Futhark.Representation.AST.Annotations
       ( Annotations (..)
       , module Futhark.Representation.AST.RetType
       )
       where

import Futhark.Representation.AST.Syntax.Core
import Futhark.Representation.AST.RetType
import Futhark.Representation.AST.Attributes.Types

class (Show (LetAttr l), Show (ExpAttr l), Show (BodyAttr l), Show (FParamAttr l), Show (LParamAttr l), Show (RetType l), Show (BranchType l), Show (Op l),
       Eq (LetAttr l), Eq (ExpAttr l), Eq (BodyAttr l), Eq (FParamAttr l), Eq (LParamAttr l), Eq (RetType l), Eq (BranchType l), Eq (Op l),
       Ord (LetAttr l), Ord (ExpAttr l), Ord (BodyAttr l), Ord (FParamAttr l), Ord (LParamAttr l), Ord (RetType l), Ord (BranchType l), Ord (Op l),
       IsRetType (RetType l), IsBodyType (BranchType l),
       Typed (FParamAttr l), Typed (LParamAttr l), Typed (LetAttr l),
       DeclTyped (FParamAttr l))
      => Annotations l where
  -- | Annotation for every let-pattern element.
  type LetAttr l :: *
  type LetAttr l = Type
  -- | Annotation for every expression.
  type ExpAttr l :: *
  type ExpAttr l = ()
  -- | Annotation for every body.
  type BodyAttr l :: *
  type BodyAttr l = ()
  -- | Annotation for every (non-lambda) function parameter.
  type FParamAttr l :: *
  type FParamAttr l = DeclType
  -- | Annotation for every lambda function parameter.
  type LParamAttr l :: *
  type LParamAttr l = Type

  -- | The return type annotation of function calls.
  type RetType l :: *
  type RetType l = DeclExtType

  -- | The return type annotation of branches.
  type BranchType l :: *
  type BranchType l = ExtType

  -- | Extensible operation.
  type Op l :: *
  type Op l = ()
