{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Futhark.Representation.AST.Annotations
       ( Annotations (..)
       )
       where

import Futhark.Representation.AST.Syntax.Core
import Futhark.Representation.AST.RetType
import Futhark.Representation.AST.Attributes.Types

class (Show (LetBound l), Show (Exp l), Show (Body l), Show (FParam l), Show (LParam l), Show (RetType l),
       Eq (LetBound l), Eq (Exp l), Eq (Body l), Eq (FParam l), Eq (LParam l), Eq (RetType l),
       Ord (LetBound l), Ord (Exp l), Ord (Body l), Ord (FParam l), Ord (LParam l), Ord (RetType l),
       IsRetType (RetType l),
       Typed (FParam l), Typed (LParam l), Typed (LetBound l),
       DeclTyped (FParam l))
      => Annotations l where
  -- | Annotation for every binding.
  type LetBound l :: *
  type LetBound l = Type
  -- | Annotation for every expression.
  type Exp l :: *
  type Exp l = ()
  -- | Annotation for every body.
  type Body l :: *
  type Body l = ()
  -- | Annotation for every (non-lambda) function parameter.
  type FParam l :: *
  type FParam l = DeclType
  -- | Annotation for every lambda function parameter.
  type LParam l :: *
  type LParam l = Type
  -- | The type of expressions and function calls.
  type RetType l :: *
  type RetType l = ExtRetType
