{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
-- | This module exports a type class covering representations of
-- function return types.
module Futhark.Representation.AST.RetType
       (
         IsRetType (..)
       )
       where

import Futhark.Representation.AST.Syntax.Core
import Futhark.Representation.AST.Attributes.Types

-- | A type representing the return type of a function.  It should
-- contain at least the information contained in a list of 'ExtType's,
-- but may have more, notably an existential context.
class (Show rt, Eq rt, Ord rt) => IsRetType rt where
  -- | Contruct a return type from a basic (scalar) type.
  basicRetType :: BasicType -> rt

  -- | Extract the simple type from the return type - although this
  -- may still involve an existential shape context.
  resTypeValues :: rt -> [ExtType]

instance IsRetType [ExtType] where
  basicRetType = staticShapes . return . Basic
  resTypeValues = id
