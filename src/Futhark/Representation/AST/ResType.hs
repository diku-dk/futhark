{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
module Futhark.Representation.AST.ResType
       (
         IsResType (..)
       )
       where

import Futhark.Representation.AST.Syntax.Core
import Futhark.Representation.AST.Attributes.Types

-- | A type denoting the return type of an expression or body.  If a
-- function returns an array, we can either know the size in advance
-- ('known'), or receive it as part of the return value ('existential'
-- - refers to the type being dependent).
class (Show rt, Eq rt, Ord rt) => IsResType rt where
  -- | If the result type has no context, return its corresponding
  -- simple type.
  simpleType :: rt -> Maybe [Type]

  basicResType :: BasicType -> rt

  resTypeValues :: rt -> [ExtType]

instance IsResType [ExtType] where
  simpleType = mapM hasStaticShape
  basicResType = staticShapes . return . Basic
  resTypeValues = id
