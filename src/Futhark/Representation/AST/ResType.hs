module Futhark.Representation.AST.ResType
       ( ResType (..)
       )
       where

import Data.Monoid

import Futhark.Representation.AST.Syntax.Core

-- | A type denoting the return type of an expression or function
-- call.  If a function returns an array, we can either know the size
-- in advance ('known'), or receive it as part of the return value
-- ('existential' - refers to the type being dependent).
class (Show rt, Eq rt, Ord rt, Monoid rt) => ResType rt where
  -- | If the result type has no context, return its corresponding
  -- simple type.
  simpleType :: rt -> Maybe [Type]

  generaliseResTypes :: rt -> rt -> rt

  -- | Create a ResType from a list of types with existential shape.
  extResType :: [ExtType] -> rt

  -- | Create a ResType from a list of types with known shape.
  staticResType :: [Type] -> rt

  -- | Return the non-context types in the 'ResType'.
  resTypeValues :: rt -> [ExtType]

  existentialiseType :: Names -> rt -> rt
