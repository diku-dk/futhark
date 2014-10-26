{-# LANGUAGE TypeFamilies, FlexibleContexts, StandaloneDeriving #-}
module Futhark.Representation.AST.Lore
       ( Lore(..)
       , ResTypeT (..)
       , ResType
       , resTypeValues
       )
       where

import Data.Monoid

import Futhark.Representation.AST.Syntax.Core

class (Show (ResTypeElem l), Show (LetBound l), Show (Exp l), Show (Body l), Show (FParam l),
       Eq (ResTypeElem l), Eq (LetBound l), Eq (Exp l), Eq (Body l), Eq (FParam l),
       Ord (ResTypeElem l), Ord (LetBound l), Ord (Exp l), Ord (Body l), Ord (FParam l))
      => Lore l where
  -- | Annotation for every binding.
  type LetBound l :: *
  type LetBound l = ()
  -- | Annotation for every expression.
  type Exp l :: *
  type Exp l = ()
  -- | Annotation for every body.
  type Body l :: *
  type Body l = ()
  -- | Annotation for ever (non-lambda) function parameter.
  type FParam l :: *
  type FParam l = ()
  -- | Annotation for 'ResType' elements.
  type ResTypeElem l :: *
  type ResTypeElem l = ()

  -- | Extract context (including the shape context).
  resTypeContext :: ResType l -> [Type]

  generaliseResTypes :: ResType l -> ResType l -> ResType l

  -- | Create a ResType from a list of types with existential shape.
  extResType :: [ExtType] -> ResType l

  doLoopResType :: [Ident] -> [Ident] -> ResType l

-- | A type denoting the return type of an expression or function
-- call.  If a function returns an array, we can either know the size
-- in advance ('known'), or receive it as part of the return value
-- ('existential' - refers to the type being dependent).
newtype ResTypeT lore =
  ResType { resTypeElems :: [(ExtType, ResTypeElem lore)] }

deriving instance Lore lore => Ord (ResTypeT lore)
deriving instance Lore lore => Show (ResTypeT lore)
deriving instance Lore lore => Eq (ResTypeT lore)

instance Monoid (ResTypeT lore) where
  ResType xs `mappend` ResType ys = ResType $ xs ++ ys
  mempty = ResType []

-- | Type alias for namespace control.
type ResType = ResTypeT

-- | Return the non-context types in the 'ResType'.
resTypeValues :: ResType l -> [ExtType]
resTypeValues (ResType ts) = map fst ts
