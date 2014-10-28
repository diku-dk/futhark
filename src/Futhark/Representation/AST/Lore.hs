{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Futhark.Representation.AST.Lore
       ( Lore(..)
       , ResType.ResType
       , ResTypeT (..)
       )
       where

import Futhark.Representation.AST.Syntax.Core
import qualified Futhark.Representation.AST.ResType as ResType

class (Show (LetBound l), Show (Exp l), Show (Body l), Show (FParam l),
       Eq (LetBound l), Eq (Exp l), Eq (Body l), Eq (FParam l),
       Ord (LetBound l), Ord (Exp l), Ord (Body l), Ord (FParam l),
       ResType.ResType (ResTypeT (ResTypeAttr l)))
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
  -- | The per-element attribute of result types.
  type ResTypeAttr l :: *
  type ResTypeAttr l = ()

-- | A simple 'ResType' structure that is just a list of pairs of
-- shape-existential types and annotations.
newtype ResTypeT annot =
  ResType { resTypeElems :: [(ExtType, annot)] }
  deriving (Eq, Ord, Show)
