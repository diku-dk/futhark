{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
module Futhark.Representation.AST.Lore
       ( Lore(..)
       , ResType (..)
       , ResTypeT (..)
       )
       where

import Data.Monoid

import Futhark.Representation.AST.Syntax.Core
import Futhark.Representation.AST.ResType
import Futhark.Representation.AST.Attributes.Types

class (Show (LetBound l), Show (Exp l), Show (Body l), Show (FParam l),
       Eq (LetBound l), Eq (Exp l), Eq (Body l), Eq (FParam l),
       Ord (LetBound l), Ord (Exp l), Ord (Body l), Ord (FParam l),
       ResType (ResTypeT (ResTypeAttr l)))
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

  -- | A constant used to disambiguate method calls.  XXX, this is a
  -- hack to get around mising type application in Haskell, sometimes
  -- resulting in ambiguous types.
  representative :: l

  -- | A loop returns not only the values indicated in the result list
  -- @res@, but may also have an existential context.  Thus,
  -- @loopResult res merge@ returns those variables in @merge@ that
  -- constitute the context.
  loopResultContext :: l -> [Ident] -> [Bindee (FParam l)] -> [Ident]

  loopResType :: l -> [Ident] -> [Bindee (FParam l)] -> ResTypeT (ResTypeAttr l)

-- | A simple 'ResType' structure that is just a list of pairs of
-- shape-existential types and annotations.
newtype ResTypeT annot =
  ResType { resTypeElems :: [(ExtType, annot)] }
  deriving (Eq, Ord, Show)

instance Monoid (ResTypeT ()) where
  mempty = ResType mempty
  ResType xs `mappend` ResType ys =
    ResType $ xs <> ys

instance ResType (ResTypeT ()) where
  simpleType = mapM hasStaticShape . resTypeValues
  extResType ts = ResType [ (t, ()) | t <- ts]
  rt1 `generaliseResTypes` rt2 =
    extResType $ resTypeValues rt1 `generaliseExtTypes` resTypeValues rt2
  staticResType = extResType . staticShapes
  resTypeValues (ResType ts) = map fst ts
  bodyResType inaccessible ts =
    extResType $ existentialiseExtTypes inaccessible $ resTypeValues ts
