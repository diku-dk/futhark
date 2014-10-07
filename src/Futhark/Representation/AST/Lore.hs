{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Futhark.Representation.AST.Lore
       ( Lore(..) )
       where

class (Show (Binding l), Show (Exp l), Show (Body l), Show (FParam l),
       Eq (Binding l), Eq (Exp l), Eq (Body l), Eq (FParam l),
       Ord (Binding l), Ord (Exp l), Ord (Body l), Ord (FParam l))
      => Lore l where
  -- | Annotation for every binding.
  type Binding l :: *
  type Binding l = ()
  -- | Annotation for every expression.
  type Exp l :: *
  type Exp l = ()
  -- | Annotation for every body.
  type Body l :: *
  type Body l = ()
  -- | Annotation for ever (non-lambda) function parameter.
  type FParam l :: *
  type FParam l = ()
