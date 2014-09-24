{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Futhark.Representation.AST.Lore
       ( Lore(..) )
       where

class (Show (Binding l), Show (Exp l), Show (Body l),
       Eq (Binding l), Eq (Exp l), Eq (Body l),
       Ord (Binding l), Ord (Exp l), Ord (Body l)) => Lore l where
  -- | Annotation for every binding.
  type Binding l :: *
  type Binding l = ()
  -- | Annotation for every expression.
  type Exp l :: *
  type Exp l = ()
  -- | Annotation for every body.
  type Body l :: *
  type Body l = ()
