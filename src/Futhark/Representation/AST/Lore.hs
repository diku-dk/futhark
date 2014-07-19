{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Futhark.Representation.AST.Lore
       ( Dimension
       , Binding

       , Proper
       )
       where

type family Dimension l :: *
type family Binding l :: *

class (Show (Dimension l), Show (Binding l),
       Eq (Dimension l), Eq (Binding l),
       Ord (Binding l)) => Proper l where
