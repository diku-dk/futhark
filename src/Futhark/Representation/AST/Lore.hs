{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Futhark.Representation.AST.Lore
       ( Binding

       , Proper
       )
       where

type family Binding l :: *

class (Show (Binding l),
       Eq (Binding l),
       Ord (Binding l)) => Proper l where
