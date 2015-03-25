module Futhark.Optimise.Simplifier.Simplifiable
       ( Simplifiable )
       where

import Futhark.Binder (Proper)
import Futhark.Representation.AST.Attributes.Ranges (Ranged)

class (Ranged lore, Proper lore) => Simplifiable lore where
