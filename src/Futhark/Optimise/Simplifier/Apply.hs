-- | This module implements compile-time evaluation of function
-- application.  This cannot currently be done as a simplification
-- rule, as it requires access to the function table.  It is put in a
-- separate module, in order to not clutter the main simplification
-- engine.
module Futhark.Optimise.Simplifier.Apply
  ( simplifyApply
  )
  where

import Futhark.Representation.AST.Lore (Lore)
import Futhark.Representation.AST
import qualified Futhark.Analysis.SymbolTable as ST

simplifyApply :: Lore lore =>
                 Prog lore -> ST.SymbolTable anylore -> Name -> [(SubExp,Diet)]
              -> Maybe [Value]
simplifyApply _ _ _ _ =
  fail "not now"
