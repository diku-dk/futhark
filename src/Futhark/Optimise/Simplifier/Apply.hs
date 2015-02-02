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
import qualified Futhark.Interpreter as Interp
import qualified Futhark.Analysis.SymbolTable as ST

simplifyApply :: Lore lore =>
                 Prog lore -> ST.SymbolTable anylore -> Name -> [(SubExp,Diet)]
              -> Maybe [Value]
simplifyApply program vtable fname args = do
  vals <- allArgsAreValues $ map fst args
  either (const Nothing) Just $ Interp.runFunNoTrace fname vals program
  where allArgsAreValues = mapM argIsValue

        argIsValue (Constant val) = Just $ BasicVal val
        argIsValue (Var v)        = ST.lookupValue (identName v) vtable
