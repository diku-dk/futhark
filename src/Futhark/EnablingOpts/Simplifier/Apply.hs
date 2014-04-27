-- | This module implements compile-time evaluation of function
-- application.  This cannot currently be done as a simplification
-- rule, as it requires access to the function table.  It is put in a
-- separate module, in order to not clutter the main simplification
-- engine.
module Futhark.EnablingOpts.Simplifier.Apply
  ( simplifyApply
  )
  where

import Futhark.InternalRep
import qualified Futhark.Interpreter as Interp
import qualified Futhark.EnablingOpts.SymbolTable as ST

simplifyApply :: Prog -> ST.SymbolTable -> Name -> [(SubExp,Diet)] -> Maybe [Value]
simplifyApply program vtable fname args = do
  vals <- allArgsAreValues $ map fst args
  either (const Nothing) Just $ Interp.runFunNoTrace fname vals program
  where allArgsAreValues = mapM argIsValue

        argIsValue (Constant val _) = Just val
        argIsValue (Var v)          = case ST.lookup (identName v) vtable of
                                        Just (ST.Value val) -> Just val
                                        _                   -> Nothing
