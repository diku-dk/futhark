-- | Common error definition for all the enabling optimisation submodules.
module Futhark.Optimise.Errors ( Error(..) )
  where

import Data.Loc
import Futhark.Representation.Basic

-- | Information about an error during enabling optimisations.  The 'Show'
-- instance for this type produces a human-readable description.
data Error = Error SrcLoc String
           -- ^ A general error happened at the given position and
           -- for the given reason.
           | DupParamError Name VName SrcLoc
           -- ^ Two function parameters share the same name.
           | CopyCtPropError SrcLoc String
           -- ^ Copy/Constant Propagation Error
           | TypeError SrcLoc String
           | DupDefinitionError Name SrcLoc SrcLoc
           | FunctionNotInFtab  Name
           | VarNotInFtab SrcLoc VName
           | SimplifyError SrcLoc String
           -- ^ A generic exp simplification error happened at
           -- the given position and for the given reason.


instance Show Error where
    show (Error pos msg) =
        "Enabling Optimization Error at " ++ locStr pos ++ ":\n" ++ msg
    show (DupParamError funname paramname pos) =
        "Parameter " ++ textual paramname ++
        " mentioned multiple times in argument list of function " ++
        nameToString funname ++ " at " ++ locStr pos ++ "."
    show (CopyCtPropError pos msg ) = --ee
        "Copy or Constant Folding and Propagation Implementation Error " ++
        msg ++ " at " ++ locStr pos -- ++ ppExp 0 ee
    show (TypeError pos s) =
        "Type error at " ++ locStr pos ++ " in " ++ s ++
        " during interpretation.  This implies a bug in the type checker."
    show (DupDefinitionError name pos1 pos2) =
        "Duplicate definition of function " ++ nameToString name ++ ".  Defined at " ++
        locStr pos1 ++ " and " ++ locStr pos2 ++ "."
    show (FunctionNotInFtab fname) =
        "Function " ++ nameToString fname ++ " not found in Function Symbol Table"
    show (VarNotInFtab pos name) =
        "Variable " ++ textual name ++ " not found in symbol table at " ++ locStr pos ++ "."
    show (SimplifyError pos msg) =
        "Generic Simplification Error at " ++ locStr pos ++ ":\n" ++ msg
