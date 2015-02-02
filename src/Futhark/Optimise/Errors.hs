-- | Common error definition for all the enabling optimisation submodules.
module Futhark.Optimise.Errors ( Error(..) )
  where

import Futhark.Representation.AST

-- | Information about an error during enabling optimisations.  The 'Show'
-- instance for this type produces a human-readable description.
data Error = Error String
           -- ^ A general error happened at the given position and
           -- for the given reason.
           | DupParamError Name VName
           -- ^ Two function parameters share the same name.
           | TypeError String
           | DupDefinitionError Name
           | FunctionNotInFtab  Name
           | SimplifyError String
           -- ^ A generic exp simplification error happened at
           -- the given position and for the given reason.


instance Show Error where
    show (Error msg) =
        "Enabling Optimization Error:\n" ++ msg
    show (DupParamError funname paramname) =
        "Parameter " ++ textual paramname ++
        " mentioned multiple times in argument list of function " ++
        nameToString funname ++ "."
    show (TypeError s) =
        "Type error in " ++ s ++
        " during interpretation.  This implies a bug in the type checker."
    show (DupDefinitionError name) =
        "Duplicate definition of function " ++ nameToString name
    show (FunctionNotInFtab fname) =
        "Function " ++ nameToString fname ++ " not found in Function Symbol Table"
    show (SimplifyError msg) =
        "Generic Simplification Error:\n" ++ msg
