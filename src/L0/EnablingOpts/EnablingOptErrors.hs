{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module L0.EnablingOpts.EnablingOptErrors ( EnablingOptError(..) )
  where
  
import Data.Loc
import L0.AbSyn
 
-- | Information about an error during type checking.  The 'Show'
-- instance for this type produces a human-readable description.
data EnablingOptError = EnablingOptError SrcLoc String
               -- ^ A general error happened at the given position and
               -- for the given reason.
               | DupParamError String String SrcLoc
               -- ^ Two function parameters share the same name.
               | CopyCtPropError SrcLoc String 
               -- ^ Copy/Constant Propagation Error
               | TypeError SrcLoc String
               | Div0Error SrcLoc
               | DupDefinitionError String SrcLoc SrcLoc
               | FunctionNotInFtab  String

instance Show EnablingOptError where
    show (EnablingOptError pos msg) =
        "Enabling Optimization Error at " ++ locStr pos ++ ":\n" ++ msg
    show (DupParamError funname paramname pos) =
        "Parameter " ++ paramname ++
        " mentioned multiple times in argument list of function " ++
        funname ++ " at " ++ locStr pos ++ "."
    show (CopyCtPropError pos msg ) = --ee
        "Copy or Constant Folding and Propagation Implementation Error " ++ 
        msg ++ " at " ++ locStr pos -- ++ ppExp 0 ee 
    show (TypeError pos s) =
        "Type error at " ++ locStr pos ++ " in " ++ s ++ 
        " during interpretation.  This implies a bug in the type checker."
    show (Div0Error pos) =
        "Division by zero Error detected during copy/constant propagation and folding at line: " 
        ++ locStr pos 
    show (DupDefinitionError name pos1 pos2) =
        "Duplicate definition of function " ++ name ++ ".  Defined at " ++
        locStr pos1 ++ " and " ++ locStr pos2 ++ "."
    show (FunctionNotInFtab fname) = 
        "Function " ++ fname ++ " not found in Function Symbol Table"

