module Futhark.TypeCheck.TypeError
  ( GenTypeError(..)
  , Several(..)
  , justOne
  )

where

import Data.Loc
import Data.List

import Text.PrettyPrint.Mainland
import Language.Futhark.Core

-- | Information about an error during type checking.  The 'Show'
-- instance for this type produces a human-readable description.
data GenTypeError vn e t pat =
    TypeError SrcLoc String
  -- ^ A general error happened at the given position and
  -- for the given reason.
  | UnifyError e t e t
  -- ^ Types of two expressions failed to unify.
  | UnexpectedType e t [t]
  -- ^ Expression of type was not one of the expected
  -- types.
  | ReturnTypeError SrcLoc Name t t
  -- ^ The body of a function definition has a different
  -- type than its declaration.
  | DupDefinitionError Name SrcLoc SrcLoc
  -- ^ Two functions have been defined with the same name.
  | DupParamError Name vn SrcLoc
  -- ^ Two function parameters share the same name.
  | DupPatternError vn SrcLoc SrcLoc
  -- ^ Two pattern variables share the same name.
  | InvalidPatternError pat t SrcLoc
  -- ^ The pattern is not compatible with the type.
  | UnknownVariableError vn SrcLoc
  -- ^ Unknown variable of the given name referenced at the given spot.
  | UnknownFunctionError Name SrcLoc
  -- ^ Unknown function of the given name called at the given spot.
  | ParameterMismatch (Maybe Name) SrcLoc (Either Int [t]) [t]
  -- ^ A function (possibly anonymous) was called with
  -- invalid arguments.  The third argument is either the
  -- number of parameters, or the specific types of
  -- parameters accepted (sometimes, only the former can
  -- be determined).
  | UseAfterConsume vn SrcLoc SrcLoc
  -- ^ A variable was attempted used after being
  -- consumed.  The last location is the point of
  -- consumption.
  | IndexingError vn Int Int SrcLoc
  -- ^ Too many indices provided.  The first integer is
  -- the number of dimensions in the array being
  -- indexed.
  | BadAnnotation SrcLoc String t t
  -- ^ One of the type annotations fails to match with the
  -- derived type.  The string is a description of the
  -- role of the type.  The last type is the new derivation.
  | BadTupleAnnotation SrcLoc String [Maybe t] [t]
  -- ^ One of the tuple type annotations fails to
  -- match with the derived type.  The string is a
  -- description of the role of the type.  The last
  -- type is the elemens of the new derivation.
  | CurriedConsumption Name SrcLoc
  -- ^ A function is being curried with an argument to be consumed.
  | BadLetWithValue SrcLoc
  -- ^ The new value for an array slice in let-with is aliased to the source.
  | ReturnAliased Name vn SrcLoc
  -- ^ The unique return value of the function aliases
  -- one of the function parameters.
  | UniqueReturnAliased Name SrcLoc
  -- ^ A unique element of the tuple returned by the
  -- function aliases some other element of the tuple.
  | NotAnArray SrcLoc e t
  | PermutationError SrcLoc [Int] Int (Maybe vn)
  -- ^ The permutation is not valid.

instance (VarName vn, Pretty e, Located e, Pretty t, Pretty pat) => Show (GenTypeError vn e t pat) where
  show (TypeError pos msg) =
    "Type error at " ++ locStr pos ++ ":\n" ++ msg
  show (UnifyError e1 t1 e2 t2) =
    "Cannot unify type " ++ ppr' t1 ++
    " of expression at " ++ locStr (srclocOf e1) ++
    " with type " ++ ppr' t2 ++
    " of expression at " ++ locStr (srclocOf e2)
  show (UnexpectedType e _ []) =
    "Type of expression at " ++ locStr (srclocOf e) ++
    " cannot have any type - possibly a bug in the type checker."
  show (UnexpectedType e t ts) =
    "Type of expression at " ++ locStr (srclocOf e) ++
    " must be one of " ++ intercalate ", " (map ppr' ts) ++ ", but is " ++
    ppr' t ++ "."
  show (ReturnTypeError pos fname rettype bodytype) =
    "Declaration of function " ++ nameToString fname ++ " at " ++ locStr pos ++
    " declares return type " ++ ppr' rettype ++ ", but body has type " ++
    ppr' bodytype
  show (DupDefinitionError name pos1 pos2) =
    "Duplicate definition of function " ++ nameToString name ++ ".  Defined at " ++
    locStr pos1 ++ " and " ++ locStr pos2 ++ "."
  show (DupParamError funname paramname pos) =
    "Parameter " ++ textual paramname ++
    " mentioned multiple times in argument list of function " ++
    nameToString funname ++ " at " ++ locStr pos ++ "."
  show (DupPatternError name pos1 pos2) =
    "Variable " ++ textual name ++ " bound twice in tuple pattern; at " ++
    locStr pos1 ++ " and " ++ locStr pos2 ++ "."
  show (InvalidPatternError pat t loc) =
    "Pattern " ++ ppr' pat ++
    " cannot match value of type " ++ ppr' t ++ " at " ++ locStr loc ++ "."
  show (UnknownVariableError name pos) =
    "Unknown variable " ++ textual name ++ " referenced at " ++ locStr pos ++ "."
  show (UnknownFunctionError fname pos) =
    "Unknown function " ++ nameToString fname ++ " called at " ++ locStr pos ++ "."
  show (ParameterMismatch fname pos expected got) =
    "In call of " ++ fname' ++ " at position " ++ locStr pos ++ ":\n" ++
    "expecting " ++ show nexpected ++ " argument(s) of type(s) " ++
     expected' ++ ", but got " ++ show ngot ++
    " arguments of types " ++ intercalate ", " (map ppr' got) ++ "."
    where (nexpected, expected') =
            case expected of
              Left i -> (i, "(polymorphic)")
              Right ts -> (length ts, intercalate ", " $ map ppr' ts)
          ngot = length got
          fname' = maybe "anonymous function" (("function "++) . nameToString) fname
  show (UseAfterConsume name rloc wloc) =
    "Variable " ++ textual name ++ " used at " ++ locStr rloc ++
    ", but it was consumed at " ++ locStr wloc ++ ".  (Possibly through aliasing)"
  show (IndexingError name dims got pos) =
    show got ++ " indices given at " ++ locStr pos ++
    ", but type of variable " ++ textual name ++
    " has " ++ show dims ++ " dimension(s)."
  show (BadAnnotation loc desc expected got) =
    "Annotation of \"" ++ desc ++ "\" type of expression at " ++
    locStr loc ++ " is " ++ ppr' expected ++
    ", but derived to be " ++ ppr' got ++ "."
  show (BadTupleAnnotation loc desc expected got) =
    "Annotation of \"" ++ desc ++ "\" type of expression at " ++
    locStr loc ++ " is a tuple {" ++
    intercalate ", " (map (maybe "(unspecified)" ppr') expected) ++
    "}, but derived to be " ++ ppTuple got ++ "."
  show (CurriedConsumption fname loc) =
    "Function " ++ nameToString fname ++
    " curried over a consuming parameter at " ++ locStr loc ++ "."
  show (BadLetWithValue loc) =
    "New value for elements in let-with shares data with source array at " ++
    locStr loc ++ ".  This is illegal, as it prevents in-place modification."
  show (ReturnAliased fname name loc) =
    "Unique return value of function " ++ nameToString fname ++ " at " ++
    locStr loc ++ " is aliased to " ++ textual name ++ ", which is not consumed."
  show (UniqueReturnAliased fname loc) =
    "A unique tuple element of return value of function " ++
    nameToString fname ++ " at " ++ locStr loc ++
    " is aliased to some other tuple component."
  show (NotAnArray loc _ t) =
    "The expression at " ++ locStr loc ++
    " is expected to be an array, but is " ++ ppr' t ++ "."
  show (PermutationError loc perm rank name) =
    "The permutation (" ++ intercalate ", " (map show perm) ++
    ") is not valid for array " ++ name' ++ "of rank " ++ show rank ++ " at " ++
    locStr loc ++ "."
    where name' = maybe "" ((++" ") . textual) name

ppTuple :: Pretty a => [a] -> String
ppTuple ts = intercalate ", " $ map ppr' ts

ppr' :: Pretty a => a -> String
ppr' = pretty 80 . ppr

-- | A list.  Its 'Pretty' instance produces a comma-separated
-- sequence enclosed in braces if the list has anything but a single
-- element.
newtype Several a = Several [a]
  deriving (Eq, Ord, Show)

instance Pretty a => Pretty (Several a) where
  ppr (Several [t]) = ppr t
  ppr (Several ts)  = braces $ commasep $ map ppr ts

-- | Turn a single value into a singleton list.
justOne :: a -> Several a
justOne x = Several [x]
