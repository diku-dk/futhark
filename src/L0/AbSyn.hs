{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, ScopedTypeVariables #-}
-- | This Is an Ever-Changing AnSyn for L0.  Some types, such as
-- @Exp@, are parametrised by type representation.
-- See "L0.TypeChecker" and the 'Exp' type for more information.
module L0.AbSyn
  ( locStr
  , Type(..)
  , TypeBox(..)
  , ppType
  , arrayDims
  , arrayShape
  , peelArray
  , baseType
  , basicType
  , arrayType
  , stripArray
  , blankValue
  , Value(..)
  , valueType
  , arrayVal
  , emptyArray
  , ppValue
  , Ident(..)
  , Exp(..)
  , expType
  , expToValue
  , ppExp
  , isBuiltInFun
  , BinOp(..)
  , opStr
  , ppBinOp
  , Lambda(..)
  , lambdaType
  , TupIdent(..)
  , ppTupId
  , FunDec
  , Prog
  , progNames
  , prettyPrint
  )
  where

import Data.Array
import Data.Data
import Data.List
import Data.Generics
import Data.Loc

isBuiltInFun :: String -> Bool
isBuiltInFun fnm = elem fnm ["toReal", "trunc", "sqrt", "log", "exp"]

locStr :: SrcLoc -> String
locStr (SrcLoc NoLoc) = "unknown location"
locStr (SrcLoc (Loc (Pos file line1 col1 _) (Pos _ line2 col2 _))) =
  -- Assume that both positions are in the same file (what would the
  -- alternative mean?)
  file ++ ":" ++ show line1 ++ ":" ++ show col1
       ++ "-" ++ show line2 ++ ":" ++ show col2

-- | L0 Types: Int, Bool, Char, Tuple, multidim-regular Array
--  TODO: please add float, double, long int, etc.
data Type = Int SrcLoc
          | Bool SrcLoc
          | Char SrcLoc
          | Real SrcLoc
          | Tuple [Type] SrcLoc
          | Array Type (Maybe (Exp (Maybe Type))) SrcLoc -- ^ 1st arg: array's element type, 2nd arg: its length
            deriving (Eq, Ord, Show, Typeable, Data)

instance Located Type where
  locOf (Int loc) = locOf loc
  locOf (Bool loc) = locOf loc
  locOf (Char loc) = locOf loc
  locOf (Real loc) = locOf loc
  locOf (Tuple _ loc) = locOf loc
  locOf (Array _ _ loc) = locOf loc

-- | Return the dimensionality of a type.  For non-arrays, this is
-- zero.  For a one-dimensional array it is one, for a two-dimensional
-- it is two, and so forth.
arrayDims :: Type -> Int
arrayDims (Array t _ _) = 1 + arrayDims t
arrayDims _             = 0

-- | A type box provides a way to box a type, and possibly retrieve
-- one.
class (Eq ty, Ord ty, Show ty, Data ty, Typeable ty) => TypeBox ty where
  unboxType :: ty -> Maybe Type
  boxType :: Type -> ty
  getExpType :: Exp ty -> ty

instance TypeBox (Maybe Type) where
  unboxType = id
  boxType = Just
  getExpType _ = Nothing

instance TypeBox Type where
  unboxType = Just
  boxType = id
  getExpType = expType

-- | @peelArray n t@ returns the type resulting from peeling the first
-- @n@ array dimensions from @t@.  Returns @Nothing@ if @t@ has less
-- than @n@ dimensions.
peelArray :: Int -> Type -> Maybe Type
peelArray 0 t = Just t
peelArray n (Array t _ _) = peelArray (n-1) t
peelArray _ _ = Nothing

-- | Returns the bottommost type of an array.
baseType :: Type -> Type
baseType (Array t _ _) = baseType t
baseType t = t

-- | A type is a basic type if it is not an array and any component
-- types are basic types.
basicType :: Type -> Bool
basicType (Array {}) = False
basicType (Tuple ts _) = all basicType ts
basicType _ = True

-- | @array n t@ is the type of @n@-dimensional arrays having @t@ as
-- the base type.
arrayType :: Int -> Type -> Type
arrayType 0 t = t
arrayType n t = arrayType (n-1) $ Array t Nothing (srclocOf t)

-- | @stripArray n t@ removes the @n@ outermost layers of the array.
-- Essentially, it is the type of indexing an array of type @t@ with
-- @n@ indexes.
stripArray :: Int -> Type -> Type
stripArray 0 t = t
stripArray n (Array t _ _) = stripArray (n-1) t
stripArray _ t = t

-- | A "blank" value of the given type - this is zero, or whatever is
-- close to it.  Don't depend on this value, but use it for creating
-- arrays to be populated by do-loops.
blankValue :: Type -> Value
blankValue (Int loc) = IntVal 0 loc
blankValue (Real loc) = RealVal 0.0 loc
blankValue (Bool loc) = LogVal False loc
blankValue (Char loc) = CharVal '\0' loc
blankValue (Tuple ts loc) = TupVal (map blankValue ts) loc
blankValue (Array et _ loc) = arrayVal [blankValue et] et loc

-- | Every possible value in L0.  Values are fully evaluated and their
-- type is always unambiguous.
data Value = IntVal !Int SrcLoc
           | RealVal !Double SrcLoc
           | LogVal !Bool SrcLoc
           | CharVal !Char SrcLoc
           | TupVal ![Value] SrcLoc
           | ArrayVal !(Array Int Value) Type SrcLoc
             -- ^ The type is the element type, not the complete array
             -- type.  It is assumed that the array is 0-indexed.
             deriving (Eq, Ord, Show, Typeable, Data)

-- | Return the type of a value.
valueType :: Value -> Type
valueType (IntVal _ pos) = Int pos
valueType (RealVal _ pos) = Real pos
valueType (LogVal _ pos) = Bool pos
valueType (CharVal _ pos) = Char pos
valueType (TupVal vs pos) = Tuple (map valueType vs) pos
valueType (ArrayVal _ t pos) = Array t Nothing pos

instance Located Value where
  locOf (IntVal _ pos) = locOf pos
  locOf (RealVal _ pos) = locOf pos
  locOf (CharVal _ pos) = locOf pos
  locOf (LogVal _ pos) = locOf pos
  locOf (TupVal _ pos) = locOf pos
  locOf (ArrayVal _ _ pos) = locOf pos

-- | Return a list of the dimensions of an array (the shape, in other
-- terms).  For non-arrays, this is the empty list.  A two-dimensional
-- array with five rows and three columns would return the list @[5,
-- 3]@.
arrayShape :: Value -> [Int]
arrayShape (ArrayVal arr _ _) =
  if size == 0 then [0] else size : arrayShape (arr ! 0)
  where size = upper - lower + 1
        (lower, upper) = bounds arr
arrayShape _ = []

-- | Construct an array value containing the given elements.
arrayVal :: [Value] -> Type -> SrcLoc -> Value
arrayVal vs = ArrayVal $ listArray (0, length vs-1) vs

emptyArray :: Type -> SrcLoc -> Value
emptyArray = arrayVal []

-- | An identifier consists of its name and the type of the value
-- bound to the identifier.
data Ident ty = Ident { identName :: String
                      , identType :: ty
                      , identSrcLoc :: SrcLoc
                      }
                deriving (Eq, Ord, Typeable, Data, Show)

instance Located (Ident ty) where
  locOf = locOf . identSrcLoc

-- | L0 Expression Language: literals + vars + int binops + array
-- constructors + array combinators (SOAC) + if + function calls +
-- let + tuples (literals & identifiers) TODO: please add float,
-- double, long int, etc.
--
-- In a value of type @Exp tt@, all 'Type' values are kept as @tt@
-- values.  -- This allows us to encode whether or not the expression
-- has been type-checked in the Haskell type of the expression.
-- Specifically, the parser will produce expressions of type @Exp
-- 'Maybe Type'@, and the type checker will convert these to @Exp
-- 'Type'@, in which type information is always present.
data Exp ty = Literal Value
            | TupLit    [Exp ty] SrcLoc
            -- ^ Tuple literals, e.g., (1+3, (x, y+z)).  Second
            -- argument is the tuple's type.
            | ArrayLit  [Exp ty] ty SrcLoc
            -- ^ Array literals, e.g., { {1+x, 3}, {2, 1+4} }.  Second
            -- arg is the element type of the array.
            | BinOp BinOp (Exp ty) (Exp ty) ty SrcLoc
            -- Binary Ops for Booleans
            | And    (Exp ty) (Exp ty) SrcLoc
            | Or     (Exp ty) (Exp ty) SrcLoc
            -- Unary Ops: Not for bools and Negate for ints
            | Not    (Exp ty) SrcLoc -- e.g., not True = False
            | Negate (Exp ty) ty SrcLoc -- e.g., ~(~1) = 1
            | If     (Exp ty) (Exp ty) (Exp ty) ty SrcLoc
            | Var    (Ident ty)
            -- Function Call and Let Construct
            | Apply  String [Exp ty] ty SrcLoc
            | LetPat (TupIdent ty) (Exp ty) (Exp ty) SrcLoc
            | LetWith (Ident ty) (Ident ty) [Exp ty] (Exp ty) (Exp ty) SrcLoc
            -- Array Indexing and Array Constructors
            | Index (Ident ty) [Exp ty] ty ty SrcLoc
             -- e.g., arr[3]; 3rd arg is the input-array element type
             -- 4th arg is the result type
            | Iota (Exp ty) SrcLoc -- e.g., iota(n) = {0,1,..,n-1}
            | Size (Exp ty) SrcLoc -- The number of elements in an array.
            | Replicate (Exp ty) (Exp ty) SrcLoc -- e.g., replicate(3,1) = {1, 1, 1}

            | Reshape [Exp ty] (Exp ty) ty ty SrcLoc
             -- 1st arg is the new shape, 2nd arg is the input array *)
             -- 3rd arg is the  input-array type *)
             -- 4th arg is the result-array type *)

            | Transpose (Exp ty) ty ty SrcLoc
             -- 1st arg is the (input) to-be-transSrcLoced array *)
             -- 2nd argument is the  input-array type *)
             -- 3rd argument is the result-array type *)

            -- Second-Order Array Combinators
            -- accept curried and anonymous
            -- functions as (first) params
            | Map (Lambda ty) (Exp ty) ty ty SrcLoc
             -- e.g., map(op +(1), {1,2,..,n}) = {2,3,..,n+1} *)
             -- 3st arg is the input-array element type *)
             -- 4th arg is the output-array element type *)

            | Reduce (Lambda ty) (Exp ty) (Exp ty) ty SrcLoc
             -- e.g., reduce(op +, 0, {1,2,..,n}) = (0+1+2+..+n) *)
             -- 4th arg is the input-array element type          *)

            | Zip [(Exp ty, ty)] SrcLoc
            -- Normal zip supporting variable number of arguments.
            -- The type paired to each expression is the element type
            -- of the array returned by that expression.

            | Unzip (Exp ty) [ty] SrcLoc
            -- Unzip that can unzip tuples of arbitrary size.  The
            -- types are the elements of the tuple.

            | Scan (Lambda ty) (Exp ty) (Exp ty) ty SrcLoc
             -- scan(plus, 0, { 1, 2, 3 }) = { 1, 3, 6 }
             -- 4th arg is the element type of the input array

            | Filter (Lambda ty) (Exp ty) ty SrcLoc
             -- 3rd arg is the element type of the input (and result) array *)

            | Mapall (Lambda ty) (Exp ty) ty ty SrcLoc
             -- e.g., mapall(op ~, {{1,~2}, {~3,4}}) = {{~1,2}, {3,~4}}
             -- 3rd and 4th args are the base types of the input and result arrays, respectively.

            | Redomap (Lambda ty) (Lambda ty) (Exp ty) (Exp ty) ty ty SrcLoc
             -- redomap(g, f, n, a) = reduce(g, n, map(f, a))    *)
             -- 5th arg is the element type of the input  array *)
             -- 6th arg is the element type of the result array *)

            | Split (Exp ty) (Exp ty) ty SrcLoc
             -- split(1, { 1, 2, 3, 4 }) = ({1},{2, 3, 4}) *)
             -- 3rd arg is the element type of the input array *)

            | Concat (Exp ty) (Exp ty) ty SrcLoc
             -- concat ({1},{2, 3, 4}) = {1, 2, 3, 4} *)
             -- 3rd arg is the element type of the input array*)

            | Copy (Exp ty) SrcLoc
            -- Copy the value return by the expression.  This only
            -- makes a difference in do-loops with merge variables.

            -- IO
            | Read Type SrcLoc
             -- e.g., read(int); 1st arg is a basic-type, i.e., of the to-be-read element *)

            | Write (Exp ty) ty SrcLoc
             -- e.g., write(map(f, replicate(3,1))) writes array {f(1),f(1),f(1)} *)
             -- 2nd arg is the type of the to-be-written expression *)
            | DoLoop [(Ident ty, Exp ty)] -- ^ Bound merge variables.
              (Ident ty) -- ^ Iterator.
              (Exp ty) -- ^ Upper bound.
              (Exp ty) -- ^ Loop body.
              (Exp ty) -- ^ Let-body.
              SrcLoc
              
              deriving (Eq, Ord, Show, Typeable, Data)

instance Located (Exp ty) where
  locOf (Literal val) = locOf val
  locOf (TupLit _ pos) = locOf pos
  locOf (ArrayLit _ _ pos) = locOf pos
  locOf (BinOp _ _ _ _ pos) = locOf pos
  locOf (And _ _ pos) = locOf pos
  locOf (Or _ _ pos) = locOf pos
  locOf (Not _ pos) = locOf pos
  locOf (Negate _ _ pos) = locOf pos
  locOf (If _ _ _ _ pos) = locOf pos
  locOf (Var ident) = locOf ident
  locOf (Apply _ _ _ pos) = locOf pos
  locOf (LetPat _ _ _ pos) = locOf pos
  locOf (LetWith _ _ _ _ _ pos) = locOf pos
  locOf (Index _ _ _ _ pos) = locOf pos
  locOf (Iota _ pos) = locOf pos
  locOf (Size _ pos) = locOf pos
  locOf (Replicate _ _ pos) = locOf pos
  locOf (Reshape _ _ _ _ pos) = locOf pos
  locOf (Transpose _ _ _ pos) = locOf pos
  locOf (Map _ _ _ _ pos) = locOf pos
  locOf (Reduce _ _ _ _ pos) = locOf pos
  locOf (Zip _ pos) = locOf pos
  locOf (Unzip _ _ pos) = locOf pos
  locOf (Scan _ _ _ _ pos) = locOf pos
  locOf (Filter _ _ _ pos) = locOf pos
  locOf (Mapall _ _ _ _ pos) = locOf pos
  locOf (Redomap _ _ _ _ _ _ pos) = locOf pos
  locOf (Split _ _ _ pos) = locOf pos
  locOf (Concat _ _ _ pos) = locOf pos
  locOf (Copy _ pos) = locOf pos
  locOf (Read _ pos) = locOf pos
  locOf (Write _ _ pos) = locOf pos
  locOf (DoLoop _ _ _ _ _ pos) = locOf pos

-- | Given an expression with known types, return its type.
expType :: Exp Type -> Type
expType (Literal val) = valueType val
expType (TupLit es loc) = Tuple (map expType es) loc
expType (ArrayLit _ t pos) = Array t Nothing pos
expType (BinOp _ _ _ t _) = t
expType (And _ _ pos) = Bool pos
expType (Or _ _ pos) = Bool pos
expType (Not _ pos) = Bool pos
expType (Negate _ t _) = t
expType (If _ _ _ t _) = t
expType (Var ident) = identType ident
expType (Apply _ _ t _) = t
expType (LetPat _ _ body _) = expType body
expType (LetWith _ _ _ _ body _) = expType body
expType (Index _ _ _ t _) = t
expType (Iota _ pos) = Array (Int pos) Nothing pos
expType (Size _ pos) = Int pos
expType (Replicate _ e pos) = Array (expType e) Nothing pos
expType (Reshape _ _ _ t _) = t
expType (Transpose _ _ t _) = t
expType (Map _ _ _ t pos) = Array t Nothing pos
expType (Reduce fun _ _ _ _) = lambdaType fun
expType (Zip es pos) = Array (Tuple (map snd es) pos) Nothing pos
expType (Unzip _ ts pos) = Tuple (map (\t -> Array t Nothing pos) ts) pos
expType (Scan fun _ _ _ _) = arrayType 1 $ lambdaType fun
expType (Filter _ _ t loc) = Array t Nothing loc
expType (Mapall fun e _ _ _) = arrayType (arrayDims $ expType e) $ lambdaType fun
expType (Redomap _ _ _ _ _ t loc) = Array t Nothing loc
expType (Split _ _ t pos) = Tuple [Array t Nothing pos, Array t Nothing pos] $ srclocOf t
expType (Concat _ _ t _) = Array t Nothing $ srclocOf t
expType (Copy e _) = expType e
expType (Read t _) = boxType t
expType (Write _ t _) = t
expType (DoLoop _ _ _ _ body _) = expType body

-- | Eagerly evaluated binary operators.  In particular, the
-- short-circuited operators && and || are not here, although an
-- eagerly evaluated variant is.
data BinOp = Plus -- Binary Ops for Numbers
           | Minus
           | Pow
           | Times
           | Divide
           | ShiftR
           | ShiftL
           | Band
           | Xor
           | Bor
           | LogAnd
           | LogOr
           -- Relational Ops for all basic types at least
           | Equal
           | Less
           | Leq
             deriving (Eq, Ord, Enum, Bounded, Typeable, Data, Show)

-- ^ Print the operator, without whitespace, that corresponds to this
-- @BinOp@.
opStr :: BinOp -> String
opStr Plus = "+"
opStr Minus = "-"
opStr Pow = "pow"
opStr Times = "*"
opStr Divide = "/"
opStr ShiftR = ">>"
opStr ShiftL = "<<"
opStr Band = "&"
opStr Xor = "^"
opStr Bor = "|"
opStr LogAnd = "&&"
opStr LogOr = "||"
opStr Equal = "="
opStr Less = "<"
opStr Leq = "<="

-- | If possible, convert an expression to a value.  This is not a
-- true constant propagator, but a quick way to convert array/tuple
-- literal expressions into literal values instead.
expToValue :: Exp Type -> Maybe Value
expToValue (Literal val) = Just val
expToValue (TupLit es loc) = do es' <- mapM expToValue es
                                Just $ TupVal es' loc
expToValue (ArrayLit es et loc) = do es' <- mapM expToValue es
                                     Just $ arrayVal es' et loc
expToValue _ = Nothing

-- | Anonymous Function
data Lambda ty = AnonymFun [Ident Type] (Exp ty) Type SrcLoc
                    -- fn int (bool x, char z) => if(x) then ord(z) else ord(z)+1 *)
               | CurryFun String [Exp ty] ty SrcLoc
                    -- op +(4) *)
                 deriving (Eq, Ord, Typeable, Data, Show)

-- | The return type of a lambda function.
lambdaType :: Lambda Type -> Type
lambdaType (AnonymFun _ _ t _) = t
lambdaType (CurryFun _ _ t _) = t

-- | Tuple Identifier, i.e., pattern matching
data TupIdent ty = TupId [TupIdent ty] SrcLoc
                 | Id (Ident ty)
                   deriving (Eq, Ord, Typeable, Data, Show)

instance Located (TupIdent ty) where
  locOf (TupId _ loc) = locOf loc
  locOf (Id ident) = locOf ident

-- | Function Declarations
type FunDec ty = (String,Type,[Ident Type],Exp ty,SrcLoc)

type Prog ty = [FunDec ty]

-- | Return a list of all variable names mentioned in program.
progNames :: forall ty.TypeBox ty => Prog ty -> [String]
progNames = everything union (mkQ [] identName')
  where identName' :: Ident ty -> [String]
        identName' k = [identName k]

-- Pretty-Printing Functionality

spaces :: Int -> String
spaces n = replicate (n*2) ' '

tildes :: String -> String
tildes = map tilde
  where tilde '-' = '~'
        tilde c   = c

-- | Pretty printing a value.
ppValue :: Value -> String
ppValue (IntVal n _)  = (tildes $ show n) ++ " "
ppValue (RealVal n _) = (tildes $ show n) ++ " "
ppValue (LogVal b _)  = (show b) ++ " "
ppValue (CharVal c _) = (show c) ++ " "
ppValue (ArrayVal arr t _)
  | [] <- elems arr = " empty (" ++ ppType t ++ " ) "
  | Just (c:cs) <- mapM char (elems arr) = show $ c:cs
  | otherwise = " { " ++ intercalate ", " (map ppValue $ elems arr) ++ " } "
    where char (CharVal c _) = Just c
          char _             = Nothing
ppValue (TupVal vs _)   =
  " ( " ++ intercalate ", " (map ppValue vs) ++ " ) "

-- | Pretty printing an expression
ppExp :: Int -> Exp ty -> String
ppExp _ (Literal val)     = ppValue val
ppExp d (ArrayLit es _ _) =
  " { " ++ intercalate ", " (map (ppExp d) es) ++ " } "
ppExp d (TupLit es _) =
  " ( " ++ intercalate ", " (map (ppExp d) es) ++ " ) "
ppExp _ (Var ident) = identName ident ++ " "

ppExp d (BinOp op e1 e2 _ _) = " ( " ++ ppExp d e1 ++ ppBinOp op ++ ppExp d e2 ++ " ) "
ppExp d (And   e1 e2 _  ) = " ( " ++ ppExp d e1 ++ " && " ++ ppExp d e2 ++ " ) "
ppExp d (Or    e1 e2 _  ) = " ( " ++ ppExp d e1 ++ " || " ++ ppExp d e2 ++ " ) "

ppExp d (Not   e _      ) = " ( " ++ "not " ++ ppExp d e ++ " ) "
ppExp d (Negate e _ _   ) = " ( " ++ "~ " ++ ppExp d e ++   " ) "

ppExp d (If    e1 e2 e3 _ _)  =
  "\n" ++
  spaces (d+1) ++ "if( "  ++ ppExp (d+2) e1 ++ " )\n" ++
  spaces (d+1) ++ "then " ++ ppExp (d+2) e2 ++ "\n" ++
  spaces (d+1) ++ "else " ++ ppExp (d+2) e3 ++ "\n" ++
  spaces d

ppExp _ (Apply f [] _ _)    = f ++ "() "
ppExp d (Apply f args _ _)  =
  f ++ "( " ++ intercalate ", " (map (ppExp d) args) ++ " ) "

ppExp d (LetPat tupid e1 body _) =
        "\n" ++ spaces d ++ "let " ++ ppTupId tupid ++ " = " ++ ppExp d e1 ++
        "in  " ++ ppExp d body
ppExp d (LetWith (Ident dest _ _) (Ident src _ _) es el e2 _)
  | dest == src =
    "\n" ++ spaces d ++ "let " ++ dest ++ "[ " ++
    intercalate ", " (map (ppExp d) es) ++
    "] = " ++ ppExp d el ++ "in  " ++ ppExp d e2
  | otherwise =
    "\n" ++ spaces d ++ "let " ++ dest ++ " = " ++ src ++
    " with [ " ++ intercalate ", " (map (ppExp d) es) ++
    "] <- " ++ ppExp d el ++ "in  " ++ ppExp d e2

ppExp d (Index (Ident name _ _) es _ _ _) =
  name ++ "[ " ++ intercalate ", " (map (ppExp d) es) ++ " ] "

-- | Array Constructs
ppExp d (Iota e _)         = "iota ( " ++ ppExp d e ++ " ) "
ppExp d (Size e _)         = "size ( " ++ ppExp d e ++ " ) "
ppExp d (Replicate e el _) = "replicate ( " ++ ppExp d e ++ ", " ++ ppExp d el ++ " ) "

ppExp d (Transpose e _ _ _) = " transpose ( " ++ ppExp d e ++ " ) "

ppExp d (Reshape es arr _ _ _) =
  " reshape ( ( " ++ intercalate ", " (map (ppExp d) es) ++ " ), "  ++
  ppExp d arr ++ " ) "

ppExp d (Map fun e _ _ _) = " map ( " ++ ppLambda (d+1) fun ++ ", " ++ ppExp (d+1) e ++ " ) "

ppExp d (Zip es _) =
  " zip ( " ++ intercalate ", " (map (ppExp d . fst) es) ++ " ) "

ppExp d (Unzip e _ _) = " unzip ( " ++ ppExp d e ++ " ) "

ppExp d (Reduce fun el lst _ _) =
  " reduce ( " ++ ppLambda (d+1) fun ++ ", " ++ ppExp (d+1) el ++ ", " ++ ppExp (d+1) lst ++ " ) "
ppExp d (Scan  fun el lst _ _) =
  " scan ( " ++ ppLambda (d+1) fun ++ ", " ++ ppExp (d+1) el ++ ", " ++ ppExp (d+1) lst ++ " ) "
ppExp d (Filter fun a _ _) =
  " filter ( " ++ ppLambda (d+1) fun ++ ", " ++ ppExp (d+1) a ++ " ) "
ppExp d (Mapall fun a _ _ _)
          = " mapall ( " ++ ppLambda (d+1) fun ++ ", " ++ ppExp (d+1) a ++ " ) "
ppExp d (Redomap id1 id2 el a _ _ _)
          = " redomap ( " ++ ppLambda (d+1) id1 ++ ", " ++ ppLambda (d+1) id2 ++ 
            ", " ++ ppExp (d+1) el ++ ", " ++ ppExp (d+1) a ++ " ) "

ppExp d (Split  idx arr _ _) = " split ( " ++ ppExp d idx ++ ", " ++ ppExp d arr ++ " ) "
ppExp d (Concat a1  a2 _ _) = " concat ( " ++ ppExp d a1 ++ ", " ++ ppExp d a2 ++ " ) "
ppExp d (Copy e _) = " copy ( " ++ ppExp d e ++ " ) "

ppExp _ (Read t _) = " read("  ++ ppType t  ++ ") "
ppExp d (Write e _ _) = " write("  ++ ppExp d e  ++ ") "
ppExp d (DoLoop mvs i n loopbody letbody _) =
  let ppMVar (v, e) = identName v ++ " = " ++ ppExp d e
  in "\n" ++ spaces d ++ "loop (" ++ intercalate ", " (map ppMVar mvs) ++
       ") = " ++ "for " ++ identName i ++ " < " ++ ppExp d n ++ " do " ++
       "\n" ++ spaces(d+1) ++ ppExp (d+1) loopbody ++ "\n" ++ spaces d ++
       "in " ++ ppExp d letbody
ppBinOp :: BinOp -> String
ppBinOp op = " " ++ opStr op ++ " "

-- | Pretty printing a type
ppType :: Type -> String
ppType (Int _) = " int "
ppType (Bool _) = " bool "
ppType (Char _) = " char "
ppType (Real _) = " real "
ppType (Array tp  Nothing _) = "[ " ++ ppType tp ++ " ] "
ppType (Array tp  (Just l) _) = "[ " ++ ppType tp ++ ", " ++ ppExp 0 l ++ " ] "
ppType (Tuple tps _) = "( " ++ intercalate " * " (map ppType tps) ++ " ) "

-- | Pretty printing a tuple id
ppTupId :: TupIdent ty -> String
ppTupId (Id ident) = " " ++ identName ident ++ " "
ppTupId (TupId pats _) = " ( " ++ intercalate ", " (map ppTupId pats) ++ " ) "

--        "\n" ++ spaces d ++ "let " ++ ppTupId tupid ++ " = " ++ ppExp d e1 ++
--        " in  " ++ ppExp d body



-- pretty printing Lambda, i.e., curried and unnamed functions *)
ppLambda :: Int -> Lambda ty -> String
ppLambda d ( AnonymFun params body rtp _) =
      let pp_bd (Ident arg tp _) = ppType tp ++ " " ++ arg
          strargs = intercalate ", " $ map pp_bd params
      in "\n" ++ spaces d ++ "fn " ++ ppType rtp ++ " ( " ++ strargs ++ 
         " ) " ++ " => " ++ ppExp (d+1) body ++ "\n" ++ spaces d
--      let pp_bd (Ident arg tp _) = ppType tp ++ " " ++ arg
--          strargs = intercalate ", " $ map pp_bd params
--      in " fn " ++ ppType rtp ++ " ( " ++ strargs ++ " ) " ++ " => " ++ ppExp 0 body
ppLambda _ ( CurryFun fid []   _  _  ) = fid
ppLambda _ ( CurryFun fid args ty pos) =
      ppExp 0 (Apply fid args ty pos)

-- | pretty printing a function declaration
ppFun :: Int -> FunDec ty -> String
ppFun d (name, ret_tp, args, body, _) =
  let -- pretty printing a list of bindings separated by commas
      ppBd (Ident argname tp _) = ppType tp ++ " " ++ argname
      pp_bindings = intercalate "," . map ppBd

  in "\n\nfun " ++ ppType ret_tp ++ name ++
     "( " ++ pp_bindings args ++ ") = \n" ++
     spaces (d+1) ++ ppExp (d+1) body

-- | Pretty printing a program.
prettyPrint :: Prog ty -> String
prettyPrint p = concatMap (ppFun 0) p ++ "\n"
