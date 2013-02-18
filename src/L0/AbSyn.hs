{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}
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
  , Value(..)
  , valueType
  , arrayVal
  , ppValue
  , Exp(..)
  , expType
  , ppExp
  , BinOp(..)
  , opStr
  , ppBinOp
  , Lambda(..)
  , lambdaType
  , TupIdent(..)
  , ppTupId
  , Binding
  , FunDec
  , Prog
  , prettyPrint
  )
  where

import Data.Array
import Data.Data
import Data.List
import Data.Loc

locStr :: Loc -> String
locStr NoLoc = "uknown location"
locStr (Loc (Pos file line1 col1 _) (Pos _ line2 col2 _)) =
  -- Assume that both positions are in the same file (what would the
  -- alternative mean?)
  file ++ ":" ++ show line1 ++ ":" ++ show col1
       ++ "-" ++ show line2 ++ ":" ++ show col2

-- | L0 Types: Int, Bool, Char, Tuple, multidim-regular Array
--  TODO: please add float, double, long int, etc.
data Type = Int Loc
          | Bool Loc
          | Char Loc
          | Real Loc
          | Tuple [Type] Loc
          | Array Type (Maybe (Exp (Maybe Type))) Loc -- ^ 1st arg: array's element type, 2nd arg: its length
            deriving (Typeable, Data)

instance Eq Type where
  Int _ == Int _ = True
  Bool _ == Bool _ = True
  Char _ == Char _ = True
  Real _ == Real _ = True
  Tuple ts1 _ == Tuple ts2 _ = ts1 == ts2
  Array t1 _ _ == Array t2 _ _ = t1 == t2
  _ == _ = False

instance Located Type where
  locOf (Int loc) = loc
  locOf (Bool loc) = loc
  locOf (Char loc) = loc
  locOf (Real loc) = loc
  locOf (Tuple _ loc) = loc
  locOf (Array _ _ loc) = loc

-- | Return the dimensionality of a type.  For non-arrays, this is
-- zero.  For a one-dimensional array it is one, for a two-dimensional
-- it is two, and so forth.
arrayDims :: Type -> Int
arrayDims (Array t _ _) = 1 + arrayDims t
arrayDims _             = 0

-- | A type box provides a way to box a type, and possibly retrieve
-- one.
class TypeBox ty where
  unboxType :: ty -> Maybe Type
  boxType :: Type -> ty

instance TypeBox (Maybe Type) where
  unboxType = id
  boxType = Just

instance TypeBox Type where
  unboxType = Just
  boxType = id

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
arrayType n t = arrayType (n-1) $ Array t Nothing (locOf t)

-- | Every possible value in L0.  Values are fully evaluated and their
-- type is always unambiguous.
data Value = IntVal !Int Loc
           | RealVal !Double Loc
           | LogVal !Bool Loc
           | CharVal !Char Loc
           | TupVal ![Value] Loc
           | ArrayVal !(Array Int Value) Type Loc
             -- ^ The type is the element type, not the complete array
             -- type.  It is assumed that the array is 0-indexed.
             deriving (Typeable, Data)

instance Eq Value where
  IntVal x _ == IntVal y _ = x == y
  RealVal x _ == RealVal y _ = x == y
  LogVal a _ == LogVal b _ = a == b
  CharVal a _ == CharVal b _ = a == b
  TupVal vs1 _ == TupVal vs2 _ = vs1 == vs2
  ArrayVal vs1 _ _ == ArrayVal vs2 _ _ = vs1 == vs2
  _ == _ = False

instance Ord Value where
  IntVal x _ <= IntVal y _ = x <= y
  RealVal x _ <= RealVal y _ = x <= y
  LogVal a _ <= LogVal b _ = a <= b
  CharVal a _ <= CharVal b _ = a <= b
  TupVal vs1 _ <= TupVal vs2 _ = vs1 <= vs2
  ArrayVal vs1 _ _ <= ArrayVal vs2 _ _ = vs1 <= vs2
  _ <= _ = False

-- | Return the type of a value.
valueType :: Value -> Type
valueType (IntVal _ pos) = Int pos
valueType (RealVal _ pos) = Real pos
valueType (LogVal _ pos) = Bool pos
valueType (CharVal _ pos) = Char pos
valueType (TupVal vs pos) = Tuple (map valueType vs) pos
valueType (ArrayVal _ t pos) = Array t Nothing pos

instance Located Value where
  locOf (IntVal _ pos) = pos
  locOf (RealVal _ pos) = pos
  locOf (CharVal _ pos) = pos
  locOf (LogVal _ pos) = pos
  locOf (TupVal _ pos) = pos
  locOf (ArrayVal _ _ pos) = pos

-- | Return a list of the dimensions of an array (the shape, in other
-- terms).  For non-arrays, this is the empty list.  A two-dimensional
-- array with five rows and three columns would return the list @[5,
-- 3]@.
arrayShape :: Value -> [Int]
arrayShape (ArrayVal arr _ _) =
  if size == 0 then [0] else size : arrayShape (arr ! 0)
  where size = uncurry (-) $ bounds arr
arrayShape _ = []

-- | Construct an array value containing the given elements.
arrayVal :: [Value] -> Type -> Loc -> Value
arrayVal vs = ArrayVal $ listArray (0, length vs-1) vs

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
            | TupLit    [Exp ty] ty Loc
            -- ^ Tuple literals, e.g., (1+3, (x, y+z)).  Second
            -- argument is the tuple's type.
            | ArrayLit  [Exp ty] ty Loc
            -- ^ Array literals, e.g., { {1+x, 3}, {2, 1+4} }.  Second
            -- arg is the element type of the array.
            | BinOp BinOp (Exp ty) (Exp ty) ty Loc
            -- Binary Ops for Booleans
            | And    (Exp ty) (Exp ty) Loc
            | Or     (Exp ty) (Exp ty) Loc
            -- Unary Ops: Not for bools and Negate for ints
            | Not    (Exp ty) Loc -- e.g., not True = False
            | Negate (Exp ty) ty Loc -- e.g., ~(~1) = 1
            | If     (Exp ty) (Exp ty) (Exp ty) ty Loc
            | Var    String ty Loc
            -- Function Call and Let Construct
            | Apply  String [Exp ty] ty Loc
            | LetPat (TupIdent ty) (Exp ty) (Exp ty) Loc
            | LetWith String (Exp ty) [Exp ty] (Exp ty) (Exp ty) Loc
            -- Array Indexing and Array Constructors
            | Index String [Exp ty] ty ty Loc
             -- e.g., arr[3]; 3rd arg is the input-array element type
             -- 4th arg is the result type
            | Iota (Exp ty) Loc -- e.g., iota(n) = {0,1,..,n-1}
            | Size (Exp ty) Loc -- The number of elements in an array.
            | Replicate (Exp ty) (Exp ty) ty Loc -- e.g., replicate(3,1) = {1, 1, 1}
                                                    -- Type is element type of output array

            | Reshape [Exp ty] (Exp ty) ty ty Loc
             -- 1st arg is the new shape, 2nd arg is the input array *)
             -- 3rd arg is the  input-array type *)
             -- 4th arg is the result-array type *)

            | Transpose (Exp ty) ty ty Loc
             -- 1st arg is the (input) to-be-transLoced array *)
             -- 2nd argument is the  input-array type *)
             -- 3rd argument is the result-array type *)

            -- Second-Order Array Combinators
            -- accept curried and anonymous
            -- functions as (first) params
            | Map (Lambda ty) (Exp ty) ty ty Loc
             -- e.g., map(op +(1), {1,2,..,n}) = {2,3,..,n+1} *)
             -- 3st arg is the input-array  type *)
             -- 4th arg is the output-array type *)

            | Reduce (Lambda ty) (Exp ty) (Exp ty) ty Loc
             -- e.g., reduce(op +, 0, {1,2,..,n}) = (0+1+2+..+n) *)
             -- 4th arg is the input-array type                  *)

            | ZipWith (Lambda ty) [Exp ty] ty Loc
             -- zipWith(plus, {1,2,3}, {4,5,6}) == {5, 7, 9}       *)
             -- 3rd arg is the type of the result array            *)

            | Zip [Exp ty] Loc
            -- Normal zip supporting variable number of arguments.

            | Unzip (Exp ty) [ty] Loc
            -- Unzip that can unzip tuples of arbitrary size.

            | Scan (Lambda ty) (Exp ty) (Exp ty) ty Loc
             -- scan(plus, 0, { 1, 2, 3 }) = { 0, 1, 3, 6 } *)
             -- 4th arg is the type of the input array      *)

            | Filter (Lambda ty) (Exp ty) ty Loc
             -- 3rd arg is the type of the input (and result) array *)

            | Mapall (Lambda ty) (Exp ty) ty ty Loc
             -- e.g., mapall(op ~, {{1,~2}, {~3,4}}) = {{~1,2}, {3,~4}}                      *)
             -- 3rd and 4th args are the types of the input and result arrays, respectively. *)

            | Redomap (Lambda ty) (Lambda ty) (Exp ty) (Exp ty) ty ty Loc
             -- redomap(g, f, n, a) = reduce(g, n, map(f, a))    *)
             -- 5th arg is the type of the input  array *)
             -- 6th arg is the type of the result array *)

            | Split (Exp ty) (Exp ty) ty Loc
             -- split(2, { 1, 2, 3, 4 }) = {{1},{2, 3, 4}} *)
             -- 3rd arg is the type of the input array *)

            | Concat (Exp ty) (Exp ty) ty Loc
             -- concat ({1},{2, 3, 4}) = {1, 2, 3, 4} *)
             -- 3rd arg is the type of the input array*)

            -- IO
            | Read Type Loc
             -- e.g., read(int); 1st arg is a basic-type, i.e., of the to-be-read element *)

            | Write (Exp ty) ty Loc
             -- e.g., write(map(f, replicate(3,1))) writes array {f(1),f(1),f(1)} *)
             -- 2nd arg is the type of the to-be-written expression *)
            | DoLoop String (Exp ty) (Exp ty) [String] Loc
              deriving (Typeable, Data)

instance Located (Exp ty) where
  locOf (Literal val) = locOf val
  locOf (TupLit _ _ pos) = pos
  locOf (ArrayLit _ _ pos) = pos
  locOf (BinOp _ _ _ _ pos) = pos
  locOf (And _ _ pos) = pos
  locOf (Or _ _ pos) = pos
  locOf (Not _ pos) = pos
  locOf (Negate _ _ pos) = pos
  locOf (If _ _ _ _ pos) = pos
  locOf (Var _ _ pos) = pos
  locOf (Apply _ _ _ pos) = pos
  locOf (LetPat _ _ _ pos) = pos
  locOf (LetWith _ _ _ _ _ pos) = pos
  locOf (Index _ _ _ _ pos) = pos
  locOf (Iota _ pos) = pos
  locOf (Size _ pos) = pos
  locOf (Replicate _ _ _ pos) = pos
  locOf (Reshape _ _ _ _ pos) = pos
  locOf (Transpose _ _ _ pos) = pos
  locOf (Map _ _ _ _ pos) = pos
  locOf (Reduce _ _ _ _ pos) = pos
  locOf (ZipWith _ _ _ pos) = pos
  locOf (Zip _ pos) = pos
  locOf (Unzip _ _ pos) = pos
  locOf (Scan _ _ _ _ pos) = pos
  locOf (Filter _ _ _ pos) = pos
  locOf (Mapall _ _ _ _ pos) = pos
  locOf (Redomap _ _ _ _ _ _ pos) = pos
  locOf (Split _ _ _ pos) = pos
  locOf (Concat _ _ _ pos) = pos
  locOf (Read _ pos) = pos
  locOf (Write _ _ pos) = pos
  locOf (DoLoop _ _ _ _ pos) = pos

-- | Given an expression with known types, return its type.
expType :: Exp Type -> Type
expType (Literal val) = valueType val
expType (TupLit _ t _) = t
expType (ArrayLit _ t pos) = Array t Nothing pos
expType (BinOp _ _ _ t _) = t
expType (And _ _ pos) = Bool pos
expType (Or _ _ pos) = Bool pos
expType (Not _ pos) = Bool pos
expType (Negate _ t _) = t
expType (If _ _ _ t _) = t
expType (Var _ t _) = t
expType (Apply _ _ t _) = t
expType (LetPat _ _ body _) = expType body
expType (LetWith _ _ _ _ body _) = expType body
expType (Index _ _ _ t _) = t
expType (Iota _ pos) = Array (Int pos) Nothing pos
expType (Size _ pos) = Int pos
expType (Replicate _ _ t _) = t
expType (Reshape _ _ _ t _) = t
expType (Transpose _ _ t _) = t
expType (Map _ _ _ t _) = t
expType (Reduce fun _ _ _ _) = lambdaType fun
expType (ZipWith _ _ t _) = t
expType (Zip es pos) = Tuple (map expType es) pos
expType (Unzip _ ts pos) = Tuple ts pos
expType (Scan fun _ _ _ _) = arrayType 1 $ lambdaType fun
expType (Filter _ _ t _) = t
expType (Mapall _ _ _ t _) = t
expType (Redomap _ _ _ _ _ t _) = t
expType (Split _ _ t _) = t
expType (Concat _ _ t _) = t
expType (Read t _) = boxType t
expType (Write _ t _) = t
expType (DoLoop _ _ body _ _) = expType body

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
             deriving (Enum, Bounded, Typeable, Data)

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

-- | Anonymous Function
data Lambda ty = AnonymFun [(String,Type)] (Exp ty) Type Loc
                    -- fn int (bool x, char z) => if(x) then ord(z) else ord(z)+1 *)
               | CurryFun String [Exp ty] ty Loc
                    -- op +(4) *)
                 deriving (Typeable, Data)

-- | The return type of a lambda function.
lambdaType :: Lambda Type -> Type
lambdaType (AnonymFun _ _ t _) = t
lambdaType (CurryFun _ _ t _) = t

-- | Tuple Identifier, i.e., pattern matching
data TupIdent ty = TupId [TupIdent ty] Loc
                 | Id String ty Loc
                   deriving (Typeable, Data)

instance Located (TupIdent ty) where
  locOf (TupId _ loc) = loc
  locOf (Id _ _ loc) = loc

-- | Function Declarations
type Binding = (String,Type)

type FunDec ty = (String,Type,[Binding],Exp ty,Loc)

type Prog ty = [FunDec ty]

-- Pretty-Printing Functionality

spaces :: Int -> String
spaces n = replicate n ' '

ppError :: Loc -> String -> a
ppError loc msg =
  error $ "Prettyprinting error: " ++ msg ++
          "\nAt " ++ locStr loc

-- | Pretty printing a value.
ppValue :: Value -> String
ppValue (IntVal n _)      = show n
ppValue (RealVal n _)     = show n
ppValue (LogVal b _)      = show b
ppValue (CharVal c _)     = show c
ppValue (ArrayVal arr _ _)
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
ppExp d (TupLit es _ _) =
  " ( " ++ intercalate ", " (map (ppExp d) es) ++ " ) "
ppExp _ (Var   var _ _)    = var

ppExp d (BinOp op e1 e2 _ _) = " ( " ++ ppExp d e1 ++ ppBinOp op ++ ppExp d e2 ++ " ) "
ppExp d (And   e1 e2 _  ) = " ( " ++ ppExp d e1 ++ " && " ++ ppExp d e2 ++ " ) "
ppExp d (Or    e1 e2 _  ) = " ( " ++ ppExp d e1 ++ " || " ++ ppExp d e2 ++ " ) "

ppExp d (Not   e _      ) = " ( " ++ "not " ++ ppExp d e ++ " ) "
ppExp d (Negate e _ _   ) = " ( " ++ "~ " ++ ppExp d e ++   " ) "

ppExp d (If    e1 e2 e3 _ _)  =
  "\n" ++
  spaces (d+1) ++ "if( " ++ ppExp d e1 ++ " )\n" ++
  spaces (d+2) ++ "then " ++ ppExp (d+2) e2 ++ "\n" ++
  spaces (d+2) ++ "else " ++ ppExp (d+2) e3 ++ "\n" ++
  spaces d

ppExp _ (Apply f [] _ _)    = f ++ "() "
ppExp d (Apply f args _ _)  =
  f ++ "( " ++ intercalate ", " (map (ppExp d) args) ++ " ) "

ppExp d (LetPat tupid e1 body _) =
        "\n" ++ spaces (d+1) ++ "let " ++ ppTupId tupid ++ " = " ++ ppExp (d+2) e1 ++
        " in  " ++ ppExp (d+2) body
ppExp d (LetWith name e1 es el e2 _) =
      let isassign = case e1 of
                       Var id1 _ _ -> id1 == name
                       _           -> False
      in if isassign
         then
              "\n" ++ spaces(d+1) ++ "let " ++ name ++ "[ " ++
              intercalate ", " (map (ppExp d) es) ++
              "] = " ++ ppExp d el ++ " in  " ++ ppExp (d+2) e2
         else
              "\n" ++ spaces(d+1) ++ "let " ++ name ++ " = " ++ ppExp (d+2) e1 ++
              " with [ " ++ intercalate ", " (map (ppExp d) es) ++
              "] <- " ++ ppExp d el ++ " in  " ++ ppExp (d+2) e2

ppExp d (Index name [e] _ _ _) = name ++ "[ " ++ ppExp d e ++ " ]"
ppExp d (Index name (e:es) _ _ _) =
  name ++ "[ " ++ ppExp d e ++ intercalate ", " (map (ppExp d) es) ++ " ]"
ppExp _ (Index _ [] _ _ pos) = ppError pos "ppExp found empty index!"

-- | Array Constructs
ppExp d (Iota e _)         = "iota ( " ++ ppExp d e ++ " ) "
ppExp d (Size e _)         = "size ( " ++ ppExp d e ++ " ) "
ppExp d (Replicate e el _ _) = "replicate ( " ++ ppExp d e ++ ", " ++ ppExp d el ++ " ) "

ppExp d (Transpose e _ _ _) = " transpose ( " ++ ppExp d e ++ " ) "

ppExp _ (Reshape [] _ _ _ pos) = ppError pos "Empty new shape reshape!"
ppExp d (Reshape (e:es) arr _ _ _) =
  " reshape ( ( " ++ intercalate ", " (map (ppExp d) (e:es)) ++ " ), "  ++
  ppExp d arr ++ " ) "

ppExp d (Map fun e _ _ _) = " map ( " ++ ppLambda fun ++ ", " ++ ppExp d e ++ " ) "

ppExp _ (ZipWith _ [] _ pos) =
  ppError pos "empty expression list for zipWith!"
ppExp d (ZipWith fun (e:es) _ _) =
  " zipWith ( " ++ ppLambda fun ++ ", " ++ ppExp d e ++
  concatMap (\x -> ", " ++ ppExp d x) es ++ " ) "

ppExp d (Zip es _) =
  " zip ( " ++ intercalate "," (map (ppExp d) es) ++ " ) "

ppExp d (Unzip e _ _) = " zip ( " ++ ppExp d e ++ " ) "

ppExp d (Reduce fun el lst _ _) =
  " reduce ( " ++ ppLambda fun ++ ", " ++ ppExp d el ++ ", " ++ ppExp d lst ++ " ) "
ppExp d (Scan  fun el lst _ _) =
  " scan ( " ++ ppLambda fun ++ ", " ++ ppExp d el ++ ", " ++ ppExp d lst ++ " ) "
ppExp d (Filter fun a _ _) =
  " filter ( " ++ ppLambda fun ++ ", " ++ ppExp d a ++ " ) "
ppExp d (Mapall fun a _ _ _)
          = " mapall ( " ++ ppLambda fun ++ ", " ++ ppExp d a ++ " ) "
ppExp d (Redomap id1 id2 el a _ _ _)
          = " redomap ( " ++ ppLambda id1 ++ ", " ++ ppLambda id2 ++ ", " ++ ppExp d el ++ ", " ++ ppExp d a ++ " ) "

ppExp d (Split  idx arr _ _) = " split ( " ++ ppExp d idx ++ ", " ++ ppExp d arr ++ " ) "
ppExp d (Concat a1  a2 _ _) = " concat ( " ++ ppExp d a1 ++ ", " ++ ppExp d a2 ++ " ) "

ppExp _ (Read t _) = " read("  ++ ppType t  ++ ") "
ppExp d (Write e _ _) = " write("  ++ ppExp d e  ++ ") "
ppExp d (DoLoop i n iter mvs _) =
              "\n" ++ spaces(d+1) ++ "for " ++ i ++ " < " ++ ppExp d n ++ " do " ++
              "\n" ++ spaces(d+2) ++ ppExp d iter ++ "\n" ++ spaces(d+1) ++
              "merge " ++ mvs'
  where mvs' = case mvs of [v] -> show v
                           _   -> "( " ++ intercalate ", " mvs ++ " )"

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
ppType (Tuple (tp:tps) _) = "( " ++ intercalate " * " (map ppType (tp:tps)) ++ " ) "
ppType (Tuple [] pos) = ppError pos "Empty tuple"

-- | Pretty printing a tuple id
ppTupId :: TupIdent ty -> String
ppTupId (Id name _ _) = " " ++ name ++ " "
ppTupId (TupId (a:lst) _) = " ( " ++ intercalate ", " (map ppTupId $ a:lst) ++ " ) "
ppTupId (TupId _ pos) = ppError pos "Tuple identifiers with less than two elements "

-- pretty printing Lambda, i.e., curried and unnamed functions *)
ppLambda :: Lambda ty -> String
ppLambda ( AnonymFun (a:rest) body rtp _) =
      let pp_bd (arg, tp) = ppType tp ++ " " ++ arg
          strargs = pp_bd a ++ concatMap (\x -> ", " ++ pp_bd x) rest
      in " fn " ++ ppType rtp ++ " ( " ++ strargs ++ " ) " ++ " => " ++ ppExp 0 body
ppLambda (AnonymFun [] _ _ pos) =
      ppError pos "Anonymous function with zero params!"
ppLambda ( CurryFun fid [] _ _) = fid
ppLambda ( CurryFun fid args ty pos) =
      ppExp 0 (Apply fid args ty pos)

-- | pretty printing a function declaration
ppFun :: Int -> FunDec ty -> String
ppFun d (name, ret_tp, args, body, _) =
  let -- pretty printing a list of bindings separated by commas
      ppBd (argname, tp) = ppType tp ++ " " ++ argname
      pp_bindings = intercalate "," . map ppBd

  in "\n\nfun " ++ ppType ret_tp ++ name ++
     "( " ++ pp_bindings args ++ ") = \n" ++
     spaces (d+1) ++ ppExp (d+1) body

-- | Pretty printing a program.
prettyPrint :: Prog ty -> String
prettyPrint p = concatMap (ppFun 0) p ++ "\n"
