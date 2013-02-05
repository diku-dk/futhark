-- | This Is an Ever-Changing AnSyn for L0.  Some types, such as
-- @Exp@, are parametrised by a functor for keeping type information.
-- See "L0.TypeChecker" and the 'Exp' type for more information.
module L0.AbSyn
  ( Pos
  , posStr
  , Type(..)
  , typePos
  , ppType
  , arrayDims
  , arrayShape
  , peelArray
  , baseType
  , array
  , Value(..)
  , valueType
  , ppValue
  , Exp(..)
  , expPos
  , ppExp
  , BinOp(..)
  , opStr
  , ppBinOp
  , Lambda(..)
  , TupIdent(..)
  , patPos
  , ppTupId
  , Binding
  , FunDec
  , Prog
  , prettyPrint
  )
  where

import Data.List

-- | position: (line, column)
type Pos = (Int,Int)

-- | Convert a 'Pos' into a human-readable position description.
posStr :: Pos -> String
posStr (0,0) = "<builtin>"
posStr (line,col) = show line ++ ":" ++ show col

-- | L0 Types: Int, Bool, Char, Tuple, multidim-regular Array
--  TODO: please add float, double, long int, etc.
data Type = Int Pos
          | Bool Pos
          | Char Pos
          | Real Pos
          | Tuple [Type] Pos
          | Array Type (Maybe (Exp Maybe)) Pos -- ^ 1st arg: array's type, 2nd arg: its length

instance Eq Type where
  Int _ == Int _ = True
  Bool _ == Bool _ = True
  Char _ == Char _ = True
  Real _ == Real _ = True
  Tuple ts1 _ == Tuple ts2 _ = ts1 == ts2
  Array t1 _ _ == Array t2 _ _ = t1 == t2
  _ == _ = False

-- | Return the position information associated with a type.
typePos :: Type -> Pos
typePos (Int pos) = pos
typePos (Bool pos) = pos
typePos (Char pos) = pos
typePos (Real pos) = pos
typePos (Tuple _ pos) = pos
typePos (Array _ _ pos) = pos

-- | Return the dimensionality of a type.  For non-arrays, this is
-- zero.  For a one-dimensional array it is one, for a two-dimensional
-- it is two, and so forth.
arrayDims :: Type -> Int
arrayDims (Array t _ _) = 1 + arrayDims t
arrayDims _             = 0

-- | Return a list of the dimensions of an array (the shape, in other
-- terms).  For non-arrays, this is the empty list.  A two-dimensional
-- array with five rows and three columns would return the list @[5,
-- 3]@.
arrayShape :: Value -> [Int]
arrayShape (ArrayVal [] _ _) = [0]
arrayShape (ArrayVal (e:els) _ _) = length (e:els) : arrayShape e
arrayShape _ = []

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

-- | @array n t@ is the type of @n@-dimensional arrays having @t@ as
-- the base type.
array :: Int -> Type -> Type
array 0 t = t
array n t = array (n-1) $ Array t Nothing (typePos t)

-- | Every possible value in L0.  Values are fully evaluated and their
-- type is always unambiguous.
data Value = IntVal Int Pos
           | RealVal Double Pos
           | LogVal Bool Pos
           | CharVal Char Pos
           | TupVal [Value] Pos
           | ArrayVal [Value] Type Pos

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

-- | L0 Expression Language: literals + vars + int binops + array
-- constructors + array combinators (SOAC) + if + function calls +
-- let + tuples (literals & identifiers) TODO: please add float,
-- double, long int, etc.
--
-- In a value of type @Exp tf@, all 'Type' values are kept as @tf
-- Type@ values.  That is, @tf@ is a functor that is applied to
-- 'Type'.  This allows us to encode whether or not the expression has
-- been type-checked in the Haskell type of the expression.
-- Specifically, the parser will produce expressions of type @Exp
-- 'Maybe'@, and the type checker will convert these to @Exp
-- 'Control.Monad.Identity.Identity'@, in which type information is
-- always present.
data Exp tf = Literal Value
            | TupLit    [Exp tf] (tf Type) Pos -- Tuple and Arrays Literals
                                                  -- e.g., (1+3, (x, y+z))
                                                  -- 2nd argument is the tuple's type
            | ArrayLit  [Exp tf] (tf Type) Pos -- e.g., { {1+x, 3}, {2, 1+4} }
                                                  -- 2nd arg is the array's type
                                                  -- 3rd arg is a list containing the
                                                  -- dimensions' lengths, e.g., [2,2]
            | BinOp BinOp (Exp tf) (Exp tf) (tf Type) Pos
            -- Binary Ops for Booleans
            | And    (Exp tf) (Exp tf) Pos
            | Or     (Exp tf) (Exp tf) Pos
            -- Unary Ops: Not for bools and Negate for ints
            | Not    (Exp tf) Pos -- e.g., not True = False
            | Negate (Exp tf) (tf Type) Pos -- e.g., ~(~1) = 1
            | If     (Exp tf) (Exp tf) (Exp tf) (tf Type) Pos
            | Var    String (tf Type) Pos
            -- Function Call and Let Construct
            | Apply  String [Exp tf] (tf Type) Pos
            | Let    TupIdent (Exp tf) (Maybe [Exp tf]) (Maybe (Exp tf)) (Exp tf) Pos -- e.g., let (x, (y,z)) = (1, (2+4,5)) in  x + y + z
                                                                    -- or,   let x = replicate(3, iota(3)) with [0,0] to 33
            -- Array Indexing and Array Constructors
            | Index String [Exp tf] (tf Type) (tf Type) Pos
             -- e.g., arr[3]; 3rd arg is the input-array element type
             -- 4th arg is the result type
            | Iota (Exp tf) Pos -- e.g., iota(n) = {0,1,..,n-1}

            | Replicate (Exp tf) (Exp tf) (tf Type) Pos -- e.g., replicate(3,1) = {1, 1, 1}
                                                    -- Type is the output-array type

            | Reshape [Exp tf] (Exp tf) (tf Type) (tf Type) Pos
             -- 1st arg is the new shape, 2nd arg is the input array *)
             -- 3rd arg is the  input-array type *)
             -- 4th arg is the result-array type *)

            | Transpose (Exp tf) (tf Type) (tf Type) Pos
             -- 1st arg is the (input) to-be-transPosed array *)
             -- 2nd argument is the  input-array type *)
             -- 3rd argument is the result-array type *)

            -- Second-Order Array Combinators
            -- accept curried and anonymous
            -- functions as (first) params
            | Map (Lambda tf) (Exp tf) (tf Type) (tf Type) Pos
             -- e.g., map(op +(1), {1,2,..,n}) = {2,3,..,n+1} *)
             -- 3st arg is the input-array  type *)
             -- 4th arg is the output-array type *)

            | Reduce (Lambda tf) (Exp tf) (Exp tf) (tf Type) Pos
             -- e.g., reduce(op +, 0, {1,2,..,n}) = (0+1+2+..+n) *)
             -- 4th arg is the input-array type                  *)

            | ZipWith (Lambda tf) [Exp tf] (tf [Type]) (tf Type) Pos
             -- zipWith(plus, {1,2,3}, {4,5,6}) == {5, 7, 9}       *)
             -- 3rd arg is a list of the types of the input arrays *)
             -- 4th arg is the type of the result array            *)

            | Scan (Lambda tf) (Exp tf) (Exp tf) (tf Type) Pos
             -- scan(plus, 0, { 1, 2, 3 }) = { 0, 1, 3, 6 } *)
             -- 4th arg is the type of the input array      *)

            | Filter (Lambda tf) (Exp tf) (tf Type) Pos
             -- 3rd arg is the type of the input (and result) array *)

            | Mapall (Lambda tf) (Exp tf) (tf Type) (tf Type) Pos
             -- e.g., mapall(op ~, {{1,~2}, {~3,4}}) = {{~1,2}, {3,~4}}                      *)
             -- 3rd and 4th args are the types of the input and result arrays, respectively. *)

            | Redomap (Lambda tf) (Lambda tf) (Exp tf) (Exp tf) (tf Type) (tf Type) Pos
             -- redomap(g, f, n, a) = reduce(g, n, map(f, a))    *)
             -- 5th arg is the type of the input  array *)
             -- 6th arg is the type of the result array *)

            | Split (Exp tf) (Exp tf) (tf Type) Pos
             -- split(2, { 1, 2, 3, 4 }) = {{1},{2, 3, 4}} *)
             -- 3rd arg is the type of the input array *)

            | Concat (Exp tf) (Exp tf) (tf Type) Pos
             -- concat ({1},{2, 3, 4}) = {1, 2, 3, 4} *)
             -- 3rd arg is the type of the input array*)

            -- IO
            | Read Type Pos
             -- e.g., read(int); 1st arg is a basic-type, i.e., of the to-be-read element *)

            | Write (Exp tf) (tf Type) Pos
             -- e.g., write(map(f, replicate(3,1))) writes array {f(1),f(1),f(1)} *)
             -- 2nd arg is the type of the to-be-written expression *)

            | DoLoop String (Exp tf) (Exp tf) [String] Pos

expPos :: Exp tf -> Pos
expPos (Literal val) = valuePos val
  where valuePos (IntVal _ pos) = pos
        valuePos (RealVal _ pos) = pos
        valuePos (CharVal _ pos) = pos
        valuePos (LogVal _ pos) = pos
        valuePos (TupVal _ pos) = pos
        valuePos (ArrayVal _ _ pos) = pos
expPos (TupLit _ _ pos) = pos
expPos (ArrayLit _ _ pos) = pos
expPos (BinOp _ _ _ _ pos) = pos
expPos (And _ _ pos) = pos
expPos (Or _ _ pos) = pos
expPos (Not _ pos) = pos
expPos (Negate _ _ pos) = pos
expPos (If _ _ _ _ pos) = pos
expPos (Var _ _ pos) = pos
expPos (Apply _ _ _ pos) = pos
expPos (Let _ _ _ _ _ pos) = pos
expPos (Index _ _ _ _ pos) = pos
expPos (Iota _ pos) = pos
expPos (Replicate _ _ _ pos) = pos
expPos (Reshape _ _ _ _ pos) = pos
expPos (Transpose _ _ _ pos) = pos
expPos (Map _ _ _ _ pos) = pos
expPos (Reduce _ _ _ _ pos) = pos
expPos (ZipWith _ _ _ _ pos) = pos
expPos (Scan _ _ _ _ pos) = pos
expPos (Filter _ _ _ pos) = pos
expPos (Mapall _ _ _ _ pos) = pos
expPos (Redomap _ _ _ _ _ _ pos) = pos
expPos (Split _ _ _ pos) = pos
expPos (Concat _ _ _ pos) = pos
expPos (Read _ pos) = pos
expPos (Write _ _ pos) = pos
expPos (DoLoop _ _ _ _ pos) = pos

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
             deriving (Enum, Bounded)

-- ^ Print the operator, without whitespace, that corresponds to this
-- @BinOp@.
opStr :: BinOp -> String
opStr Plus = "+"
opStr Minus = "-"
opStr Pow = "^"
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
data Lambda tf = AnonymFun [(String,Type)] (Exp tf) Type Pos
                    -- fn int (bool x, char z) => if(x) then ord(z) else ord(z)+1 *)
               | CurryFun String [Exp tf] (tf [Type]) (tf Type) Pos
                    -- op +(4) *)

-- | Tuple Identifier, i.e., pattern matching
data TupIdent = TupId [TupIdent] Pos
              | Id String Pos

patPos :: TupIdent -> Pos
patPos (TupId _ pos) = pos
patPos (Id _ pos) = pos

-- | Function Declarations
type Binding = (String,Type)

type FunDec tf = (String,Type,[Binding],Exp tf,Pos)

type Prog tf = [FunDec tf]


-- Pretty-Printing Functionality

spaces :: Int -> String
spaces n = replicate n ' '

ppError :: Pos -> String -> a
ppError (line,col) msg =
  error $ "Prettyprinting error: " ++ msg ++
          "\nAT position " ++ show line ++ ":" ++ show col

-- | Pretty printing a value.
ppValue :: Value -> String
ppValue (IntVal n _)      = show n
ppValue (RealVal n _)     = show n
ppValue (LogVal b _)      = show b
ppValue (CharVal c _)     = show c
ppValue (ArrayVal vs _ _)
  | Just (c:cs) <- mapM char vs = show $ c:cs
  | otherwise = " { " ++ intercalate ", " (map ppValue vs) ++ " } "
    where char (CharVal c _) = Just c
          char _             = Nothing
ppValue (TupVal vs _)   =
  " ( " ++ intercalate ", " (map ppValue vs) ++ " ) "

-- | Pretty printing an expression
ppExp :: Int -> Exp tf -> String
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

ppExp d (Let tupid e1 Nothing Nothing e2 _) =
              "\n" ++ spaces (d+1) ++ "let " ++ ppTupId tupid ++ " = " ++ ppExp (d+2) e1 ++
              " in  " ++ ppExp (d+2) e2
ppExp d (Let (Id name _) e1 (Just (e:es)) (Just el) e2 _) =
      let isassign = case e1 of
                       Var id1 _ _ -> id1 == name
                       _           -> False
      in if isassign
         then
              "\n" ++ spaces(d+1) ++ "let " ++ name ++ "[ " ++
              intercalate ", " (map (ppExp d) (e:es)) ++
              "] = " ++ ppExp d el ++ " in  " ++ ppExp (d+2) e2
         else
              "\n" ++ spaces(d+1) ++ "let " ++ name ++ " = " ++ ppExp (d+2) e1 ++
              " with [ " ++ ppExp d e ++ intercalate ", " (map (ppExp d) es) ++
              "] <- " ++ ppExp d el ++ " in  " ++ ppExp (d+2) e2
ppExp _ (Let _ _ _ _ _ pos) = ppError pos "ppExp found illegal let expression!"

ppExp d (Index name [e] _ _ _) = name ++ "[ " ++ ppExp d e ++ " ]"
ppExp d (Index name (e:es) _ _ _) =
  name ++ "[ " ++ ppExp d e ++ intercalate ", " (map (ppExp d) es) ++ " ]"
ppExp _ (Index _ [] _ _ pos) = ppError pos "ppExp found empty index!"

ppExp _ (DoLoop _ _ _ [] pos) = ppError pos "Empty merge list for the DO construct Error!"
ppExp d (DoLoop i n iter (v:ms) _) =
              "\n" ++ spaces(d+1) ++ "for " ++ i ++ " < " ++ ppExp d n ++ " do " ++
              "\n" ++ spaces(d+2) ++ ppExp d iter ++ "\n" ++ spaces(d+1) ++
              "merge " ++ intercalate ", " (v:ms)

-- | Array Constructs
ppExp d (Iota e _)         = "iota ( " ++ ppExp d e ++ " ) "
ppExp d (Replicate e el _ _) = "replicate ( " ++ ppExp d e ++ ", " ++ ppExp d el ++ " ) "

ppExp d (Transpose e _ _ _) = " transpose ( " ++ ppExp d e ++ " ) "

ppExp _ (Reshape [] _ _ _ pos) = ppError pos "Empty new shape reshape!"
ppExp d (Reshape (e:es) arr _ _ _) =
  " reshape ( ( " ++ intercalate ", " (map (ppExp d) (e:es)) ++ " ), "  ++
  ppExp d arr ++ " ) "

ppExp d (Map fun e _ _ _) = " map ( " ++ ppLambda fun ++ ", " ++ ppExp d e ++ " ) "

ppExp _ (ZipWith _ [] _ _ pos) =
  ppError pos "empty expression list for zipWith!"
ppExp d (ZipWith fun (e:es) _ _ _) =
  " zipWith ( " ++ ppLambda fun ++ ", " ++ ppExp d e ++
  concatMap (\x -> ", " ++ ppExp d x) es ++ " ) "

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
ppTupId :: TupIdent -> String
ppTupId (Id name _) = " " ++ name ++ " "
ppTupId (TupId (a:lst) _) = " ( " ++ intercalate ", " (map ppTupId $ a:lst) ++ " ) "
ppTupId (TupId _ pos) = ppError pos "Tuple identifiers with less than two elements "

-- pretty printing Lambda, i.e., curried and unnamed functions *)
ppLambda :: Lambda tf -> String
ppLambda ( AnonymFun (a:rest) body rtp _) =
      let pp_bd (arg, tp) = ppType tp ++ " " ++ arg
          strargs = pp_bd a ++ concatMap (\x -> ", " ++ pp_bd x) rest
      in " fn " ++ ppType rtp ++ " ( " ++ strargs ++ " ) " ++ " => " ++ ppExp 0 body
ppLambda (AnonymFun [] _ _ pos) =
      ppError pos "Anonymous function with zero params!"
ppLambda ( CurryFun fid [] _ _ _) = fid
ppLambda ( CurryFun fid args _ ty pos) =
      ppExp 0 (Apply fid args ty pos)

-- | pretty printing a function declaration
ppFun :: Int -> FunDec Maybe -> String
ppFun d (name, ret_tp, args, body, _) =
  let -- pretty printing a list of bindings separated by commas
      ppBd (argname, tp) = ppType tp ++ " " ++ argname
      pp_bindings = intercalate "," . map ppBd

  in "\n\nfun " ++ ppType ret_tp ++ name ++
     "( " ++ pp_bindings args ++ ") = \n" ++
     spaces (d+1) ++ ppExp (d+1) body

-- | Pretty printing a program.
prettyPrint :: Prog Maybe -> String
prettyPrint p = concatMap (ppFun 0) p ++ "\n"
