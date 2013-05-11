{-# LANGUAGE FlexibleInstances #-}
-- | This Is an Ever-Changing AnSyn for L0.  Some types, such as
-- @Exp@, are parametrised by type representation.
-- See "L0.TypeChecker" and the 'Exp' type for more information.
module L0.AbSyn
  ( locStr
  , Name
  , nameToString
  , nameFromString
  , defaultEntryPoint
  , Uniqueness(..)
  , Type(..)
  , subtypeOf
  , similarTo
  , uniqueness
  , unique
  , TypeBox(..)
  , Typed(..)
  , ppType
  , arrayDims
  , arrayShape
  , arraySize
  , peelArray
  , baseType
  , basicType
  , arrayType
  , stripArray
  , blankValue
  , Value(..)
  , arrayVal
  , emptyArray
  , arrayString
  , flattenArray
  , ppValue
  , Ident(..)
  , Exp(..)
  , expToValue
  , ppExp
  , isBuiltInFun
  , BinOp(..)
  , opStr
  , ppBinOp
  , Lambda(..)
  , TupIdent(..)
  , ppTupId
  , FunDec
  , Prog
  , funDecByName
  , prettyPrint
  )
  where

import Data.Array
import Data.List
import Data.Loc
import Data.Monoid
import qualified Data.Text as T

-- | The abstract (not really) type representing names in the L0
-- compiler.  'String's, being lists of characters, are very slow,
-- while 'T.Text's are based on byte-arrays.
type Name = T.Text

-- | Convert a name to the corresponding list of characters.
nameToString :: Name -> String
nameToString = T.unpack

-- | Convert a list of characters to the corresponding name.
nameFromString :: String -> Name
nameFromString = T.pack

-- | The name of the default program entry point (main).
defaultEntryPoint :: Name
defaultEntryPoint = nameFromString "main"

isBuiltInFun :: Name -> Bool
isBuiltInFun fnm = fnm `elem` builtins
  where builtins = map nameFromString ["toReal", "trunc", "sqrt", "log", "exp", "trace", "assertZip"]

locStr :: SrcLoc -> String
locStr (SrcLoc NoLoc) = "unknown location"
locStr (SrcLoc (Loc (Pos file line1 col1 _) (Pos _ line2 col2 _))) =
  -- Assume that both positions are in the same file (what would the
  -- alternative mean?)
  file ++ ":" ++ show line1 ++ ":" ++ show col1
       ++ "-" ++ show line2 ++ ":" ++ show col2

data Uniqueness = Unique | Nonunique
                  deriving (Eq, Ord, Show)

instance Monoid Uniqueness where
  mempty = Unique
  _ `mappend` Nonunique = Nonunique
  Nonunique `mappend` _ = Nonunique
  u `mappend` _         = u

-- | L0 Types: Int, Bool, Char, Tuple, multidim-regular Array
--  TODO: please add float, double, long int, etc.
data Type = Int
          | Bool
          | Char
          | Real
          | Tuple [Type]
          | Array Type (Maybe (Exp (Maybe Type))) Uniqueness -- ^ 1st arg: array's element type, 2nd arg: its length
            deriving (Eq, Ord, Show)

-- | Return the dimensionality of a type.  For non-arrays, this is
-- zero.  For a one-dimensional array it is one, for a two-dimensional
-- it is two, and so forth.
arrayDims :: Type -> Int
arrayDims (Array t _ _) = 1 + arrayDims t
arrayDims _             = 0

-- | @x `subuniqueOf` y@ is true if @x@ is not less unique than @y@.
subuniqueOf :: Uniqueness -> Uniqueness -> Bool
subuniqueOf Nonunique Unique = False
subuniqueOf _ _ = True

-- | @x \`subtypeOf\` y@ is true if @x@ is a subtype of @y@ (or equal to
-- @y@), meaning @x@ is valid whenever @y@ is.
subtypeOf :: Type -> Type -> Bool
subtypeOf (Array t1 _ u1) (Array t2 _ u2) =
  u1 `subuniqueOf` u2 && t1 `subtypeOf` t2
subtypeOf (Tuple ts1) (Tuple ts2) =
  and $ zipWith subtypeOf ts1 ts2
subtypeOf t1 t2 = t1 == t2

-- | @x \`similarTo\` y@ is true if @x@ and @y@ are the same type,
-- ignoring uniqueness.
similarTo :: Type -> Type -> Bool
similarTo t1 t2 = t1 `subtypeOf` t2 || t2 `subtypeOf` t1

-- | Return the uniqueness of a type.
uniqueness :: Typed a => a -> Uniqueness
uniqueness = uniqueness' . typeOf
  where uniqueness' (Array _ _ u) = u
        uniqueness' _ = Nonunique

-- | @unique t@ is 'True' if the type of the argument is unique.
unique :: Typed a => a -> Bool
unique = (==Unique) . uniqueness

-- | A type box provides a way to box a type, and possibly retrieve
-- one.
class (Eq ty, Ord ty, Show ty) => TypeBox ty where
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
  getExpType = typeOf

-- | A typed value is one from which we can retrieve a type.
class Typed v where
  typeOf :: v -> Type

instance Typed Type where
  typeOf = id

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
basicType (Tuple ts) = all basicType ts
basicType _ = True

uniqueOrBasic :: Typed t => t -> Bool
uniqueOrBasic x = basicType (typeOf x) || unique x

-- | @array n t@ is the type of @n@-dimensional arrays having @t@ as
-- the base type.
arrayType :: Int -> Type -> Uniqueness -> Type
arrayType 0 t _ = t
arrayType n t u = arrayType (n-1) (Array t Nothing u) u

-- | @stripArray n t@ removes the @n@ outermost layers of the array.
-- Essentially, it is the type of indexing an array of type @t@ with
-- @n@ indexes.
stripArray :: Int -> Type -> Type
stripArray 0 t = t
stripArray n (Array t _ _) = stripArray (n-1) t
stripArray _ t = t

withUniqueness :: Type -> Uniqueness -> Type
withUniqueness (Array et dims _) u = Array et dims u
withUniqueness (Tuple ets) u = Tuple $ map (`withUniqueness` u) ets
withUniqueness t _ = t

-- | A "blank" value of the given type - this is zero, or whatever is
-- close to it.  Don't depend on this value, but use it for creating
-- arrays to be populated by do-loops.
blankValue :: Type -> Value
blankValue Int = IntVal 0
blankValue Real = RealVal 0.0
blankValue Bool = LogVal False
blankValue Char = CharVal '\0'
blankValue (Tuple ts) = TupVal (map blankValue ts)
blankValue (Array et _ _) = arrayVal [blankValue et] et

-- | Every possible value in L0.  Values are fully evaluated and their
-- type is always unambiguous.
data Value = IntVal !Int
           | RealVal !Double
           | LogVal !Bool
           | CharVal !Char
           | TupVal ![Value]
           | ArrayVal !(Array Int Value) Type
             -- ^ The type is the element type, not the complete array
             -- type.  It is assumed that the array is 0-indexed.
             deriving (Eq, Ord, Show)

instance Typed Value where
  typeOf (IntVal _) = Int
  typeOf (RealVal _) = Real
  typeOf (LogVal _) = Bool
  typeOf (CharVal _) = Char
  typeOf (TupVal vs) = Tuple (map typeOf vs)
  typeOf (ArrayVal _ t) = Array t Nothing Nonunique

-- | Return a list of the sizes of an array (the shape, in other
-- terms).  For non-arrays, this is the empty list.  A two-dimensional
-- array with five rows and three columns would return the list @[5,
-- 3]@.
arrayShape :: Value -> [Int]
arrayShape (ArrayVal arr t) =
  case elems arr of
    [] -> 0 : replicate (arrayDims t) 0
    (v:_) -> size : arrayShape v
  where size = upper - lower + 1
        (lower, upper) = bounds arr
arrayShape _ = []

-- | Return the size of the first dimension of an array, or zero for
-- non-arrays.
arraySize :: Value -> Int
arraySize t = case arrayShape t of
                []  -> 0
                n:_ -> n

-- | Construct an array value containing the given elements.
arrayVal :: [Value] -> Type -> Value
arrayVal vs = ArrayVal $ listArray (0, length vs-1) vs

emptyArray :: Type -> Value
emptyArray = arrayVal []

-- | If the given value is a nonempty array containing only
-- characters, return the corresponding 'String', otherwise return
-- 'Nothing'.
arrayString :: Value -> Maybe String
arrayString (ArrayVal arr _)
  | c:cs <- elems arr = mapM asChar $ c:cs
  where asChar (CharVal c) = Just c
        asChar _ = Nothing
arrayString _ = Nothing

-- | Given an N-dimensional array, return a one-dimensional array
-- with the same elements.
flattenArray :: Value -> Value
flattenArray (ArrayVal arr t) =
  arrayVal (concatMap flatten $ elems arr) (baseType t)
    where flatten (ArrayVal arr' _) = concatMap flatten $ elems arr'
          flatten v = [v]
flattenArray v = v

-- | An identifier consists of its name and the type of the value
-- bound to the identifier.
data Ident ty = Ident { identName :: Name
                      , identType :: ty
                      , identSrcLoc :: SrcLoc
                      }
                deriving (Eq, Ord, Show)

instance Located (Ident ty) where
  locOf = locOf . identSrcLoc

instance Typed (Ident Type) where
  typeOf = identType

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
data Exp ty = Literal Value SrcLoc
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
            | Apply  Name [Exp ty] ty SrcLoc
            | LetPat (TupIdent ty) (Exp ty) (Exp ty) SrcLoc

            | LetWith (Ident ty) (Ident ty) [Exp ty] (Exp ty) (Exp ty) SrcLoc
            -- ^ Array Indexing and Array Constructors

            | Index (Ident ty) [Exp ty] ty ty SrcLoc
             -- ^ 3rd arg is the input-array element type 4th arg is
             -- the result type

            | Iota (Exp ty) SrcLoc
            -- ^ @iota(n) = {0,1,..,n-1@

            | Size (Exp ty) SrcLoc
            -- ^ The number of elements in an array.

            | Replicate (Exp ty) (Exp ty) SrcLoc
            -- ^ @replicate(3,1) = {1, 1, 1}@

            | Reshape [Exp ty] (Exp ty) SrcLoc
             -- ^ 1st arg is the new shape, 2nd arg is the input array *)

            | Transpose (Exp ty) SrcLoc
             -- ^ 1st arg is the (input) to-be-transSrcLoced array.

            -- Second-Order Array Combinators
            -- accept curried and anonymous
            -- functions as (first) params
            | Map (Lambda ty) (Exp ty) ty ty SrcLoc
             -- @map(op +(1), {1,2,..,n}) = {2,3,..,n+1}@
             -- 3st arg is the input-array element type
             -- 4th arg is the output-array element type

            | Reduce (Lambda ty) (Exp ty) (Exp ty) ty SrcLoc
             -- @reduce(op +, 0, {1,2,..,n}) = (0+1+2+..+n)@
             -- 4th arg is the input-array element type

            | Zip [(Exp ty, ty)] SrcLoc
            -- ^ Normal zip supporting variable number of arguments.
            -- The type paired to each expression is the element type
            -- of the array returned by that expression.

            | Unzip (Exp ty) [ty] SrcLoc
            -- ^ Unzip that can unzip tuples of arbitrary size.  The
            -- types are the elements of the tuple.

            | Scan (Lambda ty) (Exp ty) (Exp ty) ty SrcLoc
             -- ^ @scan(plus, 0, { 1, 2, 3 }) = { 1, 3, 6 }@.
             -- 4th arg is the element type of the input array

            | Filter (Lambda ty) (Exp ty) ty SrcLoc
             -- ^ 3rd arg is the element type of the input (and
             -- result) array

            | Mapall (Lambda ty) (Exp ty) ty ty SrcLoc
             -- ^ @mapall(op ~, {{1,~2}, {~3,4}}) = {{~1,2}, {3,~4}}@.
             -- 3rd and 4th args are the base types of the input and result arrays, respectively.

            | Redomap (Lambda ty) (Lambda ty) (Exp ty) (Exp ty) ty ty SrcLoc
             -- ^ @redomap(g, f, n, a) = reduce(g, n, map(f, a))@.
             -- 5th arg is the element type of the input  array.
             -- 6th arg is the result type == the element type of the reduced array

            | Split (Exp ty) (Exp ty) ty SrcLoc
             -- ^ @split(1, { 1, 2, 3, 4 }) = ({1},{2, 3, 4})@.
             -- 3rd arg is the element type of the input array

            | Concat (Exp ty) (Exp ty) ty SrcLoc
             -- ^ @concat ({1},{2, 3, 4}) = {1, 2, 3, 4}@.
             -- 3rd arg is the element type of the input array*)

            | Copy (Exp ty) SrcLoc
            -- ^ Copy the value return by the expression.  This only
            -- makes a difference in do-loops with merge variables.

            | DoLoop
              (TupIdent ty) -- Merge variable pattern
              (Exp ty) -- Initial values of merge variables.
              (Ident ty) -- Iterator.
              (Exp ty) -- Upper bound.
              (Exp ty) -- Loop body.
              (Exp ty) -- Let-body.
              SrcLoc

            -----------------------------------------------------
            -- Second-Order Array Combinators
            -- with support for n-ary multi-dim 
            -- arrays of BASIC type (i.e., no tuples inside)
            -- accept curried and anonymous
            -- functions as (first) params
            -----------------------------------------------------
            | Map2 (Lambda ty) [Exp ty] ty ty SrcLoc
             -- @map(op +(1), {1,2,..,n}) = {2,3,..,n+1}@
             -- 2nd arg is either a tuple of multi-dim arrays 
             --   of basic type, or a multi-dim array of basic type.
             -- 3st arg is the  input-array element type
             --   (either a tuple or an array)
             -- 4th arg is the output-array element type
             --   (either a tuple or an array)

            | Reduce2 (Lambda ty) (Exp ty) [Exp ty] ty SrcLoc
            | Scan2   (Lambda ty) (Exp ty) [Exp ty] ty SrcLoc
            | Filter2 (Lambda ty) [Exp ty]          ty SrcLoc
            | Mapall2 (Lambda ty) [Exp ty]       ty ty SrcLoc
            | Redomap2(Lambda ty) (Lambda ty) (Exp ty) [Exp ty] ty ty SrcLoc

              
              deriving (Eq, Ord, Show)

instance Located (Exp ty) where
  locOf (Literal _ loc) = locOf loc
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
  locOf (Reshape _ _ pos) = locOf pos
  locOf (Transpose _ pos) = locOf pos
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
  locOf (DoLoop _ _ _ _ _ _ pos) = locOf pos
  -- locOf for soac2 (Cosmin)
  locOf (Map2 _ _ _ _ pos) = locOf pos
  locOf (Reduce2 _ _ _ _ pos) = locOf pos
  locOf (Scan2 _ _ _ _ pos) = locOf pos
  locOf (Filter2 _ _ _ pos) = locOf pos
  locOf (Mapall2 _ _ _ _ pos) = locOf pos
  locOf (Redomap2 _ _ _ _ _ _ pos) = locOf pos

instance Typed (Exp Type) where
  typeOf (Literal val _) = typeOf val
  typeOf (TupLit es _) = Tuple $ map typeOf es
  typeOf (ArrayLit es t _) = arrayType 1 t $ mconcat $ map uniqueness es
  typeOf (BinOp _ _ _ t _) = t
  typeOf (And {}) = Bool
  typeOf (Or {}) = Bool
  typeOf (Not _ _) = Bool
  typeOf (Negate _ t _) = t
  typeOf (If _ _ _ t _) = t
  typeOf (Var ident) = identType ident
  typeOf (Apply _ _ t _) = t
  typeOf (LetPat _ _ body _) = typeOf body
  typeOf (LetWith _ _ _ _ body _) = typeOf body
  typeOf (Index _ _ _ t _) = t
  typeOf (Iota _ _) = arrayType 1 Int Unique
  typeOf (Size _ _) = Int
  typeOf (Replicate _ e _) = arrayType 1 (typeOf e) u
    where u | uniqueOrBasic e = Unique
            | otherwise = Nonunique
  typeOf (Reshape shape e _) = build (length shape) (baseType $ typeOf e)
    where build 0 t = t
          build n t = build (n-1) (Array t Nothing Nonunique)
  typeOf (Transpose e _) = typeOf e
  typeOf (Map _ a _ t _) = arrayType 1 t u
    where u | uniqueOrBasic t, unique a = Unique
            | otherwise = Nonunique
  typeOf (Reduce fun _ _ _ _) = typeOf fun
  typeOf (Zip es _) = arrayType 1 (Tuple $ map snd es) Nonunique
  typeOf (Unzip _ ts _) = Tuple $ map (flip (arrayType 1) Unique) ts
  typeOf (Scan fun e arr _ _) =
    arrayType 1 (typeOf fun) (uniqueness fun <> uniqueness e <> uniqueness arr)
  typeOf (Filter _ _ t _) = arrayType 1 t Nonunique
  typeOf (Mapall fun e _ _ _) = arrayType (arrayDims $ typeOf e) (typeOf fun) Nonunique
  typeOf (Redomap _ _ _ _ _ t _) = t
  typeOf (Split _ _ t _) = Tuple [arrayType 1 t Nonunique, arrayType 1 t Nonunique]
  typeOf (Concat _ _ t _) = arrayType 1 t Nonunique
  typeOf (Copy e _) = typeOf e `withUniqueness` Unique
  typeOf (DoLoop _ _ _ _ _ body _) = typeOf body
--- Begin SOAC2: (Cosmin) ---
  typeOf (Map2 _ _ _ (Tuple tps) _) = Tuple $ map (\x -> arrayType 1 x Unique) tps
  typeOf (Map2 _ _ _ tp _) = arrayType 1 tp Unique
  typeOf (Reduce2 fun _ _ _ _) = typeOf fun
  typeOf (Scan2 _ _ _ (Tuple tps) _) = Tuple $ map (\x -> arrayType 1 x Unique) tps
  typeOf (Scan2 _ _ _ tp _) = arrayType 1 tp Unique
  typeOf (Filter2 _ _ (Tuple tps) _) = Tuple $ map (\x -> arrayType 1 x Unique) tps
  typeOf (Filter2 _ _ tp _) = arrayType 1 tp Unique
  typeOf (Redomap2 redfun _ _ _ _ _ _) = typeOf redfun
  typeOf (Mapall2 fun es _ _ _) =
      let inpdim= case map typeOf es of
                    et:etps -> foldl min
                               (arrayDims et)
                               (map arrayDims etps)
                    _       -> 0
          fnrtp = typeOf fun
      in case fnrtp of
          Tuple tps -> Tuple $ map (\x -> arrayType inpdim x Unique) tps
          _ -> arrayType inpdim fnrtp Unique
--- End SOAC2: (Cosmin) ---

-- | Eagerly evaluated binary operators.  In particular, the
-- short-circuited operators && and || are not here, although an
-- eagerly evaluated variant is.
data BinOp = Plus -- Binary Ops for Numbers
           | Minus
           | Pow
           | Times
           | Divide
           | Mod
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
             deriving (Eq, Ord, Enum, Bounded, Show)

-- ^ Print the operator, without whitespace, that corresponds to this
-- @BinOp@.
opStr :: BinOp -> String
opStr Plus = "+"
opStr Minus = "-"
opStr Pow = "pow"
opStr Times = "*"
opStr Divide = "/"
opStr Mod = "%"
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
expToValue (Literal val _) = Just val
expToValue (TupLit es _) = do es' <- mapM expToValue es
                              Just $ TupVal es'
expToValue (ArrayLit es et _) = do es' <- mapM expToValue es
                                   Just $ arrayVal es' et
expToValue _ = Nothing

-- | Anonymous Function
data Lambda ty = AnonymFun [Ident Type] (Exp ty) Type SrcLoc
                    -- fn int (bool x, char z) => if(x) then ord(z) else ord(z)+1 *)
               | CurryFun Name [Exp ty] ty SrcLoc
                    -- op +(4) *)
                 deriving (Eq, Ord, Show)

instance Typed (Lambda Type) where
  typeOf (AnonymFun _ _ t _) = t
  typeOf (CurryFun _ _ t _) = t

-- | Tuple Identifier, i.e., pattern matching
data TupIdent ty = TupId [TupIdent ty] SrcLoc
                 | Id (Ident ty)
                   deriving (Eq, Ord, Show)

instance Located (TupIdent ty) where
  locOf (TupId _ loc) = locOf loc
  locOf (Id ident) = locOf ident

-- | Function Declarations
type FunDec ty = (Name,Type,[Ident Type],Exp ty,SrcLoc)

-- | An entire L0 program.
type Prog ty = [FunDec ty]

-- | Find the function of the given name in the L0 program.
funDecByName :: Name -> Prog ty -> Maybe (FunDec ty)
funDecByName fname = find (\(fname',_,_,_,_) -> fname == fname')

-- Pretty-Printing Functionality

spaces :: Int -> String
spaces n = replicate (n*2) ' '

tildes :: String -> String
tildes = map tilde
  where tilde '-' = '~'
        tilde c   = c

-- | Pretty printing a value.
ppValue :: Value -> String
ppValue (IntVal n)  = tildes (show n) ++ " "
ppValue (RealVal n) = tildes (show n) ++ " "
ppValue (LogVal b)  = show b ++ " "
ppValue (CharVal c) = show c ++ " "
ppValue v@(ArrayVal arr t)
  | [] <- elems arr = " empty (" ++ ppType t ++ " ) "
  | Just s <- arrayString v = show s
  | otherwise = " { " ++ intercalate ", " (map ppValue $ elems arr) ++ " } "
ppValue (TupVal vs)   =
  " ( " ++ intercalate ", " (map ppValue vs) ++ " ) "

-- | Pretty printing an expression
ppExp :: TypeBox ty => Int -> Exp ty -> String
ppExp _ (Literal val _)     = ppValue val
ppExp d (ArrayLit es _ _) =
  " { " ++ intercalate ", " (map (ppExp d) es) ++ " } "
ppExp d (TupLit es _) =
  " ( " ++ intercalate ", " (map (ppExp d) es) ++ " ) "
ppExp _ (Var ident) = nameToString (identName ident) ++ " "

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

ppExp _ (Apply f [] _ _)    = nameToString f ++ "() "
ppExp d (Apply f args _ _)  =
  nameToString f ++ "( " ++ intercalate ", " (map (ppExp d) args) ++ " ) "

ppExp d (LetPat tupid e1 body _) =
        "\n" ++ spaces d ++ "let " ++ ppTupId tupid ++ " = " ++ ppExp (d+2) e1 ++
        "in  " ++ ppExp d body
ppExp d (LetWith (Ident dest _ _) (Ident src _ _) es el e2 _)
  | dest == src =
    "\n" ++ spaces d ++ "let " ++ nameToString dest ++ "[ " ++
    intercalate ", " (map (ppExp d) es) ++
    "] = " ++ ppExp d el ++ "in  " ++ ppExp d e2
  | otherwise =
    "\n" ++ spaces d ++ "let " ++ nameToString dest ++ " = " ++ nameToString src ++
    " with [ " ++ intercalate ", " (map (ppExp d) es) ++
    "] <- " ++ ppExp d el ++ "in  " ++ ppExp d e2

ppExp d (Index (Ident name _ _) es _ _ _) =
  nameToString name ++ "[ " ++ intercalate ", " (map (ppExp d) es) ++ " ] "

-- | Array Constructs
ppExp d (Iota e _)         = "iota ( " ++ ppExp d e ++ " ) "
ppExp d (Size e _)         = "size ( " ++ ppExp d e ++ " ) "
ppExp d (Replicate e el _) = "replicate ( " ++ ppExp d e ++ ", " ++ ppExp d el ++ " ) "

ppExp d (Transpose e _) = " transpose ( " ++ ppExp d e ++ " ) "

ppExp d (Reshape es arr _) =
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

ppExp d (DoLoop mvpat mvexp i n loopbody letbody _) =
  "\n" ++ spaces d ++ "loop (" ++ ppTupId mvpat ++ " = " ++ ppExp d mvexp ++
    ") = " ++ "for " ++ nameToString (identName i) ++ " < " ++ ppExp d n ++ " do\n" ++
    spaces (d+1) ++ ppExp (d+1) loopbody ++ "\n" ++ spaces d ++
    "in " ++ ppExp d letbody
--- Cosmin added ppExp for soac2
ppExp d (Map2 fun lst _ elrtp _) = 
    let (pref, suff)  = case unboxType elrtp of
                            Just (Tuple {}) -> (" unzip ( ", " ) ")
                            _ -> ("","")
        mid = case lst of
                [x] -> ppExp (d+1) x
                _ -> "zip ( " ++ intercalate ", " (map (ppExp (d+1) ) lst) ++ " ) "
    in pref ++ " map ( " ++ ppLambda (d+1) fun ++ ", " ++ mid ++ suff ++ " ) "
--
ppExp d (Reduce2 fun el [arr] _ _) =
  " reduce ( " ++ ppLambda (d+1) fun ++ ", " ++ ppExp (d+1) el ++ ", " ++ ppExp (d+1) arr ++ " ) "
ppExp d (Reduce2 fun el arrs _ _) =
    " reduce ( " ++ ppLambda (d+1) fun ++ ", " ++ ppExp (d+1) el ++ ", zip ( " ++ 
    intercalate ", " (map (ppExp (d+1)) arrs) ++ " ) ) "
--
ppExp d (Scan2  fun el lst eltp _) =
    let (pref, suff)  = case unboxType eltp of
                            Just (Tuple {}) -> (" unzip ( ", " ) ")
                            _ -> ("","")
        mid = case lst of [x] -> ppExp (d+1) x
                          _ ->  "zip ( " ++ intercalate ", " (map (ppExp (d+1) ) lst) ++ " ) "
    in pref ++ " scan ( " ++ ppLambda (d+1) fun ++ ", " ++ ppExp (d+1) el ++ ", " ++ mid ++ suff ++ " ) "
--
ppExp d (Filter2 fun [a] _ _) =
  " filter ( " ++ ppLambda (d+1) fun ++ ", " ++ ppExp (d+1) a ++ " ) "
ppExp d (Filter2 fun els _ _) =
    " unzip ( filter ( " ++ ppLambda (d+1) fun ++ ", " ++ 
    " zip ( " ++ intercalate ", " (map (ppExp (d+1)) els) ++ " ) ) ) "
--
ppExp d (Redomap2 id1 id2 el [a] _ _ _)
          = " redomap ( " ++ ppLambda (d+1) id1 ++ ", " ++ ppLambda (d+1) id2 ++ 
            ", " ++ ppExp (d+1) el ++ ", " ++ ppExp (d+1) a ++ " ) "
ppExp d (Redomap2 id1 id2 el els _ _ _)
          = " redomap ( " ++ ppLambda (d+1) id1 ++ ", " ++ ppLambda (d+1) id2 ++ 
            ", " ++ ppExp (d+1) el ++ ", zip ( " ++ intercalate ", " (map (ppExp (d+1)) els) ++ " ) ) "
--
ppExp d (Mapall2 fun lst _ _ _) = 
    " mapall2 ( " ++ ppLambda (d+1) fun ++ intercalate ", " (map (ppExp (d+1)) lst) ++ " ) "
--- Cosmin end ppExp for soac2

ppBinOp :: BinOp -> String
ppBinOp op = " " ++ opStr op ++ " "

ppUniqueness :: Uniqueness -> String
ppUniqueness Unique = "*"
ppUniqueness Nonunique = ""

-- | Pretty printing a type
ppType :: Type -> String
ppType Int = " int "
ppType Bool = " bool "
ppType Char = " char "
ppType Real = " real "
ppType (Array tp  Nothing u) = ppUniqueness u ++ "[ " ++ ppType tp ++ " ] "
ppType (Array tp  (Just l) u) = ppUniqueness u ++ "[ " ++ ppType tp ++ ", " ++ ppExp 0 l ++ " ] "
ppType (Tuple tps) = "( " ++ intercalate ", " (map ppType tps) ++ " ) "

-- | Pretty printing a tuple id
ppTupId :: TupIdent ty -> String
ppTupId (Id ident) = " " ++ nameToString (identName ident) ++ " "
ppTupId (TupId pats _) = " ( " ++ intercalate ", " (map ppTupId pats) ++ " ) "

--        "\n" ++ spaces d ++ "let " ++ ppTupId tupid ++ " = " ++ ppExp d e1 ++
--        " in  " ++ ppExp d body



-- pretty printing Lambda, i.e., curried and unnamed functions *)
ppLambda :: TypeBox ty => Int -> Lambda ty -> String
ppLambda d ( AnonymFun params body rtp _) =
      let pp_bd (Ident arg tp _) = ppType tp ++ " " ++ nameToString arg
          strargs = intercalate ", " $ map pp_bd params
      in "\n" ++ spaces d ++ "fn " ++ ppType rtp ++ " ( " ++ strargs ++ 
         " ) " ++ " => " ++ ppExp (d+1) body ++ "\n" ++ spaces d
--      let pp_bd (Ident arg tp _) = ppType tp ++ " " ++ arg
--          strargs = intercalate ", " $ map pp_bd params
--      in " fn " ++ ppType rtp ++ " ( " ++ strargs ++ " ) " ++ " => " ++ ppExp 0 body
ppLambda _ ( CurryFun fid []   _  _  ) = nameToString fid
ppLambda _ ( CurryFun fid args ty pos) =
      ppExp 0 (Apply fid args ty pos)

-- | pretty printing a function declaration
ppFun ::  TypeBox ty => Int -> FunDec ty -> String
ppFun d (name, ret_tp, args, body, _) =
  let -- pretty printing a list of bindings separated by commas
      ppBd (Ident argname tp _) = ppType tp ++ " " ++ nameToString argname
      pp_bindings = intercalate "," . map ppBd

  in "\n\nfun " ++ ppType ret_tp ++ nameToString name ++
     "( " ++ pp_bindings args ++ ") = \n" ++
     spaces (d+1) ++ ppExp (d+1) body

-- | Pretty printing a program.
prettyPrint ::  TypeBox ty => Prog ty -> String
prettyPrint p = concatMap (ppFun 0) p ++ "\n"
