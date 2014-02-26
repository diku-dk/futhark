-- | This module contains very basic definitions for L0 - so basic,
-- that they can be shared between the internal and external
-- representation.
module Language.L0.Core
  ( BinOp(..)
  , opStr
  , Uniqueness(..)
  , BasicType(..)
  , BasicValue(..)
  , basicValueType
  , blankBasicValue

  -- * Location utilities
  , locStr

  -- * Name handling
  , Name
  , nameToString
  , nameFromString
  , ID(..)
  , baseName
  , baseString
  , VName
  , VarName(..)

  -- * Special identifiers
  , defaultEntryPoint
  , isBuiltInFunction
  , builtInFunctions
  )

where

import Data.Char
import Data.Hashable
import Data.Loc
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM

import Text.PrettyPrint.Mainland
import Text.Printf

-- | Binary operators.
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

-- | The Operator, without whitespace, that corresponds to this
-- @BinOp@.  For example, @opStr Plus@ gives @"+"@.
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

-- | The uniqueness attribute of a type.  This essentially indicates
-- whether or not in-place modifications are acceptable.
data Uniqueness = Unique    -- ^ At most one outer reference.
                | Nonunique -- ^ Any number of references.
                  deriving (Eq, Ord, Show)

instance Monoid Uniqueness where
  mempty = Unique
  _ `mappend` Nonunique = Nonunique
  Nonunique `mappend` _ = Nonunique
  u `mappend` _         = u

instance Hashable Uniqueness where
  hashWithSalt salt Unique    = salt
  hashWithSalt salt Nonunique = salt * 2

-- | Low-level primitive types.  TODO: please add float, double, long
-- int, etc.
data BasicType = Int
               | Bool
               | Char
               | Real
               | Cert
                 deriving (Eq, Ord, Show)

-- | Non-array values.
data BasicValue = IntVal !Int
                | RealVal !Double
                | LogVal !Bool
                | CharVal !Char
                | Checked -- ^ The only value of type @cert@.
                  deriving (Eq, Ord, Show)

-- | The type of a basic value.
basicValueType :: BasicValue -> BasicType
basicValueType (IntVal _) = Int
basicValueType (RealVal _) = Real
basicValueType (LogVal _) = Bool
basicValueType (CharVal _) = Char
basicValueType Checked = Cert

-- | A "blank" value of the given basic type - this is zero, or
-- whatever is close to it.  Don't depend on this value, but use it
-- for e.g. creating arrays to be populated by do-loops.
blankBasicValue :: BasicType -> BasicValue
blankBasicValue Int = IntVal 0
blankBasicValue Real = RealVal 0.0
blankBasicValue Bool = LogVal False
blankBasicValue Char = CharVal '\0'
blankBasicValue Cert = Checked

instance Pretty BasicType where
  ppr Int = text "int"
  ppr Char = text "char"
  ppr Bool = text "bool"
  ppr Real = text "real"
  ppr Cert = text "cert"

instance Pretty BasicValue where
  ppr (IntVal x) = text $ show x
  ppr (CharVal c) = text $ show c
  ppr (LogVal b) = text $ show b
  ppr (RealVal x) = text $ printf "%f" x
  ppr Checked = text "Checked"

-- | The name of the default program entry point (main).
defaultEntryPoint :: Name
defaultEntryPoint = nameFromString "main"

-- | @isBuiltInFunction k@ is 'True' if @k@ is an element of 'builtInFunctions'.
isBuiltInFunction :: Name -> Bool
isBuiltInFunction fnm = fnm `HM.member` builtInFunctions

-- | A map of all built-in functions and their types.
builtInFunctions :: HM.HashMap Name (BasicType,[BasicType])
builtInFunctions = HM.fromList $ map namify
                   [("toReal", (Real, [Int]))
                   ,("trunc", (Int, [Real]))
                   ,("sqrt", (Real, [Real]))
                   ,("log", (Real, [Real]))
                   ,("exp", (Real, [Real]))
                   ,("op not", (Bool, [Bool]))]
  where namify (k,v) = (nameFromString k, v)

-- | The abstract (not really) type representing names in the L0
-- compiler.  'String's, being lists of characters, are very slow,
-- while 'T.Text's are based on byte-arrays.
newtype Name = Name T.Text
  deriving (Show, Eq, Ord)

instance Pretty Name where
  ppr = text . nameToString

instance Hashable Name where
  hashWithSalt salt (Name t) = hashWithSalt salt t

-- | Convert a name to the corresponding list of characters.
nameToString :: Name -> String
nameToString (Name t) = T.unpack t

-- | Convert a list of characters to the corresponding name.
nameFromString :: String -> Name
nameFromString = Name . T.pack

-- | A human-readable location string, of the form
-- @filename:lineno:columnno@.
locStr :: SrcLoc -> String
locStr (SrcLoc NoLoc) = "unknown location"
locStr (SrcLoc (Loc (Pos file line1 col1 _) (Pos _ line2 col2 _))) =
  -- Assume that both positions are in the same file (what would the
  -- alternative mean?)
  file ++ ":" ++ show line1 ++ ":" ++ show col1
       ++ "-" ++ show line2 ++ ":" ++ show col2

-- | An arbitrary value tagged with some integer.  Only the integer is
-- used in comparisons, no matter the type of @vn@.
newtype ID vn = ID (vn, Int)
  deriving (Show)

-- | Alias for a tagged 'Name'.  This is used as the name
-- representation in most the compiler.
type VName = ID Name

-- | Return the name contained in the 'ID'.
baseName :: ID vn -> vn
baseName (ID (vn, _)) = vn

-- | Return the base 'Name' converted to a string.
baseString :: VName -> String
baseString = nameToString . baseName

instance Eq (ID vn) where
  ID (_, x) == ID (_, y) = x == y

instance Ord (ID vn) where
  ID (_, x) `compare` ID (_, y) = x `compare` y

instance Pretty vn => Pretty (ID vn) where
  ppr (ID (vn, i)) = ppr vn <> text "_" <> text (show i)

instance Hashable (ID vn) where
  hashWithSalt salt (ID (_,i)) = salt * i

-- | A type that can be used for representing variable names.  These
-- must support tagging, as well as conversion to a textual format.
class (Ord vn, Show vn, Pretty vn, Hashable vn) => VarName vn where
  -- | Set the numeric tag associated with this name.
  setID :: vn -> Int -> vn
  -- | Identity-preserving prettyprinting of a name.  This means that
  -- if and only if @x == y@, @textual x == textual y@.
  textual :: vn -> String
  -- | Create a name based on a string and a numeric tag.
  varName :: String -> Maybe Int -> vn

instance VarName vn => VarName (ID vn) where
  setID (ID (vn, _)) i = ID (vn, i)
  textual (ID (vn, i)) = textual vn ++ '_' : show i
  varName s i = ID (varName s Nothing, fromMaybe 0 i)

instance VarName Name where
  setID (Name t) i = Name $ stripSuffix t <> T.pack ('_' : show i)
  textual = nameToString
  varName s (Just i) = nameFromString s `setID` i
  varName s Nothing = nameFromString s

-- | Chop off terminating underscore followed by numbers.
stripSuffix :: T.Text -> T.Text
stripSuffix = T.dropWhileEnd (=='_') . T.dropWhileEnd isDigit
