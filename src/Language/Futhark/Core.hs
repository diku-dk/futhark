-- | This module contains very basic definitions for Futhark - so basic,
-- that they can be shared between the internal and external
-- representation.
module Language.Futhark.Core
  ( Uniqueness(..)
  , IntType(..)
  , FloatType(..)
  , PrimType(..)
  , IntValue(..)
  , FloatValue(..)
  , PrimValue(..)
  , primValueType
  , blankPrimValue
  , ChunkIntent(..)
  , StreamOrd(..)
  , Commutativity(..)

  -- * Location utilities
  , locStr

  -- * Name handling
  , Name
  , nameToString
  , nameFromString
  , ID(..)
  , baseTag
  , baseName
  , baseString
  , VName
  , VarName(..)

  -- * Special identifiers
  , defaultEntryPoint
  , isBuiltInFunction
  , builtInFunctions

    -- * Integer re-export
  , Int8, Int16, Int32, Int64
  )

where

import Data.Char
import Data.Hashable
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Loc
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM

import Text.PrettyPrint.Mainland


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

data ChunkIntent = MaxChunk
                 | MinChunk
                    deriving (Eq, Ord, Show)

data StreamOrd  = InOrder
                | Disorder
                    deriving (Eq, Ord, Show)

-- | Whether some operator is commutative or not.  The 'Monoid'
-- instance returns the least commutative of its arguments.
data Commutativity = Noncommutative
                   | Commutative
                     deriving (Eq, Ord, Show)

instance Monoid Commutativity where
  mempty = Commutative
  mappend = min

-- | An integer type.  Note that signedness is not a property of the
-- type, but a property of the operations performed on values of these
-- types.
data IntType = Int8
             | Int16
             | Int32
             | Int64
             deriving (Eq, Ord, Show, Enum, Bounded)

instance Hashable IntType where
  hashWithSalt salt = hashWithSalt salt . fromEnum

-- | A floating point type.
data FloatType = Float32
               | Float64
               deriving (Eq, Ord, Show, Enum, Bounded)

instance Hashable FloatType where
  hashWithSalt salt = hashWithSalt salt . fromEnum

-- | Low-level primitive types.
data PrimType = IntType IntType
              | FloatType FloatType
              | Bool
              | Char
              | Cert
              deriving (Eq, Ord, Show)

instance Enum PrimType where
  toEnum 0 = IntType Int8
  toEnum 1 = IntType Int16
  toEnum 2 = IntType Int32
  toEnum 3 = IntType Int64
  toEnum 4 = FloatType Float32
  toEnum 5 = FloatType Float64
  toEnum 6 = Bool
  toEnum 7 = Char
  toEnum _ = Cert

  fromEnum (IntType Int8) = 0
  fromEnum (IntType Int16) = 1
  fromEnum (IntType Int32) = 2
  fromEnum (IntType Int64) = 3
  fromEnum (FloatType Float32) = 4
  fromEnum (FloatType Float64) = 5
  fromEnum Bool = 6
  fromEnum Char = 7
  fromEnum Cert = 8

instance Bounded PrimType where
  minBound = IntType Int8
  maxBound = Cert

instance Hashable PrimType where
  hashWithSalt salt = hashWithSalt salt . fromEnum

-- | An integer value.
data IntValue = Int8Value !Int8
              | Int16Value !Int16
              | Int32Value !Int32
              | Int64Value !Int64
               deriving (Eq, Ord, Show)

intValueType :: IntValue -> IntType
intValueType Int8Value{} = Int8
intValueType Int16Value{} = Int16
intValueType Int32Value{} = Int32
intValueType Int64Value{} = Int64

-- | A floating-point value.
data FloatValue = Float32Value !Float
                | Float64Value !Double
               deriving (Eq, Ord, Show)

floatValueType :: FloatValue -> FloatType
floatValueType Float32Value{} = Float32
floatValueType Float64Value{} = Float64

-- | Non-array values.
data PrimValue = IntValue !IntValue
               | FloatValue !FloatValue
               | BoolValue !Bool
               | CharValue !Char
               | Checked -- ^ The only value of type @cert@.
               deriving (Eq, Ord, Show)

-- | The type of a basic value.
primValueType :: PrimValue -> PrimType
primValueType (IntValue v) = IntType $ intValueType v
primValueType (FloatValue v) = FloatType $ floatValueType v
primValueType BoolValue{} = Bool
primValueType CharValue{} = Char
primValueType Checked = Cert

-- | A "blank" value of the given basic type - this is zero, or
-- whatever is close to it.  Don't depend on this value, but use it
-- for e.g. creating arrays to be populated by do-loops.
blankPrimValue :: PrimType -> PrimValue
blankPrimValue (IntType Int8) = IntValue $ Int8Value 0
blankPrimValue (IntType Int16) = IntValue $ Int16Value 0
blankPrimValue (IntType Int32) = IntValue $ Int32Value 0
blankPrimValue (IntType Int64) = IntValue $ Int64Value 0
blankPrimValue (FloatType Float32) = FloatValue $ Float32Value 0.0
blankPrimValue (FloatType Float64) = FloatValue $ Float64Value 0.0
blankPrimValue Bool = BoolValue False
blankPrimValue Char = CharValue '\0'
blankPrimValue Cert = Checked

instance Pretty IntType where
  ppr Int8 = text "i8"
  ppr Int16 = text "i16"
  ppr Int32 = text "i32"
  ppr Int64 = text "i64"

instance Pretty FloatType where
  ppr Float32 = text "f32"
  ppr Float64 = text "f64"

instance Pretty PrimType where
  ppr (IntType t) = ppr t
  ppr (FloatType t) = ppr t
  ppr Char = text"char"
  ppr Bool = text "bool"
  ppr Cert = text "cert"

instance Pretty IntValue where
  ppr (Int8Value v) = text $ show v
  ppr (Int16Value v) = text $ show v
  ppr (Int32Value v) = text $ show v
  ppr (Int64Value v) = text $ show v

instance Pretty FloatValue where
  ppr (Float32Value v) = text $ show v
  ppr (Float64Value v) = text $ show v

instance Pretty PrimValue where
  ppr (IntValue v) = ppr v
  ppr (CharValue c) = text $ show c
  ppr (BoolValue b) = text $ show b
  ppr (FloatValue v) = ppr v
  ppr Checked = text "Checked"

-- | The name of the default program entry point (main).
defaultEntryPoint :: Name
defaultEntryPoint = nameFromString "main"

-- | @isBuiltInFunction k@ is 'True' if @k@ is an element of 'builtInFunctions'.
isBuiltInFunction :: Name -> Bool
isBuiltInFunction fnm = fnm `HM.member` builtInFunctions

-- | A map of all built-in functions and their types.
builtInFunctions :: HM.HashMap Name (PrimType,[PrimType])
builtInFunctions = HM.fromList $ map namify
                   [("sqrt32", (FloatType Float32, [FloatType Float32]))
                   ,("log32", (FloatType Float32, [FloatType Float32]))
                   ,("exp32", (FloatType Float32, [FloatType Float32]))
                   ,("trunc32", (IntType Int32, [FloatType Float32]))

                   ,("sqrt64", (FloatType Float64, [FloatType Float64]))
                   ,("log64", (FloatType Float64, [FloatType Float64]))
                   ,("exp64", (FloatType Float64, [FloatType Float64]))
                   ,("trunc64", (IntType Int32, [FloatType Float64]))

                   ,("num_groups", (IntType Int32, []))
                   ,("group_size", (IntType Int32, []))
                   ]
  where namify (k,v) = (nameFromString k, v)

-- | The abstract (not really) type representing names in the Futhark
-- compiler.  'String's, being lists of characters, are very slow,
-- while 'T.Text's are based on byte-arrays.
newtype Name = Name T.Text
  deriving (Show, Eq, Ord)

instance Pretty Name where
  ppr = text . nameToString

instance Hashable Name where
  hashWithSalt salt (Name t) = hashWithSalt salt t

instance Monoid Name where
  Name t1 `mappend` Name t2 = Name $ t1 <> t2
  mempty = Name mempty

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

-- | Return the tag contained in the 'ID'.
baseTag :: ID vn -> Int
baseTag (ID (_, tag)) = tag

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
