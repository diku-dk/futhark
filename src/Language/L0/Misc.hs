-- | This module provides simple definitions used by other L0 modules.
module Language.L0.Misc
  ( locStr

  -- * Names
  , Name (..)
  , nameToString
  , nameFromString

  -- * Tagged names
  , ID (..)
  , baseName
  , baseString
  , VName

  -- * Variable names
  , VarName(..)
  )
  where

import Data.Char (isDigit)
import Data.Hashable
import Data.Loc
import Data.Maybe
import qualified Data.Text as T

import Text.PrettyPrint.Mainland

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
