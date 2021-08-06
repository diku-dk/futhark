{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

-- | This module contains very basic definitions for Futhark - so basic,
-- that they can be shared between the internal and external
-- representation.
module Language.Futhark.Core
  ( Uniqueness (..),

    -- * Location utilities
    SrcLoc,
    Loc,
    Located (..),
    srclocOf,
    locStr,
    locStrRel,
    prettyStacktrace,

    -- * Name handling
    Name,
    nameToString,
    nameFromString,
    nameToText,
    nameFromText,
    VName (..),
    baseTag,
    baseName,
    baseString,
    quote,
    pquote,

    -- * Special identifiers
    defaultEntryPoint,

    -- * Number re-export
    Int8,
    Int16,
    Int32,
    Int64,
    Word8,
    Word16,
    Word32,
    Word64,
    Half,
  )
where

import Control.Category
import Data.Int (Int16, Int32, Int64, Int8)
import Data.String
import qualified Data.Text as T
import Data.Word (Word16, Word32, Word64, Word8)
import Futhark.Util.Loc
import Futhark.Util.Pretty
import Numeric.Half
import Prelude hiding (id, (.))

-- | The uniqueness attribute of a type.  This essentially indicates
-- whether or not in-place modifications are acceptable.  With respect
-- to ordering, 'Unique' is greater than 'Nonunique'.
data Uniqueness
  = -- | May have references outside current function.
    Nonunique
  | -- | No references outside current function.
    Unique
  deriving (Eq, Ord, Show)

instance Semigroup Uniqueness where
  (<>) = min

instance Monoid Uniqueness where
  mempty = Unique

instance Pretty Uniqueness where
  ppr Unique = star
  ppr Nonunique = empty

-- | The name of the default program entry point (main).
defaultEntryPoint :: Name
defaultEntryPoint = nameFromString "main"

-- | The abstract (not really) type representing names in the Futhark
-- compiler.  'String's, being lists of characters, are very slow,
-- while 'T.Text's are based on byte-arrays.
newtype Name = Name T.Text
  deriving (Show, Eq, Ord, IsString, Semigroup)

instance Pretty Name where
  ppr = text . nameToString

-- | Convert a name to the corresponding list of characters.
nameToString :: Name -> String
nameToString (Name t) = T.unpack t

-- | Convert a list of characters to the corresponding name.
nameFromString :: String -> Name
nameFromString = Name . T.pack

-- | Convert a name to the corresponding 'T.Text'.
nameToText :: Name -> T.Text
nameToText (Name t) = t

-- | Convert a 'T.Text' to the corresponding name.
nameFromText :: T.Text -> Name
nameFromText = Name

-- | A human-readable location string, of the form
-- @filename:lineno:columnno@.  This follows the GNU coding standards
-- for error messages:
-- https://www.gnu.org/prep/standards/html_node/Errors.html
--
-- This function assumes that both start and end position is in the
-- same file (it is not clear what the alternative would even mean).
locStr :: Located a => a -> String
locStr a =
  case locOf a of
    NoLoc -> "unknown location"
    Loc (Pos file line1 col1 _) (Pos _ line2 col2 _)
      -- Do not show line2 if it is identical to line1.
      | line1 == line2 ->
        first_part ++ "-" ++ show col2
      | otherwise ->
        first_part ++ "-" ++ show line2 ++ ":" ++ show col2
      where
        first_part = file ++ ":" ++ show line1 ++ ":" ++ show col1

-- | Like 'locStr', but @locStrRel prev now@ prints the location @now@
-- with the file name left out if the same as @prev@.  This is useful
-- when printing messages that are all in the context of some
-- initially printed location (e.g. the first mention contains the
-- file name; the rest just line and column name).
locStrRel :: (Located a, Located b) => a -> b -> String
locStrRel a b =
  case (locOf a, locOf b) of
    (Loc (Pos a_file _ _ _) _, Loc (Pos b_file line1 col1 _) (Pos _ line2 col2 _))
      | a_file == b_file,
        line1 == line2 ->
        first_part ++ "-" ++ show col2
      | a_file == b_file ->
        first_part ++ "-" ++ show line2 ++ ":" ++ show col2
      where
        first_part = show line1 ++ ":" ++ show col1
    _ -> locStr b

-- | Given a list of strings representing entries in the stack trace
-- and the index of the frame to highlight, produce a final
-- newline-terminated string for showing to the user.  This string
-- should also be preceded by a newline.  The most recent stack frame
-- must come first in the list.
prettyStacktrace :: Int -> [String] -> String
prettyStacktrace cur = unlines . zipWith f [(0 :: Int) ..]
  where
    -- Formatting hack: assume no stack is deeper than 100
    -- elements.  Since Futhark does not support recursion, going
    -- beyond that would require a truly perverse program.
    f i x =
      (if cur == i then "-> " else "   ")
        ++ '#' :
      show i
        ++ (if i > 9 then "" else " ")
        ++ " "
        ++ x

-- | A name tagged with some integer.  Only the integer is used in
-- comparisons, no matter the type of @vn@.
data VName = VName !Name !Int
  deriving (Show)

-- | Return the tag contained in the 'VName'.
baseTag :: VName -> Int
baseTag (VName _ tag) = tag

-- | Return the name contained in the 'VName'.
baseName :: VName -> Name
baseName (VName vn _) = vn

-- | Return the base 'Name' converted to a string.
baseString :: VName -> String
baseString = nameToString . baseName

instance Eq VName where
  VName _ x == VName _ y = x == y

instance Ord VName where
  VName _ x `compare` VName _ y = x `compare` y

-- | Enclose a string in the prefered quotes used in error messages.
-- These are picked to not collide with characters permitted in
-- identifiers.
quote :: String -> String
quote s = "\"" ++ s ++ "\""

-- | As 'quote', but works on prettyprinted representation.
pquote :: Doc -> Doc
pquote = dquotes
