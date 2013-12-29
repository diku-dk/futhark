{-# LANGUAGE FlexibleInstances #-}
-- | L0C-specific type- and class definitions built on top of the L0
-- definitions in "Language.L0".
module L0C.L0
  ( module Language.L0.Syntax
  , module Language.L0.Attributes
  , module Language.L0.Pretty
  , module Language.L0.Traversals

  -- * Tagged names
  , ID(..)
  , baseName
  , VarName(..)
  , VName

  -- * Type aliases
  --
  -- | These types contain full type information and use tagged
  -- 'VName's for efficient symbol tables.
  , GenIdent
  , Ident
  , Parameter
  , Certificates
  , Exp
  , Lambda
  , TupleLambda
  , TupIdent
  , FunDec
  , Prog
  , GenType
  , GenElemType
  , Type
  , DeclType
  , ElemType
  )
where

import Data.Char (isDigit)
import Data.Hashable
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T

import Text.PrettyPrint.Mainland

import Language.L0.Syntax
import Language.L0.Attributes
import Language.L0.Pretty
import Language.L0.Traversals

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

-- | A generic identifier parametrised on what aliasing information it records.
type GenIdent as = IdentBase (TypeBase as) VName

-- | An identifier with type- and aliasing information information.
type Ident = GenIdent Names

-- | A name with a type, but no aliasing information.  Used for
-- denoting function parameters.
type Parameter = GenIdent NoInfo

-- | An list of certificates with type information.
type Certificates = CertificatesBase (TypeBase Names) VName

-- | An expression with type information.
type Exp = ExpBase (TypeBase Names) VName

-- | A lambda with type information.
type Lambda = LambdaBase (TypeBase Names) VName

-- | A tuple lambda with type information.
type TupleLambda = TupleLambdaBase (TypeBase Names) VName

-- | A pattern with type information.
type TupIdent = TupIdentBase (TypeBase Names) VName

-- | An function declaration with type information.
type FunDec = FunDecBase (TypeBase Names) VName

-- | An L0 program with type information.
type Prog = ProgBase (TypeBase Names) VName

-- | A known type parametrised over its aliasing information.
type GenType als = TypeBase als VName

-- | A known element type parametrised over its aliasing information.
type GenElemType als = ElemTypeBase als VName

-- | A known type with aliasing information.
type Type = TypeBase Names VName

-- | A known type with no aliasing information.
type DeclType = TypeBase NoInfo VName

-- | A known element type with aliasing information.
type ElemType = ElemTypeBase Names VName
