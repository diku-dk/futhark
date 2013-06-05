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
  , Ident
  , Exp
  , Lambda
  , TupIdent
  , FunDec
  , Prog
  )
where

import Data.Char (isDigit)
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

type VName = ID Name

baseName :: ID vn -> vn
baseName (ID (vn, _)) = vn

instance Eq (ID vn) where
  ID (_, x) == ID (_, y) = x == y

instance Ord (ID vn) where
  ID (_, x) `compare` ID (_, y) = x `compare` y

instance Pretty vn => Pretty (ID vn) where
  ppr (ID (vn, i)) = ppr vn <> text "_" <> text (show i)

class (Ord vn, Show vn, Pretty vn) => VarName vn where
  -- | Set the numeric tag associated with this name.
  setID :: vn -> Int -> vn
  -- | Identity-preserving prettyprinting of a name.
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

type Ident = IdentBase Type VName

type Exp = ExpBase Type VName

type Lambda = LambdaBase Type VName

type TupIdent = TupIdentBase Type VName

type FunDec = FunDecBase Type VName

type Prog = ProgBase Type VName
