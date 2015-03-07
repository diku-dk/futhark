-- | Futhark-specific type- and class definitions built on top of the Futhark
-- definitions in "Language.Futhark".
module Futhark.Representation.External
  (
    module Language.Futhark.Syntax
  , module Language.Futhark.Attributes
  , module Language.Futhark.Pretty
  , module Language.Futhark.Traversals

  -- * Name generation
  , newNameSourceForProg

  -- * Type aliases
  --
  -- | These types contain full type information and use tagged
  -- 'VName's for efficient symbol tables.
  , GenIdent
  , Ident
  , Parameter
  , Exp
  , Lambda
  , TupIdent
  , FunDec
  , Prog
  , GenType
  , Type
  , DeclType
  , ArrayType
  )
where

import Language.Futhark.Syntax
import Language.Futhark.Attributes
import Language.Futhark.Pretty
import Language.Futhark.Traversals

import Futhark.FreshNames

-- | Create a new 'NameSource' that will never produce any of the
-- names used as variables in the given program.
newNameSourceForProg :: VarName vn => ProgBase ty vn -> NameSource vn
newNameSourceForProg = newNameSource . progNames

-- | A generic identifier parametrised on what aliasing information it records.
type GenIdent as = IdentBase (TypeBase as) VName

-- | An identifier with type- and aliasing information information.
type Ident = GenIdent Names

-- | A name with a type, but no aliasing information.  Used for
-- denoting function parameters.
type Parameter = GenIdent NoInfo

-- | An expression with type information.
type Exp = ExpBase (TypeBase Names) VName

-- | A lambda with type information.
type Lambda = LambdaBase (TypeBase Names) VName

-- | A pattern with type information.
type TupIdent = TupIdentBase (TypeBase Names) VName

-- | An function declaration with type information.
type FunDec = FunDecBase (TypeBase Names) VName

-- | An Futhark program with type information.
type Prog = ProgBase (TypeBase Names) VName

-- | A known type parametrised over its aliasing information.
type GenType als = TypeBase als VName

-- | A known type with aliasing information.
type Type = TypeBase Names VName

-- | A known type with no aliasing information.
type DeclType = TypeBase NoInfo VName

-- | A known array type with aliasing information.
type ArrayType = ArrayTypeBase Names VName
