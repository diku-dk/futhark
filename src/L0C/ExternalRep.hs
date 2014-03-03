-- | L0C-specific type- and class definitions built on top of the L0
-- definitions in "Language.L0".
module L0C.ExternalRep
  (
    module Language.L0.Syntax
  , module Language.L0.Attributes
  , module Language.L0.Pretty
  , module Language.L0.Traversals

  -- * Name generation
  , newNameSourceForProg

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

import Language.L0.Syntax
import Language.L0.Attributes
import Language.L0.Pretty
import Language.L0.Traversals

import L0C.FreshNames

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
