-- | Re-export the external Futhark modules for convenience.
module Language.Futhark
  ( module Language.Futhark.Syntax
  , module Language.Futhark.Attributes
  , module Language.Futhark.Pretty
  , module Language.Futhark.Traversals

  , Ident, DimIndex, Exp, Lambda
  , Pattern, FunDef, ConstDef, Prog
  , Type, StructType, ArrayType
  )
  where

import Language.Futhark.Syntax
import Language.Futhark.Attributes
import Language.Futhark.Pretty
import Language.Futhark.Traversals

-- | An identifier with type- and aliasing information information.
type Ident = IdentBase Info VName

-- | An index with type information.
type DimIndex = DimIndexBase Info VName

-- | An expression with type information.
type Exp = ExpBase Info VName

-- | A lambda with type information.
type Lambda = LambdaBase Info VName

-- | A pattern with type information.
type Pattern = PatternBase Info VName

-- | An function declaration with type information.
type FunDef = FunDefBase Info VName

-- | An constant declaration with type information.
type ConstDef = ConstDefBase Info VName

-- | An Futhark program with type information.
type Prog = ProgBase Info VName

-- | A known type with no shape annotations, but aliasing information.
type Type = TypeBase Rank Names VName

-- | A known type with shape annotations but no aliasing information.
type StructType = TypeBase ShapeDecl NoInfo VName

-- | A known array type with no shape annotations, but aliasing
-- information.
type ArrayType = ArrayTypeBase Rank Names VName
