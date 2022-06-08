-- | Re-export the external Futhark modules for convenience.
module Language.Futhark
  ( module Language.Futhark.Syntax,
    module Language.Futhark.Prop,
    module Language.Futhark.FreeVars,
    module Language.Futhark.Pretty,
    Ident,
    DimIndex,
    Slice,
    AppExp,
    Exp,
    Pat,
    ModExp,
    ModParam,
    SigExp,
    ModBind,
    SigBind,
    ValBind,
    Dec,
    Spec,
    Prog,
    TypeBind,
    StructTypeArg,
    ScalarType,
    TypeParam,
    Case,
  )
where

import Language.Futhark.FreeVars
import Language.Futhark.Pretty
import Language.Futhark.Prop
import Language.Futhark.Syntax

-- | An identifier with type- and aliasing information.
type Ident = IdentBase Info VName

-- | An index with type information.
type DimIndex = DimIndexBase Info VName

-- | A slice with type information.
type Slice = SliceBase Info VName

-- | An expression with type information.
type Exp = ExpBase Info VName

-- | An application expression with type information.
type AppExp = AppExpBase Info VName

-- | A pattern with type information.
type Pat = PatBase Info VName

-- | An constant declaration with type information.
type ValBind = ValBindBase Info VName

-- | A type binding with type information.
type TypeBind = TypeBindBase Info VName

-- | A type-checked module binding.
type ModBind = ModBindBase Info VName

-- | A type-checked module type binding.
type SigBind = SigBindBase Info VName

-- | A type-checked module expression.
type ModExp = ModExpBase Info VName

-- | A type-checked module parameter.
type ModParam = ModParamBase Info VName

-- | A type-checked module type expression.
type SigExp = SigExpBase Info VName

-- | A type-checked declaration.
type Dec = DecBase Info VName

-- | A type-checked specification.
type Spec = SpecBase Info VName

-- | An Futhark program with type information.
type Prog = ProgBase Info VName

-- | A known type arg with shape annotations.
type StructTypeArg = TypeArg Size

-- | A type-checked type parameter.
type TypeParam = TypeParamBase VName

-- | A known scalar type with no shape annotations.
type ScalarType = ScalarTypeBase ()

-- | A type-checked case (of a match expression).
type Case = CaseBase Info VName
