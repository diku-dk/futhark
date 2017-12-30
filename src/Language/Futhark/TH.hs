{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Strict             #-}
-- | Template Haskell instances for AST and type checker types.
module Language.Futhark.TH () where


import           Data.Loc
import           Instances.TH.Lift                  ()
import           Language.Haskell.TH.Syntax         (Lift)

import           Language.Futhark.Core
import           Language.Futhark.Syntax
import           Language.Futhark.TypeChecker
import           Language.Futhark.TypeChecker.Monad

-- Instances for things from non-Futhark packages
deriving instance Lift Pos
deriving instance Lift Loc
deriving instance Lift SrcLoc

-- Generic AST instances
deriving instance Lift a => Lift (Info a)
deriving instance Lift (NoInfo a)
deriving instance Lift a => Lift (QualName a)
deriving instance Lift a => Lift (DimDecl a)
deriving instance Lift a => Lift (TypeArgExp a)
deriving instance Lift a => Lift (TypeExp a)
deriving instance Lift TypeName
deriving instance Lift IntType
deriving instance Lift FloatType
deriving instance Lift PrimType
deriving instance (Lift a, Lift b) => Lift (TypeArg a b)
deriving instance (Lift a, Lift b) => Lift (RecordArrayElemTypeBase a b)
deriving instance (Lift a, Lift b) => Lift (ArrayTypeBase a b)
deriving instance (Lift a, Lift b) => Lift (TypeBase a b)
deriving instance Lift a => Lift (ShapeDecl a)
deriving instance Lift a => Lift (Inclusiveness a)
deriving instance Lift IntValue
deriving instance Lift FloatValue
deriving instance Lift PrimValue
deriving instance Lift Diet

-- Checked AST instances
deriving instance Lift (TypeDeclBase Info VName)
deriving instance Lift (IdentBase Info VName)
deriving instance Lift (PatternBase Info VName)
deriving instance Lift (ValBindBase Info VName)
deriving instance Lift (FieldBase Info VName)
deriving instance Lift (LoopFormBase Info VName)
deriving instance Lift (DimIndexBase Info VName)
deriving instance Lift (TypeParamBase VName)
deriving instance Lift (StreamForm Info VName)
deriving instance Lift (ExpBase Info VName)
deriving instance Lift (FunBindBase Info VName)
deriving instance Lift (TypeBindBase Info VName)
deriving instance Lift (ParamBase Info VName)
deriving instance Lift (SpecBase Info VName)
deriving instance Lift (TypeRefBase Info VName)
deriving instance Lift (SigExpBase Info VName)
deriving instance Lift (SigBindBase Info VName)
deriving instance Lift (ModParamBase Info VName)
deriving instance Lift (ModExpBase Info VName)
deriving instance Lift (ModBindBase Info VName)
deriving instance Lift (DecBase Info VName)
deriving instance Lift (ProgBase Info VName)

-- Typechecker instances
deriving instance Lift ValBinding
deriving instance Lift TypeBinding
deriving instance Lift MTy
deriving instance Lift FunSig
deriving instance Lift Mod
deriving instance Lift Namespace
deriving instance Lift Env
deriving instance Lift FileModule
