-- | Constraint solver for solving type equations produced
-- post-AUTOMAP.
module Language.Futhark.TypeChecker.Constraints
  ( Reason (..),
    SVar,
    SComp (..),
    CtType,
    CtTy (..),
    CtAM (..),
    TyVarInfo (..),
    Level,
    TyVar,
    TyVars,
    TyParams,
  )
where

import Data.Loc
import Data.Map qualified as M
import Futhark.Util.Pretty
import Language.Futhark

-- | A shape variable.
type SVar = VName

-- | A shape component. `SDim` is a single dimension of unspecified
-- size, `SVar` is a shape variable. A list of shape components should
-- then be understood as concatenation of shapes (meaning you can't
-- just take the length to determine the rank of the array).
data SComp
  = SDim
  | SVar SVar
  deriving (Eq, Ord, Show)

instance Pretty SComp where
  pretty SDim = "[]"
  pretty (SVar x) = brackets $ prettyName x

instance Pretty (Shape SComp) where
  pretty = mconcat . map pretty . shapeDims

-- | The type representation used by the constraint solver.
type CtType d = TypeBase d NoUniqueness

-- | The reason for a type constraint. Used to generate type error
-- messages. The expected type is always the first one.
data Reason t
  = -- | No particular reason.
    Reason Loc
  | -- | Arising from pattern match.
    ReasonPatMatch Loc (PatBase NoInfo VName ParamType) t
  | -- | Arising from explicit ascription.
    ReasonAscription Loc t t
  | ReasonRetType Loc t t
  | ReasonApply Loc (Maybe (QualName VName), Int) Exp t t
  | -- | Used when unifying a type with a function type in a function
    -- application. If this unification fails, it means the supposed
    -- function was not a function after all.
    ReasonApplySplit Loc (Maybe (QualName VName), Int) Exp t
  | ReasonBranches Loc t t
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Located (Reason t) where
  locOf (Reason l) = l
  locOf (ReasonPatMatch l _ _) = l
  locOf (ReasonAscription l _ _) = l
  locOf (ReasonRetType l _ _) = l
  locOf (ReasonApply l _ _ _ _) = l
  locOf (ReasonApplySplit l _ _ _) = l
  locOf (ReasonBranches l _ _) = l

-- | A type constraint.
data CtTy d = CtEq (Reason (CtType d)) (TypeBase d NoUniqueness) (TypeBase d NoUniqueness)
  deriving (Show)

ctReason :: CtTy d -> Reason (CtType d)
ctReason (CtEq r _ _) = r

instance Located (CtTy d) where
  locOf = locOf . ctReason

instance Pretty (CtTy Size) where
  pretty (CtEq _ t1 t2) = pretty t1 <+> "~" <+> pretty t2

instance Pretty (CtTy SComp) where
  pretty (CtEq _ t1 t2) = pretty t1 <+> "~" <+> pretty t2

instance Pretty (CtTy ()) where
  pretty (CtEq _ t1 t2) = pretty t1 <+> "~" <+> pretty t2

-- | Information about a flexible type variable. Every type variable
-- is associated with a location, which is the original syntax element
-- that it is the type of.
data TyVarInfo d
  = -- | Can be substituted with anything.
    TyVarFree Loc Liftedness
  | -- | Can only be substituted with these primitive types.
    TyVarPrim Loc [PrimType]
  | -- | Must be a record with these fields.
    TyVarRecord Loc (M.Map Name (CtType d))
  | -- | Must be a sum type with these fields.
    TyVarSum Loc (M.Map Name [CtType d])
  deriving (Show, Eq)

prettyTyVarInfo :: (Pretty (Shape d)) => TyVarInfo d -> Doc a
prettyTyVarInfo (TyVarFree _ l) = "free" <+> pretty l
prettyTyVarInfo (TyVarPrim _ pts) = "∈" <+> pretty pts
prettyTyVarInfo (TyVarRecord _ fs) = pretty $ Scalar $ Record fs
prettyTyVarInfo (TyVarSum _ cs) = pretty $ Scalar $ Sum cs

instance Pretty (TyVarInfo ()) where
  pretty = prettyTyVarInfo

instance Pretty (TyVarInfo SComp) where
  pretty = prettyTyVarInfo

instance Located (TyVarInfo d) where
  locOf (TyVarFree loc _) = loc
  locOf (TyVarPrim loc _) = loc
  locOf (TyVarRecord loc _) = loc
  locOf (TyVarSum loc _) = loc

-- | The name of a type variable.
type TyVar = VName

-- | The level at which a type variable is bound.  Higher means
-- deeper.  We can only unify a type variable at level @i@ with a type
-- @t@ if all type names that occur in @t@ are at most at level @i@.
type Level = Int

-- | If a VName is not in this map, it should be in the 'TyParams' -
-- the exception is abstract types, which are just missing (and
-- assumed to have smallest possible level).
type TyVars d = M.Map TyVar (Level, TyVarInfo d)

-- | Explicit type parameters.
type TyParams = M.Map TyVar (Level, Liftedness, Loc)

data CtAM = CtAM (Reason (CtType SComp)) SVar SVar (Shape SComp)

instance Located CtAM where
  locOf (CtAM r _ _ _) = locOf r

instance Pretty CtAM where
  pretty (CtAM _ r m _) = prettyName r <+> "=" <+> "•" <+> "∨" <+> prettyName m <+> "=" <+> "•"
