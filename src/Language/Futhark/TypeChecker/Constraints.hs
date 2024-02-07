module Language.Futhark.TypeChecker.Constraints
  ( Type,
    Ct (..),
    Constraints,
    TyVarInfo (..),
    TyVar,
    TyVars,
    solve,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor
import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.Util.Pretty
import Language.Futhark

-- | A shape component is currently just unit. The rank of an array is
-- then just the number of shape components it contains in its shape
-- list. When we add AUTOMAP, these components will also allow shape
-- variables. The list of components should then be understood as
-- concatenation of shapes (meaning you can't just take the length to
-- determine the rank of the array).
type SComp = ()

type Type = TypeBase SComp NoUniqueness

data Ct = CtEq Type Type
  deriving (Show)

instance Pretty Ct where
  pretty (CtEq t1 t2) = pretty t1 <+> "~" <+> pretty t2

type Constraints = [Ct]

-- | Information about a type variable.
data TyVarInfo
  = -- | Can be substituted with anything.
    TyVarFree
  | -- | Can only be substituted with these primitive types.
    TyVarPrim [PrimType]
  | -- | Must be a record with these fields.
    TyVarRecord (M.Map Name Type)
  | -- | Must be a sum type with these fields.
    TyVarSum (M.Map Name [Type])
  deriving (Show)

instance Pretty TyVarInfo where
  pretty TyVarFree = "free"
  pretty (TyVarPrim pts) = "∈" <+> pretty pts
  pretty (TyVarRecord fs) = pretty $ Scalar $ Record fs
  pretty (TyVarSum cs) = pretty $ Scalar $ Sum cs

type TyVar = VName

-- | If a VName is not in this map, it is assumed to be rigid.
type TyVars = M.Map TyVar TyVarInfo

data TyVarSol
  = -- | Has been substituted with this.
    TyVarSol Type
  | -- | Not substituted yet; has this constraint.
    TyVarUnsol TyVarInfo
  deriving (Show)

newtype SolverState = SolverState {solverTyVars :: M.Map TyVar TyVarSol}

initialState :: TyVars -> SolverState
initialState tyvars = SolverState $ M.map TyVarUnsol tyvars

solution :: SolverState -> M.Map TyVar Type
solution = undefined

newtype SolveM a = SolveM {runSolveM :: StateT SolverState (Except T.Text) a}
  deriving (Functor, Applicative, Monad, MonadState SolverState, MonadError T.Text)

solve :: Constraints -> TyVars -> Either T.Text (M.Map TyVar Type)
solve constraints tyvars =
  second solution
    . runExcept
    . flip execStateT (initialState tyvars)
    . runSolveM
    $ throwError "cannot solve"
{-# NOINLINE solve #-}
