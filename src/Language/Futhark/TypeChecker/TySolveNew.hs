module Language.Futhark.TypeChecker.TySolveNew
  ( Type,
    Solution,
    UnconTyVar,
    solve,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor
import Data.List qualified as L
import Data.Loc
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.Util.Pretty
import Language.Futhark
import Language.Futhark.TypeChecker.Constraints
import Language.Futhark.TypeChecker.Error
import Language.Futhark.TypeChecker.Monad (Notes, TypeError (..), aNote)
import Language.Futhark.TypeChecker.Types (substTyVars)
import Language.Futhark.TypeChecker.UnionFind

-- | The type representation used by the constraint solver. Agnostic
-- to sizes and uniqueness.
type Type = CtType ()

-- | A (partial) solution for a type variable.
data TyVarSol
  = -- | Has been substituted with this.
    TyVarSol Type
  | -- | Is an explicit (rigid) type parameter in the source program.
    TyVarParam Level Liftedness Loc
  | -- | Not substituted yet; has this constraint.
    TyVarUnsol (TyVarInfo ())
  deriving (Show)

newtype SolverState s = SolverState
  { -- | Left means linked to this other type variable.
    solverTyVars :: M.Map TyVar (VarNode s)
  }

-- | A solution maps a type variable to its substitution. This
-- substitution is complete, in the sense there are no right-hand
-- sides that contain a type variable.
type Solution = M.Map TyVar (Either [PrimType] (TypeBase () NoUniqueness))

-- | An unconstrained type variable comprises a name and (ironically)
-- a constraint on how it can be instantiated.
type UnconTyVar = (VName, Liftedness)

newtype SolveM a = SolveM {runSolveM :: StateT SolverState (Except TypeError) a}
  deriving (Functor, Applicative, Monad, MonadState SolverState, MonadError TypeError)

initialState :: TyParams -> TyVars () -> SolverState
initialState _typarams _tyvars = undefined

solution :: SolverState -> ([UnconTyVar], Solution)
solution _s = undefined

solveCt :: CtTy () -> SolveM ()
solveCt _ct = undefined

solveTyVar :: (VName, (Level, TyVarInfo ())) -> SolveM ()
solveTyVar _ = undefined

-- | Solve type constraints, producing either an error or a solution,
-- alongside a list of unconstrained type variables.
solve ::
  [CtTy ()] ->
  TyParams ->
  TyVars () ->
  Either TypeError ([UnconTyVar], Solution)
solve constraints typarams tyvars =
  second solution
    . runExcept
    . flip execStateT (initialState typarams tyvars)
    . runSolveM
    $ do
      mapM_ solveCt constraints
      mapM_ solveTyVar (M.toList tyvars)
{-# NOINLINE solve #-}