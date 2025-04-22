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
import Control.Monad.ST
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

type UF s = M.Map TyVar (TyVarNode s)

type Unifier s = (UF s, [CtTy ()])

newtype SolverState s = SolverState { solverTyVars :: UF s }

newtype SolveM s a = SolveM { runSolveM :: StateT (SolverState s) (ExceptT TypeError (ST s)) a }
  deriving (Functor, Applicative, Monad, MonadError TypeError, MonadState (SolverState s))

-- | A solution maps a type variable to its substitution. This
-- substitution is complete, in the sense there are no right-hand
-- sides that contain a type variable.
type Solution = M.Map TyVar (Either [PrimType] (TypeBase () NoUniqueness))

-- | An unconstrained type variable comprises a name and (ironically)
-- a constraint on how it can be instantiated.
type UnconTyVar = (VName, Liftedness)

liftST :: ST s a -> SolveM s a
liftST = SolveM . lift . lift

initialState :: TyParams -> TyVars () -> SolveM s ()
initialState typarams tyvars = do
  tyvars' <- M.traverseWithKey f tyvars
  typarams' <- M.traverseWithKey g typarams
  put $ SolverState $ typarams' <> tyvars'

  where
    f tv (lvl, info) = liftST $ makeTyVarNode tv lvl info
    g tv (lvl, lft, loc) = liftST $ makeTyParamNode tv lvl lft loc

solution :: SolverState s -> ([UnconTyVar], Solution)
solution _s = undefined

solveCt :: CtTy () -> SolveM s ()
solveCt (CtEq reason t1 t2) = solveEq reason mempty t1 t2

solveEq :: Reason Type -> BreadCrumbs -> Type -> Type -> SolveM s ()
solveEq _reason _obcs _orig_t1 _orig_t2 = undefined

solveTyVar :: (VName, (Level, TyVarInfo ())) -> SolveM s (UF s)
solveTyVar _ = gets solverTyVars -- Just a test to see if things typecheck.

-- | Solve type constraints, producing either an error or a solution,
-- alongside a list of unconstrained type variables.
solve ::
  [CtTy ()] ->
  TyParams ->
  TyVars () ->
  Either TypeError ([UnconTyVar], Solution)
solve _constraints _typarams _tyvars = undefined
{-# NOINLINE solve #-}