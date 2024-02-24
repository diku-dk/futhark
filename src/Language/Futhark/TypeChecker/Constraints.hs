module Language.Futhark.TypeChecker.Constraints
  ( SVar,
    SComp (..),
    Type,
    toType,
    Ct (..),
    Constraints,
    TyVarInfo (..),
    TyVar,
    TyVars,
    Solution,
    solve,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Futhark.Util.Pretty
import Language.Futhark

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

-- | The type representation used by the constraint solver. Agnostic
-- to sizes.
type Type = TypeBase SComp NoUniqueness

-- | Careful when using this on something that already has an SComp
-- size: it will throw away information by converting them to SDim.
toType :: TypeBase Size u -> TypeBase SComp u
toType = first (const SDim)

data Ct
  = CtEq Type Type
  | CtAM SVar SVar
  deriving (Show)

instance Pretty Ct where
  pretty (CtEq t1 t2) = pretty t1 <+> "~" <+> pretty t2
  pretty (CtAM r m) = prettyName r <+> "=" <+> "•" <+> "∨" <+> prettyName m <+> "=" <+> "•"

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
  deriving (Show, Eq)

instance Pretty TyVarInfo where
  pretty TyVarFree = "free"
  pretty (TyVarPrim pts) = "∈" <+> pretty pts
  pretty (TyVarRecord fs) = pretty $ Scalar $ Record fs
  pretty (TyVarSum cs) = pretty $ Scalar $ Sum cs

type TyVar = VName

-- | If a VName is not in this map, it is assumed to be rigid. The
-- integer is the level.
type TyVars = M.Map TyVar (Int, TyVarInfo)

data TyVarSol
  = -- | Has been substituted with this.
    TyVarSol Int Type
  | -- | Replaced by this other type variable.
    TyVarLink VName
  | -- | Not substituted yet; has this constraint.
    TyVarUnsol Int TyVarInfo
  deriving (Show)

newtype SolverState = SolverState {solverTyVars :: M.Map TyVar TyVarSol}

initialState :: TyVars -> SolverState
initialState tyvars = SolverState $ M.map (uncurry TyVarUnsol) tyvars

substTyVars :: (Monoid u) => M.Map TyVar TyVarSol -> TypeBase SComp u -> TypeBase SComp u
substTyVars m t@(Scalar (TypeVar u (QualName qs v) args)) =
  case M.lookup v m of
    Just (TyVarLink v') ->
      substTyVars m $ Scalar $ TypeVar u (QualName qs v') args
    Just (TyVarSol _ t') -> second (const mempty) $ substTyVars m t'
    Just (TyVarUnsol {}) -> t
    Nothing -> t
substTyVars _ (Scalar (Prim pt)) = Scalar $ Prim pt
substTyVars m (Scalar (Record fs)) = Scalar $ Record $ M.map (substTyVars m) fs
substTyVars m (Scalar (Sum cs)) = Scalar $ Sum $ M.map (map $ substTyVars m) cs
substTyVars m (Scalar (Arrow u pname d t1 (RetType ext t2))) =
  Scalar $ Arrow u pname d (substTyVars m t1) $ RetType ext $ substTyVars m t2 `setUniqueness` uniqueness t2
substTyVars m (Array u shape elemt) =
  arrayOfWithAliases u shape $ substTyVars m $ Scalar elemt

-- | A solution maps a type variable to its substitution. This
-- substitution is complete, in the sense there are no right-hand
-- sides that contain a type variable.
type Solution = M.Map TyVar (Either [PrimType] (TypeBase () NoUniqueness))

solution :: SolverState -> ([VName], Solution)
solution s =
  ( mapMaybe unconstrained $ M.toList $ solverTyVars s,
    M.mapMaybe mkSubst $ solverTyVars s
  )
  where
    mkSubst (TyVarSol _lvl t) =
      Just $ Right $ first (const ()) $ substTyVars (solverTyVars s) t
    mkSubst (TyVarLink v') =
      Just . fromMaybe (Right $ Scalar $ TypeVar mempty (qualName v') []) $
        mkSubst =<< M.lookup v' (solverTyVars s)
    mkSubst (TyVarUnsol _ (TyVarPrim pts)) = Just $ Left pts
    mkSubst _ = Nothing

    unconstrained (v, TyVarUnsol _ TyVarFree) = Just v
    unconstrained _ = Nothing

newtype SolveM a = SolveM {runSolveM :: StateT SolverState (Except T.Text) a}
  deriving (Functor, Applicative, Monad, MonadState SolverState, MonadError T.Text)

occursCheck :: VName -> Type -> SolveM ()
occursCheck v tp = do
  vars <- gets solverTyVars
  let tp' = substTyVars vars tp
  when (v `S.member` typeVars tp') . throwError . docText $
    "Occurs check: cannot instantiate"
      <+> prettyName v
      <+> "with"
      <+> pretty tp
      <> "."

subTyVar :: VName -> Int -> Type -> SolveM ()
subTyVar v lvl t = do
  occursCheck v t
  modify $ \s -> s {solverTyVars = M.insert v (TyVarSol lvl t) $ solverTyVars s}

linkTyVar :: VName -> VName -> SolveM ()
linkTyVar v t = do
  occursCheck v $ Scalar $ TypeVar NoUniqueness (qualName t) []
  modify $ \s -> s {solverTyVars = M.insert v (TyVarLink t) $ solverTyVars s}

-- Unify at the root, emitting new equalities that must hold.
unify :: Type -> Type -> Maybe [(Type, Type)]
unify (Scalar (Prim pt1)) (Scalar (Prim pt2))
  | pt1 == pt2 = Just []
unify (Scalar (Arrow _ _ _ t1a (RetType _ t1r))) (Scalar (Arrow _ _ _ t2a (RetType _ t2r))) =
  Just [(t1a, t2a), (t1r', t2r')]
  where
    t1r' = t1r `setUniqueness` NoUniqueness
    t2r' = t2r `setUniqueness` NoUniqueness
unify (Scalar (Record fs1)) (Scalar (Record fs2))
  | M.keys fs1 == M.keys fs2 =
      Just $ M.elems $ M.intersectionWith (,) fs1 fs2
unify (Scalar (Sum cs1)) (Scalar (Sum cs2))
  | M.keys cs1 == M.keys cs2 =
      fmap concat . forM cs' $ \(ts1, ts2) -> do
        guard $ length ts1 == length ts2
        Just $ zip ts1 ts2
  where
    cs' = M.elems $ M.intersectionWith (,) cs1 cs2
unify t1 t2
  | Just t1' <- peelArray 1 t1,
    Just t2' <- peelArray 1 t2 =
      Just [(t1', t2')]
unify _ _ = Nothing

solveCt :: Ct -> SolveM ()
solveCt ct =
  case ct of
    CtEq t1 t2 -> solveCt' (t1, t2)
    CtAM _ _ -> pure () -- Good vibes only.
  where
    bad = throwError $ "Unsolvable: " <> prettyText ct
    solveCt' (t1, t2) = do
      tyvars <- gets solverTyVars
      let flexible v = case M.lookup v tyvars of
            Just (TyVarLink v') -> flexible v'
            Just (TyVarUnsol lvl _) -> Just lvl
            Just (TyVarSol _ _) -> Nothing
            Nothing -> Nothing
          sub t@(Scalar (TypeVar u (QualName [] v) [])) =
            case M.lookup v tyvars of
              Just (TyVarLink v') -> sub $ Scalar (TypeVar u (QualName [] v') [])
              Just (TyVarSol _ t') -> sub t'
              _ -> t
          sub t = t
      case (sub t1, sub t2) of
        ( t1'@(Scalar (TypeVar _ (QualName [] v1) [])),
          t2'@(Scalar (TypeVar _ (QualName [] v2) []))
          )
            | v1 == v2 -> pure ()
            | otherwise ->
                case (flexible v1, flexible v2) of
                  (Nothing, Nothing) -> bad
                  (Just lvl, Nothing) -> subTyVar v1 lvl t2'
                  (Nothing, Just lvl) -> subTyVar v2 lvl t1'
                  (Just lvl1, Just lvl2)
                    | lvl1 <= lvl2 -> linkTyVar v1 v2
                    | otherwise -> linkTyVar v2 v1
        (Scalar (TypeVar _ (QualName [] v1) []), t2')
          | Just lvl <- flexible v1 ->
              subTyVar v1 lvl t2'
          | otherwise ->
              bad
        (t1', Scalar (TypeVar _ (QualName [] v2) []))
          | Just lvl <- flexible v2 ->
              subTyVar v2 lvl t1'
          | otherwise ->
              bad
        (t1', t2') -> case unify t1' t2' of
          Nothing -> bad
          Just eqs -> mapM_ solveCt' eqs

solve :: Constraints -> TyVars -> Either T.Text ([VName], Solution)
solve constraints tyvars =
  second solution
    . runExcept
    . flip execStateT (initialState tyvars)
    . runSolveM
    $ mapM solveCt constraints
{-# NOINLINE solve #-}
