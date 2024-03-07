module Language.Futhark.TypeChecker.Constraints
  ( Reason (..),
    SVar,
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
import Data.Loc
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.Util.Pretty
import Language.Futhark
import Language.Futhark.TypeChecker.Monad (TypeError (..))
import Language.Futhark.TypeChecker.Types (substTyVars)

-- | The reason for a type constraint. Used to generate type error
-- messages.
newtype Reason = Reason
  { reasonLoc :: Loc
  }
  deriving (Eq, Ord, Show)

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
  | CtAM SVar SVar (Shape SComp)
  deriving (Show)

instance Pretty Ct where
  pretty (CtEq t1 t2) = pretty t1 <+> "~" <+> pretty t2
  pretty (CtAM r m _) = prettyName r <+> "=" <+> "•" <+> "∨" <+> prettyName m <+> "=" <+> "•"

type Constraints = [Ct]

-- | Information about a type variable. Every type variable is
-- associated with a location, which is the original syntax element
-- that it is the type of.
data TyVarInfo
  = -- | Can be substituted with anything.
    TyVarFree Loc
  | -- | Can only be substituted with these primitive types.
    TyVarPrim Loc [PrimType]
  | -- | Must be a record with these fields.
    TyVarRecord Loc (M.Map Name Type)
  | -- | Must be a sum type with these fields.
    TyVarSum Loc (M.Map Name [Type])
  deriving (Show, Eq)

instance Pretty TyVarInfo where
  pretty (TyVarFree _) = "free"
  pretty (TyVarPrim _ pts) = "∈" <+> pretty pts
  pretty (TyVarRecord _ fs) = pretty $ Scalar $ Record fs
  pretty (TyVarSum _ cs) = pretty $ Scalar $ Sum cs

instance Located TyVarInfo where
  locOf (TyVarFree loc) = loc
  locOf (TyVarPrim loc _) = loc
  locOf (TyVarRecord loc _) = loc
  locOf (TyVarSum loc _) = loc

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

substTyVar :: (Monoid u) => M.Map TyVar TyVarSol -> VName -> Maybe (TypeBase SComp u)
substTyVar m v =
  case M.lookup v m of
    Just (TyVarLink v') -> substTyVar m v'
    Just (TyVarSol _ t') -> Just $ second (const mempty) $ substTyVars (substTyVar m) t'
    Just (TyVarUnsol {}) -> Nothing
    Nothing -> Nothing

lookupTyVar :: TyVar -> SolveM (Maybe Type)
lookupTyVar orig = do
  tyvars <- gets solverTyVars
  let f v = case M.lookup v tyvars of
        Nothing -> error $ "Unknown tyvar: " <> prettyNameString v
        Just (TyVarSol _ t) -> pure $ Just t
        Just (TyVarLink v') -> f v'
        Just (TyVarUnsol {}) -> pure Nothing
  f orig

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
      Just $ Right $ first (const ()) $ substTyVars (substTyVar (solverTyVars s)) t
    mkSubst (TyVarLink v') =
      Just . fromMaybe (Right $ Scalar $ TypeVar mempty (qualName v') []) $
        mkSubst =<< M.lookup v' (solverTyVars s)
    mkSubst (TyVarUnsol _ (TyVarPrim _ pts)) = Just $ Left pts
    mkSubst _ = Nothing

    unconstrained (v, TyVarUnsol _ (TyVarFree _)) = Just v
    unconstrained _ = Nothing

newtype SolveM a = SolveM {runSolveM :: StateT SolverState (Except TypeError) a}
  deriving (Functor, Applicative, Monad, MonadState SolverState, MonadError TypeError)

occursCheck :: VName -> Type -> SolveM ()
occursCheck v tp = do
  vars <- gets solverTyVars
  let tp' = substTyVars (substTyVar vars) tp
  when (v `S.member` typeVars tp') . throwError . TypeError mempty mempty $
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
  tyvars <- gets solverTyVars
  modify $ \s -> s {solverTyVars = M.insert v (TyVarLink t) $ solverTyVars s}
  tyvars' <-
    case (M.lookup v tyvars, M.lookup t tyvars) of
      (Just (TyVarUnsol _ info), Just (TyVarUnsol lvl (TyVarFree _))) ->
        pure $ M.insert t (TyVarUnsol lvl info) tyvars
      -- TODO: handle more cases.
      _ -> pure tyvars
  modify $ \s -> s {solverTyVars = M.insert v (TyVarLink t) tyvars'}

-- Unify at the root, emitting new equalities that must hold.
unify :: Type -> Type -> Maybe [(Type, Type)]
unify (Scalar (Prim pt1)) (Scalar (Prim pt2))
  | pt1 == pt2 = Just []
unify
  (Scalar (TypeVar _ (QualName _ v1) targs1))
  (Scalar (TypeVar _ (QualName _ v2) targs2))
    | v1 == v2 =
        Just $ mapMaybe f $ zip targs1 targs2
    where
      f (TypeArgType t1, TypeArgType t2) = Just (t1, t2)
      f _ = Nothing
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
    CtAM {} -> pure () -- Good vibes only.
  where
    bad = throwError $ TypeError mempty mempty $ "Unsolvable:" <+> pretty ct
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
        (t1', Scalar (TypeVar _ (QualName [] v2) []))
          | Just lvl <- flexible v2 ->
              subTyVar v2 lvl t1'
        (t1', t2') -> case unify t1' t2' of
          Nothing -> bad
          Just eqs -> mapM_ solveCt' eqs

solveTyVar :: (VName, (Int, TyVarInfo)) -> SolveM ()
solveTyVar (_, (_, TyVarFree {})) = pure ()
solveTyVar (tv, (_, TyVarPrim loc pts)) = do
  t <- lookupTyVar tv
  case t of
    Nothing -> pure ()
    Just t'
      | t' `elem` map (Scalar . Prim) pts -> pure ()
      | otherwise ->
          throwError . TypeError loc mempty $
            "Type must be one of"
              </> indent 2 (pretty pts)
              </> "but inferred to be"
              </> indent 2 (pretty t')
solveTyVar (tv, (_, TyVarRecord loc fs1)) = do
  tv_t <- lookupTyVar tv
  case tv_t of
    Nothing -> pure ()
    Just (Scalar (Record fs2))
      | all (`M.member` fs2) (M.keys fs1) ->
          forM_ (M.toList $ M.intersectionWith (,) fs1 fs2) $ \(_k, (t1, t2)) ->
            solveCt $ CtEq t1 t2
    Just tv_t' ->
      throwError $
        TypeError loc mempty $
          "Type must be record with fields"
            </> indent 2 (pretty (Scalar (Record fs1)))
            </> "but inferred to be"
            </> indent 2 (pretty tv_t')

solve :: Constraints -> TyVars -> Either TypeError ([VName], Solution)
solve constraints tyvars =
  second solution
    . runExcept
    . flip execStateT (initialState tyvars)
    . runSolveM
    $ do
      mapM_ solveCt constraints
      mapM_ solveTyVar (M.toList tyvars)
{-# NOINLINE solve #-}
