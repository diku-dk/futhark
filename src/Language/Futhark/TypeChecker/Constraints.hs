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
    TyParams,
    Solution,
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
import Language.Futhark.TypeChecker.Monad (TypeError (..))
import Language.Futhark.TypeChecker.Types (substTyVars)

-- | The reason for a type constraint. Used to generate type error
-- messages.
newtype Reason = Reason
  { reasonLoc :: Loc
  }
  deriving (Eq, Ord, Show)

instance Located Reason where
  locOf = reasonLoc

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
  = CtEq Reason Type Type
  | CtAM Reason SVar SVar (Shape SComp)
  deriving (Show)

ctReason :: Ct -> Reason
ctReason (CtEq r _ _) = r
ctReason (CtAM r _ _ _) = r

instance Located Ct where
  locOf = locOf . ctReason

instance Pretty Ct where
  pretty (CtEq _ t1 t2) = pretty t1 <+> "~" <+> pretty t2
  pretty (CtAM _ r m _) = prettyName r <+> "=" <+> "•" <+> "∨" <+> prettyName m <+> "=" <+> "•"

type Constraints = [Ct]

-- | Information about a flexible type variable. Every type variable
-- is associated with a location, which is the original syntax element
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
  | -- | Must be a type that supports equality.
    TyVarEql Loc
  deriving (Show, Eq)

instance Pretty TyVarInfo where
  pretty (TyVarFree _) = "free"
  pretty (TyVarPrim _ pts) = "∈" <+> pretty pts
  pretty (TyVarRecord _ fs) = pretty $ Scalar $ Record fs
  pretty (TyVarSum _ cs) = pretty $ Scalar $ Sum cs
  pretty (TyVarEql _) = "equality"

instance Located TyVarInfo where
  locOf (TyVarFree loc) = loc
  locOf (TyVarPrim loc _) = loc
  locOf (TyVarRecord loc _) = loc
  locOf (TyVarSum loc _) = loc
  locOf (TyVarEql loc) = loc

type TyVar = VName

-- | The level at which a type variable is bound.  Higher means
-- deeper.  We can only unify a type variable at level @i@ with a type
-- @t@ if all type names that occur in @t@ are at most at level @i@.
type Level = Int

-- | If a VName is not in this map, it should be in the 'TyParams' -
-- the exception is abstract types, which are just missing (and
-- assumed to have smallest possible level).
type TyVars = M.Map TyVar (Level, TyVarInfo)

-- | Explicit type parameters.
type TyParams = M.Map TyVar (Level, Loc)

data TyVarSol
  = -- | Has been substituted with this.
    TyVarSol Level Type
  | -- | Is an explicit (rigid) type parameter in the source program.
    TyVarParam Level Loc
  | -- | Not substituted yet; has this constraint.
    TyVarUnsol Level TyVarInfo
  deriving (Show)

newtype SolverState = SolverState
  { -- | Left means linked to this other type variable.
    solverTyVars :: M.Map TyVar (Either VName TyVarSol)
  }

initialState :: TyParams -> TyVars -> SolverState
initialState typarams tyvars = SolverState $ M.map g typarams <> M.map f tyvars
  where
    f (lvl, info) = Right $ TyVarUnsol lvl info
    g (lvl, loc) = Right $ TyVarParam lvl loc

substTyVar :: (Monoid u) => M.Map TyVar (Either VName TyVarSol) -> VName -> Maybe (TypeBase SComp u)
substTyVar m v =
  case M.lookup v m of
    Just (Left v') -> substTyVar m v'
    Just (Right (TyVarSol _ t')) -> Just $ second (const mempty) $ substTyVars (substTyVar m) t'
    Just (Right TyVarParam {}) -> Nothing
    Just (Right (TyVarUnsol {})) -> Nothing
    Nothing -> Nothing

lookupTyVar :: TyVar -> SolveM (Int, Either TyVarInfo Type)
lookupTyVar orig = do
  tyvars <- gets solverTyVars
  let f v = case M.lookup v tyvars of
        Nothing -> error $ "Unknown tyvar: " <> prettyNameString v
        Just (Left v') -> f v'
        Just (Right (TyVarSol lvl t)) -> pure (lvl, Right t)
        Just (Right (TyVarParam lvl _)) ->
          pure (lvl, Right $ Scalar $ TypeVar mempty (qualName orig) [])
        Just (Right (TyVarUnsol lvl info)) -> pure (lvl, Left info)
  f orig

setLink :: TyVar -> VName -> SolveM ()
setLink v info = modify $ \s -> s {solverTyVars = M.insert v (Left info) $ solverTyVars s}

setInfo :: TyVar -> TyVarSol -> SolveM ()
setInfo v info = modify $ \s -> s {solverTyVars = M.insert v (Right info) $ solverTyVars s}

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
    mkSubst (Right (TyVarSol _lvl t)) =
      Just $ Right $ first (const ()) $ substTyVars (substTyVar (solverTyVars s)) t
    mkSubst (Left v') =
      Just . fromMaybe (Right $ Scalar $ TypeVar mempty (qualName v') []) $
        mkSubst =<< M.lookup v' (solverTyVars s)
    mkSubst (Right (TyVarUnsol _ (TyVarPrim _ pts))) = Just $ Left pts
    mkSubst _ = Nothing

    unconstrained (v, Right (TyVarUnsol _ (TyVarFree _))) = Just v
    unconstrained _ = Nothing

newtype SolveM a = SolveM {runSolveM :: StateT SolverState (Except TypeError) a}
  deriving (Functor, Applicative, Monad, MonadState SolverState, MonadError TypeError)

occursCheck :: Reason -> VName -> Type -> SolveM ()
occursCheck reason v tp = do
  vars <- gets solverTyVars
  let tp' = substTyVars (substTyVar vars) tp
  when (v `S.member` typeVars tp') . throwError . TypeError (locOf reason) mempty $
    "Occurs check: cannot instantiate"
      <+> prettyName v
      <+> "with"
      <+> pretty tp
      <> "."

unifySharedConstructors ::
  Reason ->
  M.Map Name [Type] ->
  M.Map Name [Type] ->
  SolveM ()
unifySharedConstructors reason cs1 cs2 =
  forM_ (M.toList $ M.intersectionWith (,) cs1 cs2) $ \(c, (ts1, ts2)) ->
    if length ts1 == length ts2
      then zipWithM (solveEq reason) ts1 ts2
      else
        throwError . TypeError (locOf reason) mempty $
          "Cannot unify type with constructor"
            </> indent 2 (pretty (Sum (M.singleton c ts1)))
            </> "with type of constructor"
            </> indent 2 (pretty (Sum (M.singleton c ts2)))
            </> "because they differ in arity."

unifySharedFields ::
  Reason ->
  M.Map Name Type ->
  M.Map Name Type ->
  SolveM ()
unifySharedFields reason fs1 fs2 =
  forM_ (M.toList $ M.intersectionWith (,) fs1 fs2) $ \(_f, (ts1, ts2)) ->
    solveEq reason ts1 ts2

mustSupportEql :: Reason -> Type -> SolveM ()
mustSupportEql _reason _t = pure ()

scopeViolation :: Reason -> VName -> Type -> VName -> SolveM a
scopeViolation reason v1 ty v2 =
  throwError . TypeError (locOf reason) mempty $
    "Cannot unify type"
      </> indent 2 (pretty ty)
      </> "with"
      <+> dquotes (prettyName v1)
      <+> "(scope violation)."
      </> "This is because"
      <+> dquotes (prettyName v2)
      <+> "is rigidly bound in a deeper scope."

scopeCheck :: Reason -> TyVar -> Int -> Type -> SolveM ()
scopeCheck reason v v_lvl ty = do
  mapM_ check $ typeVars ty
  where
    check ty_v = do
      ty_v_info <- gets $ M.lookup ty_v . solverTyVars
      case ty_v_info of
        Just (Right (TyVarParam ty_v_lvl _))
          | ty_v_lvl > v_lvl -> scopeViolation reason v ty ty_v
        Just (Right (TyVarUnsol ty_v_lvl info))
          | ty_v_lvl /= v_lvl ->
              setInfo ty_v $ TyVarUnsol v_lvl info
        _ -> pure ()

-- Precondition: 'v' is currently flexible.
subTyVar :: Reason -> VName -> Int -> Type -> SolveM ()
subTyVar reason v v_lvl t = do
  occursCheck reason v t
  v_info <- gets $ M.lookup v . solverTyVars
  scopeCheck reason v v_lvl t
  case (v_info, t) of
    (Just (Right (TyVarUnsol _ TyVarFree {})), _) ->
      pure ()
    ( Just (Right (TyVarUnsol _ (TyVarPrim _ v_pts))),
      _
      ) ->
        if t `elem` map (Scalar . Prim) v_pts
          then pure ()
          else
            throwError . TypeError (locOf reason) mempty $
              "Cannot unify type that must be one of"
                </> indent 2 (pretty v_pts)
                </> "with"
                </> indent 2 (pretty t)
    ( Just (Right (TyVarUnsol _ (TyVarSum _ cs1))),
      Scalar (Sum cs2)
      ) ->
        if all (`elem` M.keys cs2) (M.keys cs1)
          then unifySharedConstructors reason cs1 cs2
          else
            throwError . TypeError (locOf reason) mempty $
              "Cannot unify type with constructors"
                </> indent 2 (pretty (Sum cs1))
                </> "with type"
                </> indent 2 (pretty (Sum cs2))
    ( Just (Right (TyVarUnsol _ (TyVarSum _ cs1))),
      _
      ) ->
        throwError . TypeError (locOf reason) mempty $
          "Cannot unify type with constructors"
            </> indent 2 (pretty (Sum cs1))
            </> "with type"
            </> indent 2 (pretty t)
    ( Just (Right (TyVarUnsol _ (TyVarRecord _ fs1))),
      Scalar (Record fs2)
      ) ->
        if all (`elem` M.keys fs2) (M.keys fs1)
          then unifySharedFields reason fs1 fs2
          else
            throwError . TypeError (locOf reason) mempty $
              "Cannot unify record type with fields"
                </> indent 2 (pretty (Record fs1))
                </> "with record type"
                </> indent 2 (pretty (Record fs2))
    ( Just (Right (TyVarUnsol _ (TyVarRecord _ fs1))),
      _
      ) ->
        throwError . TypeError (locOf reason) mempty $
          "Cannot unify record type with fields"
            </> indent 2 (pretty (Record fs1))
            </> "with type"
            </> indent 2 (pretty t)
    (Just (Right (TyVarUnsol _ (TyVarEql _))), _) ->
      mustSupportEql reason t
    --
    -- Internal error cases
    (Just (Right TyVarSol {}), _) ->
      error $ "Type variable already solved: " <> prettyNameString v
    (Just (Right TyVarParam {}), _) ->
      error $ "Cannot substitute type parameter: " <> prettyNameString v
    (Just Left {}, _) ->
      error $ "Type variable already linked: " <> prettyNameString v
    (Nothing, _) ->
      error $ "subTyVar: Nothing v: " <> prettyNameString v

  setInfo v (TyVarSol v_lvl t)

-- Precondition: 'v' and 't' are both currently flexible.
unionTyVars :: Reason -> VName -> VName -> SolveM ()
unionTyVars reason v t = do
  v_info <- gets $ either alreadyLinked id . fromMaybe unknown . M.lookup v . solverTyVars
  (lvl, t') <- lookupTyVar t

  case (v_info, t') of
    -- When either is completely unconstrained.
    (TyVarUnsol _ TyVarFree {}, _) ->
      pure ()
    ( TyVarUnsol _ info,
      Left (TyVarFree {})
      ) ->
        setInfo t (TyVarUnsol lvl info)
    --
    -- TyVarPrim cases
    ( TyVarUnsol _ info@TyVarPrim {},
      Left TyVarEql {}
      ) ->
        setInfo t (TyVarUnsol lvl info)
    ( TyVarUnsol _ (TyVarPrim _ v_pts),
      Left (TyVarPrim t_loc t_pts)
      ) ->
        let pts = L.intersect v_pts t_pts
         in if null pts
              then
                throwError . TypeError (locOf reason) mempty $
                  "Cannot unify type that must be one of"
                    </> indent 2 (pretty v_pts)
                    </> "with type that must be one of"
                    </> indent 2 (pretty t_pts)
              else setInfo t (TyVarUnsol lvl (TyVarPrim t_loc pts))
    ( TyVarUnsol _ (TyVarPrim _ v_pts),
      Left TyVarRecord {}
      ) ->
        throwError . TypeError (locOf reason) mempty $
          "Cannot unify type that must be one of"
            </> indent 2 (pretty v_pts)
            </> "with type that must be record."
    ( TyVarUnsol _ (TyVarPrim _ v_pts),
      Left TyVarSum {}
      ) ->
        throwError . TypeError (locOf reason) mempty $
          "Cannot unify type that must be one of"
            </> indent 2 (pretty v_pts)
            </> "with type that must be sum."
    --
    -- TyVarSum cases
    ( TyVarUnsol _ (TyVarSum _ cs1),
      Left (TyVarSum loc cs2)
      ) -> do
        unifySharedConstructors reason cs1 cs2
        let cs3 = cs1 <> cs2
        setInfo t (TyVarUnsol lvl (TyVarSum loc cs3))
    ( TyVarUnsol _ TyVarSum {},
      Left (TyVarPrim _ pts)
      ) ->
        throwError . TypeError (locOf reason) mempty $
          "A sum type cannot be one of"
            </> indent 2 (pretty pts)
    ( TyVarUnsol _ (TyVarSum _ cs1),
      Left (TyVarRecord _ fs)
      ) ->
        throwError . TypeError (locOf reason) mempty $
          "Cannot unify type with constructors"
            </> indent 2 (pretty (Sum cs1))
            </> "with type"
            </> indent 2 (pretty (Scalar (Record fs)))
    ( TyVarUnsol _ (TyVarSum _ cs1),
      Left (TyVarEql _)
      ) ->
        mapM_ (mapM_ (mustSupportEql reason)) cs1
    --
    -- TyVarRecord cases
    ( TyVarUnsol _ (TyVarRecord _ fs1),
      Left (TyVarRecord loc fs2)
      ) -> do
        unifySharedFields reason fs1 fs2
        let fs3 = fs1 <> fs2
        setInfo t (TyVarUnsol lvl (TyVarRecord loc fs3))
    ( TyVarUnsol _ TyVarRecord {},
      Left (TyVarPrim _ pts)
      ) ->
        throwError . TypeError (locOf reason) mempty $
          "A record type cannot be one of"
            </> indent 2 (pretty pts)
    ( TyVarUnsol _ (TyVarRecord _ fs1),
      Left (TyVarSum _ cs)
      ) ->
        throwError . TypeError (locOf reason) mempty $
          "Cannot unify record type"
            </> indent 2 (pretty (Record fs1))
            </> "with type"
            </> indent 2 (pretty (Scalar (Sum cs)))
    ( TyVarUnsol _ (TyVarRecord _ fs1),
      Left (TyVarEql _)
      ) ->
        mapM_ (mustSupportEql reason) fs1
    --
    -- TyVarEql cases
    (TyVarUnsol _ (TyVarEql _), Left TyVarPrim {}) ->
      pure ()
    (TyVarUnsol _ (TyVarEql _), Left TyVarEql {}) ->
      pure ()
    (TyVarUnsol _ (TyVarEql _), Left (TyVarRecord _ fs)) ->
      mustSupportEql reason $ Scalar $ Record fs
    (TyVarUnsol _ (TyVarEql _), Left (TyVarSum _ cs)) ->
      mustSupportEql reason $ Scalar $ Sum cs
    --
    -- Internal error cases
    (TyVarSol {}, _) ->
      alreadySolved
    (TyVarParam {}, _) ->
      isParam
    (_, Right t'') ->
      error $ "unionTyVars: rhs " <> prettyNameString t <> " is solved as " <> prettyString t''

  -- Finally insert the actual link.
  setLink v t
  where
    unknown = error $ "unionTyVars: Nothing v: " <> prettyNameString v
    alreadyLinked = error $ "Type variable already linked: " <> prettyNameString v
    alreadySolved = error $ "Type variable already solved: " <> prettyNameString v
    isParam = error $ "Type name is a type parameter: " <> prettyNameString v

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

solveEq :: Reason -> Type -> Type -> SolveM ()
solveEq reason orig_t1 orig_t2 = do
  solveCt' (orig_t1, orig_t2)
  where
    cannotUnify = do
      tyvars <- gets solverTyVars
      throwError . TypeError (locOf reason) mempty $
        "Cannot unify"
          </> indent 2 (pretty (substTyVars (substTyVar tyvars) orig_t1))
          </> "with"
          </> indent 2 (pretty (substTyVars (substTyVar tyvars) orig_t2))

    solveCt' (t1, t2) = do
      tyvars <- gets solverTyVars
      let flexible v = case M.lookup v tyvars of
            Just (Left v') -> flexible v'
            Just (Right (TyVarUnsol lvl _)) -> Just lvl
            Just (Right TyVarSol {}) -> Nothing
            Just (Right TyVarParam {}) -> Nothing
            Nothing -> Nothing
          sub t@(Scalar (TypeVar u (QualName [] v) [])) =
            case M.lookup v tyvars of
              Just (Left v') -> sub $ Scalar (TypeVar u (QualName [] v') [])
              Just (Right (TyVarSol _ t')) -> sub t'
              _ -> t
          sub t = t
      case (sub t1, sub t2) of
        ( t1'@(Scalar (TypeVar _ (QualName [] v1) [])),
          t2'@(Scalar (TypeVar _ (QualName [] v2) []))
          )
            | v1 == v2 -> pure ()
            | otherwise ->
                case (flexible v1, flexible v2) of
                  (Nothing, Nothing) -> cannotUnify
                  (Just lvl, Nothing) -> subTyVar reason v1 lvl t2'
                  (Nothing, Just lvl) -> subTyVar reason v2 lvl t1'
                  (Just lvl1, Just lvl2)
                    | lvl1 <= lvl2 -> unionTyVars reason v1 v2
                    | otherwise -> unionTyVars reason v2 v1
        (Scalar (TypeVar _ (QualName [] v1) []), t2')
          | Just lvl <- flexible v1 ->
              subTyVar reason v1 lvl t2'
        (t1', Scalar (TypeVar _ (QualName [] v2) []))
          | Just lvl <- flexible v2 ->
              subTyVar reason v2 lvl t1'
        (t1', t2') -> case unify t1' t2' of
          Nothing -> cannotUnify
          Just eqs -> mapM_ solveCt' eqs

solveCt :: Ct -> SolveM ()
solveCt ct =
  case ct of
    CtEq reason t1 t2 -> solveEq reason t1 t2
    CtAM {} -> pure () -- Good vibes only.

solveTyVar :: (VName, (Int, TyVarInfo)) -> SolveM ()
solveTyVar (tv, (_, TyVarRecord loc fs1)) = do
  (_, tv_t) <- lookupTyVar tv
  case tv_t of
    Left _ ->
      throwError . TypeError loc mempty $
        "Type"
          <+> prettyName tv
          <+> "is ambiguous."
          </> "Must be a record with fields"
          </> indent 2 (pretty (Scalar (Record fs1)))
    Right _ ->
      pure ()
solveTyVar (tv, (_, TyVarSum loc cs1)) = do
  (_, tv_t) <- lookupTyVar tv
  case tv_t of
    Left _ ->
      throwError . TypeError loc mempty $
        "Type is ambiguous."
          </> "Must be a sum type with constructors"
          </> indent 2 (pretty (Scalar (Sum cs1)))
    Right _ -> pure ()
solveTyVar (tv, (_, TyVarEql loc)) = do
  (_, tv_t) <- lookupTyVar tv
  case tv_t of
    Left _ -> pure ()
    Right ty
      | orderZero ty -> pure ()
      | otherwise ->
          throwError . TypeError loc mempty $
            "Type"
              </> indent 2 (align (pretty ty))
              </> "does not support equality (may contain function)."
solveTyVar (_, _) =
  pure ()

solve :: Constraints -> TyParams -> TyVars -> Either TypeError ([VName], Solution)
solve constraints typarams tyvars =
  second solution
    . runExcept
    . flip execStateT (initialState typarams tyvars)
    . runSolveM
    $ do
      mapM_ solveCt constraints
      mapM_ solveTyVar (M.toList tyvars)
{-# NOINLINE solve #-}
