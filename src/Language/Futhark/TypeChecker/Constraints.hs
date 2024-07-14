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
import Language.Futhark.TypeChecker.Monad (Notes, TypeError (..))
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
    TyVarFree Loc Liftedness
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
  pretty (TyVarFree _ l) = "free" <+> pretty l
  pretty (TyVarPrim _ pts) = "∈" <+> pretty pts
  pretty (TyVarRecord _ fs) = pretty $ Scalar $ Record fs
  pretty (TyVarSum _ cs) = pretty $ Scalar $ Sum cs
  pretty (TyVarEql _) = "equality"

instance Located TyVarInfo where
  locOf (TyVarFree loc _) = loc
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
type TyParams = M.Map TyVar (Level, Liftedness, Loc)

data TyVarSol
  = -- | Has been substituted with this.
    TyVarSol Level Type
  | -- | Is an explicit (rigid) type parameter in the source program.
    TyVarParam Level Liftedness Loc
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
    g (lvl, l, loc) = Right $ TyVarParam lvl l loc

substTyVar :: (Monoid u) => M.Map TyVar (Either VName TyVarSol) -> VName -> Maybe (TypeBase SComp u)
substTyVar m v =
  case M.lookup v m of
    Just (Left v') -> substTyVar m v'
    Just (Right (TyVarSol _ t')) -> Just $ second (const mempty) $ substTyVars (substTyVar m) t'
    Just (Right TyVarParam {}) -> Nothing
    Just (Right (TyVarUnsol {})) -> Nothing
    Nothing -> Nothing

maybeLookupTyVar :: TyVar -> SolveM (Maybe TyVarSol)
maybeLookupTyVar orig = do
  tyvars <- gets solverTyVars
  let f v = case M.lookup v tyvars of
        Nothing -> pure Nothing
        Just (Left v') -> f v'
        Just (Right info) -> pure $ Just info
  f orig

lookupTyVar :: TyVar -> SolveM (Level, Either TyVarInfo Type)
lookupTyVar orig =
  maybe bad unpack <$> maybeLookupTyVar orig
  where
    bad = error $ "Unknown tyvar: " <> prettyNameString orig
    unpack (TyVarParam {}) = error $ "Is a type param: " <> prettyNameString orig
    unpack (TyVarSol lvl t) = (lvl, Right t)
    unpack (TyVarUnsol lvl info) = (lvl, Left info)

-- | Variable must be flexible.
lookupTyVarInfo :: TyVar -> SolveM (Level, TyVarInfo)
lookupTyVarInfo v = do
  (lvl, r) <- lookupTyVar v
  case r of
    Left info -> pure (lvl, info)
    Right _ -> error $ "Tyvar is nonflexible: " <> prettyNameString v

setLink :: TyVar -> VName -> SolveM ()
setLink v info = modify $ \s -> s {solverTyVars = M.insert v (Left info) $ solverTyVars s}

setInfo :: TyVar -> TyVarSol -> SolveM ()
setInfo v info = modify $ \s -> s {solverTyVars = M.insert v (Right info) $ solverTyVars s}

-- | A solution maps a type variable to its substitution. This
-- substitution is complete, in the sense there are no right-hand
-- sides that contain a type variable.
type Solution = M.Map TyVar (Either [PrimType] (TypeBase () NoUniqueness))

-- | An unconstrained type variable comprises a name and (ironically)
-- a constraint on how it can be instantiated.
type UnconTyVar = (VName, Liftedness)

solution :: SolverState -> ([UnconTyVar], Solution)
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

    unconstrained (v, Right (TyVarUnsol _ (TyVarFree _ l))) = Just (v, l)
    unconstrained _ = Nothing

newtype SolveM a = SolveM {runSolveM :: StateT SolverState (Except TypeError) a}
  deriving (Functor, Applicative, Monad, MonadState SolverState, MonadError TypeError)

typeError :: Loc -> Notes -> Doc () -> SolveM ()
typeError loc notes msg =
  throwError $ TypeError loc notes msg

occursCheck :: Reason -> VName -> Type -> SolveM ()
occursCheck reason v tp = do
  vars <- gets solverTyVars
  let tp' = substTyVars (substTyVar vars) tp
  when (v `S.member` typeVars tp') . typeError (locOf reason) mempty $
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
      then zipWithM_ (solveEq reason) ts1 ts2
      else
        typeError (locOf reason) mempty $
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

scopeViolation :: Reason -> VName -> Type -> VName -> SolveM ()
scopeViolation reason v1 ty v2 =
  typeError (locOf reason) mempty $
    "Cannot unify type"
      </> indent 2 (pretty ty)
      </> "with"
      <+> dquotes (prettyName v1)
      <+> "(scope violation)."
      </> "This is because"
      <+> dquotes (prettyName v2)
      <+> "is rigidly bound in a deeper scope."

-- Precondition: 'v' is currently flexible.
subTyVar :: Reason -> VName -> Int -> Type -> SolveM ()
subTyVar reason v v_lvl t = do
  occursCheck reason v t
  v_info <- gets $ M.lookup v . solverTyVars

  -- Set a solution for v, then update info for t in case v has any
  -- odd constraints.
  setInfo v (TyVarSol v_lvl t)

  case (v_info, t) of
    (Just (Right (TyVarUnsol _ TyVarFree {})), _) ->
      pure ()
    ( Just (Right (TyVarUnsol _ (TyVarPrim _ v_pts))),
      _
      ) ->
        if t `elem` map (Scalar . Prim) v_pts
          then pure ()
          else
            typeError (locOf reason) mempty $
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
            typeError (locOf reason) mempty $
              "Cannot unify type with constructors"
                </> indent 2 (pretty (Sum cs1))
                </> "with type"
                </> indent 2 (pretty (Sum cs2))
    ( Just (Right (TyVarUnsol _ (TyVarSum _ cs1))),
      _
      ) ->
        typeError (locOf reason) mempty $
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
            typeError (locOf reason) mempty $
              "Cannot unify record type with fields"
                </> indent 2 (pretty (Record fs1))
                </> "with record type"
                </> indent 2 (pretty (Record fs2))
    ( Just (Right (TyVarUnsol _ (TyVarRecord _ fs1))),
      _
      ) ->
        typeError (locOf reason) mempty $
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

-- Precondition: 'v' and 't' are both currently flexible.
unionTyVars :: Reason -> VName -> VName -> SolveM ()
unionTyVars reason v t = do
  v_info <- gets $ either alreadyLinked id . fromMaybe unknown . M.lookup v . solverTyVars
  (t_lvl, t_info) <- lookupTyVarInfo t

  -- Insert the link from v to t, and then update the info of t based
  -- on the existing info of v and t.
  setLink v t

  case (v_info, t_info) of
    ( TyVarUnsol _ (TyVarFree _ v_l),
      TyVarFree t_loc t_l
      )
        | v_l /= t_l ->
            setInfo t $ TyVarUnsol t_lvl $ TyVarFree t_loc (min v_l t_l)
    -- When either is completely unconstrained.
    (TyVarUnsol _ TyVarFree {}, _) ->
      pure ()
    ( TyVarUnsol _ info,
      TyVarFree {}
      ) ->
        setInfo t (TyVarUnsol t_lvl info)
    --
    -- TyVarPrim cases
    ( TyVarUnsol _ info@TyVarPrim {},
      TyVarEql {}
      ) ->
        setInfo t (TyVarUnsol t_lvl info)
    ( TyVarUnsol _ (TyVarPrim _ v_pts),
      TyVarPrim t_loc t_pts
      ) ->
        let pts = L.intersect v_pts t_pts
         in if null pts
              then
                typeError (locOf reason) mempty $
                  "Cannot unify type that must be one of"
                    </> indent 2 (pretty v_pts)
                    </> "with type that must be one of"
                    </> indent 2 (pretty t_pts)
              else setInfo t (TyVarUnsol t_lvl (TyVarPrim t_loc pts))
    ( TyVarUnsol _ (TyVarPrim _ v_pts),
      TyVarRecord {}
      ) ->
        typeError (locOf reason) mempty $
          "Cannot unify type that must be one of"
            </> indent 2 (pretty v_pts)
            </> "with type that must be record."
    ( TyVarUnsol _ (TyVarPrim _ v_pts),
      TyVarSum {}
      ) ->
        typeError (locOf reason) mempty $
          "Cannot unify type that must be one of"
            </> indent 2 (pretty v_pts)
            </> "with type that must be sum."
    --
    -- TyVarSum cases
    ( TyVarUnsol _ (TyVarSum _ cs1),
      TyVarSum loc cs2
      ) -> do
        unifySharedConstructors reason cs1 cs2
        let cs3 = cs1 <> cs2
        setInfo t (TyVarUnsol t_lvl (TyVarSum loc cs3))
    ( TyVarUnsol _ TyVarSum {},
      TyVarPrim _ pts
      ) ->
        typeError (locOf reason) mempty $
          "A sum type cannot be one of"
            </> indent 2 (pretty pts)
    ( TyVarUnsol _ (TyVarSum _ cs1),
      TyVarRecord _ fs
      ) ->
        typeError (locOf reason) mempty $
          "Cannot unify type with constructors"
            </> indent 2 (pretty (Sum cs1))
            </> "with type"
            </> indent 2 (pretty (Scalar (Record fs)))
    ( TyVarUnsol _ (TyVarSum _ cs1),
      TyVarEql _
      ) ->
        mapM_ (mapM_ (mustSupportEql reason)) cs1
    --
    -- TyVarRecord cases
    ( TyVarUnsol _ (TyVarRecord _ fs1),
      TyVarRecord loc fs2
      ) -> do
        unifySharedFields reason fs1 fs2
        let fs3 = fs1 <> fs2
        setInfo t (TyVarUnsol t_lvl (TyVarRecord loc fs3))
    ( TyVarUnsol _ TyVarRecord {},
      TyVarPrim _ pts
      ) ->
        typeError (locOf reason) mempty $
          "A record type cannot be one of"
            </> indent 2 (pretty pts)
    ( TyVarUnsol _ (TyVarRecord _ fs1),
      TyVarSum _ cs
      ) ->
        typeError (locOf reason) mempty $
          "Cannot unify record type"
            </> indent 2 (pretty (Record fs1))
            </> "with type"
            </> indent 2 (pretty (Scalar (Sum cs)))
    ( TyVarUnsol _ (TyVarRecord _ fs1),
      TyVarEql _
      ) ->
        mapM_ (mustSupportEql reason) fs1
    --
    -- TyVarEql cases
    (TyVarUnsol _ (TyVarEql _), TyVarPrim {}) ->
      pure ()
    (TyVarUnsol _ (TyVarEql _), TyVarEql {}) ->
      pure ()
    (TyVarUnsol _ (TyVarEql _), TyVarRecord _ fs) ->
      mustSupportEql reason $ Scalar $ Record fs
    (TyVarUnsol _ (TyVarEql _), TyVarSum _ cs) ->
      mustSupportEql reason $ Scalar $ Sum cs
    --
    -- Internal error cases
    (TyVarSol {}, _) ->
      alreadySolved
    (TyVarParam {}, _) ->
      isParam
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
      typeError (locOf reason) mempty $
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

scopeCheck :: Reason -> TyVar -> Int -> Type -> SolveM ()
scopeCheck reason v v_lvl ty = do
  mapM_ check $ typeVars ty
  where
    check ty_v = do
      ty_v_info <- gets $ M.lookup ty_v . solverTyVars
      case ty_v_info of
        Just (Right (TyVarParam ty_v_lvl _ _))
          | ty_v_lvl > v_lvl -> scopeViolation reason v ty ty_v
        _ -> pure ()

-- If a type variable has a liftedness constraint, we propagate that
-- constraint to its solution. The actual checking for correct usage
-- is done later.
liftednessCheck :: Liftedness -> Type -> SolveM ()
liftednessCheck l (Scalar (TypeVar _ (QualName [] v) _)) = do
  v_info <- maybeLookupTyVar v
  case v_info of
    Nothing ->
      -- Is an opaque type.
      pure ()
    Just (TyVarSol _ v_ty) ->
      liftednessCheck l v_ty
    Just TyVarParam {} -> pure ()
    Just (TyVarUnsol lvl (TyVarFree loc v_l))
      | l /= v_l ->
          setInfo v $ TyVarUnsol lvl $ TyVarFree loc (min l v_l)
    Just TyVarUnsol {} -> pure ()
liftednessCheck _ (Scalar Prim {}) = pure ()
liftednessCheck Lifted _ = pure ()
liftednessCheck _ Array {} = pure ()
liftednessCheck _ (Scalar Arrow {}) = pure ()
liftednessCheck l (Scalar (Record fs)) =
  mapM_ (liftednessCheck l) fs
liftednessCheck l (Scalar (Sum cs)) =
  mapM_ (mapM_ $ liftednessCheck l) cs
liftednessCheck _ (Scalar TypeVar {}) = pure ()

solveTyVar :: (VName, (Level, TyVarInfo)) -> SolveM ()
solveTyVar (tv, (_, TyVarRecord loc fs1)) = do
  (_, tv_t) <- lookupTyVar tv
  case tv_t of
    Left _ ->
      typeError loc mempty $
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
      typeError loc mempty $
        "Type is ambiguous."
          </> "Must be a sum type with constructors"
          </> indent 2 (pretty (Scalar (Sum cs1)))
    Right _ -> pure ()
solveTyVar (tv, (_, TyVarEql loc)) = do
  (_, tv_t) <- lookupTyVar tv
  case tv_t of
    Left TyVarEql {} ->
      typeError loc mempty $
        "Type is ambiguous (must be equality type)"
          </> "Add a type annotation to disambiguate the type."
    Left _ -> pure ()
    Right ty
      | orderZero ty -> pure ()
      | otherwise ->
          typeError loc mempty $
            "Type"
              </> indent 2 (align (pretty ty))
              </> "does not support equality (may contain function)."
solveTyVar (tv, (lvl, TyVarFree loc l)) = do
  (_, tv_t) <- lookupTyVar tv
  case tv_t of
    Right ty -> do
      scopeCheck (Reason loc) tv lvl ty
      liftednessCheck l ty
    _ -> pure ()
solveTyVar (tv, (_, TyVarPrim loc pts)) = do
  (_, tv_t) <- lookupTyVar tv
  case tv_t of
    Right ty
      | ty `elem` map (Scalar . Prim) pts -> pure ()
      | otherwise ->
          typeError loc mempty $
            "Numeric constant inferred to be of type"
              </> indent 2 (align (pretty ty))
              </> "which is not possible."
    _ -> pure ()

solve ::
  Constraints ->
  TyParams ->
  TyVars ->
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
