-- | The constraint solver for unsized type equality constraints.
module Language.Futhark.TypeChecker.TySolve
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
import Debug.Trace (traceM)

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

newtype SolverState = SolverState
  { -- | Left means linked to this other type variable.
    solverTyVars :: M.Map TyVar (Either VName TyVarSol)
  }

initialState :: TyParams -> TyVars () -> SolverState
initialState typarams tyvars = SolverState $ M.map g typarams <> M.map f tyvars
  where
    f (_lvl, info) = Right $ TyVarUnsol info
    g (lvl, l, loc) = Right $ TyVarParam lvl l loc

substTyVar :: (Monoid u) => M.Map TyVar (Either VName TyVarSol) -> VName -> Maybe (TypeBase () u)
substTyVar m v =
  case M.lookup v m of
    Just (Left v') -> substTyVar m v'
    Just (Right (TyVarSol t')) -> Just $ second (const mempty) $ substTyVars (substTyVar m) t'
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

lookupTyVar :: TyVar -> SolveM (Either (TyVarInfo ()) Type)
lookupTyVar orig =
  maybe bad unpack <$> maybeLookupTyVar orig
  where
    bad = error $ "Unknown tyvar: " <> prettyNameString orig
    unpack (TyVarParam {}) = error $ "Is a type param: " <> prettyNameString orig
    unpack (TyVarSol t) = Right t
    unpack (TyVarUnsol info) = Left info

-- | Variable must be flexible.
lookupTyVarInfo :: TyVar -> SolveM (TyVarInfo ())
lookupTyVarInfo v = do
  r <- lookupTyVar v
  case r of
    Left info -> pure info
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

typeVar :: (Monoid u) => VName -> TypeBase dim u
typeVar v = Scalar $ TypeVar mempty (qualName v) []

solution :: SolverState -> ([UnconTyVar], Solution)
solution s =
  ( mapMaybe unconstrained $ M.toList $ solverTyVars s,
    M.mapMaybe mkSubst $ solverTyVars s
  )
  where
    mkSubst (Right (TyVarSol t)) =
      Just $ Right $ first (const ()) $ substTyVars (substTyVar (solverTyVars s)) t
    mkSubst (Left v') =
      Just . fromMaybe (Right $ Scalar $ TypeVar mempty (qualName v') []) $
        mkSubst =<< M.lookup v' (solverTyVars s)
    mkSubst (Right (TyVarUnsol (TyVarPrim _ pts))) = Just $ Left pts
    mkSubst _ = Nothing

    unconstrained (v, Right (TyVarUnsol (TyVarFree _ l))) = Just (v, l)
    unconstrained _ = Nothing

newtype SolveM a = SolveM {runSolveM :: StateT SolverState (Except TypeError) a}
  deriving (Functor, Applicative, Monad, MonadState SolverState, MonadError TypeError)

-- Try to substitute as much information as we have.
enrichType :: Type -> SolveM Type
enrichType t = do
  s <- get
  pure $ substTyVars (substTyVar (solverTyVars s)) t

typeError :: Loc -> Notes -> Doc () -> SolveM ()
typeError loc notes msg =
  throwError $ TypeError loc notes msg

occursCheck :: Reason Type -> VName -> Type -> SolveM ()
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
  Reason Type ->
  BreadCrumbs ->
  M.Map Name [Type] ->
  M.Map Name [Type] ->
  SolveM ()
unifySharedConstructors reason bcs cs1 cs2 =
  forM_ (M.toList $ M.intersectionWith (,) cs1 cs2) $ \(c, (ts1, ts2)) ->
    if length ts1 == length ts2
      then zipWithM_ (solveEq reason $ matchingConstructor c <> bcs) ts1 ts2
      else
        typeError (locOf reason) mempty $
          "Cannot unify type with constructor"
            </> indent 2 (pretty (Sum (M.singleton c ts1)))
            </> "with type of constructor"
            </> indent 2 (pretty (Sum (M.singleton c ts2)))
            </> "because they differ in arity."

unifySharedFields ::
  Reason Type ->
  BreadCrumbs ->
  M.Map Name Type ->
  M.Map Name Type ->
  SolveM ()
unifySharedFields reason bcs fs1 fs2 =
  forM_ (M.toList $ M.intersectionWith (,) fs1 fs2) $ \(f, (ts1, ts2)) ->
    solveEq reason (matchingField f <> bcs) ts1 ts2

scopeViolation :: Reason Type -> VName -> Type -> VName -> SolveM ()
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

cannotUnify ::
  Reason Type ->
  Notes ->
  BreadCrumbs ->
  Type ->
  Type ->
  SolveM ()
cannotUnify reason notes bcs t1 t2 = do
  t1' <- enrichType t1
  t2' <- enrichType t2
  case reason of
    Reason loc ->
      typeError loc notes . stack $
        [ "Cannot unify",
          indent 2 (pretty t1'),
          "with",
          indent 2 (pretty t2')
        ]
          <> [pretty bcs | not $ hasNoBreadCrumbs bcs]
    ReasonPatMatch loc pat value_t ->
      typeError loc notes . stack $
        [ "Pattern",
          indent 2 $ align $ pretty pat,
          "cannot match value of type",
          indent 2 $ align $ pretty value_t
        ]
          <> [pretty bcs | not $ hasNoBreadCrumbs bcs]
    ReasonAscription loc expected actual ->
      typeError loc notes . stack $
        [ "Expression does not have expected type from type ascription.",
          "Expected:" <+> align (pretty expected),
          "Actual:  " <+> align (pretty actual)
        ]
          <> [pretty bcs | not $ hasNoBreadCrumbs bcs]
    ReasonRetType loc expected actual -> do
      expected' <- enrichType expected
      actual' <- enrichType actual
      typeError loc notes . stack $
        [ "Function body does not have expected type.",
          "Expected:" <+> align (pretty expected'),
          "Actual:  " <+> align (pretty actual')
        ]
          <> [pretty bcs | not $ hasNoBreadCrumbs bcs]
    ReasonApply loc f e expected actual -> do
      expected' <- enrichType expected
      actual' <- enrichType actual
      typeError loc notes . stack $
        [ header,
          "Expected:" <+> align (pretty expected'),
          "Actual:  " <+> align (pretty actual')
        ]
      where
        header =
          case f of
            (Nothing, _) ->
              "Cannot apply function to"
                <+> dquotes (shorten $ group $ pretty e)
                <> " (invalid type)."
            (Just fname, _) ->
              "Cannot apply"
                <+> dquotes (pretty fname)
                <+> "to"
                <+> dquotes (align $ shorten $ group $ pretty e)
                <> " (invalid type)."
    ReasonApplySplit loc (fname, 0) _ ftype ->
      typeError loc notes $
        stack
          [ "Cannot apply"
              <+> fname'
              <+> "as function, as it has non-function type:"
              </> indent 2 (align $ pretty ftype)
          ]
      where
        fname' = maybe "expression" (dquotes . pretty) fname
    ReasonApplySplit loc (fname, i) e _ ->
      typeError loc notes $
        stack
          [ "Cannot apply"
              <+> fname'
              <+> "to"
              <+> dquotes (align $ shorten $ group $ pretty e)
              <> ".",
            "Function accepts only" <+> pretty i <+> "arguments."
          ]
      where
        fname' = maybe "expression" (dquotes . pretty) fname
    ReasonBranches loc former latter -> do
      former' <- enrichType former
      latter' <- enrichType latter
      typeError loc notes . stack $
        [ "Branches differ in type.",
          "Former:" <+> pretty former',
          "Latter:" <+> pretty latter'
        ]

-- Precondition: 'v' is currently flexible.
subTyVar :: Reason Type -> BreadCrumbs -> VName -> Type -> SolveM ()
subTyVar reason bcs v t = do
  occursCheck reason v t
  v_info <- gets $ M.lookup v . solverTyVars

  -- Set a solution for v, then update info for t in case v has any
  -- odd constraints.
  setInfo v (TyVarSol t)

  case (v_info, t) of
    ( Just (Right (TyVarUnsol TyVarFree {})), _ ) -> do
      traceM $ "jada!! " ++ show v
      pure ()
    ( Just (Right (TyVarUnsol (TyVarPrim _ v_pts))), _ ) ->
        if t `elem` map (Scalar . Prim) v_pts
          then pure ()
          else cannotUnify reason notes bcs (typeVar v) t
        where
          notes =
            aNote $
              "Cannot instantiate type that must be one of"
                </> indent 2 (pretty v_pts)
                </> "with"
                </> indent 2 (pretty t)
    ( Just (Right (TyVarUnsol (TyVarSum _ cs1))), Scalar (Sum cs2) ) ->
        if all (`elem` M.keys cs2) (M.keys cs1)
          then unifySharedConstructors reason bcs cs1 cs2
          else cannotUnify reason notes bcs (typeVar v) t
        where
          notes =
            aNote $
              "Cannot match type with constructors"
                </> indent 2 (stack (map (("#" <>) . pretty) (M.keys cs1)))
                </> "with type with constructors"
                </> indent 2 (stack (map (("#" <>) . pretty) (M.keys cs2)))
                </> unsharedConstructorsMsg cs1 cs2
    ( Just (Right (TyVarUnsol (TyVarSum _ cs1))), _ ) ->
        typeError (locOf reason) mempty $
          "Cannot unify type with constructors"
            </> indent 2 (pretty (Sum cs1))
            </> "with type"
            </> indent 2 (pretty t)
    ( Just (Right (TyVarUnsol (TyVarRecord _ fs1))), Scalar (Record fs2) ) ->
        if all (`elem` M.keys fs2) (M.keys fs1)
          then unifySharedFields reason bcs fs1 fs2
          else
            typeError (locOf reason) mempty $
              "Cannot unify record type with fields"
                </> indent 2 (pretty (Record fs1))
                </> "with record type"
                </> indent 2 (pretty (Record fs2))
    ( Just (Right (TyVarUnsol (TyVarRecord _ fs1))), _ ) ->
        typeError (locOf reason) mempty $
          "Cannot unify record type with fields"
            </> indent 2 (pretty (Record fs1))
            </> "with type"
            </> indent 2 (pretty t)
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
--
-- The purpose of this function is to combine the partial knowledge we
-- may have about these two type variables.
unionTyVars :: Reason Type -> BreadCrumbs -> VName -> VName -> SolveM ()
unionTyVars reason bcs v t = do
  v_info <- gets $ either alreadyLinked id . fromMaybe unknown . M.lookup v . solverTyVars
  t_info <- lookupTyVarInfo t

  -- Insert the link from v to t, and then update the info of t based
  -- on the existing info of v and t.
  setLink v t

  case (v_info, t_info) of
    ( TyVarUnsol (TyVarFree _ v_l),
      TyVarFree t_loc t_l
      )
        | v_l /= t_l ->
            setInfo t $ TyVarUnsol $ TyVarFree t_loc (min v_l t_l)
    -- When either is completely unconstrained.
    (TyVarUnsol TyVarFree {}, _) ->
      pure ()
    ( TyVarUnsol info,
      TyVarFree {}
      ) ->
        setInfo t (TyVarUnsol info)
    --
    -- TyVarPrim cases
    ( TyVarUnsol (TyVarPrim _ v_pts),
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
              else setInfo t (TyVarUnsol (TyVarPrim t_loc pts))
    ( TyVarUnsol (TyVarPrim _ v_pts),
      TyVarRecord {}
      ) ->
        typeError (locOf reason) mempty $
          "Cannot unify type that must be one of"
            </> indent 2 (pretty v_pts)
            </> "with type that must be a record."
    ( TyVarUnsol (TyVarPrim _ v_pts),
      TyVarSum {}
      ) ->
        typeError (locOf reason) mempty $
          "Cannot unify type that must be one of"
            </> indent 2 (pretty v_pts)
            </> "with type that must be sum."
    --
    -- TyVarSum cases
    ( TyVarUnsol (TyVarSum _ cs1),
      TyVarSum loc cs2
      ) -> do
        unifySharedConstructors reason bcs cs1 cs2
        let cs3 = cs1 <> cs2
        setInfo t (TyVarUnsol (TyVarSum loc cs3))
    ( TyVarUnsol TyVarSum {},
      TyVarPrim _ pts
      ) ->
        typeError (locOf reason) mempty $
          "A sum type cannot be one of"
            </> indent 2 (pretty pts)
    ( TyVarUnsol (TyVarSum _ cs1),
      TyVarRecord _ fs
      ) ->
        typeError (locOf reason) mempty $
          "Cannot unify type with constructors"
            </> indent 2 (pretty (Sum cs1))
            </> "with type"
            </> indent 2 (pretty (Scalar (Record fs)))
    --
    -- TyVarRecord cases
    ( TyVarUnsol (TyVarRecord _ fs1),
      TyVarRecord loc fs2
      ) -> do
        unifySharedFields reason bcs fs1 fs2
        let fs3 = fs1 <> fs2
        setInfo t (TyVarUnsol (TyVarRecord loc fs3))
    ( TyVarUnsol TyVarRecord {},
      TyVarPrim _ pts
      ) ->
        typeError (locOf reason) mempty $
          "A record type cannot be one of"
            </> indent 2 (pretty pts)
    ( TyVarUnsol (TyVarRecord _ fs1),
      TyVarSum _ cs
      ) ->
        typeError (locOf reason) mempty $
          "Cannot unify record type"
            </> indent 2 (pretty (Record fs1))
            </> "with type"
            </> indent 2 (pretty (Scalar (Sum cs)))
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

unsharedConstructorsMsg :: M.Map Name t -> M.Map Name t -> Doc a
unsharedConstructorsMsg cs1 cs2 =
  "Unshared constructors:" <+> commasep (map (("#" <>) . pretty) missing) <> "."
  where
    missing =
      filter (`notElem` M.keys cs1) (M.keys cs2)
        ++ filter (`notElem` M.keys cs2) (M.keys cs1)

-- Unify at the root, emitting new equalities that must hold.
unify :: Type -> Type -> Either (Doc a) [(BreadCrumbs, (Type, Type))]
unify (Scalar (Prim pt1)) (Scalar (Prim pt2))
  | pt1 == pt2 = Right []
unify
  (Scalar (TypeVar _ (QualName _ v1) targs1))
  (Scalar (TypeVar _ (QualName _ v2) targs2))
    | v1 == v2 =
        Right $ mapMaybe f $ zip targs1 targs2
    where
      f (TypeArgType t1, TypeArgType t2) = Just (mempty, (t1, t2))
      f _ = Nothing
unify (Scalar (Arrow _ _ _ t1a (RetType _ t1r))) (Scalar (Arrow _ _ _ t2a (RetType _ t2r))) =
  Right [(mempty, (t1a, t2a)), (mempty, (t1r', t2r'))]
  where
    t1r' = t1r `setUniqueness` NoUniqueness
    t2r' = t2r `setUniqueness` NoUniqueness
unify (Scalar (Record fs1)) (Scalar (Record fs2))
  | M.keys fs1 == M.keys fs2 =
      Right $
        map (first matchingField) $
          M.toList $
            M.intersectionWith (,) fs1 fs2
  | Just n1 <- length <$> areTupleFields fs1,
    Just n2 <- length <$> areTupleFields fs2,
    n1 /= n2 =
      Left $
        "Tuples have"
          <+> pretty n1
          <+> "and"
          <+> pretty n2
          <+> "elements respectively."
  | otherwise =
      let missing =
            filter (`notElem` M.keys fs1) (M.keys fs2)
              <> filter (`notElem` M.keys fs2) (M.keys fs1)
       in Left $
            "unshared fields:" <+> commasep (map pretty missing) <> "."
unify (Scalar (Sum cs1)) (Scalar (Sum cs2))
  | M.keys cs1 == M.keys cs2 =
      fmap concat . forM cs' $ \(c, (ts1, ts2)) -> do
        if length ts1 == length ts2
          then Right $ zipWith (curry (matchingConstructor c,)) ts1 ts2
          else Left mempty
  | otherwise =
      Left $ unsharedConstructorsMsg cs1 cs2
  where
    cs' = M.toList $ M.intersectionWith (,) cs1 cs2
unify t1 t2
  | Just t1' <- peelArray 1 t1,
    Just t2' <- peelArray 1 t2 =
      Right [(mempty, (t1', t2'))]
unify _ _ = Left mempty

solveEq :: Reason Type -> BreadCrumbs -> Type -> Type -> SolveM ()
solveEq reason obcs orig_t1 orig_t2 = do
  solveCt' (obcs, (orig_t1, orig_t2))
  where
    solveCt' (bcs, (t1, t2)) = do
      tyvars <- gets solverTyVars
      let flexible v = case M.lookup v tyvars of
            Just (Left v') -> flexible v'
            Just (Right (TyVarUnsol _)) -> True
            Just (Right TyVarSol {}) -> False
            Just (Right TyVarParam {}) -> False
            Nothing -> False
          normalize t@(Scalar (TypeVar u (QualName [] v) [])) =
            case M.lookup v tyvars of
              Just (Left v') -> normalize $ Scalar (TypeVar u (QualName [] v') [])
              Just (Right (TyVarSol t')) -> normalize t'
              _ -> t
          normalize t = t
      case (normalize t1, normalize t2) of
        ( t1'@(Scalar (TypeVar _ (QualName [] v1) [])),
          t2'@(Scalar (TypeVar _ (QualName [] v2) []))
          )
            | v1 == v2 -> pure ()
            | otherwise ->
                case (flexible v1, flexible v2) of
                  (False, False) -> cannotUnify reason mempty bcs t1 t2
                  (True, False) -> subTyVar reason bcs v1 t2'
                  (False, True) -> subTyVar reason bcs v2 t1'
                  (True, True) -> unionTyVars reason bcs v1 v2
        (Scalar (TypeVar _ (QualName [] v1) []), t2')
          | flexible v1 -> subTyVar reason bcs v1 t2'
        (t1', Scalar (TypeVar _ (QualName [] v2) []))
          | flexible v2 -> subTyVar reason bcs v2 t1'
        (t1', t2') -> case unify t1' t2' of
          Left details -> cannotUnify reason (aNote details) bcs t1' t2'
          Right eqs -> mapM_ solveCt' eqs

solveCt :: CtTy () -> SolveM ()
solveCt ct =
  case ct of
    CtEq reason t1 t2 -> solveEq reason mempty t1 t2

scopeCheck :: Reason Type -> TyVar -> Int -> Type -> SolveM ()
scopeCheck reason v v_lvl ty = mapM_ check $ typeVars ty
  where
    check ty_v = do
      ty_v_info <- gets $ M.lookup ty_v . solverTyVars
      case ty_v_info of
        Just (Right (TyVarParam ty_v_lvl _ _))
          -- Type parameter has a higher level than the type variable.
          | ty_v_lvl > v_lvl -> scopeViolation reason v ty ty_v
        Just (Right (TyVarSol ty')) ->
          mapM_ check $ typeVars ty'
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
    Just (TyVarSol v_ty) ->
      liftednessCheck l v_ty
    Just TyVarParam {} -> pure ()
    Just (TyVarUnsol (TyVarFree loc v_l))
      | l /= v_l ->
          setInfo v $ TyVarUnsol $ TyVarFree loc (min l v_l)
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

solveTyVar :: (VName, (Level, TyVarInfo ())) -> SolveM ()
solveTyVar (tv, (_, TyVarRecord loc fs1)) = do
  tv_t <- lookupTyVar tv
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
  tv_t <- lookupTyVar tv
  case tv_t of
    Left _ ->
      typeError loc mempty $
        "Type is ambiguous."
          </> "Must be a sum type with constructors"
          </> indent 2 (pretty (Scalar (Sum cs1)))
    Right _ -> pure ()
solveTyVar (tv, (lvl, TyVarFree loc l)) = do
  tv_t <- lookupTyVar tv
  case tv_t of
    Right ty -> do
      traceM $ "nejda!! " ++ show tv
      scopeCheck (Reason loc) tv lvl ty
      liftednessCheck l ty
    _ -> pure ()
solveTyVar (tv, (_, TyVarPrim loc pts)) = do
  tv_t <- lookupTyVar tv
  case tv_t of
    Right (Scalar (Prim ty))
      | [ty] == pts ->
          setInfo tv $ TyVarSol $ Scalar $ Prim ty
    Right ty
      | ty `elem` map (Scalar . Prim) pts -> pure ()
      | otherwise ->
          typeError loc mempty $
            "Numeric constant inferred to be of type"
              </> indent 2 (align (pretty ty))
              </> "which is not possible."
    _ -> pure ()

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
