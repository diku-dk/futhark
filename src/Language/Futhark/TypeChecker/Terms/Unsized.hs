{-# LANGUAGE Strict #-}

-- | Unsized type checking.
--
-- This checker generates type constraints (type 'CtTy') which are then solved
-- to find a solution. The result is a decorated AST where most of the type
-- annotations are just references to type variables. Further, all the
-- size-specific annotations (e.g. existential sizes) just contain dummy values,
-- such as empty lists.
--
-- If Futhark had no fancy type system features, then this pass would
-- essentially be all you needed.
module Language.Futhark.TypeChecker.Terms.Unsized
  ( checkValDef,
    checkSingleExp,
    checkSizeExp,
    Solution,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifoldable (bifoldMap)
import Data.Bifunctor
import Data.Bitraversable
import Data.Char (isAscii)
import Data.Either (partitionEithers)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Loc (Loc (NoLoc))
import Data.Map qualified as M
import Data.Maybe
import Data.Ord (comparing)
import Data.Set qualified as S
import Data.Text qualified as T
import Futhark.FreshNames qualified as FreshNames
import Futhark.MonadFreshNames hiding (newName)
import Futhark.Util (nubOrd)
import Futhark.Util.Pretty
import Language.Futhark
import Language.Futhark.TypeChecker.Constraints
import Language.Futhark.TypeChecker.Monad hiding (BoundV, lookupMod)
import Language.Futhark.TypeChecker.Monad qualified as TypeM
import Language.Futhark.TypeChecker.Terms.Scope hiding (envToTermScope, initialTermScope, lookupQualNameEnv)
import Language.Futhark.TypeChecker.Terms.Scope qualified as Scope
import Language.Futhark.TypeChecker.TySolve hiding (Type)
import Language.Futhark.TypeChecker.Types
import Language.Futhark.TypeChecker.Unify (mkUsage)
import Prelude hiding (mod)

type Type = CtType ()

-- | The unsized checker ignores sizes entirely, so we erase them. (The
-- constraint solver never inspects sizes; see '()' for the shape
-- representation this could use if size-aware rank inference were ever
-- wired up.)
toType :: TypeBase Size u -> TypeBase () u
toType = first (const ())

-- | Type checking happens with access to this environment.  The
-- 'TermScope' will be extended during type-checking as bindings come into
-- scope.
data TermEnv = TermEnv
  { termScope :: TermScope (),
    termLevel :: Level,
    termOuterEnv :: Env,
    termImportName :: ImportName,
    -- | The liftedness of abstract types.
    termTySet :: TySet
  }

-- | An instantiation (at the given location, of a type parameter of
-- the given polymorphic function, with the given liftedness, as the
-- given type variable), recorded such that we can check, after
-- constraint solving, that the liftedness of the type parameter is
-- respected ('checkTyInstLiftedness').
data TyInst = TyInst Loc (QualName VName) Liftedness TyVar

-- | The state is a set of constraints and a counter for generating
-- type names.  This is distinct from the usual counter we use for
-- generating unique names, as these will be user-visible.
data TermState = TermState
  { termConstraints :: [CtTy ()],
    termTyVars :: TyVars (),
    termTyParams :: TyParams,
    termTyInsts :: [TyInst],
    termCounter :: !Int,
    termWarnings :: Warnings,
    termNameSource :: VNameSource,
    -- | Mapping from artificial type variables to the actual types they represent.
    termArtificial :: M.Map TyVar Type
  }

newtype TermM a
  = TermM
      ( ReaderT
          TermEnv
          (StateT TermState (Except (Warnings, TypeError)))
          a
      )
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader TermEnv,
      MonadState TermState
    )

-- | The scope, with sizes erased. See
-- "Language.Futhark.TypeChecker.Terms.Scope".
envToTermScope :: Env -> TermScope ()
envToTermScope = Scope.envToTermScope toType

initialTermScope :: TermScope ()
initialTermScope = Scope.initialTermScope toType

runTermM :: TermM a -> TypeM a
runTermM (TermM m) = do
  initial_scope <- (initialTermScope <>) . Scope.envToTermScopeNoVals <$> askEnv
  name <- askImportName
  outer_env <- askEnv
  src <- gets stateNameSource
  abs_types <- getTySet
  let initial_env =
        TermEnv
          { termScope = initial_scope,
            termLevel = 0,
            termImportName = name,
            termOuterEnv = outer_env,
            termTySet = abs_types
          }
      initial_state =
        TermState
          { termConstraints = mempty,
            termTyVars = mempty,
            termTyParams = mempty,
            termTyInsts = mempty,
            termWarnings = mempty,
            termNameSource = src,
            termCounter = 0,
            termArtificial = mempty
          }
  case runExcept (runStateT (runReaderT m initial_env) initial_state) of
    Left (ws, e) -> do
      warnings ws
      throwError e
    Right (a, TermState {termNameSource, termWarnings}) -> do
      warnings termWarnings
      modify $ \s -> s {stateNameSource = termNameSource}
      pure a

incLevel :: TermM a -> TermM a
incLevel = local $ \env -> env {termLevel = termLevel env + 1}

curLevel :: TermM Int
curLevel = asks termLevel

incCounter :: TermM Int
incCounter = do
  s <- get
  put s {termCounter = termCounter s + 1}
  pure $ termCounter s

tyVarType :: u -> TyVar -> TypeBase dim u
tyVarType u v = Scalar $ TypeVar u (qualName v) []

newTyVarWith :: Name -> TyVarInfo () -> TermM TyVar
newTyVarWith desc info = do
  i <- incCounter
  v <- newID $ mkTypeVarName desc i
  lvl <- curLevel
  modify $ \s -> s {termTyVars = M.insert v (lvl, info) $ termTyVars s}
  pure v

newTyVar :: (Located loc) => loc -> Liftedness -> Name -> TermM TyVar
newTyVar loc l desc = newTyVarWith desc $ TyVarFree (locOf loc) l

newType :: (Located loc) => loc -> Liftedness -> Name -> u -> TermM (TypeBase dim u)
newType loc l desc u = tyVarType u <$> newTyVar loc l desc

-- | New type that must be allowed as an array element.
newElemType :: (Located loc) => loc -> Name -> u -> TermM (TypeBase dim u)
newElemType loc desc u = tyVarType u <$> newTyVar loc Unlifted desc

newTypeWithField :: SrcLoc -> Name -> Name -> Type -> TermM Type
newTypeWithField loc desc k t =
  tyVarType NoUniqueness
    <$> newTyVarWith desc (TyVarRecord (locOf loc) $ M.singleton k t)

newTypeWithConstr :: SrcLoc -> Name -> u -> Name -> [TypeBase () u] -> TermM (TypeBase d u)
newTypeWithConstr loc desc u k ts =
  tyVarType u <$> newTyVarWith desc (TyVarSum (locOf loc) $ M.singleton k ts')
  where
    ts' = map (`setUniqueness` NoUniqueness) ts

newTypeOverloaded :: SrcLoc -> Name -> [PrimType] -> TermM (TypeBase d NoUniqueness)
newTypeOverloaded loc name pts =
  tyVarType NoUniqueness <$> newTyVarWith name (TyVarPrim (locOf loc) pts)

newArtificial :: u -> TypeBase () u -> TermM (TypeBase Size u)
newArtificial u t = do
  v <- newID "artificial"
  let t' = tyVarType u v
  modify $ \s -> s {termArtificial = M.insert v (second (const NoUniqueness) t) $ termArtificial s}
  pure t'

-- The AST requires annotations to be StructTypes, but the type
-- checker works with Types. This creates artificial type "variables"
-- that allow us to connect the AST annotations with the actual
-- inferred types. The artificial variables should never occur in
-- constraints - they can be substituted away with asType.
--
-- Equal components (with fully known shapes) of the same annotation
-- are given the same artificial variable, so that the sized type
-- checker knows that they have the same sizes.
asStructType :: TypeBase () u -> TermM (TypeBase Size u)
asStructType t = evalStateT (onType t) mempty
  where
    onType ::
      TypeBase () u' ->
      StateT (M.Map (TypeBase () NoUniqueness) TyVar) TermM (TypeBase Size u')
    onType (Scalar (Prim pt)) = pure $ Scalar $ Prim pt
    onType (Scalar (TypeVar u v [])) = pure $ Scalar $ TypeVar u v []
    onType (Scalar (Arrow u pname d t1 (RetType ext t2))) = do
      t1' <- onType t1
      t2' <- onType t2
      pure $ Scalar $ Arrow u pname d t1' $ RetType ext t2'
    onType (Scalar (Record fs)) =
      Scalar . Record <$> traverse onType fs
    onType (Scalar (Sum cs)) =
      Scalar . Sum <$> traverse (mapM onType) cs
    onType t'@(Scalar (TypeVar u _ _)) = artificial u t'
    onType t'@(Array u _ _) = artificial u t'

    artificial u t'
      | anonymousShape t' = lift $ newArtificial u t'
      | otherwise = do
          let key = second (const NoUniqueness) t'
          seen <- get
          case M.lookup key seen of
            Just v -> pure $ tyVarType u v
            Nothing -> do
              v <- lift $ newID "artificial"
              lift $ modify $ \s ->
                s {termArtificial = M.insert v key $ termArtificial s}
              modify $ M.insert key v
              pure $ tyVarType u v

    anonymousShape = elem () . bifoldMap (: []) (const mempty)

asType :: (Monoid u) => TypeBase Size u -> TermM (TypeBase () u)
asType t = do
  artificial <- gets termArtificial
  pure $ substTyVars (`M.lookup` artificial) (toType t)

expType :: Exp -> TermM Type
expType = asType . typeOf -- NOTE: Only place you should use typeOf.

addCt :: CtTy () -> TermM ()
addCt ct = modify $ \s -> s {termConstraints = ct : termConstraints s}

ctEq :: Reason (CtType ()) -> TypeBase () u1 -> TypeBase () u2 -> TermM ()
ctEq reason t1 t2 =
  -- As a minor optimisation, do not add constraint if the types are
  -- equal.
  unless (t1' == t2') $ addCt $ CtEq reason t1' t2'
  where
    t1' = t1 `setUniqueness` NoUniqueness
    t2' = t2 `setUniqueness` NoUniqueness

localScope :: (TermScope () -> TermScope ()) -> TermM a -> TermM a
localScope f = local $ \tenv -> tenv {termScope = f $ termScope tenv}

withEnv :: TermEnv -> Env -> TermEnv
withEnv tenv env = tenv {termScope = termScope tenv <> envToTermScope env}

lookupQualNameEnv :: QualName VName -> TermM (TermScope ())
lookupQualNameEnv qn = asks $ \tenv -> Scope.lookupQualNameEnv toType (termScope tenv) qn

instance MonadError TypeError TermM where
  throwError e = TermM $ do
    ws <- gets termWarnings
    throwError (ws, e)

  catchError (TermM m) f =
    TermM $ m `catchError` f'
    where
      f' (_, e) = let TermM m' = f e in m'

instance MonadTypeChecker TermM where
  warnings ws = modify $ \s -> s {termWarnings = termWarnings s <> ws}

  warn loc problem = warnings $ singleWarning (locOf loc) problem

  newName v = do
    s <- get
    let (v', src') = FreshNames.newName (termNameSource s) v
    put $ s {termNameSource = src'}
    pure v'

  newID s = newName $ VName s 0

  newTypeName name = do
    i <- incCounter
    newID $ mkTypeVarName name i

  bindVal v (TypeM.BoundV tps t) m = do
    t' <- asType t
    let f scope = scope {scopeVtable = M.insert v (BoundV tps t') $ scopeVtable scope}
    localScope f m

  lookupType qn = do
    outer_env <- asks termOuterEnv
    scope <- lookupQualNameEnv qn
    case M.lookup (qualLeaf qn) $ scopeTypeTable scope of
      Nothing -> error $ "lookupType: " <> show qn
      Just (TypeAbbr l ps (RetType dims def)) ->
        pure
          ( ps,
            RetType dims $ qualifyTypeVars outer_env (map typeParamName ps) (qualQuals qn) def,
            l
          )

  typeError loc notes s =
    throwError $ TypeError (locOf loc) notes s

--- All the general machinery goes above.

arrayOfRank :: Int -> Type -> Type
arrayOfRank n = arrayOf $ Shape $ replicate n ()

require :: T.Text -> [PrimType] -> Exp -> TermM Exp
require _why [pt] e = do
  e_t <- expType e
  ctEq (Reason (locOf e)) (Scalar $ Prim pt) e_t
  pure e
require _why pts e = do
  t :: Type <- newTypeOverloaded (srclocOf e) "t" pts
  e_t <- expType e
  ctEq (Reason (locOf e)) t e_t
  pure e

-- | Instantiate a type scheme with fresh type variables for its type
-- parameters. Returns the names of the fresh type variables, the
-- instance list, and the instantiated type.
instTypeScheme ::
  QualName VName ->
  SrcLoc ->
  [TypeParam] ->
  Type ->
  TermM ([VName], Type)
instTypeScheme qn loc tparams t = do
  (names, substs) <- fmap (unzip . catMaybes) $
    forM tparams $ \tparam ->
      case tparam of
        TypeParamType l v _ -> do
          v' <- newTyVar loc l $ nameFromText $ T.takeWhile isAscii $ nameToText $ baseName v
          modify $
            \s -> s {termTyInsts = TyInst (locOf loc) qn l v' : termTyInsts s}
          pure $ Just (v, (typeParamName tparam, tyVarType NoUniqueness v'))
        TypeParamDim {} ->
          pure Nothing
  let t' = substTyVars (`lookup` substs) t
  pure (names, t')

lookupMod :: QualName VName -> TermM Mod
lookupMod qn@(QualName _ name) = do
  scope <- lookupQualNameEnv qn
  case M.lookup name $ scopeModTable scope of
    Nothing -> error $ "lookupMod: " <> show qn
    Just m -> pure m

lookupVar :: SrcLoc -> QualName VName -> TermM Type
lookupVar loc qn@(QualName qs name) = do
  scope <- lookupQualNameEnv qn
  outer_env <- asks termOuterEnv
  -- Top-level value bindings are not in the term scope (see
  -- 'envToTermScopeNoVals'); look them up on demand in the outer
  -- environment.
  case M.lookup name (scopeVtable scope) `mplus` Scope.lookupOuterVal toType outer_env name of
    Nothing ->
      error $ "lookupVar: " <> show qn
    Just (BoundV tparams t) -> do
      if null tparams && null qs
        then pure t
        else do
          (tnames, t') <- instTypeScheme qn loc tparams t
          -- Qualify abstract types, so that e.g. mismatch errors
          -- mention them by how they were accessed. The sizes need
          -- no qualification, as they are not even present here.
          pure $ qualifyTypeVarsWith (\_ _ d -> d) outer_env tnames qs t'
    Just EqualityF -> do
      argtype <- tyVarType Observe <$> newTyVarWith "t" (TyVarFree (locOf loc) Unlifted)
      pure $ foldFunType [argtype, argtype] $ RetType [] $ Scalar $ Prim Bool
    Just (OverloadedF ts pts rt) -> do
      argtype <- newTypeOverloaded loc "t" ts
      let (pts', rt') = instOverloaded argtype pts rt
      pure $ foldFunType (map (second $ const Observe) pts') $ RetType [] $ second (const Nonunique) rt'
    -- The unsized checker binds recursive functions directly (see
    -- 'checkRecursive'), so it never produces a 'RecursiveV'.
    Just RecursiveV ->
      error $ "lookupVar: unexpected RecursiveV for " <> show qn
  where
    instOverloaded argtype pts rt =
      ( map (maybe argtype (Scalar . Prim)) pts,
        maybe argtype (Scalar . Prim) rt
      )

bind ::
  [Ident StructType] ->
  TermM a ->
  TermM a
bind idents m = do
  let names = map identName idents
  ts <- mapM (asType . unInfo . identType) idents
  localScope (`bindVars` zip names ts) m
  where
    bindVars = foldl bindVar

    bindVar scope (name, t) =
      scope
        { scopeVtable = M.insert name (BoundV [] t) $ scopeVtable scope
        }

-- All this complexity is just so we can handle un-suffixed numeric
-- literals in patterns.
patLitMkType :: PatLit -> SrcLoc -> TermM ParamType
patLitMkType (PatLitInt _) loc =
  toParam Observe <$> newTypeOverloaded loc "t" anyNumberType
patLitMkType (PatLitFloat _) loc =
  toParam Observe <$> newTypeOverloaded loc "t" anyFloatType
patLitMkType (PatLitPrim v) _ =
  pure $ Scalar $ Prim $ primValueType v

checkSizeExp' :: ExpBase NoInfo VName -> TermM Exp
checkSizeExp' e = do
  e' <- checkExp e
  e_t <- expType e'
  ctEq (Reason (locOf e)) e_t (Scalar (Prim (Signed Int64)))
  pure e'

checkPat' ::
  PatBase NoInfo VName ParamType ->
  Inferred ParamType ->
  TermM (Pat ParamType)
checkPat' (PatParens p loc) t =
  PatParens <$> checkPat' p t <*> pure loc
checkPat' (PatAttr attr p loc) t =
  PatAttr <$> checkAttr attr <*> checkPat' p t <*> pure loc
checkPat' (Id name NoInfo loc) (Ascribed t) =
  pure $ Id name (Info t) loc
checkPat' (Id name NoInfo loc) NoneInferred = do
  t <- newType loc Lifted "t" Observe
  pure $ Id name (Info t) loc
checkPat' (Wildcard _ loc) (Ascribed t) = do
  pure $ Wildcard (Info t) loc
checkPat' (Wildcard NoInfo loc) NoneInferred = do
  t <- newType loc Lifted "t" Observe
  pure $ Wildcard (Info t) loc
checkPat' p@(TuplePat ps loc) (Ascribed t)
  | Just ts <- isTupleRecord t,
    length ts == length ps =
      TuplePat
        <$> zipWithM checkPat' ps (map Ascribed ts)
        <*> pure loc
  | otherwise =
      typeError loc mempty $
        "Pattern"
          </> indent 2 (pretty p)
          </> "cannot match ascribed type"
          </> indent 2 (pretty t)
checkPat' (TuplePat ps loc) NoneInferred =
  TuplePat <$> mapM (`checkPat'` NoneInferred) ps <*> pure loc
checkPat' p@(RecordPat p_fs loc) _
  | Just (L floc f, _) <- L.find (("_" `T.isPrefixOf`) . nameToText . unLoc . fst) p_fs =
      typeError floc mempty $
        "Underscore-prefixed fields are not allowed."
          </> "Did you mean"
          <> dquotes (pretty (T.drop 1 (nameToText f)) <> "=_")
          <> "?"
  | length (nubOrd (map fst p_fs)) /= length (map fst p_fs) =
      typeError loc mempty $
        "Duplicate fields in record pattern" <+> pretty p <> "."
checkPat' p@(RecordPat p_fs loc) (Ascribed t)
  | Scalar (Record t_fs) <- t,
    p_fs' <- L.sortBy (comparing fst) p_fs,
    t_fs' <- L.sortBy (comparing fst) (M.toList t_fs),
    map fst t_fs' == map (unLoc . fst) p_fs' =
      RecordPat <$> zipWithM check p_fs' t_fs' <*> pure loc
  | otherwise = do
      typeError loc mempty $
        "Pattern"
          </> indent 2 (pretty p)
          </> "cannot match ascribed type"
          </> indent 2 (pretty t)
  where
    check (L f_loc f, p_f) (_, t_f) =
      (L f_loc f,) <$> checkPat' p_f (Ascribed t_f)
checkPat' (RecordPat fs loc) NoneInferred =
  RecordPat . M.toList
    <$> traverse (`checkPat'` NoneInferred) (M.fromList fs)
    <*> pure loc
checkPat' (PatAscription p t loc) maybe_outer_t = do
  (t', _, RetType _ st, _) <- checkTypeExp checkSizeExp' t

  let st' = resToParam st

  case maybe_outer_t of
    Ascribed outer_t -> do
      unless (toType st' == toType outer_t) $
        typeError loc mempty $
          "Ascribed type"
            </> indent 2 (pretty st)
            </> "cannot match outer ascribed type"
            </> indent 2 (pretty outer_t)
      PatAscription
        <$> checkPat' p (Ascribed st')
        <*> pure t'
        <*> pure loc
    NoneInferred ->
      PatAscription
        <$> checkPat' p (Ascribed st')
        <*> pure t'
        <*> pure loc
checkPat' (PatLit l NoInfo loc) (Ascribed t) = do
  t' <- patLitMkType l loc
  ctEq (Reason (locOf loc)) (toType t') (toType t)
  pure $ PatLit l (Info t') loc
checkPat' (PatLit l NoInfo loc) NoneInferred = do
  t' <- patLitMkType l loc
  pure $ PatLit l (Info t') loc
checkPat' (PatConstr n NoInfo ps loc) (Ascribed (Scalar (Sum cs)))
  | Just ts <- M.lookup n cs,
    length ps == length ts = do
      ps' <- zipWithM checkPat' ps $ map Ascribed ts
      pure $ PatConstr n (Info (Scalar (Sum cs))) ps' loc
checkPat' p@(PatConstr {}) (Ascribed t) =
  typeError (locOf p) mempty $
    "Pattern"
      </> indent 2 (pretty p)
      </> "cannot match ascribed type"
      </> indent 2 (pretty t)
checkPat' (PatConstr n NoInfo ps loc) NoneInferred = do
  ps' <- mapM (`checkPat'` NoneInferred) ps
  t <- newTypeWithConstr loc "t" Observe n =<< mapM (asType . patternType) ps'
  pure $ PatConstr n (Info $ toParam Observe t) ps' loc

checkPat ::
  PatBase NoInfo VName (TypeBase Size u) ->
  (Pat ParamType -> TermM a) ->
  TermM a
checkPat p m =
  m =<< checkPat' (fmap (toParam Observe) p) NoneInferred

-- | Bind @let@-bound sizes. This is usually followed by 'bindLetPat'
-- immediately afterwards.
bindSizes :: [SizeBinder VName] -> TermM a -> TermM a
bindSizes [] m = m -- Minor optimisation.
bindSizes sizes m = bind (map sizeWithType sizes) m
  where
    sizeWithType size =
      Ident (sizeName size) (Info (Scalar (Prim (Signed Int64)))) (srclocOf size)

bindLetPat ::
  PatBase NoInfo VName (TypeBase Size u) ->
  Type ->
  (Pat ParamType -> TermM a) ->
  TermM a
bindLetPat p t m = do
  checkPat p $ \p' -> do
    pt <- asType $ patternType p'
    ctEq (ReasonPatMatch (locOf p) (fmap toStruct p) t) pt t
    bind (patIdents (fmap toStruct p')) $ m p'

bindTypes ::
  [(VName, TypeBinding)] ->
  TermM a ->
  TermM a
bindTypes tbinds = localScope extend
  where
    extend scope =
      scope
        { scopeTypeTable = M.fromList tbinds <> scopeTypeTable scope
        }

bindTypeParams :: [TypeParam] -> TermM a -> TermM a
bindTypeParams tparams m =
  bind idents . bindTypes types $ do
    lvl <- curLevel
    modify $ \s ->
      s
        { termTyParams =
            termTyParams s
              <> M.fromList (mapMaybe (typeParam lvl) tparams)
        }
    m
  where
    idents = mapMaybe typeParamIdent tparams
    types = mapMaybe typeParamType tparams
    typeParamType (TypeParamType l v _) =
      Just (v, TypeAbbr l [] $ RetType [] $ Scalar (TypeVar mempty (qualName v) []))
    typeParamType TypeParamDim {} = Nothing
    typeParam lvl (TypeParamType l v loc) = Just (v, (lvl, l, locOf loc))
    typeParam _ _ = Nothing

bindParams ::
  [TypeParam] ->
  [PatBase NoInfo VName ParamType] ->
  ([Pat ParamType] -> TermM a) ->
  TermM a
bindParams tps orig_ps m = bindTypeParams tps $ do
  let descend ps' (p : ps) =
        checkPat p $ \p' ->
          bind (patIdents $ fmap toStruct p') $ incLevel $ descend (p' : ps') ps
      descend ps' [] = m $ reverse ps'

  incLevel $ descend [] orig_ps

checkApplyOne ::
  SrcLoc ->
  (Maybe (QualName VName), Int) ->
  Type ->
  (Maybe Exp, Type) ->
  TermM Type
checkApplyOne loc fname ftype (arg, argtype) = do
  (a, b) <- split ftype
  let reason = case arg of
        Just arg' -> ReasonApply (locOf arg) fname arg' a argtype
        Nothing -> Reason (locOf loc)
  ctEq reason argtype a
  pure b
  where
    split (Scalar (Arrow _ _ _ a (RetType _ b))) =
      pure (a, b `setUniqueness` NoUniqueness)
    split (Array _u s t) = do
      (a, b) <- split $ Scalar t
      pure (arrayOf s a, arrayOf s b)
    split ftype' = do
      a <- newType loc Lifted "arg" NoUniqueness
      b <- newType loc Lifted "res" Nonunique
      let reason = case arg of
            Just arg' -> ReasonApplySplit (locOf loc) fname arg' ftype'
            Nothing -> Reason $ locOf loc
      ctEq reason ftype' $ Scalar $ Arrow NoUniqueness Unnamed Observe a $ RetType [] b
      pure (a, b `setUniqueness` NoUniqueness)

checkApply ::
  SrcLoc ->
  Maybe (QualName VName) ->
  Type ->
  NE.NonEmpty (Maybe Exp, Type) ->
  TermM Type
checkApply loc fname ftype args = do
  (_, rt) <- foldM onArg (0, ftype) args
  pure rt
  where
    onArg (i, f_t) arg = do
      rt <- checkApplyOne loc (fname, i) f_t arg
      pure (i + 1, rt)

checkSlice :: SliceBase NoInfo VName -> TermM [DimIndex]
checkSlice = mapM checkDimIndex
  where
    checkDimIndex (DimFix i) =
      DimFix <$> (require "use as index" anySignedType =<< checkExp i)
    checkDimIndex (DimSlice i j s) =
      DimSlice <$> traverse check i <*> traverse check j <*> traverse check s

    check = require "use in slice" [Signed Int64] <=< checkExp

isSlice :: DimIndexBase f vn -> Bool
isSlice DimSlice {} = True
isSlice DimFix {} = False

checkCase ::
  Type ->
  CaseBase NoInfo VName ->
  TermM (CaseBase Info VName, Type)
checkCase mt (CasePat p e loc) =
  bindLetPat p mt $ \p' -> do
    e' <- checkExp e
    e_t <- expType e'
    pure (CasePat (fmap toStruct p') e' loc, e_t)

checkCases ::
  Type ->
  NE.NonEmpty (CaseBase NoInfo VName) ->
  TermM (NE.NonEmpty (CaseBase Info VName), Type)
checkCases mt rest_cs = do
  let (c, rest_cs') = NE.uncons rest_cs
  (c', c_t) <- checkCase mt c
  case rest_cs' of
    Nothing ->
      pure (NE.singleton c', c_t)
    Just cs -> do
      (cs', cs_t) <- checkCases mt cs
      ctEq (ReasonBranches (locOf c) c_t cs_t) c_t cs_t
      pure (NE.cons c' cs', c_t)

checkRetDecl ::
  Exp ->
  Maybe (TypeExp (ExpBase NoInfo VName) VName) ->
  TermM (Type, Maybe (TypeExp Exp VName))
checkRetDecl body Nothing = (,Nothing) <$> expType body
checkRetDecl body (Just te) = do
  (te', _, RetType _ st, _) <- checkTypeExp checkSizeExp' te
  body_t <- expType body
  st' <- toStruct <$> asType st
  ctEq (ReasonRetType (locOf body) st' body_t) st' body_t
  pure (st', Just te')

-- Add constraints saying that the first type has a (potentially nested) part
-- containing the second type.
--
-- FIXME: the locations here are very bad.
mustHaveSteps ::
  (Pretty a, Located a) =>
  a ->
  Type ->
  [UpdateStep Info VName] ->
  Type ->
  TermM ()
mustHaveSteps src t [] ve_t =
  -- This case is probably never reached.
  ctEq (Reason (locOf src)) t ve_t
mustHaveSteps src t [UpdateStepField f] ve_t = do
  rt :: Type <- newTypeWithField (srclocOf src) "ft" f ve_t
  ctEq (Reason (locOf src)) t rt
mustHaveSteps src t (UpdateStepField f : steps) ve_t = do
  ft <- newType (locOf src) Lifted "ft" NoUniqueness
  rt :: Type <- newTypeWithField (srclocOf src) "ft" f ft
  ctEq (Reason (locOf src)) t rt
  mustHaveSteps src ft steps ve_t
mustHaveSteps src t [UpdateStepSlice slice] ve_t = do
  let num_slices = length $ filter isSlice slice
  update_elem_t <- newElemType (locOf src) "update_elem" NoUniqueness
  ctEq (Reason (locOf src)) t $ arrayOfRank (length slice) update_elem_t
  ctEq (Reason (locOf src)) ve_t $ arrayOfRank num_slices update_elem_t
mustHaveSteps src t (UpdateStepSlice slice : steps) ve_t = do
  let num_slices = length $ filter isSlice slice
  index_tv <- newTyVar (locOf src) Unlifted "index"
  index_elem_t <- newElemType (locOf src) "index_elem" NoUniqueness
  ctEq (Reason (locOf src)) (tyVarType NoUniqueness index_tv) $ arrayOfRank num_slices index_elem_t
  ctEq (Reason (locOf src)) t $ arrayOfRank (length slice) index_elem_t
  mustHaveSteps src (arrayOfRank num_slices index_elem_t) steps ve_t

checkStep :: UpdateStep NoInfo VName -> TermM (UpdateStep Info VName)
checkStep (UpdateStepField f) = pure $ UpdateStepField f
checkStep (UpdateStepSlice slice) = UpdateStepSlice <$> checkSlice slice

checkExp :: ExpBase NoInfo VName -> TermM (ExpBase Info VName)
--
checkExp (Var qn _ loc) = do
  t <- asStructType =<< lookupVar loc qn
  pure $ Var qn (Info t) loc
checkExp (OpSection op _ loc) = do
  ftype <- asStructType =<< lookupVar loc op
  pure $ OpSection op (Info ftype) loc
checkExp (Negate arg loc) = do
  arg' <- require "numeric negation" anyNumberType =<< checkExp arg
  pure $ Negate arg' loc
checkExp (Not arg loc) = do
  arg' <- require "logical negation" (Bool : anyIntType) =<< checkExp arg
  pure $ Not arg' loc
checkExp (Hole NoInfo loc) =
  Hole <$> (Info <$> newType loc Lifted "hole" NoUniqueness) <*> pure loc
checkExp (Parens e loc) =
  Parens <$> checkExp e <*> pure loc
checkExp (TupLit es loc) =
  TupLit <$> mapM checkExp es <*> pure loc
checkExp (QualParens (modname, modnameloc) e loc) = do
  mod <- lookupMod modname
  case mod of
    ModEnv env -> local (`withEnv` env) $ do
      e' <- checkExp e
      pure $ QualParens (modname, modnameloc) e' loc
    ModFun {} ->
      typeError loc mempty . withIndexLink "module-is-parametric" $
        "Module" <+> pretty modname <+> " is a parametric module."
--
checkExp (IntLit x NoInfo loc) = do
  t <- newTypeOverloaded loc "num" anyNumberType
  pure $ IntLit x (Info t) loc
checkExp (FloatLit x NoInfo loc) = do
  t <- newTypeOverloaded loc "float" anyFloatType
  pure $ FloatLit x (Info t) loc
checkExp (Literal v loc) =
  pure $ Literal v loc
checkExp (StringLit vs loc) =
  pure $ StringLit vs loc
-- No need to type check this, as these are only produced by the
-- parser if the elements are monomorphic and all match.
checkExp (ArrayVal vs t loc) =
  pure $ ArrayVal vs t loc
checkExp (ArrayLit es _ loc) = do
  -- TODO: this will produce an enormous number of constraints and
  -- type variables for pathologically large arrays with
  -- type-unsuffixed integers. Add some special case that handles that
  -- more efficiently.
  et <- newElemType loc "et" NoUniqueness
  es' <- forM es $ \e -> do
    e' <- checkExp e
    e_t <- expType e'
    et' <- asType et
    ctEq (Reason (locOf loc)) e_t et'
    pure e'
  let arr_t = arrayOf (Shape [sizeFromInteger (L.genericLength es) loc]) et
  pure $ ArrayLit es' (Info arr_t) loc
checkExp (RecordLit fs loc) =
  RecordLit <$> evalStateT (mapM checkField fs) mempty <*> pure loc
  where
    checkField (RecordFieldExplicit f e rloc) = do
      errIfAlreadySet (unLoc f) rloc
      modify $ M.insert (unLoc f) rloc
      RecordFieldExplicit f <$> lift (checkExp e) <*> pure rloc
    checkField (RecordFieldImplicit name NoInfo rloc) = do
      errIfAlreadySet (baseName (unLoc name)) rloc
      t <- lift $ asStructType =<< lookupVar rloc (qualName (unLoc name))
      modify $ M.insert (baseName (unLoc name)) rloc
      pure $ RecordFieldImplicit name (Info t) rloc

    errIfAlreadySet f rloc = do
      maybe_sloc <- gets $ M.lookup f
      case maybe_sloc of
        Just sloc ->
          lift . typeError rloc mempty $
            "Field"
              <+> dquotes (pretty f)
              <+> "previously defined at"
              <+> pretty (locStrRel rloc sloc)
              <> "."
        Nothing -> pure ()

--
checkExp (Attr info e loc) =
  Attr <$> checkAttr info <*> checkExp e <*> pure loc
checkExp (Assert e1 e2 NoInfo loc) = do
  e1' <- require "being asserted" [Bool] =<< checkExp e1
  e2' <- checkExp e2
  pure $ Assert e1' e2' (Info (prettyText e1)) loc
--
checkExp (Constr name es NoInfo loc) = do
  es' <- mapM checkExp es
  es_ts <- mapM expType es'
  t <- newTypeWithConstr loc "t" NoUniqueness name es_ts
  pure $ Constr name es' (Info t) loc
--
checkExp (AppExp (Apply fe args loc) NoInfo) = do
  fe' <- checkExp fe
  (args', apply_args) <-
    fmap NE.unzip . forM args $ \(_, arg) -> do
      arg' <- checkExp arg
      arg_t <- expType arg'
      pure (arg', (Just arg', arg_t))
  fe_t <- expType fe'
  rt <- checkApply loc fname fe_t apply_args
  rt' <- asStructType rt
  let args'' = NE.map (\arg -> (Info Nothing, arg)) args'
  pure $ AppExp (Apply fe' args'' loc) $ Info (AppRes rt' [])
  where
    fname =
      case fe of
        Var v _ _ -> Just v
        _ -> Nothing
checkExp (AppExp (BinOp (op, oploc) NoInfo (e1, _) (e2, _) loc) NoInfo) = do
  ftype <- lookupVar oploc op
  e1' <- checkExp e1
  e1_t <- expType e1'
  e2' <- checkExp e2
  e2_t <- expType e2'

  rt <-
    checkApply
      loc
      (Just op)
      ftype
      ((Just e1', e1_t) NE.:| [(Just e2', e2_t)])
  rt' <- asStructType rt

  ftype' <- asStructType ftype
  pure $
    AppExp
      (BinOp (op, oploc) (Info ftype') (e1', Info Nothing) (e2', Info Nothing) loc)
      (Info (AppRes rt' []))
--
checkExp (OpSectionLeft op _ e _ _ loc) = do
  optype <- lookupVar loc op
  e' <- checkExp e
  e_t <- expType e'
  t2 <- newType loc Lifted "t" NoUniqueness
  t2' <- asStructType t2
  rt <-
    checkApply
      loc
      (Just op)
      optype
      ((Just e', e_t) NE.:| [(Nothing, t2)])
  rt' <- asStructType rt

  t1 <- asStructType e_t
  optype' <- asStructType optype
  pure $
    OpSectionLeft
      op
      (Info optype')
      e'
      ( Info (Unnamed, toParam Observe t1, Nothing),
        Info (Unnamed, toParam Observe t2')
      )
      (Info (RetType [] (rt' `setUniqueness` Nonunique)), Info [])
      loc
checkExp (OpSectionRight op _ e _ NoInfo loc) = do
  optype <- lookupVar loc op
  e' <- checkExp e
  e_t <- expType e'
  t1 <- newType loc Lifted "t" NoUniqueness
  t1' <- asStructType t1
  rt <-
    checkApply
      loc
      (Just op)
      optype
      ((Nothing, t1) NE.:| [(Just e', e_t)])
  rt' <- asStructType rt
  t2 <- asStructType e_t

  optype' <- asStructType optype
  pure $
    OpSectionRight
      op
      (Info optype')
      e'
      -- Dummy types.
      ( Info (Unnamed, toParam Observe t1'),
        Info (Unnamed, toParam Observe t2, Nothing)
      )
      (Info $ RetType [] (rt' `setUniqueness` Nonunique))
      loc
--
checkExp e@(UpdateSection steps NoInfo loc) = do
  steps' <- mapM checkStep steps
  -- Lifted, as a pure field projection works on records with
  -- function-typed fields. Any slice steps will constrain the
  -- relevant parts to be arrays (of unlifted elements) anyway.
  src_t <- newType loc Lifted "update" NoUniqueness
  ve_t <- newType loc Lifted "update_elem" NoUniqueness
  mustHaveSteps e src_t steps' ve_t
  ft <-
    asStructType $
      Scalar $
        Arrow mempty Unnamed Observe src_t $
          second (const Nonunique) (RetType [] ve_t)
  pure $ UpdateSection steps' (Info ft) loc

--
checkExp (Lambda params body retdecl NoInfo loc) = do
  bindParams [] params $ \params' -> do
    body' <- checkExp body

    (body_t, retdecl') <- checkRetDecl body' retdecl
    body_t' <- asStructType body_t
    let ret = RetType [] $ toRes Nonunique body_t'
    pure $ Lambda params' body' retdecl' (Info ret) loc
--
checkExp (AppExp (LetPat sizes pat e body loc) _) = do
  e' <- checkExp e
  e_t <- expType e'

  bindSizes sizes . incLevel . bindLetPat pat e_t $ \pat' -> do
    body' <- incLevel $ checkExp body
    body_t <- expType body'

    body_t' <- asStructType body_t
    pure $
      AppExp
        (LetPat sizes (fmap toStruct pat') e' body' loc)
        (Info $ AppRes body_t' [])
--
checkExp (AppExp (LetFun name (tparams, params, retdecl, NoInfo, e) body loc) _) = do
  (tparams', params', retdecl', rettype, e') <-
    bindParams tparams params $ \params' -> do
      e' <- checkExp e
      (e_t, retdecl') <- checkRetDecl e' retdecl
      pure (tparams, params', retdecl', fmap (const Nonunique) e_t, e')

  params'' <- mapM (traverse asType) params'

  let entry = BoundV tparams' $ funType params'' $ RetType [] rettype
      bindF scope =
        scope
          { scopeVtable = M.insert (fst name) entry $ scopeVtable scope
          }
  body' <- localScope bindF $ checkExp body
  body_t <- expType body'

  body_t' <- asStructType body_t
  rettype' <- asStructType rettype
  pure $
    AppExp
      ( LetFun
          name
          (tparams', params', retdecl', Info (RetType [] rettype'), e')
          body'
          loc
      )
      (Info $ AppRes body_t' [])
--
checkExp (AppExp (Range start maybe_step end loc) _) = do
  start' <- require "use in range expression" anyIntType =<< checkExp start
  let check e = do
        e' <- checkExp e
        start_t <- expType start'
        e_t <- expType e'
        ctEq (Reason (locOf e')) start_t e_t
        pure e'
  maybe_step' <- traverse check maybe_step
  end' <- traverse check end
  range_t <- newElemType loc "range" NoUniqueness
  range_t' <- asType range_t
  start_t <- expType start'
  ctEq (Reason (locOf start')) range_t' (arrayOfRank 1 start_t)
  pure $ AppExp (Range start' maybe_step' end' loc) $ Info $ AppRes range_t []
--
checkExp (Project k e NoInfo loc) = do
  e' <- checkExp e
  kt <- newType loc Lifted "kt" NoUniqueness
  t <- newTypeWithField loc "t" k kt
  e_t <- expType e'
  ctEq (Reason (locOf e')) e_t t
  kt' <- asStructType kt
  pure $ Project k e' (Info kt') loc
--
checkExp (Update src steps ve NoInfo loc) = do
  src' <- checkExp src
  src_t <- expType src'
  src_t' <- asStructType src_t
  ve' <- checkExp ve
  ve_t <- expType ve'
  steps' <- mapM checkStep steps
  mustHaveSteps src' src_t steps' ve_t
  pure $ Update src' steps' ve' (Info src_t') loc

--
checkExp (AppExp (Index e slice loc) _) = do
  e' <- checkExp e
  e_t <- expType e'
  slice' <- checkSlice slice
  index_tv <- newTyVar loc Unlifted "index"
  index_elem_t <- newElemType loc "index_elem" NoUniqueness
  let num_slices = length $ filter isSlice slice
  ctEq (Reason (locOf loc)) (tyVarType NoUniqueness index_tv) $ arrayOfRank num_slices index_elem_t
  ctEq (Reason (locOf e')) e_t $ arrayOfRank (length slice) index_elem_t
  pure $ AppExp (Index e' slice' loc) (Info $ AppRes (tyVarType NoUniqueness index_tv) [])
--
checkExp (AppExp (LetWith dest src steps ve body loc) _) = do
  src_t <- lookupVar (srclocOf src) $ qualName $ identName src
  src_t' <- asStructType src_t
  let src' = src {identType = Info src_t'}
      dest' = dest {identType = Info src_t'}
  steps' <- mapM checkStep steps
  ve' <- checkExp ve
  ve_t <- expType ve'
  mustHaveSteps src' src_t steps' ve_t
  bind [dest'] $ do
    body' <- checkExp body
    body_t <- expType body'
    body_t' <- asStructType body_t
    pure $ AppExp (LetWith dest' src' steps' ve' body' loc) (Info $ AppRes body_t' [])
--
checkExp (AppExp (If e1 e2 e3 loc) _) = do
  e1' <- checkExp e1
  e1_t <- expType e1'
  e2' <- checkExp e2
  e2_t <- expType e2'
  e3' <- checkExp e3
  e3_t <- expType e3'
  if_t <- newType loc SizeLifted "if_t" NoUniqueness

  ctEq (Reason (locOf e1')) e1_t (Scalar (Prim Bool))
  ctEq (ReasonBranches (locOf loc) e2_t e3_t) e2_t if_t
  ctEq (ReasonBranches (locOf loc) e2_t e3_t) e3_t if_t

  if_t' <- asStructType if_t
  pure $ AppExp (If e1' e2' e3' loc) (Info $ AppRes if_t' [])
--
checkExp (AppExp (Match e cs loc) _) = do
  e' <- checkExp e
  e_t <- expType e'
  (cs', t) <- checkCases e_t cs

  match_t <- newType loc SizeLifted "match_t" NoUniqueness
  ctEq (Reason (locOf loc)) match_t t

  match_t' <- asStructType match_t
  pure $ AppExp (Match e' cs' loc) (Info $ AppRes match_t' [])
--
checkExp (AppExp (Loop _ pat arg form body loc) _) = do
  arg' <- checkExp $ case arg of
    LoopInitExplicit e -> e
    LoopInitImplicit _ ->
      -- Should have been filled out in Names
      error "Unspected LoopInitImplicit"
  arg_t <- expType arg'
  loop_t <- newType loc SizeLifted "loop_t" NoUniqueness
  ctEq (Reason (locOf loc)) arg_t loop_t
  bindLetPat pat arg_t $ \pat' -> do
    (form', body') <-
      case form of
        For (Ident i _ iloc) bound -> do
          bound' <- require "loop bound" anyIntType =<< checkExp bound
          bound_t <- expType bound'
          bound_t' <- asStructType bound_t
          let i' = Ident i (Info bound_t') iloc
          bind [i'] $ do
            body' <- checkExp body
            pure (For i' bound', body')
        While cond -> do
          cond' <- checkExp cond
          body' <- checkExp body
          pure (While cond', body')
        ForIn elemp arr -> do
          arr' <- checkExp arr
          elem_t <- newElemType elemp "elem" NoUniqueness
          arr_t <- expType arr'
          elem_t' <- asType elem_t
          ctEq (Reason (locOf arr')) arr_t $ arrayOfRank 1 elem_t'
          bindLetPat elemp elem_t' $ \elemp' -> do
            body' <- checkExp body
            pure (ForIn (toStruct <$> elemp') arr', body')
    body_t <- expType body'
    ctEq (Reason (locOf loc)) arg_t body_t
    pure $
      AppExp
        (Loop [] pat' (LoopInitExplicit arg') form' body' loc)
        (Info (AppRes (patternStructType pat') []))
--
checkExp (Ascript e te loc) = do
  e' <- checkExp e
  (te', _, RetType _ st, _) <- checkTypeExp checkSizeExp' te
  e_t <- expType e'
  st' <- asType st
  ctEq (ReasonAscription (locOf e') (toStruct st') (toStruct e_t)) e_t st'
  pure $ Ascript e' te' loc
checkExp (Coerce e te NoInfo loc) = do
  e' <- checkExp e
  (te', _, RetType _ st, _) <- checkTypeExp checkSizeExp' te
  e_t <- expType e'
  st' <- asType st
  ctEq (Reason (locOf e')) e_t st'
  pure $ Coerce e' te' (Info (toStruct st)) loc

doDefault ::
  [VName] ->
  VName ->
  Either [PrimType] (TypeBase () NoUniqueness) ->
  TermM (TypeBase () NoUniqueness)
doDefault tyvars_at_toplevel v (Left pts)
  | [pt] <- pts =
      pure $ Scalar $ Prim pt
  | Signed Int32 `elem` pts = do
      when (v `elem` tyvars_at_toplevel) $
        warn usage "Defaulting ambiguous type to i32."
      pure $ Scalar $ Prim $ Signed Int32
  | FloatType Float64 `elem` pts = do
      when (v `elem` tyvars_at_toplevel) $
        warn usage "Defaulting ambiguous type to f64."
      pure $ Scalar $ Prim $ FloatType Float64
  | otherwise =
      typeError usage mempty . withIndexLink "ambiguous-type" $
        "Type is ambiguous (could be one of"
          <+> commasep (map pretty pts)
          <> ")."
            </> "Add a type annotation to disambiguate the type."
  where
    usage = mkUsage NoLoc "overload"
doDefault _ _ (Right t) = pure t

-- | Apply defaults on otherwise ambiguous types. This may result in
-- some type variables becoming known, so we have to perform
-- substitutions on the RHS of the substitutions afterwards.
doDefaults ::
  [VName] ->
  M.Map TyVar (Either [PrimType] (TypeBase () NoUniqueness)) ->
  TermM (M.Map TyVar (TypeBase () NoUniqueness))
doDefaults tyvars_at_toplevel substs = do
  substs' <- M.traverseWithKey (doDefault tyvars_at_toplevel) substs
  pure $ M.map (substTyVars (`M.lookup` substs')) substs'

generalise ::
  TypeBase () NoUniqueness ->
  [UnconTyVar] ->
  Solution ->
  ([TypeParam], [VName])
generalise fun_t unconstrained solution =
  -- Candidates for let-generalisation are those type variables that
  -- are used in fun_t.
  let visible = foldMap expandTyVars $ typeVars fun_t
      onTyVar (v, l)
        | v `S.member` visible = Left $ TypeParamType l v mempty
        | otherwise = Right v
   in partitionEithers $ map onTyVar unconstrained
  where
    expandTyVars v =
      case M.lookup v solution of
        Just (Right t) -> foldMap expandTyVars $ typeVars t
        _ -> S.singleton v

generaliseAndDefaults ::
  [UnconTyVar] ->
  Solution ->
  TypeBase () NoUniqueness ->
  TermM ([TypeParam], M.Map VName (TypeBase () NoUniqueness))
generaliseAndDefaults unconstrained solution t = do
  let (generalised, unconstrained') =
        generalise t unconstrained solution
      -- See #1552 for why we resolve unconstrained and un-generalised type
      -- variables to ().
      units = M.fromList (map (,Right (Scalar (Record mempty))) unconstrained')
  solution' <- doDefaults (S.toList $ typeVars t) (units <> solution)
  pure
    ( generalised,
      solution'
    )

-- | Verify that the recorded type parameter instantiations respect the
-- liftedness of the type parameters. The constraint solver merely propagates
-- liftedness constraints; this is where they are enforced for instantiations,
-- as only here do we know why the constraints exist. (Other liftedness rules
-- are enforced by 'localChecks' in Language.Futhark.TypeChecker.Terms.)
checkTyInstLiftedness :: Solution -> TermM ()
checkTyInstLiftedness solution = do
  typarams <- gets termTyParams
  tyset <- asks termTySet
  mapM_ (check typarams tyset) . reverse =<< gets termTyInsts
  where
    -- A Lifted type parameter permits any instantiation.
    check _ _ (TyInst _ _ Lifted _) = pure ()
    check typarams tyset (TyInst loc qn l v)
      | Just (Right t) <- M.lookup v solution = do
          unless (orderZero t) . typeError loc mempty $
            "Type"
              </> indent 2 (pretty t)
              </> "found to be functional."
              </> when_inst
          let bad = case l of
                Unlifted -> [Lifted, SizeLifted]
                _ -> [Lifted]
              -- The liftedness of a type variable is given by its
              -- binding if it is a type parameter, and by the type
              -- set if it is an abstract type.
              badVar qv =
                case M.lookup (qualLeaf qv) typarams of
                  Just (_, pl, ploc) -> do
                    guard $ pl `elem` bad
                    Just $
                      "Type parameter"
                        <+> dquotes (prettyName (qualLeaf qv))
                        <+> "bound at"
                        <+> pretty (locStr ploc)
                  Nothing -> do
                    al <- M.lookup qv tyset
                    guard $ al `elem` bad
                    Just $ "Type" <+> dquotes (pretty qv)
          case mapMaybe badVar $ typeQualVars t of
            what : _ ->
              typeError loc mempty $
                what
                  <+> case l of
                    Unlifted -> "is lifted and cannot be an array element."
                    _ -> "is lifted and may be a functional type."
                  </> when_inst
            [] -> pure ()
      | otherwise = pure ()
      where
        when_inst =
          "When instantiating type parameter of" <+> dquotes (pretty qn) <> "."

-- | Check a potentially recursive function body. The function is bound to a
-- fresh monomorphic type variable while its body is checked; that variable is
-- then constrained to the actual function type, and the constraint solver ties
-- the knot. A parameterless binding cannot be recursive (see 'resolveValBind'),
-- so it is checked with no self-reference in scope. See Note [Checking recursive
-- functions] in Language.Futhark.TypeChecker.Terms.
checkRecursive ::
  VName ->
  SrcLoc ->
  [Pat ParamType] ->
  ExpBase NoInfo VName ->
  TermM (ExpBase Info VName)
checkRecursive _ _ [] body = checkExp body
checkRecursive fname loc params' body = do
  ftype <- newType loc Lifted (baseName fname) NoUniqueness
  let bindF scope =
        scope {scopeVtable = M.insert fname (BoundV [] ftype) $ scopeVtable scope}
  body' <- localScope bindF $ checkExp body
  body_t <- expType body'
  let fun_t =
        foldFunType
          (map (first (const ()) . patternType) params')
          (RetType [] $ bimap (const ()) (const Nonunique) body_t)
  ctEq (Reason (locOf loc)) ftype fun_t
  pure body'

-- | Replace artificial variables with the types they denote, so that no
-- artificial variable leaks into the result.
onArtificial ::
  M.Map TyVar (TypeBase () NoUniqueness) ->
  M.Map TyVar (TypeBase () NoUniqueness) ->
  M.Map TyVar (TypeBase () NoUniqueness)
onArtificial artificial solution =
  M.map (substTyVars (`M.lookup` solution) . first (const ())) artificial
    <> solution

-- | Type check a single value definition.
checkValDef ::
  ( VName,
    Maybe (TypeExp (ExpBase NoInfo VName) VName),
    [TypeParam],
    [PatBase NoInfo VName ParamType],
    ExpBase NoInfo VName,
    SrcLoc
  ) ->
  TypeM
    ( Either TypeError ([TypeParam], M.Map TyVar (TypeBase () NoUniqueness)),
      [Pat ParamType],
      Maybe (TypeExp Exp VName),
      Exp
    )
checkValDef (fname, retdecl, tparams, params, body, loc) = runTermM $ do
  (params', body', retdecl') <-
    bindParams tparams params $ \params' -> do
      body' <- checkRecursive fname loc params' body
      (_, retdecl') <- checkRetDecl body' retdecl
      pure (params', body', retdecl')

  cts <- gets termConstraints
  tyvars <- gets termTyVars
  typarams <- gets termTyParams
  artificial <- gets $ M.map (first (const ())) . termArtificial

  solution <-
    bitraverse
      pure
      (fmap (second (onArtificial artificial)) . onTySolution params' body')
      $ solve (reverse cts) typarams tyvars
  pure (solution, params', retdecl', body')
  where
    onTySolution params' body' (unconstrained, solution) = do
      checkTyInstLiftedness solution
      body_t <- expType body'
      let fun_t =
            foldFunType
              (map (first (const ()) . patternType) params')
              (RetType [] $ bimap (const ()) (const Nonunique) body_t)
      generaliseAndDefaults unconstrained solution fun_t

-- | Type check a single expression, which may have a polymorphic
-- type.
checkSingleExp ::
  ExpBase NoInfo VName ->
  TypeM
    ( Either TypeError ([TypeParam], M.Map TyVar (TypeBase () NoUniqueness)),
      Exp
    )
checkSingleExp e = runTermM $ do
  e' <- checkExp e
  cts <- gets termConstraints
  tyvars <- gets termTyVars
  typarams <- gets termTyParams
  artificial <- gets termArtificial

  case solve cts typarams tyvars of
    Left err -> pure (Left err, e')
    Right (unconstrained, solution) -> do
      checkTyInstLiftedness solution
      e_t <- expType e'
      x <-
        second (onArtificial (M.map (first (const ())) artificial))
          <$> generaliseAndDefaults unconstrained solution (first (const ()) e_t)
      pure (Right x, e')

-- | Type-check a single size expression in isolation, which must have
-- type @i64@.
checkSizeExp ::
  ExpBase NoInfo VName ->
  TypeM
    ( Either TypeError ([UnconTyVar], M.Map TyVar (TypeBase () NoUniqueness)),
      Exp
    )
checkSizeExp e = runTermM $ do
  e' <- checkSizeExp' e
  cts <- gets termConstraints
  tyvars <- gets termTyVars
  typarams <- gets termTyParams
  artificial <- gets termArtificial

  case solve cts typarams tyvars of
    Left err -> pure (Left err, e')
    Right (unconstrained, solution) -> do
      checkTyInstLiftedness solution
      solution' <-
        onArtificial (M.map (first (const ())) artificial)
          <$> doDefaults mempty solution
      pure (Right (unconstrained, solution'), e')
