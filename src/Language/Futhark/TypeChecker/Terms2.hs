-- | A very WIP reimplementation of type checking of terms.
--
-- The strategy is to split type checking into two (main) passes:
--
-- 1) A size-agnostic pass that generates constraints (type Ct) which
-- are then solved offline to find a solution. This produces an AST
-- where most of the type annotations are just references to type
-- variables. Further, all the size-specific annotations (e.g.
-- existential sizes) just contain dummy values, such as empty lists.
-- The constraints use a type representation where all dimensions are
-- the same. However, we do try to take to store the sizes resulting
-- from explicit type ascriptions - these cannot refer to inferred
-- existentials, so it is safe to resolve them here. We don't do
-- anything with this information, however.
--
-- 2) Pass (1) has given us a program where we know the types of
-- everything, but the sizes of nothing. Pass (2) then does
-- essentially size inference, much like the current/old type checker,
-- but of course with the massive benefit of already knowing the full
-- type of everything. This can be implemented using online constraint
-- solving (as before), or perhaps a completely syntax-driven
-- approach.
--
-- As of this writing, only the constraint generation part of pass (1)
-- has been implemented, and it is very likely that some of the
-- constraints are actually wrong. Next step is to imlement the
-- solver. Currently all we do is dump the constraints to the
-- terminal.
--
-- Also, no thought whatsoever has been put into quality of type
-- errors yet. However, I think an approach based on tacking source
-- information onto constraints should work well, as all constraints
-- ultimately originate from some bit of program syntax.
--
-- Also no thought has been put into how to handle the liftedness
-- stuff. Since it does not really affect choices made during
-- inference, perhaps we can do it in a post-inference check.
module Language.Futhark.TypeChecker.Terms2
  ( checkValDef,
    checkSingleExp,
    checkSizeExp,
    Solution,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
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
import Futhark.Util (debugTraceM, mapAccumLM, nubOrd)
import Futhark.Util.Pretty
import Language.Futhark
import Language.Futhark.TypeChecker.Constraints
import Language.Futhark.TypeChecker.Monad hiding (BoundV, lookupMod)
import Language.Futhark.TypeChecker.Monad qualified as TypeM
import Language.Futhark.TypeChecker.Rank
import Language.Futhark.TypeChecker.Types
import Language.Futhark.TypeChecker.Unify (Level, mkUsage)
import Prelude hiding (mod)

data Inferred t
  = NoneInferred
  | Ascribed t

instance Functor Inferred where
  fmap _ NoneInferred = NoneInferred
  fmap f (Ascribed t) = Ascribed (f t)

data ValBinding
  = BoundV [TypeParam] Type
  | OverloadedF [PrimType] [Maybe PrimType] (Maybe PrimType)
  | EqualityF
  deriving (Show)

data TermScope = TermScope
  { scopeVtable :: M.Map VName ValBinding,
    scopeTypeTable :: M.Map VName TypeBinding,
    scopeModTable :: M.Map VName Mod
  }
  deriving (Show)

instance Semigroup TermScope where
  TermScope vt1 tt1 mt1 <> TermScope vt2 tt2 mt2 =
    TermScope (vt2 `M.union` vt1) (tt2 `M.union` tt1) (mt1 `M.union` mt2)

-- | Type checking happens with access to this environment.  The
-- 'TermScope' will be extended during type-checking as bindings come into
-- scope.
data TermEnv = TermEnv
  { termScope :: TermScope,
    termLevel :: Level,
    termOuterEnv :: Env,
    termImportName :: ImportName
  }

-- | The state is a set of constraints and a counter for generating
-- type names.  This is distinct from the usual counter we use for
-- generating unique names, as these will be user-visible.
data TermState = TermState
  { termConstraints :: Constraints,
    termTyVars :: TyVars,
    termTyParams :: TyParams,
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

envToTermScope :: Env -> TermScope
envToTermScope env =
  TermScope
    { scopeVtable = vtable,
      scopeTypeTable = envTypeTable env,
      scopeModTable = envModTable env
    }
  where
    vtable = M.map valBinding $ envVtable env
    valBinding (TypeM.BoundV tps v) = BoundV tps $ toType v

initialTermScope :: TermScope
initialTermScope =
  TermScope
    { scopeVtable = initialVtable,
      scopeTypeTable = mempty,
      scopeModTable = mempty
    }
  where
    initialVtable = M.fromList $ mapMaybe addIntrinsicF $ M.toList intrinsics

    prim = Scalar . Prim
    arrow x y = Scalar $ Arrow mempty Unnamed Observe x y

    addIntrinsicF (name, IntrinsicMonoFun pts t) =
      Just (name, BoundV [] $ arrow pts' $ RetType [] $ prim t)
      where
        pts' = case pts of
          [pt] -> prim pt
          _ -> Scalar $ tupleRecord $ map prim pts
    addIntrinsicF (name, IntrinsicOverloadedFun ts pts rts) =
      Just (name, OverloadedF ts pts rts)
    addIntrinsicF (name, IntrinsicPolyFun tvs pts rt) =
      Just
        ( name,
          BoundV tvs $ toType $ foldFunType pts rt
        )
    addIntrinsicF (name, IntrinsicEquality) =
      Just (name, EqualityF)
    addIntrinsicF _ = Nothing

runTermM :: TermM a -> TypeM a
runTermM (TermM m) = do
  initial_scope <- (initialTermScope <>) . envToTermScope <$> askEnv
  name <- askImportName
  outer_env <- askEnv
  src <- gets stateNameSource
  let initial_env =
        TermEnv
          { termScope = initial_scope,
            termLevel = 0,
            termImportName = name,
            termOuterEnv = outer_env
          }
      initial_state =
        TermState
          { termConstraints = mempty,
            termTyVars = mempty,
            termTyParams = mempty,
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

newTyVarWith :: Name -> TyVarInfo -> TermM TyVar
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

newTypeWithConstr :: SrcLoc -> Name -> u -> Name -> [TypeBase SComp u] -> TermM (TypeBase d u)
newTypeWithConstr loc desc u k ts =
  tyVarType u <$> newTyVarWith desc (TyVarSum (locOf loc) $ M.singleton k ts')
  where
    ts' = map (`setUniqueness` NoUniqueness) ts

newTypeOverloaded :: SrcLoc -> Name -> [PrimType] -> TermM (TypeBase d NoUniqueness)
newTypeOverloaded loc name pts =
  tyVarType NoUniqueness <$> newTyVarWith name (TyVarPrim (locOf loc) pts)

newSVar :: loc -> Name -> TermM SVar
newSVar _loc desc = do
  i <- incCounter
  newID $ mkTypeVarName desc i

newArtificial :: u -> TypeBase SComp u -> TermM (TypeBase Size u)
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
asStructType :: TypeBase SComp u -> TermM (TypeBase Size u)
asStructType (Scalar (Prim pt)) = pure $ Scalar $ Prim pt
asStructType (Scalar (TypeVar u v [])) = pure $ Scalar $ TypeVar u v []
asStructType (Scalar (Arrow u pname d t1 (RetType ext t2))) = do
  t1' <- asStructType t1
  t2' <- asStructType t2
  pure $ Scalar $ Arrow u pname d t1' $ RetType ext t2'
asStructType (Scalar (Record fs)) =
  Scalar . Record <$> traverse asStructType fs
asStructType (Scalar (Sum cs)) =
  Scalar . Sum <$> traverse (mapM asStructType) cs
asStructType t@(Scalar (TypeVar u _ _)) =
  newArtificial u t
asStructType t@(Array u _ _) = do
  newArtificial u t

asType :: (Monoid u) => TypeBase Size u -> TermM (TypeBase SComp u)
asType t = do
  artificial <- gets termArtificial
  pure $ substTyVars (`M.lookup` artificial) (toType t)

expType :: Exp -> TermM Type
expType = asType . typeOf -- NOTE: Only place you should use typeOf.

addCt :: Ct -> TermM ()
addCt ct = modify $ \s -> s {termConstraints = ct : termConstraints s}

ctEq :: Reason -> TypeBase SComp u1 -> TypeBase SComp u2 -> TermM ()
ctEq reason t1 t2 =
  -- As a minor optimisation, do not add constraint if the types are
  -- equal.
  unless (t1' == t2') $ addCt $ CtEq reason t1' t2'
  where
    t1' = t1 `setUniqueness` NoUniqueness
    t2' = t2 `setUniqueness` NoUniqueness

ctAM :: Reason -> SVar -> SVar -> Shape SComp -> TermM ()
ctAM reason r m f = addCt $ CtAM reason r m f

localScope :: (TermScope -> TermScope) -> TermM a -> TermM a
localScope f = local $ \tenv -> tenv {termScope = f $ termScope tenv}

withEnv :: TermEnv -> Env -> TermEnv
withEnv tenv env = tenv {termScope = termScope tenv <> envToTermScope env}

lookupQualNameEnv :: QualName VName -> TermM TermScope
lookupQualNameEnv (QualName [q] _)
  | baseTag q <= maxIntrinsicTag = asks termScope -- Magical intrinsic module.
lookupQualNameEnv qn@(QualName quals _) = do
  scope <- asks termScope
  descend scope quals
  where
    descend scope [] = pure scope
    descend scope (q : qs)
      | Just (ModEnv q_scope) <- M.lookup q $ scopeModTable scope =
          descend (envToTermScope q_scope) qs
      | otherwise =
          error $ "lookupQualNameEnv " <> show qn

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
arrayOfRank n = arrayOf $ Shape $ replicate n SDim

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
instTypeScheme _qn loc tparams t = do
  (names, substs) <- fmap (unzip . catMaybes) $
    forM tparams $ \tparam ->
      case tparam of
        TypeParamType l v _ -> do
          v' <- newTyVar loc l $ nameFromString $ takeWhile isAscii $ baseString v
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
  case M.lookup name $ scopeVtable scope of
    Nothing ->
      error $ "lookupVar: " <> show qn
    Just (BoundV tparams t) -> do
      if null tparams && null qs
        then pure t
        else do
          (_tnames, t') <- instTypeScheme qn loc tparams t
          -- TODO - qualify type names, like in the old type checker.
          pure t'
    Just EqualityF -> do
      argtype <- tyVarType Observe <$> newTyVarWith "t" (TyVarFree (locOf loc) Unlifted)
      pure $ foldFunType [argtype, argtype] $ RetType [] $ Scalar $ Prim Bool
    Just (OverloadedF ts pts rt) -> do
      argtype <- newTypeOverloaded loc "t" ts
      let (pts', rt') = instOverloaded argtype pts rt
      pure $ foldFunType (map (second $ const Observe) pts') $ RetType [] $ second (const Nonunique) rt'
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
  Inferred (TypeBase SComp Diet) ->
  TermM (Pat ParamType)
checkPat' (PatParens p loc) t =
  PatParens <$> checkPat' p t <*> pure loc
checkPat' (PatAttr attr p loc) t =
  PatAttr <$> checkAttr attr <*> checkPat' p t <*> pure loc
checkPat' (Id name NoInfo loc) (Ascribed t) = do
  t' <- asStructType t
  pure $ Id name (Info t') loc
checkPat' (Id name NoInfo loc) NoneInferred = do
  t <- newType loc Lifted "t" Observe
  pure $ Id name (Info t) loc
checkPat' (Wildcard _ loc) (Ascribed t) = do
  t' <- asStructType t
  pure $ Wildcard (Info t') loc
checkPat' (Wildcard NoInfo loc) NoneInferred = do
  t <- newType loc Lifted "t" Observe
  pure $ Wildcard (Info t) loc
checkPat' (TuplePat ps loc) (Ascribed t)
  | Just ts <- isTupleRecord t,
    length ts == length ps =
      TuplePat
        <$> zipWithM checkPat' ps (map Ascribed ts)
        <*> pure loc
  | otherwise = do
      ps_tvs <- replicateM (length ps) (newTyVar loc Lifted "t")
      ctEq
        (ReasonPatMatch (locOf loc) (TuplePat ps loc) (toStruct t))
        (Scalar (tupleRecord $ map (tyVarType NoUniqueness) ps_tvs))
        t
      TuplePat <$> zipWithM checkPat' ps (map (Ascribed . tyVarType Observe) ps_tvs) <*> pure loc
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
      p_fs' <-
        traverse (const $ newType loc Lifted "t" NoUniqueness) $
          M.fromList $
            map (first unLoc) p_fs
      ctEq (Reason (locOf loc)) (Scalar (Record p_fs')) t
      checkPat' p $ Ascribed $ Observe <$ Scalar (Record p_fs')
  where
    check (L f_loc f, p_f) (_, t_f) =
      (L f_loc f,) <$> checkPat' p_f (Ascribed t_f)
checkPat' (RecordPat fs loc) NoneInferred =
  RecordPat . M.toList
    <$> traverse (`checkPat'` NoneInferred) (M.fromList fs)
    <*> pure loc
checkPat' (PatAscription p t loc) maybe_outer_t = do
  (t', _, RetType _ st, _) <- checkTypeExp checkSizeExp' t

  -- Uniqueness kung fu to make the Monoid(mempty) instance give what
  -- we expect.  We should perhaps stop being so implicit.
  st' <- asType $ resToParam st

  case maybe_outer_t of
    Ascribed outer_t -> do
      ctEq
        (ReasonAscription (locOf loc) (toStruct st') (toStruct outer_t))
        st'
        outer_t
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
  ctEq (Reason (locOf loc)) (toType t') t
  pure $ PatLit l (Info t') loc
checkPat' (PatLit l NoInfo loc) NoneInferred = do
  t' <- patLitMkType l loc
  pure $ PatLit l (Info t') loc
checkPat' (PatConstr n NoInfo ps loc) (Ascribed (Scalar (Sum cs)))
  | Just ts <- M.lookup n cs = do
      when (length ps /= length ts) $
        typeError loc mempty $
          "Pattern #"
            <> pretty n
            <> " expects"
              <+> pretty (length ps)
              <+> "constructor arguments, but type provides"
              <+> pretty (length ts)
              <+> "arguments."
      ps' <- zipWithM checkPat' ps $ map Ascribed ts
      cs' <- traverse (mapM asStructType) cs
      pure $ PatConstr n (Info (Scalar (Sum cs'))) ps' loc
checkPat' (PatConstr n NoInfo ps loc) (Ascribed t) = do
  ps' <- forM ps $ \p -> do
    p_t <- newType (srclocOf p) Lifted "t" Observe
    checkPat' p $ Ascribed p_t
  t' <- newTypeWithConstr loc "t" Observe n $ map (toType . patternType) ps'
  ctEq (Reason (locOf loc)) t' t
  t'' <- asStructType t'
  pure $ PatConstr n (Info $ toParam Observe t'') ps' loc
checkPat' (PatConstr n NoInfo ps loc) NoneInferred = do
  ps' <- mapM (`checkPat'` NoneInferred) ps
  t <- newTypeWithConstr loc "t" Observe n $ map (toType . patternType) ps'
  t' <- asStructType t
  pure $ PatConstr n (Info $ toParam Observe t') ps' loc

checkPat ::
  PatBase NoInfo VName (TypeBase Size u) ->
  Inferred Type ->
  (Pat ParamType -> TermM a) ->
  TermM a
checkPat p t m =
  m =<< checkPat' (fmap (toParam Observe) p) (fmap (fmap (const Observe)) t)

-- | Bind @let@-bound sizes.  This is usually followed by 'bindletPat'
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
  checkPat p (Ascribed t) $ \p' ->
    bind (patIdents (fmap toStruct p')) $
      m p'

typeParamIdent :: TypeParam -> Maybe (Ident StructType)
typeParamIdent (TypeParamDim v loc) =
  Just $ Ident v (Info $ Scalar $ Prim $ Signed Int64) loc
typeParamIdent _ = Nothing

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
        checkPat p NoneInferred $ \p' ->
          bind (patIdents $ fmap toStruct p') $ incLevel $ descend (p' : ps') ps
      descend ps' [] = m $ reverse ps'

  incLevel $ descend [] orig_ps

checkApplyOne ::
  SrcLoc ->
  (Maybe (QualName VName), Int) ->
  (Shape Size, Type) ->
  (Maybe Exp, Shape Size, Type) ->
  TermM (Type, AutoMap)
checkApplyOne loc fname (fframe, ftype) (arg, argframe, argtype) = do
  (a, b) <- split ftype
  r <- newSVar loc "R"
  m <- newSVar loc "M"
  let unit_info = Info $ Scalar $ Prim Bool
      r_var = Var (QualName [] r) unit_info mempty
      m_var = Var (QualName [] m) unit_info mempty
      lhs = arrayOf (toShape (SVar r)) argtype
      rhs = arrayOf (toShape (SVar m)) a
  ctAM (Reason (locOf loc)) r m $ fmap toSComp (toShape m_var <> fframe)
  let reason = case arg of
        Just arg' -> ReasonApply (locOf arg) fname arg' lhs rhs
        Nothing -> Reason (locOf loc)
  ctEq reason lhs rhs
  debugTraceM 3 $
    unlines
      [ "## checkApplyOne",
        "## fname",
        prettyString fname,
        "## (fframe, ftype)",
        prettyString (fframe, ftype),
        "## (argframe, argtype)",
        prettyString (argframe, argtype),
        "## r",
        prettyString r,
        "## m",
        prettyString m,
        "## lhs",
        prettyString lhs,
        "## rhs",
        prettyString rhs,
        "## ret",
        prettyString $ arrayOf (toShape (SVar m)) b
      ]
  pure
    ( arrayOf (toShape (SVar m)) b,
      AutoMap
        { autoRep = toShape r_var,
          autoMap = toShape m_var,
          autoFrame = toShape m_var <> fframe
        }
    )
  where
    toSComp (Var (QualName [] x) _ _) = SVar x
    toSComp _ = error ""
    toShape = Shape . pure
    split (Scalar (Arrow _ _ _ a (RetType _ b))) =
      pure (a, b `setUniqueness` NoUniqueness)
    split (Array _u s t) = do
      (a, b) <- split $ Scalar t
      pure (arrayOf s a, arrayOf s b)
    split ftype' = do
      a <- newType loc Lifted "arg" NoUniqueness
      b <- newType loc Lifted "res" Nonunique
      let reason = case (fname, arg) of
            ((Just fname', i), Just arg') -> ReasonApplySplit (locOf loc) (fname', i) arg'
            _ -> Reason (locOf loc)
      ctEq reason ftype' $ Scalar $ Arrow NoUniqueness Unnamed Observe a $ RetType [] b
      pure (a, b `setUniqueness` NoUniqueness)

checkApply ::
  SrcLoc ->
  Maybe (QualName VName) ->
  (Shape Size, Type) ->
  NE.NonEmpty (Maybe Exp, Shape Size, Type) ->
  TermM (Type, NE.NonEmpty AutoMap)
checkApply loc fname (fframe, ftype) args = do
  ((_, _, rt), argts) <- mapAccumLM onArg (0, fframe, ftype) args
  pure (rt, argts)
  where
    onArg (i, f_f, f_t) arg = do
      (rt, am) <- checkApplyOne loc (fname, i) (f_f, f_t) arg
      pure
        ( (i + 1, autoFrame am, rt),
          am
        )

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

-- Add constraints saying that the first type has a (potentially
-- nested) field containing the second type.
mustHaveFields :: SrcLoc -> Type -> [Name] -> Type -> TermM ()
mustHaveFields loc t [] ve_t =
  -- This case is probably never reached.
  ctEq (Reason (locOf loc)) t ve_t
mustHaveFields loc t [f] ve_t = do
  rt :: Type <- newTypeWithField loc "ft" f ve_t
  ctEq (Reason (locOf loc)) t rt
mustHaveFields loc t (f : fs) ve_t = do
  ft <- newType loc Lifted "ft" NoUniqueness
  rt <- newTypeWithField loc "rt" f ft
  mustHaveFields loc ft fs ve_t
  ctEq (Reason (locOf loc)) t rt

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
checkCases mt rest_cs =
  case NE.uncons rest_cs of
    (c, Nothing) -> do
      (c', t) <- checkCase mt c
      pure (NE.singleton c', t)
    (c, Just cs) -> do
      (c', c_t) <- checkCase mt c
      (cs', cs_t) <- checkCases mt cs
      ctEq (ReasonBranches (locOf c) c_t cs_t) c_t cs_t
      pure (NE.cons c' cs', c_t)

-- | An unmatched pattern. Used in in the generation of
-- unmatched pattern warnings by the type checker.
data Unmatched p
  = UnmatchedNum p [PatLit]
  | UnmatchedBool p
  | UnmatchedConstr p
  | Unmatched p
  deriving (Functor, Show)

instance Pretty (Unmatched (Pat StructType)) where
  pretty um = case um of
    (UnmatchedNum p nums) -> pretty' p <+> "where p is not one of" <+> pretty nums
    (UnmatchedBool p) -> pretty' p
    (UnmatchedConstr p) -> pretty' p
    (Unmatched p) -> pretty' p
    where
      pretty' (PatAscription p t _) = pretty p <> ":" <+> pretty t
      pretty' (PatParens p _) = parens $ pretty' p
      pretty' (PatAttr _ p _) = parens $ pretty' p
      pretty' (Id v _ _) = prettyName v
      pretty' (TuplePat pats _) = parens $ commasep $ map pretty' pats
      pretty' (RecordPat fs _) = braces $ commasep $ map ppField fs
        where
          ppField (name, t) = prettyName (unLoc name) <> equals <> pretty' t
      pretty' Wildcard {} = "_"
      pretty' (PatLit e _ _) = pretty e
      pretty' (PatConstr n _ ps _) = "#" <> pretty n <+> sep (map pretty' ps)

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
      pure (arg', (Just arg', frameOf arg', arg_t))
  fe_t <- expType fe'
  (rt, ams) <- checkApply loc fname (frameOf fe', fe_t) apply_args
  rt' <- asStructType rt
  let args'' =
        NE.zipWith (\am arg -> (Info (Nothing, am), arg)) ams args'
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

  (rt, ams) <-
    checkApply
      loc
      (Just op)
      (mempty, ftype)
      ((Just e1', frameOf e1', e1_t) NE.:| [(Just e2', frameOf e2', e2_t)])
  rt' <- asStructType rt
  let (am1 NE.:| [am2]) = ams

  ftype' <- asStructType ftype
  pure $
    AppExp
      (BinOp (op, oploc) (Info ftype') (e1', Info (Nothing, am1)) (e2', Info (Nothing, am2)) loc)
      (Info (AppRes rt' []))
--
checkExp (OpSectionLeft op _ e _ _ loc) = do
  optype <- lookupVar loc op
  e' <- checkExp e
  e_t <- expType e'
  t2 <- newType loc Lifted "t" NoUniqueness
  t2' <- asStructType t2
  let f1 = frameOf e'
  (rt, ams) <-
    checkApply
      loc
      (Just op)
      (mempty, optype)
      ((Just e', f1, e_t) NE.:| [(Nothing, mempty, t2)])
  rt' <- asStructType rt

  let (am1 NE.:| _) = ams
  t1 <- asStructType e_t
  optype' <- asStructType optype
  pure $
    OpSectionLeft
      op
      (Info optype')
      e'
      ( Info (Unnamed, toParam Observe t1, Nothing, am1),
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
  let f2 = frameOf e'
  (rt, ams) <-
    checkApply
      loc
      (Just op)
      (mempty, optype)
      ((Nothing, mempty, t1) NE.:| [(Just e', f2, e_t)])
  rt' <- asStructType rt
  let (_ NE.:| [am2]) = ams
  t2 <- asStructType e_t

  optype' <- asStructType optype
  pure $
    OpSectionRight
      op
      (Info optype')
      e'
      -- Dummy types.
      ( Info (Unnamed, toParam Observe t1'),
        Info (Unnamed, toParam Observe t2, Nothing, am2)
      )
      (Info $ RetType [] (rt' `setUniqueness` Nonunique))
      loc
--
checkExp (ProjectSection fields NoInfo loc) = do
  a <- newType loc Lifted "a" NoUniqueness
  b <- newType loc Lifted "b" NoUniqueness
  mustHaveFields loc a fields b
  ft <- asStructType $ Scalar $ Arrow mempty Unnamed Observe a $ RetType [] $ b `setUniqueness` Nonunique
  pure $ ProjectSection fields (Info ft) loc
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
          { scopeVtable = M.insert name entry $ scopeVtable scope
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
checkExp (RecordUpdate src fields ve NoInfo loc) = do
  src' <- checkExp src
  src_t <- expType src'
  ve' <- checkExp ve
  ve_t <- expType ve'
  mustHaveFields loc src_t fields ve_t
  src_t' <- asStructType src_t
  pure $ RecordUpdate src' fields ve' (Info src_t') loc
--
checkExp (IndexSection slice NoInfo loc) = do
  slice' <- checkSlice slice
  index_arg_t <- newElemType loc "index" NoUniqueness
  index_elem_t <- newElemType loc "index_elem" NoUniqueness
  index_res_t <- newElemType loc "index_res" NoUniqueness
  let num_slices = length $ filter isSlice slice
  ctEq (Reason (locOf loc)) index_arg_t $ arrayOfRank num_slices index_elem_t
  ctEq (Reason (locOf loc)) index_res_t $ arrayOfRank (length slice) index_elem_t
  ft <- asStructType $ Scalar $ Arrow mempty Unnamed Observe index_arg_t $ second (const Nonunique) $ RetType [] index_res_t
  pure $ IndexSection slice' (Info ft) loc
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
checkExp (Update src slice ve loc) = do
  src' <- checkExp src
  src_t <- expType src'
  slice' <- checkSlice slice
  ve' <- checkExp ve
  ve_t <- expType ve'
  let num_slices = length $ filter isSlice slice
  update_elem_t <- newElemType loc "update_elem" NoUniqueness
  ctEq (Reason (locOf src')) src_t $ arrayOfRank (length slice) update_elem_t
  ctEq (Reason (locOf ve')) ve_t $ arrayOfRank num_slices update_elem_t
  pure $ Update src' slice' ve' loc
--
checkExp (AppExp (LetWith dest src slice ve body loc) _) = do
  src_t <- lookupVar (srclocOf src) $ qualName $ identName src
  src_t' <- asStructType src_t
  let src' = src {identType = Info src_t'}
      dest' = dest {identType = Info src_t'}
  slice' <- checkSlice slice
  ve' <- checkExp ve
  ve_t <- expType ve'
  let num_slices = length $ filter isSlice slice
  update_elem_t <- newElemType loc "update_elem" NoUniqueness
  ctEq (Reason (locOf loc)) src_t $ arrayOfRank (length slice) update_elem_t
  ctEq (Reason (locOf ve')) ve_t $ arrayOfRank num_slices update_elem_t
  bind [dest'] $ do
    body' <- checkExp body
    body_t <- expType body'
    body_t' <- asStructType body_t
    pure $ AppExp (LetWith dest' src' slice' ve' body' loc) (Info $ AppRes body_t' [])
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
  solution' <- doDefaults (S.toList $ typeVars t) solution
  pure
    ( generalised,
      -- See #1552 for why we resolve unconstrained and
      -- un-generalised type variables to ().
      M.fromList (map (,Scalar (Record mempty)) unconstrained') <> solution'
    )

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
      body' <- checkExp body
      (_, retdecl') <- checkRetDecl body' retdecl
      pure (params', body', retdecl')

  cts <- gets termConstraints
  tyvars <- gets termTyVars
  typarams <- gets termTyParams
  artificial <- gets termArtificial

  debugTraceM 3 $ "\n# function " <> prettyNameString fname <> "\n# " <> locStr loc <> "\n"

  debugTraceM 3 $
    unlines
      [ "## cts:",
        unlines $ map prettyString cts,
        "## body:",
        prettyString body',
        "## tyvars:",
        unlines $ map (prettyString . first prettyNameString) $ M.toList tyvars,
        "## artificial:",
        unlines $ map (\(v, t) -> prettyNameString v <> " => " <> prettyString t) (M.toList artificial)
      ]

  onRankSolution typarams
    =<< rankAnalysis1 loc cts tyvars artificial params' body' retdecl'
  where
    onRankSolution typarams ((cts', artificial, tyvars'), params', body'', retdecl') = do
      solution <-
        bitraverse
          pure
          (fmap (second (onArtificial artificial)) . onTySolution params' body'')
          $ solve (reverse cts') typarams tyvars'
      debugTraceM 3 $
        unlines
          [ "## constraints:",
            unlines $ map prettyString cts',
            "## typarams:",
            let f (lvl, l, _) = (lvl, l)
             in unlines (map (prettyString . bimap prettyNameString f) (M.toList typarams)),
            "## tyvars':",
            unlines $ map (prettyString . first prettyNameString) $ M.toList tyvars',
            "## solution:",
            let p (v, t) = prettyNameString v <> " => " <> prettyString t
             in either (docString . prettyTypeError) (unlines . map p . M.toList . snd) solution,
            either (const mempty) (unlines . ("## generalised:" :) . map prettyString . fst) solution
          ]
      pure (solution, params', retdecl', body'')

    onTySolution params' body' (unconstrained, solution) = do
      body_t <- expType body'
      let fun_t =
            foldFunType
              (map (first (const ()) . patternType) params')
              (RetType [] $ bimap (const ()) (const Nonunique) body_t)
      generaliseAndDefaults unconstrained solution fun_t

    onArtificial artificial solution =
      M.map (substTyVars (`M.lookup` solution) . first (const ())) artificial <> solution

checkSingleExp ::
  ExpBase NoInfo VName ->
  TypeM (Either TypeError ([TypeParam], M.Map TyVar (TypeBase () NoUniqueness)), Exp)
checkSingleExp e = runTermM $ do
  e' <- checkExp e
  cts <- gets termConstraints
  tyvars <- gets termTyVars
  typarams <- gets termTyParams
  artificial <- gets termArtificial
  ((cts', _artificial', tyvars'), _, e'', _) <-
    rankAnalysis1 (srclocOf e') cts tyvars artificial [] e' Nothing
  case solve cts' typarams tyvars' of
    Left err -> pure (Left err, e'')
    Right (unconstrained, solution) -> do
      e_t <- expType e''
      x <- generaliseAndDefaults unconstrained solution $ first (const ()) e_t
      pure (Right x, e'')

-- | Type-check a single size expression in isolation.  This expression may
-- turn out to be polymorphic, in which case it is unified with i64.
checkSizeExp ::
  ExpBase NoInfo VName ->
  TypeM (Either TypeError ([UnconTyVar], M.Map TyVar (TypeBase () NoUniqueness)), Exp)
checkSizeExp e = runTermM $ do
  e' <- checkSizeExp' e
  cts <- gets termConstraints
  tyvars <- gets termTyVars
  typarams <- gets termTyParams
  artificial <- gets termArtificial

  (cts_tyvars', _, es', _) <-
    L.unzip4 <$> rankAnalysis (srclocOf e) cts tyvars artificial [] e' Nothing

  solutions <-
    forM cts_tyvars' $ \(cts', _artificial', tyvars') ->
      bitraverse pure (traverse (doDefaults mempty)) $ solve cts' typarams tyvars'

  case (solutions, es') of
    ([solution], [e'']) ->
      pure (solution, e'')
    _ -> pure (Left $ TypeError (locOf e) mempty "Ambiguous size expression", e')
