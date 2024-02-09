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
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Char (isAscii)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe
import Data.Text qualified as T
import Debug.Trace
import Futhark.FreshNames qualified as FreshNames
import Futhark.MonadFreshNames hiding (newName)
import Futhark.Util (mapAccumLM)
import Futhark.Util.Pretty
import Language.Futhark
import Language.Futhark.TypeChecker.Constraints
import Language.Futhark.TypeChecker.Monad hiding (BoundV, lookupMod)
import Language.Futhark.TypeChecker.Monad qualified as TypeM
import Language.Futhark.TypeChecker.Types
import Language.Futhark.TypeChecker.Unify (Level)
import Prelude hiding (mod)

data Inferred t
  = NoneInferred
  | Ascribed t

instance Functor Inferred where
  fmap _ NoneInferred = NoneInferred
  fmap f (Ascribed t) = Ascribed (f t)

data ValBinding
  = BoundV [TypeParam] StructType
  | OverloadedF [PrimType] [Maybe PrimType] (Maybe PrimType)
  | EqualityF
  deriving (Show)

expType :: Exp -> Type
expType = toType . typeOf

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
    termCounter :: !Int,
    termWarnings :: Warnings,
    termNameSource :: VNameSource
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
    valBinding (TypeM.BoundV tps v) = BoundV tps v

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
          BoundV tvs $ foldFunType pts rt
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
            termWarnings = mempty,
            termNameSource = src,
            termCounter = 0
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

incCounter :: TermM Int
incCounter = do
  s <- get
  put s {termCounter = termCounter s + 1}
  pure $ termCounter s

tyVarType :: (Monoid u) => TyVar -> TypeBase dim u
tyVarType v = Scalar $ TypeVar mempty (qualName v) []

newTyVarWith :: a -> Name -> TyVarInfo -> TermM TyVar
newTyVarWith loc desc info = do
  i <- incCounter
  v <- newID $ mkTypeVarName desc i
  modify $ \s -> s {termTyVars = M.insert v info $ termTyVars s}
  pure v

newTyVar :: a -> Name -> TermM TyVar
newTyVar loc desc = newTyVarWith loc desc TyVarFree

newType :: (Located loc, Monoid u) => loc -> Name -> TermM (TypeBase dim u)
newType loc desc = tyVarType <$> newTyVar loc desc

newTypeWithField :: (Monoid u) => SrcLoc -> Name -> Name -> TypeBase dim u -> TermM (TypeBase dim u)
newTypeWithField loc desc k t =
  tyVarType <$> newTyVarWith loc desc (TyVarRecord $ M.singleton k $ toType t)

newTypeWithConstr :: (Monoid u) => SrcLoc -> Name -> Name -> [Type] -> TermM (TypeBase dim u)
newTypeWithConstr loc desc k ts =
  tyVarType <$> newTyVarWith loc desc (TyVarSum $ M.singleton k ts)

newTypeOverloaded :: (Monoid u) => SrcLoc -> Name -> [PrimType] -> TermM (TypeBase dim u)
newTypeOverloaded loc name pts =
  tyVarType <$> newTyVarWith loc name (TyVarPrim pts)

asStructType :: (Monoid u) => SrcLoc -> TypeBase d u -> TermM (TypeBase Size u)
asStructType _ (Scalar (Prim pt)) = pure $ Scalar $ Prim pt
asStructType _ (Scalar (TypeVar u v [])) = pure $ Scalar $ TypeVar u v []
asStructType loc (Scalar (Arrow u pname d t1 (RetType ext t2))) = do
  t1' <- asStructType loc t1
  t2' <- asStructType loc t2
  pure $ Scalar $ Arrow u pname d t1' $ RetType ext t2'
asStructType loc t = do
  t' <- newType loc "artificial"
  ctEq t' t
  pure t'

addCt :: Ct -> TermM ()
addCt ct = modify $ \s -> s {termConstraints = ct : termConstraints s}

ctEq :: TypeBase d1 u1 -> TypeBase d2 u2 -> TermM ()
ctEq t1 t2 =
  -- As a minor optimisation, do not add constraint if the types are
  -- equal.
  unless (t1' == t2') $ addCt $ CtEq t1' t2'
  where
    t1' = toType t1
    t2' = toType t2

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
  checkExpForSize = require "use as size" [Signed Int64] <=< checkExp

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

  bindVal v (TypeM.BoundV tps t) = localScope $ \scope ->
    scope {scopeVtable = M.insert v (BoundV tps t) $ scopeVtable scope}

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
arrayOfRank 0 t = t
arrayOfRank n t = arrayOf (Shape $ replicate n ()) t

require :: T.Text -> [PrimType] -> Exp -> TermM Exp
require why pts e = do
  t :: Type <- newTypeOverloaded (srclocOf e) "t" pts
  ctEq t $ expType e
  pure e

-- | Create a new type name and insert it (unconstrained) in the set
-- of type variables.
instTypeParam ::
  (Monoid as) =>
  QualName VName ->
  SrcLoc ->
  TypeParam ->
  TermM (VName, Subst (RetTypeBase dim as))
instTypeParam qn loc tparam = do
  i <- incCounter
  let name = nameFromString (takeWhile isAscii (baseString (typeParamName tparam)))
  v <- newID $ mkTypeVarName name i
  case tparam of
    TypeParamType {} -> do
      modify $ \s -> s {termTyVars = M.insert v TyVarFree $ termTyVars s}
      pure (v, Subst [] $ RetType [] $ Scalar $ TypeVar mempty (qualName v) [])
    TypeParamDim {} ->
      pure (v, ExpSubst $ sizeFromName (qualName v) loc)

-- | Instantiate a type scheme with fresh type variables for its type
-- parameters. Returns the names of the fresh type variables, the
-- instance list, and the instantiated type.
instTypeScheme ::
  QualName VName ->
  SrcLoc ->
  [TypeParam] ->
  StructType ->
  TermM ([VName], StructType)
instTypeScheme qn loc tparams t = do
  (names, substs) <- fmap (unzip . catMaybes) $ forM tparams $ \tparam -> do
    case tparam of
      TypeParamType x _ _ -> do
        let name = nameFromString (takeWhile isAscii (baseString (typeParamName tparam)))
        v <- newTyVar loc name
        pure $ Just (v, (typeParamName tparam, Subst [] $ RetType [] $ tyVarType v))
      TypeParamDim {} ->
        pure Nothing
  let t' = applySubst (`lookup` substs) t
  pure (names, t')

lookupMod :: QualName VName -> TermM Mod
lookupMod qn@(QualName _ name) = do
  scope <- lookupQualNameEnv qn
  case M.lookup name $ scopeModTable scope of
    Nothing -> error $ "lookupMod: " <> show qn
    Just m -> pure m

lookupVar :: SrcLoc -> QualName VName -> TermM StructType
lookupVar loc qn@(QualName qs name) = do
  scope <- lookupQualNameEnv qn
  case M.lookup name $ scopeVtable scope of
    Nothing ->
      error $ "lookupVar: " <> show qn
    Just (BoundV tparams t) -> do
      if null tparams && null qs
        then pure t
        else do
          (tnames, t') <- instTypeScheme qn loc tparams t
          outer_env <- asks termOuterEnv
          pure $ qualifyTypeVars outer_env tnames qs t'
    Just EqualityF -> do
      argtype <- newType loc "t"
      pure $ foldFunType [argtype, argtype] $ RetType [] $ Scalar $ Prim Bool
    Just (OverloadedF ts pts rt) -> do
      argtype <- newTypeOverloaded loc "t" ts
      let (pts', rt') = instOverloaded (argtype :: StructType) pts rt
      pure $ foldFunType (map (toParam Observe) pts') $ RetType [] $ toRes Nonunique rt'
  where
    instOverloaded argtype pts rt =
      ( map (maybe argtype (Scalar . Prim)) pts,
        maybe argtype (Scalar . Prim) rt
      )

bind ::
  [Ident StructType] ->
  TermM a ->
  TermM a
bind idents = localScope (`bindVars` idents)
  where
    bindVars = foldl bindVar

    bindVar scope (Ident name (Info tp) _) =
      scope
        { scopeVtable = M.insert name (BoundV [] tp) $ scopeVtable scope
        }

-- All this complexity is just so we can handle un-suffixed numeric
-- literals in patterns.
patLitMkType :: PatLit -> SrcLoc -> TermM ParamType
patLitMkType (PatLitInt _) loc =
  newTypeOverloaded loc "t" anyNumberType
patLitMkType (PatLitFloat _) loc =
  newTypeOverloaded loc "t" anyFloatType
patLitMkType (PatLitPrim v) _ =
  pure $ Scalar $ Prim $ primValueType v

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
  t <- newType loc "t"
  pure $ Id name (Info t) loc
checkPat' (Wildcard _ loc) (Ascribed t) =
  pure $ Wildcard (Info t) loc
checkPat' (Wildcard NoInfo loc) NoneInferred = do
  t <- newType loc "t"
  pure $ Wildcard (Info t) loc
checkPat' p@(TuplePat ps loc) (Ascribed t)
  | Just ts <- isTupleRecord t,
    length ts == length ps =
      TuplePat
        <$> zipWithM checkPat' ps (map Ascribed ts)
        <*> pure loc
  | otherwise = do
      ps_t :: [ParamType] <- replicateM (length ps) (newType loc "t")
      ctEq (Scalar (tupleRecord ps_t)) t
      checkPat' p $ Ascribed $ toParam Observe $ Scalar $ tupleRecord ps_t
checkPat' (TuplePat ps loc) NoneInferred =
  TuplePat <$> mapM (`checkPat'` NoneInferred) ps <*> pure loc
checkPat' p@(RecordPat p_fs loc) (Ascribed t)
  | Scalar (Record t_fs) <- t,
    L.sort (map fst p_fs) == L.sort (M.keys t_fs) =
      RecordPat . M.toList <$> check t_fs <*> pure loc
  | otherwise = do
      p_fs' <- traverse (const $ newType loc "t") $ M.fromList p_fs
      ctEq (Scalar (Record p_fs') :: ParamType) t
      checkPat' p $ Ascribed $ toParam Observe $ Scalar (Record p_fs')
  where
    check t_fs =
      traverse (uncurry checkPat') $
        M.intersectionWith (,) (M.fromList p_fs) (fmap Ascribed t_fs)
checkPat' (RecordPat fs loc) NoneInferred =
  RecordPat . M.toList
    <$> traverse (`checkPat'` NoneInferred) (M.fromList fs)
    <*> pure loc
checkPat' (PatAscription p t loc) maybe_outer_t = do
  (t', _, RetType dims st, _) <- checkTypeExp t

  case maybe_outer_t of
    Ascribed outer_t -> do
      ctEq st outer_t
      PatAscription
        <$> checkPat' p (Ascribed (resToParam st))
        <*> pure t'
        <*> pure loc
    NoneInferred ->
      PatAscription
        <$> checkPat' p (Ascribed (resToParam st))
        <*> pure t'
        <*> pure loc
checkPat' (PatLit l NoInfo loc) (Ascribed t) = do
  t' <- patLitMkType l loc
  addCt $ CtEq (toType t') (toType t)
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
      pure $ PatConstr n (Info (Scalar (Sum cs))) ps' loc
checkPat' (PatConstr n NoInfo ps loc) (Ascribed t) = do
  ps' <- forM ps $ \p -> do
    p_t <- newType (srclocOf p) "t"
    checkPat' p $ Ascribed p_t
  t' <- newTypeWithConstr loc "t" n $ map (toType . patternType) ps'
  ctEq t' t
  pure $ PatConstr n (Info t') ps' loc
checkPat' (PatConstr n NoInfo ps loc) NoneInferred = do
  ps' <- mapM (`checkPat'` NoneInferred) ps
  t <- newTypeWithConstr loc "t" n $ map (toType . patternType) ps'
  pure $ PatConstr n (Info t) ps' loc

checkPat ::
  PatBase NoInfo VName (TypeBase Size u) ->
  Inferred StructType ->
  (Pat ParamType -> TermM a) ->
  TermM a
checkPat p t m =
  m =<< checkPat' (fmap (toParam Observe) p) (fmap (toParam Observe) t)

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
  StructType ->
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
bindTypeParams tparams =
  bind (mapMaybe typeParamIdent tparams)
    . bindTypes (mapMaybe typeParamType tparams)
  where
    typeParamType (TypeParamType l v _) =
      Just (v, TypeAbbr l [] $ RetType [] $ Scalar (TypeVar mempty (qualName v) []))
    typeParamType TypeParamDim {} =
      Nothing

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

checkApply :: SrcLoc -> (Maybe (QualName VName), Int) -> Type -> Exp -> TermM Type
checkApply loc (fname, _) (Scalar (Arrow _ _ _ a (RetType _ b))) arg = do
  ctEq a $ expType arg
  pure $ toType b
checkApply loc (fname, _) ftype arg = do
  a <- newType loc "arg"
  b <- newTyVar loc "res"
  ctEq ftype $ Scalar $ Arrow NoUniqueness Unnamed Observe a $ RetType [] (tyVarType b)
  ctEq a (expType arg)
  pure $ tyVarType b

checkSlice :: SliceBase NoInfo VName -> TermM [DimIndex]
checkSlice = mapM checkDimIndex
  where
    checkDimIndex (DimFix i) =
      DimFix <$> check i
    checkDimIndex (DimSlice i j s) =
      DimSlice <$> traverse check i <*> traverse check j <*> traverse check s

    check = require "use as index" anySignedType <=< checkExp

isSlice :: DimIndexBase f vn -> Bool
isSlice DimSlice {} = True
isSlice DimFix {} = False

-- Add constraints saying that the first type has a (potentially
-- nested) field containing the second type.
mustHaveFields :: SrcLoc -> Type -> [Name] -> Type -> TermM ()
mustHaveFields _ t [] ve_t =
  -- This case is probably never reached.
  ctEq t ve_t
mustHaveFields loc t [f] ve_t = do
  rt :: Type <- newTypeWithField loc "ft" f ve_t
  ctEq t rt
mustHaveFields loc t (f : fs) ve_t = do
  ft :: Type <- newType loc "ft"
  rt <- newTypeWithField loc "rt" f ft
  mustHaveFields loc ft fs ve_t
  ctEq t rt

checkCase ::
  StructType ->
  CaseBase NoInfo VName ->
  TermM (CaseBase Info VName, StructType)
checkCase mt (CasePat p e loc) =
  bindLetPat p mt $ \p' -> do
    e' <- checkExp e
    pure (CasePat (fmap toStruct p') e' loc, typeOf e')

checkCases ::
  StructType ->
  NE.NonEmpty (CaseBase NoInfo VName) ->
  TermM (NE.NonEmpty (CaseBase Info VName), StructType)
checkCases mt rest_cs =
  case NE.uncons rest_cs of
    (c, Nothing) -> do
      (c', t) <- checkCase mt c
      pure (NE.singleton c', t)
    (c, Just cs) -> do
      (c', c_t) <- checkCase mt c
      (cs', cs_t) <- checkCases mt cs
      ctEq c_t cs_t
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
          ppField (name, t) = pretty (nameToString name) <> equals <> pretty' t
      pretty' Wildcard {} = "_"
      pretty' (PatLit e _ _) = pretty e
      pretty' (PatConstr n _ ps _) = "#" <> pretty n <+> sep (map pretty' ps)

checkRetDecl :: Exp -> Maybe (TypeExp NoInfo VName) -> TermM (Maybe (TypeExp Info VName))
checkRetDecl _ Nothing = pure Nothing
checkRetDecl body (Just te) = do
  (te', _, RetType _ st, _) <- checkTypeExp te
  ctEq (typeOf body) st
  pure $ Just te'

checkExp :: ExpBase NoInfo VName -> TermM (ExpBase Info VName)
--
checkExp (Var qn _ loc) = do
  t <- lookupVar loc qn
  pure $ Var qn (Info t) loc
checkExp (OpSection op _ loc) = do
  ftype <- lookupVar loc op
  pure $ OpSection op (Info ftype) loc
checkExp (Negate arg loc) = do
  arg' <- require "numeric negation" anyNumberType =<< checkExp arg
  pure $ Negate arg' loc
checkExp (Not arg loc) = do
  arg' <- require "logical negation" (Bool : anyIntType) =<< checkExp arg
  pure $ Not arg' loc
checkExp (Hole NoInfo loc) =
  Hole <$> (Info <$> newType loc "hole") <*> pure loc
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
checkExp (ArrayLit es _ loc) = do
  -- TODO: this will produce an enormous number of constraints and
  -- type variables for pathologically large arrays with
  -- type-unsuffixed integers. Add some special case that handles that
  -- more efficiently.
  et <- newType loc "et"
  es' <- forM es $ \e -> do
    e' <- checkExp e
    ctEq (typeOf e') et
    pure e'
  let arr_t = arrayOf (Shape [sizeFromInteger (L.genericLength es) loc]) et
  pure $ ArrayLit es' (Info arr_t) loc
checkExp (RecordLit fs loc) =
  RecordLit <$> evalStateT (mapM checkField fs) mempty <*> pure loc
  where
    checkField (RecordFieldExplicit f e rloc) = do
      errIfAlreadySet f rloc
      modify $ M.insert f rloc
      RecordFieldExplicit f <$> lift (checkExp e) <*> pure rloc
    checkField (RecordFieldImplicit name NoInfo rloc) = do
      errIfAlreadySet (baseName name) rloc
      t <- lift $ lookupVar rloc $ qualName name
      modify $ M.insert (baseName name) rloc
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
  t <- newTypeWithConstr loc "t" name $ map expType es'
  pure $ Constr name es' (Info t) loc
--
checkExp (AppExp (Apply fe args loc) NoInfo) = do
  fe' <- checkExp fe
  ((_, rt), args') <- mapAccumLM onArg (0, expType fe') args
  rt' <- asStructType loc rt
  pure $ AppExp (Apply fe' args' loc) $ Info $ AppRes rt' []
  where
    fname =
      case fe of
        Var v _ _ -> Just v
        _ -> Nothing

    onArg (i, f_t) (_, arg) = do
      arg' <- checkExp arg
      rt <- checkApply loc (fname, i) (toType f_t) arg'
      pure
        ( (i + 1, rt),
          (Info Nothing, arg')
        )
--
checkExp (AppExp (BinOp (op, oploc) NoInfo (e1, _) (e2, _) loc) NoInfo) = do
  ftype <- lookupVar oploc op
  e1' <- checkExp e1
  e2' <- checkExp e2

  rt1 <- checkApply loc (Just op, 0) (toType ftype) e1'
  rt2 <- checkApply loc (Just op, 1) rt1 e2'
  rt2' <- asStructType loc rt2

  pure $
    AppExp
      (BinOp (op, oploc) (Info ftype) (e1', Info Nothing) (e2', Info Nothing) loc)
      (Info (AppRes rt2' []))
--
checkExp (OpSectionLeft op _ e _ _ loc) = do
  optype <- lookupVar loc op
  e' <- checkExp e
  void $ checkApply loc (Just op, 0) (toType optype) e'
  let t1 = typeOf e'
  t2 <- newType loc "t2"
  rt <- newType loc "rt"
  ctEq optype $ foldFunType [toParam Observe t1, t2] $ RetType [] rt
  pure $
    OpSectionLeft
      op
      (Info optype)
      e'
      ( Info (Unnamed, toParam Observe t1, Nothing),
        Info (Unnamed, t2)
      )
      (Info (RetType [] rt), Info [])
      loc
--
checkExp (OpSectionRight op _ e _ NoInfo loc) = do
  optype <- lookupVar loc op
  e' <- checkExp e
  t1 <- newType loc "t"
  let t2 = typeOf e'
  rt <- newType loc "rt"
  ctEq optype $ foldFunType [t1, toParam Observe t2] $ RetType [] rt
  pure $
    OpSectionRight
      op
      (Info optype)
      e'
      -- Dummy types.
      ( Info (Unnamed, toParam Observe t1),
        Info (Unnamed, toParam Observe t2, Nothing)
      )
      (Info $ RetType [] rt)
      loc
--
checkExp (ProjectSection fields NoInfo loc) = do
  a <- newType loc "a"
  b <- newType loc "b"
  mustHaveFields loc (toType a) fields (toType b)
  let ft = Scalar $ Arrow mempty Unnamed Observe a $ RetType [] b
  pure $ ProjectSection fields (Info ft) loc
--
checkExp (Lambda params body retdecl NoInfo loc) = do
  bindParams [] params $ \params' -> do
    body' <- checkExp body
    retdecl' <- checkRetDecl body' retdecl
    let ret = RetType [] $ toRes Nonunique $ typeOf body'
    pure $ Lambda params' body' retdecl' (Info ret) loc
--
checkExp (AppExp (LetPat sizes pat e body loc) _) = do
  e' <- checkExp e

  bindSizes sizes . incLevel . bindLetPat pat (typeOf e') $ \pat' -> do
    body' <- incLevel $ checkExp body
    pure $
      AppExp
        (LetPat sizes (fmap toStruct pat') e' body' loc)
        (Info $ AppRes (typeOf body') [])
--
checkExp (AppExp (LetFun name (tparams, params, retdecl, NoInfo, e) body loc) _) = do
  (tparams', params', retdecl', rettype, e') <-
    bindParams tparams params $ \params' -> do
      e' <- checkExp e
      let ret = RetType [] $ toRes Nonunique $ typeOf e'
      retdecl' <- checkRetDecl e' retdecl
      pure (tparams, params', retdecl', ret, e')

  let entry = BoundV tparams' $ funType params' rettype
      bindF scope =
        scope
          { scopeVtable = M.insert name entry $ scopeVtable scope
          }
  body' <- localScope bindF $ checkExp body

  pure $
    AppExp
      ( LetFun
          name
          (tparams', params', retdecl', Info rettype, e')
          body'
          loc
      )
      (Info $ AppRes (typeOf body') [])
--
checkExp (AppExp (Range start maybe_step end loc) _) = do
  start' <- checkExp' start
  maybe_step' <- traverse checkExp' maybe_step
  end' <- traverse checkExp' end
  range_t <- newType loc "range"
  ctEq range_t $ arrayOfRank 1 (toType (typeOf start'))
  pure $ AppExp (Range start' maybe_step' end' loc) $ Info $ AppRes range_t []
  where
    checkExp' = require "use in range expression" anyIntType <=< checkExp
--
checkExp (Project k e NoInfo loc) = do
  e' <- checkExp e
  kt <- newType loc "kt"
  t <- newTypeWithField loc "t" k kt
  ctEq (typeOf e') t
  pure $ Project k e' (Info kt) loc
--
checkExp (RecordUpdate src fields ve NoInfo loc) = do
  src' <- checkExp src
  ve' <- checkExp ve
  mustHaveFields loc (expType src') fields (expType ve')
  pure $ RecordUpdate src' fields ve' (Info (typeOf src')) loc
--
checkExp (IndexSection slice NoInfo loc) = do
  slice' <- checkSlice slice
  index_arg_t <- newType loc "index"
  index_elem_t <- newType loc "index_elem"
  index_res_t <- newType loc "index_res"
  let num_slices = length $ filter isSlice slice
  ctEq index_arg_t $ arrayOfRank num_slices index_elem_t
  ctEq index_res_t $ arrayOfRank (length slice) index_elem_t
  let ft = Scalar $ Arrow mempty Unnamed Observe index_arg_t $ RetType [] index_res_t
  pure $ IndexSection slice' (Info ft) loc
--
checkExp (AppExp (Index e slice loc) _) = do
  e' <- checkExp e
  slice' <- checkSlice slice
  index_t <- newType loc "index"
  index_elem_t <- newType loc "index_elem"
  let num_slices = length $ filter isSlice slice
  ctEq index_t $ arrayOfRank num_slices index_elem_t
  ctEq (typeOf e') $ arrayOfRank (length slice) index_elem_t
  pure $ AppExp (Index e' slice' loc) (Info $ AppRes index_t [])
--
checkExp (Update src slice ve loc) = do
  src' <- checkExp src
  slice' <- checkSlice slice
  ve' <- checkExp ve
  let num_slices = length $ filter isSlice slice
  update_elem_t <- newType loc "update_elem"
  ctEq (typeOf src') $ arrayOfRank (length slice) update_elem_t
  ctEq (typeOf ve') $ arrayOfRank num_slices update_elem_t
  pure $ Update src' slice' ve' loc
--
checkExp (AppExp (LetWith dest src slice ve body loc) _) = do
  src_t <- lookupVar (srclocOf src) $ qualName $ identName src
  let src' = src {identType = Info src_t}
      dest' = dest {identType = Info src_t}
  slice' <- checkSlice slice
  ve' <- checkExp ve
  let num_slices = length $ filter isSlice slice
  update_elem_t <- newType loc "update_elem"
  ctEq src_t $ arrayOfRank (length slice) update_elem_t
  ctEq (typeOf ve') $ arrayOfRank num_slices update_elem_t
  bind [dest'] $ do
    body' <- checkExp body
    pure $ AppExp (LetWith dest' src' slice' ve' body' loc) (Info $ AppRes (typeOf body') [])
--
checkExp (AppExp (If e1 e2 e3 loc) _) = do
  e1' <- checkExp e1
  e2' <- checkExp e2
  e3' <- checkExp e3

  ctEq (typeOf e1') (Scalar (Prim Bool) :: Type)
  ctEq (typeOf e2') (typeOf e3')

  pure $ AppExp (If e1' e2' e3' loc) (Info $ AppRes (typeOf e2') [])
--
checkExp (AppExp (Match e cs loc) _) = do
  e' <- checkExp e
  (cs', t) <- checkCases (typeOf e') cs
  pure $ AppExp (Match e' cs' loc) (Info $ AppRes t [])
--
checkExp (AppExp (Loop _ pat arg form body loc) _) = do
  arg' <- checkExp arg
  bindLetPat pat (typeOf arg') $ \pat' -> do
    (form', body') <-
      case form of
        For (Ident i _ iloc) bound -> do
          bound' <- require "loop bound" anyIntType =<< checkExp bound
          let i' = Ident i (Info (typeOf bound')) iloc
          bind [i'] $ do
            body' <- checkExp body
            ctEq (typeOf arg') (typeOf body')
            pure (For i' bound', body')
        While cond -> do
          cond' <- checkExp cond
          body' <- checkExp body
          ctEq (typeOf arg') (typeOf body')
          pure (While cond', body')
        ForIn elemp arr -> do
          arr' <- checkExp arr
          elem_t <- newType elemp "elem"
          ctEq (typeOf arr') $ arrayOfRank 1 (toType elem_t)
          bindLetPat elemp elem_t $ \elemp' -> do
            body' <- checkExp body
            pure (ForIn (toStruct <$> elemp') arr', body')
    pure $
      AppExp
        (Loop [] pat' arg' form' body' loc)
        (Info (AppRes (patternStructType pat') []))
--
checkExp (Ascript e te loc) = do
  e' <- checkExp e
  (te', _, RetType _ st, _) <- checkTypeExp te
  ctEq (typeOf e') st
  pure $ Ascript e' te' loc
checkExp (Coerce e te NoInfo loc) = do
  e' <- checkExp e
  (te', _, RetType _ st, _) <- checkTypeExp te
  ctEq (typeOf e') st
  pure $ Coerce e' te' (Info (toStruct st)) loc

checkValDef ::
  ( VName,
    Maybe (TypeExp NoInfo VName),
    [TypeParam],
    [PatBase NoInfo VName ParamType],
    ExpBase NoInfo VName,
    SrcLoc
  ) ->
  TypeM
    ( [TypeParam],
      [Pat ParamType],
      Maybe (TypeExp Info VName),
      Exp
    )
checkValDef (fname, retdecl, tparams, params, body, loc) = runTermM $ do
  bindParams tparams params $ \params' -> do
    body' <- checkExp body

    retdecl' <- checkRetDecl body' retdecl

    cts <- gets termConstraints
    tyvars <- gets termTyVars

    let solution = solve cts tyvars

    traceM $
      unlines
        [ "# function " <> prettyNameString fname,
          "## constraints:",
          unlines $ map prettyString cts,
          "## tyvars:",
          unlines $ map (prettyString . first prettyNameString) $ M.toList tyvars,
          "## solution:",
          either T.unpack (unlines . map (prettyString . first prettyNameString) . M.toList) $ solve cts tyvars
        ]
    pure (undefined, params', retdecl', body')
