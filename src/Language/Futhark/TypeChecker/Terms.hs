{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
-- | Facilities for type-checking Futhark terms.  Checking a term
-- requires a little more context to track uniqueness and such.
module Language.Futhark.TypeChecker.Terms
  ( checkOneExp
  , checkFunDef
  )
where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.RWS
import qualified Control.Monad.Fail as Fail
import Data.Bifunctor
import Data.List
import Data.Loc
import Data.Maybe
import qualified Data.Semigroup as Sem
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Prelude hiding (mod)

import Language.Futhark
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Monad hiding (BoundV, checkQualNameWithEnv)
import Language.Futhark.TypeChecker.Types
import qualified Language.Futhark.TypeChecker.Monad as TypeM

--- Uniqueness

data Usage = Consumed SrcLoc
           | Observed SrcLoc
           deriving (Eq, Ord, Show)

data Occurence = Occurence { observed :: Names
                           , consumed :: Names
                           , location :: SrcLoc
                           }
             deriving (Eq, Show)

instance Located Occurence where
  locOf = locOf . location

observation :: Names -> SrcLoc -> Occurence
observation = flip Occurence S.empty

consumption :: Names -> SrcLoc -> Occurence
consumption = Occurence S.empty

nullOccurence :: Occurence -> Bool
nullOccurence occ = S.null (observed occ) && S.null (consumed occ)

type Occurences = [Occurence]

type UsageMap = M.Map VName [Usage]

usageMap :: Occurences -> UsageMap
usageMap = foldl comb M.empty
  where comb m (Occurence obs cons loc) =
          let m' = S.foldl' (ins $ Observed loc) m obs
          in S.foldl' (ins $ Consumed loc) m' cons
        ins v m k = M.insertWith (++) k [v] m

combineOccurences :: VName -> Usage -> Usage -> Either TypeError Usage
combineOccurences _ (Observed loc) (Observed _) = Right $ Observed loc
combineOccurences name (Consumed wloc) (Observed rloc) =
  Left $ UseAfterConsume (baseName name) rloc wloc
combineOccurences name (Observed rloc) (Consumed wloc) =
  Left $ UseAfterConsume (baseName name) rloc wloc
combineOccurences name (Consumed loc1) (Consumed loc2) =
  Left $ ConsumeAfterConsume (baseName name) (max loc1 loc2) (min loc1 loc2)

checkOccurences :: Occurences -> Either TypeError ()
checkOccurences = void . M.traverseWithKey comb . usageMap
  where comb _    []     = Right ()
        comb name (u:us) = foldM_ (combineOccurences name) u us

allObserved :: Occurences -> Names
allObserved = S.unions . map observed

allConsumed :: Occurences -> Names
allConsumed = S.unions . map consumed

allOccuring :: Occurences -> Names
allOccuring occs = allConsumed occs <> allObserved occs

seqOccurences :: Occurences -> Occurences -> Occurences
seqOccurences occurs1 occurs2 =
  filter (not . nullOccurence) $ map filt occurs1 ++ occurs2
  where filt occ =
          occ { observed = observed occ `S.difference` postcons }
        postcons = allConsumed occurs2

altOccurences :: Occurences -> Occurences -> Occurences
altOccurences occurs1 occurs2 =
  filter (not . nullOccurence) $ map filt1 occurs1 ++ map filt2 occurs2
  where filt1 occ =
          occ { consumed = consumed occ `S.difference` cons2
              , observed = observed occ `S.difference` cons2 }
        filt2 occ =
          occ { consumed = consumed occ
              , observed = observed occ `S.difference` cons1 }
        cons1 = allConsumed occurs1
        cons2 = allConsumed occurs2

--- Scope management

data ValBinding = BoundV [TypeParam] PatternType
                -- ^ Aliases in parameters indicate the lexical
                -- closure.
                | OverloadedF [(TypeBase () (), PatternType)]
                | EqualityF
                | OpaqueF
                | WasConsumed SrcLoc
                deriving (Show)

-- | Type checking happens with access to this environment.  The
-- tables will be extended during type-checking as bindings come into
-- scope.
data TermScope = TermScope { scopeVtable  :: M.Map VName ValBinding
                           , scopeTypeTable :: M.Map VName TypeBinding
                           , scopeNameMap :: NameMap
                           } deriving (Show)

instance Sem.Semigroup TermScope where
  TermScope vt1 tt1 nt1 <> TermScope vt2 tt2 nt2 =
    TermScope (vt2 `M.union` vt1) (tt2 `M.union` tt1) (nt2 `M.union` nt1)

instance Monoid TermScope where
  mempty = TermScope mempty mempty mempty
  mappend = (Sem.<>)

envToTermScope :: Env -> TermScope
envToTermScope env = TermScope vtable (envTypeTable env) (envNameMap env)
  where vtable = M.map valBinding $ envVtable env
        valBinding (TypeM.BoundV tps v) = BoundV tps $ v `setAliases` mempty

-- | Mapping from fresh type variables, instantiated from the type
-- schemes of polymorphic functions, to specific types as determined
-- on application and the location of that application.
type Substs = M.Map VName (TypeBase () (), SrcLoc)

addSubst :: SrcLoc -> VName -> TypeBase () () -> TermTypeM ()
addSubst loc vn tp = do
  substs <- gets $ M.map $ first $ applySubst (M.singleton vn tp')
  case M.lookup vn substs of
    Just (old_t, old_loc) | tp' /= old_t ->
      throwError $ TypeError loc $ "Argument determines type parameter '" ++
      pretty (baseName vn) ++ "' as " ++ pretty tp' ++ ", but it was previously " ++
      "determined to be " ++ pretty old_t ++ " at " ++ locStr old_loc ++ "."
    _ -> put $ M.insert vn (tp', loc) substs
  where tp' = tp `setUniqueness` Nonunique


newtype TermTypeM a = TermTypeM (RWST
                                 TermScope
                                 Occurences
                                 Substs
                                 TypeM
                                 a)
  deriving (Monad, Functor, Applicative,
            MonadReader TermScope,
            MonadWriter Occurences,
            MonadState Substs,
            MonadError TypeError)

instance Fail.MonadFail TermTypeM where
  fail = throwError . TypeError noLoc

runTermTypeM :: TermTypeM a -> TypeM (a, Occurences)
runTermTypeM (TermTypeM m) = do
  initial_scope <- (initialTermScope <>) <$> (envToTermScope <$> askEnv)
  evalRWST m initial_scope mempty

liftTypeM :: TypeM a -> TermTypeM a
liftTypeM = TermTypeM . lift

initialTermScope :: TermScope
initialTermScope = TermScope initialVtable mempty topLevelNameMap
  where initialVtable = M.fromList $ mapMaybe addIntrinsicF $ M.toList intrinsics

        funF ts t = foldr (Arrow mempty Nothing . Prim) (Prim t) ts

        addIntrinsicF (name, IntrinsicMonoFun ts t) =
          Just (name, BoundV [] $ funF ts t)
        addIntrinsicF (name, IntrinsicOverloadedFun variants) =
          Just (name, OverloadedF $ map frob variants)
          where frob (pt, (pts, rt)) = (Prim pt, funF pts rt)
        addIntrinsicF (name, IntrinsicPolyFun tvs pts rt) =
          Just (name, BoundV tvs $
                      fromStruct $ vacuousShapeAnnotations $
                      foldr (Arrow mempty Nothing) rt pts)
        addIntrinsicF (name, IntrinsicEquality) =
          Just (name, EqualityF)
        addIntrinsicF (name, IntrinsicOpaque) =
          Just (name, OpaqueF)
        addIntrinsicF _ = Nothing

instance MonadTypeChecker TermTypeM where
  warn loc problem = liftTypeM $ warn loc problem
  newName = liftTypeM . newName
  newID = liftTypeM . newID

  checkQualName space name loc = snd <$> checkQualNameWithEnv space name loc

  bindNameMap m = local $ \scope ->
    scope { scopeNameMap = m <> scopeNameMap scope }

  localEnv env (TermTypeM m) = do
    cur_state <- get
    cur_scope <- ask
    let cur_scope' =
          cur_scope { scopeNameMap = scopeNameMap cur_scope `M.difference` envNameMap env }
    (x,new_state,occs) <- liftTypeM $ localTmpEnv env $
                          runRWST m cur_scope' cur_state
    tell occs
    put new_state
    return x

  lookupType loc qn = do
    outer_env <- liftTypeM askRootEnv
    (scope, qn'@(QualName qs name)) <- checkQualNameWithEnv Type qn loc
    case M.lookup name $ scopeTypeTable scope of
      Nothing -> throwError $ UndefinedType loc qn
      Just (TypeAbbr ps def) ->
        return (qn', ps, qualifyTypeVars outer_env (map typeParamName ps) qs def)

  lookupMod loc name = liftTypeM $ TypeM.lookupMod loc name
  lookupMTy loc name = liftTypeM $ TypeM.lookupMTy loc name
  lookupImport loc name = liftTypeM $ TypeM.lookupImport loc name

  lookupVar loc qn = do
    outer_env <- liftTypeM askRootEnv
    (scope, qn'@(QualName qs name)) <- checkQualNameWithEnv Term qn loc
    case M.lookup name $ scopeVtable scope of
      Nothing -> throwError $ UnknownVariableError Term qn loc
      Just (BoundV tparams t)
        | "_" `isPrefixOf` pretty name -> throwError $ UnderscoreUse loc qn
        | Arrow{} <- t -> do
            (tnames, inst_list, t') <- instantiateTypeScheme tparams t
            let qual = qualifyTypeVars outer_env tnames qs
            return (qn', inst_list, qual $ removeShapeAnnotations t')
        | otherwise ->
            return (qn', [], qualifyTypeVars outer_env [] qs $
                             removeShapeAnnotations t)
      Just EqualityF -> throwError $ FunctionIsNotValue loc qn
      Just OpaqueF -> throwError $ FunctionIsNotValue loc qn
      Just OverloadedF{} -> throwError $ FunctionIsNotValue loc qn
      Just (WasConsumed wloc) -> throwError $ UseAfterConsume (baseName name) loc wloc

checkQualNameWithEnv :: Namespace -> QualName Name -> SrcLoc -> TermTypeM (TermScope, QualName VName)
checkQualNameWithEnv space qn@(QualName [q] _) loc
  | nameToString q == "intrinsics" = do
      -- Check if we are referring to the magical intrinsics
      -- module.
      (_, QualName _ q') <- liftTypeM $ TypeM.checkQualNameWithEnv Term (QualName [] q) loc
      if baseTag q' <= maxIntrinsicTag
        then checkIntrinsic space qn loc
        else checkReallyQualName space qn loc
checkQualNameWithEnv space qn@(QualName quals name) loc = do
  scope <- ask
  case quals of
    [] | Just name' <- M.lookup (space, name) $ scopeNameMap scope ->
           return (scope, name')
    _ -> checkReallyQualName space qn loc

checkIntrinsic :: Namespace -> QualName Name -> SrcLoc -> TermTypeM (TermScope, QualName VName)
checkIntrinsic space qn@(QualName _ name) loc
  | Just v <- M.lookup (space, name) intrinsicsNameMap = do
      scope <- ask
      return (scope, v)
  | otherwise =
      throwError $ UnknownVariableError space qn loc

checkReallyQualName :: Namespace -> QualName Name -> SrcLoc -> TermTypeM (TermScope, QualName VName)
checkReallyQualName space qn loc = do
  (env, name') <- liftTypeM $ TypeM.checkQualNameWithEnv space qn loc
  return (envToTermScope env, name')

-- | In a few rare cases (overloaded builtin functions), the type of
-- the parameters actually matters.
lookupFunction :: QualName Name -> CompType -> SrcLoc
               -> TermTypeM (QualName VName, [TypeBase () ()], PatternType)
lookupFunction qn argtype loc = do
  outer_env <- liftTypeM askRootEnv
  (scope, qn'@(QualName qs name)) <- checkQualNameWithEnv Term qn loc
  case M.lookup name $ scopeVtable scope of
    Nothing -> throwError $ UnknownVariableError Term qn loc
    Just (WasConsumed wloc) -> throwError $ UseAfterConsume (baseName name) loc wloc
    Just (BoundV tparams t@Arrow{}) -> do
      (tnames, inst_list, t') <- instantiateTypeScheme tparams t
      return (qn', inst_list, qualifyTypeVars outer_env tnames qs t')
    Just (BoundV _ t) -> throwError $ ValueIsNotFunction loc qn $ removeShapeAnnotations t
    Just (OverloadedF overloads) ->
      -- Lookup the type of an overloaded function based on the type of its
      -- first argument, since that's enough to uniquely determine its type.
      case lookup (toStructural argtype) overloads of
        Nothing -> throwError $ TypeError loc $ "Overloaded function " ++ pretty qn ++
                   " not defined for arguments of type " ++ pretty argtype
        Just f -> return (qn', [], f)
    Just OpaqueF ->
      let t' = vacuousShapeAnnotations argtype
      in return (qn', [], Arrow mempty Nothing (t' `setUniqueness` Nonunique)
                          (t' `setUniqueness` Nonunique))
    Just EqualityF
      | concreteType argtype ->
          return (qn', [], vacuousShapeAnnotations $
                           Arrow mempty Nothing argtype $
                           Arrow mempty Nothing argtype $ Prim Bool)
      | otherwise ->
          throwError $ TypeError loc $ "Equality not defined for arguments of type " ++
          pretty argtype

-- | Instantiate a type scheme with fresh type variables for its type
-- parameters. Returns the names of the fresh type variables, the instance
-- list, and the instantiated type.
instantiateTypeScheme :: [TypeParam] -> PatternType
                      -> TermTypeM ([VName], [TypeBase () ()], PatternType)
instantiateTypeScheme tparams t = do
  let tparams' = filter isTypeParam tparams
      tnames = map typeParamName tparams'
  fresh_tnames <- mapM newName tnames
  let inst_list = zipWith instTypeParam tparams' fresh_tnames
      substs = M.fromList $ zip tnames inst_list
      t' = substTypesAny substs t
  return (fresh_tnames, inst_list, t')

  where instTypeParam tparam fresh_vn
          | isLifted tparam = LiftedTypeVar (typeName fresh_vn)
          | otherwise       = TypeVar (typeName fresh_vn) []
        isLifted TypeParamLiftedType{} = True
        isLifted _                     = False

--- Basic checking

-- | Determine if two types are identical, ignoring uniqueness.
-- Causes a 'TypeError' if they fail to match, and otherwise returns
-- one of them.
unifyExpTypes :: Exp -> Exp -> TermTypeM CompType
unifyExpTypes e1 e2 =
  maybe (throwError $ UnifyError
         (srclocOf e1) (toStructural t1)
         (srclocOf e2) (toStructural t2)) return $
  unifyTypes t1 t2
  where t1 = typeOf e1
        t2 = typeOf e2

--- General binding.

binding :: [Ident] -> TermTypeM a -> TermTypeM a
binding bnds = check . local (`bindVars` bnds)
  where bindVars :: TermScope -> [Ident] -> TermScope
        bindVars = foldl bindVar

        bindVar :: TermScope -> Ident -> TermScope
        bindVar scope (Ident name (Info tp) _) =
          let inedges = S.toList $ aliases tp
              update (BoundV tparams tp')
              -- If 'name' is record-typed, don't alias the components
              -- to 'name', because records have no identity beyond
              -- their components.
                | Record _ <- tp = BoundV tparams tp'
                | otherwise = BoundV tparams (tp' `addAliases` S.insert name)
              update b = b
          in scope { scopeVtable = M.insert name (BoundV [] $ vacuousShapeAnnotations tp) $
                                   adjustSeveral update inedges $
                                   scopeVtable scope
                   }

        adjustSeveral f = flip $ foldl $ flip $ M.adjust f

        -- Check whether the bound variables have been used correctly
        -- within their scope.
        check m = do
          (a, usages) <- collectBindingsOccurences m
          maybeCheckOccurences usages

          mapM_ (checkIfUsed usages) bnds

          return a

        -- Collect and remove all occurences in @bnds@.  This relies
        -- on the fact that no variables shadow any other.
        collectBindingsOccurences m = pass $ do
          (x, usage) <- listen m
          let (relevant, rest) = split usage
          return ((x, relevant), const rest)
          where split = unzip .
                        map (\occ ->
                             let (obs1, obs2) = divide $ observed occ
                                 (con1, con2) = divide $ consumed occ
                             in (occ { observed = obs1, consumed = con1 },
                                 occ { observed = obs2, consumed = con2 }))
                names = S.fromList $ map identName bnds
                divide s = (s `S.intersection` names, s `S.difference` names)

bindingTypes :: [(VName, TypeBinding)] -> TermTypeM a -> TermTypeM a
bindingTypes types = local $ \scope ->
  scope { scopeTypeTable = M.fromList types <> scopeTypeTable scope }

bindingTypeParams :: [TypeParam] -> TermTypeM a -> TermTypeM a
bindingTypeParams tparams = binding (mapMaybe typeParamIdent tparams) .
                            bindingTypes (mapMaybe typeParamType tparams)
  where typeParamType (TypeParamType v _) =
          Just (v, TypeAbbr [] $ TypeVar (typeName v) [])
        typeParamType (TypeParamLiftedType v _) =
          Just (v, TypeAbbr [] $ LiftedTypeVar (typeName v))
        typeParamType TypeParamDim{} =
          Nothing

typeParamIdent :: TypeParam -> Maybe Ident
typeParamIdent (TypeParamDim v loc) =
  Just $ Ident v (Info (Prim (Signed Int32))) loc
typeParamIdent _ = Nothing

bindingIdent :: IdentBase NoInfo Name -> CompType -> (Ident -> TermTypeM a)
             -> TermTypeM a
bindingIdent (Ident v NoInfo vloc) t m =
  bindSpaced [(Term, v)] $ do
    v' <- checkName Term v vloc
    let ident = Ident v' (Info t) vloc
    binding [ident] $ m ident

bindingPatternGroup :: [UncheckedTypeParam]
                    -> [(UncheckedPattern, InferredType)]
                    -> ([TypeParam] -> [Pattern] -> TermTypeM a) -> TermTypeM a
bindingPatternGroup tps orig_ps m = do
  checkForDuplicateNames $ map fst orig_ps
  checkTypeParams tps $ \tps' -> bindingTypeParams tps' $ do
    let descend ps' ((p,t):ps) =
          checkPattern p t $ \p' ->
            binding (S.toList $ patIdentSet p') $ descend (p':ps') ps
        descend ps' [] = do
          -- Perform an observation of every type parameter.  This
          -- prevents unused-name warnings for otherwise unused
          -- dimensions.
          mapM_ observe $ mapMaybe typeParamIdent tps'
          checkTypeParamsUsed tps' ps'

          m tps' $ reverse ps'

    descend [] orig_ps

bindingPattern :: [UncheckedTypeParam]
               -> PatternBase NoInfo Name -> InferredType
               -> ([TypeParam] -> Pattern -> TermTypeM a) -> TermTypeM a
bindingPattern tps p t m = do
  checkForDuplicateNames [p]
  checkTypeParams tps $ \tps' -> bindingTypeParams tps' $
    checkPattern p t $ \p' -> binding (S.toList $ patIdentSet p') $ do
      -- Perform an observation of every declared dimension.  This
      -- prevents unused-name warnings for otherwise unused dimensions.
      mapM_ observe $ patternDims p'
      checkTypeParamsUsed tps' [p']

      m tps' p'

checkTypeParamsUsed :: [TypeParam] -> [Pattern] -> TermTypeM ()
checkTypeParamsUsed tps ps = do
  let uses = mconcat $ map patternUses ps
      check (TypeParamDim pv loc)
        | qualName pv `elem` patternDimUses uses = return ()
        | otherwise =
            throwError $ TypeError loc $
            "Type parameter " ++ pretty (baseName pv) ++
            " not used in value parameters."
      check tparam
        | qualName pv `elem` patternTypeUses uses = return ()
        | otherwise =
            throwError $ TypeError loc $
            "Type parameter " ++ pretty (baseName pv) ++
            " not used in value parameters."
        where pv = typeParamName tparam
              loc = srclocOf tparam

  mapM_ check tps

noTypeParamsPermitted :: [UncheckedTypeParam] -> TermTypeM ()
noTypeParamsPermitted ps =
  case mapMaybe typeParamLoc ps of
    loc:_ -> throwError $ TypeError loc "Type parameters are not permitted here."
    []    -> return ()
  where typeParamLoc (TypeParamDim _ _) = Nothing
        typeParamLoc tparam             = Just $ srclocOf tparam

patternDims :: Pattern -> [Ident]
patternDims (PatternParens p _) = patternDims p
patternDims (TuplePattern pats _) = concatMap patternDims pats
patternDims (PatternAscription p (TypeDecl _ (Info t))) =
  patternDims p <> mapMaybe (dimIdent (srclocOf p)) (nestedDims t)
  where dimIdent _ AnyDim            = Nothing
        dimIdent _ (ConstDim _)      = Nothing
        dimIdent _ NamedDim{}        = Nothing
patternDims _ = []

data PatternUses = PatternUses { patternDimUses :: [QualName VName]
                               , patternTypeUses :: [QualName VName]
                               }

instance Sem.Semigroup PatternUses where
  PatternUses x1 y1 <> PatternUses x2 y2 =
    PatternUses (x1<>x2) (y1<>y2)

instance Monoid PatternUses where
  mempty = PatternUses mempty mempty
  mappend = (Sem.<>)

patternUses :: Pattern -> PatternUses
patternUses Id{} = mempty
patternUses Wildcard{} = mempty
patternUses (PatternParens p _) = patternUses p
patternUses (TuplePattern ps _) = mconcat $ map patternUses ps
patternUses (RecordPattern fs _) = mconcat $ map (patternUses . snd) fs
patternUses (PatternAscription p (TypeDecl declte _)) =
  patternUses p <> typeExpUses declte
  where typeExpUses (TEVar qn _) = PatternUses [] [qn]
        typeExpUses (TETuple tes _) = mconcat $ map typeExpUses tes
        typeExpUses (TERecord fs _) = mconcat $ map (typeExpUses . snd) fs
        typeExpUses (TEArray te d _) = typeExpUses te <> dimDeclUses d
        typeExpUses (TEUnique te _) = typeExpUses te
        typeExpUses (TEApply te targ _) = typeExpUses te <> typeArgUses targ
        typeExpUses (TEArrow _ t1 t2 _) = typeExpUses t1 <> typeExpUses t2

        typeArgUses (TypeArgExpDim d _) = dimDeclUses d
        typeArgUses (TypeArgExpType te) = typeExpUses te

        dimDeclUses (NamedDim v) = PatternUses [v] []
        dimDeclUses _ = mempty

--- Main checkers

checkExp :: UncheckedExp -> TermTypeM Exp

checkExp (Literal val loc) =
  return $ Literal val loc

checkExp (TupLit es loc) =
  TupLit <$> mapM checkExp es <*> pure loc

checkExp (RecordLit fs loc) = do
  -- It is easy for programmers to forget that record literals are
  -- right-biased.  Hence, emit a warning if we encounter literal
  -- fields whose values would never be used.

  fs' <- evalStateT (mapM checkField fs) mempty

  return $ RecordLit fs' loc
  where checkField (RecordFieldExplicit f e rloc) = do
          errIfAlreadySet f rloc
          modify $ M.insert f rloc
          RecordFieldExplicit f <$> lift (checkExp e) <*> pure rloc
        checkField (RecordFieldImplicit name NoInfo rloc) = do
          errIfAlreadySet name rloc
          (QualName _ name', _, t) <- lift $ lookupVar rloc $ qualName name
          modify $ M.insert name rloc
          lift $ observe $ Ident name' (Info t) rloc
          return $ RecordFieldImplicit name' (Info t) rloc

        errIfAlreadySet f rloc = do
          maybe_sloc <- gets $ M.lookup f
          case maybe_sloc of
            Just sloc ->
              throwError $ TypeError rloc $ "Field '" ++ pretty f ++
              " previously defined at " ++ locStr sloc ++ "."
            Nothing -> return ()

checkExp (ArrayLit es _ loc) = do
  es' <- mapM checkExp es
  -- Find the universal type of the array arguments.
  et <- case es' of
          [] -> throwError $ TypeError loc "Empty array literal"
          e:es'' ->
            let check elemt eleme
                  | Just elemt' <- elemt `unifyTypes` typeOf eleme =
                    return elemt'
                  | otherwise =
                    throwError $ TypeError loc $ pretty eleme ++
                    " is not of expected type " ++ pretty elemt ++ "."
            in foldM check (typeOf e) es''

  t <- arrayOfM loc et (rank 1) Unique

  return $ ArrayLit es' (Info t) loc

checkExp (Range start maybe_step end NoInfo loc) = do
  start' <- require anyIntType =<< checkExp start
  let start_t = toStructural $ typeOf start'
  maybe_step' <- case maybe_step of
    Nothing -> return Nothing
    Just step -> do
      let warning = warn loc "First and second element of range are identical, this will produce an empty array."
      case (start, step) of
        (Literal x _, Literal y _) -> when (x == y) warning
        (Var x_name _ _, Var y_name _ _) -> when (x_name == y_name) warning
        _ -> return ()
      Just <$> (require [start_t] =<< checkExp step)

  end' <- case end of
    DownToExclusive e -> DownToExclusive <$> (require [start_t] =<< checkExp e)
    UpToExclusive e -> UpToExclusive <$> (require [start_t] =<< checkExp e)
    ToInclusive e -> ToInclusive <$> (require [start_t] =<< checkExp e)

  t <- arrayOfM loc (typeOf start') (rank 1) Unique

  return $ Range start' maybe_step' end' (Info (t `setAliases` mempty)) loc

checkExp (Empty decl NoInfo loc) = do
  decl' <- checkTypeDecl decl
  t <- arrayOfM loc (removeShapeAnnotations $ unInfo $ expandedType decl') (rank 1) Unique
  return $ Empty decl' (Info $ t `setAliases` mempty) loc

checkExp (Ascript e decl loc) = do
  decl' <- checkTypeDecl decl
  e' <- require [removeShapeAnnotations $ unInfo $ expandedType decl']
        =<< checkExp e
  return $ Ascript e' decl' loc

checkExp (BinOp op NoInfo (e1,_) (e2,_) NoInfo loc) = do
  (e1', e1_arg) <- checkArg e1
  (e2', e2_arg) <- checkArg e2

  (op', il, ftype) <- lookupFunction op (argType e1_arg) loc

  (e1_pt : e2_pt : pts, rettype') <-
    checkFuncall loc ftype [e1_arg, e2_arg]
  return $ BinOp op' (Info il) (e1', Info e1_pt) (e2', Info e2_pt)
    (Info (pts, removeShapeAnnotations rettype')) loc

checkExp (Project k e NoInfo loc) = do
  e' <- checkExp e
  case typeOf e' of
    Record fs | Just t <- M.lookup k fs ->
                return $ Project k e' (Info t) loc
    _ -> throwError $ InvalidField loc (typeOf e') (pretty k)

checkExp (If e1 e2 e3 _ loc) =
  sequentially (require [Prim Bool] =<< checkExp e1) $ \e1' _ -> do
  ((e2', e3'), dflow) <- tapOccurences $ checkExp e2 `alternative` checkExp e3
  brancht <- unifyExpTypes e2' e3'
  unless (orderZero brancht) $
    throwError $ TypeError loc $ "Branches of conditional have type " ++
    pretty brancht ++ ", but they are only allowed to have base type."
  let t' = addAliases brancht (`S.difference` allConsumed dflow)
  return $ If e1' e2' e3' (Info t') loc

checkExp (Parens e loc) =
  Parens <$> checkExp e <*> pure loc

checkExp (QualParens modname e loc) = do
  (modname',mod) <- lookupMod loc modname
  case mod of
    ModEnv env -> localEnv (qualifyEnv modname' env) $ do
      e' <- checkExp e
      return $ QualParens modname' e' loc
    ModFun{} ->
      throwError $ TypeError loc $ "Module " ++ pretty modname ++ " is a parametric module."
  where qualifyEnv modname' env =
          env { envNameMap = M.map (qualify' modname') $ envNameMap env }
        qualify' modname' (QualName qs name) =
          QualName (qualQuals modname' ++ [qualLeaf modname'] ++ qs) name

checkExp (Var qn NoInfo loc) = do
  -- The qualifiers of a variable is divided into two parts: first a
  -- possibly-empty sequence of module qualifiers, followed by a
  -- possible-empty sequence of record field accesses.  We use scope
  -- information to perform the split, by taking qualifiers off the
  -- end until we find a module.

  (qn', il, t, fields) <- findRootVar (qualQuals qn) (qualLeaf qn)
  observe $ Ident (qualLeaf qn') (Info t) loc
  let (ps, ret) = unfoldFunType t
      ps' = map (vacuousShapeAnnotations . toStruct) ps

  foldM checkField (Var qn' (Info (il, ps', ret)) loc) fields
  where findRootVar qs name = do
          r <- (Right <$> lookupVar loc (QualName qs name))
               `catchError` handler
          case r of
            Left err | null qs -> throwError err
                     | otherwise -> do
                         (qn', il, t, fields) <- findRootVar (init qs) (last qs)
                         return (qn', il, t, fields++[name])
            Right (qn', il, t) -> return (qn', il, t, [])

        handler (UnknownVariableError ns qn' _)
          | null (qualQuals qn') = return . Left $ UnknownVariableError ns qn loc
        handler e = return $ Left e

        checkField e k =
          case typeOf e of
            Record fs | Just t <- M.lookup k fs ->
                        return $ Project k e (Info t) loc
            _ -> throwError $ InvalidField loc (typeOf e) (pretty k)

checkExp (Negate arg loc) = do
  arg' <- require anyNumberType =<< checkExp arg
  return $ Negate arg' loc

checkExp (Apply e1 e2 NoInfo NoInfo loc) = do
  (e2', arg) <- checkArg e2
  let tp2 = argType arg
  case e1 of
    Var qn _ var_loc -> do
      r <- (Right <$> lookupFunction qn tp2 loc)
            `catchError` (return . Left)
      case r of
        Right (fname, il, ftype) -> do
          let ftype' = removeShapeAnnotations ftype
          (paramtypes@(t1 : rest), rettype) <- checkApply loc ftype' arg
          return $ Apply (Var fname (Info (il, paramtypes, rettype)) var_loc)
                   e2' (Info $ diet t1) (Info (rest, rettype)) loc

        -- Even if the function lookup failed, the applied expression
        -- may still be a record projection of a function.
        Left _ -> checkGeneralApp e2' arg

    _ -> checkGeneralApp e2' arg

  where checkGeneralApp e2' arg = do
          e1' <- checkExp e1
          (t1 : paramtypes, rettype) <- checkApply loc (typeOf e1') arg
          return $ Apply e1' e2' (Info $ diet t1)
                   (Info (paramtypes, rettype)) loc

checkExp (LetPat tparams pat e body pos) = do
  noTypeParamsPermitted tparams
  sequentially (checkExp e) $ \e' _ ->
    -- Not technically an ascription, but we want the pattern to have
    -- exactly the type of 'e'.
    bindingPattern tparams pat (Ascribed $ vacuousShapeAnnotations $ typeOf e') $ \tparams' pat' -> do
    body' <- checkExp body
    return $ LetPat tparams' pat' e' body' pos

checkExp (LetFun name (tparams, params, maybe_retdecl, NoInfo, e) body loc) =
  bindSpaced [(Term, name)] $
  sequentially (checkFunDef' (name, maybe_retdecl, tparams, params, e, loc)) $
    \(name', tparams', params', maybe_retdecl', rettype, e') closure -> do

    let ftype = foldr (uncurry (Arrow ()) . patternParam) rettype params'
        entry = BoundV tparams' $ ftype `setAliases` allOccuring closure
        bindF scope = scope { scopeVtable = M.insert name' entry $ scopeVtable scope }
    body' <- local bindF $ checkExp body

    return $ LetFun name' (tparams', params', maybe_retdecl', Info rettype, e') body' loc

checkExp (LetWith dest src idxes ve body pos) = do
  src' <- checkIdent src

  unless (unique $ unInfo $ identType src') $
    throwError $ TypeError pos $ "Source '" ++ pretty (identName src) ++
    "' has type " ++ pretty (unInfo $ identType src') ++ ", which is not unique"

  idxes' <- mapM checkDimIndex idxes
  case peelArray (length $ filter isFix idxes') (unInfo $ identType src') of
    Nothing -> throwError $ IndexingError
               (arrayRank $ unInfo $ identType src') (length idxes) (srclocOf src)
    Just elemt -> do
      let elemt' = toStructural elemt `setUniqueness` Nonunique
      sequentially (require [elemt'] =<< checkExp ve) $ \ve' _ -> do
        when (identName src' `S.member` aliases (typeOf ve')) $
          throwError $ BadLetWithValue pos

        bindingIdent dest (unInfo (identType src') `setAliases` S.empty) $ \dest' -> do
          body' <- consuming src' $ checkExp body
          return $ LetWith dest' src' idxes' ve' body' pos
  where isFix DimFix{} = True
        isFix _        = False

checkExp (Update src idxes ve loc) =
  sequentially (checkExp src) $ \src' _ -> do
    let src_t = typeOf src'
        src_als = aliases src_t

    unless (unique src_t) $
      throwError $ TypeError loc $ "Source '" ++ pretty src ++
      "' has type " ++ pretty src_t ++ ", which is not unique"

    idxes' <- mapM checkDimIndex idxes
    case peelArray (length $ filter isFix idxes') src_t of
      Nothing -> throwError $ IndexingError (arrayRank src_t) (length idxes) (srclocOf src)
      Just elemt -> do
        ve' <- require [toStructural elemt] =<< checkExp ve

        unless (S.null $ aliases (typeOf src') `S.intersection` aliases (typeOf ve')) $
          throwError $ BadLetWithValue loc

        consume loc src_als
        return $ Update src' idxes' ve' loc
  where isFix DimFix{} = True
        isFix _        = False

checkExp (Index e idxes pos) = do
  e' <- checkExp e
  let vt = typeOf e'
  when (arrayRank vt < length idxes) $
    throwError $ IndexingError (arrayRank vt) (length idxes) pos
  idxes' <- mapM checkDimIndex idxes
  return $ Index e' idxes' pos

checkExp (Reshape shapeexp arrexp loc) = do
  shapeexp' <- checkExp shapeexp
  arrexp' <- checkExp arrexp

  case typeOf shapeexp' of
    t | Just ts <- isTupleRecord t,
        all ((`elem` anyIntType) . toStruct) ts -> return ()
    Prim Signed{} -> return ()
    Prim Unsigned{} -> return ()
    t -> throwError $ TypeError loc $ "Shape argument " ++ pretty shapeexp ++
      " to reshape must be integer or tuple of integers, but is " ++ pretty t

  case typeOf arrexp' of
    Array{} -> return ()
    t -> throwError $ TypeError loc $
         "Array argument to reshape must be an array, but has type " ++ pretty t

  return $ Reshape shapeexp' arrexp' loc

checkExp (Rearrange perm arrexp pos) = do
  arrexp' <- checkExp arrexp
  let r = arrayRank $ typeOf arrexp'
  when (length perm /= r || sort perm /= [0..r-1]) $
    throwError $ PermutationError pos perm r
  return $ Rearrange perm arrexp' pos

checkExp (Rotate d offexp arrexp loc) = do
  arrexp' <- checkExp arrexp
  offexp' <- require [Prim $ Signed Int32] =<< checkExp offexp
  let r= arrayRank (typeOf arrexp')
  when (r <= d) $
    throwError $ TypeError loc $ "Attempting to rotate dimension " ++ show d ++
    " of array " ++ pretty arrexp ++
    " which has only " ++ show r ++ " dimensions."
  return $ Rotate d offexp' arrexp' loc

checkExp (Zip i e es NoInfo NoInfo loc) = do
  e' <- checkExp e
  es' <- mapM checkExp es

  ts <- forM (e':es') $ \arr_e ->
    let arr_e_t = typeOf arr_e
    in case typeToRecordArrayElem =<< peelArray (i+1) arr_e_t of
         Just t -> return t
         Nothing -> throwError $ TypeError (srclocOf arr_e) $
                    "Expected array with at least " ++ show (1+i) ++
                    " dimensions, but got " ++ pretty arr_e_t ++ "."

  let u = mconcat $ map (uniqueness . typeOf) $ e':es'
  return $ Zip i e' es' (Info ts) (Info u) loc

checkExp (Unzip e _ loc) = do
  e' <- checkExp e
  case typeOf e' of
    Array (ArrayRecordElem fs) shape u
      | Just ets <- map (componentType shape u) <$> areTupleFields fs ->
          return $ Unzip e' (map Info ets) loc
    t ->
      throwError $ TypeError loc $
      "Argument to unzip is not an array of tuples, but " ++
      pretty t ++ "."
  where componentType shape u et =
          case et of
            RecordArrayElem et' ->
              Array et' shape u
            RecordArrayArrayElem et' et_shape et_u ->
              Array et' (shape <> et_shape) (u `max` et_u)

checkExp (Unsafe e loc) =
  Unsafe <$> checkExp e <*> pure loc

checkExp (Map fun arrexps NoInfo loc) = do
  (arrexps', args) <- unzip <$> mapM checkSOACArrayArg arrexps
  (fun', rt) <- checkFunExp fun args
  t <- arrayOfM loc rt (rank 1) Unique
  return $ Map fun' arrexps' (Info $ t `setAliases` mempty) loc

checkExp (Reduce comm fun startexp arrexp pos) = do
  (startexp', startarg) <- checkArg startexp
  (arrexp', arrarg) <- checkSOACArrayArg arrexp
  (fun', redtype) <- checkFunExp fun [startarg, arrarg]
  unless (typeOf startexp' `subtypeOf` redtype) $
    throwError $ TypeError pos $ "Initial value is of type " ++ pretty (typeOf startexp') ++ ", but reduce function returns type " ++ pretty redtype ++ "."
  unless (argType arrarg `subtypeOf` redtype) $
    throwError $ TypeError pos $ "Array element value is of type " ++ pretty (argType arrarg) ++ ", but reduce function returns type " ++ pretty redtype ++ "."
  return $ Reduce comm fun' startexp' arrexp' pos

checkExp (Scan fun startexp arrexp pos) = do
  (startexp', startarg) <- checkArg startexp
  (arrexp', arrarg@(inrowt, _, _)) <- checkSOACArrayArg arrexp
  (fun', scantype) <- checkFunExp fun [startarg, arrarg]
  unless (typeOf startexp' `subtypeOf` scantype) $
    throwError $ TypeError pos $ "Initial value is of type " ++ pretty (typeOf startexp') ++ ", but scan function returns type " ++ pretty scantype ++ "."
  unless (inrowt `subtypeOf` scantype) $
    throwError $ TypeError pos $ "Array element value is of type " ++ pretty inrowt ++ ", but scan function returns type " ++ pretty scantype ++ "."
  return $ Scan fun' startexp' arrexp' pos

checkExp (Filter fun arrexp loc) = do
  (arrexp', (rowelemt, argflow, argloc)) <- checkSOACArrayArg arrexp
  let nonunique_arg = (rowelemt `setUniqueness` Nonunique,
                       argflow, argloc)
  (fun', lam_t) <- checkFunExp fun [nonunique_arg]
  when (lam_t /= Prim Bool) $
    throwError $ TypeError loc $ "Filter function must return bool, but returns " ++ pretty lam_t ++ "."

  return $ Filter fun' arrexp' loc

checkExp (Partition funs arrexp pos) = do
  (arrexp', (rowelemt, argflow, argloc)) <- checkSOACArrayArg arrexp
  let nonunique_arg = (rowelemt `setUniqueness` Nonunique,
                       argflow, argloc)
  funs' <- forM funs $ \fun -> do
    (fun', fun_t) <- checkFunExp fun [nonunique_arg]
    when (fun_t /= Prim Bool) $
      throwError $ TypeError (srclocOf fun') "Partition function does not return bool."
    return fun'

  return $ Partition funs' arrexp' pos

checkExp (Stream form lam arr pos) = do
  (arr',arrarg) <- checkArg arr
  -- arr must have an array type
  unless (arrayRank (typeOf arr') > 0) $
    throwError $ TypeError pos $ "Stream with input array of non-array type " ++ pretty (typeOf arr') ++ "."

  macctup <- case form of
               MapLike{} -> return Nothing
               RedLike{} -> return Nothing

  let fakearg = (fromStruct $ typeOf arr', mempty, srclocOf pos)
      (aas,faas) = case macctup of
                    Nothing        -> ([arrarg],        [fakearg])
                    Just(_,accarg) -> ([accarg, arrarg],[accarg, fakearg])

  (lam', lam_t) <- checkFunExp lam aas
  (_, dflow)<- collectOccurences $ checkFunExp lam faas
  let arr_aliasses = S.toList $ aliases $ typeOf arr'
  let usages = usageMap dflow
  when (any (`M.member` usages) arr_aliasses) $
     throwError $ TypeError pos "Stream with input array used inside lambda."

  -- (i) properly check the lambda on its parameter and
  --(ii) make some fake arguments, which do not alias `arr', and
  --     check that aliases of `arr' are not used inside lam.
  -- check that the result type of lambda matches the accumulator part
  case macctup of
    Just (acc',_) ->
      case lam_t of
        t | Just (acctp:_) <- isTupleRecord t ->
          unless (typeOf acc' `subtypeOf` removeShapeAnnotations acctp) $
          throwError $ TypeError pos ("Stream with accumulator-type missmatch"++
                                "or result arrays of non-array type.")
        rtp' -> unless (typeOf acc' `subtypeOf` removeShapeAnnotations rtp') $
          throwError $ TypeError pos "Stream with accumulator-type missmatch."
    Nothing -> return ()

  -- typecheck stream form lambdas
  form' <-
    case form of
      MapLike o -> return $ MapLike o
      RedLike o comm lam0 -> do
        let accarg :: Arg
            accarg = (fromStruct lam_t, mempty, srclocOf lam')

        (lam0', redtype) <- checkFunExp lam0 [accarg, accarg]
        unless (argType accarg `subtypeOf` redtype) $
            throwError $ TypeError pos $ "Stream's fold fun: Fold function returns type type " ++
                  pretty (argType accarg) ++ ", but reduce fun returns type "++pretty redtype++"."
        return $ RedLike o comm lam0'

  return $ Stream form' lam' arr' pos

checkExp (Concat i arr1exp arr2exps loc) = do
  arr1exp'  <- checkExp arr1exp
  let arr1_t = toStructural (typeOf arr1exp') `setUniqueness` Nonunique
  arr2exps' <- mapM (require [arr1_t] <=< checkExp) arr2exps
  mapM_ ofProperRank arr2exps'
  return $ Concat i arr1exp' arr2exps' loc
  where ofProperRank e
          | arrayRank t <= i =
              throwError $ TypeError loc $ "Cannot concat array " ++ pretty e
              ++ " of type " ++ pretty t
              ++ " across dimension " ++ pretty i ++ "."
          | otherwise = return ()
          where t = typeOf e

checkExp (Lambda tparams params body maybe_retdecl NoInfo loc) =
  bindingPatternGroup tparams (zip params $ repeat NoneInferred) $ \tparams' params' -> do
    maybe_retdecl' <- traverse checkTypeDecl maybe_retdecl
    body' <- checkFunBody (nameFromString "<anonymous>") body
                          (unInfo . expandedType <$> maybe_retdecl') loc
    (maybe_retdecl'', rettype) <- case maybe_retdecl' of
      Just retdecl'@(TypeDecl _ (Info st)) -> return (Just retdecl', st)
      Nothing -> return (Nothing, vacuousShapeAnnotations . toStruct $ typeOf body')
    return $ Lambda tparams' params' body' maybe_retdecl'' (Info rettype) loc

checkExp (OpSection op _ _ _ _ loc) = do
  (op', il, ftype) <- lookupVar loc op
  let (paramtypes, rettype) = unfoldFunType ftype
  case paramtypes of
    t1 : t2 : rest -> do
      let t1' = vacuousShapeAnnotations $ toStruct t1
          t2' = vacuousShapeAnnotations $ toStruct t2
      return $ OpSection op' (Info il) (Info t1') (Info t2')
                 (Info $ foldr (Arrow mempty Nothing) rettype rest ) loc
    _ -> throwError $ TypeError loc $
         "Operator section with invalid operator of type " ++ pretty ftype

checkExp (OpSectionLeft op _ e _ _ loc) = do
  (op', il, ftype) <- lookupVar loc op
  (e', e_arg) <- checkArg e
  (paramtypes, rettype) <- checkApply loc ftype e_arg
  case paramtypes of
    t1 : t2 : rest ->
      let rettype' = foldr (Arrow mempty Nothing .
                             removeShapeAnnotations . fromStruct) rettype rest
      in return $ OpSectionLeft op' (Info il) e' (Info t1, Info t2) (Info rettype') loc
    _ -> throwError $ TypeError loc $
         "Operator section with invalid operator of type " ++ pretty ftype

checkExp (OpSectionRight op _ e _ _ loc) = do
  (op', il, ftype) <- lookupVar loc op
  (e', e_arg) <- checkArg e
  case ftype of
    Arrow as1 m1 t1 (Arrow as2 m2 t2 ret) -> do
      (t2' : t1' : rest, rettype) <-
        checkApply loc (Arrow as2 m2 t2 (Arrow as1 m1 t1 ret)) e_arg
      let rettype' = foldr (Arrow mempty Nothing .
                             removeShapeAnnotations . fromStruct) rettype rest
      return $ OpSectionRight op' (Info il) e' (Info t1', Info t2') (Info rettype') loc
    _ -> throwError $ TypeError loc $
         "Operator section with invalid operator of type " ++ pretty ftype

checkExp (DoLoop tparams mergepat mergeexp form loopbody loc) =
  sequentially (checkExp mergeexp) $ \mergeexp' _ -> do

  noTypeParamsPermitted tparams

  unless (orderZero $ typeOf mergeexp') $
    throwError $ TypeError loc $
    "The type of the merge expression in a do-loop must be of order 0, " ++
    "but it has type " ++ pretty (typeOf mergeexp') ++ "."

  let merge_t =
        Ascribed $ vacuousShapeAnnotations $ typeOf mergeexp' `setAliases` mempty

  -- First we do a basic check of the loop body to figure out which of
  -- the merge parameters are being consumed.  For this, we first need
  -- to check the merge pattern, which requires the (initial) merge
  -- expression.
  --
  -- Play a little with occurences to ensure it does not look like
  -- none of the merge variables are being used.
  ((tparams', mergepat', form', loopbody'), bodyflow) <-
    case form of
      For i uboundexp -> do
        uboundexp' <- require anySignedType =<< checkExp uboundexp
        bindingIdent i (typeOf uboundexp') $ \i' ->
          noUnique $ bindingPattern tparams mergepat merge_t $
          \tparams' mergepat' -> onlySelfAliasing $ tapOccurences $ do
            loopbody' <- checkExp loopbody
            return (tparams',
                    mergepat',
                    For i' uboundexp',
                    loopbody')

      ForIn xpat e -> do
        e' <- checkExp e
        case typeOf e' of
          t | Just t' <- peelArray 1 t ->
                bindingPattern [] xpat (Ascribed $ vacuousShapeAnnotations t') $ \_ xpat' ->
                noUnique $ bindingPattern tparams mergepat merge_t $
                \tparams' mergepat' -> onlySelfAliasing $ tapOccurences $ do
                  loopbody' <- checkExp loopbody
                  return (tparams',
                          mergepat',
                          ForIn xpat' e',
                          loopbody')
            | otherwise ->
                throwError $ TypeError (srclocOf e) $
                "Iteratee of a for-in loop must be an array, but expression has type " ++ pretty t

      While cond ->
        noUnique $ bindingPattern tparams mergepat merge_t $ \tparams' mergepat' ->
        onlySelfAliasing $ tapOccurences $
        sequentially (require [Prim Bool] =<< checkExp cond) $ \cond' _ -> do
          loopbody' <- checkExp loopbody
          return (tparams',
                  mergepat',
                  While cond',
                  loopbody')

  mergepat'' <- convergePattern mergepat'
                (allConsumed bodyflow) (typeOf loopbody') (srclocOf loopbody')

  let consumeMerge (Id _ (Info pt) ploc) mt
        | unique pt = consume ploc $ aliases mt
      consumeMerge (TuplePattern pats _) t | Just ts <- isTupleRecord t =
        zipWithM_ consumeMerge pats ts
      consumeMerge (PatternParens pat _) t =
        consumeMerge pat t
      consumeMerge (PatternAscription pat _) t =
        consumeMerge pat t
      consumeMerge _ _ =
        return ()
  consumeMerge mergepat'' $ typeOf mergeexp'
  return $ DoLoop tparams' mergepat'' mergeexp' form' loopbody' loc

  where
    convergePattern pat body_cons body_t body_loc = do
      let consumed_merge = S.map identName (patIdentSet pat) `S.intersection`
                           body_cons
          uniquePat (Wildcard (Info t) wloc) =
            Wildcard (Info $ t `setUniqueness` Nonunique) wloc
          uniquePat (PatternParens p ploc) =
            PatternParens (uniquePat p) ploc
          uniquePat (Id name (Info t) iloc)
            | name `S.member` consumed_merge =
                let t' = t `setUniqueness` Unique `setAliases` mempty
                in Id name (Info t') iloc
            | otherwise =
                let t' = case t of Record{} -> t
                                   _        -> t `setUniqueness` Nonunique
                in Id name (Info t') iloc
          uniquePat (TuplePattern pats ploc) =
            TuplePattern (map uniquePat pats) ploc
          uniquePat (RecordPattern fs ploc) =
            RecordPattern (map (fmap uniquePat) fs) ploc
          uniquePat (PatternAscription p t) =
            PatternAscription p t

          -- Make the pattern unique where needed.
          pat' = uniquePat pat

      -- Now check that the loop returned the right type.
      unless (body_t `subtypeOf` patternType pat') $
        throwError $ UnexpectedType body_loc
        (toStructural body_t)
        [toStructural $ patternType pat']

      -- Check that the new values of consumed merge parameters do not
      -- alias something bound outside the loop, AND that anything
      -- returned for a unique merge parameter does not alias anything
      -- else returned.
      bound_outside <- asks $ S.fromList . M.keys . scopeVtable
      let checkMergeReturn (Id pat_v (Info pat_t) _) t
            | unique pat_t,
              v:_ <- S.toList $ aliases t `S.intersection` bound_outside =
                lift $ throwError $ TypeError loc $ "Loop return value corresponding to merge parameter " ++
                pretty pat_v ++ " aliases " ++ pretty v ++ "."
            | otherwise = do
                (cons,obs) <- get
                unless (S.null $ aliases t `S.intersection` cons) $
                  lift $ throwError $ TypeError loc $ "Loop return value for merge parameter " ++
                  pretty pat_v ++ " aliases other consumed merge parameter."
                when (unique pat_t &&
                      not (S.null (aliases t `S.intersection` (cons<>obs)))) $
                  lift $ throwError $ TypeError loc $ "Loop return value for consuming merge parameter " ++
                  pretty pat_v ++ " aliases previously returned value." ++ show (aliases t, cons, obs)
                if unique pat_t
                  then put (cons<>aliases t, obs)
                  else put (cons, obs<>aliases t)
          checkMergeReturn (TuplePattern pats _) t | Just ts <- isTupleRecord t =
            zipWithM_ checkMergeReturn pats ts
          checkMergeReturn _ _ =
            return ()
      (pat_cons, _) <- execStateT (checkMergeReturn pat' body_t) (mempty, mempty)
      let body_cons' = body_cons <> pat_cons
      if body_cons' == body_cons && patternType pat' == patternType pat
        then return pat'
        else convergePattern pat' body_cons' body_t body_loc

checkSOACArrayArg :: ExpBase NoInfo Name
                  -> TermTypeM (Exp, Arg)
checkSOACArrayArg e = do
  (e', (t, dflow, argloc)) <- checkArg e
  case peelArray 1 t of
    Nothing -> throwError $ TypeError argloc "SOAC argument is not an array"
    Just rt -> return (e', (rt, dflow, argloc))

checkIdent :: IdentBase NoInfo Name -> TermTypeM Ident
checkIdent (Ident name _ loc) = do
  (QualName _ name', _, vt) <- lookupVar loc (qualName name)
  return $ Ident name' (Info vt) loc

checkDimIndex :: DimIndexBase NoInfo Name -> TermTypeM DimIndex
checkDimIndex (DimFix i) =
  DimFix <$> (require [Prim $ Signed Int32] =<< checkExp i)
checkDimIndex (DimSlice i j s) =
  DimSlice
  <$> maybe (return Nothing) (fmap Just . require [Prim $ Signed Int32] <=< checkExp) i
  <*> maybe (return Nothing) (fmap Just . require [Prim $ Signed Int32] <=< checkExp) j
  <*> maybe (return Nothing) (fmap Just . require [Prim $ Signed Int32] <=< checkExp) s

sequentially :: TermTypeM a -> (a -> Occurences -> TermTypeM b) -> TermTypeM b
sequentially m1 m2 = do
  (a, m1flow) <- collectOccurences m1
  (b, m2flow) <- collectOccurences $ m2 a m1flow
  occur $ m1flow `seqOccurences` m2flow
  return b

findFuncall :: UncheckedExp -> TermTypeM (QualName Name, [UncheckedExp])
findFuncall (Parens e _) =
  findFuncall e
findFuncall (Var fname _ _) =
  return (fname, [])
findFuncall (Apply f arg _ _ _) = do
  (fname, args) <- findFuncall f
  return (fname, args ++ [arg])
findFuncall e =
  throwError $ TypeError (srclocOf e) "Invalid function expression in application."

constructFuncall :: SrcLoc -> QualName VName -> [TypeBase () ()]
                 -> [Exp] -> [StructType] -> TypeBase dim Names
                 -> TermTypeM Exp
constructFuncall loc fname il args paramtypes rettype = do
  let rettype' = removeShapeAnnotations rettype
  return $ foldl (\f (arg,d,remnant) -> Apply f arg (Info d) (Info (remnant, rettype')) loc)
                 (Var fname (Info (il, paramtypes, rettype')) loc)
                 (zip3 args (map diet paramtypes) $ drop 1 $ tails paramtypes)


type Arg = (CompType, Occurences, SrcLoc)

argType :: Arg -> CompType
argType (t, _, _) = t

checkArg :: UncheckedExp -> TermTypeM (Exp, Arg)
checkArg arg = do
  (arg', dflow) <- collectOccurences $ checkExp arg
  return (arg', (typeOf arg', dflow, srclocOf arg'))

checkApply :: SrcLoc -> CompType -> Arg
           -> TermTypeM ([StructType], CompType)
checkApply loc (Arrow as _ tp1 tp2) (argtype, dflow, argloc) = do
  instantiate loc (toStruct tp1) (toStruct argtype)
  let (paramtypes, rettype) = unfoldFunType tp2

  -- Perform substitutions of instantiated variables in the types.
  substs <- gets $ M.map fst
  let rettype' = applySubst substs rettype
      (tp1' : paramtypes') = map (vacuousShapeAnnotations . toStruct .
                                  applySubst substs) (tp1 : paramtypes)

  occur [observation as loc]

  maybeCheckOccurences dflow
  let occurs = consumeArg argloc argtype (diet tp1')
  occur $ dflow `seqOccurences` occurs

  return (tp1' : paramtypes',
          returnType (toStruct rettype') [diet tp1'] [argtype])

checkApply loc ftype arg =
  throwError $ TypeError loc $
  "Attempt to apply an expression of type " ++ pretty ftype ++
  " to an argument of type " ++ pretty (argType arg) ++ "."

checkFuncall :: SrcLoc -> PatternType -> [Arg]
             -> TermTypeM ([StructType], PatternType)
checkFuncall _ ftype [] = return ([], ftype)
checkFuncall loc ftype ((argtype, dflow, argloc) : args) =
  case ftype of
    Arrow as _ t1 t2 -> do
      instantiate loc (toStructural t1) (toStruct argtype)
      substs <- gets $ M.map fst
      let t1' = toStruct $ applySubst substs t1
          t2' = applySubst substs t2

      occur [observation as loc]
      maybeCheckOccurences dflow
      let occurs = consumeArg argloc argtype (diet t1')
      occur $ dflow `seqOccurences` occurs

      (ps, ret) <- checkFuncall loc t2' args
      return (t1' : ps, ret)

    _ -> throwError $ TypeError loc $
         "Attempt to apply an expression of type " ++ pretty ftype ++
         " to an argument of type " ++ pretty argtype ++ "."

consumeArg :: SrcLoc -> CompType -> Diet -> [Occurence]
consumeArg loc (Record ets) (RecordDiet ds) =
  concat $ M.elems $ M.intersectionWith (consumeArg loc) ets ds
consumeArg loc at Consume = [consumption (aliases at) loc]
consumeArg loc at _       = [observation (aliases at) loc]

checkOneExp :: UncheckedExp -> TypeM Exp
checkOneExp e = fmap fst . runTermTypeM $ updateExpTypes =<< checkExp e

maybePermitRecursion :: VName -> [TypeParam] -> [Pattern] -> Maybe StructType
                     -> TermTypeM a -> TermTypeM a
maybePermitRecursion fname tparams params (Just rettype) m = do
  permit <- liftTypeM recursionPermitted
  if permit then
    let patternType' = toStruct . vacuousShapeAnnotations . patternType
        entry = BoundV tparams $
                foldr (Arrow () Nothing . patternType') rettype params `setAliases` mempty
        bindF scope = scope { scopeVtable = M.insert fname entry $ scopeVtable scope }
    in local bindF m
    else m
maybePermitRecursion _ _ _ Nothing m = m

checkFunDef :: (Name, Maybe UncheckedTypeExp,
                [UncheckedTypeParam], [UncheckedPattern],
                UncheckedExp, SrcLoc)
            -> TypeM (VName, [TypeParam], [Pattern], Maybe (TypeExp VName), StructType, Exp)
checkFunDef = fmap fst . runTermTypeM . checkFunDef'

checkFunDef' :: (Name, Maybe UncheckedTypeExp,
                 [UncheckedTypeParam], [UncheckedPattern],
                 UncheckedExp, SrcLoc)
             -> TermTypeM (VName, [TypeParam], [Pattern], Maybe (TypeExp VName), StructType, Exp)
checkFunDef' (fname, maybe_retdecl, tparams, params, body, loc) = noUnique $ do
  fname' <- checkName Term fname loc

  when (baseString fname' == "&&") $
    throwError $ TypeError loc "The && operator may not be redefined."

  when (baseString fname' == "||") $
    throwError $ TypeError loc "The || operator may not be redefined."

  bindingPatternGroup tparams (zip params $ repeat NoneInferred) $ \tparams' params' -> do
    maybe_retdecl' <- traverse checkTypeExp maybe_retdecl

    body' <- maybePermitRecursion fname' tparams' params' (snd <$> maybe_retdecl') $
             checkFunBody fname body (snd <$> maybe_retdecl') loc
    (maybe_retdecl'', rettype) <- case maybe_retdecl' of
      Just (retdecl', retdecl_type) -> do
        let rettype_structural = toStructural retdecl_type
        checkReturnAlias rettype_structural params' $ typeOf body'
        return (Just retdecl', retdecl_type)
      Nothing -> return (Nothing, vacuousShapeAnnotations $ toStruct $ typeOf body')

    body'' <- updateExpTypes body'

    return (fname', tparams', params', maybe_retdecl'', rettype, body'')

  where -- | Check that unique return values do not alias a
        -- non-consumed parameter.
        checkReturnAlias rettp params' =
          foldM_ (checkReturnAlias' params') S.empty . returnAliasing rettp
        checkReturnAlias' params' seen (Unique, names)
          | any (`S.member` S.map snd seen) $ S.toList names =
            throwError $ UniqueReturnAliased fname loc
          | otherwise = do
            notAliasingParam params' names
            return $ seen `S.union` tag Unique names
        checkReturnAlias' _ seen (Nonunique, names)
          | any (`S.member` seen) $ S.toList $ tag Unique names =
            throwError $ UniqueReturnAliased fname loc
          | otherwise = return $ seen `S.union` tag Nonunique names

        notAliasingParam params' names =
          forM_ params' $ \p ->
          let consumedNonunique p' =
                not (unique $ unInfo $ identType p') && (identName p' `S.member` names)
          in case find consumedNonunique $ S.toList $ patIdentSet p of
               Just p' ->
                 throwError $ ReturnAliased fname (baseName $ identName p') loc
               Nothing ->
                 return ()

        tag u = S.map $ \name -> (u, name)

        returnAliasing (Record ets1) (Record ets2) =
          concat $ M.elems $ M.intersectionWith returnAliasing ets1 ets2
        returnAliasing expected got = [(uniqueness expected, aliases got)]

checkFunBody :: Name
             -> ExpBase NoInfo Name
             -> Maybe StructType
             -> SrcLoc
             -> TermTypeM Exp
checkFunBody fname body maybe_rettype loc = do
  body' <- checkExp body

  case maybe_rettype of
    Just rettype -> do
      let rettype_structural = toStructural rettype
      unless (toStructural (typeOf body') `subtypeOf` rettype_structural) $
        throwError $ ReturnTypeError loc fname rettype_structural $ toStructural $ typeOf body'
    Nothing -> return ()

  return body'

-- | Checking an expression that is in function position, like the
-- functional argument to a map.
checkFunExp :: UncheckedExp -> [Arg] -> TermTypeM (Exp, TypeBase () ())
checkFunExp (Parens e loc) args = do
  (e', t) <- checkFunExp e args
  return (Parens e' loc, t)

checkFunExp (Lambda tparams params body maybe_ret NoInfo loc) args
  | length params == length args = do
      let params_with_ts = zip params $ map (Inferred . fromStruct . argType) args
      (maybe_ret', tparams', params', body') <-
        noUnique $ bindingPatternGroup tparams params_with_ts $ \tparams' params' -> do
        maybe_ret' <- traverse checkTypeDecl maybe_ret
        body' <- checkFunBody (nameFromString "<anonymous>") body
                 (unInfo . expandedType <$> maybe_ret') loc
        return (maybe_ret', tparams', params', body')
      let ret' = case maybe_ret' of
                   Nothing -> vacuousShapeAnnotations $ typeOf body' `setAliases` ()
                   Just (TypeDecl _ (Info ret)) -> ret
          lamt = foldr (Arrow () Nothing . patternStructType) ret' params' `setAliases` mempty
      (_, ret'') <- checkFuncall loc lamt args
      return (Lambda tparams' params' body' maybe_ret' (Info $ toStruct ret'') loc,
              removeShapeAnnotations $ toStruct ret'')
  | otherwise = throwError $ TypeError loc $ "Anonymous function defined with " ++
                show (length params) ++ " parameters, but expected to take " ++
                show (length args) ++ " arguments."

checkFunExp (OpSection op NoInfo NoInfo NoInfo NoInfo loc) args
  | [x_arg,y_arg] <- args = do
  (op', il, ftype) <- lookupFunction op (argType x_arg) loc
  (paramtypes', rettype') <- checkFuncall loc ftype [x_arg,y_arg]

  case paramtypes' of
    [x_t, y_t] ->
      return (OpSection op' (Info il)
              (Info x_t) (Info y_t)
              (Info $ removeShapeAnnotations rettype') loc,
              removeShapeAnnotations rettype' `setAliases` mempty)
    _ ->
      fail "Internal type checker error: BinOpFun got bad parameter type."

  | otherwise =
      throwError $ ParameterMismatch (Just op) loc (Left 2) $
      map (toStructural . argType) args

checkFunExp (OpSectionLeft binop NoInfo x _ _ loc) args
  | [arg] <- args = do
      (x', binop', il, xt, yt, ret) <- checkCurryBinOp id binop x loc arg
      return (OpSectionLeft binop' (Info il)
              x' (Info xt, Info yt) (Info ret) loc,
              ret `setAliases` mempty)
  | otherwise =
      throwError $ ParameterMismatch (Just binop) loc (Left 1) $
      map (toStructural . argType) args

checkFunExp (OpSectionRight binop NoInfo x _ _ loc) args
  | [arg] <- args = do
      (x', binop', il, xt, yt, ret) <- checkCurryBinOp (uncurry $ flip (,)) binop x loc arg
      return (OpSectionRight binop' (Info il)
               x' (Info xt, Info yt) (Info ret) loc,
              ret `setAliases` mempty)
  | otherwise =
      throwError $ ParameterMismatch (Just binop) loc (Left 1) $
      map (toStructural . argType) args

checkFunExp e args = do
  (fname, curryargexps) <- findFuncall e
  (curryargexps', curryargs) <- unzip <$> mapM checkArg curryargexps
  let all_args = curryargs ++ args
  (fname', instance_list, ftype) <- case all_args of
    [] -> error "Function expression in SOAC with no arguments."
    (arg : _) -> lookupFunction fname (argType arg) loc

  (paramtypes, rettype) <- checkFuncall loc ftype all_args

  case find (unique . snd) $ zip curryargexps paramtypes of
    Just (arg, _) -> throwError $ CurriedConsumption fname $ srclocOf arg
    _             -> return ()

  let rettype' = removeShapeAnnotations rettype
  e' <- constructFuncall loc fname' instance_list curryargexps' paramtypes rettype'
  return (e', rettype' `setAliases` mempty)
  where loc = srclocOf e

checkCurryBinOp :: ((Arg,Arg) -> (Arg,Arg))
                -> QualName Name -> ExpBase NoInfo Name -> SrcLoc -> Arg
                -> TermTypeM (Exp, QualName VName, [TypeBase () ()],
                              StructType, StructType, CompType)
checkCurryBinOp arg_ordering binop x loc y_arg = do
  (x', x_arg) <- checkArg x
  let (first_arg, second_arg) = arg_ordering (x_arg, y_arg)
  (binop', il, fun) <- lookupFunction binop (argType first_arg) loc
  ([xt, yt], rettype) <- checkFuncall loc fun [first_arg,second_arg]
  return (x', binop', il, xt, yt, removeShapeAnnotations rettype)

--- Consumption

occur :: Occurences -> TermTypeM ()
occur = tell

-- | Proclaim that we have made read-only use of the given variable.
observe :: Ident -> TermTypeM ()
observe (Ident nm (Info t) loc) =
  let als = nm `S.insert` aliases t
  in occur [observation als loc]

-- | Proclaim that we have written to the given variable.
consume :: SrcLoc -> Names -> TermTypeM ()
consume loc als = occur [consumption als loc]

-- | Proclaim that we have written to the given variable, and mark
-- accesses to it and all of its aliases as invalid inside the given
-- computation.
consuming :: Ident -> TermTypeM a -> TermTypeM a
consuming (Ident name (Info t) loc) m = do
  consume loc $ name `S.insert` aliases t
  local consume' m
  where consume' scope =
          scope { scopeVtable = M.insert name (WasConsumed loc) $ scopeVtable scope }

collectOccurences :: TermTypeM a -> TermTypeM (a, Occurences)
collectOccurences m = pass $ do
  (x, dataflow) <- listen m
  return ((x, dataflow), const mempty)

tapOccurences :: TermTypeM a -> TermTypeM (a, Occurences)
tapOccurences = listen

maybeCheckOccurences :: Occurences -> TermTypeM ()
maybeCheckOccurences = badOnLeft . checkOccurences

checkIfUsed :: Occurences -> Ident -> TermTypeM ()
checkIfUsed occs v
  | not $ identName v `S.member` allOccuring occs,
    not $ "_" `isPrefixOf` pretty (identName v) =
      warn (srclocOf v) $ "Unused variable '"++pretty (baseName $ identName v)++"'."
  | otherwise =
      return ()

alternative :: TermTypeM a -> TermTypeM b -> TermTypeM (a,b)
alternative m1 m2 = pass $ do
  (x, occurs1) <- listen m1
  (y, occurs2) <- listen m2
  maybeCheckOccurences occurs1
  maybeCheckOccurences occurs2
  let usage = occurs1 `altOccurences` occurs2
  return ((x, y), const usage)

-- | Make all bindings nonunique.
noUnique :: TermTypeM a -> TermTypeM a
noUnique = local (\scope -> scope { scopeVtable = M.map set $ scopeVtable scope})
  where set (BoundV tparams t) = BoundV tparams $ t `setUniqueness` Nonunique
        set (OverloadedF f)    = OverloadedF f
        set EqualityF          = EqualityF
        set OpaqueF            = OpaqueF
        set (WasConsumed loc)  = WasConsumed loc

onlySelfAliasing :: TermTypeM a -> TermTypeM a
onlySelfAliasing = local (\scope -> scope { scopeVtable = M.mapWithKey set $ scopeVtable scope})
  where set k (BoundV tparams t) = BoundV tparams $ t `addAliases` S.intersection (S.singleton k)
        set _ (OverloadedF f)    = OverloadedF f
        set _ EqualityF          = EqualityF
        set _ OpaqueF            = OpaqueF
        set _ (WasConsumed loc)  = WasConsumed loc


-- | Instantiates the type variables of a polymorphic function, and creates
-- new bindings in the state for the substitutions that are determined.
instantiate :: SrcLoc -> TypeBase () () -> TypeBase () () -> TermTypeM ()
instantiate _ (Prim t1) (Prim t2)
  | t1 == t2 = return ()
instantiate loc (Record fs) (Record arg_fs)
  | M.keys fs == M.keys arg_fs =
      mapM_ (uncurry $ instantiate loc) $ M.intersectionWith (,) fs arg_fs
instantiate loc (TypeVar (TypeName _ tn) targs) (TypeVar (TypeName _ arg_tn) arg_targs)
  | tn == arg_tn, length targs == length arg_targs =
      zipWithM_ instantiateTypeArg targs arg_targs

  where instantiateTypeArg TypeArgDim{} TypeArgDim{} = return ()
        instantiateTypeArg (TypeArgType t _) (TypeArgType arg_t _) =
          instantiate loc t arg_t
        instantiateTypeArg _ _ = throwError $ TypeError loc
          "Cannot instantiate a type argument with a dimension argument (or vice versa)."

instantiate loc param_t@(TypeVar (TypeName [] tn) []) arg_t
  | orderZero arg_t = addSubst loc tn arg_t
  | otherwise = throwError $ FunctionalValueTypeVariable loc param_t arg_t
instantiate loc param_t arg_t@(TypeVar (TypeName [] tn) [])
  | orderZero param_t = addSubst loc tn arg_t
  | otherwise = throwError $ FunctionalValueTypeVariable loc arg_t param_t

instantiate loc (LiftedTypeVar (TypeName [] tn)) arg_t =
  addSubst loc tn arg_t
instantiate loc param_t (LiftedTypeVar (TypeName [] tn)) =
  addSubst loc tn param_t

instantiate loc (Arrow _ _ t1 t1') (Arrow _ _ t2 t2') = do
  instantiate loc t1 t2
  subs <- gets $ M.map fst
  instantiate loc (applySubst subs t1') (applySubst subs t2')
instantiate loc (Array et shape u) arg_t@(Array _ _ p_u)
  | Just arg_t' <- peelArray (shapeRank shape) arg_t,
    p_u `subuniqueOf` u =
      instantiateArrayElemType et arg_t'

  where instantiateArrayElemType (ArrayPrimElem pt ()) (Prim arg_pt)
          | pt == arg_pt = return ()
        instantiateArrayElemType  (ArrayRecordElem fs) (Record arg_fs)
          | M.keys fs == M.keys arg_fs =
              mapM_ (uncurry instantiateRecordArrayElemType) $
              M.intersectionWith (,) fs arg_fs
        instantiateArrayElemType (ArrayPolyElem tn [] ()) arg_t' =
          instantiate loc (TypeVar tn []) arg_t'
        instantiateArrayElemType et' arg_t' =
          throwError $ TypeError loc $
          "Could not instantiate array element type " ++ pretty et' ++
          " with argument type " ++ pretty arg_t'

        instantiateRecordArrayElemType (RecordArrayElem et') arg_t' =
          instantiateArrayElemType et' arg_t'
        instantiateRecordArrayElemType (RecordArrayArrayElem et' shape' u') arg_t' =
          instantiate loc (Array et' shape' u') arg_t'

instantiate loc t arg_t = throwError $ TypeError loc $
                          "Argument of type " ++ pretty arg_t ++
                          " passed for parameter of type " ++ pretty t

-- | Perform substitutions of instantiated variables on the type
-- annotations (including the instance lists) of an expression.
updateExpTypes :: Exp -> TermTypeM Exp
updateExpTypes e = do
  substs <- gets $ M.map fst
  let tv = ASTMapper { mapOnExp         = astMap tv
                     , mapOnName        = pure
                     , mapOnQualName    = pure
                     , mapOnType        = pure . applySubst substs
                     , mapOnCompType    = pure . applySubst substs
                     , mapOnStructType  = pure . applySubst substs
                     , mapOnPatternType = pure . applySubst substs
                     }
  astMap tv e
