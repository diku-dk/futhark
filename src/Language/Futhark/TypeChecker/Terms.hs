{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, TupleSections #-}
-- | Facilities for type-checking Futhark terms.  Checking a term
-- requires a little more context to track uniqueness and such.
module Language.Futhark.TypeChecker.Terms
  ( checkOneExp
  , checkFunDef
  )
where

import Control.Applicative
import Control.Monad.Except hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.Writer hiding (mapM)
import Control.Monad.State hiding (mapM)
import Data.List
import Data.Loc
import Data.Maybe
import Data.Either
import Data.Ord
import Data.Traversable (mapM)

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Prelude hiding (mapM)

import Language.Futhark
import Language.Futhark.TypeChecker.Monad hiding (ValBinding, BoundV, BoundF, checkQualNameWithEnv)
import Language.Futhark.TypeChecker.Types
import qualified Language.Futhark.TypeChecker.Monad as TypeM

--- Uniqueness

data Usage = Consumed SrcLoc
           | Observed SrcLoc
           deriving (Eq, Ord, Show)

data Occurence = Occurence { observed :: Names VName
                           , consumed :: Names VName
                           , location :: SrcLoc
                           }
             deriving (Eq, Show)

instance Located Occurence where
  locOf = locOf . location

observation :: Names VName -> SrcLoc -> Occurence
observation = flip Occurence S.empty

consumption :: Names VName -> SrcLoc -> Occurence
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
  Left $ UseAfterConsume (baseName name) (max loc1 loc2) (min loc1 loc2)

checkOccurences :: Occurences -> Either TypeError ()
checkOccurences = void . M.traverseWithKey comb . usageMap
  where comb _    []     = Right ()
        comb name (u:us) = foldM_ (combineOccurences name) u us

allObserved :: Occurences -> Names VName
allObserved = S.unions . map observed

allConsumed :: Occurences -> Names VName
allConsumed = S.unions . map consumed

allOccuring :: Occurences -> Names VName
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

data ValBinding = BoundV Type
                | BoundF FunBinding Occurences
                -- ^ The occurences is non-empty only for local functions.
                | OverloadedF [([TypeBase Rank ()],FunBinding)]
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

instance Monoid TermScope where
  mempty = TermScope mempty mempty mempty
  TermScope vt1 tt1 nt1 `mappend` TermScope vt2 tt2 nt2 =
    TermScope (vt2 `M.union` vt1) (tt2 `M.union` tt1) (nt2 `M.union` nt1)

envToTermScope :: Env -> TermScope
envToTermScope env = TermScope vtable (envTypeTable env) (envNameMap env)
  where vtable = M.map valBinding $ envVtable env
        valBinding (TypeM.BoundV v) =
          BoundV $ removeShapeAnnotations $ v `setAliases` mempty
        valBinding (TypeM.BoundF f) =
          BoundF f mempty

newtype TermTypeM a = TermTypeM (ReaderT
                                 TermScope
                                 (WriterT
                                  Occurences
                                  TypeM)
                                 a)
  deriving (Monad, Functor, Applicative,
            MonadReader TermScope,
            MonadWriter Occurences,
            MonadError TypeError)

runTermTypeM :: TermTypeM a -> TypeM a
runTermTypeM (TermTypeM m) = do
  initial_scope <- (initialTermScope<>) <$> (envToTermScope <$> askEnv)
  fst <$> runWriterT (runReaderT m initial_scope)

liftTypeM :: TypeM a -> TermTypeM a
liftTypeM = TermTypeM . lift . lift

initialTermScope :: TermScope
initialTermScope = TermScope initialVtable mempty topLevelNameMap
  where initialVtable = M.fromList $ mapMaybe addIntrinsicF $ M.toList intrinsics

        addIntrinsicF (name, IntrinsicMonoFun ts t) =
          Just (name, BoundF ([], map Prim ts, Prim t) mempty)
        addIntrinsicF (name, IntrinsicOverloadedFun variants) =
          Just (name, OverloadedF $ map frob variants)
          where frob (pts, rt) = (map Prim pts, ([], map Prim pts, Prim rt))
        addIntrinsicF (name, IntrinsicPolyFun tvs pts rt) =
          Just (name, BoundF (tvs,
                              map vacuousShapeAnnotations pts,
                              vacuousShapeAnnotations rt)
                      mempty)
        addIntrinsicF (name, IntrinsicEquality) =
          Just (name, EqualityF)
        addIntrinsicF (name, IntrinsicOpaque) =
          Just (name, OpaqueF)
        addIntrinsicF _ = Nothing

instance MonadTypeChecker TermTypeM where
  bad = liftTypeM . bad
  warn loc problem = liftTypeM $ warn loc problem
  newName = liftTypeM . newName
  newID = liftTypeM . newID

  checkQualName space name loc = snd <$> checkQualNameWithEnv space name loc

  bindNameMap m = local $ \scope ->
    scope { scopeNameMap = m <> scopeNameMap scope }

  lookupType loc qn = do
    (scope, qn'@(QualName _ name)) <- checkQualNameWithEnv Type qn loc
    case M.lookup name $ scopeTypeTable scope of
      Nothing -> bad $ UndefinedType loc qn
      Just (TypeAbbr ps def) -> return (qn', ps, def)

  lookupMod loc name = liftTypeM $ TypeM.lookupMod loc name
  lookupMTy loc name = liftTypeM $ TypeM.lookupMTy loc name
  lookupImport loc name = liftTypeM $ TypeM.lookupImport loc name

  lookupVar loc qn = do
    (scope, qn'@(QualName _ name)) <- checkQualNameWithEnv Term qn loc
    case M.lookup name $ scopeVtable scope of
      Nothing -> bad $ UnknownVariableError Term qn loc
      Just (BoundV t) | "_" `isPrefixOf` pretty name -> bad $ UnderscoreUse loc qn
                      | otherwise -> return (qn', t)
      Just BoundF{} -> bad $ FunctionIsNotValue loc qn
      Just EqualityF -> bad $ FunctionIsNotValue loc qn
      Just OpaqueF -> bad $ FunctionIsNotValue loc qn
      Just OverloadedF{} -> bad $ FunctionIsNotValue loc qn
      Just (WasConsumed wloc) -> bad $ UseAfterConsume (baseName name) loc wloc

checkQualNameWithEnv :: Namespace -> QualName Name -> SrcLoc -> TermTypeM (TermScope, QualName VName)
checkQualNameWithEnv space qn@(QualName [q] _) loc
  | nameToString q == "intrinsics" = do
      -- Check if we are referring to the magical intrinsics
      -- module.
      (_, QualName _ q') <- liftTypeM $ TypeM.checkQualNameWithEnv Structure (QualName [] q) loc
      if baseTag q' <= maxIntrinsicTag
        then checkIntrinsic space qn loc
        else checkReallyQualName space qn loc
checkQualNameWithEnv space qn@(QualName quals name) loc = do
  scope <- ask
  descend scope quals
  where descend scope []
          | Just name' <- M.lookup (space, name) $ scopeNameMap scope =
              return (scope, QualName quals name')

        descend _ _ =
          checkReallyQualName space qn loc

checkIntrinsic :: Namespace -> QualName Name -> SrcLoc -> TermTypeM (TermScope, QualName VName)
checkIntrinsic space qn@(QualName _ name) loc
  | Just v <- M.lookup (space, name) intrinsicsNameMap = do
      scope <- ask
      return (scope, QualName [nameFromString "intrinsics"] v)
  | otherwise =
      bad $ UnknownVariableError space qn loc

checkReallyQualName :: Namespace -> QualName Name -> SrcLoc -> TermTypeM (TermScope, QualName VName)
checkReallyQualName space qn loc = do
  (env, name') <- liftTypeM $ TypeM.checkQualNameWithEnv space qn loc
  return (envToTermScope env, name')

-- | In a few rare cases (overloaded builtin functions), the type of
-- the parameters actually matters.
lookupFunction :: QualName Name -> [Type] -> SrcLoc -> TermTypeM (QualName VName, FunBinding, Occurences)
lookupFunction qn argtypes loc = do
  (scope, qn'@(QualName _ name)) <- checkQualNameWithEnv Term qn loc
  case M.lookup name $ scopeVtable scope of
    Nothing -> bad $ UnknownVariableError Term qn loc
    Just (WasConsumed wloc) -> bad $ UseAfterConsume (baseName name) loc wloc
    Just (BoundV t) -> bad $ ValueIsNotFunction loc qn t
    Just (BoundF f closure) -> return (qn', f, closure)
    Just (OverloadedF overloads) ->
      case lookup (map toStructural argtypes) overloads of
        Nothing -> bad $ TypeError loc $ "Overloaded function " ++ pretty qn ++
                   " not defined for arguments of types " ++
                   intercalate ", " (map pretty argtypes)
        Just f -> return (qn', f, mempty)
    Just OpaqueF
      | [t] <- argtypes ->
          let t' = vacuousShapeAnnotations $ toStruct t
          in return (qn', ([], [t' `setUniqueness` Nonunique], t' `setUniqueness` Nonunique), mempty)
      | otherwise ->
          bad $ TypeError loc "Opaque function takes just a single argument."
    Just EqualityF
      | [t1,t2] <- argtypes,
        concreteType t1,
        concreteType t2,
        t1 == t2 ->
          return (qn', ([],
                        map (vacuousShapeAnnotations . toStruct) [t1, t2],
                        Prim Bool),
                       mempty)
      | otherwise ->
          bad $ TypeError loc $ "Equality not defined for arguments of types " ++
          intercalate ", " (map pretty argtypes)

--- Basic checking

-- | Determine if two types are identical, ignoring uniqueness.
-- Causes a 'TypeError' if they fail to match, and otherwise returns
-- one of them.
unifyExpTypes :: Exp -> Exp -> TermTypeM Type
unifyExpTypes e1 e2 =
  maybe (bad $ UnifyError
         (srclocOf e1) (toStructural t1)
         (srclocOf e2) (toStructural t2)) return $
  unifyTypes (typeOf e1) (typeOf e2)
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
              update (BoundV tp')
              -- If 'name' is record-typed, don't alias the components
              -- to 'name', because records have no identity beyond
              -- their components.
                | Record _ <- tp = BoundV tp'
                | otherwise      = BoundV (tp' `addAliases` S.insert name)
              update b = b
          in scope { scopeVtable = M.insert name (BoundV tp) $
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
        typeParamType TypeParamDim{} =
          Nothing

typeParamIdent :: TypeParam -> Maybe Ident
typeParamIdent (TypeParamDim v loc) =
  Just $ Ident v (Info (Prim (Signed Int32))) loc
typeParamIdent TypeParamType{} =
  Nothing

bindingIdent :: IdentBase NoInfo Name -> Type -> (Ident -> TermTypeM a)
             -> TermTypeM a
bindingIdent (Ident v NoInfo vloc) t m =
  bindSpaced [(Term, v)] $ do
    v' <- checkName Term v vloc
    let ident = Ident v' (Info t) vloc
    binding [ident] $ m ident

bindingPatternGroup :: [UncheckedTypeParam]
                    -> [(UncheckedPattern, InferredType)]
                    -> ([TypeParam] -> [Pattern] -> TermTypeM a) -> TermTypeM a
bindingPatternGroup tps ps m =
  checkTypeParams tps $ \tps' -> bindingTypeParams tps' $
  checkPatternGroup tps' ps $ \ps' ->
  binding (S.toList $ S.unions $ map patIdentSet ps') $ do
    -- Perform an observation of every type parameter and every
    -- declared dimension.  This prevents unused-name warnings for
    -- otherwise unused dimensions.
    mapM_ observe $ concatMap patternDims ps' ++ mapMaybe typeParamIdent tps'

    checkTypeParamsUsed tps' ps'

    m tps' ps'

bindingPattern :: [UncheckedTypeParam]
               -> PatternBase NoInfo Name -> InferredType
               -> ([TypeParam] -> Pattern -> TermTypeM a) -> TermTypeM a
bindingPattern tps p t m =
  checkTypeParams tps $ \tps' -> bindingTypeParams tps' $
  checkPattern tps' p t $ \p' ->
  binding (S.toList $ patIdentSet p') $ do
    -- Perform an observation of every declared dimension.  This
    -- prevents unused-name warnings for otherwise unused dimensions.
    mapM_ observe $ patternDims p'

    checkTypeParamsUsed tps' [p']

    m tps' p'

checkTypeParamsUsed :: [TypeParam] -> [Pattern] -> TermTypeM ()
checkTypeParamsUsed tps ps = do
  let uses = mconcat $ map patternUses ps
      check (TypeParamType pv loc)
        | qualName pv `elem` patternTypeUses uses = return ()
        | otherwise =
            throwError $ TypeError loc $
            "Type parameter " ++ pretty (baseName pv) ++
            " not used in value parameters."
      check (TypeParamDim pv loc)
        | qualName pv `elem` patternDimUses uses = return ()
        | otherwise =
            throwError $ TypeError loc $
            "Type parameter " ++ pretty (baseName pv) ++
            " not used in value parameters."

  mapM_ check tps

noTypeParamsPermitted :: [UncheckedTypeParam] -> TermTypeM ()
noTypeParamsPermitted ps =
  case mapMaybe isTypeParam ps of
    loc:_ -> throwError $ TypeError loc "Type parameters are not permitted here."
    []    -> return ()
  where isTypeParam (TypeParamType _ loc) = Just loc
        isTypeParam _                     = Nothing

patternDims :: Pattern -> [Ident]
patternDims (PatternParens p _) = patternDims p
patternDims (TuplePattern pats _) = concatMap patternDims pats
patternDims (PatternAscription p (TypeDecl _ (Info t))) =
  patternDims p <> mapMaybe (dimIdent (srclocOf p)) (nestedDims t)
  where dimIdent _ AnyDim            = Nothing
        dimIdent _ (ConstDim _)      = Nothing
        dimIdent _ NamedDim{}        = Nothing
        dimIdent loc (BoundDim name) = Just $ Ident name (Info (Prim (Signed Int32))) loc
patternDims _ = []

data PatternUses = PatternUses { patternDimUses :: [QualName VName]
                               , patternTypeUses :: [QualName VName]
                               }

instance Monoid PatternUses where
  mempty = PatternUses mempty mempty
  PatternUses x1 y1 `mappend` PatternUses x2 y2 =
    PatternUses (x1<>x2) (y1<>y2)

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
        typeExpUses (TEApply qn targs _) =
          PatternUses [] [qn] <> mconcat (map typeArgUses targs)

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
  where checkField (RecordField f e rloc) = do
          warnIfAlreadySet f rloc
          modify $ M.insert f rloc
          RecordField f <$> lift (checkExp e) <*> pure rloc
        checkField (RecordRecord e) = do
          e' <- lift $ checkExp e
          case typeOf e' of
            Record rfs -> do
              mapM_ (`warnIfAlreadySet` srclocOf e) $ M.keys rfs
              return $ RecordRecord e'
            t ->
              lift $ bad $ TypeError loc $
              "Expression in record literal must be of record type, but is " ++ pretty t

        warnIfAlreadySet f rloc = do
          maybe_sloc <- gets $ M.lookup f
          case maybe_sloc of
            Just sloc ->
              lift $ warn sloc $ "This value for field " ++ pretty f ++
              " is redundant, due to an overriding definition of the same field at " ++
              locStr rloc ++ "."
            Nothing -> return ()

checkExp (ArrayLit es _ loc) = do
  es' <- mapM checkExp es
  -- Find the universal type of the array arguments.
  et <- case es' of
          [] -> bad $ TypeError loc "Empty array literal"
          e:es'' ->
            let check elemt eleme
                  | Just elemt' <- elemt `unifyTypes` typeOf eleme =
                    return elemt'
                  | otherwise =
                    bad $ TypeError loc $ pretty eleme ++ " is not of expected type " ++ pretty elemt ++ "."
            in foldM check (typeOf e) es''

  return $ ArrayLit es' (Info et) loc

checkExp (Empty decl loc) = do
  decl' <- checkTypeDecl loc decl
  return $ Empty decl' loc

checkExp (Ascript e decl loc) = do
  decl' <- checkTypeDecl loc decl
  e' <- require [removeShapeAnnotations $ unInfo $ expandedType decl']
        =<< checkExp e
  return $ Ascript e' decl' loc

checkExp (BinOp op (e1,_) (e2,_) NoInfo loc) = do
  (e1', e1_arg) <- checkArg e1
  (e2', e2_arg) <- checkArg e2

  (op', (tparams, paramtypes, ftype), closure) <-
    lookupFunction op (map argType [e1_arg,e2_arg]) loc

  case paramtypes of
    [e1_pt, e2_pt] -> do
      occur closure
      (_, rettype') <-
        checkFuncall (Just op) loc (tparams, paramtypes, ftype) [e1_arg, e2_arg]
      return $ BinOp op' (e1', diet e1_pt) (e2', diet e2_pt)
        (Info $ removeShapeAnnotations rettype') loc
    _ ->
      fail $ "Internal typechecker error: got invalid parameter types back from type checking binary operator " ++ pretty op

checkExp (Project k e NoInfo loc) = do
  e' <- checkExp e
  case typeOf e' of
    Record fs | Just t <- M.lookup k fs ->
                return $ Project k e' (Info t) loc
    _ -> bad $ InvalidField loc (typeOf e') (pretty k)

checkExp (If e1 e2 e3 _ pos) =
  sequentially (require [Prim Bool] =<< checkExp e1) $ \e1' _ -> do
  ((e2', e3'), dflow) <- tapOccurences $ checkExp e2 `alternative` checkExp e3
  brancht <- unifyExpTypes e2' e3'
  let t' = addAliases brancht (`S.difference` allConsumed dflow)
  return $ If e1' e2' e3' (Info t') pos

checkExp (Parens e loc) =
  Parens <$> checkExp e <*> pure loc

checkExp (Var qn NoInfo loc) = do
  (qn'@(QualName _ name'), t) <- lookupVar loc qn
  observe $ Ident name' (Info t) loc
  return $ Var qn' (Info t) loc

checkExp (Negate arg loc) = do
  arg' <- require anyNumberType =<< checkExp arg
  return $ Negate arg' loc

checkExp (Apply fname args _ loc) = do
  (args', argflows) <- unzip <$> mapM (\(arg,_) -> checkArg arg) args
  (fname', (tparams, paramtypes, ftype), closure) <-
    lookupFunction fname (map argType argflows) loc

  occur closure
  (_, rettype') <- checkFuncall (Just fname) loc (tparams, paramtypes, ftype) argflows

  return $ Apply fname'
    (zip args' $ map diet paramtypes) (Info $ removeShapeAnnotations rettype') loc

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

    let patternType' = toStruct . vacuousShapeAnnotations . patternType
        entry = BoundF (tparams', map patternType' params', rettype) closure
        bindF scope = scope { scopeVtable = M.insert name' entry $ scopeVtable scope }
    body' <- local bindF $ checkExp body

    return $ LetFun name' (tparams', params', maybe_retdecl', Info rettype, e') body' loc

checkExp (LetWith dest src idxes ve body pos) = do
  src' <- checkIdent src

  unless (unique $ unInfo $ identType src') $
    bad $ TypeError pos $ "Source '" ++ pretty (identName src) ++
    "' has type " ++ pretty (unInfo $ identType src') ++ ", which is not unique"

  idxes' <- mapM checkDimIndex idxes
  case peelArray (length $ filter isFix idxes') (unInfo $ identType src') of
    Nothing -> bad $ IndexingError
               (arrayRank $ unInfo $ identType src') (length idxes) (srclocOf src)
    Just elemt ->
      sequentially (require [toStructural elemt] =<< checkExp ve) $ \ve' _ -> do
        when (identName src' `S.member` aliases (typeOf ve')) $
          bad $ BadLetWithValue pos

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
      bad $ TypeError loc $ "Source '" ++ pretty src ++
      "' has type " ++ pretty src_t ++ ", which is not unique"

    idxes' <- mapM checkDimIndex idxes
    case peelArray (length $ filter isFix idxes') src_t of
      Nothing -> bad $ IndexingError (arrayRank src_t) (length idxes) (srclocOf src)
      Just elemt -> do
        ve' <- require [toStructural elemt] =<< checkExp ve

        unless (S.null $ aliases (typeOf src') `S.intersection` aliases (typeOf ve')) $
          bad $ BadLetWithValue loc

        consume loc src_als
        return $ Update src' idxes' ve' loc
  where isFix DimFix{} = True
        isFix _        = False

checkExp (Index e idxes pos) = do
  e' <- checkExp e
  let vt = typeOf e'
  when (arrayRank vt < length idxes) $
    bad $ IndexingError (arrayRank vt) (length idxes) pos
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
    t -> bad $ TypeError loc $ "Shape argument " ++ pretty shapeexp ++
      " to reshape must be integer or tuple of integers, but is " ++ pretty t

  return $ Reshape shapeexp' arrexp' loc

checkExp (Rearrange perm arrexp pos) = do
  arrexp' <- checkExp arrexp
  let rank = arrayRank $ typeOf arrexp'
  when (length perm /= rank || sort perm /= [0..rank-1]) $
    bad $ PermutationError pos perm rank
  return $ Rearrange perm arrexp' pos

checkExp (Rotate d offexp arrexp loc) = do
  arrexp' <- checkExp arrexp
  offexp' <- require [Prim $ Signed Int32] =<< checkExp offexp
  let rank = arrayRank (typeOf arrexp')
  when (rank <= d) $
    bad $ TypeError loc $ "Attempting to rotate dimension " ++ show d ++
    " of array " ++ pretty arrexp ++
    " which has only " ++ show rank ++ " dimensions."
  return $ Rotate d offexp' arrexp' loc

checkExp (Zip i e es loc) = do
  e' <- checkExp e
  es' <- mapM checkExp es

  forM_ (e':es') $ \arr_e ->
    when (arrayRank (typeOf arr_e) < 1+i) $
    bad $ TypeError (srclocOf arr_e) $
    "Expected array with at least " ++ show (1+i) ++
    " dimensions, but got " ++ pretty (typeOf arr_e) ++ "."

  return $ Zip i e' es' loc

checkExp (Unzip e _ pos) = do
  e' <- checkExp e
  case typeOf e' of
    Array (RecordArray fs shape u) | Just ets <- areTupleFields fs ->
      let componentType et =
            let et' = recordArrayElemToType et
                u' = max u $ recordArrayElemUniqueness et
            in arrayOf et' shape u'
      in return $ Unzip e' (map (Info . componentType) ets) pos
    t ->
      bad $ TypeError pos $
      "Argument to unzip is not an array of tuples, but " ++
      pretty t ++ "."

checkExp (Unsafe e loc) =
  Unsafe <$> checkExp e <*> pure loc

checkExp (Map fun arrexps pos) = do
  (arrexps', args) <- unzip <$> mapM checkSOACArrayArg arrexps
  fun' <- checkLambda fun args
  return $ Map fun' arrexps' pos

checkExp (Reduce comm fun startexp arrexp pos) = do
  (startexp', startarg) <- checkArg startexp
  (arrexp', arrarg) <- checkSOACArrayArg arrexp
  fun' <- checkLambda fun [startarg, arrarg]
  let redtype = lambdaReturnType fun'
  unless (typeOf startexp' `subtypeOf` redtype) $
    bad $ TypeError pos $ "Initial value is of type " ++ pretty (typeOf startexp') ++ ", but reduce function returns type " ++ pretty redtype ++ "."
  unless (argType arrarg `subtypeOf` redtype) $
    bad $ TypeError pos $ "Array element value is of type " ++ pretty (argType arrarg) ++ ", but reduce function returns type " ++ pretty redtype ++ "."
  return $ Reduce comm fun' startexp' arrexp' pos

checkExp (Scan fun startexp arrexp pos) = do
  (startexp', startarg) <- checkArg startexp
  (arrexp', arrarg@(inrowt, _, _)) <- checkSOACArrayArg arrexp
  fun' <- checkLambda fun [startarg, arrarg]
  let scantype = lambdaReturnType fun'
  unless (typeOf startexp' `subtypeOf` scantype) $
    bad $ TypeError pos $ "Initial value is of type " ++ pretty (typeOf startexp') ++ ", but scan function returns type " ++ pretty scantype ++ "."
  unless (inrowt `subtypeOf` scantype) $
    bad $ TypeError pos $ "Array element value is of type " ++ pretty inrowt ++ ", but scan function returns type " ++ pretty scantype ++ "."
  return $ Scan fun' startexp' arrexp' pos

checkExp (Filter fun arrexp pos) = do
  (arrexp', (rowelemt, argflow, argloc)) <- checkSOACArrayArg arrexp
  let nonunique_arg = (rowelemt `setUniqueness` Nonunique,
                       argflow, argloc)
  fun' <- checkLambda fun [nonunique_arg]
  when (lambdaReturnType fun' /= Prim Bool) $
    bad $ TypeError pos "Filter function does not return bool."

  return $ Filter fun' arrexp' pos

checkExp (Partition funs arrexp pos) = do
  (arrexp', (rowelemt, argflow, argloc)) <- checkSOACArrayArg arrexp
  let nonunique_arg = (rowelemt `setUniqueness` Nonunique,
                       argflow, argloc)
  funs' <- forM funs $ \fun -> do
    fun' <- checkLambda fun [nonunique_arg]
    when (lambdaReturnType fun' /= Prim Bool) $
      bad $ TypeError (srclocOf fun') "Partition function does not return bool."
    return fun'

  return $ Partition funs' arrexp' pos

checkExp (Stream form lam arr pos) = do
  (arr',arrarg) <- checkArg arr
  -- arr must have an array type
  unless (arrayRank (typeOf arr') > 0) $
    bad $ TypeError pos $ "Stream with input array of non-array type " ++ pretty (typeOf arr') ++ "."

  macctup <- case form of
               MapLike{} -> return Nothing
               RedLike{} -> return Nothing
               Sequential acc -> do
                 (acc',accarg) <- checkArg acc
                 return $ Just (acc',accarg)

  let fakearg = (fromStruct $ typeOf arr', mempty, srclocOf pos)
      (aas,faas) = case macctup of
                    Nothing        -> ([arrarg],        [fakearg])
                    Just(_,accarg) -> ([accarg, arrarg],[accarg, fakearg])

  lam' <- checkLambda lam aas
  (_, dflow)<- collectOccurences $ checkLambda lam faas
  let arr_aliasses = S.toList $ aliases $ typeOf arr'
  let usages = usageMap dflow
  when (any (`M.member` usages) arr_aliasses) $
     bad $ TypeError pos "Stream with input array used inside lambda."

  -- (i) properly check the lambda on its parameter and
  --(ii) make some fake arguments, which do not alias `arr', and
  --     check that aliases of `arr' are not used inside lam.
  -- check that the result type of lambda matches the accumulator part
  case macctup of
    Just (acc',_) ->
      case lambdaReturnType lam' of
        t | Just (acctp:_) <- isTupleRecord t ->
          unless (typeOf acc' `subtypeOf` removeShapeAnnotations acctp) $
          bad $ TypeError pos ("Stream with accumulator-type missmatch"++
                                "or result arrays of non-array type.")
        rtp' -> unless (typeOf acc' `subtypeOf` removeShapeAnnotations rtp') $
          bad $ TypeError pos "Stream with accumulator-type missmatch."
    Nothing -> return ()

  -- typecheck stream form lambdas
  form' <-
    case form of
      MapLike o -> return $ MapLike o
      RedLike o comm lam0 -> do
        let accarg :: Arg
            accarg = (fromStruct $ lambdaReturnType lam', mempty, srclocOf lam')

        lam0' <- checkLambda lam0 [accarg, accarg]
        let redtype = lambdaReturnType lam0'
        unless (argType accarg `subtypeOf` redtype) $
            bad $ TypeError pos $ "Stream's fold fun: Fold function returns type type " ++
                  pretty (argType accarg) ++ ", but reduce fun returns type "++pretty redtype++"."
        return $ RedLike o comm lam0'
      Sequential acc -> do
        (acc',_) <- checkArg acc
        return $ Sequential acc'

  return $ Stream form' lam' arr' pos

checkExp (Split i splitexp arrexp loc) = do
  splitexp' <- checkExp splitexp
  arrexp' <- checkExp arrexp

  case typeOf splitexp' of
    t | Just ts <- isTupleRecord t,
        all (==(Prim $ Signed Int32)) ts -> return ()
    Prim (Signed Int32) -> return ()
    _ -> bad $ TypeError loc $ "Argument " ++ pretty splitexp ++
         " to split must be integer or tuple of integers."

  let t = typeOf arrexp'
  when (arrayRank t <= i) $
    bad $ TypeError loc $ "Cannot split array " ++ pretty arrexp'
    ++ " of type " ++ pretty t
    ++ " across dimension " ++ pretty i ++ "."
  return $ Split i splitexp' arrexp' loc

checkExp (Concat i arr1exp arr2exps loc) = do
  arr1exp'  <- checkExp arr1exp
  arr2exps' <- mapM (require [toStructural $ typeOf arr1exp'] <=< checkExp) arr2exps
  mapM_ ofProperRank arr2exps'
  return $ Concat i arr1exp' arr2exps' loc
  where ofProperRank e
          | arrayRank t <= i =
              bad $ TypeError loc $ "Cannot concat array " ++ pretty e
              ++ " of type " ++ pretty t
              ++ " across dimension " ++ pretty i ++ "."
          | otherwise = return ()
          where t = typeOf e

checkExp (DoLoop tparams mergepat mergeexp form loopbody loc) =
  sequentially (checkExp mergeexp) $ \mergeexp' _ -> do

  noTypeParamsPermitted tparams

  -- First we do a basic check of the loop body to figure out which of
  -- the merge parameters are being consumed.  For this, we first need
  -- to check the merge pattern, which requires the (initial) merge
  -- expression.
  --
  -- Play a little with occurences to ensure it does not look like
  -- none of the merge variables are being used.
  ((tparams', mergepat', form', loopbody'), bodyflow) <-
    case form of
      For dir lboundexp i uboundexp -> do
        uboundexp' <- require anySignedType =<< checkExp uboundexp
        lboundexp' <-
          case lboundexp of
            ZeroBound -> return ZeroBound
            ExpBound e -> do
                e' <- require anySignedType =<< checkExp e
                void $ unifyExpTypes e' uboundexp'
                return $ ExpBound e'
        bindingIdent i (typeOf uboundexp') $ \i' ->
          noUnique $ bindingPattern tparams mergepat
          (Ascribed $ vacuousShapeAnnotations $ typeOf mergeexp' `setAliases` mempty) $
          \tparams' mergepat' -> onlySelfAliasing $ tapOccurences $ do
            loopbody' <- checkExp loopbody
            return (tparams',
                    mergepat',
                    For dir lboundexp' i' uboundexp',
                    loopbody')
      While cond ->
        noUnique $ bindingPattern tparams mergepat
        (Ascribed $ vacuousShapeAnnotations $
          typeOf mergeexp' `setAliases` mempty) $ \tparams' mergepat' ->
        onlySelfAliasing $ tapOccurences $
        sequentially (require [Prim Bool] =<< checkExp cond) $ \cond' _ -> do
          loopbody' <- checkExp loopbody
          return (tparams',
                  mergepat',
                  While cond',
                  loopbody')

  let consumed_merge = S.map identName (patIdentSet mergepat') `S.intersection`
                       allConsumed bodyflow
      uniquePat (Wildcard (Info t) wloc) =
        Wildcard (Info $ t `setUniqueness` Nonunique) wloc
      uniquePat (PatternParens p ploc) =
        PatternParens (uniquePat p) ploc
      uniquePat (Id (Ident name (Info t) iloc))
        | name `S.member` consumed_merge =
            let t' = t `setUniqueness` Unique `setAliases` mempty
            in Id (Ident name (Info t') iloc)
        | otherwise =
            let t' = case t of Record{} -> t
                               _        -> t `setUniqueness` Nonunique
            in Id (Ident name (Info t') iloc)
      uniquePat (TuplePattern pats ploc) =
        TuplePattern (map uniquePat pats) ploc
      uniquePat (RecordPattern fs ploc) =
        RecordPattern (map (fmap uniquePat) fs) ploc
      uniquePat (PatternAscription p t) =
        PatternAscription (uniquePat p) t

      -- Make the pattern unique where needed.
      mergepat'' = uniquePat mergepat'

  -- Now check that the loop returned the right type.
  unless (typeOf loopbody' `subtypeOf` patternType mergepat'') $
    bad $ UnexpectedType (srclocOf loopbody')
    (toStructural $ typeOf loopbody')
    [toStructural $ patternType mergepat'']

  -- Check that the new values of consumed merge parameters do not
  -- alias something bound outside the loop, AND that anything
  -- returned for a unique merge parameter does not alias anything
  -- else returned.
  bound_outside <- asks $ S.fromList . M.keys . scopeVtable
  let checkMergeReturn (Id ident) t
        | unique $ unInfo $ identType ident,
          v:_ <- S.toList $ aliases t `S.intersection` bound_outside =
            lift $ bad $ TypeError loc $ "Loop return value corresponding to merge parameter " ++
            pretty (identName ident) ++ " aliases " ++ pretty v ++ "."
        | otherwise = do
            (cons,obs) <- get
            unless (S.null $ aliases t `S.intersection` cons) $
              lift $ bad $ TypeError loc $ "Loop return value for merge parameter " ++
              pretty (identName ident) ++ " aliases other consumed merge parameter."
            when (unique (unInfo $ identType ident) &&
                  not (S.null (aliases t `S.intersection` (cons<>obs)))) $
              lift $ bad $ TypeError loc $ "Loop return value for consuming merge parameter " ++
              pretty (identName ident) ++ " aliases previously returned value." ++ show (aliases t, cons, obs)
            if unique (unInfo $ identType ident)
              then put (cons<>aliases t, obs)
              else put (cons, obs<>aliases t)
      checkMergeReturn (TuplePattern pats _) t | Just ts <- isTupleRecord t =
        zipWithM_ checkMergeReturn pats ts
      checkMergeReturn _ _ =
        return ()
  evalStateT (checkMergeReturn mergepat'' $ typeOf loopbody') (mempty, mempty)

  let consumeMerge (Id (Ident _ (Info pt) ploc)) mt
        | unique pt = consume ploc $ aliases mt
      consumeMerge (TuplePattern pats _) t | Just ts <- isTupleRecord t =
        zipWithM_ consumeMerge pats ts
      consumeMerge _ _ =
        return ()
  consumeMerge mergepat'' $ typeOf mergeexp'
  return $ DoLoop tparams' mergepat'' mergeexp' form' loopbody' loc

checkSOACArrayArg :: ExpBase NoInfo Name
                  -> TermTypeM (Exp, Arg)
checkSOACArrayArg e = do
  (e', (t, dflow, argloc)) <- checkArg e
  case peelArray 1 t of
    Nothing -> bad $ TypeError argloc "SOAC argument is not an array"
    Just rt -> return (e', (rt, dflow, argloc))

checkIdent :: IdentBase NoInfo Name -> TermTypeM Ident
checkIdent (Ident name _ loc) = do
  (QualName _ name', vt) <- lookupVar loc (qualName name)
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

type Arg = (Type, Occurences, SrcLoc)

argType :: Arg -> Type
argType (t, _, _) = t

checkArg :: ExpBase NoInfo Name -> TermTypeM (Exp, Arg)
checkArg arg = do
  (arg', dflow) <- collectOccurences $ checkExp arg
  return (arg', (typeOf arg', dflow, srclocOf arg'))

checkFuncall :: Maybe (QualName Name) -> SrcLoc
             -> FunBinding -> [Arg]
             -> TermTypeM ([TypeBase (ShapeDecl VName) ()],
                            TypeBase (ShapeDecl VName) (Names VName))
checkFuncall fname loc funbind args = do
  (_, paramtypes, rettype) <-
    instantiatePolymorphicFunction fname loc funbind args

  forM_ (zip (map diet paramtypes) args) $ \(d, (t, dflow, argloc)) -> do
    maybeCheckOccurences dflow
    let occurs = consumeArg argloc t d
    occur $ dflow `seqOccurences` occurs

  return (paramtypes,
          returnType rettype (map diet paramtypes) (map argType args))

-- | Find concrete types for a call to a polymorphic function.
instantiatePolymorphicFunction :: MonadTypeChecker m =>
                                  Maybe (QualName Name) -> SrcLoc
                               -> FunBinding -> [Arg]
                               -> m FunBinding
instantiatePolymorphicFunction maybe_fname call_loc (tparams, pts, ret) args = do
  unless (length pts == length args) $
    throwError $ TypeError call_loc $ prefix $
    "expecting " ++ pretty (length pts) ++ " arguments, but got " ++
    pretty (length args) ++ " arguments."

  substs <- foldM instantiateArg mempty $ zip (map toStructural pts) args
  let substs' = M.map (TypeSub . TypeAbbr [] . vacuousShapeAnnotations . fst) substs
  return ([],
          map (substituteTypes substs') pts,
          substituteTypes substs' ret)
  where
    prefix = (("In call of function " ++ fname ++ ": ")++)
    fname = maybe "anonymous function" pretty maybe_fname
    tnames = map typeParamName tparams

    instantiateArg substs (pt, (arg_t, _, arg_loc)) =
      case instantiatePolymorphic tnames arg_loc substs pt (toStructural arg_t) of
        Left (Just e) -> throwError $ TypeError arg_loc $ prefix e
        Left Nothing -> throwError $ TypeError arg_loc $ prefix $
                        "argument of type " ++ pretty arg_t ++
                        " passed for parameter of type " ++ pretty pt
        Right v -> return v


consumeArg :: SrcLoc -> Type -> Diet -> [Occurence]
consumeArg loc (Record ets) (RecordDiet ds) =
  concat $ M.elems $ M.intersectionWith (consumeArg loc) ets ds
consumeArg loc at Consume = [consumption (aliases at) loc]
consumeArg loc at _       = [observation (aliases at) loc]

checkOneExp :: UncheckedExp -> TypeM Exp
checkOneExp = runTermTypeM . checkExp

checkFunDef :: (Name, Maybe UncheckedTypeExp,
                [UncheckedTypeParam], [UncheckedPattern],
                UncheckedExp, SrcLoc)
            -> TypeM (VName, [TypeParam], [Pattern], Maybe (TypeExp VName), StructType, Exp)
checkFunDef = runTermTypeM . checkFunDef'

checkFunDef' :: (Name, Maybe UncheckedTypeExp,
                 [UncheckedTypeParam], [UncheckedPattern],
                 UncheckedExp, SrcLoc)
             -> TermTypeM (VName, [TypeParam], [Pattern], Maybe (TypeExp VName), StructType, Exp)
checkFunDef' (fname, maybe_retdecl, tparams, params, body, loc) = do
  fname' <- checkName Term fname loc

  when (baseString fname' == "&&") $
    bad $ TypeError loc "The && operator may not be redefined."

  when (baseString fname' == "||") $
    bad $ TypeError loc "The || operator may not be redefined."

  bindingPatternGroup tparams (zip params $ repeat NoneInferred) $ \tparams' params' -> do
    maybe_retdecl' <-
      case maybe_retdecl of
        Just rettype -> do
          (rettype', rettype_st, ret_implicit) <- checkTypeExp rettype
          if M.null $ implicitNameMap ret_implicit
            then return $ Just (rettype', rettype_st)
            else throwError $ TypeError loc
                 "Fresh sizes may not be bound in return type."
        Nothing -> return Nothing

    body' <- checkFunBody fname body (snd <$> maybe_retdecl') loc
    (maybe_retdecl'', rettype) <- case maybe_retdecl' of
      Just (retdecl', retdecl_type) -> do
        let rettype_structural = toStructural retdecl_type
        checkReturnAlias rettype_structural params' $ typeOf body'
        return (Just retdecl', retdecl_type)
      Nothing -> return (Nothing, vacuousShapeAnnotations $ toStruct $ typeOf body')

    return (fname', tparams', params', maybe_retdecl'', rettype, body')

  where -- | Check that unique return values do not alias a
        -- non-consumed parameter.
        checkReturnAlias rettp params' =
          foldM_ (checkReturnAlias' params') S.empty . returnAliasing rettp
        checkReturnAlias' params' seen (Unique, names)
          | any (`S.member` S.map snd seen) $ S.toList names =
            bad $ UniqueReturnAliased fname loc
          | otherwise = do
            notAliasingParam params' names
            return $ seen `S.union` tag Unique names
        checkReturnAlias' _ seen (Nonunique, names)
          | any (`S.member` seen) $ S.toList $ tag Unique names =
            bad $ UniqueReturnAliased fname loc
          | otherwise = return $ seen `S.union` tag Nonunique names

        notAliasingParam params' names =
          forM_ params' $ \p ->
          let consumedNonunique p' =
                not (unique $ unInfo $ identType p') && (identName p' `S.member` names)
          in case find consumedNonunique $ S.toList $ patIdentSet p of
               Just p' ->
                 bad $ ReturnAliased fname (baseName $ identName p') loc
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
        bad $ ReturnTypeError loc fname rettype_structural $ toStructural $ typeOf body'
    Nothing -> return ()

  return body'

checkLambda :: LambdaBase NoInfo Name -> [Arg]
            -> TermTypeM Lambda
checkLambda (AnonymFun tparams params body maybe_ret NoInfo loc) args
  | length params == length args = do
      let params_with_ts = zip params $ map (Inferred . fromStruct . argType) args
      (maybe_ret', tparams', params', body') <-
        noUnique $ bindingPatternGroup tparams params_with_ts $ \tparams' params' -> do
        maybe_ret' <- maybe (pure Nothing) (fmap Just . checkTypeDecl loc) maybe_ret
        body' <- checkFunBody (nameFromString "<anonymous>") body
                 (unInfo . expandedType <$> maybe_ret') loc
        return (maybe_ret', tparams', params', body')
      let ret' = case maybe_ret' of
                   Nothing -> toStruct $ vacuousShapeAnnotations $ typeOf body'
                   Just (TypeDecl _ (Info ret)) -> ret
      (_, ret'') <- checkFuncall Nothing loc ([], map patternStructType params', ret') args
      return $ AnonymFun tparams' params' body' maybe_ret' (Info $ toStruct ret'') loc
  | otherwise = bad $ TypeError loc $ "Anonymous function defined with " ++ show (length params) ++ " parameters, but expected to take " ++ show (length args) ++ " arguments."

checkLambda (CurryFun fname curryargexps _ loc) args = do
  (curryargexps', curryargs) <- unzip <$> mapM checkArg curryargexps
  (fname', (tparams, paramtypes, rt), closure) <- lookupFunction fname (map argType $ curryargs++args) loc
  case find (unique . snd) $ zip curryargexps paramtypes of
    Just (e, _) -> bad $ CurriedConsumption fname $ srclocOf e
    _           -> return ()

  occur closure
  (paramtypes', rettype') <-
    checkFuncall Nothing loc (tparams, paramtypes, rt) (curryargs ++ args)

  return $ CurryFun fname'
    curryargexps' (Info (map (removeShapeAnnotations . (`setAliases` mempty)) paramtypes',
                         removeShapeAnnotations rettype')) loc

checkLambda (BinOpFun op NoInfo NoInfo NoInfo loc) [x_arg,y_arg] = do
  (op', (tparams, paramtypes, rt), closure) <- lookupFunction op (map argType [x_arg,y_arg]) loc
  let paramtypes' = map (fromStruct . removeShapeAnnotations) paramtypes

  occur closure
  (_, rettype') <-
    checkFuncall Nothing loc (tparams, paramtypes, rt) [x_arg,y_arg]

  case paramtypes' of
    [x_t, y_t] ->
      return $ BinOpFun op'
        (Info x_t) (Info y_t) (Info $ removeShapeAnnotations rettype') loc
    _ ->
      fail "Internal type checker error: BinOpFun got bad parameter type."

checkLambda (BinOpFun op NoInfo NoInfo NoInfo loc) args =
  bad $ ParameterMismatch (Just op) loc (Left 2) $
  map (toStructural . argType) args

checkLambda (CurryBinOpLeft binop x _ _ loc) [arg] = do
  (x', binop', ret) <- checkCurryBinOp id binop x loc arg
  return $ CurryBinOpLeft binop'
    x' (Info (typeOf x'), Info (argType arg)) (Info ret) loc

checkLambda (CurryBinOpLeft binop _ _ _ loc) args =
  bad $ ParameterMismatch (Just binop) loc (Left 1) $
  map (toStructural . argType) args

checkLambda (CurryBinOpRight binop x _ _ loc) [arg] = do
  (x', binop', ret) <- checkCurryBinOp (uncurry $ flip (,)) binop x loc arg
  return $ CurryBinOpRight binop'
    x' (Info (argType arg), Info (typeOf x')) (Info ret) loc

checkLambda (CurryBinOpRight binop _ _ _ loc) args =
  bad $ ParameterMismatch (Just binop) loc (Left 1) $
  map (toStructural . argType) args

checkCurryBinOp :: ((Arg,Arg) -> (Arg,Arg))
                -> QualName Name -> ExpBase NoInfo Name -> SrcLoc -> Arg
                -> TermTypeM (Exp, QualName VName, Type)
checkCurryBinOp arg_ordering binop x loc y_arg = do
  (x', x_arg) <- checkArg x
  let (first_arg, second_arg) = arg_ordering (x_arg, y_arg)
  (binop', fun, closure) <-
    lookupFunction binop [argType first_arg, argType second_arg] loc

  occur closure
  (_, rettype) <-
    checkFuncall Nothing loc fun [first_arg,second_arg]

  return (x', binop', removeShapeAnnotations rettype)

--- Consumption

occur :: Occurences -> TermTypeM ()
occur = tell

-- | Proclaim that we have made read-only use of the given variable.
observe :: Ident -> TermTypeM ()
observe (Ident nm (Info t) loc) =
  let als = nm `S.insert` aliases t
  in occur [observation als loc]

-- | Proclaim that we have written to the given variable.
consume :: SrcLoc -> Names VName -> TermTypeM ()
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
  where set (BoundV t)         = BoundV $ t `setUniqueness` Nonunique
        set (BoundF f closure) = BoundF f closure
        set (OverloadedF f)    = OverloadedF f
        set EqualityF          = EqualityF
        set OpaqueF            = OpaqueF
        set (WasConsumed loc)  = WasConsumed loc

onlySelfAliasing :: TermTypeM a -> TermTypeM a
onlySelfAliasing = local (\scope -> scope { scopeVtable = M.mapWithKey set $ scopeVtable scope})
  where set k (BoundV t)         = BoundV $ t `addAliases` S.intersection (S.singleton k)
        set _ (BoundF f closure) = BoundF f closure
        set _ (OverloadedF f)    = OverloadedF f
        set _ EqualityF          = EqualityF
        set _ OpaqueF            = OpaqueF
        set _ (WasConsumed loc)  = WasConsumed loc
