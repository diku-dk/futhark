{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, TupleSections #-}
-- | Facilities for type-checking Futhark terms.  Checking a term
-- requires a little more context to track uniqueness and such.
module Language.Futhark.TypeChecker.Terms
  ( checkExp
  , checkFunDef
  , checkTypeExp
  , runTermTypeM
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

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Prelude hiding (mapM)

import Language.Futhark
import Language.Futhark.TypeChecker.Monad hiding (ValBinding, BoundV, BoundF, checkQualName)
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
observation = flip Occurence HS.empty

consumption :: Names VName -> SrcLoc -> Occurence
consumption = Occurence HS.empty

nullOccurence :: Occurence -> Bool
nullOccurence occ = HS.null (observed occ) && HS.null (consumed occ)

type Occurences = [Occurence]

type UsageMap = HM.HashMap VName [Usage]

usageMap :: Occurences -> UsageMap
usageMap = foldl comb HM.empty
  where comb m (Occurence obs cons loc) =
          let m' = HS.foldl' (ins $ Observed loc) m obs
          in HS.foldl' (ins $ Consumed loc) m' cons
        ins v m k = HM.insertWith (++) k [v] m

combineOccurences :: VName -> Usage -> Usage -> Either TypeError Usage
combineOccurences _ (Observed loc) (Observed _) = Right $ Observed loc
combineOccurences name (Consumed wloc) (Observed rloc) =
  Left $ UseAfterConsume (baseName name) rloc wloc
combineOccurences name (Observed rloc) (Consumed wloc) =
  Left $ UseAfterConsume (baseName name) rloc wloc
combineOccurences name (Consumed loc1) (Consumed loc2) =
  Left $ UseAfterConsume (baseName name) (max loc1 loc2) (min loc1 loc2)

checkOccurences :: Occurences -> Either TypeError ()
checkOccurences = void . HM.traverseWithKey comb . usageMap
  where comb _    []     = Right ()
        comb name (u:us) = foldM_ (combineOccurences name) u us

allObserved :: Occurences -> Names VName
allObserved = HS.unions . map observed

allConsumed :: Occurences -> Names VName
allConsumed = HS.unions . map consumed

allOccuring :: Occurences -> Names VName
allOccuring occs = allConsumed occs <> allObserved occs

seqOccurences :: Occurences -> Occurences -> Occurences
seqOccurences occurs1 occurs2 =
  filter (not . nullOccurence) $ map filt occurs1 ++ occurs2
  where filt occ =
          occ { observed = observed occ `HS.difference` postcons }
        postcons = allConsumed occurs2

altOccurences :: Occurences -> Occurences -> Occurences
altOccurences occurs1 occurs2 =
  filter (not . nullOccurence) $ map filt1 occurs1 ++ map filt2 occurs2
  where filt1 occ =
          occ { consumed = consumed occ `HS.difference` cons2
              , observed = observed occ `HS.difference` cons2 }
        filt2 occ =
          occ { consumed = consumed occ
              , observed = observed occ `HS.difference` cons1 }
        cons1 = allConsumed occurs1
        cons2 = allConsumed occurs2

--- Scope management

data ValBinding = BoundV Type
                | BoundF FunBinding Occurences
                -- ^ The occurences is non-empty only for local functions.
                | OverloadedF [([TypeBase Rank ()],FunBinding)]
                | EqualityF
                | WasConsumed SrcLoc
                deriving (Show)

-- | Type checking happens with access to this environment.  The
-- tables will be extended during type-checking as bindings come into
-- scope.
data TermScope = TermScope { scopeVtable  :: HM.HashMap VName ValBinding
                           , scopeNameMap :: NameMap
                           } deriving (Show)

instance Monoid TermScope where
  mempty = TermScope mempty mempty
  TermScope vt1 nt1 `mappend` TermScope vt2 nt2 =
    TermScope (vt2 `HM.union` vt1) (nt2 `HM.union` nt1)

envToTermScope :: Env -> TermScope
envToTermScope env = TermScope vtable (envNameMap env)
  where vtable = HM.map valBinding $ envVtable env
        valBinding (TypeM.BoundV v) = BoundV v
        valBinding (TypeM.BoundF f) = BoundF f mempty

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
initialTermScope = TermScope initialVtable topLevelNameMap
  where initialVtable = HM.fromList $ mapMaybe addIntrinsicF $ HM.toList intrinsics

        addIntrinsicF (name, IntrinsicMonoFun ts t) =
          Just (name, BoundF (map Prim ts, Prim t) mempty)
        addIntrinsicF (name, IntrinsicPolyFun variants) =
          Just (name, OverloadedF $ map frob variants)
          where frob (pts, rt) = (map Prim pts, (map Prim pts, Prim rt))
        addIntrinsicF (name, IntrinsicEquality) =
          Just (name, EqualityF)
        addIntrinsicF _ = Nothing

instance MonadTypeChecker TermTypeM where
  bad = liftTypeM . bad
  warn loc problem = liftTypeM $ warn loc problem
  newName = liftTypeM . newName
  newID = liftTypeM . newID

  checkName space name loc = do
    (_, QualName _ name') <- checkQualName space (qualName name) loc
    return name'

  lookupType loc name = liftTypeM $ TypeM.lookupType loc name
  lookupMod loc name = liftTypeM $ TypeM.lookupMod loc name
  lookupSig loc name = liftTypeM $ TypeM.lookupSig loc name
  lookupFunctor loc name = liftTypeM $ TypeM.lookupFunctor loc name
  lookupImport loc name = liftTypeM $ TypeM.lookupImport loc name

checkQualName :: Namespace -> QualName Name -> SrcLoc -> TermTypeM (TermScope, QualName VName)
checkQualName space qn@(QualName [q] _) loc
  | nameToString q == "intrinsics" = do
      -- Check if we are referring to the magical intrinsics
      -- module.
      (_, QualName _ q') <- liftTypeM $ TypeM.checkQualName Structure (QualName [] q) loc
      if baseTag q' <= maxIntrinsicTag
        then checkIntrinsic space qn loc
        else checkReallyQualName space qn loc
checkQualName space qn@(QualName quals name) loc = do
  scope <- ask
  descend scope quals
  where descend scope []
          | Just name' <- HM.lookup (space, name) $ scopeNameMap scope =
              return (scope, QualName quals name')

        descend _ _ =
          checkReallyQualName space qn loc

checkIntrinsic :: Namespace -> QualName Name -> SrcLoc -> TermTypeM (TermScope, QualName VName)
checkIntrinsic space qn@(QualName _ name) loc
  | Just v <- HM.lookup (space, name) intrinsicsNameMap = do
      scope <- ask
      return (scope, QualName [nameFromString "intrinsics"] v)
  | otherwise =
      bad $ UnknownVariableError space qn loc

checkReallyQualName :: Namespace -> QualName Name -> SrcLoc -> TermTypeM (TermScope, QualName VName)
checkReallyQualName space qn loc = do
  (env, name') <- liftTypeM $ TypeM.checkQualName space qn loc
  return (envToTermScope env, name')

-- | In a few rare cases (overloaded builtin functions), the type of
-- the parameters actually matters.
lookupFunction :: QualName Name -> [Type] -> SrcLoc -> TermTypeM (QualName VName, FunBinding, Occurences)
lookupFunction qn argtypes loc = do
  (scope, qn'@(QualName _ name)) <- checkQualName Term qn loc
  case HM.lookup name $ scopeVtable scope of
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
    Just EqualityF
      | [t1,t2] <- argtypes,
        concreteType t1,
        concreteType t2,
        t1 == t2 ->
          return (qn', (map (vacuousShapeAnnotations . toStruct) [t1, t2],
                        Prim Bool),
                       mempty)
      | otherwise ->
          bad $ TypeError loc $ "Equality not defined for arguments of types " ++
          intercalate ", " (map pretty argtypes)

lookupVar :: QualName Name -> SrcLoc -> TermTypeM (QualName VName, Type)
lookupVar qn loc = do
  (scope, qn'@(QualName _ name)) <- checkQualName Term qn loc
  case HM.lookup name $ scopeVtable scope of
    Nothing -> bad $ UnknownVariableError Term qn loc
    Just (BoundV t) | "_" `isPrefixOf` pretty name -> bad $ UnderscoreUse loc qn
                    | otherwise -> return (qn', t)
    Just BoundF{} -> bad $ FunctionIsNotValue loc qn
    Just EqualityF -> bad $ FunctionIsNotValue loc qn
    Just OverloadedF{} -> bad $ FunctionIsNotValue loc qn
    Just (WasConsumed wloc) -> bad $ UseAfterConsume (baseName name) loc wloc

bindSpaced :: [(Namespace, Name)] -> TermTypeM a -> TermTypeM a
bindSpaced names body = do
  names' <- mapM (newID . snd) names
  let mapping = HM.fromList (zip names names')
  bindNameMap mapping body

bindNameMap :: NameMap -> TermTypeM a -> TermTypeM a
bindNameMap m = local $ \scope -> scope { scopeNameMap = m <> scopeNameMap scope }

--- Basic checking

-- | @t1 `unifyTypes` t2@ attempts to unify @t2@ and @t2@.  If
-- unification cannot happen, 'Nothing' is returned, otherwise a type
-- that combines the aliasing of @t1@ and @t2@ is returned.  The
-- uniqueness of the resulting type will be the least of the
-- uniqueness of @t1@ and @t2@.
unifyTypes :: Monoid (as vn) =>
              TypeBase Rank (as vn)
           -> TypeBase Rank (as vn)
           -> Maybe (TypeBase Rank (as vn))
unifyTypes (Prim t1) (Prim t2)
  | t1 == t2  = Just $ Prim t1
  | otherwise = Nothing
unifyTypes (TypeVar t1) (TypeVar t2)
  | t1 == t2  = Just $ TypeVar t1
  | otherwise = Nothing
unifyTypes (Array at1) (Array at2) =
  Array <$> unifyArrayTypes at1 at2
unifyTypes (Tuple ts1) (Tuple ts2)
  | length ts1 == length ts2 =
    Tuple <$> zipWithM unifyTypes ts1 ts2
unifyTypes _ _ = Nothing

unifyArrayTypes :: Monoid (as vn) =>
                   ArrayTypeBase Rank (as vn)
                -> ArrayTypeBase Rank (as vn)
                -> Maybe (ArrayTypeBase Rank (as vn))
unifyArrayTypes (PrimArray bt1 shape1 u1 als1) (PrimArray bt2 shape2 u2 als2)
  | shapeRank shape1 == shapeRank shape2, bt1 == bt2 =
    Just $ PrimArray bt1 shape1 (u1 <> u2) (als1 <> als2)
unifyArrayTypes (PolyArray bt1 shape1 u1 als1) (PolyArray bt2 shape2 u2 als2)
  | shapeRank shape1 == shapeRank shape2, bt1 == bt2 =
    Just $ PolyArray bt1 shape1 (u1 <> u2) (als1 <> als2)
unifyArrayTypes (TupleArray et1 shape1 u1) (TupleArray et2 shape2 u2)
  | shapeRank shape1 == shapeRank shape2 =
    TupleArray <$> zipWithM unifyTupleArrayElemTypes et1 et2 <*>
    pure shape1 <*> pure (u1 <> u2)
unifyArrayTypes _ _ =
  Nothing

unifyTupleArrayElemTypes :: Monoid (as vn) =>
                            TupleArrayElemTypeBase Rank (as vn)
                         -> TupleArrayElemTypeBase Rank (as vn)
                         -> Maybe (TupleArrayElemTypeBase Rank (as vn))
unifyTupleArrayElemTypes (PrimArrayElem bt1 als1 u1) (PrimArrayElem bt2 als2 u2)
  | bt1 == bt2 = Just $ PrimArrayElem bt1 (als1 <> als2) (u1 <> u2)
  | otherwise  = Nothing
unifyTupleArrayElemTypes (PolyArrayElem bt1 als1 u1) (PolyArrayElem bt2 als2 u2)
  | bt1 == bt2 = Just $ PolyArrayElem bt1 (als1 <> als2) (u1 <> u2)
  | otherwise  = Nothing
unifyTupleArrayElemTypes (ArrayArrayElem at1) (ArrayArrayElem at2) =
  ArrayArrayElem <$> unifyArrayTypes at1 at2
unifyTupleArrayElemTypes (TupleArrayElem ts1) (TupleArrayElem ts2) =
  TupleArrayElem <$> zipWithM unifyTupleArrayElemTypes ts1 ts2
unifyTupleArrayElemTypes _ _ =
  Nothing

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
          let inedges = HS.toList $ aliases tp
              update (BoundV tp')
              -- If 'name' is tuple-typed, don't alias the components
              -- to 'name', because tuples have no identity beyond
              -- their components.
                | Tuple _ <- tp = BoundV tp'
                | otherwise     = BoundV (tp' `addAliases` HS.insert name)
              update b = b
          in scope { scopeVtable = HM.insert name (BoundV tp) $
                                   adjustSeveral update inedges $
                                   scopeVtable scope
                   }

        adjustSeveral f = flip $ foldl $ flip $ HM.adjust f

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
                names = HS.fromList $ map identName bnds
                divide s = (s `HS.intersection` names, s `HS.difference` names)

-- | A hack that also binds the names in the name map.  This is useful
-- if the same names are visible in two entirely different expressions
-- (e.g. for do loops).
bindingAlsoNames :: [Ident] -> TermTypeM a -> TermTypeM a
bindingAlsoNames idents body = do
  let varnames = map ((Term,) . baseName . identName) idents
      substs   = map identName idents
  bindNameMap (HM.fromList (zip varnames substs)) $
    binding idents body

bindingIdent :: IdentBase NoInfo Name -> Type -> (Ident -> TermTypeM a)
             -> TermTypeM a
bindingIdent (Ident v NoInfo vloc) t m =
  bindSpaced [(Term, v)] $ do
    v' <- checkName Term v vloc
    let ident = Ident v' (Info t) vloc
    binding [ident] $ m ident

bindingPatterns :: [(PatternBase NoInfo Name, InferredType)]
               -> ([Pattern] -> TermTypeM a) -> TermTypeM a
bindingPatterns ps m = do
  names <- checkForDuplicateNames $ map fst ps
  bindSpaced names $ do
    ps' <- mapM (uncurry checkPattern) ps
    binding (HS.toList $ HS.unions $ map patIdentSet ps') $ do
      -- Perform an observation of every declared dimension.  This
      -- prevents unused-name warnings for otherwise unused dimensions.
      mapM_ observe $ concatMap patternDims ps'
      m ps'

bindingPattern :: PatternBase NoInfo Name -> InferredType
               -> (Pattern -> TermTypeM a) -> TermTypeM a
bindingPattern p t m = do
  names <- checkForDuplicateNames [p]
  bindSpaced names $ do
    p' <- checkPattern p t
    binding (HS.toList $ patIdentSet p') $ do
      -- Perform an observation of every declared dimension.  This
      -- prevents unused-name warnings for otherwise unused dimensions.
      mapM_ observe $ patternDims p'
      m p'

patternDims :: Pattern -> [Ident]
patternDims (TuplePattern pats _) = concatMap patternDims pats
patternDims (PatternAscription p t) =
  patternDims p <> mapMaybe (dimIdent (srclocOf p)) (nestedDims' (declaredType t))
  where dimIdent _ AnyDim            = Nothing
        dimIdent _ (ConstDim _)      = Nothing
        dimIdent loc (NamedDim name) = Just $ Ident name (Info (Prim (Signed Int32))) loc
patternDims _ = []

--- Main checkers

checkExp :: UncheckedExp -> TermTypeM Exp

checkExp (Literal val loc) =
  return $ Literal val loc

checkExp (TupLit es loc) =
  TupLit <$> mapM checkExp es <*> pure loc

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
  decl' <- checkTypeDecl decl
  return $ Empty decl' loc

checkExp (BinOp op (e1,_) (e2,_) NoInfo loc) = do
  (e1', e1_arg) <- checkArg e1
  (e2', e2_arg) <- checkArg e2

  (op', (paramtypes, ftype), closure) <-
    lookupFunction op (map argType [e1_arg,e2_arg]) loc

  case paramtypes of
    [e1_pt, e2_pt] -> do
      let rettype' = returnType (removeShapeAnnotations ftype)
                     (map diet paramtypes) (map typeOf [e1', e2'])

      occur closure
      checkFuncall (Just op) loc paramtypes [e1_arg, e2_arg]

      return $ BinOp op' (e1', diet e1_pt) (e2', diet e2_pt) (Info rettype') loc
    _ ->
      fail $ "Internal typechecker error: got invalid parameter types back from type checking binary operator " ++ pretty op

checkExp (TupleProject i e NoInfo loc) = do
  e' <- checkExp e
  case typeOf e' of
    Tuple ts | t:_ <- drop i ts -> return $ TupleProject i e' (Info t) loc
    _ -> bad $ InvalidField loc (typeOf e') (show i)

checkExp (If e1 e2 e3 _ pos) =
  sequentially (require [Prim Bool] =<< checkExp e1) $ \e1' _ -> do
  ((e2', e3'), dflow) <- tapOccurences $ checkExp e2 `alternative` checkExp e3
  brancht <- unifyExpTypes e2' e3'
  let t' = addAliases brancht (`HS.difference` allConsumed dflow)
  return $ If e1' e2' e3' (Info t') pos

checkExp (Parens e loc) =
  Parens <$> checkExp e <*> pure loc

checkExp (Var qn NoInfo loc) = do
  (qn'@(QualName _ name'), t) <- lookupVar qn loc
  observe $ Ident name' (Info t) loc
  return $ Var qn' (Info t) loc

checkExp (Negate arg loc) = do
  arg' <- require anyNumberType =<< checkExp arg
  return $ Negate arg' loc

checkExp (Apply fname args _ loc) = do
  (args', argflows) <- unzip <$> mapM (\(arg,_) -> checkArg arg) args
  (fname', (paramtypes, ftype), closure) <-
    lookupFunction fname (map argType argflows) loc

  let rettype' = returnType (removeShapeAnnotations ftype)
                 (map diet paramtypes) (map typeOf args')

  occur closure
  checkFuncall (Just fname) loc paramtypes argflows

  return $ Apply fname' (zip args' $ map diet paramtypes) (Info rettype') loc

checkExp (LetPat pat e body pos) =
  sequentially (checkExp e) $ \e' _ ->
  -- Not technically an ascription, but we want the pattern to have
  -- exactly the type of 'e'.
  bindingPattern pat (Ascribed $ typeOf e') $ \pat' -> do
    when (hasShapeDecl pat') $
      bad $ TypeError (srclocOf pat')
      "Type ascription for let-binding may not have shape declarations."
    body' <- checkExp body
    return $ LetPat pat' e' body' pos
  where -- HACK: Until we figure out what they should mean, shape
        -- declarations are banned in let binding type ascriptions.
    hasShapeDecl (TuplePattern ps _) = any hasShapeDecl ps
    hasShapeDecl (PatternParens p _) = hasShapeDecl p
    hasShapeDecl Id{} = False
    hasShapeDecl Wildcard{} = False
    hasShapeDecl (PatternAscription p td) =
      hasShapeDecl p ||
      hasShapeDecl' (unInfo $ expandedType td)

    hasShapeDecl' Prim{} = False
    hasShapeDecl' TypeVar{} = False
    hasShapeDecl' (Tuple ts) = any hasShapeDecl' ts
    hasShapeDecl' (Array at) = arrayElemHasShapeDecl at

    arrayElemHasShapeDecl (PrimArray _ shape _ _) =
      any (/=AnyDim) (shapeDims shape)
    arrayElemHasShapeDecl (PolyArray _ shape _ _) =
      any (/=AnyDim) (shapeDims shape)
    arrayElemHasShapeDecl (TupleArray ts shape _) =
      any (/=AnyDim) (shapeDims shape) ||
      any tupleArrayElemHasShapeDecl ts

    tupleArrayElemHasShapeDecl (ArrayArrayElem at) =
      arrayElemHasShapeDecl at
    tupleArrayElemHasShapeDecl (TupleArrayElem ts) =
      any tupleArrayElemHasShapeDecl ts
    tupleArrayElemHasShapeDecl PrimArrayElem{} =
      False
    tupleArrayElemHasShapeDecl PolyArrayElem{} =
      False

checkExp (LetFun name (params, maybe_retdecl, NoInfo, e) body loc) =
  bindSpaced [(Term, name)] $
  sequentially (checkFunDef (name, maybe_retdecl, params, e, loc)) $
    \(name', params', maybe_retdecl', rettype, e') closure -> do

    let paramType = toStruct . vacuousShapeAnnotations . patternType
        entry = BoundF (map paramType params', rettype) closure
        bindF scope = scope { scopeVtable = HM.insert name' entry $ scopeVtable scope }
    body' <- local bindF $ checkExp body

    return $ LetFun name' (params', maybe_retdecl', Info rettype, e') body' loc

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
        when (identName src' `HS.member` aliases (typeOf ve')) $
          bad $ BadLetWithValue pos

        bindingIdent dest (unInfo (identType src') `setAliases` HS.empty) $ \dest' -> do
          body' <- consuming src' $ checkExp body
          return $ LetWith dest' src' idxes' ve' body' pos
  where isFix DimFix{} = True
        isFix _        = False

checkExp (Index e idxes pos) = do
  e' <- checkExp e
  let vt = typeOf e'
  when (arrayRank vt < length idxes) $
    bad $ IndexingError (arrayRank vt) (length idxes) pos
  idxes' <- mapM checkDimIndex idxes
  return $ Index e' idxes' pos

checkExp (Iota e pos) = do
  e' <- require anyIntType =<< checkExp e
  return $ Iota e' pos

checkExp (Shape e loc) = do
  e' <- checkExp e
  case typeOf e' of
    t | arrayRank t > 0 -> return $ Shape e' loc
      | otherwise ->
          bad $ TypeError loc
          $ "Argument to shape must be an array, not of type " ++ pretty (typeOf e') ++ "."

checkExp (Replicate countexp valexp pos) = do
  countexp' <- require anyIntType =<< checkExp countexp
  valexp' <- checkExp valexp
  return $ Replicate countexp' valexp' pos

checkExp (Reshape shapeexp arrexp loc) = do
  shapeexp' <- checkExp shapeexp
  arrexp' <- checkExp arrexp

  case typeOf shapeexp' of
    Tuple ts | all ((`elem` anyIntType) . toStruct) ts -> return ()
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

checkExp (Transpose arrexp pos) = do
  arrexp' <- checkExp arrexp
  when (arrayRank (typeOf arrexp') /= 2) $
    bad $ TypeError pos "Argument to transpose is not two-dimensional array."
  return $ Transpose arrexp' pos

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
    Array (TupleArray ets shape u) ->
      let componentType et =
            let et' = tupleArrayElemToType et
                u' = max u $ tupleArrayElemUniqueness et
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
  let arr_aliasses = HS.toList $ aliases $ typeOf arr'
  let usages = usageMap dflow
  when (any (`HM.member` usages) arr_aliasses) $
     bad $ TypeError pos "Stream with input array used inside lambda."

  -- (i) properly check the lambda on its parameter and
  --(ii) make some fake arguments, which do not alias `arr', and
  --     check that aliases of `arr' are not used inside lam.
  -- check that the result type of lambda matches the accumulator part
  case macctup of
    Just (acc',_) ->
      case lambdaReturnType lam' of
        Tuple (acctp:_) ->
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
    Tuple ts | all (==(Prim $ Signed Int32)) ts -> return ()
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

checkExp (Copy e pos) = do
  e' <- checkExp e
  return $ Copy e' pos

checkExp (DoLoop mergepat mergeexp form loopbody letbody loc) = do
  (mergeexp', mergeflow) <- collectOccurences $ checkExp mergeexp

  -- First we do a basic check of the loop body to figure out which of
  -- the merge parameters are being consumed.  For this, we first need
  -- to check the merge pattern, which requires the (initial) merge
  -- expression.
  --
  -- Play a little with occurences to ensure it does not look like
  -- none of the merge variables are being used.
  (((mergepat', form', loopbody'), bodyflow), freeflow) <-
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
          binding [i'] $ noUnique $ collectOccurences $
            bindingPattern mergepat (Ascribed $ typeOf mergeexp' `setAliases` mempty) $ \mergepat' ->
            onlySelfAliasing $ tapOccurences $ do
            loopbody' <- checkExp loopbody
            return (mergepat',
                    For dir lboundexp' i' uboundexp',
                    loopbody')
      While cond ->
        noUnique $ collectOccurences $
        bindingPattern mergepat (Ascribed $ typeOf mergeexp' `setAliases` mempty) $ \mergepat' ->
        onlySelfAliasing $ tapOccurences $
        sequentially (require [Prim Bool] =<< checkExp cond) $ \cond' _ -> do
          loopbody' <- checkExp loopbody
          return (mergepat',
                  While cond',
                  loopbody')

  let consumed_merge = HS.map identName (patIdentSet mergepat') `HS.intersection`
                       allConsumed bodyflow
      uniquePat (Wildcard (Info t) wloc) =
        Wildcard (Info $ t `setUniqueness` Nonunique) wloc
      uniquePat (PatternParens p ploc) =
        PatternParens (uniquePat p) ploc
      uniquePat (Id (Ident name (Info t) iloc))
        | name `HS.member` consumed_merge =
            let t' = t `setUniqueness` Unique `setAliases` mempty
            in Id (Ident name (Info t') iloc)
        | otherwise =
            let t' = case t of Tuple{} -> t
                               _       -> t `setUniqueness` Nonunique
            in Id (Ident name (Info t') iloc)
      uniquePat (TuplePattern pats ploc) =
        TuplePattern (map uniquePat pats) ploc
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
  bound_outside <- asks $ HS.fromList . HM.keys . scopeVtable
  let checkMergeReturn (Id ident) t
        | unique $ unInfo $ identType ident,
          v:_ <- HS.toList $ aliases t `HS.intersection` bound_outside =
            lift $ bad $ TypeError loc $ "Loop return value corresponding to merge parameter " ++
            pretty (identName ident) ++ " aliases " ++ pretty v ++ "."
        | otherwise = do
            (cons,obs) <- get
            unless (HS.null $ aliases t `HS.intersection` cons) $
              lift $ bad $ TypeError loc $ "Loop return value for merge parameter " ++
              pretty (identName ident) ++ " aliases other consumed merge parameter."
            when (unique (unInfo $ identType ident) &&
                  not (HS.null (aliases t `HS.intersection` (cons<>obs)))) $
              lift $ bad $ TypeError loc $ "Loop return value for consuming merge parameter " ++
              pretty (identName ident) ++ " aliases previously returned value." ++ show (aliases t, cons, obs)
            if unique (unInfo $ identType ident)
              then put (cons<>aliases t, obs)
              else put (cons, obs<>aliases t)
      checkMergeReturn (TuplePattern pats _) (Tuple ts) =
        zipWithM_ checkMergeReturn pats ts
      checkMergeReturn _ _ =
        return ()
  evalStateT (checkMergeReturn mergepat'' $ typeOf loopbody') (mempty, mempty)

  let consumeMerge (Id (Ident _ (Info pt) ploc)) mt
        | unique pt = consume ploc $ aliases mt
      consumeMerge (TuplePattern pats _) (Tuple ts) =
        zipWithM_ consumeMerge pats ts
      consumeMerge _ _ =
        return ()
  ((), merge_consume) <-
    collectOccurences $ consumeMerge mergepat'' $ typeOf mergeexp'

  let loopOccur = do
        occur $ mergeflow `seqOccurences` merge_consume `seqOccurences` freeflow
        mapM_ observe $ HS.toList $ patIdentSet mergepat''

  bindingAlsoNames (HS.toList $ patIdentSet mergepat'') $ do
    -- It is OK for merge parameters to not occur here, because they
    -- might be useful for the loop body.
    letbody' <- sequentially loopOccur $ \_ _ -> checkExp letbody
    return $ DoLoop mergepat'' mergeexp'
                    form'
                    loopbody' letbody' loc

checkExp (Write i v a pos) = do
  i' <- checkExp i
  v' <- checkExp v
  (a', aflow) <- collectOccurences . checkExp $ a

  -- Check indexes type.
  case typeOf i' of
    Array (PrimArray (Signed Int32) (Rank 1) _ _) ->
      return ()
    _ -> bad $ TypeError pos
         "A write index array must consist of signed 32-bit ints only."

  -- Check that values arrays and I/O arrays have the same structure.
  void $ unifyExpTypes v' a'

  -- Check that all I/O arrays are properly unique.
  let at = typeOf a'
  if unique at
    then occur $ aflow `seqOccurences` [consumption (aliases at) pos]
    else bad $ TypeError pos $ "Write source '" ++
         pretty a' ++
         "' has type " ++ pretty at ++
         ", which is not unique."

  return (Write i' v' a' pos)

checkSOACArrayArg :: ExpBase NoInfo Name
                  -> TermTypeM (Exp, Arg)
checkSOACArrayArg e = do
  (e', (t, dflow, argloc)) <- checkArg e
  case peelArray 1 t of
    Nothing -> bad $ TypeError argloc "SOAC argument is not an array"
    Just rt -> return (e', (rt, dflow, argloc))

checkIdent :: IdentBase NoInfo Name -> TermTypeM Ident
checkIdent (Ident name _ pos) = do
  (QualName _ name', vt) <- lookupVar (qualName name) pos
  return $ Ident name' (Info vt) pos

data InferredType = NoneInferred
                  | Inferred Type
                  | Ascribed Type

checkPattern :: PatternBase NoInfo Name -> InferredType
             -> TermTypeM Pattern
checkPattern (PatternParens p loc) t =
  PatternParens <$> checkPattern p t <*> pure loc
checkPattern (Id (Ident name NoInfo loc)) (Inferred t) = do
  name' <- checkName Term name loc
  let t' = typeOf $ Var (qualName name') (Info t) loc
  return $ Id $ Ident name' (Info $ t' `setUniqueness` Nonunique) loc
checkPattern (Id (Ident name NoInfo loc)) (Ascribed t) = do
  name' <- checkName Term name loc
  let t' = typeOf $ Var (qualName name') (Info t) loc
  return $ Id $ Ident name' (Info t') loc
checkPattern (Wildcard _ loc) (Inferred t) =
  return $ Wildcard (Info $ t `setUniqueness` Nonunique) loc
checkPattern (Wildcard _ loc) (Ascribed t) =
  return $ Wildcard (Info $ t `setUniqueness` Nonunique) loc
checkPattern (TuplePattern ps loc) (Inferred (Tuple ts)) | length ts == length ps =
  TuplePattern <$> zipWithM checkPattern ps (map Inferred ts) <*> pure loc
checkPattern (TuplePattern ps loc) (Ascribed (Tuple ts)) | length ts == length ps =
  TuplePattern <$> zipWithM checkPattern ps (map Ascribed ts) <*> pure loc
checkPattern p@TuplePattern{} (Inferred t) =
  bad $ TypeError (srclocOf p) $ "Pattern " ++ pretty p ++ " cannot match " ++ pretty t
checkPattern p@TuplePattern{} (Ascribed t) =
  bad $ TypeError (srclocOf p) $ "Pattern " ++ pretty p ++ " cannot match " ++ pretty t
checkPattern (TuplePattern ps loc) NoneInferred =
  TuplePattern <$> mapM (`checkPattern` NoneInferred) ps <*> pure loc
checkPattern fullp@(PatternAscription p td) maybe_outer_t = do
  td' <- checkBindingTypeDecl td
  let maybe_outer_t' = case maybe_outer_t of Inferred t -> Just t
                                             Ascribed t -> Just t
                                             NoneInferred -> Nothing
      t' = fromStruct (removeShapeAnnotations $ unInfo $ expandedType td')
           `addAliases` (<> maybe mempty aliases maybe_outer_t')
  case maybe_outer_t' of
    Just outer_t
      | not (outer_t `subtypeOf` t') ->
          bad $ InvalidPatternError fullp (toStructural outer_t) Nothing $ srclocOf p
    _ -> PatternAscription <$> checkPattern p (Ascribed t') <*> pure td'
checkPattern p NoneInferred =
  bad $ TypeError (srclocOf p) $ "Cannot determine type of " ++ pretty p

-- | Check for duplication of names inside a binding group.
-- Duplication of shape names are permitted, but only if they are not
-- also bound as values.
checkForDuplicateNames :: [PatternBase NoInfo Name] -> TermTypeM [(Namespace, Name)]
checkForDuplicateNames = fmap toNames . flip execStateT mempty . mapM_ check
  where check (Id v) = seeing BoundAsVar (identName v) (srclocOf v)
        check (PatternParens p _) = check p
        check Wildcard{} = return ()
        check (TuplePattern ps _) = mapM_ check ps
        check (PatternAscription p t) = do
          check p
          mapM_ (checkDimDecl $ srclocOf p) $ nestedDims' $ declaredType t

        seeing b name vloc = do
          seen <- get
          case (b, HM.lookup name seen) of
            (_, Just (BoundAsVar, loc)) ->
              lift $ bad $ DupPatternError name vloc loc
            (BoundAsVar, Just (BoundAsDim, loc)) ->
              lift $ bad $ DupPatternError name vloc loc
            _ -> modify $ HM.insert name (b, vloc)

        checkDimDecl _ AnyDim = return ()
        checkDimDecl _ ConstDim{} = return ()
        checkDimDecl loc (NamedDim v) = seeing BoundAsDim v loc

        toNames = map pairUp . HM.toList
        pairUp (name, _) = (Term, name)


data Bindage = BoundAsDim | BoundAsVar

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

validApply :: [StructTypeBase VName] -> [Type] -> Bool
validApply expected got =
  length got == length expected &&
  and (zipWith subtypeOf (map toStructural got) (map toStructural expected))

type Arg = (Type, Occurences, SrcLoc)

argType :: Arg -> Type
argType (t, _, _) = t

checkArg :: ExpBase NoInfo Name -> TermTypeM (Exp, Arg)
checkArg arg = do
  (arg', dflow) <- collectOccurences $ checkExp arg
  return (arg', (typeOf arg', dflow, srclocOf arg'))

checkFuncall :: Maybe (QualName Name) -> SrcLoc
             -> [StructType] -> [Arg]
             -> TermTypeM ()
checkFuncall fname loc paramtypes args = do
  let argts = map argType args

  unless (validApply paramtypes argts) $
    bad $ ParameterMismatch fname loc
          (Right $ map toStructural paramtypes) (map toStructural argts)

  forM_ (zip (map diet paramtypes) args) $ \(d, (t, dflow, argloc)) -> do
    maybeCheckOccurences dflow
    let occurs = consumeArg argloc t d
    occur $ dflow `seqOccurences` occurs

consumeArg :: SrcLoc -> Type -> Diet -> [Occurence]
consumeArg loc (Tuple ets) (TupleDiet ds) =
  concat $ zipWith (consumeArg loc) ets ds
consumeArg loc at Consume = [consumption (aliases at) loc]
consumeArg loc at _       = [observation (aliases at) loc]

checkFunDef :: (Name, Maybe UncheckedTypeExp, [UncheckedPattern], UncheckedExp, SrcLoc)
            -> TermTypeM (VName, [Pattern], Maybe (TypeExp VName), StructType, Exp)
checkFunDef (fname, maybe_retdecl, params, body, loc) = do
  fname' <- checkName Term fname loc

  when (baseString fname' == "&&") $
    bad $ TypeError loc "The && operator may not be redefined."

  when (baseString fname' == "||") $
    bad $ TypeError loc "The || operator may not be redefined."

  bindingPatterns (zip params $ repeat NoneInferred) $ \params' -> do
    maybe_retdecl' <- case maybe_retdecl of
                        Just rettype -> Just <$> checkTypeExp rettype
                        Nothing      -> return Nothing

    body' <- checkFunBody fname body (snd <$> maybe_retdecl') loc
    (maybe_retdecl'', rettype) <- case maybe_retdecl' of
      Just (retdecl', retdecl_type) -> do
        let rettype_structural = toStructural retdecl_type
        checkReturnAlias rettype_structural params' $ typeOf body'
        return (Just retdecl', retdecl_type)
      Nothing -> return (Nothing, vacuousShapeAnnotations $ toStruct $ typeOf body')

    return (fname', params', maybe_retdecl'', rettype, body')

  where -- | Check that unique return values do not alias a
        -- non-consumed parameter.
        checkReturnAlias rettp params' =
          foldM_ (checkReturnAlias' params') HS.empty . returnAliasing rettp
        checkReturnAlias' params' seen (Unique, names)
          | any (`HS.member` HS.map snd seen) $ HS.toList names =
            bad $ UniqueReturnAliased fname loc
          | otherwise = do
            notAliasingParam params' names
            return $ seen `HS.union` tag Unique names
        checkReturnAlias' _ seen (Nonunique, names)
          | any (`HS.member` seen) $ HS.toList $ tag Unique names =
            bad $ UniqueReturnAliased fname loc
          | otherwise = return $ seen `HS.union` tag Nonunique names

        notAliasingParam params' names =
          forM_ params' $ \p ->
          let consumedNonunique p' =
                not (unique $ unInfo $ identType p') && (identName p' `HS.member` names)
          in case find consumedNonunique $ HS.toList $ patIdentSet p of
               Just p' ->
                 bad $ ReturnAliased fname (baseName $ identName p') loc
               Nothing ->
                 return ()

        tag u = HS.map $ \name -> (u, name)

        returnAliasing (Tuple ets1) (Tuple ets2) =
          concat $ zipWith returnAliasing ets1 ets2
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
checkLambda (AnonymFun params body maybe_ret NoInfo loc) args
  | length params == length args = do
      let params_with_ts = zip params $ map (Inferred . fromStruct . argType) args
      (maybe_ret', params', body') <- noUnique $ bindingPatterns params_with_ts $ \params' -> do
        maybe_ret' <- maybe (pure Nothing) (fmap Just . checkTypeDecl) maybe_ret
        body' <- checkFunBody (nameFromString "<anonymous>") body
                 (unInfo . expandedType <$> maybe_ret') loc
        return (maybe_ret', params', body')
      checkFuncall Nothing loc (map patternStructType params') args
      let ret' = case maybe_ret' of
                   Nothing -> toStruct $ vacuousShapeAnnotations $ typeOf body'
                   Just (TypeDecl _ (Info ret)) -> ret
      return $ AnonymFun params' body' maybe_ret' (Info ret') loc
  | otherwise = bad $ TypeError loc $ "Anonymous function defined with " ++ show (length params) ++ " parameters, but expected to take " ++ show (length args) ++ " arguments."

checkLambda (CurryFun fname curryargexps _ loc) args = do
  (curryargexps', curryargs) <- unzip <$> mapM checkArg curryargexps
  (fname', (paramtypes, rt), closure) <- lookupFunction fname (map argType $ curryargs++args) loc
  let rettype' = fromStruct $ removeShapeAnnotations rt
      paramtypes' = map (fromStruct . removeShapeAnnotations) paramtypes
  case find (unique . snd) $ zip curryargexps paramtypes of
    Just (e, _) -> bad $ CurriedConsumption fname $ srclocOf e
    _           -> return ()

  occur closure
  checkFuncall Nothing loc paramtypes $ curryargs ++ args

  return $ CurryFun fname' curryargexps' (Info (paramtypes', rettype')) loc

checkLambda (BinOpFun op NoInfo NoInfo NoInfo loc) [x_arg,y_arg] = do
  (op', (paramtypes, rt), closure) <- lookupFunction op (map argType [x_arg,y_arg]) loc
  let rettype' = fromStruct $ removeShapeAnnotations rt
      paramtypes' = map (fromStruct . removeShapeAnnotations) paramtypes

  occur closure
  checkFuncall Nothing loc paramtypes [x_arg,y_arg]

  case paramtypes' of
    [x_t, y_t] ->
      return $ BinOpFun op' (Info x_t) (Info y_t) (Info rettype') loc
    _ ->
      fail "Internal type checker error: BinOpFun got bad parameter type."

checkLambda (BinOpFun op NoInfo NoInfo NoInfo loc) args =
  bad $ ParameterMismatch (Just op) loc (Left 2) $
  map (toStructural . argType) args

checkLambda (CurryBinOpLeft binop x _ _ loc) [arg] = do
  (x', binop', ret) <- checkCurryBinOp id binop x loc arg
  return $ CurryBinOpLeft binop' x' (Info (typeOf x'), Info (argType arg)) (Info ret) loc

checkLambda (CurryBinOpLeft binop _ _ _ loc) args =
  bad $ ParameterMismatch (Just binop) loc (Left 1) $
  map (toStructural . argType) args

checkLambda (CurryBinOpRight binop x _ _ loc) [arg] = do
  (x', binop', ret) <- checkCurryBinOp (uncurry $ flip (,)) binop x loc arg
  return $ CurryBinOpRight binop' x' (Info (argType arg), Info (typeOf x')) (Info ret) loc

checkLambda (CurryBinOpRight binop _ _ _ loc) args =
  bad $ ParameterMismatch (Just binop) loc (Left 1) $
  map (toStructural . argType) args

checkCurryBinOp :: ((Arg,Arg) -> (Arg,Arg))
                -> QualName Name -> ExpBase NoInfo Name -> SrcLoc -> Arg
                -> TermTypeM (Exp, QualName VName, Type)
checkCurryBinOp arg_ordering binop x loc y_arg = do
  (x', x_arg) <- checkArg x
  let (first_arg, second_arg) = arg_ordering (x_arg, y_arg)
  (binop', (paramtypes, ret), closure) <-
    lookupFunction binop [argType first_arg, argType second_arg] loc

  occur closure
  checkFuncall Nothing loc paramtypes [first_arg,second_arg]

  return (x', binop', fromStruct $ removeShapeAnnotations ret)

checkDim :: SrcLoc -> DimDecl Name -> TermTypeM (DimDecl VName)
checkDim _ AnyDim =
  return AnyDim
checkDim _ (ConstDim k) =
  return $ ConstDim k
checkDim loc (NamedDim name) = do
  (QualName _ name', t) <- lookupVar (qualName name) loc
  observe $ Ident name' (Info (Prim (Signed Int32))) loc
  case t of
    Prim (Signed Int32) -> return $ NamedDim name'
    _                   -> bad $ DimensionNotInteger loc name

checkTypeExp :: TypeExp Name -> TermTypeM (TypeExp VName, StructType)
checkTypeExp = expandType checkDim

checkTypeExpBindingDims :: TypeExp Name -> TermTypeM (TypeExp VName, StructType)
checkTypeExpBindingDims = expandType checkBindingDim
  where checkBindingDim _ AnyDim = return AnyDim
        checkBindingDim _ (ConstDim k) = return $ ConstDim k
        checkBindingDim loc (NamedDim name) = NamedDim <$> checkName Term name loc

checkTypeDecl :: TypeDeclBase NoInfo Name -> TermTypeM (TypeDeclBase Info VName)
checkTypeDecl (TypeDecl t NoInfo) = do
  (t', st) <- checkTypeExp t
  return $ TypeDecl t' $ Info st

checkBindingTypeDecl :: TypeDeclBase NoInfo Name -> TermTypeM (TypeDeclBase Info VName)
checkBindingTypeDecl (TypeDecl t NoInfo) = do
  (t', st) <- checkTypeExpBindingDims t
  return $ TypeDecl t' $ Info st

--- Consumption

occur :: Occurences -> TermTypeM ()
occur = tell

-- | Proclaim that we have made read-only use of the given variable.
observe :: Ident -> TermTypeM ()
observe (Ident nm (Info t) loc) =
  let als = nm `HS.insert` aliases t
  in occur [observation als loc]

-- | Proclaim that we have written to the given variable.
consume :: SrcLoc -> Names VName -> TermTypeM ()
consume loc als = occur [consumption als loc]

-- | Proclaim that we have written to the given variable, and mark
-- accesses to it and all of its aliases as invalid inside the given
-- computation.
consuming :: Ident -> TermTypeM a -> TermTypeM a
consuming (Ident name (Info t) loc) m = do
  consume loc $ name `HS.insert` aliases t
  local consume' m
  where consume' scope =
          scope { scopeVtable = HM.insert name (WasConsumed loc) $ scopeVtable scope }

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
  | not $ identName v `HS.member` allOccuring occs,
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
noUnique = local (\scope -> scope { scopeVtable = HM.map set $ scopeVtable scope})
  where set (BoundV t)         = BoundV $ t `setUniqueness` Nonunique
        set (BoundF f closure) = BoundF f closure
        set (OverloadedF f)    = OverloadedF f
        set EqualityF          = EqualityF
        set (WasConsumed loc)  = WasConsumed loc

onlySelfAliasing :: TermTypeM a -> TermTypeM a
onlySelfAliasing = local (\scope -> scope { scopeVtable = HM.mapWithKey set $ scopeVtable scope})
  where set k (BoundV t)         = BoundV $ t `addAliases` HS.intersection (HS.singleton k)
        set _ (BoundF f closure) = BoundF f closure
        set _ (OverloadedF f)    = OverloadedF f
        set _ EqualityF          = EqualityF
        set _ (WasConsumed loc)  = WasConsumed loc
