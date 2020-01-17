{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, DeriveFunctor #-}
{-# Language TupleSections #-}
-- | Facilities for type-checking Futhark terms.  Checking a term
-- requires a little more context to track uniqueness and such.
--
-- Type inference is implemented through a variation of
-- Hindley-Milner.  The main complication is supporting the rich
-- number of built-in language constructs, as well as uniqueness
-- types.  This is mostly done in an ad hoc way, and many programs
-- will require the programmer to fall back on type annotations.
module Language.Futhark.TypeChecker.Terms
  ( checkOneExp
  , checkFunDef
  )
where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.RWS hiding (Sum)
import qualified Control.Monad.Fail as Fail
import Data.Char (isAlpha)
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Loc
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Prelude hiding (mod)

import Language.Futhark
import Language.Futhark.Semantic (includeToString)
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Monad hiding (BoundV, checkQualNameWithEnv)
import Language.Futhark.TypeChecker.Types hiding (checkTypeDecl)
import Language.Futhark.TypeChecker.Unify hiding (Usage)
import qualified Language.Futhark.TypeChecker.Types as Types
import qualified Language.Futhark.TypeChecker.Monad as TypeM
import Futhark.Util.Pretty hiding (space, bool, group)

--- Uniqueness

data Usage = Consumed SrcLoc
           | Observed SrcLoc
           deriving (Eq, Ord, Show)

type Names = S.Set VName

-- | The consumption set is a Maybe so we can distinguish whether a
-- consumption took place, but the variable went out of scope since,
-- or no consumption at all took place.
data Occurence = Occurence { observed :: Names
                           , consumed :: Maybe Names
                           , location :: SrcLoc
                           }
             deriving (Eq, Show)

instance Located Occurence where
  locOf = locOf . location

observation :: Aliasing -> SrcLoc -> Occurence
observation = flip Occurence Nothing . S.map aliasVar

consumption :: Aliasing -> SrcLoc -> Occurence
consumption = Occurence S.empty . Just . S.map aliasVar

-- | A null occurence is one that we can remove without affecting
-- anything.
nullOccurence :: Occurence -> Bool
nullOccurence occ = S.null (observed occ) && isNothing (consumed occ)

-- | A seminull occurence is one that does not contain references to
-- any variables in scope.  The big difference is that a seminull
-- occurence may denote a consumption, as long as the array that was
-- consumed is now out of scope.
seminullOccurence :: Occurence -> Bool
seminullOccurence occ = S.null (observed occ) && maybe True S.null (consumed occ)

type Occurences = [Occurence]

type UsageMap = M.Map VName [Usage]

usageMap :: Occurences -> UsageMap
usageMap = foldl comb M.empty
  where comb m (Occurence obs cons loc) =
          let m' = S.foldl' (ins $ Observed loc) m obs
          in S.foldl' (ins $ Consumed loc) m' $ fromMaybe mempty cons
        ins v m k = M.insertWith (++) k [v] m

combineOccurences :: MonadTypeChecker m => VName -> Usage -> Usage -> m Usage
combineOccurences _ (Observed loc) (Observed _) = return $ Observed loc
combineOccurences name (Consumed wloc) (Observed rloc) =
  useAfterConsume (baseName name) rloc wloc
combineOccurences name (Observed rloc) (Consumed wloc) =
  useAfterConsume (baseName name) rloc wloc
combineOccurences name (Consumed loc1) (Consumed loc2) =
  consumeAfterConsume (baseName name) (max loc1 loc2) (min loc1 loc2)

checkOccurences :: MonadTypeChecker m => Occurences -> m ()
checkOccurences = void . M.traverseWithKey comb . usageMap
  where comb _    []     = return ()
        comb name (u:us) = foldM_ (combineOccurences name) u us

allObserved :: Occurences -> Names
allObserved = S.unions . map observed

allConsumed :: Occurences -> Names
allConsumed = S.unions . map (fromMaybe mempty . consumed)

allOccuring :: Occurences -> Names
allOccuring occs = allConsumed occs <> allObserved occs

anyConsumption :: Occurences -> Maybe Occurence
anyConsumption = find (isJust . consumed)

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
          occ { consumed = S.difference <$> consumed occ <*> pure cons2
              , observed = observed occ `S.difference` cons2 }
        filt2 occ =
          occ { consumed = consumed occ
              , observed = observed occ `S.difference` cons1 }
        cons1 = allConsumed occurs1
        cons2 = allConsumed occurs2

--- Scope management

-- | Whether something is a global or a local variable.
data Locality = Local | Global
              deriving (Show)

data ValBinding = BoundV Locality [TypeParam] PatternType
                -- ^ Aliases in parameters indicate the lexical
                -- closure.
                | OverloadedF [PrimType] [Maybe PrimType] (Maybe PrimType)
                | EqualityF
                | OpaqueF
                | WasConsumed SrcLoc
                deriving (Show)

-- | Type checking happens with access to this environment.  The
-- 'TermScope' will be extended during type-checking as bindings come into
-- scope.
data TermEnv = TermEnv { termScope :: TermScope
                       , termBreadCrumbs :: [BreadCrumb]
                         -- ^ Most recent first.
                       , termLevel :: Level
                       }

data TermScope = TermScope { scopeVtable  :: M.Map VName ValBinding
                           , scopeTypeTable :: M.Map VName TypeBinding
                           , scopeModTable :: M.Map VName Mod
                           , scopeNameMap :: NameMap
                           } deriving (Show)

instance Semigroup TermScope where
  TermScope vt1 tt1 mt1 nt1 <> TermScope vt2 tt2 mt2 nt2 =
    TermScope (vt2 `M.union` vt1) (tt2 `M.union` tt1) (mt1 `M.union` mt2) (nt2 `M.union` nt1)

envToTermScope :: Env -> TermScope
envToTermScope env = TermScope { scopeVtable = vtable
                               , scopeTypeTable = envTypeTable env
                               , scopeNameMap = envNameMap env
                               , scopeModTable = envModTable env
                               }
  where vtable = M.mapWithKey valBinding $ envVtable env
        valBinding k (TypeM.BoundV tps v) =
          BoundV Global tps $ v `setAliases`
          (if arrayRank v > 0 then S.singleton (AliasBound k) else mempty)

withEnv :: TermEnv -> Env -> TermEnv
withEnv tenv env = tenv { termScope = termScope tenv <> envToTermScope env }

overloadedTypeVars :: Constraints -> Names
overloadedTypeVars = mconcat . map f . M.elems
  where f (_, HasFields fs _) = mconcat $ map typeVars $ M.elems fs
        f _ = mempty

-- | Get the type of an expression, with all type variables
-- substituted.  Never call 'typeOf' directly (except in a few
-- carefully inspected locations)!
expType :: Exp -> TermTypeM PatternType
expType = normaliseType . typeOf

-- | The state is a set of constraints and a counter for generating
-- type names.  This is distinct from the usual counter we use for
-- generating unique names, as these will be user-visible.
type TermTypeState = (Constraints, Int)

newtype TermTypeM a = TermTypeM (RWST
                                 TermEnv
                                 Occurences
                                 TermTypeState
                                 TypeM
                                 a)
  deriving (Monad, Functor, Applicative,
            MonadReader TermEnv,
            MonadWriter Occurences,
            MonadState TermTypeState,
            MonadError TypeError)

instance Fail.MonadFail TermTypeM where
  fail = typeError (noLoc :: SrcLoc) . ("unknown failure (likely a bug): "++)

instance MonadUnify TermTypeM where
  getConstraints = gets fst
  putConstraints x = modify $ \s -> (x, snd s)

  newTypeVar loc desc = do
    i <- incCounter
    v <- newID $ mkTypeVarName desc i
    constrain v $ NoConstraint Lifted $ mkUsage' loc
    return $ Scalar $ TypeVar mempty Nonunique (typeName v) []

  curLevel = asks termLevel

instance MonadBreadCrumbs TermTypeM where
  breadCrumb bc = local $ \env ->
    env { termBreadCrumbs = bc : termBreadCrumbs env }
  getBreadCrumbs = asks termBreadCrumbs

runTermTypeM :: TermTypeM a -> TypeM (a, Occurences)
runTermTypeM (TermTypeM m) = do
  initial_scope <- (initialTermScope <>) . envToTermScope <$> askEnv
  let initial_tenv = TermEnv { termScope = initial_scope
                             , termBreadCrumbs = mempty
                             , termLevel = 0
                             }
  evalRWST m initial_tenv (mempty, 0)

liftTypeM :: TypeM a -> TermTypeM a
liftTypeM = TermTypeM . lift

localScope :: (TermScope -> TermScope) -> TermTypeM a -> TermTypeM a
localScope f = local $ \tenv -> tenv { termScope = f $ termScope tenv }

incCounter :: TermTypeM Int
incCounter = do (x, i) <- get
                put (x, i+1)
                return i

constrain :: VName -> Constraint -> TermTypeM ()
constrain v c = do
  lvl <- curLevel
  modifyConstraints $ M.insert v (lvl, c)

incLevel :: TermTypeM a -> TermTypeM a
incLevel = local $ \env -> env { termLevel = termLevel env + 1 }

initialTermScope :: TermScope
initialTermScope = TermScope { scopeVtable = initialVtable
                             , scopeTypeTable = mempty
                             , scopeNameMap = topLevelNameMap
                             , scopeModTable = mempty
                             }
  where initialVtable = M.fromList $ mapMaybe addIntrinsicF $ M.toList intrinsics

        prim = Scalar . Prim
        arrow x y = Scalar $ Arrow mempty Unnamed x y

        addIntrinsicF (name, IntrinsicMonoFun pts t) =
          Just (name, BoundV Global [] $ arrow pts' $ prim t)
          where pts' = case pts of [pt] -> prim pt
                                   _    -> tupleRecord $ map prim pts

        addIntrinsicF (name, IntrinsicOverloadedFun ts pts rts) =
          Just (name, OverloadedF ts pts rts)
        addIntrinsicF (name, IntrinsicPolyFun tvs pts rt) =
          Just (name, BoundV Global tvs $
                      fromStruct $ anyDimShapeAnnotations $
                      Scalar $ Arrow mempty Unnamed pts' rt)
          where pts' = case pts of [pt] -> pt
                                   _    -> tupleRecord pts
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

  bindNameMap m = localScope $ \scope ->
    scope { scopeNameMap = m <> scopeNameMap scope }

  bindVal v (TypeM.BoundV tps t) = localScope $ \scope ->
    scope { scopeVtable = M.insert v vb $ scopeVtable scope }
    where vb = BoundV Local tps $ fromStruct t

  lookupType loc qn = do
    outer_env <- liftTypeM askEnv
    (scope, qn'@(QualName qs name)) <- checkQualNameWithEnv Type qn loc
    case M.lookup name $ scopeTypeTable scope of
      Nothing -> undefinedType loc qn
      Just (TypeAbbr l ps def) ->
        return (qn', ps, qualifyTypeVars outer_env (map typeParamName ps) qs def, l)

  lookupMod loc qn = do
    (scope, qn'@(QualName _ name)) <- checkQualNameWithEnv Term qn loc
    case M.lookup name $ scopeModTable scope of
      Nothing -> unknownVariableError Term qn loc
      Just m  -> return (qn', m)

  lookupVar loc qn = do
    outer_env <- liftTypeM askEnv
    (scope, qn'@(QualName qs name)) <- checkQualNameWithEnv Term qn loc
    let usage = mkUsage loc $ "use of " ++ quote (pretty qn)

    t <- case M.lookup name $ scopeVtable scope of
      Nothing -> throwError $ TypeError loc $
                 "Unknown variable " ++ quote (pretty qn) ++ "."

      Just (WasConsumed wloc) -> useAfterConsume (baseName name) loc wloc

      Just (BoundV _ tparams t)
        | "_" `isPrefixOf` baseString name -> underscoreUse loc qn
        | otherwise -> do
            (tnames, t') <- instantiateTypeScheme loc tparams t
            let qual = qualifyTypeVars outer_env tnames qs
            qual . anyDimShapeAnnotations <$> normaliseType t'

      Just OpaqueF -> do
        argtype <- newTypeVar loc "t"
        return $ Scalar $ Arrow mempty Unnamed argtype argtype

      Just EqualityF -> do
        argtype <- newTypeVar loc "t"
        equalityType usage argtype
        return $
          Scalar $ Arrow mempty Unnamed argtype $
          Scalar $ Arrow mempty Unnamed argtype $ Scalar $ Prim Bool

      Just (OverloadedF ts pts rt) -> do
        argtype <- newTypeVar loc "t"
        mustBeOneOf ts usage argtype
        let (pts', rt') = instOverloaded argtype pts rt
            arrow xt yt = Scalar $ Arrow mempty Unnamed xt yt
        return $ fromStruct $ vacuousShapeAnnotations $ foldr arrow rt' pts'

    observe $ Ident name (Info t) loc
    return (qn', t)

      where instOverloaded argtype pts rt =
              (map (maybe (toStruct argtype) (Scalar . Prim)) pts,
               maybe (toStruct argtype) (Scalar . Prim) rt)

  checkNamedDim loc v = do
    (v', t) <- lookupVar loc v
    unify (mkUsage loc "use as array size") (toStructural t) $
      Scalar $ Prim $ Signed Int32
    return v'

checkQualNameWithEnv :: Namespace -> QualName Name -> SrcLoc -> TermTypeM (TermScope, QualName VName)
checkQualNameWithEnv space qn@(QualName quals name) loc = do
  scope <- asks termScope
  descend scope quals
  where descend scope []
          | Just name' <- M.lookup (space, name) $ scopeNameMap scope =
              return (scope, name')
          | otherwise =
              unknownVariableError space qn loc

        descend scope (q:qs)
          | Just (QualName _ q') <- M.lookup (Term, q) $ scopeNameMap scope,
            Just res <- M.lookup q' $ scopeModTable scope =
              case res of
                -- Check if we are referring to the magical intrinsics
                -- module.
                _ | baseTag q' <= maxIntrinsicTag ->
                      checkIntrinsic space qn loc
                ModEnv q_scope -> do
                  (scope', QualName qs' name') <- descend (envToTermScope q_scope) qs
                  return (scope', QualName (q':qs') name')
                ModFun{} -> unappliedFunctor loc
          | otherwise =
              unknownVariableError space qn loc

checkIntrinsic :: Namespace -> QualName Name -> SrcLoc -> TermTypeM (TermScope, QualName VName)
checkIntrinsic space qn@(QualName _ name) loc
  | Just v <- M.lookup (space, name) intrinsicsNameMap = do
      me <- liftTypeM askImportName
      unless ("/futlib" `isPrefixOf` includeToString me) $
        warn loc "Using intrinsic functions directly can easily crash the compiler or result in wrong code generation."
      scope <- asks termScope
      return (scope, v)
  | otherwise =
      unknownVariableError space qn loc

-- | Wrap 'Types.checkTypeDecl' to also perform an observation of
-- every size in the type.
checkTypeDecl :: TypeDeclBase NoInfo Name -> TermTypeM (TypeDeclBase Info VName)
checkTypeDecl tdecl = do
  (tdecl', _) <- Types.checkTypeDecl [] tdecl
  mapM_ observeDim $ nestedDims $ unInfo $ expandedType tdecl'
  return tdecl'
  where observeDim (NamedDim v) =
          observe $ Ident (qualLeaf v) (Info $ Scalar $ Prim $ Signed Int32) noLoc
        observeDim _ = return ()

-- | Instantiate a type scheme with fresh type variables for its type
-- parameters. Returns the names of the fresh type variables, the instance
-- list, and the instantiated type.
instantiateTypeScheme :: SrcLoc -> [TypeParam] -> PatternType
                      -> TermTypeM ([VName], PatternType)
instantiateTypeScheme loc tparams t = do
  let tparams' = filter isTypeParam tparams
      tnames = map typeParamName tparams'
  (fresh_tnames, substs) <- unzip <$> mapM (instantiateTypeParam loc) tparams'
  let substs' = M.fromList $ zip tnames substs
      t' = substTypesAny (`M.lookup` substs') t
  return (fresh_tnames, t')

-- | Create a new type name and insert it (unconstrained) in the
-- substitution map.
instantiateTypeParam :: Monoid as => SrcLoc -> TypeParam -> TermTypeM (VName, Subst (TypeBase dim as))
instantiateTypeParam loc tparam = do
  i <- incCounter
  v <- newID $ mkTypeVarName (takeWhile isAlpha (baseString (typeParamName tparam))) i
  constrain v $ NoConstraint l $ mkUsage' loc
  return (v, Subst $ Scalar $ TypeVar mempty Nonunique (typeName v) [])
  where l = case tparam of TypeParamType x _ _ -> x
                           _                   -> Lifted

newArrayType :: SrcLoc -> String -> Int -> TermTypeM (TypeBase () (), TypeBase () ())
newArrayType loc desc r = do
  v <- newID $ nameFromString desc
  constrain v $ NoConstraint Unlifted $ mkUsage' loc
  let rowt = TypeVar () Nonunique (typeName v) []
  return (Array () Nonunique rowt (ShapeDecl $ replicate r ()),
          Scalar rowt)

--- Errors

funName :: Maybe Name -> String
funName Nothing = "anonymous function"
funName (Just fname) = "function " ++ quote (pretty fname)

useAfterConsume :: MonadTypeChecker m => Name -> SrcLoc -> SrcLoc -> m a
useAfterConsume name rloc wloc =
  throwError $ TypeError rloc $
  "Variable " ++ quote (pretty name) ++ " previously consumed at " ++
  locStr wloc ++ ".  (Possibly through aliasing)"

consumeAfterConsume :: MonadTypeChecker m => Name -> SrcLoc -> SrcLoc -> m a
consumeAfterConsume name loc1 loc2 =
  throwError $ TypeError loc2 $
  "Variable " ++ pretty name ++ " previously consumed at " ++ locStr loc1 ++ "."

badLetWithValue :: MonadTypeChecker m => SrcLoc -> m a
badLetWithValue loc =
  throwError $ TypeError loc
  "New value for elements in let-with shares data with source array.  This is illegal, as it prevents in-place modification."

returnAliased :: MonadTypeChecker m => Maybe Name -> Name -> SrcLoc -> m ()
returnAliased fname name loc =
  throwError $ TypeError loc $
  "Unique return value of " ++ funName fname ++
  " is aliased to " ++ quote (pretty name) ++ ", which is not consumed."

uniqueReturnAliased :: MonadTypeChecker m => Maybe Name -> SrcLoc -> m a
uniqueReturnAliased fname loc =
  throwError $ TypeError loc $
  "A unique tuple element of return value of " ++
  funName fname ++ " is aliased to some other tuple component."

--- Basic checking

-- | Determine if two types are identical, ignoring uniqueness.
-- Causes a 'TypeError' if they fail to match, and otherwise returns
-- one of them.
unifyExpTypes :: Exp -> Exp -> TermTypeM PatternType
unifyExpTypes e1 e2 = do
  e1_t <- expType e1
  e2_t <- expType e2
  unify (mkUsage (srclocOf e2) "requiring equality of types") (toStructural e1_t) (toStructural e2_t)
  return $ unifyTypeAliases e1_t e2_t

-- | Assumes that the two types have already been unified.
unifyTypeAliases :: PatternType -> PatternType -> PatternType
unifyTypeAliases t1 t2 =
  case (t1, t2) of
    (Array als1 u1 et1 shape1, Array als2 u2 _ _) ->
      Array (als1<>als2) (min u1 u2) et1 shape1
    (Scalar (Record f1), Scalar (Record f2)) ->
      Scalar $ Record $ M.intersectionWith unifyTypeAliases f1 f2
    (Scalar (TypeVar als1 u v targs1), Scalar (TypeVar als2 _ _ targs2)) ->
      Scalar $ TypeVar (als1 <> als2) u v $ zipWith unifyTypeArg targs1 targs2
    _ -> t1
  where unifyTypeArg (TypeArgType t1' loc) (TypeArgType _ _) =
          TypeArgType t1' loc
        unifyTypeArg a _ = a

--- General binding.

doNotShadow :: [String]
doNotShadow = ["&&", "||"]

data InferredType = NoneInferred
                  | Ascribed PatternType


checkPattern' :: UncheckedPattern -> InferredType
              -> TermTypeM Pattern

checkPattern' (PatternParens p loc) t =
  PatternParens <$> checkPattern' p t <*> pure loc

checkPattern' (Id name _ loc) _
  | name' `elem` doNotShadow =
      typeError loc $ "The " ++ name' ++ " operator may not be redefined."
  where name' = nameToString name

checkPattern' (Id name NoInfo loc) (Ascribed t) = do
  name' <- newID name
  return $ Id name' (Info t) loc
checkPattern' (Id name NoInfo loc) NoneInferred = do
  name' <- newID name
  t <- newTypeVar loc "t"
  return $ Id name' (Info t) loc

checkPattern' (Wildcard _ loc) (Ascribed t) =
  return $ Wildcard (Info $ t `setUniqueness` Nonunique) loc
checkPattern' (Wildcard NoInfo loc) NoneInferred = do
  t <- newTypeVar loc "t"
  return $ Wildcard (Info t) loc

checkPattern' (TuplePattern ps loc) (Ascribed t)
  | Just ts <- isTupleRecord t, length ts == length ps =
      TuplePattern <$> zipWithM checkPattern' ps (map Ascribed ts) <*> pure loc
checkPattern' p@(TuplePattern ps loc) (Ascribed t) = do
  ps_t <- replicateM (length ps) (newTypeVar loc "t")
  unify (mkUsage loc "matching a tuple pattern") (tupleRecord ps_t) $ toStructural t
  t' <- normaliseType t
  checkPattern' p $ Ascribed t'
checkPattern' (TuplePattern ps loc) NoneInferred =
  TuplePattern <$> mapM (`checkPattern'` NoneInferred) ps <*> pure loc

checkPattern' (RecordPattern p_fs _) _
  | Just (f, fp) <- find (("_" `isPrefixOf`) . nameToString . fst) p_fs =
      typeError fp $ unlines [ "Underscore-prefixed fields are not allowed."
                             , "Did you mean " ++
                               quote (drop 1 (nameToString f) ++ "=_") ++ "?"]

checkPattern' (RecordPattern p_fs loc) (Ascribed (Scalar (Record t_fs)))
  | sort (map fst p_fs) == sort (M.keys t_fs) =
    RecordPattern . M.toList <$> check <*> pure loc
    where check = traverse (uncurry checkPattern') $ M.intersectionWith (,)
                  (M.fromList p_fs) (fmap Ascribed t_fs)
checkPattern' p@(RecordPattern fields loc) (Ascribed t) = do
  fields' <- traverse (const $ newTypeVar loc "t") $ M.fromList fields

  when (sort (M.keys fields') /= sort (map fst fields)) $
    typeError loc $ "Duplicate fields in record pattern " ++ pretty p

  unify (mkUsage loc "matching a record pattern") (Scalar (Record fields')) $ toStructural t
  t' <- normaliseType t
  checkPattern' p $ Ascribed t'
checkPattern' (RecordPattern fs loc) NoneInferred =
  RecordPattern . M.toList <$> traverse (`checkPattern'` NoneInferred) (M.fromList fs) <*> pure loc

checkPattern' (PatternAscription p (TypeDecl t NoInfo) loc) maybe_outer_t = do
  (t', st, _) <- checkTypeExp t

  let st' = fromStruct st
  case maybe_outer_t of
    Ascribed outer_t -> do
      unify (mkUsage loc "explicit type ascription") (toStructural st) (toStructural outer_t)

      -- We also have to make sure that uniqueness and shapes match.
      -- This is done explicitly, because they are ignored by
      -- unification.
      st'' <- normaliseType st'
      outer_t' <- normaliseType outer_t
      case unifyTypesU unifyUniqueness st' outer_t' of
        Just outer_t'' ->
          PatternAscription <$> checkPattern' p (Ascribed outer_t'') <*>
          pure (TypeDecl t' (Info st)) <*> pure loc
        Nothing ->
          typeError loc $ "Cannot match type " ++ quote (pretty outer_t') ++ " with expected type " ++
          quote (pretty st'') ++ "."

    NoneInferred ->
      PatternAscription <$> checkPattern' p (Ascribed st') <*>
      pure (TypeDecl t' (Info st)) <*> pure loc
 where unifyUniqueness u1 u2 = if u2 `subuniqueOf` u1 then Just u1 else Nothing

checkPattern' (PatternLit e NoInfo loc) (Ascribed t) = do
  e' <- checkExp e
  t' <- expType e'
  unify (mkUsage loc "matching against literal") (toStructural t') (toStructural t)
  return $ PatternLit e' (Info t') loc

checkPattern' (PatternLit e NoInfo loc) NoneInferred = do
  e' <- checkExp e
  t' <- expType e'
  return $ PatternLit e' (Info t') loc

checkPattern' (PatternConstr n NoInfo ps loc) (Ascribed (Scalar (Sum cs)))
  | Just ts <- M.lookup n cs = do
      ps' <- zipWithM checkPattern' ps $ map Ascribed ts
      return $ PatternConstr n (Info (Scalar (Sum cs))) ps' loc

checkPattern' (PatternConstr n NoInfo ps loc) (Ascribed t) = do
  t' <- newTypeVar loc "t"
  ps' <- mapM (`checkPattern'` NoneInferred) ps
  mustHaveConstr usage n t' (toStructural . patternType <$> ps')
  unify usage t' (toStructural t)
  t'' <- normaliseType t
  return $ PatternConstr n (Info t'') ps' loc
  where usage = mkUsage loc "matching against constructor"

checkPattern' (PatternConstr n NoInfo ps loc) NoneInferred = do
  ps' <- mapM (`checkPattern'` NoneInferred) ps
  t <- newTypeVar loc "t"
  mustHaveConstr usage n t (toStructural . patternType <$> ps')
  return $ PatternConstr n (Info t) ps' loc
  where usage = mkUsage loc "matching against constructor"

patternNameMap :: Pattern -> NameMap
patternNameMap = M.fromList . map asTerm . S.toList . patternIdents
  where asTerm v = ((Term, baseName $ identName v), qualName $ identName v)

checkPattern :: UncheckedPattern -> InferredType -> (Pattern -> TermTypeM a)
             -> TermTypeM a
checkPattern p t m = do
  checkForDuplicateNames [p]
  p' <- checkPattern' p t
  bindNameMap (patternNameMap p') $ m p'

binding :: [Ident] -> TermTypeM a -> TermTypeM a
binding bnds = check . localScope (`bindVars` bnds)
  where bindVars :: TermScope -> [Ident] -> TermScope
        bindVars = foldl bindVar

        bindVar :: TermScope -> Ident -> TermScope
        bindVar scope (Ident name (Info tp) _) =
          let inedges = boundAliases $ aliases tp
              update (BoundV l tparams in_t)
                -- If 'name' is record or sum-typed, don't alias the
                -- components to 'name', because these no identity
                -- beyond their components.
                | Array{} <- tp = BoundV l tparams (in_t `addAliases` S.insert (AliasBound name))
                | otherwise = BoundV l tparams in_t
              update b = b

              tp' = tp `addAliases` S.insert (AliasBound name)
          in scope { scopeVtable = M.insert name (BoundV Local [] tp') $
                                   adjustSeveral update inedges $
                                   scopeVtable scope
                   }

        adjustSeveral f = flip $ foldl $ flip $ M.adjust f

        -- Check whether the bound variables have been used correctly
        -- within their scope.
        check m = do
          (a, usages) <- collectBindingsOccurences m
          checkOccurences usages

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
                                 occ_cons = divide <$> consumed occ
                                 con1 = fst <$> occ_cons
                                 con2 = snd <$> occ_cons
                             in (occ { observed = obs1, consumed = con1 },
                                 occ { observed = obs2, consumed = con2 }))
                names = S.fromList $ map identName bnds
                divide s = (s `S.intersection` names, s `S.difference` names)

bindingTypes :: [(VName, (TypeBinding, Constraint))] -> TermTypeM a -> TermTypeM a
bindingTypes types m = do
  lvl <- curLevel
  modifyConstraints (<>M.map ((lvl,) . snd) (M.fromList types))
  localScope extend m
  where extend scope = scope {
          scopeTypeTable = M.map fst (M.fromList types) <> scopeTypeTable scope
          }

bindingTypeParams :: [TypeParam] -> TermTypeM a -> TermTypeM a
bindingTypeParams tparams = binding (mapMaybe typeParamIdent tparams) .
                            bindingTypes (mapMaybe typeParamType tparams)
  where typeParamType (TypeParamType l v loc) =
          Just (v, (TypeAbbr l [] (Scalar (TypeVar () Nonunique (typeName v) [])),
                    ParamType l loc))
        typeParamType TypeParamDim{} =
          Nothing

typeParamIdent :: TypeParam -> Maybe Ident
typeParamIdent (TypeParamDim v loc) =
  Just $ Ident v (Info $ Scalar $ Prim $ Signed Int32) loc
typeParamIdent _ = Nothing

bindingIdent :: IdentBase NoInfo Name -> PatternType -> (Ident -> TermTypeM a)
             -> TermTypeM a
bindingIdent (Ident v NoInfo vloc) t m =
  bindSpaced [(Term, v)] $ do
    v' <- checkName Term v vloc
    let ident = Ident v' (Info t) vloc
    binding [ident] $ m ident

bindingPatternGroup :: [UncheckedTypeParam]
                    -> [UncheckedPattern]
                    -> ([TypeParam] -> [Pattern] -> TermTypeM a) -> TermTypeM a
bindingPatternGroup tps orig_ps m = do
  checkForDuplicateNames orig_ps
  checkTypeParams tps $ \tps' -> bindingTypeParams tps' $ do
    let descend ps' (p:ps) =
          checkPattern p NoneInferred $ \p' ->
            binding (S.toList $ patternIdents p') $ descend (p':ps') ps
        descend ps' [] = do
          -- Perform an observation of every type parameter.  This
          -- prevents unused-name warnings for otherwise unused
          -- dimensions.
          mapM_ observe $ mapMaybe typeParamIdent tps'
          let ps'' = reverse ps'
          checkShapeParamUses patternUses tps' ps''

          m tps' ps''

    descend [] orig_ps

bindingPattern :: PatternBase NoInfo Name -> InferredType
               -> (Pattern -> TermTypeM a) -> TermTypeM a
bindingPattern p t m = do
  checkForDuplicateNames [p]
  checkPattern p t $ \p' -> binding (S.toList $ patternIdents p') $ do
    -- Perform an observation of every declared dimension.  This
    -- prevents unused-name warnings for otherwise unused dimensions.
    mapM_ observe $ patternDims p'

    m p'

-- | Return the shapes used in a given pattern in postive and negative
-- position, respectively.
patternUses :: Pattern -> ([VName], [VName])
patternUses Id{} = mempty
patternUses Wildcard{} = mempty
patternUses PatternLit{} = mempty
patternUses (PatternParens p _) = patternUses p
patternUses (TuplePattern ps _) = foldMap patternUses ps
patternUses (RecordPattern fs _) = foldMap (patternUses . snd) fs
patternUses (PatternAscription p (TypeDecl declte _) _) =
  patternUses p <> typeExpUses declte
patternUses (PatternConstr _ _ ps _) = foldMap patternUses ps

patternDims :: Pattern -> [Ident]
patternDims (PatternParens p _) = patternDims p
patternDims (TuplePattern pats _) = concatMap patternDims pats
patternDims (PatternAscription p (TypeDecl _ (Info t)) _) =
  patternDims p <> mapMaybe (dimIdent (srclocOf p)) (nestedDims t)
  where dimIdent _ AnyDim            = Nothing
        dimIdent _ (ConstDim _)      = Nothing
        dimIdent _ NamedDim{}        = Nothing
patternDims _ = []

--- Main checkers

-- | @require ts e@ causes a 'TypeError' if @expType e@ is not one of
-- the types in @ts@.  Otherwise, simply returns @e@.
require :: String -> [PrimType] -> Exp -> TermTypeM Exp
require why ts e = do mustBeOneOf ts (mkUsage (srclocOf e) why) . toStructural =<< expType e
                      return e

unifies :: String -> TypeBase () () -> Exp -> TermTypeM Exp
unifies why t e = do
  unify (mkUsage (srclocOf e) why) t =<< toStructural <$> expType e
  return e

-- The closure of a lambda or local function are those variables that
-- it references, and which local to the current top-level function.
lexicalClosure :: [Pattern] -> Occurences -> TermTypeM Aliasing
lexicalClosure params closure = do
  vtable <- asks $ scopeVtable . termScope
  let isLocal v = case v `M.lookup` vtable of
                    Just (BoundV Local _ _) -> True
                    _ -> False
  return $ S.map AliasBound $ S.filter isLocal $
    allOccuring closure S.\\
    S.map identName (mconcat (map patternIdents params))

checkExp :: UncheckedExp -> TermTypeM Exp

checkExp (Literal val loc) =
  return $ Literal val loc

checkExp (StringLit vs loc) =
  return $ StringLit vs loc

checkExp (IntLit val NoInfo loc) = do
  t <- newTypeVar loc "t"
  mustBeOneOf anyNumberType (mkUsage loc "integer literal") t
  return $ IntLit val (Info $ vacuousShapeAnnotations $ fromStruct t) loc

checkExp (FloatLit val NoInfo loc) = do
  t <- newTypeVar loc "t"
  mustBeOneOf anyFloatType (mkUsage loc "float literal") t
  return $ FloatLit val (Info $ vacuousShapeAnnotations $ fromStruct t) loc

checkExp (TupLit es loc) =
  TupLit <$> mapM checkExp es <*> pure loc

checkExp (RecordLit fs loc) = do
  fs' <- evalStateT (mapM checkField fs) mempty

  return $ RecordLit fs' loc
  where checkField (RecordFieldExplicit f e rloc) = do
          errIfAlreadySet f rloc
          modify $ M.insert f rloc
          RecordFieldExplicit f <$> lift (checkExp e) <*> pure rloc
        checkField (RecordFieldImplicit name NoInfo rloc) = do
          errIfAlreadySet name rloc
          (QualName _ name', t) <- lift $ lookupVar rloc $ qualName name
          modify $ M.insert name rloc
          return $ RecordFieldImplicit name' (Info t) rloc

        errIfAlreadySet f rloc = do
          maybe_sloc <- gets $ M.lookup f
          case maybe_sloc of
            Just sloc ->
              lift $ typeError rloc $ "Field '" ++ pretty f ++
              " previously defined at " ++ locStr sloc ++ "."
            Nothing -> return ()

checkExp (ArrayLit all_es _ loc) =
  -- Construct the result type and unify all elements with it.  We
  -- only create a type variable for empty arrays; otherwise we use
  -- the type of the first element.  This significantly cuts down on
  -- the number of type variables generated for pathologically large
  -- multidimensional array literals.
  case all_es of
    [] -> do et <- newTypeVar loc "t"
             t <- arrayOfM loc et (ShapeDecl [AnyDim]) Unique
             return $ ArrayLit [] (Info t) loc
    e:es -> do
      e' <- checkExp e
      et <- expType e'
      es' <- mapM (unifies "type of first array element" (toStructural et) <=< checkExp) es
      et' <- normaliseType et
      t <- arrayOfM loc et' (ShapeDecl [AnyDim]) Unique
      return $ ArrayLit (e':es') (Info t) loc

checkExp (Range start maybe_step end NoInfo loc) = do
  start' <- require "use in range expression" anyIntType =<< checkExp start
  start_t <- toStructural <$> expType start'
  maybe_step' <- case maybe_step of
    Nothing -> return Nothing
    Just step -> do
      let warning = warn loc "First and second element of range are identical, this will produce an empty array."
      case (start, step) of
        (Literal x _, Literal y _) -> when (x == y) warning
        (Var x_name _ _, Var y_name _ _) -> when (x_name == y_name) warning
        _ -> return ()
      Just <$> (unifies "use in range expression" start_t =<< checkExp step)

  end' <- case end of
    DownToExclusive e -> DownToExclusive <$>
                         (unifies "use in range expression" start_t =<< checkExp e)
    UpToExclusive e -> UpToExclusive <$>
                       (unifies "use in range expression" start_t =<< checkExp e)
    ToInclusive e -> ToInclusive <$>
                     (unifies "use in range expression" start_t =<< checkExp e)

  t <- arrayOfM loc (vacuousShapeAnnotations start_t) (rank 1) Unique

  return $ Range start' maybe_step' end'
    (Info (t `setAliases` mempty)) loc

checkExp (Ascript e decl NoInfo loc) = do
  decl' <- checkTypeDecl decl
  e' <- checkExp e
  t <- expType e'
  let decl_t = unInfo $ expandedType decl'
  unify (mkUsage loc "explicit type ascription") (toStructural decl_t) (toStructural t)

  -- We also have to make sure that uniqueness matches.  This is done
  -- explicitly, because uniqueness is ignored by unification.
  t' <- normaliseType t
  decl_t' <- normaliseType decl_t
  unless (t' `subtypeOf` anyDimShapeAnnotations decl_t') $
    typeError loc $ "Type " ++ quote (pretty t') ++ " is not a subtype of " ++
    quote (pretty decl_t') ++ "."

  let t'' = flip unifyTypeAliases t' $ combineTypeShapes t' $ fromStruct decl_t'
  return $ Ascript e' decl' (Info t'') loc

checkExp (BinOp (op, oploc) NoInfo (e1,_) (e2,_) NoInfo loc) = do
  (op', ftype) <- lookupVar oploc op
  (e1', e1_arg) <- checkArg e1
  (e2', e2_arg) <- checkArg e2

  (p1_t, rt) <- checkApply loc ftype e1_arg
  (p2_t, rt') <- checkApply loc rt e2_arg

  return $ BinOp (op', oploc) (Info ftype)
    (e1', Info $ toStruct p1_t) (e2', Info $ toStruct p2_t)
    (Info rt') loc

checkExp (Project k e NoInfo loc) = do
  e' <- checkExp e
  t <- expType e'
  kt <- mustHaveField (mkUsage loc $ "projection of field " ++ quote (pretty k)) k t
  return $ Project k e' (Info kt) loc

checkExp (If e1 e2 e3 _ loc) =
  sequentially checkCond $ \e1' _ -> do
  ((e2', e3'), dflow) <- tapOccurences $ checkExp e2 `alternative` checkExp e3
  brancht <- unifyExpTypes e2' e3'
  let t' = addAliases brancht (`S.difference` S.map AliasBound (allConsumed dflow))
  zeroOrderType (mkUsage loc "returning value of this type from 'if' expression") "returned from branch" t'
  return $ If e1' e2' e3' (Info t') loc
  where checkCond = do
          e1' <- checkExp e1
          unify (mkUsage (srclocOf e1') "use as 'if' condition") (Scalar $ Prim Bool) . toStructural =<< expType e1'
          return e1'

checkExp (Parens e loc) =
  Parens <$> checkExp e <*> pure loc

checkExp (QualParens (modname, modnameloc) e loc) = do
  (modname',mod) <- lookupMod loc modname
  case mod of
    ModEnv env -> local (`withEnv` qualifyEnv modname' env) $ do
      e' <- checkExp e
      return $ QualParens (modname', modnameloc) e' loc
    ModFun{} ->
      typeError loc $ "Module " ++ pretty modname ++ " is a parametric module."
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

  (qn', t, fields) <- findRootVar (qualQuals qn) (qualLeaf qn)

  foldM checkField (Var qn' (Info t) loc) fields

  where findRootVar qs name =
          (whenFound <$> lookupVar loc (QualName qs name)) `catchError` notFound qs name

        whenFound (qn', t) = (qn', t, [])

        notFound qs name err
          | null qs = throwError err
          | otherwise = do
              (qn', t, fields) <- findRootVar (init qs) (last qs) `catchError`
                                  const (throwError err)
              return (qn', t, fields++[name])

        checkField e k = do
          t <- expType e
          let usage = mkUsage loc $ "projection of field " ++ quote (pretty k)
          kt <- mustHaveField usage k t
          return $ Project k e (Info kt) loc

checkExp (Negate arg loc) = do
  arg' <- require "numeric negation" anyNumberType =<< checkExp arg
  return $ Negate arg' loc

checkExp (Apply e1 e2 NoInfo NoInfo loc) = do
  e1' <- checkExp e1
  (e2', arg) <- checkArg e2
  t <- expType e1'
  (t1, rt) <- checkApply loc t arg
  return $ Apply e1' e2' (Info $ diet t1) (Info rt) loc

checkExp (LetPat pat e body NoInfo loc) =
  sequentially (checkExp e) $ \e' e_occs -> do
    -- Not technically an ascription, but we want the pattern to have
    -- exactly the type of 'e'.
    t <- expType e'
    case anyConsumption e_occs of
      Just c ->
        let msg = "of value computed with consumption at " ++ locStr (location c)
        in zeroOrderType (mkUsage loc "consumption in right-hand side of 'let'-binding") msg t
      _ -> return ()
    bindingPattern pat (Ascribed $ anyDimShapeAnnotations t) $ \pat' -> do
      body' <- checkExp body
      body_t <- unscopeType (S.map identName $ patternIdents pat') <$> expType body'
      return $ LetPat pat' e' body' (Info body_t) loc

checkExp (LetFun name (tparams, params, maybe_retdecl, NoInfo, e) body loc) =
  sequentially (checkBinding (Just name, maybe_retdecl, tparams, params, e, loc)) $
  \(tparams', params', maybe_retdecl', rettype, e') closure -> do

    closure' <- lexicalClosure params' closure

    bindSpaced [(Term, name)] $ do
      name' <- checkName Term name loc

      let arrow (xp, xt) yt = Scalar $ Arrow () xp xt yt
          ftype = foldr (arrow . patternParam) rettype params'
          entry = BoundV Local tparams' $ ftype `setAliases` closure'
          bindF scope = scope { scopeVtable = M.insert name' entry $ scopeVtable scope
                              , scopeNameMap = M.insert (Term, name) (qualName name') $
                                               scopeNameMap scope }
      body' <- localScope bindF $ checkExp body

      return $ LetFun name' (tparams', params', maybe_retdecl', Info rettype, e') body' loc

checkExp (LetWith dest src idxes ve body NoInfo loc) = do
  (t, _) <- newArrayType (srclocOf src) "src" $ length idxes
  let elemt = stripArray (length $ filter isFix idxes) t
  sequentially (checkIdent src) $ \src' _ -> do
    let src'' = Var (qualName $ identName src') (identType src') (srclocOf src)
    void $ unifies "type of target array" t src''

    unless (unique $ unInfo $ identType src') $
      typeError loc $ "Source " ++ quote (pretty (identName src)) ++
      " has type " ++ pretty (unInfo $ identType src') ++ ", which is not unique."
    vtable <- asks $ scopeVtable . termScope
    forM_ (aliases $ unInfo $ identType src') $ \v ->
      case aliasVar v `M.lookup` vtable of
        Just (BoundV Local _ v_t)
          | not $ unique v_t ->
              typeError loc $ "Source " ++ quote (pretty (identName src)) ++
              " aliases " ++ quote (prettyName (aliasVar v)) ++ ", which is not consumable."
        _ -> return ()

    idxes' <- mapM checkDimIndex idxes
    sequentially (unifies "type of target array" elemt =<< checkExp ve) $ \ve' _ -> do
      ve_t <- expType ve'
      when (AliasBound (identName src') `S.member` aliases ve_t) $
        badLetWithValue loc

      bindingIdent dest (unInfo (identType src') `setAliases` S.empty) $ \dest' -> do
        body' <- consuming src' $ checkExp body
        body_t <- unscopeType (S.singleton $ identName dest') <$> expType body'
        return $ LetWith dest' src' idxes' ve' body' (Info body_t) loc
  where isFix DimFix{} = True
        isFix _        = False

checkExp (Update src idxes ve loc) = do
  (t, _) <- newArrayType (srclocOf src) "src" $ length idxes
  let elemt = stripArray (length $ filter isFix idxes) t
  sequentially (checkExp ve >>= unifies "type of target array" elemt) $ \ve' _ ->
    sequentially (checkExp src >>= unifies "type of target array" t) $ \src' _ -> do

    idxes' <- mapM checkDimIndex idxes

    src_t <- expType src'
    unless (unique src_t) $
      typeError loc $ "Source " ++ quote (pretty src) ++
      " has type " ++ pretty src_t ++ ", which is not unique"

    let src_als = aliases src_t
    ve_t <- expType ve'
    unless (S.null $ src_als `S.intersection` aliases ve_t) $ badLetWithValue loc

    consume loc src_als
    return $ Update src' idxes' ve' loc
  where isFix DimFix{} = True
        isFix _        = False

checkExp (RecordUpdate src fields ve NoInfo loc) = do
  src' <- checkExp src
  ve' <- checkExp ve
  a <- expType src'
  let usage = mkUsage loc "record update"
  r <- foldM (flip $ mustHaveField usage) a fields
  ve_t <- expType ve'
  unify usage (toStructural r) (toStructural ve_t)
  a' <- onRecordField (`setAliases` aliases ve_t) fields <$> expType src'
  return $ RecordUpdate src' fields ve' (Info a') loc

checkExp (Index e idxes NoInfo loc) = do
  (t, _) <- newArrayType (srclocOf e) "e" $ length idxes
  e' <- unifies "being indexed at" t =<< checkExp e
  idxes' <- mapM checkDimIndex idxes
  t' <- anyDimShapeAnnotations .
        stripArray (length $ filter isFix idxes) <$>
        normaliseType (typeOf e')
  return $ Index e' idxes' (Info t') loc
  where isFix DimFix{} = True
        isFix _        = False

checkExp (Unsafe e loc) =
  Unsafe <$> checkExp e <*> pure loc

checkExp (Assert e1 e2 NoInfo loc) = do
  e1' <- require "being asserted" [Bool] =<< checkExp e1
  e2' <- checkExp e2
  return $ Assert e1' e2' (Info (pretty e1)) loc

checkExp (Lambda params body rettype_te NoInfo loc) =
  removeSeminullOccurences $
  bindingPatternGroup [] params $ \_ params' -> do
    rettype_checked <- traverse checkTypeExp rettype_te
    let declared_rettype =
          case rettype_checked of Just (_, st, _) -> Just st
                                  Nothing -> Nothing
    (body', closure) <-
      tapOccurences $ noUnique $ checkFunBody body declared_rettype loc
    body_t <- expType body'
    let (rettype', rettype_st) =
          case rettype_checked of
            Just (te, st, _) -> (Just te, st)
            Nothing -> (Nothing, inferReturnUniqueness params' body_t)

    checkGlobalAliases params' body_t loc

    closure' <- lexicalClosure params' closure

    return $ Lambda params' body' rettype' (Info (closure', rettype_st)) loc

checkExp (OpSection op _ loc) = do
  (op', ftype) <- lookupVar loc op
  return $ OpSection op' (Info ftype) loc

checkExp (OpSectionLeft op _ e _ _ loc) = do
  (op', ftype) <- lookupVar loc op
  (e', e_arg) <- checkArg e
  (t1, rt) <- checkApply loc ftype e_arg
  case rt of
    Scalar (Arrow _ _ t2 rettype) ->
      return $ OpSectionLeft op' (Info ftype) e'
      (Info $ toStruct t1, Info $ toStruct t2) (Info rettype) loc
    _ -> typeError loc $
         "Operator section with invalid operator of type " ++ pretty ftype

checkExp (OpSectionRight op _ e _ _ loc) = do
  (op', ftype) <- lookupVar loc op
  (e', e_arg) <- checkArg e
  case ftype of
    Scalar (Arrow as1 m1 t1 (Scalar (Arrow as2 m2 t2 ret))) -> do
      (t2', Scalar (Arrow _ _ t1' rettype)) <-
        checkApply loc (Scalar $ Arrow as2 m2 t2 $ Scalar $ Arrow as1 m1 t1 ret) e_arg
      return $ OpSectionRight op' (Info ftype) e'
        (Info $ toStruct t1', Info $ toStruct t2') (Info rettype) loc
    _ -> typeError loc $
         "Operator section with invalid operator of type " ++ pretty ftype

checkExp (ProjectSection fields NoInfo loc) = do
  a <- newTypeVar loc "a"
  let usage = mkUsage loc "projection at"
  b <- foldM (flip $ mustHaveField usage) a fields
  return $ ProjectSection fields (Info $ Scalar $ Arrow mempty Unnamed a b) loc

checkExp (IndexSection idxes NoInfo loc) = do
  (t, _) <- newArrayType loc "e" (length idxes)
  idxes' <- mapM checkDimIndex idxes
  let t' = stripArray (length $ filter isFix idxes) t
  return $ IndexSection idxes' (Info $ vacuousShapeAnnotations $ fromStruct $
                                Scalar $ Arrow mempty Unnamed t t') loc
  where isFix DimFix{} = True
        isFix _        = False

checkExp (DoLoop mergepat mergeexp form loopbody loc) =
  sequentially (checkExp mergeexp) $ \mergeexp' _ -> do

  zeroOrderType (mkUsage (srclocOf mergeexp) "use as loop variable")
    "used as loop variable" (typeOf mergeexp')

  merge_t <- do
    merge_t <- expType mergeexp'
    return $ Ascribed $ anyDimShapeAnnotations $ merge_t `setAliases` mempty

  -- First we do a basic check of the loop body to figure out which of
  -- the merge parameters are being consumed.  For this, we first need
  -- to check the merge pattern, which requires the (initial) merge
  -- expression.
  --
  -- Play a little with occurences to ensure it does not look like
  -- none of the merge variables are being used.
  ((mergepat', form', loopbody'), bodyflow) <-
    case form of
      For i uboundexp -> do
        uboundexp' <- require "being the bound in a 'for' loop" anySignedType =<< checkExp uboundexp
        bound_t <- expType uboundexp'
        bindingIdent i bound_t $ \i' ->
          noUnique $ bindingPattern mergepat merge_t $
          \mergepat' -> onlySelfAliasing $ tapOccurences $ do
            loopbody' <- checkExp loopbody
            return (mergepat',
                    For i' uboundexp',
                    loopbody')

      ForIn xpat e -> do
        (arr_t, _) <- newArrayType (srclocOf e) "e" 1
        e' <- unifies "being iterated in a 'for-in' loop" arr_t =<< checkExp e
        t <- expType e'
        case t of
          _ | Just t' <- peelArray 1 t ->
                bindingPattern xpat (Ascribed t') $ \xpat' ->
                noUnique $ bindingPattern mergepat merge_t $
                \mergepat' -> onlySelfAliasing $ tapOccurences $ do
                  loopbody' <- checkExp loopbody
                  return (mergepat',
                          ForIn xpat' e',
                          loopbody')
            | otherwise ->
                typeError (srclocOf e) $
                "Iteratee of a for-in loop must be an array, but expression has type " ++ pretty t

      While cond ->
        noUnique $ bindingPattern mergepat merge_t $ \mergepat' ->
        onlySelfAliasing $ tapOccurences $
        sequentially (checkExp cond >>=
                      unifies "being the condition of a 'while' loop" (Scalar $ Prim Bool)) $ \cond' _ -> do
          loopbody' <- checkExp loopbody
          return (mergepat',
                  While cond',
                  loopbody')

  mergepat'' <- do
    loop_t <- expType loopbody'
    convergePattern mergepat' (allConsumed bodyflow) loop_t $
      mkUsage (srclocOf loopbody') "being (part of) the result of the loop body"

  let consumeMerge (Id _ (Info pt) ploc) mt
        | unique pt = consume ploc $ aliases mt
      consumeMerge (TuplePattern pats _) t | Just ts <- isTupleRecord t =
        zipWithM_ consumeMerge pats ts
      consumeMerge (PatternParens pat _) t =
        consumeMerge pat t
      consumeMerge (PatternAscription pat _ _) t =
        consumeMerge pat t
      consumeMerge _ _ =
        return ()
  consumeMerge mergepat'' =<< expType mergeexp'
  return $ DoLoop mergepat'' mergeexp' form' loopbody' loc

  where
    convergePattern pat body_cons body_t body_loc = do
      let consumed_merge = S.map identName (patternIdents pat) `S.intersection`
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
                let t' = case t of Scalar Record{} -> t
                                   _               -> t `setUniqueness` Nonunique
                in Id name (Info t') iloc
          uniquePat (TuplePattern pats ploc) =
            TuplePattern (map uniquePat pats) ploc
          uniquePat (RecordPattern fs ploc) =
            RecordPattern (map (fmap uniquePat) fs) ploc
          uniquePat (PatternAscription p t ploc) =
            PatternAscription p t ploc
          uniquePat p@PatternLit{} = p
          uniquePat (PatternConstr n t ps ploc) =
            PatternConstr n t (map uniquePat ps) ploc

          -- Make the pattern unique where needed.
          pat' = uniquePat pat

      -- Now check that the loop returned the right type.
      unify body_loc (toStructural body_t) $ toStructural $ patternType pat'
      body_t' <- normaliseType body_t
      pat_t <- normaliseType $ patternType pat'
      unless (body_t' `subtypeOf` pat_t) $
        unexpectedType (srclocOf body_loc)
        (toStructural body_t')
        [toStructural pat_t]

      -- Check that the new values of consumed merge parameters do not
      -- alias something bound outside the loop, AND that anything
      -- returned for a unique merge parameter does not alias anything
      -- else returned.  We also update the aliases for the pattern.
      bound_outside <- asks $ S.fromList . M.keys . scopeVtable . termScope
      let combAliases t1 t2 =
            case t1 of Scalar Record{} -> t1
                       _ -> t1 `addAliases` (<>aliases t2)

          checkMergeReturn (Id pat_v (Info pat_v_t) patloc) t
            | unique pat_v_t,
              v:_ <- S.toList $
                     S.map aliasVar (aliases t) `S.intersection` bound_outside =
                lift $ typeError loc $
                "Loop return value corresponding to merge parameter " ++
                quote (prettyName pat_v) ++ " aliases " ++ prettyName v ++ "."

            | otherwise = do
                (cons,obs) <- get
                unless (S.null $ aliases t `S.intersection` cons) $
                  lift $ typeError loc $
                  "Loop return value for merge parameter " ++
                  quote (prettyName pat_v) ++
                  " aliases other consumed merge parameter."
                when (unique pat_v_t &&
                      not (S.null (aliases t `S.intersection` (cons<>obs)))) $
                  lift $ typeError loc $
                  "Loop return value for consuming merge parameter " ++
                  quote (prettyName pat_v) ++ " aliases previously returned value."
                if unique pat_v_t
                  then put (cons<>aliases t, obs)
                  else put (cons, obs<>aliases t)

                return $ Id pat_v (Info (combAliases pat_v_t t)) patloc

          checkMergeReturn (Wildcard (Info pat_v_t) patloc) t =
            return $ Wildcard (Info (combAliases pat_v_t t)) patloc

          checkMergeReturn (PatternParens p _) t =
            checkMergeReturn p t

          checkMergeReturn (PatternAscription p _ _) t =
            checkMergeReturn p t

          checkMergeReturn (RecordPattern pfs patloc) (Scalar (Record tfs)) =
            RecordPattern . M.toList <$> sequence pfs' <*> pure patloc
            where pfs' = M.intersectionWith checkMergeReturn
                         (M.fromList pfs) tfs

          checkMergeReturn (TuplePattern pats patloc) t
            | Just ts <- isTupleRecord t =
                TuplePattern
                <$> zipWithM checkMergeReturn pats ts
                <*> pure patloc

          checkMergeReturn p _ =
            return p

      (pat'', (pat_cons, _)) <-
        runStateT (checkMergeReturn pat' body_t') (mempty, mempty)

      let body_cons' = body_cons <> S.map aliasVar pat_cons
      if body_cons' == body_cons && patternType pat'' == patternType pat
        then return pat'
        else convergePattern pat'' body_cons' body_t' body_loc

checkExp (Constr name es NoInfo loc) = do
  t <- newTypeVar loc "t"
  es' <- mapM checkExp es
  ets <- mapM expType es'
  mustHaveConstr (mkUsage loc "use of constructor") name t (toStructural <$> ets)
  -- A sum value aliases *anything* that went into its construction.
  let als = mconcat (map aliases ets)
  return $ Constr name es' (Info $ t `addAliases` (<>als)) loc

checkExp (Match e cs NoInfo loc) =
  sequentially (checkExp e) $ \e' _ -> do
    mt <- expType e'
    (cs', t) <- checkCases mt cs
    zeroOrderType (mkUsage loc "being returned 'match'") "returned from pattern match" t
    return $ Match e' cs' (Info t) loc

checkCases :: PatternType
           -> NE.NonEmpty (CaseBase NoInfo Name)
           -> TermTypeM (NE.NonEmpty (CaseBase Info VName), PatternType)
checkCases mt rest_cs =
  case NE.uncons rest_cs of
    (c, Nothing) -> do
      (c', t) <- checkCase mt c
      return (c' NE.:| [], t)
    (c, Just cs) -> do
      (((c', c_t), (cs', cs_t)), dflow) <-
        tapOccurences $ checkCase mt c `alternative` checkCases mt cs
      unify (mkUsage (srclocOf c) "pattern match") (toStructural c_t) (toStructural cs_t)
      let t = unifyTypeAliases c_t cs_t `addAliases`
              (`S.difference` S.map AliasBound (allConsumed dflow))
      return (NE.cons c' cs', t)

checkCase :: PatternType -> CaseBase NoInfo Name
          -> TermTypeM (CaseBase Info VName, PatternType)
checkCase mt (CasePat p caseExp loc) =
  bindingPattern p (Ascribed mt) $ \p' -> do
    caseExp' <- checkExp caseExp
    caseType <- expType caseExp'
    return (CasePat p' caseExp' loc, caseType)

-- | An unmatched pattern. Used in in the generation of
-- unmatched pattern warnings by the type checker.
data Unmatched p = UnmatchedNum p [ExpBase Info VName]
                 | UnmatchedBool p
                 | UnmatchedConstr p
                 | Unmatched p
                 deriving (Functor, Show)

instance Pretty (Unmatched (PatternBase Info VName)) where
  ppr um = case um of
      (UnmatchedNum p nums) -> ppr' p <+> text "where p is not one of" <+> ppr nums
      (UnmatchedBool p)     -> ppr' p
      (UnmatchedConstr p)     -> ppr' p
      (Unmatched p)         -> ppr' p
    where
      ppr' (PatternAscription p t _) = ppr p <> text ":" <+> ppr t
      ppr' (PatternParens p _)       = parens $ ppr' p
      ppr' (Id v _ _)                = pprName v
      ppr' (TuplePattern pats _)     = parens $ commasep $ map ppr' pats
      ppr' (RecordPattern fs _)      = braces $ commasep $ map ppField fs
        where ppField (name, t)      = text (nameToString name) <> equals <> ppr' t
      ppr' Wildcard{}                = text "_"
      ppr' (PatternLit e _ _)        = ppr e
      ppr' (PatternConstr n _ ps _)   = text "#" <> ppr n <+> sep (map ppr' ps)

unpackPat :: Pattern -> [Maybe Pattern]
unpackPat Wildcard{} = [Nothing]
unpackPat (PatternParens p _) = unpackPat p
unpackPat Id{} = [Nothing]
unpackPat (TuplePattern ps _) = Just <$> ps
unpackPat (RecordPattern fs _) = Just . snd <$> sortFields (M.fromList fs)
unpackPat (PatternAscription p _ _) = unpackPat p
unpackPat p@PatternLit{} = [Just p]
unpackPat p@PatternConstr{} = [Just p]

wildPattern :: Pattern -> Int -> Unmatched Pattern -> Unmatched Pattern
wildPattern (TuplePattern ps loc) pos um = wildTuple <$> um
  where wildTuple p = TuplePattern (take (pos - 1) ps' ++ [p] ++ drop pos ps') loc
        ps' = map wildOut ps
        wildOut p = Wildcard (Info (patternType p)) (srclocOf p)
wildPattern (RecordPattern fs loc) pos um = wildRecord <$> um
  where wildRecord p =
          RecordPattern (take (pos - 1) fs' ++ [(fst (fs!!(pos - 1)), p)] ++ drop pos fs') loc
        fs' = map wildOut fs
        wildOut (f,p) = (f, Wildcard (Info (patternType p)) (srclocOf p))
wildPattern (PatternAscription p _ _) pos um = wildPattern p pos um
wildPattern (PatternParens p _) pos um = wildPattern p pos um
wildPattern (PatternConstr n t ps loc) pos um = wildConstr <$> um
  where wildConstr p = PatternConstr n t (take (pos - 1) ps' ++ [p] ++ drop pos ps') loc
        ps' = map wildOut ps
        wildOut p = Wildcard (Info (patternType p)) (srclocOf p)
wildPattern _ _ um = um

checkUnmatched :: (MonadBreadCrumbs m, MonadTypeChecker m) => Exp -> m ()
checkUnmatched e = void $ checkUnmatched' e >> astMap tv e
  where checkUnmatched' (Match _ cs _ loc) =
          let ps = fmap (\(CasePat p _ _) -> p) cs
          in case unmatched id $ NE.toList ps of
              []  -> return ()
              ps' -> typeError loc $ "Unmatched cases in match expression: \n"
                                     ++ unlines (map (("  " ++) . pretty) ps')
        checkUnmatched' _ = return ()
        tv = ASTMapper { mapOnExp =
                           \e' -> checkUnmatched' e' >> return e'
                       , mapOnName        = pure
                       , mapOnQualName    = pure
                       , mapOnStructType  = pure
                       , mapOnPatternType = pure
                       }

-- | A data type for constructor patterns.  This is used to make the
-- code for detecting unmatched constructors cleaner, by separating
-- the constructor-pattern cases from other cases.
data ConstrPat = ConstrPat { constrName :: Name
                           , constrType :: PatternType
                           , constrPayload :: [Pattern]
                           , constrSrcLoc :: SrcLoc
                           }

-- Be aware of these fishy equality instances!

instance Eq ConstrPat where
  ConstrPat c1 _ _ _ == ConstrPat c2 _ _ _ = c1 == c2

instance Ord ConstrPat where
  ConstrPat c1 _ _ _ `compare` ConstrPat c2 _ _ _ = c1 `compare` c2

unmatched :: (Unmatched Pattern -> Unmatched Pattern) -> [Pattern] -> [Unmatched Pattern]
unmatched hole orig_ps
  | p:_ <- orig_ps,
    sameStructure labeledCols = do
    (i, cols) <- labeledCols
    let hole' = if isConstr p then hole else hole . wildPattern p i
    case sequence cols of
      Nothing -> []
      Just cs
        | all isPatternLit cs  -> map hole' $ localUnmatched cs
        | otherwise            -> unmatched hole' cs
  | otherwise = []

  where labeledCols = zip [1..] $ transpose $ map unpackPat orig_ps

        localUnmatched :: [Pattern] -> [Unmatched Pattern]
        localUnmatched [] = []
        localUnmatched ps'@(p':_) =
          case patternType p'  of
            Scalar (Sum cs'') ->
              -- We now know that we are matching a sum type, and thus
              -- that all patterns ps' are constructors (checked by
              -- 'all isPatternLit' before this function is called).
              let constrs   = M.keys cs''
                  matched   = mapMaybe constr ps'
                  unmatched' = map (UnmatchedConstr . buildConstr cs'') $
                               constrs \\ map constrName matched
             in case unmatched' of
                [] ->
                  let constrGroups   = group (sort matched)
                      removedConstrs = mapMaybe stripConstrs constrGroups
                      transposed     = (fmap . fmap) transpose removedConstrs
                      findUnmatched (pc, trans) = do
                        col <- trans
                        case col of
                          []           -> []
                          ((i, _):_) -> unmatched (wilder i pc) (map snd col)
                      wilder i pc s = (`PatternParens` noLoc) <$> wildPattern pc i s
                  in concatMap findUnmatched transposed
                _ -> unmatched'
            Scalar (Prim t) | not (any idOrWild ps') ->
              -- We now know that we are matching a sum type, and thus
              -- that all patterns ps' are literals (checked by 'all
              -- isPatternLit' before this function is called).
                case t of
                  Bool ->
                    let matched = nub $ mapMaybe (pExp >=> bool) $ filter isPatternLit ps'
                    in map (UnmatchedBool . buildBool (Scalar (Prim t))) $ [True, False] \\ matched
                  _ ->
                    let matched = mapMaybe pExp $ filter isPatternLit ps'
                    in [UnmatchedNum (buildId (Info $ Scalar $ Prim t) "p") matched]
            _ -> []

        isConstr PatternConstr{} = True
        isConstr (PatternParens p _) = isConstr p
        isConstr _ = False


        stripConstrs :: [ConstrPat] -> Maybe (Pattern, [[(Int, Pattern)]])
        stripConstrs (pc@ConstrPat{} : cs') = Just (unConstr pc, stripConstr pc : map stripConstr cs')
        stripConstrs [] = Nothing

        stripConstr :: ConstrPat -> [(Int, Pattern)]
        stripConstr (ConstrPat _ _  ps' _) = zip [1..] ps'

        sameStructure [] = True
        sameStructure (x:xs) = all (\y -> length y == length x' ) xs'
          where (x':xs') = map snd (x:xs)

        pExp (PatternLit e' _ _) = Just e'
        pExp _ = Nothing

        constr (PatternConstr c (Info t) ps loc) = Just $ ConstrPat c t ps loc
        constr (PatternParens p _) = constr p
        constr (PatternAscription p' _ _)  = constr p'
        constr _ = Nothing

        unConstr p =
          PatternConstr (constrName p) (Info $ constrType p) (constrPayload p) (constrSrcLoc p)

        isPatternLit PatternLit{} = True
        isPatternLit (PatternAscription p' _ _) = isPatternLit p'
        isPatternLit (PatternParens p' _)  = isPatternLit p'
        isPatternLit PatternConstr{} = True
        isPatternLit _ = False

        idOrWild Id{} = True
        idOrWild Wildcard{} = True
        idOrWild (PatternAscription p' _ _) = idOrWild p'
        idOrWild (PatternParens p' _) = idOrWild p'
        idOrWild _ = False

        bool (Literal (BoolValue b) _ ) = Just b
        bool _ = Nothing

        buildConstr m c =
          let t      = Scalar $ Sum m
              cs     = m M.! c
              wildCS = map (\ct -> Wildcard (Info ct) noLoc) cs
          in if null wildCS
               then PatternConstr c (Info t) [] noLoc
               else PatternParens (PatternConstr c (Info t) wildCS noLoc) noLoc
        buildBool t b =
          PatternLit (Literal (BoolValue b) noLoc) (Info (vacuousShapeAnnotations t)) noLoc
        buildId t n =
          -- The VName tag here will never be used since the value
          -- exists exclusively for printing warnings.
          Id (VName (nameFromString n) (-1)) t noLoc

checkIdent :: IdentBase NoInfo Name -> TermTypeM Ident
checkIdent (Ident name _ loc) = do
  (QualName _ name', vt) <- lookupVar loc (qualName name)
  return $ Ident name' (Info vt) loc

checkDimIndex :: DimIndexBase NoInfo Name -> TermTypeM DimIndex
checkDimIndex (DimFix i) =
  DimFix <$> (unifies "use as index" (Scalar $ Prim $ Signed Int32) =<< checkExp i)
checkDimIndex (DimSlice i j s) =
  DimSlice <$> check i <*> check j <*> check s
  where check = maybe (return Nothing) $
                fmap Just . unifies "use as index" (Scalar $ Prim $ Signed Int32) <=< checkExp

sequentially :: TermTypeM a -> (a -> Occurences -> TermTypeM b) -> TermTypeM b
sequentially m1 m2 = do
  (a, m1flow) <- collectOccurences m1
  (b, m2flow) <- collectOccurences $ m2 a m1flow
  occur $ m1flow `seqOccurences` m2flow
  return b

type Arg = (PatternType, Occurences, SrcLoc)

argType :: Arg -> PatternType
argType (t, _, _) = t

checkArg :: UncheckedExp -> TermTypeM (Exp, Arg)
checkArg arg = do
  (arg', dflow) <- collectOccurences $ checkExp arg
  arg_t <- expType arg'
  return (arg', (arg_t, dflow, srclocOf arg'))

checkApply :: SrcLoc -> PatternType -> Arg
           -> TermTypeM (PatternType, PatternType)
checkApply loc (Scalar (Arrow as _ tp1 tp2)) (argtype, dflow, argloc) = do
  unify (mkUsage argloc "use as function argument") (toStructural tp1) (toStructural argtype)

  -- Perform substitutions of instantiated variables in the types.
  tp1' <- normaliseType tp1
  tp2' <- normaliseType tp2
  argtype' <- normaliseType argtype

  occur [observation as loc]

  checkOccurences dflow
  occurs <- consumeArg argloc argtype' (diet tp1')

  case anyConsumption dflow of
    Just c ->
      let msg = "of value computed with consumption at " ++ locStr (location c)
      in zeroOrderType (mkUsage argloc "potential consumption in expression") msg tp1
    _ -> return ()

  occur $ dflow `seqOccurences` occurs
  let tp2'' = anyDimShapeAnnotations $ returnType tp2' (diet tp1') argtype'
  return (tp1', tp2'')

checkApply loc tfun@(Scalar TypeVar{}) arg = do
  tv <- newTypeVar loc "b"
  unify (mkUsage loc "use as function") (toStructural tfun) $
    Scalar $ Arrow mempty Unnamed (toStructural (argType arg)) tv
  constraints <- getConstraints
  checkApply loc (applySubst (`lookupSubst` constraints) tfun) arg

checkApply loc ftype arg =
  typeError loc $
  "Attempt to apply an expression of type " ++ pretty ftype ++
  " to an argument of type " ++ pretty (argType arg) ++ "."

-- | @returnType ret_type arg_diet arg_type@ gives result of applying
-- an argument the given types to a function with the given return
-- type, consuming the argument with the given diet.
returnType :: PatternType
           -> Diet
           -> PatternType
           -> PatternType
returnType (Array _ Unique et shape) _ _ =
  Array mempty Unique et shape
returnType (Array als Nonunique et shape) d arg =
  Array (als<>arg_als) Unique et shape -- Intentional!
  where arg_als = aliases $ maskAliases arg d
returnType (Scalar (Record fs)) d arg =
  Scalar $ Record $ fmap (\et -> returnType et d arg) fs
returnType (Scalar (Prim t)) _ _ =
  Scalar $ Prim t
returnType (Scalar (TypeVar _ Unique t targs)) _ _ =
  Scalar $ TypeVar mempty Unique t targs
returnType (Scalar (TypeVar als Nonunique t targs)) d arg =
  Scalar $ TypeVar (als<>arg_als) Unique t targs -- Intentional!
  where arg_als = aliases $ maskAliases arg d
returnType (Scalar (Arrow _ v t1 t2)) d arg =
  Scalar $ Arrow als v (t1 `setAliases` mempty) (t2 `setAliases` als)
  where als = aliases $ maskAliases arg d
returnType (Scalar (Sum cs)) d arg =
  Scalar $ Sum $ (fmap . fmap) (\et -> returnType et d arg) cs

-- | @t `maskAliases` d@ removes aliases (sets them to 'mempty') from
-- the parts of @t@ that are denoted as consumed by the 'Diet' @d@.
maskAliases :: Monoid as =>
               TypeBase shape as
            -> Diet
            -> TypeBase shape as
maskAliases t Consume = t `setAliases` mempty
maskAliases t Observe = t
maskAliases (Scalar (Record ets)) (RecordDiet ds) =
  Scalar $ Record $ M.intersectionWith maskAliases ets ds
maskAliases t FuncDiet{} = t
maskAliases _ _ = error "Invalid arguments passed to maskAliases."

consumeArg :: SrcLoc -> PatternType -> Diet -> TermTypeM [Occurence]
consumeArg loc (Scalar (Record ets)) (RecordDiet ds) =
  concat . M.elems <$> traverse (uncurry $ consumeArg loc) (M.intersectionWith (,) ets ds)
consumeArg loc (Array _ Nonunique _ _) Consume =
  typeError loc "Consuming parameter passed non-unique argument."
consumeArg loc (Scalar (Arrow _ _ t1 _)) (FuncDiet d _)
  | not $ contravariantArg t1 d =
      typeError loc "Non-consuming higher-order parameter passed consuming argument."
  where contravariantArg (Array _ Unique _ _) Observe =
          False
        contravariantArg (Scalar (TypeVar _ Unique _ _)) Observe =
          False
        contravariantArg (Scalar (Record ets)) (RecordDiet ds) =
          and (M.intersectionWith contravariantArg ets ds)
        contravariantArg (Scalar (Arrow _ _ tp tr)) (FuncDiet dp dr) =
          contravariantArg tp dp && contravariantArg tr dr
        contravariantArg _ _ =
          True
consumeArg loc (Scalar (Arrow _ _ _ t2)) (FuncDiet _ pd) =
  consumeArg loc t2 pd
consumeArg loc at Consume = return [consumption (aliases at) loc]
consumeArg loc at _       = return [observation (aliases at) loc]

checkOneExp :: UncheckedExp -> TypeM ([TypeParam], Exp)
checkOneExp e = fmap fst . runTermTypeM $ do
  e' <- checkExp e
  let t = toStruct $ typeOf e'
  tparams <- letGeneralise [] t
  fixOverloadedTypes
  e'' <- updateExpTypes e'
  return (tparams, e'')

-- | Type-check a top-level (or module-level) function definition.
-- Despite the name, this is also used for checking constant
-- definitions, by treating them as 0-ary functions.
checkFunDef :: (Name, Maybe UncheckedTypeExp,
                [UncheckedTypeParam], [UncheckedPattern],
                UncheckedExp, SrcLoc)
            -> TypeM (VName, [TypeParam], [Pattern], Maybe (TypeExp VName), StructType, Exp)
checkFunDef (fname, maybe_retdecl, tparams, params, body, loc) =
  fmap fst $ runTermTypeM $ do
  (tparams', params', maybe_retdecl', rettype', body') <-
    checkBinding (Just fname, maybe_retdecl, tparams, params, body, loc)

  -- Since this is a top-level function, we also resolve overloaded
  -- types, using either defaults or complaining about ambiguities.
  fixOverloadedTypes

  -- Then replace all inferred types in the body and parameters.
  body'' <- updateExpTypes body'
  params'' <- updateExpTypes params'
  maybe_retdecl'' <- traverse updateExpTypes maybe_retdecl'
  rettype'' <- normaliseType rettype'

  -- Check if pattern matches are exhaustive and yield
  -- errors if not.
  checkUnmatched body''

  bindSpaced [(Term, fname)] $ do
    fname' <- checkName Term fname loc
    when (nameToString fname `elem` doNotShadow) $
      typeError loc $ "The " ++ nameToString fname ++ " operator may not be redefined."

    return (fname', tparams', params'', maybe_retdecl'', rettype'', body'')

-- | This is "fixing" as in "setting them", not "correcting them".  We
-- only make very conservative fixing.
fixOverloadedTypes :: TermTypeM ()
fixOverloadedTypes = getConstraints >>= mapM_ fixOverloaded . M.toList . M.map snd
  where fixOverloaded (v, Overloaded ots usage)
          | Signed Int32 `elem` ots = do
              unify usage (Scalar (TypeVar () Nonunique (typeName v) [])) $
                Scalar $ Prim $ Signed Int32
              warn usage "Defaulting ambiguous type to `i32`."
          | FloatType Float64 `elem` ots = do
              unify usage (Scalar (TypeVar () Nonunique (typeName v) [])) $
                Scalar $ Prim $ FloatType Float64
              warn usage "Defaulting ambiguous type to `f64`."
          | otherwise =
              typeError usage $
              unlines ["Type is ambiguous (could be one of " ++ intercalate ", " (map pretty ots) ++ ").",
                       "Add a type annotation to disambiguate the type."]

        fixOverloaded (_, NoConstraint _ usage) =
          typeError usage $ unlines ["Type of expression is ambiguous.",
                                     "Add a type annotation to disambiguate the type."]

        fixOverloaded (_, Equality usage) =
          typeError usage $ unlines ["Type is ambiguous (must be equality type).",
                                     "Add a type annotation to disambiguate the type."]

        fixOverloaded (_, HasFields fs usage) =
          typeError usage $ unlines ["Type is ambiguous.  Must be record with fields:",
                                     unlines $ map field $ M.toList fs,
                                     "Add a type annotation to disambiguate the type."]
          where field (l, t) = pretty $ indent 2 $ ppr l <> colon <+> align (ppr t)

        fixOverloaded (_, HasConstrs cs usage) =
          typeError usage $ unlines [ "Type is ambiguous (must be a sum type with constructors: " ++
                                      pretty (Sum cs) ++ ")."
                                    , "Add a type annotation to disambiguate the type."]

        fixOverloaded _ = return ()

checkBinding :: (Maybe Name, Maybe UncheckedTypeExp,
                 [UncheckedTypeParam], [UncheckedPattern],
                 UncheckedExp, SrcLoc)
             -> TermTypeM ([TypeParam], [Pattern], Maybe (TypeExp VName), StructType, Exp)
checkBinding (fname, maybe_retdecl, tparams, params, body, loc) =
  noUnique $ incLevel $ bindingPatternGroup tparams params $ \tparams' params' -> do
    maybe_retdecl' <- traverse checkTypeExp maybe_retdecl

    body' <- checkFunBody body ((\(_,t,_)->t) <$> maybe_retdecl') (maybe loc srclocOf maybe_retdecl)

    params'' <- updateExpTypes params'
    body_t <- expType body'

    (maybe_retdecl'', rettype) <- case maybe_retdecl' of
      Just (retdecl', retdecl_type, _) -> do
        let rettype_structural = toStructural retdecl_type
        checkReturnAlias rettype_structural params'' body_t

        when (null params) $ nothingMustBeUnique loc rettype_structural

        warnOnDubiousShapeAnnotations loc params'' retdecl_type

        return (Just retdecl', retdecl_type)
      Nothing
        | null params ->
            return (Nothing, toStruct $ body_t `setUniqueness` Nonunique)
        | otherwise ->
            return (Nothing, inferReturnUniqueness params'' body_t)

    let fun_t = foldFunType (map patternStructType params'') rettype
    tparams'' <- letGeneralise tparams' fun_t

    checkGlobalAliases params'' body_t loc

    return (tparams'', params'', maybe_retdecl'', rettype, body')

  where -- | Check that unique return values do not alias a
        -- non-consumed parameter.
        checkReturnAlias rettp params' =
          foldM_ (checkReturnAlias' params') S.empty . returnAliasing rettp
        checkReturnAlias' params' seen (Unique, names)
          | any (`S.member` S.map snd seen) $ S.toList names =
              uniqueReturnAliased fname loc
          | otherwise = do
              notAliasingParam params' names
              return $ seen `S.union` tag Unique names
        checkReturnAlias' _ seen (Nonunique, names)
          | any (`S.member` seen) $ S.toList $ tag Unique names =
            uniqueReturnAliased fname loc
          | otherwise = return $ seen `S.union` tag Nonunique names

        notAliasingParam params' names =
          forM_ params' $ \p ->
          let consumedNonunique p' =
                not (unique $ unInfo $ identType p') && (identName p' `S.member` names)
          in case find consumedNonunique $ S.toList $ patternIdents p of
               Just p' ->
                 returnAliased fname (baseName $ identName p') loc
               Nothing ->
                 return ()

        tag u = S.map (u,)

        returnAliasing (Scalar (Record ets1)) (Scalar (Record ets2)) =
          concat $ M.elems $ M.intersectionWith returnAliasing ets1 ets2
        returnAliasing expected got =
          [(uniqueness expected, S.map aliasVar $ aliases got)]

warnOnDubiousShapeAnnotations :: SrcLoc -> [Pattern] -> StructType -> TermTypeM ()
warnOnDubiousShapeAnnotations loc params rettype =
  onDubiousNames $ S.filter patternNameButNotParamName $
  mconcat $ map typeDimNames $
  rettype : map patternStructType params
  where named (Named v) = Just v
        named Unnamed = Nothing
        param_names = S.fromList $ mapMaybe (named . fst . patternParam) params
        all_pattern_names = S.map identName $ mconcat $ map patternIdents params
        patternNameButNotParamName v = v `S.member` all_pattern_names && not (v `S.member` param_names)
        onDubiousNames dubious
          | S.null dubious = return ()
          | otherwise = warn loc $ unlines
                        [ "Size annotations in parameter and/or return type refers to the following names,"
                        , "which will not be visible to the caller, because they are nested in tuples or records:"
                        , "  " ++ intercalate ", " (map (quote . prettyName) $ S.toList dubious)
                        , "To eliminate this warning, make these names parameters on their own."]

checkGlobalAliases :: [Pattern] -> PatternType -> SrcLoc -> TermTypeM ()
checkGlobalAliases params body_t loc = do
  vtable <- asks $ scopeVtable . termScope
  let isLocal v = case v `M.lookup` vtable of
                    Just (BoundV Local _ _) -> True
                    _ -> False
  let als = filter (not . isLocal) $ S.toList $
            boundArrayAliases body_t `S.difference`
            S.map identName (mconcat (map patternIdents params))
  case als of
    v:_ | not $ null params ->
      typeError loc $
      unlines [ "Function result aliases the free variable " <>
                quote (prettyName v) <> "."
              , "Use " ++ quote "copy" ++ " to break the aliasing."]
    _ ->
      return ()


inferReturnUniqueness :: [Pattern] -> PatternType -> StructType
inferReturnUniqueness params t =
  let forbidden = aliasesMultipleTimes t
      uniques = uniqueParamNames params
      delve (Scalar (Record fs)) =
        Scalar $ Record $ M.map delve fs
      delve t'
        | all (`S.member` uniques) (boundArrayAliases t'),
          not $ any ((`S.member` forbidden) . aliasVar) (aliases t') =
            toStruct t'
        | otherwise =
            toStruct $ t' `setUniqueness` Nonunique
  in delve t

-- An alias inhibits uniqueness if it is used in disjoint values.
aliasesMultipleTimes :: PatternType -> Names
aliasesMultipleTimes = S.fromList . map fst . filter ((>1) . snd) . M.toList . delve
  where delve (Scalar (Record fs)) =
          foldl' (M.unionWith (+)) mempty $ map delve $ M.elems fs
        delve t =
          M.fromList $ zip (map aliasVar $ S.toList (aliases t)) $ repeat (1::Int)

uniqueParamNames :: [Pattern] -> Names
uniqueParamNames =
  S.fromList . map identName
  . filter (unique . unInfo . identType)
  . S.toList . mconcat . map patternIdents

boundArrayAliases :: PatternType -> S.Set VName
boundArrayAliases (Array als _ _ _) = boundAliases als
boundArrayAliases (Scalar Prim{}) = mempty
boundArrayAliases (Scalar (Record fs)) = foldMap boundArrayAliases fs
boundArrayAliases (Scalar (TypeVar als _ _ _)) = boundAliases als
boundArrayAliases (Scalar Arrow{}) = mempty
boundArrayAliases (Scalar (Sum fs)) =
  mconcat $ concatMap (map boundArrayAliases) $ M.elems fs

-- | The set of in-scope variables that are being aliased.
boundAliases :: Aliasing -> S.Set VName
boundAliases = S.map aliasVar . S.filter bound
  where bound AliasBound{} = True
        bound AliasFree{} = False

nothingMustBeUnique :: SrcLoc -> TypeBase () () -> TermTypeM ()
nothingMustBeUnique loc = check
  where check (Array _ Unique _ _) = bad
        check (Scalar (TypeVar _ Unique _ _)) = bad
        check (Scalar (Record fs)) = mapM_ check fs
        check _ = return ()
        bad = typeError loc "A top-level constant cannot have a unique type."

letGeneralise :: [TypeParam] -> StructType -> TermTypeM [TypeParam]
letGeneralise tparams t = do
  now_substs <- getConstraints
  -- Candidates for let-generalisation are those type variables that
  --
  -- (1) were not known before we checked this function, and
  --
  -- (2) are not used in the (new) definition of any type variables
  -- known before we checked this function.
  --
  -- (3) are not referenced from an overloaded type (for example,
  -- are the element types of an incompletely resolved record type).
  -- This is a bit more restrictive than I'd like, and SML for
  -- example does not have this restriction.
  --
  -- Criteria (1) and (2) is implemented by looking at the binding
  -- level of the type variables.
  let keep_type_vars = overloadedTypeVars now_substs

  cur_lvl <- curLevel
  let candiate k (lvl, _) = (k `S.notMember` keep_type_vars) && lvl == cur_lvl
      new_substs = M.filterWithKey candiate now_substs
  tparams' <- closeOverTypes new_substs tparams t

  -- We keep those type variables that were not closed over by
  -- let-generalisation.
  modifyConstraints $ M.filterWithKey $ \k _ -> k `notElem` map typeParamName tparams'

  return tparams'

checkFunBody :: ExpBase NoInfo Name
             -> Maybe StructType
             -> SrcLoc
             -> TermTypeM Exp
checkFunBody body maybe_rettype _loc = do
  body' <- checkExp body

  -- Unify body return type with return annotation, if one exists.
  case maybe_rettype of
    Just rettype -> do
      let rettype_structural = toStructural rettype
      void $ unifies "return type annotation" rettype_structural body'
      -- We also have to make sure that uniqueness matches.  This is done
      -- explicitly, because uniqueness is ignored by unification.
      rettype' <- normaliseType rettype
      body_t <- expType body'
      unless (body_t `subtypeOf` anyDimShapeAnnotations rettype') $
        typeError (srclocOf body) $ "Body type " ++ quote (pretty body_t) ++
        " is not a subtype of annotated type " ++
        quote (pretty rettype') ++ "."

    Nothing -> return ()

  return body'

-- | Find at all type variables in the given type that are covered by
-- the constraints, and produce type parameters that close over them.
--
-- The passed-in list of type parameters is always prepended to the
-- produced list of type parameters.
closeOverTypes :: Constraints -> [TypeParam] -> StructType -> TermTypeM [TypeParam]
closeOverTypes substs tparams t =
  fmap ((tparams++) . catMaybes) $ mapM closeOver $ M.toList $ M.map snd to_close_over
  where to_close_over = M.filterWithKey (\k _ -> k `S.member` visible) substs
        visible = typeVars t

        -- Avoid duplicate type parameters.
        closeOver (k, _)
          | k `elem` map typeParamName tparams =
              return Nothing
        closeOver (k, NoConstraint Unlifted usage) =
          return $ Just $ TypeParamType Unlifted k $ srclocOf usage
        closeOver (k, NoConstraint _ usage) =
          return $ Just $ TypeParamType Lifted k $ srclocOf usage
        closeOver (k, ParamType l loc) =
          return $ Just $ TypeParamType l k loc
        closeOver (_, _) =
          return Nothing

--- Consumption

occur :: Occurences -> TermTypeM ()
occur = tell

-- | Proclaim that we have made read-only use of the given variable.
observe :: Ident -> TermTypeM ()
observe (Ident nm (Info t) loc) =
  let als = AliasBound nm `S.insert` aliases t
  in occur [observation als loc]

-- | Proclaim that we have written to the given variable.
consume :: SrcLoc -> Aliasing -> TermTypeM ()
consume loc als = do
  vtable <- asks $ scopeVtable . termScope
  let consumable v = case M.lookup v vtable of
                       Just (BoundV Local _ t)
                         | arrayRank t > 0 -> unique t
                         | otherwise -> True
                       _ -> False
  case filter (not . consumable) $ map aliasVar $ S.toList als of
    v:_ -> typeError loc $ "Attempt to consume variable " ++ quote (prettyName v)
           ++ ", which is not allowed."
    [] -> occur [consumption als loc]

-- | Proclaim that we have written to the given variable, and mark
-- accesses to it and all of its aliases as invalid inside the given
-- computation.
consuming :: Ident -> TermTypeM a -> TermTypeM a
consuming (Ident name (Info t) loc) m = do
  consume loc $ AliasBound name `S.insert` aliases t
  localScope consume' m
  where consume' scope =
          scope { scopeVtable = M.insert name (WasConsumed loc) $ scopeVtable scope }

collectOccurences :: TermTypeM a -> TermTypeM (a, Occurences)
collectOccurences m = pass $ do
  (x, dataflow) <- listen m
  return ((x, dataflow), const mempty)

tapOccurences :: TermTypeM a -> TermTypeM (a, Occurences)
tapOccurences = listen

removeSeminullOccurences :: TermTypeM a -> TermTypeM a
removeSeminullOccurences = censor $ filter $ not . seminullOccurence

checkIfUsed :: Occurences -> Ident -> TermTypeM ()
checkIfUsed occs v
  | not $ identName v `S.member` allOccuring occs,
    not $ "_" `isPrefixOf` prettyName (identName v) =
      warn (srclocOf v) $ "Unused variable " ++ quote (pretty $ baseName $ identName v) ++ "."
  | otherwise =
      return ()

alternative :: TermTypeM a -> TermTypeM b -> TermTypeM (a,b)
alternative m1 m2 = pass $ do
  (x, occurs1) <- listen m1
  (y, occurs2) <- listen m2
  checkOccurences occurs1
  checkOccurences occurs2
  let usage = occurs1 `altOccurences` occurs2
  return ((x, y), const usage)

-- | Make all bindings nonunique.
noUnique :: TermTypeM a -> TermTypeM a
noUnique = localScope (\scope -> scope { scopeVtable = M.map set $ scopeVtable scope})
  where set (BoundV l tparams t)    = BoundV l tparams $ t `setUniqueness` Nonunique
        set (OverloadedF ts pts rt) = OverloadedF ts pts rt
        set EqualityF               = EqualityF
        set OpaqueF                 = OpaqueF
        set (WasConsumed loc)       = WasConsumed loc

onlySelfAliasing :: TermTypeM a -> TermTypeM a
onlySelfAliasing = localScope (\scope -> scope { scopeVtable = M.mapWithKey set $ scopeVtable scope})
  where set k (BoundV l tparams t)    = BoundV l tparams $
                                        t `addAliases` S.intersection (S.singleton (AliasBound k))
        set _ (OverloadedF ts pts rt) = OverloadedF ts pts rt
        set _ EqualityF               = EqualityF
        set _ OpaqueF                 = OpaqueF
        set _ (WasConsumed loc)       = WasConsumed loc

arrayOfM :: (Pretty (ShapeDecl dim), Monoid as) =>
            SrcLoc
         -> TypeBase dim as -> ShapeDecl dim -> Uniqueness
         -> TermTypeM (TypeBase dim as)
arrayOfM loc t shape u = do
  zeroOrderType (mkUsage loc "use as array element") "used in array" t
  return $ arrayOf t shape u

-- | Perform substitutions of instantiated variables on the type
-- annotations (including the instance lists) of an expression, or
-- something else.
updateExpTypes :: ASTMappable e => e -> TermTypeM e
updateExpTypes e = do
  constraints <- getConstraints
  let look = (`lookupSubst` constraints)
      tv = ASTMapper { mapOnExp         = astMap tv
                     , mapOnName        = pure
                     , mapOnQualName    = pure
                     , mapOnStructType  = pure . applySubst look
                     , mapOnPatternType = pure . applySubst look
                     }
  astMap tv e
