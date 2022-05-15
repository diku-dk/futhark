{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

-- | Facilities for type-checking terms.  Factored out of
-- "Language.Futhark.TypeChecker.Terms" to prevent the module from
-- being gigantic.
--
-- Incidentally also a nice place to put Haddock comments to make the
-- internal API of the type checker easier to browse.
module Language.Futhark.TypeChecker.Terms.Monad
  ( TermTypeM,
    runTermTypeM,
    liftTypeM,
    ValBinding (..),
    Locality (..),
    SizeSource (SourceBound, SourceSlice),
    NameReason (..),
    InferredType (..),
    Checking (..),
    withEnv,
    localScope,
    TermEnv (..),
    TermScope (..),
    TermTypeState (..),
    onFailure,
    extSize,
    expType,
    expTypeFully,
    constrain,
    newArrayType,
    allDimsFreshInType,
    updateTypes,

    -- * Primitive checking
    unifies,
    require,
    checkTypeExpNonrigid,
    checkTypeExpRigid,

    -- * Sizes
    isInt64,
    maybeDimFromExp,
    dimFromExp,
    dimFromArg,
    noSizeEscape,

    -- * Control flow
    collectOccurrences,
    tapOccurrences,
    alternative,
    sequentially,
    incLevel,

    -- * Consumption and uniqueness
    Names,
    Occurrence (..),
    Occurrences,
    onlySelfAliasing,
    noUnique,
    removeSeminullOccurrences,
    occur,
    observe,
    consume,
    consuming,
    observation,
    consumption,
    checkIfConsumable,
    seqOccurrences,
    checkOccurrences,
    allConsumed,

    -- * Errors
    useAfterConsume,
    unusedSize,
    notConsumable,
    uniqueReturnAliased,
    returnAliased,
    badLetWithValue,
    anyConsumption,
    allOccurring,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Bitraversable
import Data.Char (isAscii)
import Data.List (find, isPrefixOf, sort)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Futhark.Util.Pretty hiding (bool, group, space)
import Language.Futhark
import Language.Futhark.Semantic (includeToFilePath)
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Monad hiding (BoundV)
import qualified Language.Futhark.TypeChecker.Monad as TypeM
import Language.Futhark.TypeChecker.Types
import Language.Futhark.TypeChecker.Unify hiding (Usage)
import Prelude hiding (mod)

--- Uniqueness

data Usage
  = Consumed SrcLoc
  | Observed SrcLoc
  deriving (Eq, Ord, Show)

type Names = S.Set VName

-- | The consumption set is a Maybe so we can distinguish whether a
-- consumption took place, but the variable went out of scope since,
-- or no consumption at all took place.
data Occurrence = Occurrence
  { observed :: Names,
    consumed :: Maybe Names,
    location :: SrcLoc
  }
  deriving (Eq, Show)

instance Located Occurrence where
  locOf = locOf . location

observation :: Aliasing -> SrcLoc -> Occurrence
observation = flip Occurrence Nothing . S.map aliasVar

consumption :: Aliasing -> SrcLoc -> Occurrence
consumption = Occurrence S.empty . Just . S.map aliasVar

-- | A null occurence is one that we can remove without affecting
-- anything.
nullOccurrence :: Occurrence -> Bool
nullOccurrence occ = S.null (observed occ) && isNothing (consumed occ)

-- | A seminull occurence is one that does not contain references to
-- any variables in scope.  The big difference is that a seminull
-- occurence may denote a consumption, as long as the array that was
-- consumed is now out of scope.
seminullOccurrence :: Occurrence -> Bool
seminullOccurrence occ = S.null (observed occ) && maybe True S.null (consumed occ)

type Occurrences = [Occurrence]

type UsageMap = M.Map VName [Usage]

usageMap :: Occurrences -> UsageMap
usageMap = foldl comb M.empty
  where
    comb m (Occurrence obs cons loc) =
      let m' = S.foldl' (ins $ Observed loc) m obs
       in S.foldl' (ins $ Consumed loc) m' $ fromMaybe mempty cons
    ins v m k = M.insertWith (++) k [v] m

combineOccurrences :: VName -> Usage -> Usage -> TermTypeM Usage
combineOccurrences _ (Observed loc) (Observed _) = pure $ Observed loc
combineOccurrences name (Consumed wloc) (Observed rloc) =
  useAfterConsume name rloc wloc
combineOccurrences name (Observed rloc) (Consumed wloc) =
  useAfterConsume name rloc wloc
combineOccurrences name (Consumed loc1) (Consumed loc2) =
  useAfterConsume name (max loc1 loc2) (min loc1 loc2)

checkOccurrences :: Occurrences -> TermTypeM ()
checkOccurrences = void . M.traverseWithKey comb . usageMap
  where
    comb _ [] = pure ()
    comb name (u : us) = foldM_ (combineOccurrences name) u us

allObserved :: Occurrences -> Names
allObserved = S.unions . map observed

allConsumed :: Occurrences -> Names
allConsumed = S.unions . map (fromMaybe mempty . consumed)

allOccurring :: Occurrences -> Names
allOccurring occs = allConsumed occs <> allObserved occs

anyConsumption :: Occurrences -> Maybe Occurrence
anyConsumption = find (isJust . consumed)

seqOccurrences :: Occurrences -> Occurrences -> Occurrences
seqOccurrences occurs1 occurs2 =
  filter (not . nullOccurrence) $ map filt occurs1 ++ occurs2
  where
    filt occ =
      occ {observed = observed occ `S.difference` postcons}
    postcons = allConsumed occurs2

altOccurrences :: Occurrences -> Occurrences -> Occurrences
altOccurrences occurs1 occurs2 =
  filter (not . nullOccurrence) $ map filt1 occurs1 ++ map filt2 occurs2
  where
    filt1 occ =
      occ
        { consumed = S.difference <$> consumed occ <*> pure cons2,
          observed = observed occ `S.difference` cons2
        }
    filt2 occ =
      occ
        { consumed = consumed occ,
          observed = observed occ `S.difference` cons1
        }
    cons1 = allConsumed occurs1
    cons2 = allConsumed occurs2

-- | How something was bound.
data Locality
  = -- | In the current function
    Local
  | -- | In an enclosing function, but not the current one.
    Nonlocal
  | -- | At global scope.
    Global
  deriving (Show, Eq, Ord)

data ValBinding
  = -- | Aliases in parameters indicate the lexical
    -- closure.
    BoundV Locality [TypeParam] PatType
  | OverloadedF [PrimType] [Maybe PrimType] (Maybe PrimType)
  | EqualityF
  | WasConsumed SrcLoc
  deriving (Show)

--- Errors

describeVar :: SrcLoc -> VName -> TermTypeM Doc
describeVar loc v =
  gets $
    maybe ("variable" <+> pquote (pprName v)) (nameReason loc)
      . M.lookup v
      . stateNames

useAfterConsume :: VName -> SrcLoc -> SrcLoc -> TermTypeM a
useAfterConsume name rloc wloc = do
  name' <- describeVar rloc name
  typeError rloc mempty . withIndexLink "use-after-consume" $
    "Using" <+> name' <> ", but this was consumed at"
      <+> text (locStrRel rloc wloc) <> ".  (Possibly through aliasing.)"

badLetWithValue :: (Pretty arr, Pretty src) => arr -> src -> SrcLoc -> TermTypeM a
badLetWithValue arre vale loc =
  typeError loc mempty $
    "Source array for in-place update"
      </> indent 2 (ppr arre)
      </> "might alias update value"
      </> indent 2 (ppr vale)
      </> "Hint: use" <+> pquote "copy" <+> "to remove aliases from the value."

returnAliased :: Name -> SrcLoc -> TermTypeM ()
returnAliased name loc =
  typeError loc mempty . withIndexLink "return-aliased" $
    "Unique-typed return value is aliased to"
      <+> pquote (pprName name) <> ", which is not consumable."

uniqueReturnAliased :: SrcLoc -> TermTypeM a
uniqueReturnAliased loc =
  typeError loc mempty . withIndexLink "unique-return-aliased" $
    "A unique-typed component of the return value is aliased to some other component."

notConsumable :: MonadTypeChecker m => SrcLoc -> Doc -> m b
notConsumable loc v =
  typeError loc mempty . withIndexLink "not-consumable" $
    "Would consume" <+> v <> ", which is not consumable."

unusedSize :: (MonadTypeChecker m) => SizeBinder VName -> m a
unusedSize p =
  typeError p mempty . withIndexLink "unused-size" $
    "Size" <+> ppr p <+> "unused in pattern."

--- Scope management

data InferredType
  = NoneInferred
  | Ascribed PatType

data Checking
  = CheckingApply (Maybe (QualName VName)) Exp StructType StructType
  | CheckingReturn StructType StructType
  | CheckingAscription StructType StructType
  | CheckingLetGeneralise Name
  | CheckingParams (Maybe Name)
  | CheckingPat UncheckedPat InferredType
  | CheckingLoopBody StructType StructType
  | CheckingLoopInitial StructType StructType
  | CheckingRecordUpdate [Name] StructType StructType
  | CheckingRequired [StructType] StructType
  | CheckingBranches StructType StructType

instance Pretty Checking where
  ppr (CheckingApply f e expected actual) =
    header
      </> "Expected:" <+> align (ppr expected)
      </> "Actual:  " <+> align (ppr actual)
    where
      header =
        case f of
          Nothing ->
            "Cannot apply function to"
              <+> pquote (shorten $ pretty $ flatten $ ppr e) <> " (invalid type)."
          Just fname ->
            "Cannot apply" <+> pquote (ppr fname) <+> "to"
              <+> pquote (shorten $ pretty $ flatten $ ppr e) <> " (invalid type)."
  ppr (CheckingReturn expected actual) =
    "Function body does not have expected type."
      </> "Expected:" <+> align (ppr expected)
      </> "Actual:  " <+> align (ppr actual)
  ppr (CheckingAscription expected actual) =
    "Expression does not have expected type from explicit ascription."
      </> "Expected:" <+> align (ppr expected)
      </> "Actual:  " <+> align (ppr actual)
  ppr (CheckingLetGeneralise fname) =
    "Cannot generalise type of" <+> pquote (ppr fname) <> "."
  ppr (CheckingParams fname) =
    "Invalid use of parameters in" <+> pquote fname' <> "."
    where
      fname' = maybe "anonymous function" ppr fname
  ppr (CheckingPat pat NoneInferred) =
    "Invalid pattern" <+> pquote (ppr pat) <> "."
  ppr (CheckingPat pat (Ascribed t)) =
    "Pat" <+> pquote (ppr pat)
      <+> "cannot match value of type"
      </> indent 2 (ppr t)
  ppr (CheckingLoopBody expected actual) =
    "Loop body does not have expected type."
      </> "Expected:" <+> align (ppr expected)
      </> "Actual:  " <+> align (ppr actual)
  ppr (CheckingLoopInitial expected actual) =
    "Initial loop values do not have expected type."
      </> "Expected:" <+> align (ppr expected)
      </> "Actual:  " <+> align (ppr actual)
  ppr (CheckingRecordUpdate fs expected actual) =
    "Type mismatch when updating record field" <+> pquote fs' <> "."
      </> "Existing:" <+> align (ppr expected)
      </> "New:     " <+> align (ppr actual)
    where
      fs' = mconcat $ punctuate "." $ map ppr fs
  ppr (CheckingRequired [expected] actual) =
    "Expression must must have type" <+> ppr expected <> "."
      </> "Actual type:" <+> align (ppr actual)
  ppr (CheckingRequired expected actual) =
    "Type of expression must must be one of " <+> expected' <> "."
      </> "Actual type:" <+> align (ppr actual)
    where
      expected' = commasep (map ppr expected)
  ppr (CheckingBranches t1 t2) =
    "Branches differ in type."
      </> "Former:" <+> ppr t1
      </> "Latter:" <+> ppr t2

-- | Type checking happens with access to this environment.  The
-- 'TermScope' will be extended during type-checking as bindings come into
-- scope.
data TermEnv = TermEnv
  { termScope :: TermScope,
    termChecking :: Maybe Checking,
    termLevel :: Level
  }

data TermScope = TermScope
  { scopeVtable :: M.Map VName ValBinding,
    scopeTypeTable :: M.Map VName TypeBinding,
    scopeModTable :: M.Map VName Mod,
    scopeNameMap :: NameMap
  }
  deriving (Show)

instance Semigroup TermScope where
  TermScope vt1 tt1 mt1 nt1 <> TermScope vt2 tt2 mt2 nt2 =
    TermScope (vt2 `M.union` vt1) (tt2 `M.union` tt1) (mt1 `M.union` mt2) (nt2 `M.union` nt1)

envToTermScope :: Env -> TermScope
envToTermScope env =
  TermScope
    { scopeVtable = vtable,
      scopeTypeTable = envTypeTable env,
      scopeNameMap = envNameMap env,
      scopeModTable = envModTable env
    }
  where
    vtable = M.mapWithKey valBinding $ envVtable env
    valBinding k (TypeM.BoundV tps v) =
      BoundV Global tps $ selfAliasing (S.singleton (AliasBound k)) v
    -- FIXME: hack, #1675
    selfAliasing als (Scalar (Record ts)) =
      Scalar $ Record $ M.map (selfAliasing als) ts
    selfAliasing als t =
      t `setAliases` (if arrayRank t > 0 then als else mempty)

withEnv :: TermEnv -> Env -> TermEnv
withEnv tenv env = tenv {termScope = termScope tenv <> envToTermScope env}

-- | Wrap a function name to give it a vacuous Eq instance for SizeSource.
newtype FName = FName (Maybe (QualName VName))
  deriving (Show)

instance Eq FName where
  _ == _ = True

instance Ord FName where
  compare _ _ = EQ

-- | What was the source of some existential size?  This is used for
-- using the same existential variable if the same source is
-- encountered in multiple locations.
data SizeSource
  = SourceArg FName (ExpBase NoInfo VName)
  | SourceBound (ExpBase NoInfo VName)
  | SourceSlice
      (Maybe (DimDecl VName))
      (Maybe (ExpBase NoInfo VName))
      (Maybe (ExpBase NoInfo VName))
      (Maybe (ExpBase NoInfo VName))
  deriving (Eq, Ord, Show)

-- | A description of where an artificial compiler-generated
-- intermediate name came from.
data NameReason
  = -- | Name is the result of a function application.
    NameAppRes (Maybe (QualName VName)) SrcLoc

nameReason :: SrcLoc -> NameReason -> Doc
nameReason loc (NameAppRes Nothing apploc) =
  "result of application at" <+> text (locStrRel loc apploc)
nameReason loc (NameAppRes fname apploc) =
  "result of applying" <+> pquote (ppr fname)
    <+> parens ("at" <+> text (locStrRel loc apploc))

-- | The state is a set of constraints and a counter for generating
-- type names.  This is distinct from the usual counter we use for
-- generating unique names, as these will be user-visible.
data TermTypeState = TermTypeState
  { stateConstraints :: Constraints,
    stateCounter :: !Int,
    -- | Mapping function arguments encountered to
    -- the sizes they ended up generating (when
    -- they could not be substituted directly).
    -- This happens for function arguments that are
    -- not constants or names.
    stateDimTable :: M.Map SizeSource VName,
    stateNames :: M.Map VName NameReason,
    stateOccs :: Occurrences
  }

newtype TermTypeM a
  = TermTypeM (ReaderT TermEnv (StateT TermTypeState TypeM) a)
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader TermEnv,
      MonadState TermTypeState,
      MonadError TypeError
    )

liftTypeM :: TypeM a -> TermTypeM a
liftTypeM = TermTypeM . lift . lift

incCounter :: TermTypeM Int
incCounter = do
  s <- get
  put s {stateCounter = stateCounter s + 1}
  pure $ stateCounter s

constrain :: VName -> Constraint -> TermTypeM ()
constrain v c = do
  lvl <- curLevel
  modifyConstraints $ M.insert v (lvl, c)

instance MonadUnify TermTypeM where
  getConstraints = gets stateConstraints
  putConstraints x = modify $ \s -> s {stateConstraints = x}

  newTypeVar loc desc = do
    i <- incCounter
    v <- newID $ mkTypeVarName desc i
    constrain v $ NoConstraint Lifted $ mkUsage' loc
    pure $ Scalar $ TypeVar mempty Nonunique (typeName v) []

  curLevel = asks termLevel

  newDimVar loc rigidity name = do
    dim <- newTypeName name
    case rigidity of
      Rigid rsrc -> constrain dim $ UnknowableSize loc rsrc
      Nonrigid -> constrain dim $ Size Nothing $ mkUsage' loc
    pure dim

  unifyError loc notes bcs doc = do
    checking <- asks termChecking
    case checking of
      Just checking' ->
        throwError $
          TypeError (locOf loc) notes $
            ppr checking' <> line </> doc <> ppr bcs
      Nothing ->
        throwError $ TypeError (locOf loc) notes $ doc <> ppr bcs

  matchError loc notes bcs t1 t2 = do
    checking <- asks termChecking
    case checking of
      Just checking'
        | hasNoBreadCrumbs bcs ->
            throwError $
              TypeError (locOf loc) notes $
                ppr checking'
        | otherwise ->
            throwError $
              TypeError (locOf loc) notes $
                ppr checking' <> line </> doc <> ppr bcs
      Nothing ->
        throwError $ TypeError (locOf loc) notes $ doc <> ppr bcs
    where
      doc =
        "Types"
          </> indent 2 (ppr t1)
          </> "and"
          </> indent 2 (ppr t2)
          </> "do not match."

-- | Instantiate a type scheme with fresh type variables for its type
-- parameters. Returns the names of the fresh type variables, the
-- instance list, and the instantiated type.
instantiateTypeScheme ::
  QualName VName ->
  SrcLoc ->
  [TypeParam] ->
  PatType ->
  TermTypeM ([VName], PatType)
instantiateTypeScheme qn loc tparams t = do
  let tnames = map typeParamName tparams
  (tparam_names, tparam_substs) <- unzip <$> mapM (instantiateTypeParam qn loc) tparams
  let substs = M.fromList $ zip tnames tparam_substs
      t' = applySubst (`M.lookup` substs) t
  pure (tparam_names, t')

-- | Create a new type name and insert it (unconstrained) in the
-- substitution map.
instantiateTypeParam ::
  Monoid as =>
  QualName VName ->
  SrcLoc ->
  TypeParam ->
  TermTypeM (VName, Subst (RetTypeBase dim as))
instantiateTypeParam qn loc tparam = do
  i <- incCounter
  let name = nameFromString (takeWhile isAscii (baseString (typeParamName tparam)))
  v <- newID $ mkTypeVarName name i
  case tparam of
    TypeParamType x _ _ -> do
      constrain v . NoConstraint x . mkUsage loc $
        "instantiated type parameter of " <> quote (pretty qn) <> "."
      pure (v, Subst [] $ RetType [] $ Scalar $ TypeVar mempty Nonunique (typeName v) [])
    TypeParamDim {} -> do
      constrain v . Size Nothing . mkUsage loc $
        "instantiated size parameter of " <> quote (pretty qn) <> "."
      pure (v, SizeSubst $ NamedDim $ qualName v)

checkQualNameWithEnv :: Namespace -> QualName Name -> SrcLoc -> TermTypeM (TermScope, QualName VName)
checkQualNameWithEnv space qn@(QualName quals name) loc = do
  scope <- asks termScope
  descend scope quals
  where
    descend scope []
      | Just name' <- M.lookup (space, name) $ scopeNameMap scope =
          pure (scope, name')
      | otherwise =
          unknownVariable space qn loc
    descend scope (q : qs)
      | Just (QualName _ q') <- M.lookup (Term, q) $ scopeNameMap scope,
        Just res <- M.lookup q' $ scopeModTable scope =
          case res of
            -- Check if we are referring to the magical intrinsics
            -- module.
            _
              | baseTag q' <= maxIntrinsicTag ->
                  checkIntrinsic space qn loc
            ModEnv q_scope -> do
              (scope', QualName qs' name') <- descend (envToTermScope q_scope) qs
              pure (scope', QualName (q' : qs') name')
            ModFun {} -> unappliedFunctor loc
      | otherwise =
          unknownVariable space qn loc

checkIntrinsic :: Namespace -> QualName Name -> SrcLoc -> TermTypeM (TermScope, QualName VName)
checkIntrinsic space qn@(QualName _ name) loc
  | Just v <- M.lookup (space, name) intrinsicsNameMap = do
      me <- liftTypeM askImportName
      unless (isBuiltin (includeToFilePath me)) $
        warn loc "Using intrinsic functions directly can easily crash the compiler or result in wrong code generation."
      scope <- asks termScope
      pure (scope, v)
  | otherwise =
      unknownVariable space qn loc

localScope :: (TermScope -> TermScope) -> TermTypeM a -> TermTypeM a
localScope f = local $ \tenv -> tenv {termScope = f $ termScope tenv}

instance MonadTypeChecker TermTypeM where
  warn loc problem = liftTypeM $ warn loc problem
  newName = liftTypeM . newName
  newID = liftTypeM . newID

  newTypeName name = do
    i <- incCounter
    newID $ mkTypeVarName name i

  checkQualName space name loc = snd <$> checkQualNameWithEnv space name loc

  bindNameMap m = localScope $ \scope ->
    scope {scopeNameMap = m <> scopeNameMap scope}

  bindVal v (TypeM.BoundV tps t) = localScope $ \scope ->
    scope {scopeVtable = M.insert v vb $ scopeVtable scope}
    where
      vb = BoundV Local tps $ fromStruct t

  lookupType loc qn = do
    outer_env <- liftTypeM askEnv
    (scope, qn'@(QualName qs name)) <- checkQualNameWithEnv Type qn loc
    case M.lookup name $ scopeTypeTable scope of
      Nothing -> unknownType loc qn
      Just (TypeAbbr l ps (RetType dims def)) ->
        pure
          ( qn',
            ps,
            RetType dims $ qualifyTypeVars outer_env (map typeParamName ps) qs def,
            l
          )

  lookupMod loc qn = do
    (scope, qn'@(QualName _ name)) <- checkQualNameWithEnv Term qn loc
    case M.lookup name $ scopeModTable scope of
      Nothing -> unknownVariable Term qn loc
      Just m -> pure (qn', m)

  lookupVar loc qn = do
    outer_env <- liftTypeM askEnv
    (scope, qn'@(QualName qs name)) <- checkQualNameWithEnv Term qn loc
    let usage = mkUsage loc $ "use of " ++ quote (pretty qn)

    t <- case M.lookup name $ scopeVtable scope of
      Nothing ->
        typeError loc mempty $
          "Unknown variable" <+> pquote (ppr qn) <> "."
      Just (WasConsumed wloc) -> useAfterConsume name loc wloc
      Just (BoundV _ tparams t)
        | "_" `isPrefixOf` baseString name -> underscoreUse loc qn
        | otherwise -> do
            (tnames, t') <- instantiateTypeScheme qn' loc tparams t
            pure $ qualifyTypeVars outer_env tnames qs t'
      Just EqualityF -> do
        argtype <- newTypeVar loc "t"
        equalityType usage argtype
        pure $
          Scalar . Arrow mempty Unnamed argtype . RetType [] $
            Scalar $ Arrow mempty Unnamed argtype $ RetType [] $ Scalar $ Prim Bool
      Just (OverloadedF ts pts rt) -> do
        argtype <- newTypeVar loc "t"
        mustBeOneOf ts usage argtype
        let (pts', rt') = instOverloaded argtype pts rt
            arrow xt yt = Scalar $ Arrow mempty Unnamed xt $ RetType [] yt
        pure $ fromStruct $ foldr arrow rt' pts'

    observe $ Ident name (Info t) loc
    pure (qn', t)
    where
      instOverloaded argtype pts rt =
        ( map (maybe (toStruct argtype) (Scalar . Prim)) pts,
          maybe (toStruct argtype) (Scalar . Prim) rt
        )

  checkNamedDim loc v = do
    (v', t) <- lookupVar loc v
    onFailure (CheckingRequired [Scalar $ Prim $ Signed Int64] (toStruct t)) $
      unify (mkUsage loc "use as array size") (toStruct t) $
        Scalar $ Prim $ Signed Int64
    pure v'

  typeError loc notes s = do
    checking <- asks termChecking
    case checking of
      Just checking' ->
        throwError $ TypeError (locOf loc) notes (ppr checking' <> line </> s)
      Nothing ->
        throwError $ TypeError (locOf loc) notes s

onFailure :: Checking -> TermTypeM a -> TermTypeM a
onFailure c = local $ \env -> env {termChecking = Just c}

extSize :: SrcLoc -> SizeSource -> TermTypeM (DimDecl VName, Maybe VName)
extSize loc e = do
  prev <- gets $ M.lookup e . stateDimTable
  case prev of
    Nothing -> do
      let rsrc = case e of
            SourceArg (FName fname) e' ->
              RigidArg fname $ prettyOneLine e'
            SourceBound e' ->
              RigidBound $ prettyOneLine e'
            SourceSlice d i j s ->
              RigidSlice d $ prettyOneLine $ DimSlice i j s
      d <- newDimVar loc (Rigid rsrc) "n"
      modify $ \s -> s {stateDimTable = M.insert e d $ stateDimTable s}
      pure
        ( NamedDim $ qualName d,
          Just d
        )
    Just d ->
      pure
        ( NamedDim $ qualName d,
          Just d
        )

incLevel :: TermTypeM a -> TermTypeM a
incLevel = local $ \env -> env {termLevel = termLevel env + 1}

-- | Get the type of an expression, with top level type variables
-- substituted.  Never call 'typeOf' directly (except in a few
-- carefully inspected locations)!
expType :: Exp -> TermTypeM PatType
expType = normPatType . typeOf

-- | Get the type of an expression, with all type variables
-- substituted.  Slower than 'expType', but sometimes necessary.
-- Never call 'typeOf' directly (except in a few carefully inspected
-- locations)!
expTypeFully :: Exp -> TermTypeM PatType
expTypeFully = normTypeFully . typeOf

newArrayType :: SrcLoc -> Name -> Int -> TermTypeM (StructType, StructType)
newArrayType loc desc r = do
  v <- newTypeName desc
  constrain v $ NoConstraint Unlifted $ mkUsage' loc
  dims <- replicateM r $ newDimVar loc Nonrigid "dim"
  let rowt = TypeVar () Nonunique (typeName v) []
  pure
    ( Array () Nonunique (ShapeDecl $ map (NamedDim . qualName) dims) rowt,
      Scalar rowt
    )

-- | Replace *all* dimensions with distinct fresh size variables.
allDimsFreshInType ::
  SrcLoc ->
  Rigidity ->
  Name ->
  TypeBase (DimDecl VName) als ->
  TermTypeM (TypeBase (DimDecl VName) als, M.Map VName (DimDecl VName))
allDimsFreshInType loc r desc t =
  runStateT (bitraverse onDim pure t) mempty
  where
    onDim d = do
      v <- lift $ newDimVar loc r desc
      modify $ M.insert v d
      pure $ NamedDim $ qualName v

-- | Replace all type variables with their concrete types.
updateTypes :: ASTMappable e => e -> TermTypeM e
updateTypes = astMap tv
  where
    tv =
      ASTMapper
        { mapOnExp = astMap tv,
          mapOnName = pure,
          mapOnQualName = pure,
          mapOnStructType = normTypeFully,
          mapOnPatType = normTypeFully,
          mapOnStructRetType = normTypeFully,
          mapOnPatRetType = normTypeFully
        }

--- Basic checking

unifies :: String -> StructType -> Exp -> TermTypeM Exp
unifies why t e = do
  unify (mkUsage (srclocOf e) why) t . toStruct =<< expType e
  pure e

-- | @require ts e@ causes a 'TypeError' if @expType e@ is not one of
-- the types in @ts@.  Otherwise, simply returns @e@.
require :: String -> [PrimType] -> Exp -> TermTypeM Exp
require why ts e = do
  mustBeOneOf ts (mkUsage (srclocOf e) why) . toStruct =<< expType e
  pure e

termCheckTypeExp :: TypeExp Name -> TermTypeM (TypeExp VName, [VName], StructRetType)
termCheckTypeExp te = do
  (te', svars, rettype, _l) <- checkTypeExp te

  -- No guarantee that the locally bound sizes in rettype are globally
  -- unique, but we want to turn them into size variables, so let's
  -- give them some unique names.  Maybe this should be done below,
  -- where we actually turn these into size variables?
  RetType dims st <- renameRetType rettype

  -- Observe the sizes so we do not get any warnings about them not
  -- being used.
  mapM_ observeDim $ nestedDims st
  pure (te', svars, RetType dims st)
  where
    observeDim (NamedDim v) =
      observe $ Ident (qualLeaf v) (Info $ Scalar $ Prim $ Signed Int64) mempty
    observeDim _ = pure ()

checkTypeExpNonrigid :: TypeExp Name -> TermTypeM (TypeExp VName, StructType, [VName])
checkTypeExpNonrigid te = do
  (te', svars, RetType dims st) <- termCheckTypeExp te
  forM_ (svars ++ dims) $ \v ->
    constrain v $ Size Nothing $ mkUsage (srclocOf te) "anonymous size in type expression"
  pure (te', st, svars ++ dims)

checkTypeExpRigid ::
  TypeExp Name ->
  RigidSource ->
  TermTypeM (TypeExp VName, StructType, [VName])
checkTypeExpRigid te rsrc = do
  (te', svars, RetType dims st) <- termCheckTypeExp te
  forM_ (svars ++ dims) $ \v ->
    constrain v $ UnknowableSize (srclocOf te) rsrc
  pure (te', st, svars ++ dims)

--- Sizes

isInt64 :: Exp -> Maybe Int64
isInt64 (Literal (SignedValue (Int64Value k')) _) = Just $ fromIntegral k'
isInt64 (IntLit k' _ _) = Just $ fromInteger k'
isInt64 (Negate x _) = negate <$> isInt64 x
isInt64 _ = Nothing

maybeDimFromExp :: Exp -> Maybe (DimDecl VName)
maybeDimFromExp (Var v _ _) = Just $ NamedDim v
maybeDimFromExp (Parens e _) = maybeDimFromExp e
maybeDimFromExp (QualParens _ e _) = maybeDimFromExp e
maybeDimFromExp e = ConstDim . fromIntegral <$> isInt64 e

dimFromExp :: (Exp -> SizeSource) -> Exp -> TermTypeM (DimDecl VName, Maybe VName)
dimFromExp rf (Attr _ e _) = dimFromExp rf e
dimFromExp rf (Assert _ e _ _) = dimFromExp rf e
dimFromExp rf (Parens e _) = dimFromExp rf e
dimFromExp rf (QualParens _ e _) = dimFromExp rf e
dimFromExp rf e
  | Just d <- maybeDimFromExp e =
      pure (d, Nothing)
  | otherwise =
      extSize (srclocOf e) $ rf e

dimFromArg :: Maybe (QualName VName) -> Exp -> TermTypeM (DimDecl VName, Maybe VName)
dimFromArg fname = dimFromExp $ SourceArg (FName fname) . bareExp

-- | Any argument sizes created with 'extSize' inside the given action
-- will be removed once the action finishes.  This is to ensure that
-- just because e.g. @n+1@ appears as a size in one branch of a
-- conditional, that doesn't mean it's also available in the other
-- branch.
noSizeEscape :: TermTypeM a -> TermTypeM a
noSizeEscape m = do
  dimtable <- gets stateDimTable
  x <- m
  modify $ \s -> s {stateDimTable = dimtable}
  pure x

--- Control flow

tapOccurrences :: TermTypeM a -> TermTypeM (a, Occurrences)
tapOccurrences m = do
  (x, occs) <- collectOccurrences m
  occur occs
  pure (x, occs)

collectOccurrences :: TermTypeM a -> TermTypeM (a, Occurrences)
collectOccurrences m = do
  old <- gets stateOccs
  modify $ \s -> s {stateOccs = mempty}
  x <- m
  new <- gets stateOccs
  modify $ \s -> s {stateOccs = old}
  pure (x, new)

alternative :: TermTypeM a -> TermTypeM b -> TermTypeM (a, b)
alternative m1 m2 = do
  (x, occurs1) <- collectOccurrences $ noSizeEscape m1
  (y, occurs2) <- collectOccurrences $ noSizeEscape m2
  checkOccurrences occurs1
  checkOccurrences occurs2
  occur $ occurs1 `altOccurrences` occurs2
  pure (x, y)

sequentially :: TermTypeM a -> (a -> Occurrences -> TermTypeM b) -> TermTypeM b
sequentially m1 m2 = do
  (a, m1flow) <- collectOccurrences m1
  (b, m2flow) <- collectOccurrences $ m2 a m1flow
  occur $ m1flow `seqOccurrences` m2flow
  pure b

--- Consumption

occur :: Occurrences -> TermTypeM ()
occur occs = modify $ \s -> s {stateOccs = stateOccs s <> occs}

-- | Proclaim that we have made read-only use of the given variable.
observe :: Ident -> TermTypeM ()
observe (Ident nm (Info t) loc) =
  let als = AliasBound nm `S.insert` aliases t
   in occur [observation als loc]

onlySelfAliasing :: TermTypeM a -> TermTypeM a
onlySelfAliasing = localScope (\scope -> scope {scopeVtable = M.mapWithKey set $ scopeVtable scope})
  where
    set k (BoundV l tparams t) =
      BoundV l tparams $
        t `addAliases` S.intersection (S.singleton (AliasBound k))
    set _ (OverloadedF ts pts rt) = OverloadedF ts pts rt
    set _ EqualityF = EqualityF
    set _ (WasConsumed loc) = WasConsumed loc

-- | Enter a context where nothing outside can be consumed (i.e. the
-- body of a function definition).
noUnique :: TermTypeM a -> TermTypeM a
noUnique m = do
  (x, occs) <- collectOccurrences $ localScope f m
  checkOccurrences occs
  occur $ fst $ split occs
  pure x
  where
    f scope = scope {scopeVtable = M.map set $ scopeVtable scope}

    set (BoundV l tparams t) = BoundV (max l Nonlocal) tparams t
    set (OverloadedF ts pts rt) = OverloadedF ts pts rt
    set EqualityF = EqualityF
    set (WasConsumed loc) = WasConsumed loc

    split = unzip . map (\occ -> (occ {consumed = mempty}, occ {observed = mempty}))

removeSeminullOccurrences :: TermTypeM a -> TermTypeM a
removeSeminullOccurrences m = do
  (x, occs) <- collectOccurrences m
  occur $ filter (not . seminullOccurrence) occs
  pure x

checkIfConsumable :: SrcLoc -> Aliasing -> TermTypeM ()
checkIfConsumable loc als = do
  vtable <- asks $ scopeVtable . termScope
  let consumable v = case M.lookup v vtable of
        Just (BoundV Local _ t)
          | Scalar Arrow {} <- t -> False
          | otherwise -> True
        Just (BoundV l _ _) -> l == Local
        _ -> True
  -- The sort ensures that AliasBound vars are shown before AliasFree.
  case map aliasVar $ sort $ filter (not . consumable . aliasVar) $ S.toList als of
    v : _ -> notConsumable loc =<< describeVar loc v
    [] -> pure ()

-- | Proclaim that we have written to the given variable.
consume :: SrcLoc -> Aliasing -> TermTypeM ()
consume loc als = do
  checkIfConsumable loc als
  occur [consumption als loc]

-- | Proclaim that we have written to the given variable, and mark
-- accesses to it and all of its aliases as invalid inside the given
-- computation.
consuming :: Ident -> TermTypeM a -> TermTypeM a
consuming (Ident name (Info t) loc) m = do
  t' <- normTypeFully t
  consume loc $ AliasBound name `S.insert` aliases t'
  localScope consume' m
  where
    consume' scope =
      scope {scopeVtable = M.insert name (WasConsumed loc) $ scopeVtable scope}

-- Running

initialTermScope :: TermScope
initialTermScope =
  TermScope
    { scopeVtable = initialVtable,
      scopeTypeTable = mempty,
      scopeNameMap = topLevelNameMap,
      scopeModTable = mempty
    }
  where
    initialVtable = M.fromList $ mapMaybe addIntrinsicF $ M.toList intrinsics

    prim = Scalar . Prim
    arrow x y = Scalar $ Arrow mempty Unnamed x y

    addIntrinsicF (name, IntrinsicMonoFun pts t) =
      Just (name, BoundV Global [] $ arrow pts' $ RetType [] $ prim t)
      where
        pts' = case pts of
          [pt] -> prim pt
          _ -> Scalar $ tupleRecord $ map prim pts
    addIntrinsicF (name, IntrinsicOverloadedFun ts pts rts) =
      Just (name, OverloadedF ts pts rts)
    addIntrinsicF (name, IntrinsicPolyFun tvs pts rt) =
      Just
        ( name,
          BoundV Global tvs $
            fromStruct $ Scalar $ Arrow mempty Unnamed pts' rt
        )
      where
        pts' = case pts of
          [pt] -> pt
          _ -> Scalar $ tupleRecord pts
    addIntrinsicF (name, IntrinsicEquality) =
      Just (name, EqualityF)
    addIntrinsicF _ = Nothing

runTermTypeM :: TermTypeM a -> TypeM (a, Occurrences)
runTermTypeM (TermTypeM m) = do
  initial_scope <- (initialTermScope <>) . envToTermScope <$> askEnv
  let initial_tenv =
        TermEnv
          { termScope = initial_scope,
            termChecking = Nothing,
            termLevel = 0
          }
  second stateOccs
    <$> runStateT
      (runReaderT m initial_tenv)
      (TermTypeState mempty 0 mempty mempty mempty)
