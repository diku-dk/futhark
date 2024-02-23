{-# LANGUAGE Strict #-}

-- | Facilities for type-checking terms.  Factored out of
-- "Language.Futhark.TypeChecker.Terms" to prevent the module from
-- being gigantic.
--
-- Incidentally also a nice place to put Haddock comments to make the
-- internal API of the type checker easier to browse.
module Language.Futhark.TypeChecker.Terms.Monad
  ( TermTypeM,
    runTermTypeM,
    ValBinding (..),
    SizeSource (SourceBound, SourceSlice),
    Inferred (..),
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
    Names,

    -- * Primitive checking
    unifies,
    require,
    checkTypeExpNonrigid,
    lookupVar,
    lookupMod,

    -- * Sizes
    isInt64,

    -- * Control flow
    incLevel,

    -- * Errors
    unusedSize,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bitraversable
import Data.Char (isAscii)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Futhark.FreshNames hiding (newName)
import Futhark.FreshNames qualified
import Futhark.Util.Pretty hiding (space)
import Language.Futhark
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Monad hiding (BoundV, lookupMod, stateNameSource)
import Language.Futhark.TypeChecker.Monad qualified as TypeM
import Language.Futhark.TypeChecker.Types
import Language.Futhark.TypeChecker.Unify
import Prelude hiding (mod)

type Names = S.Set VName

data ValBinding
  = BoundV [TypeParam] StructType
  | OverloadedF [PrimType] [Maybe PrimType] (Maybe PrimType)
  | EqualityF
  deriving (Show)

unusedSize :: (MonadTypeChecker m) => SizeBinder VName -> m a
unusedSize p =
  typeError p mempty . withIndexLink "unused-size" $
    "Size" <+> pretty p <+> "unused in pattern."

data Inferred t
  = NoneInferred
  | Ascribed t

instance Functor Inferred where
  fmap _ NoneInferred = NoneInferred
  fmap f (Ascribed t) = Ascribed (f t)

data Checking
  = CheckingApply (Maybe (QualName VName)) Exp StructType StructType
  | CheckingReturn ResType StructType
  | CheckingAscription StructType StructType
  | CheckingLetGeneralise Name
  | CheckingParams (Maybe Name)
  | CheckingPat (PatBase NoInfo VName StructType) (Inferred StructType)
  | CheckingLoopBody StructType StructType
  | CheckingLoopInitial StructType StructType
  | CheckingRecordUpdate [Name] StructType StructType
  | CheckingRequired [StructType] StructType
  | CheckingBranches StructType StructType

instance Pretty Checking where
  pretty (CheckingApply f e expected actual) =
    header
      </> "Expected:"
      <+> align (pretty expected)
      </> "Actual:  "
      <+> align (pretty actual)
    where
      header =
        case f of
          Nothing ->
            "Cannot apply function to"
              <+> dquotes (shorten $ group $ pretty e)
              <> " (invalid type)."
          Just fname ->
            "Cannot apply"
              <+> dquotes (pretty fname)
              <+> "to"
              <+> dquotes (align $ shorten $ group $ pretty e)
              <> " (invalid type)."
  pretty (CheckingReturn expected actual) =
    "Function body does not have expected type."
      </> "Expected:"
      <+> align (pretty expected)
      </> "Actual:  "
      <+> align (pretty actual)
  pretty (CheckingAscription expected actual) =
    "Expression does not have expected type from explicit ascription."
      </> "Expected:"
      <+> align (pretty expected)
      </> "Actual:  "
      <+> align (pretty actual)
  pretty (CheckingLetGeneralise fname) =
    "Cannot generalise type of" <+> dquotes (pretty fname) <> "."
  pretty (CheckingParams fname) =
    "Invalid use of parameters in" <+> dquotes fname' <> "."
    where
      fname' = maybe "anonymous function" pretty fname
  pretty (CheckingPat pat NoneInferred) =
    "Invalid pattern" <+> dquotes (pretty pat) <> "."
  pretty (CheckingPat pat (Ascribed t)) =
    "Pattern"
      </> indent 2 (pretty pat)
      </> "cannot match value of type"
      </> indent 2 (pretty t)
  pretty (CheckingLoopBody expected actual) =
    "Loop body does not have expected type."
      </> "Expected:"
      <+> align (pretty expected)
      </> "Actual:  "
      <+> align (pretty actual)
  pretty (CheckingLoopInitial expected actual) =
    "Initial loop values do not have expected type."
      </> "Expected:"
      <+> align (pretty expected)
      </> "Actual:  "
      <+> align (pretty actual)
  pretty (CheckingRecordUpdate fs expected actual) =
    "Type mismatch when updating record field"
      <+> dquotes fs'
      <> "."
        </> "Existing:"
        <+> align (pretty expected)
        </> "New:     "
        <+> align (pretty actual)
    where
      fs' = mconcat $ punctuate "." $ map pretty fs
  pretty (CheckingRequired [expected] actual) =
    "Expression must have type"
      <+> pretty expected
      <> "."
        </> "Actual type:"
        <+> align (pretty actual)
  pretty (CheckingRequired expected actual) =
    "Type of expression must be one of "
      <+> expected'
      <> "."
        </> "Actual type:"
        <+> align (pretty actual)
    where
      expected' = commasep (map pretty expected)
  pretty (CheckingBranches t1 t2) =
    "Branches differ in type."
      </> "Former:"
      <+> pretty t1
      </> "Latter:"
      <+> pretty t2

-- | Type checking happens with access to this environment.  The
-- 'TermScope' will be extended during type-checking as bindings come into
-- scope.
data TermEnv = TermEnv
  { termScope :: TermScope,
    termChecking :: Maybe Checking,
    termLevel :: Level,
    termChecker :: ExpBase NoInfo VName -> TermTypeM Exp,
    termOuterEnv :: Env,
    termImportName :: ImportName
  }

data TermScope = TermScope
  { scopeVtable :: M.Map VName ValBinding,
    scopeTypeTable :: M.Map VName TypeBinding,
    scopeModTable :: M.Map VName Mod
  }
  deriving (Show)

instance Semigroup TermScope where
  TermScope vt1 tt1 mt1 <> TermScope vt2 tt2 mt2 =
    TermScope (vt2 `M.union` vt1) (tt2 `M.union` tt1) (mt1 `M.union` mt2)

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
      (Maybe Size)
      (Maybe (ExpBase NoInfo VName))
      (Maybe (ExpBase NoInfo VName))
      (Maybe (ExpBase NoInfo VName))
  deriving (Eq, Ord, Show)

-- | The state is a set of constraints and a counter for generating
-- type names.  This is distinct from the usual counter we use for
-- generating unique names, as these will be user-visible.
data TermTypeState = TermTypeState
  { stateConstraints :: Constraints,
    stateCounter :: !Int,
    stateWarnings :: Warnings,
    stateNameSource :: VNameSource
  }

newtype TermTypeM a
  = TermTypeM
      ( ReaderT
          TermEnv
          (StateT TermTypeState (Except (Warnings, TypeError)))
          a
      )
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader TermEnv,
      MonadState TermTypeState
    )

instance MonadError TypeError TermTypeM where
  throwError e = TermTypeM $ do
    ws <- gets stateWarnings
    throwError (ws, e)

  catchError (TermTypeM m) f =
    TermTypeM $ m `catchError` f'
    where
      f' (_, e) = let TermTypeM m' = f e in m'

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
    pure $ Scalar $ TypeVar mempty (qualName v) []

  curLevel = asks termLevel

  newDimVar usage rigidity name = do
    dim <- newTypeName name
    case rigidity of
      Rigid rsrc -> constrain dim $ UnknownSize (locOf usage) rsrc
      Nonrigid -> constrain dim $ Size Nothing usage
    pure dim

  unifyError loc notes bcs doc = do
    checking <- asks termChecking
    case checking of
      Just checking' ->
        throwError $
          TypeError (locOf loc) notes $
            pretty checking' <> line </> doc <> pretty bcs
      Nothing ->
        throwError $ TypeError (locOf loc) notes $ doc <> pretty bcs

  matchError loc notes bcs t1 t2 = do
    checking <- asks termChecking
    case checking of
      Just checking'
        | hasNoBreadCrumbs bcs ->
            throwError $
              TypeError (locOf loc) notes $
                pretty checking'
        | otherwise ->
            throwError $
              TypeError (locOf loc) notes $
                pretty checking' <> line </> doc <> pretty bcs
      Nothing ->
        throwError $ TypeError (locOf loc) notes $ doc <> pretty bcs
    where
      doc =
        "Types"
          </> indent 2 (pretty t1)
          </> "and"
          </> indent 2 (pretty t2)
          </> "do not match."

-- | Instantiate a type scheme with fresh type variables for its type
-- parameters. Returns the names of the fresh type variables, the
-- instance list, and the instantiated type.
instantiateTypeScheme ::
  QualName VName ->
  SrcLoc ->
  [TypeParam] ->
  StructType ->
  TermTypeM ([VName], StructType)
instantiateTypeScheme qn loc tparams t = do
  let tnames = map typeParamName tparams
  (tparam_names, tparam_substs) <- mapAndUnzipM (instantiateTypeParam qn loc) tparams
  let substs = M.fromList $ zip tnames tparam_substs
      t' = applySubst (`M.lookup` substs) t
  pure (tparam_names, t')

-- | Create a new type name and insert it (unconstrained) in the
-- substitution map.
instantiateTypeParam ::
  (Monoid as) =>
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
      constrain v . NoConstraint x . mkUsage loc . docText $
        "instantiated type parameter of " <> dquotes (pretty qn)
      pure (v, Subst [] $ RetType [] $ Scalar $ TypeVar mempty (qualName v) [])
    TypeParamDim {} -> do
      constrain v . Size Nothing . mkUsage loc . docText $
        "instantiated size parameter of " <> dquotes (pretty qn)
      pure (v, ExpSubst $ sizeFromName (qualName v) loc)

lookupQualNameEnv :: QualName VName -> TermTypeM TermScope
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

lookupMod :: QualName VName -> TermTypeM Mod
lookupMod qn@(QualName _ name) = do
  scope <- lookupQualNameEnv qn
  case M.lookup name $ scopeModTable scope of
    Nothing -> error $ "lookupMod: " <> show qn
    Just m -> pure m

localScope :: (TermScope -> TermScope) -> TermTypeM a -> TermTypeM a
localScope f = local $ \tenv -> tenv {termScope = f $ termScope tenv}

instance MonadTypeChecker TermTypeM where
  warnings ws =
    modify $ \s -> s {stateWarnings = stateWarnings s <> ws}

  warn loc problem = warnings $ singleWarning (locOf loc) problem

  newName v = do
    s <- get
    let (v', src') = Futhark.FreshNames.newName (stateNameSource s) v
    put $ s {stateNameSource = src'}
    pure v'

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

  typeError loc notes s = do
    checking <- asks termChecking
    case checking of
      Just checking' ->
        throwError $ TypeError (locOf loc) notes (pretty checking' <> line </> s)
      Nothing ->
        throwError $ TypeError (locOf loc) notes s

lookupVar :: SrcLoc -> QualName VName -> TermTypeM StructType
lookupVar loc qn@(QualName qs name) = do
  scope <- lookupQualNameEnv qn
  let usage = mkUsage loc $ docText $ "use of " <> dquotes (pretty qn)

  case M.lookup name $ scopeVtable scope of
    Nothing ->
      error $ "lookupVar: " <> show qn
    Just (BoundV tparams t) -> do
      if null tparams && null qs
        then pure t
        else do
          (tnames, t') <- instantiateTypeScheme qn loc tparams t
          outer_env <- asks termOuterEnv
          pure $ qualifyTypeVars outer_env tnames qs t'
    Just EqualityF -> do
      argtype <- newTypeVar loc "t"
      equalityType usage argtype
      pure $
        Scalar . Arrow mempty Unnamed Observe argtype . RetType [] $
          Scalar $
            Arrow mempty Unnamed Observe argtype $
              RetType [] $
                Scalar $
                  Prim Bool
    Just (OverloadedF ts pts rt) -> do
      argtype <- newTypeVar loc "t"
      mustBeOneOf ts usage argtype
      let (pts', rt') = instOverloaded argtype pts rt
      pure $ foldFunType (map (toParam Observe) pts') $ RetType [] $ toRes Nonunique rt'
  where
    instOverloaded argtype pts rt =
      ( map (maybe (toStruct argtype) (Scalar . Prim)) pts,
        maybe (toStruct argtype) (Scalar . Prim) rt
      )

onFailure :: Checking -> TermTypeM a -> TermTypeM a
onFailure c = local $ \env -> env {termChecking = Just c}

extSize :: SrcLoc -> SizeSource -> TermTypeM (Size, Maybe VName)
extSize loc e = do
  let rsrc = case e of
        SourceArg (FName fname) e' ->
          RigidArg fname $ prettyTextOneLine e'
        SourceBound e' ->
          RigidBound $ prettyTextOneLine e'
        SourceSlice d i j s ->
          RigidSlice d $ prettyTextOneLine $ DimSlice i j s
  d <- newRigidDim loc rsrc "n"
  pure
    ( sizeFromName (qualName d) loc,
      Just d
    )

incLevel :: TermTypeM a -> TermTypeM a
incLevel = local $ \env -> env {termLevel = termLevel env + 1}

-- | Get the type of an expression, with top level type variables
-- substituted.  Never call 'typeOf' directly (except in a few
-- carefully inspected locations)!
expType :: Exp -> TermTypeM StructType
expType = normType . typeOf

-- | Get the type of an expression, with all type variables
-- substituted.  Slower than 'expType', but sometimes necessary.
-- Never call 'typeOf' directly (except in a few carefully inspected
-- locations)!
expTypeFully :: Exp -> TermTypeM StructType
expTypeFully = normTypeFully . typeOf

newArrayType :: Usage -> Name -> Int -> TermTypeM (StructType, StructType)
newArrayType usage desc r = do
  v <- newTypeName desc
  constrain v $ NoConstraint Unlifted usage
  dims <- replicateM r $ newDimVar usage Nonrigid "dim"
  let rowt = TypeVar mempty (qualName v) []
      mkSize = flip sizeFromName (srclocOf usage) . qualName
  pure
    ( Array mempty (Shape $ map mkSize dims) rowt,
      Scalar rowt
    )

-- | Replace *all* dimensions with distinct fresh size variables.
allDimsFreshInType ::
  Usage ->
  Rigidity ->
  Name ->
  TypeBase Size als ->
  TermTypeM (TypeBase Size als, M.Map VName Size)
allDimsFreshInType usage r desc t =
  runStateT (bitraverse onDim pure t) mempty
  where
    onDim d = do
      v <- lift $ newDimVar usage r desc
      modify $ M.insert v d
      pure $ sizeFromName (qualName v) $ srclocOf usage

-- | Replace all type variables with their concrete types.
updateTypes :: (ASTMappable e) => e -> TermTypeM e
updateTypes = astMap tv
  where
    tv =
      ASTMapper
        { mapOnExp = astMap tv,
          mapOnName = pure,
          mapOnStructType = normTypeFully,
          mapOnParamType = normTypeFully,
          mapOnResRetType = normTypeFully
        }

--- Basic checking

unifies :: T.Text -> StructType -> Exp -> TermTypeM Exp
unifies why t e = do
  unify (mkUsage (srclocOf e) why) t . toStruct =<< expType e
  pure e

-- | @require ts e@ causes a 'TypeError' if @expType e@ is not one of
-- the types in @ts@.  Otherwise, simply returns @e@.
require :: T.Text -> [PrimType] -> Exp -> TermTypeM Exp
require why ts e = do
  mustBeOneOf ts (mkUsage (srclocOf e) why) . toStruct =<< expType e
  pure e

checkExpForSize :: ExpBase NoInfo VName -> TermTypeM Exp
checkExpForSize e = do
  checker <- asks termChecker
  e' <- checker e
  let t = toStruct $ typeOf e'
  unify (mkUsage (locOf e') "Size expression") t (Scalar (Prim (Signed Int64)))
  updateTypes e'

checkTypeExpNonrigid ::
  TypeExp (ExpBase NoInfo VName) VName ->
  TermTypeM (TypeExp Exp VName, ResType, [VName])
checkTypeExpNonrigid te = do
  (te', svars, rettype, _l) <- checkTypeExp checkExpForSize te

  -- No guarantee that the locally bound sizes in rettype are globally
  -- unique, but we want to turn them into size variables, so let's
  -- give them some unique names.
  RetType dims st <- renameRetType rettype

  forM_ (svars ++ dims) $ \v ->
    constrain v $ Size Nothing $ mkUsage (srclocOf te) "anonymous size in type expression"
  pure (te', st, svars ++ dims)

--- Sizes

isInt64 :: Exp -> Maybe Int64
isInt64 (Literal (SignedValue (Int64Value k')) _) = Just $ fromIntegral k'
isInt64 (IntLit k' _ _) = Just $ fromInteger k'
isInt64 (Negate x _) = negate <$> isInt64 x
isInt64 (Parens x _) = isInt64 x
isInt64 _ = Nothing

-- Running

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

runTermTypeM :: (ExpBase NoInfo VName -> TermTypeM Exp) -> TermTypeM a -> TypeM a
runTermTypeM checker (TermTypeM m) = do
  initial_scope <- (initialTermScope <>) . envToTermScope <$> askEnv
  name <- askImportName
  outer_env <- askEnv
  src <- gets TypeM.stateNameSource
  let initial_tenv =
        TermEnv
          { termScope = initial_scope,
            termChecking = Nothing,
            termLevel = 0,
            termChecker = checker,
            termImportName = name,
            termOuterEnv = outer_env
          }
      initial_state =
        TermTypeState
          { stateConstraints = mempty,
            stateCounter = 0,
            stateWarnings = mempty,
            stateNameSource = src
          }
  case runExcept (runStateT (runReaderT m initial_tenv) initial_state) of
    Left (ws, e) -> do
      warnings ws
      throwError e
    Right (a, TermTypeState {stateNameSource, stateWarnings}) -> do
      warnings stateWarnings
      modify $ \s -> s {TypeM.stateNameSource = stateNameSource}
      pure a
