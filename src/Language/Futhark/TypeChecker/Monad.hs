-- | Main monad in which the type checker runs, as well as ancillary
-- data definitions.
module Language.Futhark.TypeChecker.Monad
  ( TypeM,
    runTypeM,
    askEnv,
    askImportName,
    atTopLevel,
    enteringModule,
    bindSpaced,
    qualifyTypeVars,
    lookupMTy,
    lookupImport,
    localEnv,
    TypeError (..),
    prettyTypeError,
    prettyTypeErrorNoLoc,
    withIndexLink,
    unappliedFunctor,
    unknownVariable,
    unknownType,
    underscoreUse,
    Notes,
    aNote,
    MonadTypeChecker (..),
    checkName,
    checkAttr,
    badOnLeft,
    module Language.Futhark.Warnings,
    Env (..),
    TySet,
    FunSig (..),
    ImportTable,
    NameMap,
    BoundV (..),
    Mod (..),
    TypeBinding (..),
    MTy (..),
    anySignedType,
    anyUnsignedType,
    anyIntType,
    anyFloatType,
    anyNumberType,
    anyPrimType,
    Namespace (..),
    intrinsicsNameMap,
    topLevelNameMap,
    mkTypeVarName,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Either
import Data.List (find, isPrefixOf)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Version qualified as Version
import Futhark.FreshNames hiding (newName)
import Futhark.FreshNames qualified
import Futhark.Util.Pretty hiding (space)
import Language.Futhark
import Language.Futhark.Semantic
import Language.Futhark.Warnings
import Paths_futhark qualified
import Prelude hiding (mapM, mod)

newtype Note = Note (Doc ())

-- | A collection of extra information regarding a type error.
newtype Notes = Notes [Note]
  deriving (Semigroup, Monoid)

instance Pretty Note where
  pretty (Note msg) = unAnnotate $ "Note:" <+> align msg

instance Pretty Notes where
  pretty (Notes notes) = unAnnotate $ foldMap (((line <> line) <>) . pretty) notes

-- | A single note.
aNote :: Doc () -> Notes
aNote = Notes . pure . Note

-- | Information about an error during type checking.
data TypeError = TypeError Loc Notes (Doc ())

-- | Prettyprint type error.
prettyTypeError :: TypeError -> Doc AnsiStyle
prettyTypeError (TypeError loc notes msg) =
  annotate
    (bold <> color Red)
    ("Error at " <> pretty (locText (srclocOf loc)) <> ":")
    </> prettyTypeErrorNoLoc (TypeError loc notes msg)

-- | Prettyprint type error, without location information.  This can
-- be used for cases where the location is printed in some other way.
prettyTypeErrorNoLoc :: TypeError -> Doc AnsiStyle
prettyTypeErrorNoLoc (TypeError _ notes msg) =
  unAnnotate msg <> pretty notes <> hardline

errorIndexUrl :: Doc a
errorIndexUrl = version_url <> "error-index.html"
  where
    version = Paths_futhark.version
    base_url = "https://futhark.readthedocs.io/en/"
    version_url
      | last (Version.versionBranch version) == 0 = base_url <> "latest/"
      | otherwise = base_url <> "v" <> pretty (Version.showVersion version) <> "/"

-- | Attach a reference to documentation explaining the error in more detail.
withIndexLink :: Doc a -> Doc a -> Doc a
withIndexLink href msg =
  stack
    [ msg,
      "\nFor more information, see:",
      indent 2 (errorIndexUrl <> "#" <> href)
    ]

-- | An unexpected functor appeared!
unappliedFunctor :: MonadTypeChecker m => SrcLoc -> m a
unappliedFunctor loc =
  typeError loc mempty "Cannot have parametric module here."

-- | An unknown variable was referenced.
unknownVariable ::
  MonadTypeChecker m =>
  Namespace ->
  QualName Name ->
  SrcLoc ->
  m a
unknownVariable space name loc =
  typeError loc mempty $
    "Unknown" <+> pretty space <+> dquotes (pretty name)

-- | An unknown type was referenced.
unknownType :: MonadTypeChecker m => SrcLoc -> QualName Name -> m a
unknownType loc name =
  typeError loc mempty $ "Unknown type" <+> pretty name <> "."

-- | A name prefixed with an underscore was used.
underscoreUse ::
  MonadTypeChecker m =>
  SrcLoc ->
  QualName Name ->
  m a
underscoreUse loc name =
  typeError loc mempty $
    "Use of"
      <+> dquotes (pretty name)
        <> ": variables prefixed with underscore may not be accessed."

-- | A mapping from import import names to 'Env's.  This is used to
-- resolve @import@ declarations.
type ImportTable = M.Map ImportName Env

data Context = Context
  { contextEnv :: Env,
    contextImportTable :: ImportTable,
    contextImportName :: ImportName,
    -- | Currently type-checking at the top level?  If false, we are
    -- inside a module.
    contextAtTopLevel :: Bool
  }

data TypeState = TypeState
  { stateNameSource :: VNameSource,
    stateWarnings :: Warnings,
    stateCounter :: Int
  }

-- | The type checker runs in this monad.
newtype TypeM a
  = TypeM
      ( ReaderT
          Context
          (StateT TypeState (Except (Warnings, TypeError)))
          a
      )
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader Context,
      MonadState TypeState
    )

instance MonadError TypeError TypeM where
  throwError e = TypeM $ do
    ws <- gets stateWarnings
    throwError (ws, e)

  catchError (TypeM m) f =
    TypeM $ m `catchError` f'
    where
      f' (_, e) =
        let TypeM m' = f e
         in m'

-- | Run a 'TypeM' computation.
runTypeM ::
  Env ->
  ImportTable ->
  ImportName ->
  VNameSource ->
  TypeM a ->
  (Warnings, Either TypeError (a, VNameSource))
runTypeM env imports fpath src (TypeM m) = do
  let ctx = Context env imports fpath True
      s = TypeState src mempty 0
  case runExcept $ runStateT (runReaderT m ctx) s of
    Left (ws, e) -> (ws, Left e)
    Right (x, TypeState src' ws _) -> (ws, Right (x, src'))

-- | Retrieve the current 'Env'.
askEnv :: TypeM Env
askEnv = asks contextEnv

-- | The name of the current file/import.
askImportName :: TypeM ImportName
askImportName = asks contextImportName

-- | Are we type-checking at the top level, or are we inside a nested
-- module?
atTopLevel :: TypeM Bool
atTopLevel = asks contextAtTopLevel

-- | We are now going to type-check the body of a module.
enteringModule :: TypeM a -> TypeM a
enteringModule = local $ \ctx -> ctx {contextAtTopLevel = False}

-- | Look up a module type.
lookupMTy :: SrcLoc -> QualName Name -> TypeM (QualName VName, MTy)
lookupMTy loc qn = do
  (scope, qn'@(QualName _ name)) <- checkQualNameWithEnv Signature qn loc
  (qn',) <$> maybe explode pure (M.lookup name $ envSigTable scope)
  where
    explode = unknownVariable Signature qn loc

-- | Look up an import.
lookupImport :: SrcLoc -> FilePath -> TypeM (ImportName, Env)
lookupImport loc file = do
  imports <- asks contextImportTable
  my_path <- asks contextImportName
  let canonical_import = mkImportFrom my_path file
  case M.lookup canonical_import imports of
    Nothing ->
      typeError loc mempty $
        "Unknown import"
          <+> dquotes (pretty (includeToText canonical_import))
          </> "Known:"
          <+> commasep (map (pretty . includeToText) (M.keys imports))
    Just scope -> pure (canonical_import, scope)

-- | Evaluate a 'TypeM' computation within an extended (/not/
-- replaced) environment.
localEnv :: Env -> TypeM a -> TypeM a
localEnv env = local $ \ctx ->
  let env' = env <> contextEnv ctx
   in ctx {contextEnv = env'}

incCounter :: TypeM Int
incCounter = do
  s <- get
  put s {stateCounter = stateCounter s + 1}
  pure $ stateCounter s

-- | Monads that support type checking.  The reason we have this
-- internal interface is because we use distinct monads for checking
-- expressions and declarations.
class Monad m => MonadTypeChecker m where
  warn :: Located loc => loc -> Doc () -> m ()

  newName :: VName -> m VName
  newID :: Name -> m VName
  newTypeName :: Name -> m VName

  bindNameMap :: NameMap -> m a -> m a
  bindVal :: VName -> BoundV -> m a -> m a

  checkQualName :: Namespace -> QualName Name -> SrcLoc -> m (QualName VName)

  lookupType :: SrcLoc -> QualName Name -> m (QualName VName, [TypeParam], StructRetType, Liftedness)
  lookupMod :: SrcLoc -> QualName Name -> m (QualName VName, Mod)
  lookupVar :: SrcLoc -> QualName Name -> m (QualName VName, PatType)

  checkNamedSize :: SrcLoc -> QualName Name -> m (QualName VName)
  checkNamedSize loc v = do
    (v', t) <- lookupVar loc v
    case t of
      Scalar (Prim (Signed Int64)) -> pure v'
      _ ->
        typeError loc mempty $
          "Sizes must have type i64, but"
            <+> dquotes (pretty v)
            <+> "has type:"
            </> pretty t

  typeError :: Located loc => loc -> Notes -> Doc () -> m a

-- | Elaborate the given name in the given namespace at the given
-- location, producing the corresponding unique 'VName'.
checkName :: MonadTypeChecker m => Namespace -> Name -> SrcLoc -> m VName
checkName space name loc = qualLeaf <$> checkQualName space (qualName name) loc

-- | Map source-level names do fresh unique internal names, and
-- evaluate a type checker context with the mapping active.
bindSpaced :: MonadTypeChecker m => [(Namespace, Name)] -> m a -> m a
bindSpaced names body = do
  names' <- mapM (newID . snd) names
  let mapping = M.fromList (zip names $ map qualName names')
  bindNameMap mapping body

instance MonadTypeChecker TypeM where
  warn loc problem =
    modify $ \s ->
      s
        { stateWarnings = stateWarnings s <> singleWarning (srclocOf loc) problem
        }

  newName v = do
    s <- get
    let (v', src') = Futhark.FreshNames.newName (stateNameSource s) v
    put $ s {stateNameSource = src'}
    pure v'

  newID s = newName $ VName s 0

  newTypeName name = do
    i <- incCounter
    newID $ mkTypeVarName name i

  bindNameMap m = local $ \ctx ->
    let env = contextEnv ctx
     in ctx {contextEnv = env {envNameMap = m <> envNameMap env}}

  bindVal v t = local $ \ctx ->
    ctx
      { contextEnv =
          (contextEnv ctx)
            { envVtable = M.insert v t $ envVtable $ contextEnv ctx
            }
      }

  checkQualName space name loc = snd <$> checkQualNameWithEnv space name loc

  lookupType loc qn = do
    outer_env <- askEnv
    (scope, qn'@(QualName qs name)) <- checkQualNameWithEnv Type qn loc
    case M.lookup name $ envTypeTable scope of
      Nothing -> unknownType loc qn
      Just (TypeAbbr l ps (RetType dims def)) ->
        pure (qn', ps, RetType dims $ qualifyTypeVars outer_env mempty qs def, l)

  lookupMod loc qn = do
    (scope, qn'@(QualName _ name)) <- checkQualNameWithEnv Term qn loc
    case M.lookup name $ envModTable scope of
      Nothing -> unknownVariable Term qn loc
      Just m -> pure (qn', m)

  lookupVar loc qn = do
    outer_env <- askEnv
    (env, qn'@(QualName qs name)) <- checkQualNameWithEnv Term qn loc
    case M.lookup name $ envVtable env of
      Nothing -> unknownVariable Term qn loc
      Just (BoundV _ t)
        | "_" `isPrefixOf` baseString name -> underscoreUse loc qn
        | otherwise ->
            case getType t of
              Nothing ->
                typeError loc mempty $
                  "Attempt to use function" <+> prettyName name <+> "as value."
              Just t' ->
                pure
                  ( qn',
                    fromStruct $
                      qualifyTypeVars outer_env mempty qs t'
                  )

  typeError loc notes s = throwError $ TypeError (locOf loc) notes s

-- | Extract from a type a first-order type.
getType :: TypeBase dim as -> Maybe (TypeBase dim as)
getType (Scalar Arrow {}) = Nothing
getType t = Just t

checkQualNameWithEnv :: Namespace -> QualName Name -> SrcLoc -> TypeM (Env, QualName VName)
checkQualNameWithEnv space qn@(QualName quals name) loc = do
  env <- askEnv
  descend env quals
  where
    descend scope []
      | Just name' <- M.lookup (space, name) $ envNameMap scope =
          pure (scope, name')
      | otherwise =
          unknownVariable space qn loc
    descend scope (q : qs)
      | Just (QualName _ q') <- M.lookup (Term, q) $ envNameMap scope,
        Just res <- M.lookup q' $ envModTable scope =
          case res of
            ModEnv q_scope -> do
              (scope', QualName qs' name') <- descend q_scope qs
              pure (scope', QualName (q' : qs') name')
            ModFun {} -> unappliedFunctor loc
      | otherwise =
          unknownVariable space qn loc

-- | Try to prepend qualifiers to the type names such that they
-- represent how to access the type in some scope.
qualifyTypeVars ::
  Env ->
  [VName] ->
  [VName] ->
  TypeBase Size as ->
  TypeBase Size as
qualifyTypeVars outer_env orig_except ref_qs = onType (S.fromList orig_except)
  where
    onType ::
      S.Set VName ->
      TypeBase Size as ->
      TypeBase Size as
    onType except (Array as u shape et) =
      Array as u (fmap (onDim except) shape) (onScalar except et)
    onType except (Scalar t) =
      Scalar $ onScalar except t

    onScalar _ (Prim t) = Prim t
    onScalar except (TypeVar as u qn targs) =
      TypeVar as u (qual except qn) (map (onTypeArg except) targs)
    onScalar except (Record m) =
      Record $ M.map (onType except) m
    onScalar except (Sum m) =
      Sum $ M.map (map $ onType except) m
    onScalar except (Arrow as p d t1 (RetType dims t2)) =
      Arrow as p d (onType except' t1) $ RetType dims (onType except' t2)
      where
        except' = case p of
          Named p' -> S.insert p' except
          Unnamed -> except

    onTypeArg except (TypeArgDim d loc) =
      TypeArgDim (onDim except d) loc
    onTypeArg except (TypeArgType t loc) =
      TypeArgType (onType except t) loc

    onDim except (NamedSize qn) = NamedSize $ qual except qn
    onDim _ d = d

    qual except (QualName orig_qs name)
      | name `elem` except || reachable orig_qs name outer_env =
          QualName orig_qs name
      | otherwise =
          prependAsNecessary [] ref_qs $ QualName orig_qs name

    prependAsNecessary qs rem_qs (QualName orig_qs name)
      | reachable (qs ++ orig_qs) name outer_env = QualName (qs ++ orig_qs) name
      | otherwise = case rem_qs of
          q : rem_qs' -> prependAsNecessary (qs ++ [q]) rem_qs' (QualName orig_qs name)
          [] -> QualName orig_qs name

    reachable [] name env =
      name `M.member` envVtable env
        || isJust (find matches $ M.elems (envTypeTable env))
      where
        matches (TypeAbbr _ _ (RetType _ (Scalar (TypeVar _ _ (QualName x_qs name') _)))) =
          null x_qs && name == name'
        matches _ = False
    reachable (q : qs') name env
      | Just (ModEnv env') <- M.lookup q $ envModTable env =
          reachable qs' name env'
      | otherwise = False

-- | Turn a 'Left' 'TypeError' into an actual error.
badOnLeft :: Either TypeError a -> TypeM a
badOnLeft = either throwError pure

-- | All signed integer types.
anySignedType :: [PrimType]
anySignedType = map Signed [minBound .. maxBound]

-- | All unsigned integer types.
anyUnsignedType :: [PrimType]
anyUnsignedType = map Unsigned [minBound .. maxBound]

-- | All integer types.
anyIntType :: [PrimType]
anyIntType = anySignedType ++ anyUnsignedType

-- | All floating-point types.
anyFloatType :: [PrimType]
anyFloatType = map FloatType [minBound .. maxBound]

-- | All number types.
anyNumberType :: [PrimType]
anyNumberType = anyIntType ++ anyFloatType

-- | All primitive types.
anyPrimType :: [PrimType]
anyPrimType = Bool : anyIntType ++ anyFloatType

--- Name handling

-- | The 'NameMap' corresponding to the intrinsics module.
intrinsicsNameMap :: NameMap
intrinsicsNameMap = M.fromList $ map mapping $ M.toList intrinsics
  where
    mapping (v, IntrinsicType {}) = ((Type, baseName v), QualName [] v)
    mapping (v, _) = ((Term, baseName v), QualName [] v)

-- | The names that are available in the initial environment.
topLevelNameMap :: NameMap
topLevelNameMap = M.filterWithKey (\k _ -> available k) intrinsicsNameMap
  where
    available :: (Namespace, Name) -> Bool
    available (Type, _) = True
    available (Term, v) = v `S.member` (type_names <> binop_names <> fun_names)
      where
        type_names = S.fromList $ map (nameFromText . prettyText) anyPrimType
        binop_names =
          S.fromList $
            map
              (nameFromText . prettyText)
              [minBound .. (maxBound :: BinOp)]
        fun_names = S.fromList $ map nameFromString ["shape"]
    available _ = False

-- | Construct the name of a new type variable given a base
-- description and a tag number (note that this is distinct from
-- actually constructing a VName; the tag here is intended for human
-- consumption but the machine does not care).
mkTypeVarName :: Name -> Int -> Name
mkTypeVarName desc i =
  desc <> nameFromString (mapMaybe subscript (show i))
  where
    subscript = flip lookup $ zip "0123456789" "₀₁₂₃₄₅₆₇₈₉"

-- | Type-check an attribute.
checkAttr :: MonadTypeChecker m => AttrInfo Name -> m (AttrInfo VName)
checkAttr (AttrComp f attrs loc) =
  AttrComp f <$> mapM checkAttr attrs <*> pure loc
checkAttr (AttrAtom (AtomName v) loc) =
  pure $ AttrAtom (AtomName v) loc
checkAttr (AttrAtom (AtomInt x) loc) =
  pure $ AttrAtom (AtomInt x) loc
