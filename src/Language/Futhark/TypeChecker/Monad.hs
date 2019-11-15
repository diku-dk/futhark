{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, TupleSections #-}
-- | Main monad in which the type checker runs, as well as ancillary
-- data definitions.
module Language.Futhark.TypeChecker.Monad
  ( TypeM
  , runTypeM
  , askEnv
  , askImportName
  , checkQualNameWithEnv
  , bindSpaced
  , qualifyTypeVars
  , lookupMTy
  , lookupImport
  , localEnv

  , TypeError(..)
  , unexpectedType
  , undefinedType
  , unappliedFunctor
  , unknownVariableError
  , underscoreUse
  , functionIsNotValue

  , BreadCrumb(..)
  , MonadBreadCrumbs(..)
  , typeError

  , MonadTypeChecker(..)
  , checkName
  , badOnLeft

  , module Language.Futhark.Warnings

  , Env(..)
  , TySet
  , FunSig(..)
  , ImportTable
  , NameMap
  , BoundV(..)
  , Mod(..)
  , TypeBinding(..)
  , MTy(..)

  , anySignedType
  , anyUnsignedType
  , anyIntType
  , anyFloatType
  , anyNumberType
  , anyPrimType

  , Namespace(..)
  , intrinsicsNameMap
  , topLevelNameMap
  , ppSpace
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS.Strict
import Control.Monad.Identity
import Data.List
import Data.Loc
import Data.Maybe
import Data.Either
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Prelude hiding (mapM, mod)

import Language.Futhark
import Language.Futhark.Semantic
import Language.Futhark.Traversals
import Language.Futhark.Warnings
import Futhark.FreshNames hiding (newName)
import qualified Futhark.FreshNames

-- | Information about an error during type checking.  The 'Show'
-- instance for this type produces a human-readable description.
data TypeError = TypeError SrcLoc String

unexpectedType :: MonadTypeChecker m => SrcLoc -> StructType -> [StructType] -> m a
unexpectedType loc _ [] =
  throwError $ TypeError loc $
  "Type of expression at " ++ locStr loc ++
  "cannot have any type - possibly a bug in the type checker."
unexpectedType loc t ts =
  throwError $ TypeError loc $
  "Type of expression at " ++ locStr loc ++ " must be one of " ++
  intercalate ", " (map pretty ts) ++ ", but is " ++
  pretty t ++ "."

undefinedType :: MonadTypeChecker m => SrcLoc -> QualName Name -> m a
undefinedType loc name =
  throwError $ TypeError loc $
  "Unknown type " ++ pretty name ++ "."

functionIsNotValue :: MonadTypeChecker m => SrcLoc -> QualName Name -> m a
functionIsNotValue loc name =
  throwError $ TypeError loc $
  "Attempt to use function " ++ pretty name ++ " as value at " ++ locStr loc ++ "."

unappliedFunctor :: MonadTypeChecker m => SrcLoc -> m a
unappliedFunctor loc =
  throwError $ TypeError loc "Cannot have parametric module here."

unknownVariableError :: MonadTypeChecker m =>
                        Namespace -> QualName Name -> SrcLoc -> m a
unknownVariableError space name loc =
  throwError $ TypeError loc $
  "Unknown " ++ ppSpace space ++ " " ++ quote (pretty name)

underscoreUse :: MonadTypeChecker m =>
                 SrcLoc -> QualName Name -> m a
underscoreUse loc name =
  throwError $ TypeError loc $
  "Use of " ++ quote (pretty name) ++
  ": variables prefixed with underscore may not be accessed."

instance Show TypeError where
  show (TypeError pos msg) =
    "Error at " ++ locStr pos ++ ":\n" ++ msg

type ImportTable = M.Map String Env

data Context = Context { contextEnv :: Env
                       , contextImportTable :: ImportTable
                       , contextImportName :: ImportName
                       }

-- | The type checker runs in this monad.
newtype TypeM a = TypeM (RWST
                         Context -- Reader
                         Warnings           -- Writer
                         VNameSource        -- State
                         (Except TypeError) -- Inner monad
                         a)
  deriving (Monad, Functor, Applicative,
            MonadReader Context,
            MonadWriter Warnings,
            MonadState VNameSource,
            MonadError TypeError)

runTypeM :: Env -> ImportTable -> ImportName -> VNameSource
         -> TypeM a
         -> Either TypeError (a, Warnings, VNameSource)
runTypeM env imports fpath src (TypeM m) = do
  (x, src', ws) <- runExcept $ runRWST m (Context env imports fpath) src
  return (x, ws, src')

askEnv :: TypeM Env
askEnv = asks contextEnv

-- | The name of the current file/import.
askImportName :: TypeM ImportName
askImportName = asks contextImportName

lookupMTy :: SrcLoc -> QualName Name -> TypeM (QualName VName, MTy)
lookupMTy loc qn = do
  (scope, qn'@(QualName _ name)) <- checkQualNameWithEnv Signature qn loc
  (qn',) <$> maybe explode return (M.lookup name $ envSigTable scope)
  where explode = unknownVariableError Signature qn loc

lookupImport :: SrcLoc -> FilePath -> TypeM (FilePath, Env)
lookupImport loc file = do
  imports <- asks contextImportTable
  my_path <- asks contextImportName
  let canonical_import = includeToString $ mkImportFrom my_path file loc
  case M.lookup canonical_import imports of
    Nothing    -> throwError $ TypeError loc $
                  unlines ["Unknown import \"" ++ canonical_import ++ "\"",
                           "Known: " ++ intercalate ", " (M.keys imports)]
    Just scope -> return (canonical_import, scope)

localEnv :: Env -> TypeM a -> TypeM a
localEnv env = local $ \ctx ->
  let env' = env <> contextEnv ctx
  in ctx { contextEnv = env' }

-- | A piece of information that describes what process the type
-- checker currently performing.  This is used to give better error
-- messages.
data BreadCrumb = MatchingTypes StructType StructType
                | MatchingFields Name
                | Matching String

instance Show BreadCrumb where
  show (MatchingTypes t1 t2) =
    "When matching type\n" ++ indent (pretty t1) ++
    "\nwith\n" ++ indent (pretty t2)
    where indent = intercalate "\n" . map ("  "++) . lines
  show (MatchingFields field) =
    "When matching types of record field " ++ quote (pretty field) ++ "."
  show (Matching s) =
    s

-- | Tracking breadcrumbs to give a kind of "stack trace" in errors.
class Monad m => MonadBreadCrumbs m where
  breadCrumb :: BreadCrumb -> m a -> m a
  breadCrumb _ m = m

  getBreadCrumbs :: m [BreadCrumb]
  getBreadCrumbs = return []

typeError :: (Located loc, MonadError TypeError m, MonadBreadCrumbs m) =>
             loc -> String -> m a
typeError loc s = do
  bc <- getBreadCrumbs
  let bc' | null bc = ""
          | otherwise = "\n" ++ unlines (map show bc)
  throwError $ TypeError (srclocOf loc) $ s ++ bc'

class MonadError TypeError m => MonadTypeChecker m where
  warn :: Located loc => loc -> String -> m ()

  newName :: VName -> m VName
  newID :: Name -> m VName

  bindNameMap :: NameMap -> m a -> m a
  bindVal :: VName -> BoundV -> m a -> m a

  checkQualName :: Namespace -> QualName Name -> SrcLoc -> m (QualName VName)

  lookupType :: SrcLoc -> QualName Name -> m (QualName VName, [TypeParam], StructType, Liftedness)
  lookupMod :: SrcLoc -> QualName Name -> m (QualName VName, Mod)
  lookupVar :: SrcLoc -> QualName Name -> m (QualName VName, PatternType)

  checkNamedDim :: SrcLoc -> QualName Name -> m (QualName VName)
  checkNamedDim loc v = do
    (v', t) <- lookupVar loc v
    case t of
      Scalar (Prim (Signed Int32)) -> return v'
      _ -> throwError $ TypeError loc $
           "Dimension declaration " ++ pretty v ++ " should be of type `i32`."

checkName :: MonadTypeChecker m => Namespace -> Name -> SrcLoc -> m VName
checkName space name loc = qualLeaf <$> checkQualName space (qualName name) loc

bindSpaced :: MonadTypeChecker m => [(Namespace, Name)] -> m a -> m a
bindSpaced names body = do
  names' <- mapM (newID . snd) names
  let mapping = M.fromList (zip names $ map qualName names')
  bindNameMap mapping body

instance MonadTypeChecker TypeM where
  warn loc problem = tell $ singleWarning (srclocOf loc) problem

  newName s = do src <- get
                 let (s', src') = Futhark.FreshNames.newName src s
                 put src'
                 return s'

  newID s = newName $ VName s 0

  bindNameMap m = local $ \ctx ->
    let env = contextEnv ctx
    in ctx { contextEnv = env { envNameMap = m <> envNameMap env } }

  bindVal v t = local $ \ctx ->
    ctx { contextEnv = (contextEnv ctx)
                       { envVtable = M.insert v t $ envVtable $ contextEnv ctx } }

  checkQualName space name loc = snd <$> checkQualNameWithEnv space name loc

  lookupType loc qn = do
    outer_env <- askEnv
    (scope, qn'@(QualName qs name)) <- checkQualNameWithEnv Type qn loc
    case M.lookup name $ envTypeTable scope of
      Nothing -> undefinedType loc qn
      Just (TypeAbbr l ps def) -> return (qn', ps, qualifyTypeVars outer_env mempty qs def, l)

  lookupMod loc qn = do
    (scope, qn'@(QualName _ name)) <- checkQualNameWithEnv Term qn loc
    case M.lookup name $ envModTable scope of
      Nothing -> unknownVariableError Term qn loc
      Just m  -> return (qn', m)

  lookupVar loc qn = do
    outer_env <- askEnv
    (env, qn'@(QualName qs name)) <- checkQualNameWithEnv Term qn loc
    case M.lookup name $ envVtable env of
      Nothing -> unknownVariableError Term qn loc
      Just (BoundV _ t)
        | "_" `isPrefixOf` baseString name -> underscoreUse loc qn
        | otherwise ->
            case getType t of
              Left{} -> throwError $ TypeError loc $
                        "Attempt to use function " ++ baseString name ++ " as value."
              Right t' -> return (qn', fromStruct $
                                       qualifyTypeVars outer_env mempty qs t')

-- | Extract from a type either a function type comprising a list of
-- parameter types and a return type, or a first-order type.
getType :: TypeBase dim as
        -> Either ([(PName, TypeBase dim as)], TypeBase dim as)
                  (TypeBase dim as)
getType (Scalar (Arrow _ v t1 t2)) =
  case getType t2 of
    Left (ps, r) -> Left ((v, t1) : ps, r)
    Right _ -> Left ([(v, t1)], t2)
getType t = Right t

checkQualNameWithEnv :: Namespace -> QualName Name -> SrcLoc -> TypeM (Env, QualName VName)
checkQualNameWithEnv space qn@(QualName quals name) loc = do
  env <- askEnv
  descend env quals
  where descend scope []
          | Just name' <- M.lookup (space, name) $ envNameMap scope =
              return (scope, name')
          | otherwise =
              unknownVariableError space qn loc

        descend scope (q:qs)
          | Just (QualName _ q') <- M.lookup (Term, q) $ envNameMap scope,
            Just res <- M.lookup q' $ envModTable scope =
              case res of
                ModEnv q_scope -> do
                  (scope', QualName qs' name') <- descend q_scope qs
                  return (scope', QualName (q':qs') name')
                ModFun{} -> unappliedFunctor loc
          | otherwise =
              unknownVariableError space qn loc

-- Try to prepend qualifiers to the type names such that they
-- represent how to access the type in some scope.
qualifyTypeVars :: ASTMappable t => Env -> [VName] -> [VName] -> t -> t
qualifyTypeVars outer_env except ref_qs = runIdentity . astMap mapper
  where mapper = ASTMapper { mapOnExp = pure
                           , mapOnName = pure
                           , mapOnQualName = pure . qual
                           , mapOnStructType = pure
                           , mapOnPatternType = pure
                           }
        qual (QualName orig_qs name)
          | name `elem` except || reachable orig_qs name outer_env =
              QualName orig_qs name
          | otherwise =
              prependAsNecessary [] ref_qs $ QualName orig_qs name

        prependAsNecessary qs rem_qs (QualName orig_qs name)
          | reachable (qs++orig_qs) name outer_env = QualName (qs++orig_qs) name
          | otherwise = case rem_qs of
                          q:rem_qs' -> prependAsNecessary (qs++[q]) rem_qs' (QualName orig_qs name)
                          []       -> QualName (qs++orig_qs) name

        reachable [] name env =
          isJust $ find matches $ M.elems (envTypeTable env)
          where matches (TypeAbbr _ _ (Scalar (TypeVar _ _ (TypeName x_qs name') _))) =
                  null x_qs && name == name'
                matches _ = False

        reachable (q:qs') name env
          | Just (ModEnv env') <- M.lookup q $ envModTable env =
              reachable qs' name env'
          | otherwise = False

badOnLeft :: MonadTypeChecker m => Either TypeError a -> m a
badOnLeft = either throwError return

anySignedType :: [PrimType]
anySignedType = map Signed [minBound .. maxBound]

anyUnsignedType :: [PrimType]
anyUnsignedType = map Unsigned [minBound .. maxBound]

anyIntType :: [PrimType]
anyIntType = anySignedType ++ anyUnsignedType

anyFloatType :: [PrimType]
anyFloatType = map FloatType [minBound .. maxBound]

anyNumberType :: [PrimType]
anyNumberType = anyIntType ++ anyFloatType

anyPrimType :: [PrimType]
anyPrimType = Bool : anyIntType ++ anyFloatType

--- Name handling

ppSpace :: Namespace -> String
ppSpace Term = "name"
ppSpace Type = "type"
ppSpace Signature = "module type"

intrinsicsNameMap :: NameMap
intrinsicsNameMap = M.fromList $ map mapping $ M.toList intrinsics
  where mapping (v, IntrinsicType{}) = ((Type, baseName v), QualName [] v)
        mapping (v, _)               = ((Term, baseName v), QualName [] v)

topLevelNameMap :: NameMap
topLevelNameMap = M.filterWithKey (\k _ -> atTopLevel k) intrinsicsNameMap
  where atTopLevel :: (Namespace, Name) -> Bool
        atTopLevel (Type, _) = True
        atTopLevel (Term, v) = v `S.member` (type_names <> binop_names <> unop_names <> fun_names)
          where type_names = S.fromList $ map (nameFromString . pretty) anyPrimType
                binop_names = S.fromList $ map (nameFromString . pretty)
                              [minBound..(maxBound::BinOp)]
                unop_names = S.fromList $ map nameFromString ["!"]
                fun_names = S.fromList $ map nameFromString ["shape"]
        atTopLevel _         = False
