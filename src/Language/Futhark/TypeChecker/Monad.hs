{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, TupleSections #-}
-- | Main monad in which the type checker runs, as well as ancillary
-- data definitions.
module Language.Futhark.TypeChecker.Monad
  ( TypeError(..)
  , TypeM
  , runTypeM
  , askEnv
  , askImportTable
  , checkQualName

  , MonadTypeChecker(..)
  , badOnLeft
  , expandType

  , require

  , Warnings

  , Env(..)
  , TySet
  , FunSig(..)
  , ImportTable
  , NameMap
  , ValBinding(..)
  , Mod(..)
  , FunBinding
  , TypeBinding(..)
  , MTy(..)

  , envVals
  , envMods

  , anySignedType
  , anyUnsignedType
  , anyIntType
  , anyFloatType
  , anyNumberType
  , anyPrimType

  , Namespace(..)
  , ppSpace
  , intrinsicsNameMap
  , topLevelNameMap
  )
where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
import Data.List
import Data.Loc
import Data.Maybe
import Data.Either
import Data.Ord
import Data.Hashable

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Prelude hiding (mapM)

import Language.Futhark
import Futhark.FreshNames hiding (newName)
import qualified Futhark.FreshNames

-- | Information about an error during type checking.  The 'Show'
-- instance for this type produces a human-readable description.
data TypeError =
    TypeError SrcLoc String
  | UnifyError SrcLoc (TypeBase Rank ()) SrcLoc (TypeBase Rank ())
  | UnexpectedType SrcLoc
    (TypeBase Rank ()) [TypeBase Rank ()]
  | ReturnTypeError SrcLoc Name (TypeBase Rank ()) (TypeBase Rank ())
  | DupDefinitionError Namespace Name SrcLoc SrcLoc
  | DupPatternError Name SrcLoc SrcLoc
  | InvalidPatternError (PatternBase NoInfo Name)
    (TypeBase Rank ()) (Maybe String) SrcLoc
  | UnknownVariableError Namespace (QualName Name) SrcLoc
  | ParameterMismatch (Maybe (QualName Name)) SrcLoc
    (Either Int [TypeBase Rank ()]) [TypeBase Rank ()]
  | UseAfterConsume Name SrcLoc SrcLoc
  | IndexingError Int Int SrcLoc
  | CurriedConsumption (QualName Name) SrcLoc
  | BadLetWithValue SrcLoc
  | ReturnAliased Name Name SrcLoc
  | UniqueReturnAliased Name SrcLoc
  | PermutationError SrcLoc [Int] Int
  | DimensionNotInteger SrcLoc Name
  | InvalidUniqueness SrcLoc (TypeBase Rank ())
  | UndefinedType SrcLoc (QualName Name)
  | InvalidField SrcLoc Type String
  | UnderscoreUse SrcLoc (QualName Name)
  | ValueIsNotFunction SrcLoc (QualName Name) Type
  | FunctionIsNotValue SrcLoc (QualName Name)
  | UniqueConstType SrcLoc Name (TypeBase Rank ())
  | EntryPointConstReturnDecl SrcLoc Name (QualName Name)
  | UndeclaredFunctionReturnType SrcLoc (QualName Name)
  | UnappliedFunctor SrcLoc

instance Show TypeError where
  show (TypeError pos msg) =
    "Type error at " ++ locStr pos ++ ":\n" ++ msg
  show (UnifyError e1loc t1 e2loc t2) =
    "Cannot unify type " ++ pretty t1 ++
    " of expression at " ++ locStr e1loc ++
    "\nwith type " ++ pretty t2 ++
    " of expression at " ++ locStr e2loc
  show (UnexpectedType loc _ []) =
    "Type of expression at " ++ locStr loc ++
    "cannot have any type - possibly a bug in the type checker."
  show (UnexpectedType loc t ts) =
    "Type of expression at " ++ locStr loc ++ " must be one of " ++
    intercalate ", " (map pretty ts) ++ ", but is " ++
    pretty t ++ "."
  show (ReturnTypeError pos fname rettype bodytype) =
    "Declaration of function " ++ nameToString fname ++ " at " ++ locStr pos ++
    " declares return type " ++ pretty rettype ++ ", but body has type " ++
    pretty bodytype
  show (DupDefinitionError space name pos1 pos2) =
    "Duplicate definition of " ++ ppSpace space ++ " " ++ nameToString name ++ ".  Defined at " ++
    locStr pos1 ++ " and " ++ locStr pos2 ++ "."
  show (DupPatternError name pos1 pos2) =
    "Duplicate binding of '" ++ pretty name ++ "'; at " ++
    locStr pos1 ++ " and " ++ locStr pos2 ++ "."
  show (InvalidPatternError pat t desc loc) =
    "Pattern " ++ pretty pat ++
    " cannot match value of type " ++ pretty t ++ " at " ++ locStr loc ++ end
    where end = case desc of Nothing -> "."
                             Just desc' -> ":\n" ++ desc'
  show (UnknownVariableError space name pos) =
    "Unknown " ++ ppSpace space ++ " " ++ pretty name ++ " referenced at " ++ locStr pos ++ "."
  show (ParameterMismatch fname pos expected got) =
    "In call of " ++ fname' ++ " at position " ++ locStr pos ++ ":\n" ++
    "expecting " ++ show nexpected ++ " argument(s) of type(s) " ++
     expected' ++ ", but got " ++ show ngot ++
    " arguments of types " ++ intercalate ", " (map pretty got) ++ "."
    where (nexpected, expected') =
            case expected of
              Left i -> (i, "(polymorphic)")
              Right ts -> (length ts, intercalate ", " $ map pretty ts)
          ngot = length got
          fname' = maybe "anonymous function" (("function "++) . pretty) fname
  show (UseAfterConsume name rloc wloc) =
    "Variable " ++ pretty name ++ " used at " ++ locStr rloc ++
    ", but it was consumed at " ++ locStr wloc ++ ".  (Possibly through aliasing)"
  show (IndexingError dims got pos) =
    show got ++ " indices given at " ++ locStr pos ++
    ", but type of indexee  has " ++ show dims ++ " dimension(s)."
  show (CurriedConsumption fname loc) =
    "Function " ++ pretty fname ++
    " curried over a consuming parameter at " ++ locStr loc ++ "."
  show (BadLetWithValue loc) =
    "New value for elements in let-with shares data with source array at " ++
    locStr loc ++ ".  This is illegal, as it prevents in-place modification."
  show (ReturnAliased fname name loc) =
    "Unique return value of function " ++ nameToString fname ++ " at " ++
    locStr loc ++ " is aliased to " ++ pretty name ++ ", which is not consumed."
  show (UniqueReturnAliased fname loc) =
    "A unique tuple element of return value of function " ++
    nameToString fname ++ " at " ++ locStr loc ++
    " is aliased to some other tuple component."
  show (PermutationError loc perm rank) =
    "The permutation (" ++ intercalate ", " (map show perm) ++
    ") is not valid for array argument of rank " ++ show rank ++ " at " ++
    locStr loc ++ "."
  show (DimensionNotInteger loc name) =
    "Dimension declaration " ++ pretty name ++ " at " ++ locStr loc ++
    " should be an integer."
  show (InvalidUniqueness loc t) =
    "Attempt to declare unique non-array " ++ pretty t ++ " at " ++ locStr loc ++ "."
  show (UndefinedType loc name) =
    "Unknown type " ++ pretty name ++ " referenced at " ++ locStr loc ++ "."
  show (InvalidField loc t field) =
    "Attempt to access field '" ++ field ++ "' of value of type " ++
    pretty t ++ " at " ++ locStr loc ++ "."
  show (UnderscoreUse loc name) =
    "Use of " ++ pretty name ++ " at " ++ locStr loc ++
    ": variables prefixed with underscore must not be accessed."
  show (ValueIsNotFunction loc name t) =
    "Attempt to use value " ++ pretty name ++ " of type " ++ pretty t ++
    " as function at " ++ locStr loc ++ "."
  show (FunctionIsNotValue loc name) =
    "Attempt to use function " ++ pretty name ++ " as value at " ++ locStr loc ++ "."
  show (UniqueConstType loc name t) =
    "Constant " ++ pretty name ++ " defined with unique type " ++ pretty t ++ " at " ++
    locStr loc ++ ", which is not allowed."
  show (EntryPointConstReturnDecl loc fname cname) =
    "Use of constant " ++ pretty cname ++
    " to annotate return type of entry point " ++ pretty fname ++
    " at " ++ locStr loc ++ " is not allowed."
  show (UndeclaredFunctionReturnType loc fname) =
    "Function '" ++ pretty fname ++ "' with no return type declaration called at " ++
    locStr loc
  show (UnappliedFunctor loc) =
    "Cannot have parametric module at " ++ locStr loc ++ "."

-- | A set of abstract types and where their definition is expected.
type TySet = S.Set (QualName VName)

-- | Representation of a module, which is either a plain environment,
-- or a parametric module ("functor" in SML).
data Mod = ModEnv Env
         | ModFun FunSig
         deriving (Show)

-- | A parametric functor consists of a set of abstract types, the
-- environment of its parameter, and the resulting module type.
data FunSig = FunSig TySet Env MTy
            deriving (Show)

-- | Return type and a list of argument types, and names that are used
-- free inside the function.
type FunBinding = ([StructType], StructType)

-- | Representation of a module type.
data MTy = MTy { mtyAbs :: TySet
                 -- ^ Abstract types in the module type.
               , mtyMod :: Mod
               }
         deriving (Show)

-- | A binding from a name to its definition as a type.
newtype TypeBinding = TypeAbbr StructType
                    deriving (Show)

data ValBinding = BoundV Type
                | BoundF FunBinding
                deriving (Show)

type NameMap = M.Map (Namespace, Name) VName

-- | Modules produces environment with this representation.
data Env = Env { envVtable :: M.Map VName ValBinding
               , envTypeTable :: M.Map VName TypeBinding
               , envSigTable :: M.Map VName MTy
               , envModTable :: M.Map VName Mod
               , envNameMap :: NameMap
               } deriving (Show)

instance Monoid Env where
  mempty = Env mempty mempty mempty mempty mempty
  Env vt1 tt1 st1 mt1 nt1 `mappend` Env vt2 tt2 st2 mt2 nt2 =
    Env (vt1<>vt2) (tt1<>tt2) (st1<>st2) (mt1<>mt2) (nt1<>nt2)

envVals :: Env -> [(VName, ([StructType], StructType))]
envVals = map select . M.toList . envVtable
  where select (name, BoundF fun) =
          (name, fun)
        select (name, BoundV t) =
          (name, ([], vacuousShapeAnnotations $ toStruct t))

envMods :: Env -> [(VName,Mod)]
envMods = M.toList . envModTable

-- | The warnings produced by the type checker.  The 'Show' instance
-- produces a human-readable description.
newtype Warnings = Warnings [(SrcLoc, String)]

instance Monoid Warnings where
  mempty = Warnings mempty
  Warnings ws1 `mappend` Warnings ws2 = Warnings $ ws1 <> ws2

instance Show Warnings where
  show (Warnings []) = ""
  show (Warnings ws) =
    intercalate "\n\n" ws' ++ "\n"
    where ws' = map showWarning $ sortBy (comparing (off . locOf . fst)) ws
          off NoLoc = 0
          off (Loc p _) = posCoff p
          showWarning (loc, w) =
            "Warning at " ++ locStr loc ++ ":\n  " ++ w

singleWarning :: SrcLoc -> String -> Warnings
singleWarning loc problem = Warnings [(loc, problem)]

type ImportTable = M.Map FilePath Env

-- | The type checker runs in this monad.
newtype TypeM a = TypeM (RWST
                         (Env, ImportTable) -- Reader
                         Warnings           -- Writer
                         VNameSource        -- State
                         (Except TypeError) -- Inner monad
                         a)
  deriving (Monad, Functor, Applicative,
            MonadReader (Env, ImportTable),
            MonadWriter Warnings,
            MonadState VNameSource,
            MonadError TypeError)

runTypeM :: Env -> ImportTable -> VNameSource -> TypeM a
         -> Either TypeError (a, Warnings, VNameSource)
runTypeM env imports src (TypeM m) = do
  (x, src', ws) <- runExcept $ runRWST m (env, imports) src
  return (x, ws, src')

askImportTable :: TypeM ImportTable
askImportTable = asks snd

askEnv :: TypeM Env
askEnv = asks fst

class MonadError TypeError m => MonadTypeChecker m where
  bad :: TypeError -> m a
  warn :: SrcLoc -> String -> m ()

  newName :: VName -> m VName
  newID :: Name -> m VName

  checkName :: Namespace -> Name -> SrcLoc -> m VName

  lookupType :: SrcLoc -> QualName Name -> m (QualName VName, StructType)
  lookupMod :: SrcLoc -> QualName Name -> m (QualName VName, Mod)
  lookupMTy :: SrcLoc -> QualName Name -> m (QualName VName, MTy)
  lookupImport :: SrcLoc -> FilePath -> m Env

expandType :: MonadTypeChecker m =>
              (SrcLoc -> DimDecl Name -> m (DimDecl VName))
           -> TypeExp Name
           -> m (TypeExp VName, StructType)
expandType _ (TEVar name loc) = do
  (name', t) <- lookupType loc name
  return (TEVar name' loc, t)
expandType look (TETuple ts loc) = do
  (ts', ts_s) <- unzip <$> mapM (expandType look) ts
  return (TETuple ts' loc, tupleRecord ts_s)
expandType look t@(TERecord fs loc) = do
  -- Check for duplicate field names.
  let field_names = map fst fs
  unless (sort field_names == sort (nub field_names)) $
    bad $ TypeError loc $ "Duplicate record fields in " ++ pretty t

  fs_and_ts <- traverse (expandType look) $ M.fromList fs
  let fs' = fmap fst fs_and_ts
      ts_s = fmap snd fs_and_ts
  return (TERecord (M.toList fs') loc, Record ts_s)
expandType look (TEArray t d loc) = do
  (t', st) <- expandType look t
  d' <- look loc d
  return (TEArray t' d' loc, arrayOf st (ShapeDecl [d']) Nonunique)
expandType look (TEUnique t loc) = do
  (t', st) <- expandType look t
  case st of
    Array{} -> return (t', st `setUniqueness` Unique)
    _       -> throwError $ InvalidUniqueness loc $ toStructural st

-- | @require ts e@ causes a 'TypeError' if @typeOf e@ does not unify
-- with one of the types in @ts@.  Otherwise, simply returns @e@.
require :: MonadTypeChecker m => [TypeBase Rank ()] -> Exp -> m Exp
require ts e
  | any (toStruct (typeOf e) `similarTo`) ts = return e
  | otherwise = bad $ UnexpectedType (srclocOf e)
                      (toStructural $ typeOf e) ts

instance MonadTypeChecker TypeM where
  bad = throwError

  warn loc problem = tell $ singleWarning loc problem

  newName s = do src <- get
                 let (s', src') = Futhark.FreshNames.newName src s
                 put src'
                 return s'

  newID s = newName $ ID (s, 0)

  checkName space name loc = do
    (_, QualName _ name') <- checkQualName space (qualName name) loc
    return name'

  lookupType loc qn = do
    (scope, qn'@(QualName _ name)) <- checkQualName Type qn loc
    case M.lookup name $ envTypeTable scope of
      Nothing             -> bad $ UndefinedType loc qn
      Just (TypeAbbr def) -> return (qn', def)

  lookupMod loc qn = do
    (scope, qn'@(QualName _ name)) <- checkQualName Structure qn loc
    case M.lookup name $ envModTable scope of
      Nothing -> bad $ UnknownVariableError Structure qn loc
      Just m  -> return (qn', m)

  lookupMTy loc qn = do
    (scope, qn'@(QualName _ name)) <- checkQualName Signature qn loc
    (qn',) <$> maybe explode return (M.lookup name $ envSigTable scope)
    where explode = bad $ UnknownVariableError Signature qn loc

  lookupImport loc file = do
    imports <- askImportTable
    case M.lookup file imports of
      Nothing    -> bad $ TypeError loc $ "Unknown import \"" ++ file ++ "\""
      Just scope -> return scope

checkQualName :: Namespace -> QualName Name -> SrcLoc -> TypeM (Env, QualName VName)
checkQualName space qn@(QualName quals name) loc = do
  env <- askEnv
  descend env quals
  where descend scope []
          | Just name' <- M.lookup (space, name) $ envNameMap scope =
              return (scope, QualName quals name')
          | otherwise =
              bad $ UnknownVariableError space qn loc

        descend scope (q:qs)
          | Just q' <- M.lookup (Structure, q) $ envNameMap scope,
            Just res <- M.lookup q' $ envModTable scope =
              case res of
                ModEnv q_scope -> descend q_scope qs
                ModFun{} -> bad $ UnappliedFunctor loc
          | otherwise =
              bad $ UnknownVariableError space qn loc

badOnLeft :: MonadTypeChecker m => Either TypeError a -> m a
badOnLeft = either bad return

anySignedType :: [TypeBase Rank ()]
anySignedType = map (Prim . Signed) [minBound .. maxBound]

anyUnsignedType :: [TypeBase Rank ()]
anyUnsignedType = map (Prim . Unsigned) [minBound .. maxBound]

anyIntType :: [TypeBase Rank ()]
anyIntType = anySignedType ++ anyUnsignedType

anyFloatType :: [TypeBase Rank ()]
anyFloatType = map (Prim . FloatType) [minBound .. maxBound]

anyNumberType :: [TypeBase Rank ()]
anyNumberType = anyIntType ++ anyFloatType

anyPrimType :: [TypeBase Rank ()]
anyPrimType = Prim Bool : anyIntType ++ anyFloatType

--- Name handling

data Namespace = Term -- ^ Functions and values.
               | Type
               | Structure
               | Signature
               deriving (Eq, Ord, Show, Enum)

ppSpace :: Namespace -> String
ppSpace Term = "value"
ppSpace Type = "type"
ppSpace Structure = "module"
ppSpace Signature = "module type"

instance Hashable Namespace where
  hashWithSalt salt = hashWithSalt salt . fromEnum

intrinsicsNameMap :: NameMap
intrinsicsNameMap = M.fromList $ map mapping $ M.toList intrinsics
  where mapping (v, IntrinsicType{}) = ((Type, baseName v), v)
        mapping (v, _)               = ((Term, baseName v), v)

topLevelNameMap :: NameMap
topLevelNameMap = M.filterWithKey (\k _ -> atTopLevel k) intrinsicsNameMap
  where atTopLevel :: (Namespace, Name) -> Bool
        atTopLevel (Type, _) = True
        atTopLevel (Term, v) = v `S.member` (type_names <> binop_names <> unop_names)
          where type_names = S.fromList $ map (nameFromString . pretty) anyPrimType
                binop_names = S.fromList $ map (nameFromString . pretty)
                              [minBound..(maxBound::BinOp)]
                unop_names = S.fromList $ map nameFromString ["~", "!"]
        atTopLevel _         = False
