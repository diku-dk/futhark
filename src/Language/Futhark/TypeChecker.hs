{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, TupleSections #-}
-- | The type checker checks whether the program is type-consistent
-- and adds type annotations and various other elaborations.  The
-- program does not need to have any particular properties for the
-- type checker to function; in particular it does not need unique
-- names.
module Language.Futhark.TypeChecker
  ( checkProg
  , TypeError
  , Warnings
  , FileModule
  , Imports
  )
  where

import Control.Applicative
import Control.Monad.Except hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.Writer hiding (mapM)
import Control.Monad.State hiding (mapM)
import Control.Monad.RWS hiding (mapM)
import Data.List
import Data.Loc
import Data.Maybe
import Data.Either
import Data.Ord
import Data.Hashable
import Data.Traversable (mapM)

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

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
  | UnappliedFunctor (QualName Name) SrcLoc

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
  show (UnappliedFunctor name loc) =
    "Parametrised module " ++ pretty name ++ " used at " ++ locStr loc ++
    " without an argument."

-- | Return type and a list of argument types, and names that are used
-- free inside the function.  The latter is non-empty only for local
-- functions.
type FunBinding = ([StructType], StructType, Occurences)

-- | A binding from a name to its definition as a type.
data TypeBinding = TypeAbbr StructType
                 | TypeAbs
                 deriving (Show)

data ModBinding = ModMod Scope
                | ModFunctor FunctorF
                deriving (Show)

data Binding = BoundV Type
             | BoundF FunBinding
             | OverloadedF [([TypeBase Rank ()],FunBinding)]
             | EqualityF
             | UnknownF [StructType]
             | UnknownV
             | WasConsumed SrcLoc
             deriving (Show)

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

type NameMap = HM.HashMap (Namespace, Name) VName

-- | A function for applying a functor to a module.
newtype FunctorF = FunctorF { applyFunctor :: SrcLoc -> Scope
                                           -> TypeM (Scope,
                                                     HM.HashMap VName VName,
                                                     HM.HashMap VName VName)
                            }

instance Show FunctorF where
  show _ = "#<FunctorF>"

-- | Type checking happens with access to this environment.  The
-- tables will be extended during type-checking as bindings come into
-- scope.
data Scope  = Scope { envVtable :: HM.HashMap VName Binding
                    , envTypeTable :: HM.HashMap VName TypeBinding
                    , envSigTable :: HM.HashMap VName Scope
                    , envModTable :: HM.HashMap Name ModBinding
                    , envImportTable :: HM.HashMap FilePath Scope
                    , envNameMap :: NameMap
                    } deriving (Show)

instance Monoid Scope where
  mempty = Scope mempty mempty mempty mempty mempty mempty
  Scope vt1 tt1 st1 mt1 it1 nt1 `mappend` Scope vt2 tt2 st2 mt2 it2 nt2 =
    Scope (vt1<>vt2) (tt1<>tt2) (st1<>st2) (mt1<>mt2) (it1<>it2) (nt1<>nt2)

initialScope :: Scope
initialScope = intrinsicsModule
               { envModTable = initialModTable
               , envNameMap = HM.insert
                              (Structure, nameFromString "Intrinsics")
                              (ID (nameFromString "Intrinsics", 0))
                              topLevelNameMap
               }
  where initialVtable = HM.fromList $ mapMaybe addIntrinsicF $ HM.toList intrinsics
        initialTypeTable = HM.fromList $ mapMaybe addIntrinsicT $ HM.toList intrinsics
        initialModTable = HM.singleton (nameFromString "Intrinsics") (ModMod intrinsicsModule)

        intrinsicsModule = Scope initialVtable initialTypeTable mempty mempty mempty intrinsicsNameMap

        addIntrinsicF (name, IntrinsicMonoFun ts t) =
          Just (name, BoundF (map Prim ts, Prim t, mempty))
        addIntrinsicF (name, IntrinsicPolyFun variants) =
          Just (name, OverloadedF $ map frob variants)
          where frob :: ([PrimType], PrimType) -> ([TypeBase Rank ()],FunBinding)
                frob (pts, rt) = (map Prim pts, (map Prim pts, Prim rt, mempty))
        addIntrinsicF (name, IntrinsicEquality) =
          Just (name, EqualityF)
        addIntrinsicF _ = Nothing

        addIntrinsicT (name, IntrinsicType t) =
          Just (name, TypeAbbr $ Prim t)
        addIntrinsicT _ =
          Nothing

        intrinsicsNameMap :: NameMap
        intrinsicsNameMap = HM.fromList $ map mapping $ HM.toList intrinsics
          where mapping (v, IntrinsicType{}) = ((Type, baseName v), v)
                mapping (v, _)               = ((Term, baseName v), v)

        topLevelNameMap :: NameMap
        topLevelNameMap = HM.filterWithKey (\k _ -> atTopLevel k) intrinsicsNameMap

        atTopLevel :: (Namespace, Name) -> Bool
        atTopLevel (Type, _) = True
        atTopLevel (Term, v) = v `HS.member` (type_names <> binop_names <> unop_names)
          where type_names = HS.fromList $ map (nameFromString . pretty) anyPrimType
                binop_names = HS.fromList $ map (nameFromString . pretty)
                              [minBound..(maxBound::BinOp)]
                unop_names = HS.fromList $ map nameFromString ["~", "!"]
        atTopLevel _         = False

scopeTypeAbbrs :: Scope -> [(VName,StructType)]
scopeTypeAbbrs = mapMaybe unTypeAbbr . HM.toList . envTypeTable
  where unTypeAbbr (v, TypeAbbr t) = Just (v, t)
        unTypeAbbr (_, TypeAbs)    = Nothing

scopeAbsTypes :: Scope -> [VName]
scopeAbsTypes = mapMaybe select . HM.toList . envTypeTable
  where select (name, TypeAbs) = Just name
        select _               = Nothing

scopeVals :: Scope -> [(VName, ([StructType], StructType, Occurences))]
scopeVals = mapMaybe select . HM.toList . envVtable
  where select (name, BoundF fun) =
          Just (name, fun)
        select (name, BoundV t) =
          Just (name, ([], vacuousShapeAnnotations $ toStruct t, mempty))
        select _ =
          Nothing

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

-- | The (abstract) result of type checking some file.  Can be passed
-- to further invocations of the type checker.
newtype FileModule = FileModule { fileScope :: Scope }

-- | A mapping from import names to imports.
type Imports = HM.HashMap FilePath FileModule

-- | The type checker runs in this monad.
newtype TypeM a = TypeM (RWST
                         Scope       -- Reader
                         Occurences  -- Writer
                         VNameSource -- State
                         (WriterT Warnings (Except TypeError)) -- Inner monad
                         a)
  deriving (Monad, Functor, Applicative,
            MonadReader Scope,
            MonadWriter Occurences,
            MonadState VNameSource,
            MonadError TypeError)

runTypeM :: Scope -> VNameSource -> TypeM a
         -> Either TypeError (a, Warnings, VNameSource)
runTypeM env src (TypeM m) = do
  ((x, src', _), ws) <- runExcept $ runWriterT $ runRWST m env src
  return (x, ws, src')

bad :: TypeError -> TypeM a
bad = throwError

warn :: SrcLoc -> String -> TypeM ()
warn loc problem = TypeM $ lift $ tell $ singleWarning loc problem

newName :: VName -> TypeM VName
newName s = do src <- get
               let (s', src') = Futhark.FreshNames.newName src s
               put src'
               return s'

newID :: Name -> TypeM VName
newID s = newName $ ID (s, 0)

liftEither :: Either TypeError a -> TypeM a
liftEither = either bad return

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

bindSpaced :: [(Namespace, Name)] -> TypeM a -> TypeM a
bindSpaced names body = do
  names' <- mapM (newID . snd) names
  let mapping = HM.fromList (zip names names')
  bindNameMap mapping body

bindNameMap :: NameMap -> TypeM a -> TypeM a
bindNameMap m = local $ \env -> env { envNameMap = m <> envNameMap env }

--- Consumption

occur :: Occurences -> TypeM ()
occur = tell

-- | Proclaim that we have made read-only use of the given variable.
observe :: Ident -> TypeM ()
observe (Ident nm (Info t) loc) =
  let als = nm `HS.insert` aliases t
  in occur [observation als loc]

-- | Proclaim that we have written to the given variable.
consume :: SrcLoc -> Names VName -> TypeM ()
consume loc als = occur [consumption als loc]

-- | Proclaim that we have written to the given variable, and mark
-- accesses to it and all of its aliases as invalid inside the given
-- computation.
consuming :: Ident -> TypeM a -> TypeM a
consuming (Ident name (Info t) loc) m = do
  consume loc $ name `HS.insert` aliases t
  local consume' m
  where consume' env =
          env { envVtable = HM.insert name (WasConsumed loc) $ envVtable env }

collectOccurences :: TypeM a -> TypeM (a, Occurences)
collectOccurences m = pass $ do
  (x, dataflow) <- listen m
  return ((x, dataflow), const mempty)

tapOccurences :: TypeM a -> TypeM (a, Occurences)
tapOccurences = listen

maybeCheckOccurences :: Occurences -> TypeM ()
maybeCheckOccurences = liftEither . checkOccurences

checkIfUsed :: Occurences -> Ident -> TypeM ()
checkIfUsed occs v
  | not $ identName v `HS.member` allOccuring occs,
    not $ "_" `isPrefixOf` pretty (identName v) =
      warn (srclocOf v) $ "Unused variable '"++pretty (baseName $ identName v)++"'."
  | otherwise =
      return ()

alternative :: TypeM a -> TypeM b -> TypeM (a,b)
alternative m1 m2 = pass $ do
  (x, occurs1) <- listen m1
  (y, occurs2) <- listen m2
  maybeCheckOccurences occurs1
  maybeCheckOccurences occurs2
  let usage = occurs1 `altOccurences` occurs2
  return ((x, y), const usage)

-- | Make all bindings nonunique.
noUnique :: TypeM a -> TypeM a
noUnique = local (\env -> env { envVtable = HM.map set $ envVtable env})
  where set (BoundV t)        = BoundV $ t `setUniqueness` Nonunique
        set (BoundF f)        = BoundF f
        set (OverloadedF f)   = OverloadedF f
        set EqualityF         = EqualityF
        set (UnknownF ts)     = UnknownF ts
        set UnknownV          = UnknownV
        set (WasConsumed loc) = WasConsumed loc

onlySelfAliasing :: TypeM a -> TypeM a
onlySelfAliasing = local (\env -> env { envVtable = HM.mapWithKey set $ envVtable env})
  where set k (BoundV t)        = BoundV $ t `addAliases` HS.intersection (HS.singleton k)
        set _ (BoundF f)        = BoundF f
        set _ (OverloadedF f)   = OverloadedF f
        set _ EqualityF         = EqualityF
        set _ (UnknownF ts)     = UnknownF ts
        set _ UnknownV          = UnknownV
        set _ (WasConsumed loc) = WasConsumed loc

--- General binding.

binding :: [Ident] -> TypeM a -> TypeM a
binding bnds = check . local (`bindVars` bnds)
  where bindVars :: Scope -> [Ident] -> Scope
        bindVars = foldl bindVar

        bindVar :: Scope -> Ident -> Scope
        bindVar env (Ident name (Info tp) _) =
          let inedges = HS.toList $ aliases tp
              update (BoundV tp')
              -- If 'name' is tuple-typed, don't alias the components
              -- to 'name', because tuples have no identity beyond
              -- their components.
                | Tuple _ <- tp = BoundV tp'
                | otherwise     = BoundV (tp' `addAliases` HS.insert name)
              update b = b
          in env { envVtable = HM.insert name (BoundV tp) $
                               adjustSeveral update inedges $
                               envVtable env }

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
bindingAlsoNames :: [Ident] -> TypeM a -> TypeM a
bindingAlsoNames idents body = do
  let varnames = map ((Term,) . baseName . identName) idents
      substs   = map identName idents
  bindNameMap (HM.fromList (zip varnames substs)) $
    binding idents body

bindingIdent :: IdentBase NoInfo Name -> Type -> (Ident -> TypeM a)
             -> TypeM a
bindingIdent (Ident v NoInfo vloc) t m =
  bindSpaced [(Term, v)] $ do
    v' <- checkName Term v vloc
    let ident = Ident v' (Info t) vloc
    binding [ident] $ m ident

bindingPatterns :: [(PatternBase NoInfo Name, InferredType)]
               -> ([Pattern] -> TypeM a) -> TypeM a
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
               -> (Pattern -> TypeM a) -> TypeM a
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

--- The main checker

checkQualName :: Namespace -> QualName Name -> SrcLoc -> TypeM (Scope, QualName VName)
checkQualName space qn@(QualName quals name) loc = do
  scope <- ask
  descend scope quals
  where descend scope []
          | Just name' <- HM.lookup (space, name) $ envNameMap scope =
              return (scope, QualName quals name')
          | otherwise =
              bad $ UnknownVariableError space qn loc

        descend scope (q:qs)
          | Just res <- HM.lookup q $ envModTable scope =
              case res of
                ModMod q_scope -> descend q_scope qs
                ModFunctor{} -> bad $ UnappliedFunctor qn loc
          | otherwise =
              bad $ UnknownVariableError space qn loc

checkName :: Namespace -> Name -> SrcLoc -> TypeM VName
checkName space name loc = do
  (_, QualName _ name') <- checkQualName space (qualName name) loc
  return name'

checkVar :: QualName Name -> SrcLoc -> TypeM (QualName VName, Type)
checkVar qn loc = do
  (scope, qn'@(QualName _ name)) <- checkQualName Term qn loc
  case HM.lookup name $ envVtable scope of
    Nothing -> bad $ UnknownVariableError Term qn loc
    Just (BoundV t) | "_" `isPrefixOf` pretty name -> bad $ UnderscoreUse loc qn
                    | otherwise -> return (qn', t)
    Just BoundF{} -> bad $ FunctionIsNotValue loc qn
    Just EqualityF -> bad $ FunctionIsNotValue loc qn
    Just OverloadedF{} -> bad $ FunctionIsNotValue loc qn
    Just UnknownF{} -> bad $ FunctionIsNotValue loc qn
    Just UnknownV -> bad $ TypeError loc $ pretty qn ++ " has unknown type"
    Just (WasConsumed wloc) -> bad $ UseAfterConsume (baseName name) loc wloc

-- | In a few rare cases (overloaded builtin functions), the type of
-- the parameters actually matters.
lookupFunction :: QualName Name -> [Type] -> SrcLoc -> TypeM (QualName VName, FunBinding)
lookupFunction qn argtypes loc = do
  (scope, qn'@(QualName _ name)) <- checkQualName Term qn loc
  case HM.lookup name $ envVtable scope of
    Nothing -> bad $ UnknownVariableError Term qn loc
    Just (WasConsumed wloc) -> bad $ UseAfterConsume (baseName name) loc wloc
    Just (BoundV t) -> bad $ ValueIsNotFunction loc qn t
    Just UnknownF{} -> bad $ UndeclaredFunctionReturnType loc qn
    Just UnknownV -> bad $ TypeError loc $ pretty qn ++ " has unknown type"
    Just (BoundF f) -> return (qn', f)
    Just (OverloadedF overloads) ->
      case lookup (map toStructural argtypes) overloads of
        Nothing -> bad $ TypeError loc $ "Overloaded function " ++ pretty qn ++
                   " not defined for arguments of types " ++
                   intercalate ", " (map pretty argtypes)
        Just f -> return (qn', f)
    Just EqualityF
      | [t1,t2] <- argtypes,
        concreteType t1,
        concreteType t2,
        t1 == t2 ->
          return (qn', (map (vacuousShapeAnnotations . toStruct) [t1, t2],
                        Prim Bool,
                        mempty))
      | otherwise ->
          bad $ TypeError loc $ "Equality not defined for arguments of types " ++
          intercalate ", " (map pretty argtypes)

lookupType :: SrcLoc -> QualName Name -> TypeM (QualName VName, StructType)
lookupType loc qn = do
  (scope, qn'@(QualName _ name)) <- checkQualName Type qn loc
  case HM.lookup name $ envTypeTable scope of
    Nothing             -> bad $ UndefinedType loc qn
    Just (TypeAbbr def) -> return (qn', def)
    Just TypeAbs        -> return (qn', TypeVar $ typeNameFromQualName qn')

lookupSig :: SrcLoc -> QualName Name -> TypeM (QualName VName, Scope)
lookupSig loc qn = do
  (scope, qn'@(QualName _ name)) <- checkQualName Signature qn loc
  (qn',) <$> maybe explode return (HM.lookup name $ envSigTable scope)
  where explode = bad $ UnknownVariableError Signature qn loc

lookupMod :: SrcLoc -> QualName Name -> TypeM (QualName VName, Scope)
lookupMod loc qn = do
  (scope, qn'@(QualName _ name)) <- checkQualName Structure qn loc
  case HM.lookup (baseName name) $ envModTable scope of
    Nothing                -> bad $ UnknownVariableError Structure qn loc
    Just (ModMod modscope) -> return (qn', modscope)
    Just ModFunctor{}      -> bad $ UnappliedFunctor qn loc

lookupImport :: SrcLoc -> FilePath -> TypeM Scope
lookupImport loc file = do
  imports <- asks envImportTable
  case HM.lookup file imports of
    Nothing    -> bad $ TypeError loc $ "Unknown import \"" ++ file ++ "\""
    Just scope -> return scope

lookupFunctor :: SrcLoc -> QualName Name -> TypeM (QualName VName, FunctorF)
lookupFunctor loc qn = do
  (scope, qn'@(QualName _ name)) <- checkQualName Structure qn loc
  case HM.lookup (baseName name) $ envModTable scope of
    Nothing -> bad $ UnknownVariableError Structure qn loc
    Just ModMod{}            -> bad $ TypeError loc "Non-parametrised module given an argument."
    Just (ModFunctor fscope) -> return (qn', fscope)

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
unifyExpTypes :: Exp -> Exp -> TypeM Type
unifyExpTypes e1 e2 =
  maybe (bad $ UnifyError
         (srclocOf e1) (toStructural t1)
         (srclocOf e2) (toStructural t2)) return $
  unifyTypes (typeOf e1) (typeOf e2)
  where t1 = typeOf e1
        t2 = typeOf e2

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

-- | @require ts e@ causes a 'TypeError' if @typeOf e@ does not unify
-- with one of the types in @ts@.  Otherwise, simply returns @e@.
-- This function is very useful in 'checkExp'.
require :: [TypeBase Rank ()] -> Exp -> TypeM Exp
require ts e
  | any (toStruct (typeOf e) `similarTo`) ts = return e
  | otherwise = bad $ UnexpectedType (srclocOf e)
                      (toStructural $ typeOf e) ts

chompDecs :: [DecBase NoInfo Name]
          -> ([ValDecBase NoInfo Name], [DecBase NoInfo Name])
chompDecs (ValDec dec : xs) = let (valdecs, xs') = chompDecs xs
                              in (dec : valdecs, xs')
chompDecs xs                = ([], xs)

buildScopeFromDecs :: [ValDecBase NoInfo Name]
                   -> TypeM Scope
buildScopeFromDecs = foldM expandV mempty
  where
    paramDeclaredType (PatternAscription _ t) =
      Just $ declaredType t
    paramDeclaredType (TuplePattern ps loc) =
      TETuple <$> mapM paramDeclaredType ps <*> pure loc
    paramDeclaredType _ =
      Nothing

    expandV scope (FunDec (FunBind _ fname maybe_ret _ params _ loc)) = do
      fname' <- checkName Term fname loc
      argtypes <- forM params $ \param ->
        case paramDeclaredType param of
          Just t -> return t
          Nothing -> bad $ TypeError (srclocOf param) $
                     "Missing type information for parameter " ++
                     pretty param
      argtypes' <- mapM (fmap snd . checkTypeExpNoDims) argtypes
      boundf <- case maybe_ret of
        Just ret -> do (_, ret') <- checkTypeExpNoDims ret
                       return $ BoundF (argtypes', ret', mempty)
        Nothing -> return $ UnknownF argtypes'
      return scope { envVtable = HM.insert fname' boundf $
                                 envVtable scope
                   , envNameMap = HM.insert (Term, fname) fname' $
                                  envNameMap scope
                   }

    expandV scope (ConstDec (ConstBind cname maybe_t NoInfo _ loc)) = do
      cname' <- checkName Term cname loc
      entry <- case maybe_t of
        Just t -> do (_, st') <- checkTypeExpNoDims t
                     return $ BoundV $ removeShapeAnnotations $ fromStruct st'
        Nothing -> return UnknownV
      return scope { envVtable = HM.insert cname' entry $
                                 envVtable scope
                   , envNameMap = HM.insert (Term, cname) cname' $
                                  envNameMap scope
                   }

valDecScope :: ValDecBase Info VName -> Scope
valDecScope (FunDec fb) =
  mempty { envVtable =
             HM.singleton (funBindName fb)
             (BoundF (map paramType (funBindParams fb),
                      unInfo $ funBindRetType fb,
                      mempty))
         }
  where paramType :: Pattern -> StructType
        paramType = vacuousShapeAnnotations . toStruct . patternType
valDecScope (ConstDec cb) =
  mempty { envVtable =
             HM.singleton (constBindName cb)
             (BoundV $ removeShapeAnnotations $ fromStruct $ unInfo $ constBindType cb)
         }

-- | Type check a program containing no type information, yielding
-- either a type error or a program with complete type information.
-- Accepts a mapping from file names (excluding extension) to
-- previously type checker results.
checkProg :: Imports
          -> VNameSource
          -> UncheckedProg
          -> Either TypeError ((FileModule, Prog), Warnings, VNameSource)
checkProg files src prog = runTypeM scope' src $ checkProg' prog
  where scope' = initialScope { envImportTable = HM.map fileScope files }

checkProg' :: UncheckedProg -> TypeM (FileModule, Prog)
checkProg' (Prog decs) = do
  checkForDuplicateDecs decs
  (scope, decs') <- checkDecs decs
  return (FileModule scope, Prog decs')

checkForDuplicateDecs :: [DecBase NoInfo Name] -> TypeM ()
checkForDuplicateDecs =
  foldM_ (flip f) mempty
  where check namespace name loc known =
          case HM.lookup (namespace, name) known of
            Just loc' ->
              bad $ DupDefinitionError namespace name loc loc'
            _ -> return $ HM.insert (namespace, name) loc known

        f (ValDec (FunDec (FunBind _ name _ _ _ _ loc))) =
          check Term name loc

        f (ValDec (ConstDec (ConstBind name _ _ _ loc))) =
          check Term name loc

        f (TypeDec (TypeBind name _ loc)) =
          check Type name loc

        f (SigDec (SigBind name _ loc)) =
          check Signature name loc

        f (StructDec (StructBind name _ loc)) =
          check Structure name loc

        f (FunctorDec (FunctorBind name _ _ _ loc)) =
          check Structure name loc

        f OpenDec{} = return

checkSpecs :: [SpecBase NoInfo Name] -> TypeM (Scope, [SpecBase Info VName])

checkSpecs [] = return (mempty, [])

checkSpecs (ValSpec name paramtypes rettype loc : specs) =
  bindSpaced [(Term, name)] $ do
    name' <- checkName Term name loc
    paramtypes' <- mapM checkTypeDecl paramtypes
    rettype' <- checkTypeDecl rettype
    let paramtypes'' = map (unInfo . expandedType) paramtypes'
        rettype'' = unInfo $ expandedType rettype'
        valscope =
          mempty { envVtable = HM.singleton name' $
                               if null paramtypes''
                               then BoundV $ removeShapeAnnotations $
                                    rettype'' `addAliases` mempty
                               else BoundF (paramtypes'', rettype'', mempty)
                 , envNameMap = HM.singleton (Term, name) name'
                 }
    (scope, specs') <- local (valscope<>) $ checkSpecs specs
    return (scope <> valscope,
            ValSpec name' paramtypes' rettype' loc : specs')

checkSpecs (TypeAbbrSpec tdec : specs) =
  bindSpaced [(Type, typeAlias tdec)] $ do
    (tscope, tdec') <- checkTypeBind tdec
    (scope, specs') <- local (tscope<>) $ checkSpecs specs
    return (tscope <> scope,
            TypeAbbrSpec tdec' : specs')

checkSpecs (TypeSpec name loc : specs) =
  bindSpaced [(Type, name)] $ do
    name' <- checkName Type name loc
    let tscope = mempty { envNameMap = HM.singleton (Type, name) name'
                        , envTypeTable = HM.singleton name' TypeAbs
                        }
    (scope, specs') <- local (tscope<>) $ checkSpecs specs
    return (tscope <> scope,
            TypeSpec name' loc : specs')

checkSpecs (IncludeSpec e loc : specs) = do
  (e_scope, e') <- checkSigExp e
  (scope, specs') <- local (e_scope<>) $ checkSpecs specs
  return (e_scope <> scope,
          IncludeSpec e' loc : specs')

checkSigExp :: SigExpBase NoInfo Name -> TypeM (Scope, SigExpBase Info VName)
checkSigExp (SigVar name loc) = do
  (name', scope) <- lookupSig loc name
  return (scope, SigVar name' loc)
checkSigExp (SigSpecs specs loc) = do
  checkForDuplicateSpecs specs
  (scope, specs') <- checkSpecs specs
  return (scope, SigSpecs specs' loc)
checkSigExp (SigWith s (TypeRef tname td) loc) = do
  (s_scope, s') <- checkSigExp s
  td' <- checkTypeDecl td
  tname' <- local (s_scope<>) $ snd <$> checkQualName Type tname loc
  s_scope' <- refineScope loc s_scope tname' $ unInfo $ expandedType td'
  return (s_scope', SigWith s' (TypeRef tname' td') loc)

checkSigBind :: SigBindBase NoInfo Name -> TypeM (Scope, SigBindBase Info VName)
checkSigBind (SigBind name e loc) = do
  name' <- checkName Signature name loc
  (scope, e') <- checkSigExp e
  -- As a small convenience(?), binding a signature also implicitly
  -- binds a structure of the same name, which contains the type
  -- abbreviations in the signature.
  let sigmod = typeAbbrScopeFromSig scope
  return (mempty { envSigTable = HM.singleton name' scope
                 , envModTable = HM.singleton name $ ModMod sigmod
                 , envNameMap = HM.singleton (Signature, name) name'
                 },
          SigBind name' e' loc)
  where typeAbbrScopeFromSig :: Scope -> Scope
        typeAbbrScopeFromSig scope =
          let types = HM.map TypeAbbr $ HM.fromList $ scopeTypeAbbrs scope
              names = HM.fromList $ map nameMapping $ HM.toList types
          in mempty { envNameMap = names
                    , envTypeTable = types }
        nameMapping (v, _) = ((Type, baseName v), v)

checkModExp :: ModExpBase NoInfo Name -> TypeM (Scope, ModExpBase Info VName)
checkModExp (ModDecs decs loc) = do
  checkForDuplicateDecs decs
  (scope, decs') <- checkDecs decs
  return (scope, ModDecs decs' loc)
checkModExp (ModVar v loc) = do
  (v', scope) <- lookupMod loc v
  when (baseName (qualLeaf v') == nameFromString "Intrinsics" &&
        baseTag (qualLeaf v') <= maxIntrinsicTag) $
    bad $ TypeError loc "The Intrinsics module may not be used in module expressions."
  return (scope, ModVar v' loc)
checkModExp (ModImport name loc) = do
  scope <- lookupImport loc name
  return (scope, ModImport name loc)
checkModExp (ModApply f e NoInfo NoInfo loc) = do
  (f', functor) <- lookupFunctor loc f
  (e_scope, e') <- checkModExp e
  (f_scope, psubsts, rsubsts) <- applyFunctor functor loc e_scope
  return (f_scope, ModApply f' e' (Info psubsts) (Info rsubsts) loc)
checkModExp (ModAscript me se NoInfo loc) = do
  (scope, me') <- checkModExp me
  (sigscope, se') <- checkSigExp se
  (scope', _) <- liftEither $ matchScopes scope sigscope loc
  -- See issue #262 for martinsubst justification.
  (scope'', martinsubst) <- newNamesForScope mempty scope'
  return (scope'', ModAscript me' se' (Info martinsubst) loc)

checkStructBind :: StructBindBase NoInfo Name -> TypeM (Scope, StructBindBase Info VName)
checkStructBind (StructBind name e loc) = do
  name' <- checkName Structure name loc
  (scope, e') <- checkModExp e
  return (mempty { envModTable = HM.singleton name $ ModMod scope
                 , envNameMap = HM.singleton (Structure, name) name'
                 },
          StructBind name' e' loc)

checkForDuplicateSpecs :: [SpecBase NoInfo Name] -> TypeM ()
checkForDuplicateSpecs =
  foldM_ (flip f) mempty
  where check namespace name loc known =
          case HM.lookup (namespace, name) known of
            Just loc' ->
              bad $ DupDefinitionError namespace name loc loc'
            _ -> return $ HM.insert (namespace, name) loc known

        f (ValSpec name _ _ loc) =
          check Term name loc

        f (TypeAbbrSpec (TypeBind name _ loc)) =
          check Type name loc

        f (TypeSpec name loc) =
          check Type name loc

        f IncludeSpec{} =
          return


checkFunctorBind :: FunctorBindBase NoInfo Name -> TypeM (Scope, FunctorBindBase Info VName)
checkFunctorBind (FunctorBind name (p, psig_e) maybe_fsig_e body_e loc) = do
  name' <- checkName Structure name loc
  (p_scope, psig_e') <- checkSigExp psig_e
  bindSpaced [(Structure, p)] $ do
    p' <- checkName Structure p loc
    let in_body_scope = mempty { envModTable = HM.singleton p $ ModMod p_scope }
    local (in_body_scope<>) $ do
      (body_scope, body_e') <- checkModExp body_e
      (maybe_fsig_e', scope') <-
        case maybe_fsig_e of
          Nothing ->
            return (Nothing, body_scope)
          Just fsig_e -> do
            (fsig_scope, fsig_e') <- checkSigExp fsig_e
            (scope', _) <- liftEither $ matchScopes body_scope fsig_scope loc
            return (Just fsig_e', scope')
      return (mempty { envModTable =
                         HM.singleton name $
                         ModFunctor $ FunctorF $ apply scope' p_scope
                     , envNameMap =
                         HM.singleton (Structure, name) name'
                     },
              FunctorBind name' (p', psig_e') maybe_fsig_e' body_e' loc)

  where apply body_scope p_sig applyloc a_scope = do
          (_, sig_subst) <- liftEither $ matchScopes a_scope p_sig applyloc

          -- Type substitutions from the argument scope.  Importantly,
          -- we use the scope before it is restricted by the parameter
          -- signature ascription.  There are two important things we
          -- must do: turn abstract types into type abbreviations (if
          -- applicable), and fix references to abstract types.  The
          -- body_scope will refer to names in p_sig, and we'll have
          -- to refer to those in a_scope instead.  sig_subst gives us
          -- exactly the type name mappings, but we'll need some care
          -- for the abbreviations.
          let type_substs = scopeTypeAbbrs a_scope
              typeSubst v
                | Just t <- lookup v type_substs = TypeAbbr t
                | otherwise                      = TypeAbbr $ TypeVar $ typeName v
              type_substs'' = HM.map typeSubst sig_subst

          (body_scope', body_subst) <-
            newNamesForScope sig_subst $
            substituteTypesInScope type_substs'' body_scope
          return (body_scope', sig_subst, body_subst)

checkTypeBind :: TypeBindBase NoInfo Name
              -> TypeM (Scope, TypeBindBase Info VName)
checkTypeBind (TypeBind name td loc) = do
  name' <- checkName Type name loc
  td' <- checkTypeDecl td
  return (mempty { envTypeTable =
                     HM.singleton name' $ TypeAbbr $ unInfo $ expandedType td',
                   envNameMap =
                     HM.singleton (Type, name) name'
                 },
          TypeBind name' td' loc)

checkDecs :: [DecBase NoInfo Name] -> TypeM (Scope, [DecBase Info VName])
checkDecs (StructDec struct:rest) =
  bindSpaced [(Structure, structName struct)] $ do
    (modscope, struct') <- checkStructBind struct
    local (modscope<>) $ do
      (scope, rest') <- checkDecs rest
      return (scope <> modscope, StructDec struct' : rest')

checkDecs (SigDec sig:rest) =
  bindSpaced [(Signature, sigName sig)] $ do
    (sigscope, sig') <- checkSigBind sig
    local (sigscope<>) $ do
      (scope, rest') <- checkDecs rest
      return (scope <> sigscope, SigDec sig' : rest')

checkDecs (FunctorDec func:rest) =
  bindSpaced [(Structure, functorName func)] $ do
    (funcscope, func') <- checkFunctorBind func
    local (funcscope<>) $ do
      (scope, rest') <- checkDecs rest
      return (funcscope <> scope, FunctorDec func' : rest')

checkDecs (TypeDec tdec:rest) =
  bindSpaced [(Type, typeAlias tdec)] $ do
    (tscope, tdec') <- checkTypeBind tdec
    local (tscope<>) $ do
      (scope, rest') <- checkDecs rest
      return (scope <> tscope, TypeDec tdec' : rest')

checkDecs (OpenDec x xs loc:rest) = do
  (x_mod, x') <- checkModExp x
  (xs_mods, xs') <- unzip <$> mapM checkModExp xs
   -- We cannot use mconcat, as mconcat is a right-fold.
  let scope_ext = foldl (flip mappend) x_mod xs_mods
  local (scope_ext<>) $ do
    (scope, rest') <- checkDecs rest
    return (scope <> scope_ext,
            OpenDec x' xs' loc: rest')

checkDecs [] =
  return (mempty, [])

checkDecs decs@(ValDec{}:_) = do
  let (t_and_f_decs, rest) = chompDecs decs
      bound = concatMap lhs t_and_f_decs

  bindSpaced bound $ do
    t_and_f_scope <- buildScopeFromDecs t_and_f_decs
    local (t_and_f_scope<>) $ do
      (scope, decs') <- checkValDecs t_and_f_decs rest
      return (scope <> t_and_f_scope,
              decs')

  where lhs (FunDec dec)  = [(Term, funBindName dec)]
        lhs (ConstDec dec) = [(Term, constBindName dec)]

checkValDecs :: [ValDecBase NoInfo Name]
             -> [DecBase NoInfo Name]
             -> TypeM (Scope, [DecBase Info VName])
checkValDecs [] ds = checkDecs ds
checkValDecs (FunDec fundec:vds) ds = do
  fundec' <- checkFun fundec
  let ext = valDecScope $ FunDec fundec'
  local (ext<>) $ do
    (scope, vds') <- checkValDecs vds ds
    return (scope <> ext, ValDec (FunDec fundec') : vds')
checkValDecs (ConstDec constdec:vds) ds = do
  constdec' <- checkConst constdec
  let ext = valDecScope $ ConstDec constdec'
  local (ext<>) $ do
    (scope, vds') <- checkValDecs vds ds
    return (scope <> ext, ValDec (ConstDec constdec') : vds')

checkConst :: ConstBindBase NoInfo Name -> TypeM ConstBind
checkConst (ConstBind name maybe_t NoInfo e loc) = do
  name' <- checkName Term name loc
  (maybe_t', e') <- case maybe_t of
    Just t  -> do
      (tdecl, tdecl_type) <- checkTypeExp t
      let t_structural = toStructural tdecl_type
      when (anythingUnique t_structural) $
        bad $ UniqueConstType loc name t_structural
      e' <- require [t_structural] =<< checkExp e
      return (Just tdecl, e')
    Nothing -> do
      e' <- checkExp e
      return (Nothing, e')
  let e_t = vacuousShapeAnnotations $ toStructural $ typeOf e'
  return $ ConstBind name' maybe_t' (Info e_t) e' loc
  where anythingUnique (Tuple ts) = any anythingUnique ts
        anythingUnique et         = unique et

checkFun :: FunBindBase NoInfo Name -> TypeM FunBind
checkFun (FunBind entry fname maybe_retdecl NoInfo params body loc) = do
  (fname', params', maybe_retdecl', rettype, body') <-
    checkFunDef (fname, maybe_retdecl, params, body, loc)

  when entry $
    case maybe_retdecl of
      Just retdecl
        | Just problem <-
            find (not . (`HS.member` mconcat (map patNameSet params))) $
            mapMaybe dimDeclName $ arrayDims' retdecl ->
              bad $ EntryPointConstReturnDecl loc fname $ qualName problem
      _ -> return ()

  return $ FunBind entry fname' maybe_retdecl' (Info rettype) params' body' loc

  where dimDeclName (NamedDim name) = Just name
        dimDeclName _               = Nothing

checkFunDef :: (Name, Maybe UncheckedTypeExp, [UncheckedPattern], UncheckedExp, SrcLoc)
            -> TypeM (VName, [Pattern], Maybe (TypeExp VName), StructType, Exp)
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
             -> TypeM Exp
checkFunBody fname body maybe_rettype loc = do
  body' <- checkExp body

  case maybe_rettype of
    Just rettype -> do
      let rettype_structural = toStructural rettype
      unless (toStructural (typeOf body') `subtypeOf` rettype_structural) $
        bad $ ReturnTypeError loc fname rettype_structural $ toStructural $ typeOf body'
    Nothing -> return ()

  return body'

checkExp :: ExpBase NoInfo Name
         -> TypeM Exp

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

  (op', (paramtypes, ftype, closure)) <-
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

checkExp (Var qn NoInfo loc) = do
  (qn'@(QualName _ name'), t) <- checkVar qn loc
  observe $ Ident name' (Info t) loc
  return $ Var qn' (Info t) loc

checkExp (Negate arg loc) = do
  arg' <- require anyNumberType =<< checkExp arg
  return $ Negate arg' loc

checkExp (Apply fname args _ loc) = do
  (args', argflows) <- unzip <$> mapM (\(arg,_) -> checkArg arg) args
  (fname', (paramtypes, ftype, closure)) <-
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
        entry = BoundF (map paramType params', rettype, closure)
        bindF env = env { envVtable = HM.insert name' entry $ envVtable env }
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
  bound_outside <- asks $ HS.fromList . HM.keys . envVtable
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
                  -> TypeM (Exp, Arg)
checkSOACArrayArg e = do
  (e', (t, dflow, argloc)) <- checkArg e
  case peelArray 1 t of
    Nothing -> bad $ TypeError argloc "SOAC argument is not an array"
    Just rt -> return (e', (rt, dflow, argloc))

checkIdent :: IdentBase NoInfo Name -> TypeM Ident
checkIdent (Ident name _ pos) = do
  (QualName _ name', vt) <- checkVar (qualName name) pos
  return $ Ident name' (Info vt) pos

data InferredType = NoneInferred
                  | Inferred Type
                  | Ascribed Type

checkPattern :: PatternBase NoInfo Name -> InferredType
             -> TypeM Pattern
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
checkForDuplicateNames :: [PatternBase NoInfo Name] -> TypeM [(Namespace, Name)]
checkForDuplicateNames = fmap toNames . flip execStateT mempty . mapM_ check
  where check (Id v) = seeing BoundAsVar (identName v) (srclocOf v)
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

checkDimIndex :: DimIndexBase NoInfo Name -> TypeM DimIndex
checkDimIndex (DimFix i) =
  DimFix <$> (require [Prim $ Signed Int32] =<< checkExp i)
checkDimIndex (DimSlice i j s) =
  DimSlice
  <$> maybe (return Nothing) (fmap Just . require [Prim $ Signed Int32] <=< checkExp) i
  <*> maybe (return Nothing) (fmap Just . require [Prim $ Signed Int32] <=< checkExp) j
  <*> maybe (return Nothing) (fmap Just . require [Prim $ Signed Int32] <=< checkExp) s

sequentially :: TypeM a -> (a -> Occurences -> TypeM b) -> TypeM b
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

checkArg :: ExpBase NoInfo Name -> TypeM (Exp, Arg)
checkArg arg = do
  (arg', dflow) <- collectOccurences $ checkExp arg
  return (arg', (typeOf arg', dflow, srclocOf arg'))

checkFuncall :: Maybe (QualName Name) -> SrcLoc
             -> [StructType] -> [Arg]
             -> TypeM ()
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

checkLambda :: LambdaBase NoInfo Name -> [Arg]
            -> TypeM Lambda
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
  (fname', (paramtypes, rt, closure)) <- lookupFunction fname (map argType $ curryargs++args) loc
  let rettype' = fromStruct $ removeShapeAnnotations rt
      paramtypes' = map (fromStruct . removeShapeAnnotations) paramtypes
  case find (unique . snd) $ zip curryargexps paramtypes of
    Just (e, _) -> bad $ CurriedConsumption fname $ srclocOf e
    _           -> return ()

  occur closure
  checkFuncall Nothing loc paramtypes $ curryargs ++ args

  return $ CurryFun fname' curryargexps' (Info (paramtypes', rettype')) loc

checkLambda (BinOpFun op NoInfo NoInfo NoInfo loc) [x_arg,y_arg] = do
  (op', (paramtypes, rt, closure)) <- lookupFunction op (map argType [x_arg,y_arg]) loc
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
                -> TypeM (Exp, QualName VName, Type)
checkCurryBinOp arg_ordering binop x loc y_arg = do
  (x', x_arg) <- checkArg x
  let (first_arg, second_arg) = arg_ordering (x_arg, y_arg)
  (binop', (paramtypes, ret, closure)) <-
    lookupFunction binop [argType first_arg, argType second_arg] loc

  occur closure
  checkFuncall Nothing loc paramtypes [first_arg,second_arg]

  return (x', binop', fromStruct $ removeShapeAnnotations ret)

checkDim :: SrcLoc -> DimDecl Name -> TypeM (DimDecl VName)
checkDim _ AnyDim =
  return AnyDim
checkDim _ (ConstDim k) =
  return $ ConstDim k
checkDim loc (NamedDim name) = do
  (QualName _ name', t) <- checkVar (qualName name) loc
  observe $ Ident name' (Info (Prim (Signed Int32))) loc
  case t of
    Prim (Signed Int32) -> return $ NamedDim name'
    _                   -> bad $ DimensionNotInteger loc name

expandType :: (SrcLoc -> DimDecl Name -> TypeM (DimDecl VName))
           -> TypeExp Name
           -> TypeM (TypeExp VName, StructType)
expandType _ (TEVar name loc) = do
  (name', t) <- lookupType loc name
  return (TEVar name' loc, t)
expandType look (TETuple ts loc) = do
  (ts', ts_s) <- unzip <$> mapM (expandType look) ts
  return (TETuple ts' loc, Tuple ts_s)
expandType look (TEArray t d loc) = do
  (t', st) <- expandType look t
  d' <- look loc d
  return (TEArray t' d' loc, arrayOf st (ShapeDecl [d']) Nonunique)
expandType look (TEUnique t loc) = do
  (t', st) <- expandType look t
  case st of
    Array{} -> return (t', st `setUniqueness` Unique)
    _       -> throwError $ InvalidUniqueness loc $ toStructural st

checkTypeExp :: TypeExp Name -> TypeM (TypeExp VName, StructType)
checkTypeExp = expandType checkDim

checkTypeExpNoDims :: TypeExp Name -> TypeM (TypeExp VName, StructType)
checkTypeExpNoDims = expandType $ \_ _ -> return AnyDim

checkTypeExpBindingDims :: TypeExp Name -> TypeM (TypeExp VName, StructType)
checkTypeExpBindingDims = expandType checkBindingDim
  where checkBindingDim _ AnyDim = return AnyDim
        checkBindingDim _ (ConstDim k) = return $ ConstDim k
        checkBindingDim loc (NamedDim name) = NamedDim <$> checkName Term name loc

checkTypeDecl :: TypeDeclBase NoInfo Name -> TypeM (TypeDeclBase Info VName)
checkTypeDecl (TypeDecl t NoInfo) = do
  (t', st) <- checkTypeExp t
  return $ TypeDecl t' $ Info st

checkBindingTypeDecl :: TypeDeclBase NoInfo Name -> TypeM (TypeDeclBase Info VName)
checkBindingTypeDecl (TypeDecl t NoInfo) = do
  (t', st) <- checkTypeExpBindingDims t
  return $ TypeDecl t' $ Info st

--- Signature matching

-- Return new renamed/abstracted scope, as well as a mapping from
-- names in the signature to names in the new scope.  This is used for
-- functor application.  The first scope is the module scope, and the
-- second the scope it must match.
matchScopes :: Scope -> Scope -> SrcLoc
            -> Either TypeError (Scope, HM.HashMap VName VName)
matchScopes scope sig loc = do
  -- Check that abstract types in 'sig' have an implementation in
  -- 'scope'.  This also gives us a substitution that we use to check
  -- the types of values.
  abs_substs <- fmap HM.fromList $ forM (scopeAbsTypes sig) $ \name ->
    case findBinding envTypeTable Type (baseName name) of
      Just (name', TypeAbs) ->
        return (name, (name', TypeAbbr $ TypeVar $ typeName name'))
      Just (name', TypeAbbr t) ->
        return (name, (name', TypeAbbr t))
      Nothing ->
        missingType $ baseName name

  let abs_names = map fst $ HM.elems abs_substs
      abs_subst_to_type = HM.map snd abs_substs
      abs_subst_to_name = HM.map (TypeAbbr . TypeVar . typeName . fst) abs_substs
      abs_name_substs   = HM.map fst abs_substs

  -- Check that all type abbreviations are correctly defined.
  abbr_substs <- fmap HM.fromList $ forM (scopeTypeAbbrs sig) $ \(name,spec_t) -> do
    let spec_t' = substituteTypes abs_subst_to_type spec_t
    case findBinding envTypeTable Type (baseName name) of
      Just (name', TypeAbbr t)
        | spec_t' == t ->
            return (name, (name', substituteTypes abs_subst_to_type spec_t))
        | otherwise ->
            mismatchedType (baseName name) ([], spec_t) ([], t)
      Just (name', TypeAbs)
        | TypeVar (typeName name') == spec_t ->
            return (name, (name', substituteTypes abs_subst_to_type spec_t))
        | otherwise ->
          Left $ TypeError loc $
          "Type abbreviation " ++ pretty (baseName name) ++ " = " ++ pretty spec_t ++
          " defined as abstract in module."
      Nothing -> missingType $ baseName name

  let abbrs = HM.map TypeAbbr $ HM.fromList $ HM.elems abbr_substs
      abbr_name_substs = HM.map fst abbr_substs

  -- Check that all values are defined correctly, substituting the
  -- types first.
  vals_and_substs <- fmap HM.fromList $ forM (scopeVals sig) $ \(name, (spec_pts, spec_t, _)) -> do
    let spec_pts' = map (substituteTypes abs_subst_to_type) spec_pts
        spec_t'   = substituteTypes abs_subst_to_type spec_t
        impl_pts' = map (substituteTypes abs_subst_to_name) spec_pts
        impl_t'   = substituteTypes abs_subst_to_name spec_t
    case findBinding envVtable Term $ baseName name of
      Just (name', BoundV t)
        | null spec_pts', toStructural t `subtypeOf` toStructural spec_t' ->
            return (name, (name', BoundV $ removeShapeAnnotations $ fromStruct impl_t'))
        | otherwise ->
            mismatchedVal name (spec_pts', spec_t') ([], t)
      Just (name', BoundF (pts, ret, _))
        | and (zipWith subtypeOf (map toStructural pts) (map toStructural spec_pts')),
          toStructural ret `subtypeOf` toStructural spec_t' ->
            return (name, (name', BoundF (impl_pts', impl_t', mempty)))
        | otherwise ->
            mismatchedVal (baseName name) (spec_pts', spec_t') (pts, ret)
      Just (_, UnknownF{}) ->
        Left $ TypeError loc $ "Function " ++ pretty (baseName name) ++
        " missing a return type declaration."
      _ -> missingVal (baseName name)

  let vals = HM.fromList $ HM.elems vals_and_substs
      val_substs = HM.map fst vals_and_substs

      names = HM.filter isInSig $ envNameMap scope
      types = abbrs <> HM.fromList (zip abs_names $ repeat TypeAbs)
      res_scope = Scope { envVtable = vals
                        , envTypeTable = types
                        , envSigTable = mempty
                        , envModTable = mempty
                        , envNameMap = names
                        , envImportTable = mempty
                        }
  return (res_scope,
          abs_name_substs <> abbr_name_substs <> val_substs)
  where missingType name =
          Left $ TypeError loc $
          "Structure does not define a type named " ++ pretty name ++ "."

        missingVal name =
          Left $ TypeError loc $
          "Structure does not define a value named " ++ pretty name ++ "."

        mismatchedType name spec_t scope_t =
          Left $ TypeError loc $ "Type " ++ pretty name ++ " specified as " ++
          ppFunType spec_t ++ " in signature, but " ++ ppFunType scope_t ++ " in structure."

        mismatchedVal name spec_t scope_t =
          Left $ TypeError loc $ "Value " ++ pretty name ++ " specified as type " ++
          ppFunType spec_t ++ " in signature, but has " ++ ppFunType scope_t ++ " in structure."

        findBinding :: (Scope -> HM.HashMap VName v)
                    -> Namespace -> Name
                    -> Maybe (VName, v)
        findBinding table namespace name = do
          name' <- HM.lookup (namespace, name) $ envNameMap scope
          (name',) <$> HM.lookup name' (table scope)

        isInSig x = baseName x `elem` sig_names
          where sig_names = map baseName $ HM.elems $ envNameMap sig

        ppFunType (paramts, ret) =
          intercalate " -> " $ map pretty $ paramts ++ [ret]

substituteTypesInScope :: HM.HashMap VName TypeBinding -> Scope -> Scope
substituteTypesInScope substs scope =
  scope { envVtable    = HM.map subV $ envVtable scope
        , envTypeTable = HM.mapWithKey subT $ envTypeTable scope }
  where subV (BoundV t) =
          BoundV $ fromStruct $ toStructural $
          substituteTypes substs $
          vacuousShapeAnnotations $ toStruct t
        subV (BoundF (ts, t, _)) =
          BoundF (map (substituteTypes substs) ts,
                  substituteTypes substs t,
                  mempty)
        subV b = b

        subT name _
          | Just t <- HM.lookup name substs = t
        subT _ (TypeAbbr t) = TypeAbbr $ substituteTypes substs t
        subT _ TypeAbs      = TypeAbs

substituteTypes :: HM.HashMap VName TypeBinding -> StructType -> StructType
substituteTypes substs (TypeVar v)
  | Just (TypeAbbr t) <-
      HM.lookup (qualLeaf (qualNameFromTypeName v)) substs = t
  | otherwise                                              = TypeVar v
substituteTypes _ (Prim t) = Prim t
substituteTypes substs (Array at) = substituteTypesInArray at
  where substituteTypesInArray (PrimArray t shape u ()) =
          Array $ PrimArray t shape u ()
        substituteTypesInArray (PolyArray v shape u ())
          | Just (TypeAbbr t) <- HM.lookup (qualLeaf (qualNameFromTypeName v)) substs =
              arrayOf t shape u
          | otherwise =
              Array $ PolyArray v shape u ()
        substituteTypesInArray (TupleArray ts shape u) =
          Array $ TupleArray ts' shape u
          where ts' = map (flip typeToTupleArrayElem u .
                            substituteTypes substs .
                            tupleArrayElemToType) ts
substituteTypes substs (Tuple ts) = Tuple $ map (substituteTypes substs) ts

-- New names for everything defined in the scope, with passed-in
-- exceptions.  Removes signatures.
newNamesForScope :: HM.HashMap VName VName -> Scope -> TypeM (Scope, HM.HashMap VName VName)
newNamesForScope except orig_scope = do
  -- Create unique renames for the scope.
  let rename (k, v)
        | Just v' <- HM.lookup v except =
            return ((k,v'), (v,v'))
        | otherwise = do v' <- newName v
                         return ((k,v'), (v,v'))
  (names_list, substs_list) <-
    mapAndUnzipM rename $ HM.toList $ envNameMap orig_scope
  let names = HM.fromList names_list
      substs = HM.fromList substs_list

  -- Now we have to substitute everything else in the scope.
  return ((substituteInScope substs orig_scope) { envNameMap = names },
          substs)
  where substituteInScope :: HM.HashMap VName VName -> Scope -> Scope
        substituteInScope substs scope =
          Scope { envVtable = substituteInMap substs substituteInBinding $
                              envVtable scope
                , envTypeTable = substituteInMap substs substituteInTypeBinding $
                                 envTypeTable scope
                , envSigTable = mempty
                , envModTable = HM.map (substituteInModBinding substs) $
                                envModTable scope
                , envNameMap = mempty
                , envImportTable = mempty
                }

        substituteInMap substs f m =
          let (ks, vs) = unzip $ HM.toList m
          in HM.fromList $
             zip (map (\k -> fromMaybe k $ HM.lookup k substs) ks)
                 (map (f substs) vs)

        substituteInBinding :: HM.HashMap VName VName -> Binding -> Binding
        substituteInBinding substs (UnknownF ts) =
          UnknownF $ map (substituteInType substs) ts
        substituteInBinding _ UnknownV =
          UnknownV
        substituteInBinding substs (BoundV t) =
          BoundV $ fromStruct $ toStructural $
          substituteInType substs $
          vacuousShapeAnnotations $ toStruct t
        substituteInBinding substs (BoundF (pts,t, _)) =
          BoundF (map (substituteInType substs) pts,
                  substituteInType substs t,
                  mempty)
        substituteInBinding _ (OverloadedF f) =
          OverloadedF f
        substituteInBinding _ EqualityF =
          EqualityF
        substituteInBinding _ (WasConsumed wloc) =
          WasConsumed wloc

        substituteInModBinding :: HM.HashMap VName VName -> ModBinding -> ModBinding
        substituteInModBinding substs (ModMod scope) =
          ModMod $ substituteInScope substs scope
        substituteInModBinding substs (ModFunctor f) =
          ModFunctor $ FunctorF $ \loc scope -> do
          -- Nested functor or something?  This should never happen!
          (f_scope, f_sig_subst, f_body_subst) <- applyFunctor f loc scope
          return (substituteInScope substs f_scope,
                  substs <> f_sig_subst, substs <> f_body_subst)

        substituteInTypeBinding substs (TypeAbbr t) =
          TypeAbbr $ substituteInType substs t
        substituteInTypeBinding _ TypeAbs =
          TypeAbs

        substituteInType :: HM.HashMap VName VName -> StructType -> StructType
        substituteInType substs = substituteTypes $
                                  HM.map (TypeAbbr . TypeVar . typeNameFromQualName . qualName) substs

-- | Refine the given type name in the given scope.
refineScope :: SrcLoc -> Scope -> QualName VName -> StructType -> TypeM Scope
refineScope loc scope tname t =
  -- tname must be an abstract type in scope.
  case HM.lookup (qualLeaf tname) $ envTypeTable scope of
    Nothing ->
      bad $ TypeError loc $
      pretty tname ++ " is not a known type in the module type."
    Just TypeAbbr{} ->
      bad $ TypeError loc $
      pretty tname ++ " is not an abstract type in the module type."
    Just TypeAbs ->
      let subst = HM.singleton (qualLeaf tname) $ TypeAbbr t
      in return $ substituteTypesInScope subst scope
