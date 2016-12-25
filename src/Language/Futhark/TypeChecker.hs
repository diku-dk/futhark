{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, TupleSections #-}
-- | The type checker checks whether the program is type-consistent.
-- Whether type annotations are already present is irrelevant, but if
-- they are, the type checker will signal an error if they are wrong.
-- The program does not need to have any particular properties for the
-- type checker to function; in particular it does not need unique
-- names.
module Language.Futhark.TypeChecker
  ( checkProg
  , TypeError
  , Warnings
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

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Prelude

import Language.Futhark
import Futhark.FreshNames hiding (newName)
import qualified Futhark.FreshNames

-- | Information about an error during type checking.  The 'Show'
-- instance for this type produces a human-readable description.
data TypeError =
    TypeError SrcLoc String
  | UnifyError SrcLoc (TypeBase Rank NoInfo ()) SrcLoc (TypeBase Rank NoInfo ())
  | UnexpectedType SrcLoc
    (TypeBase Rank NoInfo ()) [TypeBase Rank NoInfo ()]
  | ReturnTypeError SrcLoc Name (TypeBase Rank NoInfo ()) (TypeBase Rank NoInfo ())
  | DupDefinitionError Name SrcLoc SrcLoc
  | DupPatternError Name SrcLoc SrcLoc
  | InvalidPatternError (PatternBase NoInfo Name)
    (TypeBase Rank NoInfo ()) (Maybe String) SrcLoc
  | UnknownVariableError Namespace (QualName Name) SrcLoc
  | ParameterMismatch (Maybe (QualName Name)) SrcLoc
    (Either Int [TypeBase Rank NoInfo ()]) [TypeBase Rank NoInfo ()]
  | UseAfterConsume Name SrcLoc SrcLoc
  | IndexingError Int Int SrcLoc
  | BadAnnotation SrcLoc String
    (TypeBase Rank NoInfo ()) (TypeBase Rank NoInfo ())
  | BadTupleAnnotation SrcLoc String
    [Maybe (TypeBase Rank NoInfo ())] [TypeBase Rank NoInfo ()]
  | CurriedConsumption (QualName Name) SrcLoc
  | BadLetWithValue SrcLoc
  | ReturnAliased Name Name SrcLoc
  | UniqueReturnAliased Name SrcLoc
  | NotAnArray SrcLoc (ExpBase CompTypeBase Name) (TypeBase Rank NoInfo ())
  | PermutationError SrcLoc [Int] Int
  | DimensionNotInteger SrcLoc Name
  | CyclicalTypeDefinition SrcLoc (QualName Name)
  | UndefinedAlias SrcLoc (QualName Name)
  | DupTypeAlias SrcLoc Name
  | DupSigError SrcLoc (QualName Name)
  | InvalidUniqueness SrcLoc (TypeBase Rank NoInfo ())
  | UndefinedType SrcLoc (QualName Name)
  | InvalidField SrcLoc Type String
  | InvalidEntryPointReturnType SrcLoc Name
  | InvalidEntryPointParamType SrcLoc Name (PatternBase NoInfo Name)
  | UnderscoreUse SrcLoc (QualName Name)
  | ValueIsNotFunction SrcLoc (QualName Name) Type
  | FunctionIsNotValue SrcLoc (QualName Name)
  | UniqueConstType SrcLoc Name (TypeBase Rank NoInfo ())
  | EntryPointConstReturnDecl SrcLoc Name (QualName Name)
  | UndeclaredFunctionReturnType SrcLoc (QualName Name)

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
  show (DupDefinitionError name pos1 pos2) =
    "Duplicate definition of function " ++ nameToString name ++ ".  Defined at " ++
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
    "Unknown " ++ space' ++ " " ++ pretty name ++ " referenced at " ++ locStr pos ++ "."
    where space' = case space of Term -> "variable"
                                 Type -> "type"
                                 Structure -> "structure"
                                 Signature -> "signature"
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
  show (BadAnnotation loc desc expected got) =
    "Annotation of \"" ++ desc ++ "\" type of expression at " ++
    locStr loc ++ " is " ++ pretty expected ++
    ", but derived to be " ++ pretty got ++ "."
  show (BadTupleAnnotation loc desc expected got) =
    "Annotation of \"" ++ desc ++ "\" type of expression at " ++
    locStr loc ++ " is a tuple {" ++
    intercalate ", " (map (maybe "(unspecified)" pretty) expected) ++
    "}, but derived to be " ++ prettyTuple got ++ "."
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
  show (NotAnArray loc _ t) =
    "The expression at " ++ locStr loc ++
    " is expected to be an array, but is " ++ pretty t ++ "."
  show (PermutationError loc perm rank) =
    "The permutation (" ++ intercalate ", " (map show perm) ++
    ") is not valid for array argument of rank " ++ show rank ++ " at " ++
    locStr loc ++ "."
  show (DimensionNotInteger loc name) =
    "Dimension declaration " ++ pretty name ++ " at " ++ locStr loc ++
    " should be an integer."
  show (CyclicalTypeDefinition loc name) =
    "Type alias " ++ pretty name ++ " at " ++ locStr loc ++
    " is cyclically defined."
  show (UndefinedAlias loc name) =
    "Type alias '" ++ pretty name ++ "' referenced at line " ++ locStr loc
    ++ ", but not defined."
  show (DupTypeAlias loc name) =
    "Type alias '" ++ pretty name ++ "' defined twice at line " ++ show loc
  show (DupSigError loc name) =
    "Duplicate definition of type '" ++ pretty name ++ "' at line " ++ locStr loc
  show (InvalidUniqueness loc t) =
    "Attempt to declare unique non-array " ++ pretty t ++ " at " ++ locStr loc ++ "."
  show (UndefinedType loc name) =
    "Unknown type " ++ pretty name ++ " referenced at " ++ locStr loc ++ "."
  show (InvalidField loc t field) =
    "Attempt to access field '" ++ field ++ "' of value of type " ++
    pretty t ++ " at " ++ locStr loc ++ "."
  show (InvalidEntryPointReturnType loc fname) =
    "Entry point '" ++ nameToString fname ++ "' at " ++ locStr loc ++
     " has invalid return type.\n" ++
    "Entry points may not return nested tuples.  Sorry."
  show (InvalidEntryPointParamType loc fname p) =
    "Entry point '" ++ nameToString fname ++ "' parameter '" ++ pretty p ++
    "' at " ++ locStr loc ++ " has has invalid type.\n" ++
    "Entry point parameters may not be tuples.  Sorry."
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

-- | Return type and a list of argument types.
type FunBinding = (StructType, [StructType])

-- | A type binding with resolved names and an expansion.
type TypeBinding = StructType

data Binding = BoundV Type
             | BoundF FunBinding
             | UnknownF
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

type NameMap = HM.HashMap (Namespace, QualName Name) VName

-- | Type checking happens with access to this environment.  The
-- tables will be extended during type-checking as bindings come into
-- scope.
data Scope  = Scope { envVtable :: HM.HashMap VName Binding
                    , envTAtable :: HM.HashMap VName TypeBinding
                    , envNameMap :: NameMap
                    } deriving (Show)

instance Monoid Scope where
  mempty = Scope mempty mempty mempty
  Scope vt1 tt1 nt1 `mappend` Scope vt2 tt2 nt2 =
    Scope (vt1<>vt2) (tt1<>tt2) (nt1<>nt2)

initialScope :: Scope
initialScope = Scope initialVtable mempty builtInMap
  where initialVtable = HM.fromList $ map addBuiltinF $ HM.toList builtInFunctions

        addBuiltinF (name, (t, ts)) =
          (name, BoundF (Prim t, map Prim ts))

        builtInMap :: NameMap
        builtInMap = HM.fromList $ map mapping $ HM.keys builtInFunctions
          where mapping v = ((Term, QualName ([], baseName v)), v)

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

-- | The type checker runs in this monad.  The 'Either' monad is used
-- for error handling.
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

instance Hashable Namespace where
  hashWithSalt salt = hashWithSalt salt . fromEnum

bindSpaced :: [(Namespace, QualName Name)] -> TypeM a -> TypeM a
bindSpaced varnames body = do
  vars' <- mapM newQual varnames
  let mapping = HM.fromList (zip varnames vars')
  bindNameMap mapping body
  where newQual (_, QualName (_, v)) = newID v

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
        set UnknownF          = UnknownF
        set (WasConsumed loc) = WasConsumed loc

onlySelfAliasing :: TypeM a -> TypeM a
onlySelfAliasing = local (\env -> env { envVtable = HM.mapWithKey set $ envVtable env})
  where set k (BoundV t)        = BoundV $ t `addAliases` HS.intersection (HS.singleton k)
        set _ (BoundF f)        = BoundF f
        set _ UnknownF          = UnknownF
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
  let varnames = map ((Term,) . qual . baseName . identName) idents
      substs   = map identName idents
  bindNameMap (HM.fromList (zip varnames substs)) $
    binding idents body
  where qual v = QualName ([], v)

bindingIdents :: [(IdentBase NoInfo Name, Type)] -> ([Ident] -> TypeM a) -> TypeM a
bindingIdents vs m = descend [] vs
  where descend vs' [] = m $ reverse vs'
        descend vs' ((v,t):rest) = bindingIdent v t $ \v' -> descend (v':vs') rest

bindingIdent :: IdentBase NoInfo Name -> Type -> (Ident -> TypeM a)
             -> TypeM a
bindingIdent (Ident v NoInfo vloc) t m =
  bindSpaced [(Term, QualName ([], v))] $ do
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

checkQualName :: Namespace -> QualName Name -> SrcLoc -> TypeM (QualName VName)
checkQualName space qn@(QualName (qs,_)) loc = do
  name' <- maybe (bad $ UnknownVariableError space qn loc) return =<<
           asks (HM.lookup (space, qn) . envNameMap)
  return $ QualName (qs, name')

checkName :: Namespace -> Name -> SrcLoc -> TypeM VName
checkName space name loc = do
  QualName (_, name') <- checkQualName space (QualName ([], name)) loc
  return name'

checkVar :: QualName Name -> SrcLoc -> TypeM (QualName VName, Type)
checkVar qn loc = do
  qn'@(QualName (_, name)) <- checkQualName Term qn loc
  bnd <- asks $ HM.lookup name . envVtable
  case bnd of
    Nothing -> bad $ UnknownVariableError Term qn loc
    Just (BoundV t) | "_" `isPrefixOf` pretty name -> bad $ UnderscoreUse loc qn
                    | otherwise -> return (qn', t)
    Just (BoundF _) -> bad $ FunctionIsNotValue loc qn
    Just UnknownF -> bad $ FunctionIsNotValue loc qn
    Just (WasConsumed wloc) -> bad $ UseAfterConsume (baseName name) loc wloc

lookupFunction :: QualName Name -> SrcLoc -> TypeM (QualName VName, FunBinding)
lookupFunction qn loc = do
  qn'@(QualName (_, name)) <- checkQualName Term qn loc
  bnd <- asks $ HM.lookup name . envVtable
  case bnd of
    Nothing -> bad $ UnknownVariableError Term qn loc
    Just (WasConsumed wloc) -> bad $ UseAfterConsume (baseName name) loc wloc
    Just (BoundV t) -> bad $ ValueIsNotFunction loc qn t
    Just UnknownF -> bad $ UndeclaredFunctionReturnType loc qn
    Just (BoundF f) -> return (qn', f)

lookupType :: SrcLoc -> QualName Name -> TypeM (QualName VName, StructType)
lookupType loc qn = do
  qn'@(QualName (_, name)) <- checkQualName Type qn loc
  (qn',) <$> (maybe explode return =<< asks (HM.lookup name . envTAtable))
  where explode = bad $ UndefinedType loc qn

-- | @t1 `unifyTypes` t2@ attempts to unify @t2@ and @t2@.  If
-- unification cannot happen, 'Nothing' is returned, otherwise a type
-- that combines the aliasing of @t1@ and @t2@ is returned.  The
-- uniqueness of the resulting type will be the least of the
-- uniqueness of @t1@ and @t2@.
unifyTypes :: Monoid (as vn) =>
              TypeBase Rank as vn
           -> TypeBase Rank as vn
           -> Maybe (TypeBase Rank as vn)
unifyTypes (Prim t1) (Prim t2)
  | t1 == t2  = Just $ Prim t1
  | otherwise = Nothing
unifyTypes (Array at1) (Array at2) =
  Array <$> unifyArrayTypes at1 at2
unifyTypes (Tuple ts1) (Tuple ts2)
  | length ts1 == length ts2 =
    Tuple <$> zipWithM unifyTypes ts1 ts2
unifyTypes _ _ = Nothing

unifyArrayTypes :: Monoid (as vn) =>
                   ArrayTypeBase Rank as vn
                -> ArrayTypeBase Rank as vn
                -> Maybe (ArrayTypeBase Rank as vn)
unifyArrayTypes (PrimArray bt1 shape1 u1 als1) (PrimArray bt2 shape2 u2 als2)
  | shapeRank shape1 == shapeRank shape2, bt1 == bt2 =
    Just $ PrimArray bt1 shape1 (u1 <> u2) (als1 <> als2)
unifyArrayTypes (TupleArray et1 shape1 u1) (TupleArray et2 shape2 u2)
  | shapeRank shape1 == shapeRank shape2 =
    TupleArray <$> zipWithM unifyTupleArrayElemTypes et1 et2 <*>
    pure shape1 <*> pure (u1 <> u2)
unifyArrayTypes _ _ =
  Nothing

unifyTupleArrayElemTypes :: Monoid (as vn) =>
                            TupleArrayElemTypeBase Rank as vn
                         -> TupleArrayElemTypeBase Rank as vn
                         -> Maybe (TupleArrayElemTypeBase Rank as vn)
unifyTupleArrayElemTypes (PrimArrayElem bt1 als1 u1) (PrimArrayElem bt2 als2 u2)
  | bt1 == bt2 = Just $ PrimArrayElem bt1 (als1 <> als2) (u1 <> u2)
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

anySignedType :: [TypeBase Rank NoInfo ()]
anySignedType = map (Prim . Signed) [minBound .. maxBound]

anyUnsignedType :: [TypeBase Rank NoInfo ()]
anyUnsignedType = map (Prim . Unsigned) [minBound .. maxBound]

anyIntType :: [TypeBase Rank NoInfo ()]
anyIntType = anySignedType ++ anyUnsignedType

anyFloatType :: [TypeBase Rank NoInfo ()]
anyFloatType = map (Prim . FloatType) [minBound .. maxBound]

anyNumberType :: [TypeBase Rank NoInfo ()]
anyNumberType = anyIntType ++ anyFloatType

-- | @require ts e@ causes a 'TypeError' if @typeOf e@ does not unify
-- with one of the types in @ts@.  Otherwise, simply returns @e@.
-- This function is very useful in 'checkExp'.
require :: [TypeBase Rank NoInfo ()] -> Exp -> TypeM Exp
require ts e
  | any (removeNames (typeOf e) `similarTo`) ts = return e
  | otherwise = bad $ UnexpectedType (srclocOf e)
                      (toStructural $ typeOf e) ts

chompDecs :: [DecBase NoInfo Name]
          -> ([ValDecBase NoInfo Name], [DecBase NoInfo Name])
chompDecs (ValDec dec : xs) = let (valdecs, xs') = chompDecs xs
                              in (dec : valdecs, xs')
chompDecs xs                = ([], xs)

buildScopeFromDecs :: [ValDecBase NoInfo Name]
                   -> TypeM Scope
buildScopeFromDecs [] = ask
buildScopeFromDecs decs = foldM expandV mempty decs
  where
    paramDeclaredType (PatternAscription _ t) =
      Just $ declaredType t
    paramDeclaredType (TuplePattern ps loc) =
      UserTuple <$> mapM paramDeclaredType ps <*> pure loc
    paramDeclaredType _ =
      Nothing

    expandV scope (FunDec (FunBind _ fname (Just ret) _ params _ loc)) = do
      fname' <- checkName Term fname loc
      argtypes <- forM params $ \param ->
        case paramDeclaredType param of
          Just t -> return t
          Nothing -> bad $ TypeError (srclocOf param) $
                     "Missing type information for parameter " ++
                     pretty param
      (_, ret') <- checkUserTypeNoDims ret
      argtypes' <- mapM (fmap snd . checkUserTypeNoDims) argtypes
      return scope { envVtable = HM.insert fname' (BoundF (ret' , argtypes')) $
                                 envVtable scope
                   , envNameMap = HM.insert (Term, QualName ([], fname)) fname' $
                                  envNameMap scope
                   }

    expandV scope (FunDec (FunBind _ fname Nothing _ _ _ loc)) = do
      fname' <- checkName Term fname loc
      return scope { envVtable = HM.insert fname' UnknownF $
                                 envVtable scope
                   , envNameMap = HM.insert (Term, QualName ([], fname)) fname' $
                                  envNameMap scope
                   }

    expandV scope (ConstDec (ConstBind cname (TypeDecl t NoInfo) _ loc)) = do
      cname' <- checkName Term cname loc
      (_, t') <- checkUserTypeNoDims t
      let entry = BoundV $ removeShapeAnnotations $ t' `addAliases` mempty
      return scope { envVtable = HM.insert cname' entry $
                                 envVtable scope
                   , envNameMap = HM.insert (Term, QualName ([], cname)) cname' $
                                  envNameMap scope
                   }

-- | Type check a program containing arbitrary no information,
-- yielding either a type error or a program with complete type
-- information.
checkProg :: UncheckedProg -> Either TypeError (Prog, Warnings, VNameSource)
checkProg prog =
  runTypeM initialScope src $ Prog <$> checkProg' (progDecs prog)
  where src = newNameSource $ succ $ maximum $ map baseTag $
              HM.elems $ envNameMap initialScope

checkProg' :: [DecBase NoInfo Name] -> TypeM [DecBase Info VName]
checkProg' decs = do
  checkForDuplicateDecs decs
  (_, decs') <- checkDecs decs
  return decs'

checkForDuplicateDecs :: [DecBase NoInfo Name] -> TypeM ()
checkForDuplicateDecs =
  foldM_ f mempty
  where f known (ValDec (FunDec (FunBind _ name _ _ _ _ loc))) =
          case HM.lookup (name, Term) known of
            Just loc' ->
              bad $ DupDefinitionError name loc loc'
            _ -> return $ HM.insert (name, Term) loc known

        f known (TypeDec (TypeBind name _ loc)) =
          case HM.lookup (name, Type) known of
            Just loc' ->
              bad $ DupDefinitionError name loc loc'
            _ -> return $ HM.insert (name, Type) loc known

        f known (ValDec (ConstDec (ConstBind name _ _ loc))) =
          case HM.lookup (name, Term) known of
            Just loc' ->
              bad $ DupDefinitionError name loc loc'
            _ -> return $ HM.insert (name, Term) loc known

        f known (SigDec (SigBind name _ loc)) =
          case HM.lookup (name, Signature) known of
            Just loc' ->
              bad $ DupDefinitionError name loc loc'
            _ -> return $ HM.insert (name, Signature) loc known

        f known (StructDec (StructBind name _ _ loc)) =
          case HM.lookup (name, Structure) known of
            Just loc' ->
              bad $ DupDefinitionError name loc loc'
            _ -> return $ HM.insert (name, Structure) loc known


checkMod :: StructBindBase NoInfo Name -> TypeM (Scope, StructBindBase Info VName)
checkMod (StructBind name sig decs loc) = do
  name' <- checkName Structure name loc
  sig' <- maybe (return Nothing) (\v -> Just <$> checkName Signature v loc) sig
  checkForDuplicateDecs decs
  (scope, decs') <- checkDecs decs
  return (qualScope scope, StructBind name' sig' decs' loc)
  where
    -- | Prefix the structure name to all names in the scope.
    qualScope scope = scope { envNameMap = qualNameMap $ envNameMap scope }
    qualNameMap = HM.fromList . map qual . HM.toList
    qual ((space, QualName (qs, v)), x) =
      ((space, QualName (name : qs, v)), x)


checkTypeBind :: TypeBindBase NoInfo Name
              -> TypeM (Scope, TypeBindBase Info VName)
checkTypeBind (TypeBind name td loc) = do
  name' <- checkName Type name loc
  td' <- checkTypeDecl td
  return (mempty { envTAtable =
                     HM.singleton name' $ unInfo $  expandedType td',
                   envNameMap =
                     HM.singleton (Type, QualName ([],name)) name'
                 },
          TypeBind name' td' loc)

checkDecs :: [DecBase NoInfo Name] -> TypeM (Scope, [DecBase Info VName])
checkDecs (StructDec modd:rest) =
  bindSpaced [(Structure, QualName ([],structName modd))] $ do
    (modscope, modd') <- checkMod modd
    local (modscope<>) $ do
      (scope, rest') <- checkDecs rest
      return (modscope <> scope, StructDec modd' : rest' )
checkDecs (SigDec _:rest) = checkDecs rest

checkDecs (TypeDec tdec:rest) =
  bindSpaced [(Type, QualName ([],typeAlias tdec))] $ do
    (tscope, tdec') <- checkTypeBind tdec
    local (tscope<>) $ do
      (scope, rest') <- checkDecs rest
      return (scope<>tscope, TypeDec tdec' : rest')

checkDecs [] =
  return (mempty, [])

checkDecs decs = do
  let (t_and_f_decs, rest) = chompDecs decs
      bound = concatMap lhs t_and_f_decs

  bindSpaced bound $ do
    t_and_f_scope <- buildScopeFromDecs t_and_f_decs
    local (t_and_f_scope<>) $ do
      t_and_f_decs' <- checkValDecs t_and_f_decs
      (scope, rest') <- checkDecs rest
      return (scope<>t_and_f_scope , map ValDec t_and_f_decs' ++ rest')

  where lhs (FunDec dec)  = [(Term, QualName ([], funBindName dec))]
        lhs (ConstDec dec) = [(Term, QualName ([], constBindName dec))]

checkValDecs :: [ValDecBase NoInfo Name] -> TypeM [ValDecBase Info VName]
checkValDecs = mapM checkValDec
  where checkValDec (FunDec fundef) = FunDec <$> checkFun fundef
        checkValDec (ConstDec constdec) = ConstDec <$> checkConst constdec

checkConst :: ConstBindBase NoInfo Name -> TypeM ConstBind
checkConst (ConstBind name t e loc) = do
  name' <- checkName Term name loc
  t' <- checkTypeDecl t
  let expanded_type = unInfo $ expandedType t'
  when (anythingUnique expanded_type) $
    bad $ UniqueConstType loc name $ toStructural expanded_type
  e' <- require [toStructural expanded_type] =<< checkExp e
  return $ ConstBind name' t' e' loc
  where anythingUnique (Tuple ts) = any anythingUnique ts
        anythingUnique et         = unique et

checkFun :: FunBindBase NoInfo Name -> TypeM FunBind
checkFun (FunBind entry fname maybe_retdecl NoInfo params body loc) = do
  fname' <- checkName Term fname loc
  bindingPatterns (zip params $ repeat NoneInferred) $ \params' -> do
    maybe_retdecl' <- case maybe_retdecl of
                        Just rettype -> Just <$> checkUserType rettype
                        Nothing      -> return Nothing

    body' <- checkFunBody fname body (snd <$> maybe_retdecl') loc
    (maybe_retdecl'', rettype) <- case maybe_retdecl' of
      Just (retdecl', retdecl_type) -> do
        let rettype_structural = toStructural retdecl_type
        checkReturnAlias rettype_structural params' $ typeOf body'
        return (Just retdecl', retdecl_type)
      Nothing -> return (Nothing, vacuousShapeAnnotations $ toStruct $ typeOf body')

    when entry $ do
      unless (okEntryPointType rettype) $
        bad $ InvalidEntryPointReturnType loc fname

      case maybe_retdecl of
        Just retdecl
          | Just problem <-
              find (not . (`HS.member` mconcat (map patNameSet params))) $
              mapMaybe dimDeclName $ arrayDims' retdecl ->
                bad $ EntryPointConstReturnDecl loc fname (QualName ([], problem))
        _ -> return ()

      forM_ (zip params' params) $ \(param, orig_param) ->
        unless (okEntryPointType (patternType param)) $
          bad $ InvalidEntryPointParamType (srclocOf param) fname orig_param

    return $ FunBind entry fname' maybe_retdecl'' (Info rettype) params' body' loc

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

        okEntryPointType (Tuple ts) = all okEntryPointParamType ts
        okEntryPointType t = okEntryPointParamType t

        okEntryPointParamType Tuple{} = False
        okEntryPointParamType (Prim _) = True
        okEntryPointParamType (Array TupleArray{}) = False
        okEntryPointParamType (Array _) = True

        dimDeclName (NamedDim name) = Just name
        dimDeclName _               = Nothing

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

checkExp (BinOp op e1 e2 NoInfo pos) = checkBinOp op e1 e2 pos

checkExp (UnOp Not e NoInfo pos) = do
  e' <- require [Prim Bool] =<< checkExp e
  return $ UnOp Not e' (Info $ Prim Bool) pos

checkExp (UnOp Complement e NoInfo loc) = do
  e' <- require anyIntType =<< checkExp e
  return $ UnOp Complement e' (Info $ typeOf e') loc

checkExp (UnOp Negate e NoInfo loc) = do
  e' <- require anyNumberType =<< checkExp e
  return $ UnOp Negate e' (Info $ typeOf e') loc

checkExp (UnOp Abs e NoInfo loc) = do
  e' <- require anyNumberType =<< checkExp e
  return $ UnOp Abs e' (Info $ typeOf e') loc

checkExp (UnOp Signum e NoInfo loc) = do
  e' <- require anyIntType =<< checkExp e
  return $ UnOp Signum e' (Info $ typeOf e') loc

checkExp (UnOp (ToFloat t) e NoInfo loc) = do
  e' <- require (anyNumberType <> [Prim Bool]) =<< checkExp e
  return $ UnOp (ToFloat t) e' (Info (Prim (FloatType t))) loc

checkExp (UnOp (ToSigned t) e NoInfo loc) = do
  e' <- require (anyNumberType <> [Prim Bool]) =<< checkExp e
  return $ UnOp (ToSigned t) e' (Info (Prim (Signed t))) loc

checkExp (UnOp (ToUnsigned t) e NoInfo loc) = do
  e' <- require (anyNumberType <> [Prim Bool]) =<< checkExp e
  return $ UnOp (ToUnsigned t) e' (Info (Prim (Unsigned t))) loc

checkExp (UnOp (TupleProject i) e NoInfo loc) = do
  e' <- checkExp e
  case typeOf e' of
    Tuple ts | t:_ <- drop i ts -> return $ UnOp (TupleProject i) e' (Info t) loc
    _ -> bad $ InvalidField loc (typeOf e') (show i)

checkExp (If e1 e2 e3 _ pos) =
  sequentially (require [Prim Bool] =<< checkExp e1) $ \e1' _ -> do
  ((e2', e3'), dflow) <- tapOccurences $ checkExp e2 `alternative` checkExp e3
  brancht <- unifyExpTypes e2' e3'
  let t' = addAliases brancht (`HS.difference` allConsumed dflow)
  return $ If e1' e2' e3' (Info t') pos

checkExp (Var qn NoInfo loc) = do
  (qn'@(QualName (_,name')), t) <- checkVar qn loc
  observe $ Ident name' (Info t) loc
  return $ Var qn' (Info t) loc

checkExp (Apply fname args _ loc) = do
  (fname', (ftype, paramtypes)) <- lookupFunction fname loc
  (args', argflows) <- unzip <$> mapM (\(arg,_) -> (checkArg arg)) args

  let rettype' = returnType (removeShapeAnnotations ftype)
                 (map diet paramtypes) (map typeOf args')

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
    hasShapeDecl' (Tuple ts) = any hasShapeDecl' ts
    hasShapeDecl' (Array at) = arrayElemHasShapeDecl at

    arrayElemHasShapeDecl (PrimArray _ shape _ _) =
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
          body' <- consuming src' $ binding [dest'] $ checkExp body
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
    Tuple ts | all ((`elem` anyIntType) . removeNames) ts -> return ()
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

  let fakearg = (typeOf arr' `setAliases` HS.empty, mempty, srclocOf pos)
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
            accarg = (lambdaReturnType lam' `setAliases` mempty, mempty, srclocOf lam')

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
        occur $ mergeflow `seqOccurences` freeflow `seqOccurences` merge_consume
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
  (QualName (_, name'), vt) <- checkVar (QualName ([],name)) pos
  return $ Ident name' (Info vt) pos

data InferredType = NoneInferred
                  | Inferred Type
                  | Ascribed Type

checkPattern :: PatternBase NoInfo Name -> InferredType
             -> TypeM Pattern
checkPattern (Id (Ident name NoInfo loc)) (Inferred t) = do
  name' <- checkName Term name loc
  let t' = typeOf $ Var (QualName ([],name')) (Info t) loc
  return $ Id $ Ident name' (Info $ t' `setUniqueness` Nonunique) loc
checkPattern (Id (Ident name NoInfo loc)) (Ascribed t) = do
  name' <- checkName Term name loc
  let t' = typeOf $ Var (QualName ([],name')) (Info t) loc
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
checkForDuplicateNames :: [PatternBase NoInfo Name] -> TypeM [(Namespace, QualName Name)]
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
        pairUp (name, _) = (Term, QualName ([], name))


data Bindage = BoundAsDim | BoundAsVar

checkBinOp :: BinOp -> ExpBase NoInfo Name -> ExpBase NoInfo Name -> SrcLoc
           -> TypeM Exp
checkBinOp Plus e1 e2 pos = checkPolyBinOp Plus anyNumberType e1 e2 pos
checkBinOp Minus e1 e2 pos = checkPolyBinOp Minus anyNumberType e1 e2 pos
checkBinOp Pow e1 e2 pos = checkPolyBinOp Pow anyNumberType e1 e2 pos
checkBinOp Times e1 e2 pos = checkPolyBinOp Times anyNumberType e1 e2 pos
checkBinOp Divide e1 e2 pos = checkPolyBinOp Divide anyNumberType e1 e2 pos
checkBinOp Mod e1 e2 pos = checkPolyBinOp Mod anyIntType e1 e2 pos
checkBinOp Quot e1 e2 pos = checkPolyBinOp Quot anyIntType e1 e2 pos
checkBinOp Rem e1 e2 pos = checkPolyBinOp Rem anyIntType e1 e2 pos
checkBinOp ShiftR e1 e2 pos = checkPolyBinOp ShiftR anyIntType e1 e2 pos
checkBinOp ZShiftR e1 e2 pos = checkPolyBinOp ZShiftR anyIntType e1 e2 pos
checkBinOp ShiftL e1 e2 pos = checkPolyBinOp ShiftL anyIntType e1 e2 pos
checkBinOp Band e1 e2 pos = checkPolyBinOp Band anyIntType e1 e2 pos
checkBinOp Xor e1 e2 pos = checkPolyBinOp Xor anyIntType e1 e2 pos
checkBinOp Bor e1 e2 pos = checkPolyBinOp Bor anyIntType e1 e2 pos
checkBinOp LogAnd e1 e2 pos = checkPolyBinOp LogAnd [Prim Bool] e1 e2 pos
checkBinOp LogOr e1 e2 pos = checkPolyBinOp LogOr [Prim Bool] e1 e2 pos
checkBinOp Equal e1 e2 pos = checkEqualOp Equal e1 e2 pos
checkBinOp NotEqual e1 e2 pos = checkEqualOp NotEqual e1 e2 pos
checkBinOp Less e1 e2 pos = checkRelOp Less anyNumberType e1 e2 pos
checkBinOp Leq e1 e2 pos = checkRelOp Leq anyNumberType e1 e2 pos
checkBinOp Greater e1 e2 pos = checkRelOp Greater anyNumberType e1 e2 pos
checkBinOp Geq e1 e2 pos = checkRelOp Geq anyNumberType e1 e2 pos

checkRelOp :: BinOp -> [TypeBase Rank NoInfo ()]
           -> ExpBase NoInfo Name -> ExpBase NoInfo Name -> SrcLoc
           -> TypeM Exp
checkRelOp op tl e1 e2 pos = do
  e1' <- require tl =<< checkExp e1
  e2' <- require tl =<< checkExp e2
  _ <- unifyExpTypes e1' e2'
  return $ BinOp op e1' e2' (Info $ Prim Bool) pos

checkEqualOp :: BinOp
             -> ExpBase NoInfo Name -> ExpBase NoInfo Name -> SrcLoc
             -> TypeM Exp
checkEqualOp op e1 e2 pos = do
  e1' <- checkExp e1
  e2' <- checkExp e2
  _ <- unifyExpTypes e1' e2'
  return $ BinOp op e1' e2' (Info $ Prim Bool) pos

checkPolyBinOp :: BinOp -> [TypeBase Rank NoInfo ()]
               -> ExpBase NoInfo Name -> ExpBase NoInfo Name -> SrcLoc
               -> TypeM Exp
checkPolyBinOp op tl e1 e2 pos = do
  e1' <- require tl =<< checkExp e1
  e2' <- require tl =<< checkExp e2
  t' <- unifyExpTypes e1' e2'
  return $ BinOp op e1' e2' (Info t') pos

checkDimIndex :: DimIndexBase NoInfo Name -> TypeM DimIndex
checkDimIndex (DimFix i) =
  DimFix <$> (require [Prim $ Signed Int32] =<< checkExp i)
checkDimIndex (DimSlice i j) =
  DimSlice
  <$> maybe (return Nothing) (fmap Just . require [Prim $ Signed Int32] <=< checkExp) i
  <*> maybe (return Nothing) (fmap Just . require [Prim $ Signed Int32] <=< checkExp) j

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

argOccurences :: Arg -> Occurences
argOccurences (_, occs, _) = occs

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
      let params_with_ts = zip params $ map (Inferred . (`setAliases` mempty) . argType) args
      (maybe_ret', params', body') <- noUnique $ bindingPatterns params_with_ts $ \params' -> do
        maybe_ret' <- maybe (pure Nothing) (fmap Just . checkTypeDecl) maybe_ret
        body' <- checkFunBody (nameFromString "<anonymous>") body
                 (unInfo . expandedType <$> maybe_ret') loc
        return (maybe_ret', params', body')
      checkFuncall Nothing loc (map patternStructType params') args
      let ret' = case maybe_ret' of
                   Nothing -> flip setAliases NoInfo $ vacuousShapeAnnotations $ typeOf body'
                   Just (TypeDecl _ (Info ret)) -> ret
      return $ AnonymFun params' body' maybe_ret' (Info ret') loc
  | otherwise = bad $ TypeError loc $ "Anonymous function defined with " ++ show (length params) ++ " parameters, but expected to take " ++ show (length args) ++ " arguments."

checkLambda (CurryFun fname curryargexps _ loc) args = do
  (curryargexps', curryargs) <- unzip <$> mapM checkArg curryargexps
  (fname', (rt, paramtypes)) <- lookupFunction fname loc
  let rettype' = fromStruct $ removeShapeAnnotations rt
  case find (unique . snd) $ zip curryargexps paramtypes of
    Just (e, _) -> bad $ CurriedConsumption fname $ srclocOf e
    _           -> return ()
  checkFuncall Nothing loc paramtypes $ curryargs ++ args
  return $ CurryFun fname' curryargexps' (Info rettype') loc

checkLambda (UnOpFun unop NoInfo NoInfo loc) [arg] = do
  occur $ argOccurences arg
  let xident = Ident (nameFromString "x") NoInfo loc
  bindingIdents [(xident, argType arg)] $ \_ -> do
    e <- checkExp $ UnOp unop (Var (QualName ([],identName xident)) NoInfo loc) NoInfo loc
    return $ UnOpFun unop (Info (argType arg)) (Info (typeOf e)) loc

checkLambda (UnOpFun unop NoInfo NoInfo loc) args =
  bad $ ParameterMismatch (Just $ nameToQualName $ nameFromString $ pretty unop) loc (Left 1) $
  map (toStructural . argType) args

checkLambda (BinOpFun op NoInfo NoInfo NoInfo loc) args =
  checkPolyLambdaOp op [] args loc

checkLambda (CurryBinOpLeft binop x _ _ loc) [arg] =
  checkCurryBinOp CurryBinOpLeft binop x loc arg

checkLambda (CurryBinOpLeft binop _ _ _ loc) args =
  bad $ ParameterMismatch (Just $ nameToQualName $ nameFromString $ pretty binop) loc (Left 1) $
  map (toStructural . argType) args

checkLambda (CurryBinOpRight binop x _ _ loc) [arg] =
  checkCurryBinOp CurryBinOpRight binop x loc arg

checkLambda (CurryBinOpRight binop _ _ _ loc) args =
  bad $ ParameterMismatch (Just $ nameToQualName $ nameFromString $ pretty binop) loc (Left 1) $
  map (toStructural . argType) args

checkCurryBinOp :: (BinOp -> Exp -> Info Type -> Info (CompTypeBase VName) -> SrcLoc -> b)
                -> BinOp -> ExpBase NoInfo Name -> SrcLoc -> Arg -> TypeM b
checkCurryBinOp f binop x loc arg = do
  x' <- checkExp x
  let y_ident = Ident (nameFromString "y") NoInfo loc
      x_ident = Ident (nameFromString "x") NoInfo loc
  occur $ argOccurences arg
  bindingIdents [(y_ident, argType arg), (x_ident, typeOf x')] $ \_ -> do
    let y_var = Var (QualName ([], identName y_ident)) NoInfo loc
        x_var = Var (QualName ([], identName x_ident)) NoInfo loc
    e <- checkExp (BinOp binop y_var x_var NoInfo loc)
    return $ f binop x' (Info $ argType arg) (Info $ typeOf e) loc

checkPolyLambdaOp :: BinOp -> [ExpBase NoInfo Name] -> [Arg]
                  -> SrcLoc -> TypeM Lambda
checkPolyLambdaOp op curryargexps args pos = do
  curryargexpts <- map typeOf <$> mapM checkExp curryargexps
  let argts = [ argt | (argt, _, _) <- args ]
  tp <- case curryargexpts ++ argts of
          [t1, t2] | t1 == t2 -> return t1
          l -> bad $ ParameterMismatch (Just fname) pos (Left 2) $ map toStructural l
  let xident = Ident (nameFromString "x") NoInfo pos
      yident = Ident (nameFromString "y") NoInfo pos
  (x,y,params) <- case curryargexps of
                    [] -> return (Var (QualName ([], identName xident)) NoInfo pos,
                                  Var (QualName ([], identName yident)) NoInfo pos,
                                  [(xident, tp), (yident, tp)])
                    [e] -> return (e,
                                   Var (QualName ([], identName yident)) NoInfo pos,
                                   [(yident, tp)])
                    (e1:e2:_) -> return (e1, e2, [])
  rettype <- typeOf <$> bindingIdents params (\_ -> checkBinOp op x y pos)
  mapM_ (occur . argOccurences) args
  return $ BinOpFun op (Info tp) (Info tp) (Info rettype) pos
  where fname = nameToQualName $ nameFromString $ pretty op

checkDim :: SrcLoc -> DimDecl Name -> TypeM (DimDecl VName)
checkDim _ AnyDim =
  return AnyDim
checkDim _ (ConstDim k) =
  return $ ConstDim k
checkDim loc (NamedDim name) = do
  (QualName (_, name'), t) <- checkVar (QualName ([],name)) loc
  observe $ Ident name' (Info (Prim (Signed Int32))) loc
  case t of
    Prim (Signed Int32) -> return $ NamedDim name'
    _                   -> bad $ DimensionNotInteger loc name

expandType :: (SrcLoc -> DimDecl Name -> TypeM (DimDecl VName))
           -> UserType Name
           -> TypeM (UserType VName, StructType)
expandType _ (UserTypeAlias name loc) = do
  (name', t) <- lookupType loc name
  return (UserTypeAlias name' loc, t)
expandType _ (UserPrim prim loc) =
  return (UserPrim prim loc, Prim prim)
expandType look (UserTuple ts loc) = do
  (ts', ts_s) <- unzip <$> mapM (expandType look) ts
  return (UserTuple ts' loc, Tuple ts_s)
expandType look (UserArray t d loc) = do
  (t', st) <- expandType look t
  d' <- look loc d
  return (UserArray t' d' loc, arrayOf st (ShapeDecl [d']) Nonunique)
expandType look (UserUnique t loc) = do
  (t', st) <- expandType look t
  case st of
    Array{} -> return (t', st `setUniqueness` Unique)
    _       -> throwError $ InvalidUniqueness loc $ toStructural st

checkUserType :: UserType Name -> TypeM (UserType VName, StructType)
checkUserType = expandType checkDim

checkUserTypeNoDims :: UserType Name -> TypeM (UserType VName, StructType)
checkUserTypeNoDims = expandType $ \_ _ -> return AnyDim

checkUserTypeBindingDims :: UserType Name -> TypeM (UserType VName, StructType)
checkUserTypeBindingDims = expandType checkBindingDim
  where checkBindingDim _ AnyDim = return AnyDim
        checkBindingDim _ (ConstDim k) = return $ ConstDim k
        checkBindingDim loc (NamedDim name) = NamedDim <$> checkName Term name loc

checkTypeDecl :: TypeDeclBase NoInfo Name -> TypeM (TypeDeclBase Info VName)
checkTypeDecl (TypeDecl t NoInfo) = do
  (t', st) <- checkUserType t
  return $ TypeDecl t' $ Info st

checkBindingTypeDecl :: TypeDeclBase NoInfo Name -> TypeM (TypeDeclBase Info VName)
checkBindingTypeDecl (TypeDecl t NoInfo) = do
  (t', st) <- checkUserTypeBindingDims t
  return $ TypeDecl t' $ Info st
