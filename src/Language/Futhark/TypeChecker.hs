{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, ScopedTypeVariables #-}
-- | The type checker checks whether the program is type-consistent.
-- Whether type annotations are already present is irrelevant, but if
-- they are, the type checker will signal an error if they are wrong.
-- The program does not need to have any particular properties for the
-- type checker to function; in particular it does not need unique
-- names.
module Language.Futhark.TypeChecker
  ( checkProg
  , TypeError
  )
  where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
import Data.Array
import Data.List
import Data.Loc
import Data.Maybe
import Data.Either

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Prelude

import Language.Futhark
import Language.Futhark.Renamer
  (tagProg, untagPattern)
import Futhark.FreshNames hiding (newName)
import qualified Futhark.FreshNames

-- | Information about an error during type checking.  The 'Show'
-- instance for this type produces a human-readable description.
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
  | UnknownVariableError Name SrcLoc
  | UnknownFunctionError QualName SrcLoc
  | ParameterMismatch (Maybe QualName) SrcLoc
    (Either Int [TypeBase Rank NoInfo ()]) [TypeBase Rank NoInfo ()]
  | UseAfterConsume Name SrcLoc SrcLoc
  | IndexingError Int Int SrcLoc
  | BadAnnotation SrcLoc String
    (TypeBase Rank NoInfo ()) (TypeBase Rank NoInfo ())
  | BadTupleAnnotation SrcLoc String
    [Maybe (TypeBase Rank NoInfo ())] [TypeBase Rank NoInfo ()]
  | CurriedConsumption QualName SrcLoc
  | BadLetWithValue SrcLoc
  | ReturnAliased Name Name SrcLoc
  | UniqueReturnAliased Name SrcLoc
  | NotAnArray SrcLoc (ExpBase CompTypeBase Name) (TypeBase Rank NoInfo ())
  | PermutationError SrcLoc [Int] Int (Maybe Name)
  | DimensionNotInteger SrcLoc Name
  | CyclicalTypeDefinition SrcLoc Name
  | UndefinedAlias SrcLoc Name
  | DupTypeAlias SrcLoc Name
  | DupSigError SrcLoc Name
  | InvalidUniqueness SrcLoc (TypeBase Rank NoInfo ())
  | UndefinedQualName SrcLoc QualName
  | InvalidField SrcLoc Type String
  | InvalidEntryPointReturnType SrcLoc Name
  | InvalidEntryPointParamType SrcLoc Name (PatternBase NoInfo Name)

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
  show (UnknownVariableError name pos) =
    "Unknown variable " ++ pretty name ++ " referenced at " ++ locStr pos ++ "."
  show (UnknownFunctionError fname pos) =
    "Unknown function " ++ longnameToString fname ++ " called at " ++ locStr pos ++ "."
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
          fname' = maybe "anonymous function" (("function "++) . longnameToString) fname
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
    "Function " ++ longnameToString fname ++
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
  show (PermutationError loc perm rank name) =
    "The permutation (" ++ intercalate ", " (map show perm) ++
    ") is not valid for array " ++ name' ++ "of rank " ++ show rank ++ " at " ++
    locStr loc ++ "."
    where name' = maybe "" ((++" ") . pretty) name
  show (DimensionNotInteger loc name) =
    "Dimension declaration " ++ pretty name ++ " at " ++ locStr loc ++
    " should be an integer."
  show (CyclicalTypeDefinition loc name) =
    "Type alias " ++ pretty name ++ " at " ++ locStr loc ++
    " is cyclically defined."
  show (UndefinedAlias loc name) =
    "Type alias '" ++ nameToString name ++ "' referenced at line " ++ locStr loc
    ++ ", but not defined."
  show (DupTypeAlias loc name) =
    "Type alias '" ++ nameToString name ++ "' defined twice at line " ++ show loc
  show (DupSigError loc name) =
    "Duplicate definition of type '" ++ nameToString name ++ "' at line " ++ locStr loc
  show (InvalidUniqueness loc t) =
    "Attempt to declare unique non-array " ++ pretty t ++ " at " ++ locStr loc ++ "."
  show (UndefinedQualName loc longname) =
    "Attempt to use undefined " ++ show longname ++ " at " ++ locStr loc ++ "."
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

-- | A tuple of a return type and a list of argument types.
type FunBinding = (QualName, StructTypeBase VName, [StructTypeBase VName])
type TypeBinding = TypeBase ShapeDecl NoInfo VName

data Binding = Bound Type
             | WasConsumed SrcLoc

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

allConsumed :: Occurences -> Names VName
allConsumed = HS.unions . map consumed

seqOccurences :: Occurences -> Occurences -> Occurences
seqOccurences occurs1 occurs2 =
  filter (not . nullOccurence) $ map filt occurs1 ++ occurs2
  where filt occ =
          occ { observed = observed occ `HS.difference` postcons }
        postcons = allConsumed occurs2

altOccurences :: Occurences -> Occurences -> Occurences
altOccurences occurs1 occurs2 =
  filter (not . nullOccurence) $ map filt occurs1 ++ occurs2
  where filt occ =
          occ { consumed = consumed occ `HS.difference` postcons
              , observed = observed occ `HS.difference` postcons }
        postcons = allConsumed occurs2

-- | A pair of a variable table and a function table.  Type checking
-- happens with access to this environment.  The function table is
-- only initialised at the very beginning, but the variable table will
-- be extended during type-checking when let-expressions are
-- encountered.
data Scope  = Scope { envVtable :: HM.HashMap VName Binding
                    , envFtable :: HM.HashMap Name FunBinding
                    , envTAtable :: HM.HashMap Name TypeBinding
                    , envModTable :: HM.HashMap Name Scope
                    , envBreadcrumb :: QualName
                    }

initialScope :: Scope
initialScope = Scope  HM.empty
                      initialFtable
                      HM.empty
                      HM.empty
                      ([] , nameFromString "")


-- | The type checker runs in this monad.  The 'Either' monad is used
-- for error handling.
newtype TypeM a = TypeM (RWST
                         Scope       -- Reader
                         Occurences  -- Writer
                         VNameSource -- State
                         (Except TypeError) -- Inner monad
                         a)
  deriving (Monad, Functor, Applicative,
            MonadReader Scope,
            MonadWriter Occurences,
            MonadState VNameSource,
            MonadError TypeError)

runTypeM :: Scope -> VNameSource -> TypeM a
         -> Either TypeError (a, VNameSource)
runTypeM env src (TypeM m) = do
  (x, src', _) <- runExcept $ runRWST m env src
  return (x, src')

bad :: TypeError -> TypeM a
bad = throwError

newName :: VName -> TypeM VName
newName s = do src <- get
               let (s', src') = Futhark.FreshNames.newName src s
               put src'
               return s'

newID :: Name -> TypeM VName
newID s = newName $ ID (s, 0)

newIDFromString :: String -> TypeM VName
newIDFromString = newID . nameFromString

newIdent :: String -> Type -> SrcLoc -> TypeM Ident
newIdent s t loc = do
  s' <- newID $ nameFromString s
  return $ Ident s' (Info t) loc

liftEither :: Either TypeError a -> TypeM a
liftEither = either bad return

occur :: Occurences -> TypeM ()
occur = tell

-- | Proclaim that we have made read-only use of the given variable.
-- No-op unless the variable is array-typed.
observe :: Ident -> TypeM ()
observe (Ident nm (Info t) loc)
  | primType t = return ()
  | otherwise   = let als = nm `HS.insert` aliases t
                  in occur [observation als loc]

-- | Proclaim that we have written to the given variable.
consume :: SrcLoc -> Names VName -> TypeM ()
consume loc als = occur [consumption als loc]

-- | Proclaim that we have written to the given variable, and mark
-- accesses to it and all of its aliases as invalid inside the given
-- computation.
consuming :: Ident -> TypeM a -> TypeM a
consuming (Ident name (Info t) loc) m = do
  consume loc $ aliases t
  local consume' m
  where consume' env =
          env { envVtable = HM.insert name (WasConsumed loc) $ envVtable env }

collectOccurences :: TypeM a -> TypeM (a, Occurences)
collectOccurences m = pass $ do
  (x, dataflow) <- listen m
  return ((x, dataflow), const mempty)

maybeCheckOccurences :: Occurences -> TypeM ()
maybeCheckOccurences = liftEither . checkOccurences

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
noUnique = local (\env -> env { envVtable = HM.map f $ envVtable env})
  where f (Bound t)         = Bound $ t `setUniqueness` Nonunique
        f (WasConsumed loc) = WasConsumed loc

binding :: [Ident] -> TypeM a -> TypeM a
binding bnds = check . local (`bindVars` bnds)
  where bindVars :: Scope -> [Ident] -> Scope
        bindVars = foldl bindVar

        bindVar :: Scope -> Ident -> Scope
        bindVar env (Ident name (Info tp) _) =
          let inedges = HS.toList $ aliases tp
              update (Bound tp')
              -- If 'name' is tuple-typed, don't alias the components
              -- to 'name', because tuples have no identity beyond
              -- their components.
                | Tuple _ <- tp = Bound tp'
                | otherwise     = Bound (tp' `addAliases` HS.insert name)
              update b = b
          in env { envVtable = HM.insert name (Bound tp) $
                               adjustSeveral update inedges $
                               envVtable env }

        adjustSeveral f = flip $ foldl $ flip $ HM.adjust f

        -- Check whether the bound variables have been used correctly
        -- within their scope.
        check m = do
          (a, usages) <- collectBindingsOccurences m
          maybeCheckOccurences usages
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

bindingPattern :: [Pattern] -> TypeM a -> TypeM a
bindingPattern ps m = do
  checkForDuplicateNames ps
  binding (HS.toList $ HS.unions $ map patIdentSet ps) m

lookupVar :: VName -> SrcLoc -> TypeM Type
lookupVar name pos = do
  bnd <- asks $ HM.lookup name . envVtable
  case bnd of
    Nothing -> bad $ UnknownVariableError (baseName name) pos
    Just (Bound t) -> return t
    Just (WasConsumed wloc) -> bad $ UseAfterConsume (baseName name) pos wloc

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

anySignedType :: [Type]
anySignedType = map (Prim . Signed) [minBound .. maxBound]

anyUnsignedType :: [Type]
anyUnsignedType = map (Prim . Unsigned) [minBound .. maxBound]

anyIntType :: [Type]
anyIntType = anySignedType ++ anyUnsignedType

anyFloatType :: [Type]
anyFloatType = map (Prim . FloatType) [minBound .. maxBound]

anyNumberType :: [Type]
anyNumberType = anyIntType ++ anyFloatType

-- | @require ts e@ causes a 'TypeError' if @typeOf e@ does not unify
-- with one of the types in @ts@.  Otherwise, simply returns @e@.
-- This function is very useful in 'checkExp'.
require :: [Type] -> Exp -> TypeM Exp
require ts e
  | any (typeOf e `similarTo`) ts = return e
  | otherwise = bad $ UnexpectedType (srclocOf e)
                      (toStructural $ typeOf e) $
                      map toStructural ts

chompDecs :: [DecBase NoInfo VName]
          -> ([FunOrTypeDecBase NoInfo VName], [DecBase NoInfo VName])
chompDecs decs = f ([], decs)
  where f (foo , FunOrTypeDec dec : xs ) = f (dec:foo , xs)
        f (foo , bar) = (foo, bar)


buildScopeFromDecs :: [FunOrTypeDecBase NoInfo VName]
                   -> TypeM Scope
buildScopeFromDecs [] = ask
buildScopeFromDecs decs = do
  scope     <- ask
  scope'    <- buildTAtable scope
  buildFtable scope'

  where

    -- To build the ftable we loop through the list of function
    -- definitions.  In addition to the normal ftable information
    -- (name, return type, argument types), we also keep track of
    -- position information, in order to report both locations of
    -- duplicate function definitions.  The position information is
    -- removed at the end.

    buildFtable scope = do
           ftable' <- HM.map rmLoc <$>
             foldM (expandFun scope) (HM.map addLoc $ envFtable scope) (mapMaybe (isFun . FunOrTypeDec) decs)
           return $ scope {envFtable = ftable'}

    buildTAtable = typeAliasTableFromProg (mapMaybe (isType . FunOrTypeDec) decs)

    paramDeclaredType (PatternAscription _ t) =
      Just $ declaredType t
    paramDeclaredType (TuplePattern ps loc) =
      UserTuple <$> mapM paramDeclaredType ps <*> pure loc
    paramDeclaredType _ =
      Nothing

    expandFun scope fntable (FunDef _ (name,_) (TypeDecl ret NoInfo) params _ pos) = do
      argtypes <- forM params $ \param ->
        case paramDeclaredType param of
          Just t -> return t
          Nothing -> bad $ TypeError (srclocOf param) $
                     "Missing type information for parameter " ++
                     pretty (untagPattern param)
      let (prefixes, _) = envBreadcrumb scope
          look tname tloc =
            maybe (throwError $ UndefinedQualName tloc tname) return $
            typeFromScope tname scope
      ret' <- expandType look ret
      argtypes' <- mapM (expandType look) argtypes
      return $ HM.insert name ( (prefixes, name) , ret' , argtypes' , pos) fntable
    rmLoc (longname, ret,args,_) = (longname, ret, args)
    addLoc (longname, t, ts) = (longname, t, ts, noLoc)

-- | Type check a program containing arbitrary no information,
-- yielding either a type error or a program with complete type
-- information.
checkProg :: UncheckedProg -> Either TypeError (Prog, VNameSource)
checkProg prog = do
  prog_checked <- runTypeM initialScope src $ Prog <$> checkProg' (progDecs prog')
  return $ flattenProgFunctions prog_checked
  where
    (prog', src) = tagProg blankNameSource prog

checkProg' :: [DecBase NoInfo VName] -> TypeM [DecBase Info VName]
checkProg' decs = do
  checkForDuplicateDecs decs
  (_, decs') <- checkDecs decs
  return decs'

checkForDuplicateDecs :: [DecBase NoInfo VName] -> TypeM ()
checkForDuplicateDecs =
  foldM_ f mempty
  where f known (FunOrTypeDec (FunDec (FunDef _ (name,_) _ _ _ loc))) =
          case HM.lookup (name, "function") known of
            Just loc' ->
              bad $ DupDefinitionError name loc loc'
            _ -> return $ HM.insert (name, "function") loc known

        f known (FunOrTypeDec (TypeDec (TypeDef name _ loc))) =
          case HM.lookup (name, "type") known of
            Just loc' ->
              bad $ DupDefinitionError name loc loc'
            _ -> return $ HM.insert (name, "type") loc known

        f known (SigDec (SigDef name _ loc)) =
          case HM.lookup (name, "signature") known of
            Just loc' ->
              bad $ DupDefinitionError name loc loc'
            _ -> return $ HM.insert (name, "signature") loc known

        f known (ModDec (ModDef name _ loc)) =
          case HM.lookup (name, "module") known of
            Just loc' ->
              bad $ DupDefinitionError name loc loc'
            _ -> return $ HM.insert (name, "module") loc known


checkMod :: ModDefBase NoInfo VName -> TypeM (Scope , ModDefBase Info VName)
checkMod (ModDef name decs loc) =
  local (`addBreadcrumb` name) $ do
    checkForDuplicateDecs decs
    (scope, decs') <- checkDecs decs
    return (scope, ModDef name decs' loc)

checkDecs :: [DecBase NoInfo VName] -> TypeM (Scope, [DecBase Info VName])
checkDecs (ModDec modd:rest) = do
  (modscope, modd') <- checkMod modd
  local (addModule modscope) $
    do
      (scope, rest') <- checkDecs rest
      return (scope, ModDec modd' : rest' )

checkDecs (SigDec _:rest) = checkDecs rest

checkDecs [] = do
  scope <- ask
  return (scope, [])

checkDecs decs = do
    let (funOrTypeDecs, rest) = chompDecs decs
    scopeFromFunOrTypeDecs <- buildScopeFromDecs funOrTypeDecs
    local (const scopeFromFunOrTypeDecs) $ do
      checkedeDecs <- checkFunOrTypeDec funOrTypeDecs
      (scope, rest') <- checkDecs rest
      return (scope , checkedeDecs ++ rest')


checkFunOrTypeDec :: [FunOrTypeDecBase NoInfo VName] -> TypeM [DecBase Info VName]
checkFunOrTypeDec (FunDec fundef:decs) = do
    fundef' <- checkFun fundef
    decs' <- checkFunOrTypeDec decs
    return $ FunOrTypeDec (FunDec fundef') : decs'

checkFunOrTypeDec (TypeDec _:decs) = checkFunOrTypeDec decs

checkFunOrTypeDec [] = return []


initialFtable :: HM.HashMap Name FunBinding
initialFtable = HM.fromList $ map addBuiltin $ HM.toList builtInFunctions
  where addBuiltin (name, (t, ts)) = (name, (([],name), Prim t, map Prim ts))

checkFun :: FunDefBase NoInfo VName -> TypeM FunDef
checkFun (FunDef entry fullname@(fname,_) rettype params body loc) = do
  params' <- mapM (`checkPattern` NoneInferred) params
  checkForDuplicateNames params'

  bindingPattern params' $ do
    rettype' <- checkTypeDecl rettype
    body' <- checkFunBody fname body (Just rettype') loc
    let rettype_structural = toStructural $ unInfo $ expandedType rettype'

    checkReturnAlias rettype_structural params' $ typeOf body'

    when entry $ do
      unless (okEntryPointType (unInfo $ expandedType rettype')) $
        bad $ InvalidEntryPointReturnType loc fname

      forM_ (zip params' params) $ \(param, orig_param) ->
        unless (okEntryPointType (patternType param)) $
          bad $ InvalidEntryPointParamType (srclocOf param) fname $ untagPattern orig_param

    return $ FunDef entry fullname rettype' params' body' loc

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

checkFunBody :: Name
             -> ExpBase NoInfo VName
             -> Maybe (TypeDeclBase Info VName)
             -> SrcLoc
             -> TypeM Exp
checkFunBody fname body maybe_rettype loc = do
  body' <- checkExp body

  case maybe_rettype of
    Just rettype -> do
      checkRetType loc $ unInfo $ expandedType rettype
      let rettype_structural = toStructural $ unInfo $ expandedType rettype
      unless (toStructural (typeOf body') `subtypeOf` rettype_structural) $
        bad $ ReturnTypeError loc fname rettype_structural $ toStructural $ typeOf body'
    Nothing -> return ()

  return body'

checkExp :: ExpBase NoInfo VName
         -> TypeM Exp

checkExp (Literal val pos) =
  Literal <$> checkLiteral pos val <*> pure pos

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

checkExp (Empty decl loc) =
  Empty <$> checkTypeDecl decl <*> pure loc

checkExp (BinOp op e1 e2 NoInfo pos) = checkBinOp op e1 e2 pos

checkExp (UnOp Not e pos) = do
  e' <- require [Prim Bool] =<< checkExp e
  return $ UnOp Not e' pos

checkExp (UnOp Complement e loc) = do
  e' <- require anyIntType =<< checkExp e
  return $ UnOp Complement e' loc

checkExp (UnOp Negate e loc) = do
  e' <- require anyNumberType =<< checkExp e
  return $ UnOp Negate e' loc

checkExp (UnOp Abs e loc) = do
  e' <- require anyNumberType =<< checkExp e
  return $ UnOp Abs e' loc

checkExp (UnOp Signum e loc) = do
  e' <- require anyIntType =<< checkExp e
  return $ UnOp Signum e' loc

checkExp (UnOp (ToFloat t) e loc) = do
  e' <- require anyNumberType =<< checkExp e
  return $ UnOp (ToFloat t) e' loc

checkExp (UnOp (ToSigned t) e loc) = do
  e' <- require anyNumberType =<< checkExp e
  return $ UnOp (ToSigned t) e' loc

checkExp (UnOp (ToUnsigned t) e loc) = do
  e' <- require anyNumberType =<< checkExp e
  return $ UnOp (ToUnsigned t) e' loc

checkExp (If e1 e2 e3 _ pos) = do
  e1' <- require [Prim Bool] =<< checkExp e1
  ((e2', e3'), dflow) <- collectOccurences $ checkExp e2 `alternative` checkExp e3
  tell dflow
  brancht <- unifyExpTypes e2' e3'
  let t' = addAliases brancht
           (`HS.difference` allConsumed dflow)
  return $ If e1' e2' e3' (Info t') pos

checkExp (Var ident) = do
  ident' <- checkIdent ident
  observe ident'
  return $ Var ident'

checkExp (Apply fname args _ loc) = do
  bnd <- asks (funFromScope fname)
  case bnd of
    Nothing -> bad $ UnknownFunctionError fname loc
    Just (longname, ftype, paramtypes) -> do
      (args', argflows) <- unzip <$> mapM (\(arg,_) -> (checkArg arg)) args

      let rettype' = returnType (removeShapeAnnotations ftype)
                     (map diet paramtypes) (map typeOf args')

      checkFuncall (Just fname) loc paramtypes argflows

      return $ Apply longname (zip args' $ map diet paramtypes) (Info rettype') loc

checkExp (LetPat pat e body pos) = do
  (e', dflow) <- collectOccurences $ checkExp e
  (scope, pat') <- checkBinding pat (typeOf e') dflow
  scope $ do
    body' <- checkExp body
    return $ LetPat pat' e' body' pos

checkExp (LetWith d@(Ident dest _ destpos) src idxes ve body pos) = do
  src' <- checkIdent src
  idxes' <- mapM checkDimIndex idxes
  let destt' = unInfo (identType src') `setAliases` HS.empty
      dest' = Ident dest (Info destt') destpos

  unless (unique $ unInfo $ identType src') $
    bad $ TypeError pos $ "Source '" ++ pretty (baseName $ identName src) ++
    "' has type " ++ pretty (unInfo $ identType src') ++ ", which is not unique"

  case peelArray (length $ filter isFix idxes') (unInfo $ identType src') of
    Nothing -> bad $ IndexingError
                     (arrayRank $ unInfo $ identType src') (length idxes) (srclocOf src)
    Just elemt ->
      sequentially (require [elemt] =<< checkExp ve) $ \ve' _ -> do
        when (identName src `HS.member` aliases (typeOf ve')) $
          bad $ BadLetWithValue pos
        (scope, _) <- checkBinding (Id d) destt' mempty
        body' <- consuming src' $ scope $ checkExp body
        return $ LetWith dest' src' idxes' ve' body' pos
  where isFix DimFix{} = True
        isFix _        = False

checkExp (Update v idxes ve loc) = do
  v' <- checkIdent v
  idxes' <- mapM checkDimIndex idxes

  unless (unique $ unInfo $ identType v') $
    bad $ TypeError loc $ "Source '" ++ pretty (baseName $ identName v) ++
    "' has type " ++ pretty (unInfo $ identType v') ++ ", which is not unique"

  case peelArray (length $ filter isFix idxes') (unInfo $ identType v') of
    Nothing -> bad $ IndexingError
                     (arrayRank $ unInfo $ identType v') (length idxes) (srclocOf v)
    Just elemt ->
      sequentially (require [elemt] =<< checkExp ve) $ \ve' _ -> do
        when (identName v `HS.member` aliases (typeOf ve')) $
          bad $ BadLetWithValue loc
        return $ Update v' idxes' ve' loc
  where isFix DimFix{} = True
        isFix _        = False

checkExp (Index e idxes pos) = do
  e' <- checkExp e
  let vt = typeOf e'
  when (arrayRank vt < length idxes) $
    bad $ IndexingError (arrayRank vt) (length idxes) pos
  idxes' <- mapM checkDimIndex idxes
  return $ Index e' idxes' pos

checkExp (TupleIndex e i NoInfo loc) = do
  e' <- checkExp e
  case typeOf e' of
    Tuple ts | t:_ <- drop i ts -> return $ TupleIndex e' i (Info t) loc
    _ -> bad $ InvalidField loc (typeOf e') (show i)

checkExp (Iota e pos) = do
  e' <- require [Prim $ Signed Int32] =<< checkExp e
  return $ Iota e' pos

checkExp (Shape e loc) = do
  e' <- checkExp e
  case typeOf e' of
    t | arrayRank t > 0 -> return $ Shape e' loc
      | otherwise ->
          bad $ TypeError loc
          $ "Argument to shape must be an array, not of type " ++ pretty (typeOf e') ++ "."

checkExp (Replicate countexp valexp pos) = do
  countexp' <- require [Prim $ Signed Int32] =<< checkExp countexp
  valexp' <- checkExp valexp
  return $ Replicate countexp' valexp' pos

checkExp (Reshape shapeexp arrexp loc) = do
  shapeexp' <- checkExp shapeexp
  arrexp' <- checkExp arrexp

  case typeOf shapeexp' of
    Tuple ts | all (==(Prim $ Signed Int32)) ts -> return ()
    Prim (Signed Int32) -> return ()
    t -> bad $ TypeError loc $ "Shape argument " ++ pretty shapeexp ++
      " to reshape must be i32 or tuple of i32s, but is " ++ pretty t

  return $ Reshape shapeexp' arrexp' loc

checkExp (Rearrange perm arrexp pos) = do
  arrexp' <- checkExp arrexp
  let rank = arrayRank $ typeOf arrexp'
  when (length perm /= rank || sort perm /= [0..rank-1]) $
    bad $ PermutationError pos perm rank name
  return $ Rearrange perm arrexp' pos
  where name = case arrexp of Var v -> Just $ baseName $ identName v
                              _     -> Nothing

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
  let isArrayType arrtp =
        case arrtp of
          Array _ -> True
          _       -> False
  let lit_int0 = Literal (PrimValue $ SignedValue $ Int32Value 0) pos
  [(_, intarg),(arr',arrarg)] <- mapM checkArg [lit_int0, arr]
  -- arr must have an array type
  unless (isArrayType $ typeOf arr') $
    bad $ TypeError pos $ "Stream with input array of non-array type " ++ pretty (typeOf arr') ++ "."
  -- typecheck stream's lambdas
  (form', macctup) <-
    case form of
      MapLike o -> return (MapLike o, Nothing)
      RedLike o comm lam0 acc -> do
        (acc',accarg) <- checkArg acc
        lam0' <- checkLambda lam0 [accarg, accarg]
        let redtype = lambdaReturnType lam0'
        unless (typeOf acc' `subtypeOf` redtype) $
            bad $ TypeError pos $ "Stream's reduce fun: Initial value is of type " ++
                  pretty (typeOf acc') ++ ", but reduce fun returns type "++pretty redtype++"."
        return (RedLike o comm lam0' acc', Just(acc',accarg))
      Sequential acc -> do
        (acc',accarg) <- checkArg acc
        return (Sequential acc', Just(acc',accarg))
  -- (i) properly check the lambda on its parameter and
  --(ii) make some fake arguments, which do not alias `arr', and
  --     check that aliases of `arr' are not used inside lam.
  let fakearg = (typeOf arr' `setAliases` HS.empty, mempty, srclocOf pos)
      (aas,faas) = case macctup of
                    Nothing        -> ([intarg, arrarg],        [intarg,fakearg]         )
                    Just(_,accarg) -> ([intarg, accarg, arrarg],[intarg, accarg, fakearg])

  lam' <- checkLambda lam aas
  (_, dflow)<- collectOccurences $ checkLambda lam faas
  let arr_aliasses = HS.toList $ aliases $ typeOf arr'
  let usages = usageMap dflow
  when (any (`HM.member` usages) arr_aliasses) $
     bad $ TypeError pos "Stream with input array used inside lambda."
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
  arr2exps' <- mapM (require [typeOf arr1exp'] <=< checkExp) arr2exps
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
  -- First we do a basic check of the loop body to figure out which of
  -- the merge parameters are being consumed.  For this, we first need
  -- to check the merge pattern, which requires the (initial) merge
  -- expression.
  ((mergeexp', bindExtra), mergeflow) <-
    collectOccurences $ do
      mergeexp' <- checkExp mergeexp
      return $
        case form of
          For _ _ (Ident loopvar _ _) _ ->
            let iparam = Ident loopvar (Info $ Prim $ Signed Int32) loc
            in (mergeexp', [iparam])
          While _ ->
            (mergeexp', [])

  -- Check the loop body.
  (firstscope, mergepat') <- checkBinding mergepat (typeOf mergeexp') mempty
  ((form', loopbody'), bodyflow) <-
    noUnique $ firstscope $ binding bindExtra $ collectOccurences $
    case form of
      For dir lboundexp (Ident loopvar _ loopvarloc) uboundexp -> do
        lboundexp' <- require [Prim $ Signed Int32] =<< checkExp lboundexp
        uboundexp' <- require [Prim $ Signed Int32] =<< checkExp uboundexp
        loopbody' <- checkExp loopbody
        return (For dir lboundexp' (Ident loopvar (Info $ Prim $ Signed Int32) loopvarloc) uboundexp',
                loopbody')
      While condexp -> do
        (condexp', condflow) <-
          collectOccurences $ require [Prim Bool] =<< checkExp condexp
        (loopbody', bodyflow) <-
          collectOccurences $ checkExp loopbody
        occur $ condflow `seqOccurences` bodyflow
        return (While condexp',
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

  occur $ mergeflow `seqOccurences` merge_consume

  binding (HS.toList $ patIdentSet mergepat'') $ do
    letbody' <- checkExp letbody
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

checkSOACArrayArg :: ExpBase NoInfo VName
                  -> TypeM (Exp, Arg)
checkSOACArrayArg e = do
  (e', (t, dflow, argloc)) <- checkArg e
  case peelArray 1 t of
    Nothing -> bad $ TypeError argloc "SOAC argument is not an array"
    Just rt -> return (e', (rt, dflow, argloc))

checkLiteral :: SrcLoc -> Value -> TypeM Value
checkLiteral _ (PrimValue bv) = return $ PrimValue bv
checkLiteral loc (TupValue vals) = do
  vals' <- mapM (checkLiteral loc) vals
  return $ TupValue vals'
checkLiteral loc (ArrayValue arr rt) = do
  vals <- mapM (checkLiteral loc) (elems arr)
  case find ((/=rt) . removeNames . valueType) vals of
    Just wrong -> bad $ TypeError loc $ pretty wrong ++ " is not of expected type " ++ pretty rt ++ "."
    _          -> return ()
  return $ ArrayValue (listArray (bounds arr) vals) rt

checkIdent :: IdentBase NoInfo VName -> TypeM Ident
checkIdent (Ident name _ pos) = do
  vt <- lookupVar name pos
  return $ Ident name (Info vt) pos

data InferredType = NoneInferred
                  | Inferred Type
                  | Ascribed Type

checkPattern :: PatternBase NoInfo VName -> InferredType
             -> TypeM Pattern
checkPattern (Id (Ident name NoInfo loc)) (Inferred t) =
  let t' = typeOf $ Var $ Ident name (Info t) loc
  in return $ Id (Ident name (Info $ t' `setUniqueness` Nonunique) loc)
checkPattern (Id (Ident name NoInfo loc)) (Ascribed t) =
  let t' = typeOf $ Var $ Ident name (Info t) loc
  in return $ Id (Ident name (Info t') loc)
checkPattern (Wildcard _ loc) (Inferred t) =
  return $ Wildcard (Info $ t `setUniqueness` Nonunique) loc
checkPattern (Wildcard _ loc) (Ascribed t) =
  return $ Wildcard (Info $ t `setUniqueness` Nonunique) loc
checkPattern (TuplePattern ps loc) (Inferred (Tuple ts)) =
  TuplePattern <$> zipWithM checkPattern ps (map Inferred ts) <*> pure loc
checkPattern (TuplePattern ps loc) (Ascribed (Tuple ts)) =
  TuplePattern <$> zipWithM checkPattern ps (map Ascribed ts) <*> pure loc
checkPattern p@TuplePattern{} (Inferred t) =
  bad $ TypeError (srclocOf p) $ "Pattern " ++ pretty (untagPattern p) ++ " cannot match " ++ pretty t
checkPattern p@TuplePattern{} (Ascribed t) =
  bad $ TypeError (srclocOf p) $ "Pattern " ++ pretty (untagPattern p) ++ " cannot match " ++ pretty t
checkPattern (TuplePattern ps loc) NoneInferred =
  TuplePattern <$> zipWithM checkPattern ps (repeat NoneInferred) <*> pure loc
checkPattern fullp@(PatternAscription p td) maybe_outer_t = do
  td' <- checkTypeDecl td
  let maybe_outer_t' = case maybe_outer_t of Inferred t -> Just t
                                             Ascribed t -> Just t
                                             NoneInferred -> Nothing
      t' = fromStruct (removeShapeAnnotations $ unInfo $ expandedType td')
           `addAliases` (<> maybe mempty aliases maybe_outer_t')
  case maybe_outer_t' of
    Just outer_t
      | not (outer_t `subtypeOf` t') ->
          bad $ InvalidPatternError (untagPattern fullp) (toStructural outer_t) Nothing $ srclocOf p
    _ -> PatternAscription <$> checkPattern p (Ascribed t') <*> pure td'
checkPattern p NoneInferred =
  bad $ TypeError (srclocOf p) $ "Cannot determine type of " ++ pretty (untagPattern p)

checkForDuplicateNames :: [Pattern] -> TypeM ()
checkForDuplicateNames = flip evalStateT mempty . mapM_ check
  where check (Id v) = seeing v
        check Wildcard{} = return ()
        check (TuplePattern ps _) = mapM_ check ps
        check (PatternAscription p _) = check p

        seeing v = do
          let name = baseName $ identName v
          seen <- get
          case HM.lookup name seen of
            Just loc -> lift $ bad $ DupPatternError name (srclocOf v) loc
            Nothing -> modify $ HM.insert name $ srclocOf v

checkBinOp :: BinOp -> ExpBase NoInfo VName -> ExpBase NoInfo VName -> SrcLoc
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
checkBinOp Equal e1 e2 pos = checkRelOp Equal anyNumberType e1 e2 pos
checkBinOp NotEqual e1 e2 pos = checkRelOp NotEqual anyNumberType e1 e2 pos
checkBinOp Less e1 e2 pos = checkRelOp Less anyNumberType e1 e2 pos
checkBinOp Leq e1 e2 pos = checkRelOp Leq anyNumberType e1 e2 pos
checkBinOp Greater e1 e2 pos = checkRelOp Greater anyNumberType e1 e2 pos
checkBinOp Geq e1 e2 pos = checkRelOp Geq anyNumberType e1 e2 pos

checkRelOp :: BinOp -> [Type]
           -> ExpBase NoInfo VName -> ExpBase NoInfo VName -> SrcLoc
           -> TypeM Exp
checkRelOp op tl e1 e2 pos = do
  e1' <- require tl =<< checkExp e1
  e2' <- require tl =<< checkExp e2
  _ <- unifyExpTypes e1' e2'
  return $ BinOp op e1' e2' (Info $ Prim Bool) pos

checkPolyBinOp :: BinOp -> [Type]
               -> ExpBase NoInfo VName -> ExpBase NoInfo VName -> SrcLoc
               -> TypeM Exp
checkPolyBinOp op tl e1 e2 pos = do
  e1' <- require tl =<< checkExp e1
  e2' <- require tl =<< checkExp e2
  t' <- unifyExpTypes e1' e2'
  return $ BinOp op e1' e2' (Info t') pos

checkDimIndex :: DimIndexBase NoInfo VName -> TypeM DimIndex
checkDimIndex (DimFix i) =
  DimFix <$> (require [Prim $ Signed Int32] =<< checkExp i)
checkDimIndex (DimSlice i j) =
  DimSlice
  <$> (require [Prim $ Signed Int32] =<< checkExp i)
  <*> (require [Prim $ Signed Int32] =<< checkExp j)

sequentially :: TypeM a -> (a -> Occurences -> TypeM b) -> TypeM b
sequentially m1 m2 = do
  (a, m1flow) <- collectOccurences m1
  (b, m2flow) <- collectOccurences $ m2 a m1flow
  occur $ m1flow `seqOccurences` m2flow
  return b

checkBinding :: PatternBase NoInfo VName -> Type -> Occurences
             -> TypeM (TypeM a -> TypeM a, Pattern)
checkBinding pat et dflow = do
  -- Not technically an ascription, but we want the pattern to have exactly the type of 'e'.
  pat' <- checkPattern pat $ Ascribed et
  return (\m -> sequentially (tell dflow) (const . const $ bindingPattern [pat'] m), pat')

validApply :: [StructTypeBase VName] -> [Type] -> Bool
validApply expected got =
  length got == length expected &&
  and (zipWith subtypeOf (map toStructural got) (map toStructural expected))

type Arg = (Type, Occurences, SrcLoc)

argType :: Arg -> Type
argType (t, _, _) = t

checkArg :: ExpBase NoInfo VName -> TypeM (Exp, Arg)
checkArg arg = do
  (arg', dflow) <- collectOccurences $ checkExp arg
  return (arg', (typeOf arg', dflow, srclocOf arg'))

checkFuncall :: Maybe QualName -> SrcLoc
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

checkLambda :: LambdaBase NoInfo VName -> [Arg]
            -> TypeM Lambda
checkLambda (AnonymFun params body maybe_ret NoInfo loc) args
  | length params == length args = do
      params' <- zipWithM checkPattern params $
                 map (Inferred . (`setAliases` mempty) . argType) args
      maybe_ret' <- maybe (pure Nothing) (fmap Just . checkTypeDecl) maybe_ret
      body' <- bindingPattern params' $
        checkFunBody (nameFromString "<anonymous>") body maybe_ret' loc
      checkFuncall Nothing loc (map patternStructType params') args
      let ret' = case maybe_ret' of
                   Nothing -> flip setAliases NoInfo $ vacuousShapeAnnotations $ typeOf body'
                   Just (TypeDecl _ (Info ret)) -> ret
      return $ AnonymFun params' body' maybe_ret' (Info ret') loc
  | otherwise = bad $ TypeError loc $ "Anonymous function defined with " ++ show (length params) ++ " parameters, but expected to take " ++ show (length args) ++ " arguments."

checkLambda (CurryFun fname curryargexps _ loc) args = do
  (curryargexps', curryargs) <- unzip <$> mapM checkArg curryargexps
  bnd <- asks (funFromScope fname)
  case bnd of
    Nothing -> bad $ UnknownFunctionError fname loc
    Just (longname, rt, paramtypes) -> do
      let rettype' = fromStruct $ removeShapeAnnotations rt
      case find (unique . snd) $ zip curryargexps paramtypes of
        Just (e, _) -> bad $ CurriedConsumption fname $ srclocOf e
        _           -> return ()
      checkFuncall Nothing loc paramtypes $ curryargs ++ args
      return $ CurryFun longname curryargexps' (Info rettype') loc

checkLambda (UnOpFun unop NoInfo NoInfo loc) [arg] = do
  var <- newIdent "x" (argType arg) loc
  binding [var] $ do
    e <- checkExp (UnOp unop (Var var { identType = NoInfo }) loc)
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
                -> BinOp -> ExpBase NoInfo VName -> SrcLoc -> Arg -> TypeM b
checkCurryBinOp f binop x loc arg = do
  x' <- checkExp x
  y <- newIdent "y" (argType arg) loc
  xvar <- newIdent "x" (typeOf x') loc
  binding [y, xvar] $ do
    e <- checkExp (BinOp binop (Var $ untype y) (Var $ untype xvar) NoInfo loc)
    return $ f binop x' (Info $ argType arg) (Info $ typeOf e) loc
  where untype (Ident name _ varloc) = Ident name NoInfo varloc

checkPolyLambdaOp :: BinOp -> [ExpBase NoInfo VName] -> [Arg]
                  -> SrcLoc -> TypeM Lambda
checkPolyLambdaOp op curryargexps args pos = do
  curryargexpts <- map typeOf <$> mapM checkExp curryargexps
  let argts = [ argt | (argt, _, _) <- args ]
  tp <- case curryargexpts ++ argts of
          [t1, t2] | t1 == t2 -> return t1
          l -> bad $ ParameterMismatch (Just fname) pos (Left 2) $ map toStructural l
  xname <- newIDFromString "x"
  yname <- newIDFromString "y"
  let xident t = Ident xname t pos
      yident t = Ident yname t pos
  (x,y,params) <- case curryargexps of
                    [] -> return (Var $ xident NoInfo,
                                  Var $ yident NoInfo,
                                  [xident $ Info tp, yident $ Info tp])
                    [e] -> return (e,
                                   Var $ yident NoInfo,
                                   [yident $ Info tp])
                    (e1:e2:_) -> return (e1, e2, [])
  rettype <- typeOf <$> binding params (checkBinOp op x y pos)
  return $ BinOpFun op (Info tp) (Info tp) (Info rettype) pos
  where fname = nameToQualName $ nameFromString $ pretty op

checkRetType :: SrcLoc -> StructType -> TypeM ()
checkRetType loc (Tuple ts) = mapM_ (checkRetType loc) ts
checkRetType _ (Prim _) = return ()
checkRetType loc (Array at) =
  checkArrayType loc at

checkArrayType :: SrcLoc
               -> DeclArrayTypeBase VName
               -> TypeM ()
checkArrayType loc (PrimArray _ ds _ _) =
  mapM_ (checkDim loc) $ shapeDims ds
checkArrayType loc (TupleArray cts ds _) = do
  mapM_ (checkDim loc) $ shapeDims ds
  mapM_ (checkTupleArrayElem loc) cts

checkTupleArrayElem :: SrcLoc
                    -> DeclTupleArrayElemTypeBase VName
                    -> TypeM ()
checkTupleArrayElem _ PrimArrayElem{} =
  return ()
checkTupleArrayElem loc (ArrayArrayElem at) =
  checkArrayType loc at
checkTupleArrayElem loc (TupleArrayElem cts) =
  mapM_ (checkTupleArrayElem loc) cts

checkDim :: SrcLoc -> DimDecl VName -> TypeM ()
checkDim _ AnyDim =
  return ()
checkDim _ (ConstDim _) =
  return ()
checkDim loc (NamedDim name) = do
  t <- lookupVar name loc
  case t of
    Prim (Signed Int32) -> return ()
    _                   -> bad $ DimensionNotInteger loc $ baseName name

expandType :: (Applicative m, MonadError TypeError m) =>
               (QualName -> SrcLoc -> m (StructTypeBase VName))
            -> UserType VName
            -> m (StructTypeBase VName)

expandType look (UserTypeAlias longname loc) =
  look longname loc
expandType _ (UserPrim prim _) =
  return $ Prim prim
expandType look (UserTuple ts _) =
  Tuple <$> mapM (expandType look) ts
expandType look (UserArray t d _) = do
  t' <- expandType look t
  return $ arrayOf t' (ShapeDecl [d]) Nonunique
expandType look (UserUnique t loc) = do
  t' <- expandType look t
  case t' of
    Array{} -> return $ t' `setUniqueness` Unique
    _       -> throwError $ InvalidUniqueness loc $ toStructural t'

checkTypeDecl :: TypeDeclBase NoInfo VName -> TypeM (TypeDeclBase Info VName)
checkTypeDecl (TypeDecl t NoInfo) =
  TypeDecl t . Info <$> expandType look t
  where look longname loc = do
          types <- asks (typeFromScope longname)
          case types of
            Nothing    -> throwError $ UndefinedQualName loc longname
            Just namet -> return namet

-- Creating the initial type alias table is done by maintaining a
-- table of the type aliases we have processed (initialised to empty),
-- then initialising every type alias.  This ensures we do not waste
-- time processing any alias more than once.  The monadic structure is
-- a Reader and a State on top of an Either.

type TypeAliasTableM =
  ReaderT (HS.HashSet QualName) (StateT Scope TypeM)

typeAliasTableFromProg :: [TypeDefBase NoInfo VName]
                       -> Scope
                       -> TypeM Scope
typeAliasTableFromProg defs scope = do
  checkForDuplicateTypes defs
  execStateT (runReaderT (mapM_ process defs) mempty) scope
  where
        findDefByName name = find ((==name) . typeAlias) defs

        process :: TypeDefBase NoInfo VName
                -> TypeAliasTableM (StructTypeBase VName)
        process (TypeDef name (TypeDecl ut NoInfo) _) = do
          t <- expandType typeOfName ut
          modify $ addType name t
          return t

        typeOfName :: QualName -> SrcLoc
                   -> TypeAliasTableM (StructTypeBase VName)
        typeOfName (prefixes, name) loc = do
          inside <- ask
          known <- get
          case typeFromScope (prefixes, name) known of
            Just t -> return t
            Nothing
              | (prefixes, name) `HS.member` inside ->
                  throwError $ CyclicalTypeDefinition loc name
              | Just def <- findDefByName name ->
                  local (HS.insert (prefixes, name)) $ process def
              | otherwise ->
                  throwError $ UndefinedAlias loc name

addBreadcrumb :: Scope -> Name -> Scope
addBreadcrumb scope name =
  let (names, _) = envBreadcrumb scope
  in scope {envBreadcrumb = (names ++ [name], name)}

addModule :: Scope -> Scope -> Scope
addModule modd scope =
  let moddName = moduleName modd
  in scope {envModTable = HM.insert moddName modd $ envModTable scope}

moduleName :: Scope -> Name
moduleName modd =
  let (_, name) = envBreadcrumb modd
  in name

envFromScope :: [Name] -> Scope -> Maybe Scope
envFromScope (x:xs) scope = case HM.lookup x $ envModTable scope
  of
    Just scope' -> envFromScope xs scope'
    Nothing -> Nothing
envFromScope [] scope = Just scope


typeFromScope :: QualName -> Scope -> Maybe TypeBinding
typeFromScope (prefixes, name) scope = do
  scope' <- envFromScope prefixes scope
  let taTable = envTAtable scope'
  HM.lookup name taTable

addType :: Name -> StructTypeBase VName -> Scope -> Scope
addType name tp scope =
  scope {envTAtable = HM.insert name tp $ envTAtable scope}

funFromScope :: QualName -> Scope -> Maybe FunBinding
funFromScope (prefixes, name) scope = do
  scope' <- envFromScope prefixes scope
  let taTable = envFtable scope'
  HM.lookup name taTable

checkForDuplicateTypes :: [TypeDefBase NoInfo VName] -> TypeM ()
checkForDuplicateTypes = foldM_ check mempty
  where check seen def
          | name `HS.member` seen =
              throwError $ DupTypeAlias (srclocOf def) name
          | otherwise =
              return $ name `HS.insert` seen
              where name = typeAlias def


flattenProgFunctions :: (Prog, a) -> (Prog, a)
flattenProgFunctions (prog, a) = let
  topLongname = ([], nameFromString "")
  bottomFuns = map (giveLongname topLongname) $ mapMaybe isFun $ progDecs prog
  moduleFuns = concatMap (flattenModule topLongname) $ mapMaybe isMod $ progDecs prog
  funs = map (FunOrTypeDec . FunDec) (bottomFuns ++ moduleFuns)
  in (Prog funs , a)

flattenModule :: QualName -> ModDefBase f vn -> [FunDefBase f vn]
flattenModule longName modd =
  let appendedName = appendNameToQualName (modName modd) longName
   in flattenModule' appendedName modd

flattenModule' :: QualName -> ModDefBase f vn -> [FunDefBase f vn]
flattenModule' longname moddefbase = let
  functions = mapMaybe isFun $ modDecls moddefbase
  modules = mapMaybe isMod $ modDecls moddefbase
  functions' = map (giveLongname longname) functions
  modulefunctions = concatMap (flattenModule longname) modules
  in functions' ++ modulefunctions

appendNameToQualName :: Name -> QualName -> QualName
appendNameToQualName name (prefixes, a) = (prefixes ++ [name] , a)

giveLongname :: QualName -> FunDefBase f vn -> FunDefBase f vn
giveLongname (prefixes, _) fundef =
  let (funname, realizedName) = funDefName fundef in
  fundef { funDefName = (longnameToName (prefixes, funname), realizedName) }
