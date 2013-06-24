{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The type checker checks whether the program is type-consistent.
-- Whether type annotations are already present is irrelevant, but if
-- they are, the type checker will signal an error if they are wrong.
-- The program does not need to have any particular properties for the
-- type checker to function; in particular it does not need unique
-- names.
module L0C.TypeChecker ( checkProg
                       , checkProgNoUniqueness
                       , TypeError(..))
  where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
import Data.Array
import Data.List
import Data.Loc
import Data.Maybe
import qualified Data.Traversable as T

import qualified Data.Map as M
import qualified Data.Set as S

import L0C.L0
import L0C.Renamer (tagProg', untagProg, untagExp, untagPattern, untagType)
import L0C.FreshNames

-- | Information about an error during type checking.  The 'Show'
-- instance for this type produces a human-readable description.
data TypeError vn = TypeError SrcLoc String
                  -- ^ A general error happened at the given position and
                  -- for the given reason.
                  | UnifyError (ExpBase CompTypeBase vn) (ExpBase CompTypeBase vn)
                  -- ^ Types of two expressions failed to unify.
                  | UnexpectedType (ExpBase CompTypeBase vn) [DeclTypeBase (ID vn)]
                  -- ^ Expression of type was not one of the expected
                  -- types.
                  | ReturnTypeError SrcLoc Name (DeclTypeBase (ID vn)) (DeclTypeBase (ID vn))
                  -- ^ The body of a function definition has a different
                  -- type than its declaration.
                  | DupDefinitionError Name SrcLoc SrcLoc
                  -- ^ Two functions have been defined with the same name.
                  | DupParamError Name vn SrcLoc
                  -- ^ Two function parameters share the same name.
                  | DupPatternError vn SrcLoc SrcLoc
                  -- ^ Two pattern variables share the same name.
                  | InvalidPatternError (TupIdentBase NoInfo vn) (CompTypeBase vn) SrcLoc
                  -- ^ The pattern is not compatible with the type.
                  | UnknownVariableError vn SrcLoc
                  -- ^ Unknown variable of the given name referenced at the given spot.
                  | UnknownFunctionError Name SrcLoc
                  -- ^ Unknown function of the given name called at the given spot.
                  | ParameterMismatch (Maybe Name) SrcLoc (Either Int [DeclTypeBase vn]) [CompTypeBase vn]
                  -- ^ A function (possibly anonymous) was called with
                  -- invalid arguments.  The third argument is either the
                  -- number of parameters, or the specific types of
                  -- parameters accepted (sometimes, only the former can
                  -- be determined).
                  | UseAfterConsume vn SrcLoc SrcLoc
                  -- ^ A variable was attempted used after being
                  -- consumed.  The last location is the point of
                  -- consumption.
                  | IndexingError vn Int Int SrcLoc
                  -- ^ Too many indices provided.  The first integer is
                  -- the number of dimensions in the array being
                  -- indexed.
                  | BadAnnotation SrcLoc String (CompTypeBase (ID vn)) (CompTypeBase (ID vn))
                  -- ^ One of the type annotations fails to match with the
                  -- derived type.  The string is a description of the
                  -- role of the type.  The last type is the new derivation.
                  | CurriedConsumption Name SrcLoc
                  -- ^ A function is being curried with an argument to be consumed.
                  | BadLetWithValue SrcLoc
                  -- ^ The new value for an array slice in let-with is aliased to the source.
                  | ReturnAliased Name vn SrcLoc
                  -- ^ The unique return value of the function aliases
                  -- one of the function parameters.
                  | UniqueReturnAliased Name SrcLoc
                  -- ^ A unique element of the tuple returned by the
                  -- function aliases some other element of the tuple.


instance VarName vn => Show (TypeError vn) where
  show (TypeError pos msg) =
    "Type error at " ++ locStr pos ++ ":\n" ++ msg
  show (UnifyError e1 e2) =
    "Cannot unify type " ++ ppType (typeOf e1) ++
    " of expression at " ++ locStr (srclocOf e1) ++
    " with type " ++ ppType (typeOf e2) ++
    " of expression at " ++ locStr (srclocOf e2)
  show (UnexpectedType e []) =
    "Type of expression at " ++ locStr (srclocOf e) ++
    " cannot have any type - possibly a bug in the type checker."
  show (UnexpectedType e ts) =
    "Type of expression at " ++ locStr (srclocOf e) ++
    " must be one of " ++ intercalate ", " (map ppType ts) ++ ", but is " ++
    ppType (typeOf e) ++ "."
  show (ReturnTypeError pos fname rettype bodytype) =
    "Declaration of function " ++ nameToString fname ++ " at " ++ locStr pos ++
    " declares return type " ++ ppType rettype ++ ", but body has type " ++
    ppType bodytype
  show (DupDefinitionError name pos1 pos2) =
    "Duplicate definition of function " ++ nameToString name ++ ".  Defined at " ++
    locStr pos1 ++ " and " ++ locStr pos2 ++ "."
  show (DupParamError funname paramname pos) =
    "Parameter " ++ textual paramname ++
    " mentioned multiple times in argument list of function " ++
    nameToString funname ++ " at " ++ locStr pos ++ "."
  show (DupPatternError name pos1 pos2) =
    "Variable " ++ textual name ++ " bound twice in tuple pattern; at " ++
    locStr pos1 ++ " and " ++ locStr pos2 ++ "."
  show (InvalidPatternError pat t loc) =
    "Pattern " ++ ppTupId pat ++ " at " ++ locStr (srclocOf pat) ++
    " cannot match value of type " ++ ppType t ++ " at " ++ locStr loc ++ "."
  show (UnknownVariableError name pos) =
    "Unknown variable " ++ textual name ++ " referenced at " ++ locStr pos ++ "."
  show (UnknownFunctionError fname pos) =
    "Unknown function " ++ nameToString fname ++ " called at " ++ locStr pos ++ "."
  show (ParameterMismatch fname pos expected got) =
    "In call of " ++ fname' ++ " at position " ++ locStr pos ++
    ": expecting " ++ show nexpected ++ " argument(s) of type(s) " ++
     expected' ++ ", but got " ++ show ngot ++
    " arguments of types " ++ intercalate ", " (map ppType got) ++ "."
    where (nexpected, expected') =
            case expected of
              Left i -> (i, "(polymorphic)")
              Right ts -> (length ts, intercalate ", " $ map ppType ts)
          ngot = length got
          fname' = maybe "anonymous function" (("function "++) . nameToString) fname
  show (UseAfterConsume name rloc wloc) =
    "Variable " ++ textual name ++ " used at " ++ locStr rloc ++
    ", but it was consumed at " ++ locStr wloc ++ ".  (Possibly through aliasing)"
  show (IndexingError name dims got pos) =
    show got ++ " indices given at " ++ locStr pos ++
    ", but type of variable " ++ textual name ++
    " has " ++ show dims ++ " dimension(s)."
  show (BadAnnotation loc desc expected got) =
    "Annotation of \"" ++ desc ++ "\" type of expression at " ++
    locStr loc ++ " is " ++ ppType expected ++
    ", but derived to be " ++ ppType got ++ "."
  show (CurriedConsumption fname loc) =
    "Function " ++ nameToString fname ++
    " curried over a consuming parameter at " ++ locStr loc ++ "."
  show (BadLetWithValue loc) =
    "New value for elements in let-with shares data with source array at " ++
    locStr loc ++ ".  This is illegal, as it prevents in-place modification."
  show (ReturnAliased fname name loc) =
    "Unique return value of function " ++ nameToString fname ++ " at " ++
    locStr loc ++ " is aliased to " ++ textual name ++ ", which is not consumed."
  show (UniqueReturnAliased fname loc) =
    "A unique tuple element of return value of function " ++
    nameToString fname ++ " at " ++ locStr loc ++
    " is aliased to some other tuple component."

type TaggedIdent ty vn = IdentBase ty (ID vn)

type TaggedExp ty vn = ExpBase ty (ID vn)

type TaggedLambda ty vn = LambdaBase ty (ID vn)

type TaggedTupIdent ty vn = TupIdentBase ty (ID vn)

type TaggedFunDec ty vn = FunDecBase ty (ID vn)

type TaggedType vn = TypeBase Names (ID vn)

type TaggedElemType vn = ElemTypeBase Names (ID vn)

-- | A tuple of a return type and a list of argument types.
type FunBinding vn = (DeclTypeBase (ID vn), [DeclTypeBase (ID vn)])

data Binding vn = Bound (TaggedType vn)
                | Consumed SrcLoc

data Usage = Consume SrcLoc
           | Observe SrcLoc
             deriving (Eq, Ord, Show)

data Occurence vn = Occurence { observed :: Names (ID vn)
                              , consumed :: Names (ID vn)
                              , location :: SrcLoc
                              }
             deriving (Eq, Ord, Show)

instance Located (Occurence vn) where
  locOf = locOf . location

observation :: Names (ID vn) -> SrcLoc -> Occurence vn
observation = flip Occurence S.empty

consumption :: Names (ID vn) -> SrcLoc -> Occurence vn
consumption = Occurence S.empty

nullOccurence :: Occurence vn -> Bool
nullOccurence occ = S.null (observed occ) && S.null (consumed occ)

type Occurences vn = [Occurence vn]

type UsageMap vn = M.Map (ID vn) [Usage]

usageMap :: Occurences vn -> UsageMap vn
usageMap = foldl comb M.empty
  where comb m (Occurence obs cons loc) =
          let m' = S.foldl (ins $ Observe loc) m obs
          in S.foldl (ins $ Consume loc) m' cons
        ins v m k = M.insertWith (++) k [v] m

combineOccurences :: ID vn -> Usage -> Usage -> Either (TypeError vn) Usage
combineOccurences _ (Observe loc) (Observe _) = Right $ Observe loc
combineOccurences name (Consume wloc) (Observe rloc) =
  Left $ UseAfterConsume (baseName name) rloc wloc
combineOccurences name (Observe rloc) (Consume wloc) =
  Left $ UseAfterConsume (baseName name) rloc wloc
combineOccurences name (Consume loc1) (Consume loc2) =
  Left $ UseAfterConsume (baseName name) (max loc1 loc2) (min loc1 loc2)

checkOccurences :: Occurences vn -> Either (TypeError vn) ()
checkOccurences = void . T.sequence . M.mapWithKey comb . usageMap
  where comb _    []     = Right []
        comb name (u:us) = (:[]) <$> foldM (combineOccurences name) u us

allConsumed :: Occurences vn -> Names (ID vn)
allConsumed = S.unions . map consumed

seqOccurences :: Occurences vn -> Occurences vn -> Occurences vn
seqOccurences occurs1 occurs2 =
  filter (not . nullOccurence) $ map filt occurs1 ++ occurs2
  where filt occ =
          occ { observed = observed occ S.\\ postcons }
        postcons = allConsumed occurs2

altOccurences :: Occurences vn -> Occurences vn -> Occurences vn
altOccurences occurs1 occurs2 =
  filter (not . nullOccurence) $ map filt occurs1 ++ occurs2
  where filt occ =
          occ { consumed = consumed occ S.\\ postcons
              , observed = observed occ S.\\ postcons }
        postcons = allConsumed occurs2


-- | The 'VarUsage' data structure is used to keep track of which
-- variables have been referenced inside an expression, as well as
-- which variables the resulting expression may possibly alias.
data Dataflow vn = Dataflow {
    usageOccurences :: Occurences vn
  } deriving (Show)

instance VarName vn => Monoid (Dataflow vn) where
  mempty = Dataflow mempty
  Dataflow o1 `mappend` Dataflow o2 =
    Dataflow (o1 ++ o2)

-- | A pair of a variable table and a function table.  Type checking
-- happens with access to this environment.  The function table is
-- only initialised at the very beginning, but the variable table will
-- be extended during type-checking when let-expressions are
-- encountered.
data TypeEnv vn = TypeEnv { envVtable :: M.Map (ID vn) (Binding vn)
                          , envFtable :: M.Map Name (FunBinding vn)
                          , envCheckOccurences :: Bool
                          }

-- | The type checker runs in this monad.  The 'Either' monad is used
-- for error handling.
newtype TypeM vn a = TypeM (RWST
                            (TypeEnv vn)            -- Reader
                            (Dataflow vn)           -- Writer
                            (NameSource (ID vn))    -- State
                            (Either (TypeError vn)) -- Inner monad
                            a)
  deriving (Monad, Functor, Applicative,
            MonadReader (TypeEnv vn),
            MonadWriter (Dataflow vn),
            MonadState (NameSource (ID vn)))

runTypeM :: TypeEnv vn -> NameSource (ID vn) -> TypeM vn a
         -> Either (TypeError vn) a
runTypeM env src (TypeM m) = fst <$> evalRWST m env src

bad :: VarName vn => TypeError vn -> TypeM vn a
bad = TypeM . lift . Left

liftEither :: VarName vn => Either (TypeError vn) a -> TypeM vn a
liftEither = either bad return

-- | Return a fresh, unique name.  The @VName@ is prepended to the
-- name.
new :: VarName vn => String -> TypeM vn (ID vn)
new k = state $ newName $ varName k Nothing

newIdent :: VarName vn => String -> TaggedType vn -> SrcLoc
         -> TypeM vn (TaggedIdent CompTypeBase vn)
newIdent k t loc = Ident <$> new k <*> pure t <*> pure loc

occur :: VarName vn => Occurences vn -> TypeM vn ()
occur occurs = tell Dataflow { usageOccurences = occurs }

-- | Proclaim that we have made read-only use of the given variable.
observe :: VarName vn => TaggedIdent CompTypeBase vn -> TypeM vn ()
observe (Ident _ t loc) = do
  let als = aliases t
  occur [observation als loc]

-- | Proclaim that we have written to the given variable.
consume :: VarName vn => SrcLoc -> Names (ID vn) -> TypeM vn ()
consume loc als = occur [consumption als loc]

-- | Proclaim that we have written to the given variable, and mark
-- accesses to it and all of its aliases as invalid inside the given
-- computation.
consuming :: VarName vn => TaggedIdent CompTypeBase vn -> TypeM vn a -> TypeM vn a
consuming (Ident name t loc) m = do
  consume loc $ aliases t
  local consume' m
  where consume' env =
          env { envVtable = M.insert name (Consumed loc) $ envVtable env }

collectDataflow :: VarName vn => TypeM vn a -> TypeM vn (a, Dataflow vn)
collectDataflow m = pass $ do
  (x, dataflow) <- listen m
  return ((x, dataflow), const mempty)

-- | Information about which parts of a pattern are consumed within
-- its scope.
data Diet = TupleCon [Diet]
          | VarCon
          | NoCon
            deriving (Show)

diet :: TaggedTupIdent CompTypeBase vn -> Occurences vn -> Diet
diet pat occs = diet' pat
  where cons =  allConsumed occs
        diet' (Id k)
          | identName k `S.member` cons = VarCon
          | otherwise                   = NoCon
        diet' (TupId pats _)            = TupleCon $ map diet' pats

maybeCheckOccurences :: VarName vn => Occurences vn -> TypeM vn ()
maybeCheckOccurences us = do
  check <- asks envCheckOccurences
  when check $ liftEither $ checkOccurences us

alternative :: VarName vn => TypeM vn a -> TypeM vn b -> TypeM vn (a,b)
alternative m1 m2 = pass $ do
  (x, Dataflow occurs1) <- listen m1
  (y, Dataflow occurs2) <- listen m2
  maybeCheckOccurences occurs1
  maybeCheckOccurences occurs2
  let usage = Dataflow $ occurs1 `altOccurences` occurs2
  return ((x, y), const usage)

-- | Remove all variable bindings from the vtable inside the given
-- computation.
unbinding :: VarName vn => TypeM vn a -> TypeM vn a
unbinding = local (\env -> env { envVtable = M.empty})

binding :: VarName vn => [TaggedIdent CompTypeBase vn] -> TypeM vn a -> TypeM vn a
binding bnds = check . local (`bindVars` bnds)
  where bindVars :: TypeEnv vn -> [TaggedIdent CompTypeBase vn] -> TypeEnv vn
        bindVars = foldl bindVar

        bindVar :: TypeEnv vn -> TaggedIdent CompTypeBase vn -> TypeEnv vn
        bindVar env (Ident name tp _) =
          let inedges = aliases tp
              update k (Bound tp')
              -- If 'name' is tuple-typed, don't alias the components
              -- to 'name', because tuples have no identity beyond
              -- their components.
                | Elem (Tuple _) <- tp = Bound tp'
                | k `S.member` inedges = Bound (tp' `addAliases` S.insert name)
                | otherwise            = Bound tp'
              update _ b = b
          in env { envVtable = M.insert name (Bound tp) $
                               M.mapWithKey update $
                               envVtable env }

        -- Check whether the bound variables have been used correctly
        -- within their scope.
        check m = do
          (a, usages) <- collectOccurences m
          maybeCheckOccurences usages
          return a

        -- Collect and remove all occurences in @bnds@.  This relies
        -- on the fact that no variables shadow any other.
        collectOccurences m = pass $ do
          (x, usage) <- listen m
          let (relevant, rest) = split $ usageOccurences usage
          return ((x, relevant), const $ usage { usageOccurences = rest })
          where split = unzip .
                        map (\occ ->
                             let (obs1, obs2) = S.partition (`elem` names) $ observed occ
                                 (con1, con2) = S.partition (`elem` names) $ consumed occ
                             in (occ { observed = obs1, consumed = con1 },
                                 occ { observed = obs2, consumed = con2 }))
                names = map identName bnds

lookupVar :: VarName vn => ID vn -> SrcLoc -> TypeM vn (TaggedType vn)
lookupVar name pos = do
  bnd <- asks $ M.lookup name . envVtable
  case bnd of
    Nothing -> bad $ UnknownVariableError (baseName name) pos
    Just (Bound t) -> return t
    Just (Consumed wloc) -> bad $ UseAfterConsume (baseName name) pos wloc

-- | Determine if two types are identical, ignoring uniqueness.
-- Causes a '(TypeError vn)' if they fail to match, and otherwise returns
-- one of them.
unifyExpTypes :: VarName vn =>
                 TaggedExp CompTypeBase vn
              -> TaggedExp CompTypeBase vn
              -> TypeM vn (TaggedType vn)
unifyExpTypes e1 e2 =
  maybe (bad $ UnifyError (untagExp e1) (untagExp e2)) return $
  unifyKnownTypes (typeOf e1) (typeOf e2)

unifyElemTypes :: Monoid (as (ID vn)) =>
                  ElemTypeBase as (ID vn) -> ElemTypeBase as (ID vn)
               -> Maybe (ElemTypeBase as (ID vn))
unifyElemTypes Int Int = Just Int
unifyElemTypes Char Char = Just Char
unifyElemTypes Bool Bool = Just Bool
unifyElemTypes Real Real = Just Real
unifyElemTypes (Tuple ts1) (Tuple ts2)
  | length ts1 == length ts2 = do
  ts <- zipWithM unifyKnownTypes ts1 ts2
  Just $ Tuple ts
unifyElemTypes _ _ = Nothing

unifyKnownTypes :: Monoid (as (ID vn)) => TypeBase as (ID vn) -> TypeBase as (ID vn)
                -> Maybe (TypeBase as (ID vn))
unifyKnownTypes (Elem t1) (Elem t2) = Elem <$> t1 `unifyElemTypes` t2
unifyKnownTypes (Array t1 ds1 u1 als1) (Array t2 ds2 u2 als2)
  | length ds1 == length ds2 = do
  t <- t1 `unifyElemTypes` t2
  Just $ Array t ds1 (u1 <> u2) (als1 <> als2)
unifyKnownTypes _ _ = Nothing

-- | @checkAnnotation loc s t1 t2@ returns @t2@ if @t1@ contains no
-- type, and otherwise tries to unify them with 'unifyKnownTypes'.  If
-- this fails, a 'BadAnnotation' is raised.
checkAnnotation :: (VarName vn, TypeBox ty) =>
                   SrcLoc -> String -> ty (ID vn) -> TaggedType vn
                -> TypeM vn (TaggedType vn)
checkAnnotation loc desc t1 t2 =
  case unboxType t1 of
    Nothing -> return t2
    Just t1' -> case unifyKnownTypes (t1' `setAliases` S.empty) t2 of
                  Nothing -> bad $ BadAnnotation loc desc t1' t2
                  Just t  -> return t

-- | @require ts e@ causes a '(TypeError vn)' if @typeOf e@ does not unify
-- with one of the types in @ts@.  Otherwise, simply returns @e@.
-- This function is very useful in 'checkExp'.
require :: VarName vn => [TaggedType vn] -> TaggedExp CompTypeBase vn -> TypeM vn (TaggedExp CompTypeBase vn)
require ts e
  | any (typeOf e `similarTo`) ts = return e
  | otherwise = bad $ UnexpectedType (untagExp e) $ map toDecl ts

rowTypeM :: VarName vn => TaggedExp CompTypeBase vn -> TypeM vn (TaggedType vn)
rowTypeM e = maybe wrong return $ peelArray 1 $ typeOf e
  where wrong = bad $ TypeError (srclocOf e) $ "Type of expression is not array, but " ++ ppType (typeOf e) ++ "."

-- | Type check a program containing arbitrary type information,
-- yielding either a type error or a program with complete type
-- information.
checkProg :: (TypeBox ty, VarName vn) =>
             ProgBase ty vn -> Either (TypeError vn) (ProgBase CompTypeBase vn)
checkProg = checkProg' True

-- | As 'checkProg', but don't check whether uniqueness constraints
-- are being upheld.  The uniqueness of types must still be correct.
checkProgNoUniqueness :: (VarName vn, TypeBox ty) =>
                         ProgBase ty vn -> Either (TypeError vn) (ProgBase CompTypeBase vn)
checkProgNoUniqueness = checkProg' False

checkProg' :: (VarName vn, TypeBox ty) =>
              Bool -> ProgBase ty vn -> Either (TypeError vn) (ProgBase CompTypeBase vn)
checkProg' checkoccurs prog = do
  ftable <- buildFtable
  let typeenv = TypeEnv { envVtable = M.empty
                        , envFtable = ftable
                        , envCheckOccurences = checkoccurs
                        }

  liftM (untagProg . Prog) $
        runTypeM typeenv src $ mapM checkFun $ progFunctions prog'
  where
    (prog', src) = tagProg' prog
    -- To build the ftable we loop through the list of function
    -- definitions.  In addition to the normal ftable information
    -- (name, return type, argument types), we also keep track of
    -- position information, in order to report both locations of
    -- duplicate function definitions.  The position information is
    -- removed at the end.
    buildFtable = M.map rmLoc <$> foldM expand builtins (progFunctions prog')
    expand ftable (name,ret,args,_,pos)
      | Just (_,_,pos2) <- M.lookup name ftable =
        Left $ DupDefinitionError name pos pos2
      | otherwise =
        let argtypes = map (toDecl . identType) args -- Throw away argument names.
        in Right $ M.insert name (ret,argtypes,pos) ftable
    rmLoc (ret,args,_) = (ret,args)
    builtins = M.mapKeys nameFromString $
               M.fromList [("toReal", (Elem Real, [Elem Int], noLoc))
                          ,("trunc", (Elem Int, [Elem Real], noLoc))
                          ,("sqrt", (Elem Real, [Elem Real], noLoc))
                          ,("log", (Elem Real, [Elem Real], noLoc))
                          ,("exp", (Elem Real, [Elem Real], noLoc))
                          ,("op not", (Elem Bool, [Elem Bool], noLoc))]

checkFun :: (TypeBox ty, VarName vn) =>
            TaggedFunDec ty vn -> TypeM vn (TaggedFunDec CompTypeBase vn)
checkFun (fname, rettype, params, body, loc) = do
  params' <- checkParams
  (body', _) <-
    collectDataflow $ binding (map fromParam params') $ checkExp body

  checkReturnAlias $ typeOf body'

  if toDecl (typeOf body') `subtypeOf` rettype then
    return (fname, rettype, params', body', loc)
  else bad $ ReturnTypeError loc fname rettype $ toDecl $ typeOf body'

  where checkParams = reverse <$> foldM expand [] params

        expand params' ident@(Ident pname _ _)
          | Just _ <- find ((==identName ident) . identName) params' =
            bad $ DupParamError fname (baseName pname) loc
          | otherwise =
            return $ ident : params'

        notAliasingParam names =
          forM_ params $ \p ->
            when (not (unique $ identType p) &&
                  identName p `S.member` names) $
              bad $ ReturnAliased fname (baseName $ identName p) loc

        -- | Check that unique return values do not alias a
        -- non-consumed parameter.
        checkReturnAlias =
          foldM_ checkReturnAlias' S.empty . returnAliasing rettype

        checkReturnAlias' seen (Unique, names)
          | any (`S.member` S.map snd seen) $ S.toList names =
            bad $ UniqueReturnAliased fname loc
          | otherwise = do
            notAliasingParam names
            return $ seen `S.union` tag Unique names
        checkReturnAlias' seen (Nonunique, names)
          | any (`S.member` seen) $ S.toList $ tag Unique names =
            bad $ UniqueReturnAliased fname loc
          | otherwise = return $ seen `S.union` tag Nonunique names

        tag u = S.map $ \name -> (u, name)

        returnAliasing (Elem (Tuple ets1)) (Elem (Tuple ets2)) =
          concat $ zipWith returnAliasing ets1 ets2
        returnAliasing expected got = [(uniqueness expected, aliases got)]

checkExp :: (TypeBox ty, VarName vn) =>
             TaggedExp ty vn -> TypeM vn (TaggedExp CompTypeBase vn)

checkExp (Literal val pos) =
  Literal <$> checkLiteral pos val <*> pure pos

checkExp (TupLit es pos) = do
  es' <- mapM checkExp es
  let res = TupLit es' pos
  return $ fromMaybe res (Literal <$> expToValue res <*> pure pos)

checkExp (ArrayLit es t loc) = do
  es' <- mapM checkExp es
  -- Find the universal type of the array arguments.
  et <- case es' of
          [] -> bad $ TypeError loc "Empty array literal"
          e:es'' ->
            let check elemt eleme
                  | Just elemt' <- elemt `unifyKnownTypes` typeOf eleme =
                    return elemt'
                  | otherwise =
                    bad $ TypeError loc $ ppExp eleme ++ " is not of expected type " ++ ppType elemt ++ "."
            in foldM check (typeOf e) es''

  -- Unify that type with the one given for the array literal.
  t' <- checkAnnotation loc "array-element" t et

  let lit = ArrayLit es' t' loc
  return $ fromMaybe lit (Literal <$> expToValue lit <*> pure loc)

checkExp (BinOp op e1 e2 t pos) = checkBinOp op e1 e2 t pos

checkExp (And e1 e2 pos) = do
  e1' <- require [Elem Bool] =<< checkExp e1
  e2' <- require [Elem Bool] =<< checkExp e2
  return $ And e1' e2' pos

checkExp (Or e1 e2 pos) = do
  e1' <- require [Elem Bool] =<< checkExp e1
  e2' <- require [Elem Bool] =<< checkExp e2
  return $ Or e1' e2' pos

checkExp (Not e pos) = do
  e' <- require [Elem Bool] =<< checkExp e
  return $ Not e' pos

checkExp (Negate e t pos) = do
  e' <- require [Elem Int, Elem Real] =<< checkExp e
  t' <- checkAnnotation pos "result" t $ typeOf e'
  return $ Negate e' t' pos

checkExp (If e1 e2 e3 t pos) = do
  e1' <- require [Elem Bool] =<< checkExp e1
  (e2', e3') <- checkExp e2 `alternative` checkExp e3
  bt <- checkAnnotation pos "result" t =<< unifyExpTypes e2' e3'
  return $ If e1' e2' e3' bt pos

checkExp (Var ident) = do
  ident' <- checkIdent ident
  observe ident'
  return $ Var ident'

checkExp (Apply fname args t pos)
  | "trace" <- nameToString fname =
  case args of
    [e] -> do
      e' <- checkExp e
      t' <- checkAnnotation pos "return" t $ typeOf e'
      return $ Apply fname [e'] t' pos
    _ -> bad $ TypeError pos "Trace function takes a single parameter"

checkExp (Apply fname args t pos)
  | "assertZip" <- nameToString fname = do
  args' <- mapM checkExp args
  mapM_ rowTypeM args'
  t' <- checkAnnotation pos "return" t $ Elem Bool
  return $ Apply fname args' t' pos

checkExp (Apply fname args rettype loc) = do
  bnd <- asks $ M.lookup fname . envFtable
  case bnd of
    Nothing -> bad $ UnknownFunctionError fname loc
    Just (ftype, paramtypes) -> do
      (args', argflows) <- unzip <$> mapM checkArg args

      rettype' <- checkAnnotation loc "return" rettype $
                  returnType ftype $ map typeOf args'

      checkFuncall (Just fname) loc paramtypes (toDecl rettype') argflows

      return $ Apply fname args' rettype' loc

checkExp (LetPat pat e body pos) = do
  (e', dataflow) <- collectDataflow $ checkExp e
  (scope, pat') <- checkBinding pat (typeOf e') dataflow
  scope $ do
    body' <- checkExp body
    return $ LetPat pat' e' body' pos

checkExp (LetWith (Ident dest destt destpos) src idxes ve body pos) = do
  src' <- checkIdent src
  idxes' <- mapM (require [Elem Int] <=< checkExp) idxes
  destt' <- checkAnnotation pos "source" destt $ identType src' `setAliases` S.empty
  let dest' = Ident dest destt' destpos

  unless (unique (identType src') || basicType (identType src')) $
    bad $ TypeError pos $ "Source '" ++ textual (baseName $ identName src) ++ "' is not unique"

  case peelArray (length idxes) (identType src') of
    Nothing -> bad $ IndexingError (baseName $ identName src)
                     (arrayDims $ identType src') (length idxes) (srclocOf src)
    Just elemt ->
      sequentially (require [elemt] =<< checkExp ve) $ \ve' _ -> do
        when (identName src `S.member` aliases (typeOf ve')) $
          bad $ BadLetWithValue pos
        (scope, _) <- checkBinding (Id dest') destt' mempty
        body' <- consuming src' $ scope $ checkExp body
        return $ LetWith dest' src' idxes' ve' body' pos

checkExp (Index ident idxes restype pos) = do
  ident' <- checkIdent ident
  observe ident'
  vt <- lookupVar (identName ident') pos
  case peelArray (length idxes) vt of
    Nothing -> bad $ IndexingError (baseName $ identName ident)
                     (arrayDims vt) (length idxes) pos
    Just et -> do
      restype' <- checkAnnotation pos "indexing result" restype et
      idxes' <- mapM (require [Elem Int] <=< checkExp) idxes
      return $ Index ident' idxes' restype' pos

checkExp (Iota e pos) = do
  e' <- require [Elem Int] =<< checkExp e
  return $ Iota e' pos

checkExp (Shape e pos) = do
  e' <- checkExp e
  case typeOf e' of
    Array {} -> return $ Shape e' pos
    _        -> bad $ TypeError pos "Argument to shape must be array."

checkExp (Replicate countexp valexp pos) = do
  countexp' <- require [Elem Int] =<< checkExp countexp
  valexp' <- checkExp valexp
  return $ Replicate countexp' valexp' pos

checkExp (Reshape shapeexps arrexp pos) = do
  shapeexps' <- mapM (require [Elem Int] <=< checkExp) shapeexps
  arrexp' <- checkExp arrexp
  return (Reshape shapeexps' arrexp' pos)

checkExp (Transpose arrexp pos) = do
  arrexp' <- checkExp arrexp
  when (arrayDims (typeOf arrexp') < 2) $
    bad $ TypeError pos "Argument to transpose does not have two dimensions."
  return $ Transpose arrexp' pos

checkExp (Zip arrexps pos) = do
  arrexps' <- mapM (checkExp . fst) arrexps
  inelemts <- mapM rowTypeM arrexps'
  inelemts' <- zipWithM (checkAnnotation pos "operand element") (map snd arrexps) inelemts
  return $ Zip (zip arrexps' inelemts') pos

checkExp (Unzip e _ pos) = do
  e' <- checkExp e
  case peelArray 1 $ typeOf e' of
    Just (Elem (Tuple ts)) -> return $ Unzip e' ts pos
    _ -> bad $ TypeError pos $ "Argument to unzip is not an array of tuples, but " ++ ppType (typeOf e') ++ "."

checkExp (Map fun arrexp intype pos) = do
  (arrexp', arg@(rt, _, _)) <- checkSOACArrayArg arrexp
  fun' <- checkLambda fun [arg]
  intype' <- checkAnnotation pos "input element" intype rt
  return (Map fun' arrexp' intype' pos)

checkExp (Reduce fun startexp arrexp intype pos) = do
  (startexp', startarg) <- checkArg startexp
  (arrexp', arrarg@(inrowt, _, _)) <- checkSOACArrayArg arrexp
  intype' <- checkAnnotation pos "element" intype inrowt
  fun' <- checkLambda fun [startarg, arrarg]
  let redtype = lambdaType fun' [typeOf startexp', typeOf arrexp']
  unless (redtype `subtypeOf` typeOf startexp') $
    bad $ TypeError pos $ "Initial value is of type " ++ ppType (typeOf startexp') ++ ", but scan function returns type " ++ ppType redtype ++ "."
  unless (redtype `subtypeOf` intype') $
    bad $ TypeError pos $ "Array element value is of type " ++ ppType intype' ++ ", but scan function returns type " ++ ppType redtype ++ "."
  return $ Reduce fun' startexp' arrexp' intype' pos

checkExp (Scan fun startexp arrexp intype pos) = do
  (startexp', startarg) <- checkArg startexp
  (arrexp', arrarg@(inrowt, _, _)) <- checkSOACArrayArg arrexp
  intype' <- checkAnnotation pos "element" intype inrowt
  fun' <- checkLambda fun [startarg, arrarg]
  let scantype = lambdaType fun' [typeOf startexp', typeOf arrexp']
  unless (scantype `subtypeOf` typeOf startexp') $
    bad $ TypeError pos $ "Initial value is of type " ++ ppType (typeOf startexp') ++ ", but scan function returns type " ++ ppType scantype ++ "."
  unless (scantype `subtypeOf` intype') $
    bad $ TypeError pos $ "Array element value is of type " ++ ppType intype' ++ ", but scan function returns type " ++ ppType scantype ++ "."
  return $ Scan fun' startexp' arrexp' intype' pos

checkExp (Filter fun arrexp rowtype pos) = do
  (arrexp', arrarg@(rowelemt, _, _)) <- checkSOACArrayArg arrexp
  rowtype' <- checkAnnotation pos "row" rowtype rowelemt
  fun' <- checkLambda fun [arrarg]
  when (lambdaType fun' [rowtype'] /= Elem Bool) $
    bad $ TypeError pos "Filter function does not return bool."
  return $ Filter fun' arrexp' rowtype' pos

checkExp (Mapall fun arrexp pos) = do
  (arrexp', (_, dflow, argloc)) <- checkSOACArrayArg arrexp
  let arg = (Elem $ elemType $ typeOf arrexp', dflow, argloc)
  fun' <- checkLambda fun [arg]
  return $ Mapall fun' arrexp' pos

checkExp (Redomap redfun mapfun accexp arrexp intype pos) = do
  (accexp', accarg) <- checkArg accexp
  (arrexp', arrarg@(rt, _, _)) <- checkSOACArrayArg arrexp
  (mapfun', maparg) <- checkLambdaArg mapfun [arrarg]
  redfun' <- checkLambda redfun [accarg, maparg]
  let redtype = lambdaType redfun' [typeOf accexp', typeOf arrexp']
  _ <- require [redtype] accexp'
  intype' <- checkAnnotation pos "input element" intype rt
  return $ Redomap redfun' mapfun' accexp' arrexp' intype' pos

checkExp (Split splitexp arrexp intype pos) = do
  splitexp' <- require [Elem Int] =<< checkExp splitexp
  arrexp' <- checkExp arrexp
  et <- rowTypeM arrexp'
  intype' <- checkAnnotation pos "element" intype et
  return $ Split splitexp' arrexp' intype' pos

checkExp (Concat arr1exp arr2exp pos) = do
  arr1exp' <- checkExp arr1exp
  arr2exp' <- require [typeOf arr1exp'] =<< checkExp arr2exp
  _ <- rowTypeM arr2exp' -- Just check that it's an array.
  return $ Concat arr1exp' arr2exp' pos

checkExp (Copy e pos) = do
  e' <- checkExp e
  return $ Copy e' pos

-- Checking of loops is done by synthesing the (almost) equivalent
-- function and type-checking a call to it.  The difficult part is
-- assigning uniqueness attributes to the parameters of the function -
-- we'll do this by inspecting the loop body, and look at which of the
-- variables in mergepat are actually consumed.  Also, any variables
-- that are free in the loop body must be passed along as (non-unique)
-- parameters to the function.
checkExp (DoLoop mergepat mergeexp (Ident loopvar _ _)
          boundexp loopbody letbody loc) = do
  -- First, check the bound and initial merge expression and throw
  -- away the dataflow.  The dataflow will be reconstructed later, but
  -- we need the result of this to synthesize the function.
  ((boundexp', mergeexp'), _) <-
    collectDataflow $ do boundexp' <- require [Elem Int] =<< checkExp boundexp
                         mergeexp' <- checkExp mergeexp
                         return (boundexp', mergeexp')
  let iparam = Ident loopvar (Elem Int) loc

  -- Check the loop body.  We tap the dataflow before leaving scope,
  -- so we'll be able to see occurences of variables bound by
  -- mergepat.
  (firstscope, mergepat') <- checkBinding mergepat (typeOf mergeexp') mempty
  (loopbody', loopflow) <- firstscope $
                           collectDataflow $ binding [iparam] $ checkExp loopbody

  -- We can use the name generator in a slightly hacky way to generate a
  -- Name for the function.
  fname <- nameFromString . textual <$> new "loop_fun"
  pname <- new "merge_val"
  bound <- newIdent "loop_bound" (Elem Int) loc

  let -- | Change the uniqueness attribute of a type to reflect how it
      -- was used.
      param (Array et sz _ _) con = Array et sz u NoInfo
        where u = case con of VarCon     -> Unique
                              TupleCon _ -> Unique
                              NoCon      -> Nonunique
      param (Elem (Tuple ts)) (TupleCon cons) =
        Elem $ Tuple $ zipWith param ts cons
      param t _ = t `setAliases` NoInfo

      -- We use the type of the merge expression, but with uniqueness
      -- attributes reflected to show how the parts of the merge
      -- pattern are used - if something was consumed, it has to be a
      -- unique parameter to the function.
      rettype = param (typeOf mergeexp') $
                diet mergepat' $ usageOccurences loopflow

      boundnames = S.insert loopvar $ patNames mergepat
      unbound ident = not $ identName ident `S.member` boundnames
      ununique ident =
        ident { identType = param (identType ident) NoCon }
      -- Find the free variables of the loop body.
      free = map ununique $ filter unbound $ S.toList $
             freeInExp loopbody'

      merge  = Ident pname (fromDecl rettype) $ srclocOf mergeexp'

      -- These are the parameters expected by the function: All of the
      -- free variables, followed by the index, followed by the upper
      -- bound (currently not used), followed by the merge value.
      params = map toParam free ++
               [iparam, toParam bound, toParam merge]
      bindfun env = env { envFtable = M.insert fname
                                      (rettype, map identType params) $
                                      envFtable env }

      -- The body of the function will be the loop body, but with all
      -- tails replaced with recursive calls.
      recurse e = Apply fname (map (Var . fromParam) free ++
                               [Var iparam, Var bound, e])
                  (fromDecl rettype) (srclocOf e)
      funbody' = LetPat mergepat' (Var merge) (mapTails recurse loopbody')
                 (srclocOf loopbody')

  (funcall, callflow) <- collectDataflow $ local bindfun $ do
    -- Check that the function is internally consistent.
    _ <- unbinding $ checkFun (fname, rettype, params, funbody', loc)
    -- Check the actual function call - we start by computing the
    -- bound and initial merge value, in case they use something
    -- consumed in the call.  This reintroduces the dataflow for
    -- boundexp and mergeexp that we previously threw away.
    checkExp $ LetPat (Id bound) boundexp'
                (LetPat (Id merge) mergeexp'
                 (Apply fname (map (Var . fromParam) free ++
                               [Literal (IntVal 0) loc, Var bound, Var merge])
                        (fromDecl rettype) $ srclocOf mergeexp)
                 (srclocOf mergeexp))
                (srclocOf mergeexp)

  -- Now we just need to bind the result of the function call to the
  -- original merge pattern...
  (secondscope, _) <- checkBinding mergepat (typeOf funcall) callflow

  -- And then check the let-body.
  secondscope $ do
    letbody' <- checkExp letbody
    return $ DoLoop mergepat' mergeexp'
                    (Ident loopvar (Elem Int) loc) boundexp'
                    loopbody' letbody' loc

----------------------------------------------
---- BEGIN Cosmin added SOAC2 combinators ----
----------------------------------------------
checkExp (Map2 fun arrexps intype pos) = do
  (arrexps', arrargs) <- unzip <$> mapM checkSOACArrayArg arrexps
  ineltp  <- soac2ArgType pos "Map2" $ map argType arrargs
  fun'    <- checkLambda fun arrargs
  intype' <- checkAnnotation pos "input element" intype ineltp
  return $ Map2 fun' arrexps' intype' pos

checkExp (Reduce2 fun startexps arrexps intype pos) = do
  (startexps', startargs) <- unzip <$> mapM checkArg startexps
  startt <- soac2ArgType pos "Reduce2" $ map typeOf startexps'
  (arrexps', arrargs) <- unzip <$> mapM checkSOACArrayArg arrexps
  ineltp  <- soac2ArgType pos "Reduce2" $ map argType arrargs
  intype' <- checkAnnotation pos "input element" intype ineltp
  fun'    <- checkLambda fun $ startargs ++ arrargs
  let funret = lambdaType fun' $ map argType $ startargs ++ arrargs
  unless (funret `subtypeOf` startt) $
    bad $ TypeError pos $ "Accumulator is of type " ++ ppType startt ++
                          ", but reduce function returns type " ++ ppType funret ++ "."
  return $ Reduce2 fun' startexps' arrexps' intype' pos

checkExp (Scan2 fun startexps arrexps intype pos) = do
  (startexps', startargs) <- unzip <$> mapM checkArg startexps
  startt <- soac2ArgType pos "Scan2" $ map typeOf startexps'
  (arrexps', arrargs)   <- unzip <$> mapM checkSOACArrayArg arrexps
  inelemt   <- soac2ArgType pos "Scan2" $ map argType arrargs
  intype'   <- checkAnnotation pos "element" intype inelemt
  fun'      <- checkLambda fun $ startargs ++ startargs
  let funret = lambdaType fun' $ map argType $ startargs ++ startargs
  unless (funret `subtypeOf` startt) $
    bad $ TypeError pos $ "Initial value is of type " ++ ppType startt ++
                          ", but scan function returns type " ++ ppType funret ++ "."
  unless (funret `subtypeOf` intype') $
    bad $ TypeError pos $ "Array element value is of type " ++ ppType intype' ++
                          ", but scan function returns type " ++ ppType funret ++ "."
  return $ Scan2 fun' startexps' arrexps' intype' pos

checkExp (Filter2 fun arrexps pos) = do
  (arrexps', arrargs) <- unzip <$> mapM checkSOACArrayArg arrexps
  fun' <- checkLambda fun arrargs
  let funret = lambdaType fun' $ map argType arrargs
  when (funret /= Elem Bool) $
    bad $ TypeError pos "Filter function does not return bool."
  return $ Filter2 fun' arrexps' pos

checkExp (Mapall2 fun arrexps pos) = do
  (arrexps', arrargs) <- unzip <$> mapM checkSOACArrayArg arrexps

  arrargs' <-
    case arrargs of
      []   -> bad $ TypeError pos "Empty tuple given to Mapall2"
      a:as ->
        let mindim = foldl min (arrayDims $ argType a) $
                     map (arrayDims . argType) as
        in forM (a:as) $ \(t, dflow, argloc) ->
             case peelArray mindim t of
               Nothing -> bad $ TypeError argloc "Array of smaller rank than others in mapall2"
               Just t' -> return (t', dflow, argloc)

  fun' <- checkLambda fun arrargs'
  return $ Mapall2 fun' arrexps' pos

checkExp (Redomap2 redfun mapfun accexps arrexps intype pos) = do
  (arrexps', arrargs) <- unzip <$> mapM checkSOACArrayArg arrexps
  (mapfun', maparg) <- checkLambdaArg mapfun arrargs

  (accexps', accargs) <- unzip <$> mapM checkArg accexps
  acct <- soac2ArgType pos "Redomap2" $ map typeOf accexps'
  maparg' <- splitTupleArg maparg
  redfun' <- checkLambda redfun $ accargs ++ maparg'
  let redret = lambdaType redfun' $ map argType $ accargs ++ maparg'
  unless (redret `subtypeOf` acct) $
    bad $ TypeError pos $ "Initial value is of type " ++ ppType acct ++
                          ", but redomap2 reduction returns type " ++ ppType redret ++ "."

  et <- soac2ArgType pos "Redomap2" $ map argType arrargs
  intype' <- checkAnnotation pos "input element" intype et
  return $ Redomap2 redfun' mapfun' accexps' arrexps' intype' pos

soacArrayArg :: VarName vn => Arg vn -> TypeM vn (Arg vn)
soacArrayArg (t, dflow, argloc) =
  case peelArray 1 t of
    Nothing -> bad $ TypeError argloc "SOAC argument is not an array"
    Just rt -> return (rt, dflow, argloc)

checkSOACArrayArg :: (TypeBox ty, VarName vn) =>
                     TaggedExp ty vn -> TypeM vn (TaggedExp CompTypeBase vn, Arg vn)
checkSOACArrayArg e = do
  (e', arg) <- checkArg e
  arg' <- soacArrayArg arg
  return (e', arg')

splitTupleArg :: VarName vn => Arg vn -> TypeM vn [Arg vn]
splitTupleArg (Elem (Tuple ts), dflow, loc) = do
  maybeCheckOccurences $ usageOccurences dflow
  return [ (t, mempty, loc) | t <- ts ]
splitTupleArg arg = return [arg]

soac2ArgType :: VarName vn =>
                SrcLoc -> String -> [TaggedType vn] -> TypeM vn (TaggedType vn)
soac2ArgType loc op [] = bad $ TypeError loc $ "Empty tuple given to " ++ op
soac2ArgType _ _ [et] = return et
soac2ArgType _ _ ets = return $ Elem $ Tuple ets

checkLiteral :: VarName vn => SrcLoc -> Value -> TypeM vn Value
checkLiteral _ (IntVal k) = return $ IntVal k
checkLiteral _ (RealVal x) = return $ RealVal x
checkLiteral _ (LogVal b) = return $ LogVal b
checkLiteral _ (CharVal c) = return $ CharVal c
checkLiteral loc (TupVal vals) = do
  vals' <- mapM (checkLiteral loc) vals
  return $ TupVal vals'
checkLiteral loc (ArrayVal arr rt) = do
  vals <- mapM (checkLiteral loc) (elems arr)
  case find ((/=rt) . removeNames . valueType) vals of
    Just wrong -> bad $ TypeError loc $ ppValue wrong ++ " is not of expected type " ++ ppType rt ++ "."
    _          -> return ()
  return $ ArrayVal (listArray (bounds arr) vals) rt

checkIdent :: (TypeBox ty, VarName vn) =>
              TaggedIdent ty vn -> TypeM vn (TaggedIdent CompTypeBase vn)
checkIdent (Ident name t pos) = do
  vt <- lookupVar name pos
  t' <- checkAnnotation pos ("variable " ++ textual (baseName name)) t vt
  return $ Ident name t' pos

checkBinOp :: (TypeBox ty, VarName vn) =>
              BinOp -> TaggedExp ty vn -> TaggedExp ty vn -> ty (ID vn) -> SrcLoc
           -> TypeM vn (TaggedExp CompTypeBase vn)
checkBinOp Plus e1 e2 t pos = checkPolyBinOp Plus [Real, Int] e1 e2 t pos
checkBinOp Minus e1 e2 t pos = checkPolyBinOp Minus [Real, Int] e1 e2 t pos
checkBinOp Pow e1 e2 t pos = checkPolyBinOp Pow [Real, Int] e1 e2 t pos
checkBinOp Times e1 e2 t pos = checkPolyBinOp Times [Real, Int] e1 e2 t pos
checkBinOp Divide e1 e2 t pos = checkPolyBinOp Divide [Real, Int] e1 e2 t pos
checkBinOp Mod e1 e2 t pos = checkPolyBinOp Mod [Int] e1 e2 t pos
checkBinOp ShiftR e1 e2 t pos = checkPolyBinOp ShiftR [Int] e1 e2 t pos
checkBinOp ShiftL e1 e2 t pos = checkPolyBinOp ShiftL [Int] e1 e2 t pos
checkBinOp Band e1 e2 t pos = checkPolyBinOp Band [Int] e1 e2 t pos
checkBinOp Xor e1 e2 t pos = checkPolyBinOp Xor [Int] e1 e2 t pos
checkBinOp Bor e1 e2 t pos = checkPolyBinOp Bor [Int] e1 e2 t pos
checkBinOp LogAnd e1 e2 t pos = checkPolyBinOp LogAnd [Bool] e1 e2 t pos
checkBinOp LogOr e1 e2 t pos = checkPolyBinOp LogOr [Bool] e1 e2 t pos
checkBinOp Equal e1 e2 t pos = checkRelOp Equal [Int, Real] e1 e2 t pos
checkBinOp Less e1 e2 t pos = checkRelOp Less [Int, Real] e1 e2 t pos
checkBinOp Leq e1 e2 t pos = checkRelOp Leq [Int, Real] e1 e2 t pos

checkRelOp :: (TypeBox ty, VarName vn) =>
              BinOp -> [TaggedElemType vn]
           -> TaggedExp ty vn -> TaggedExp ty vn
           -> ty (ID vn) -> SrcLoc
           -> TypeM vn (TaggedExp CompTypeBase vn)
checkRelOp op tl e1 e2 t pos = do
  e1' <- require (map Elem tl) =<< checkExp e1
  e2' <- require (map Elem tl) =<< checkExp e2
  _ <- unifyExpTypes e1' e2'
  t' <- checkAnnotation pos "result" t $ Elem Bool
  return $ BinOp op e1' e2' t' pos

checkPolyBinOp :: (TypeBox ty, VarName vn) =>
                  BinOp -> [TaggedElemType vn]
               -> TaggedExp ty vn -> TaggedExp ty vn -> ty (ID vn) -> SrcLoc
               -> TypeM vn (TaggedExp CompTypeBase vn)
checkPolyBinOp op tl e1 e2 t pos = do
  e1' <- require (map Elem tl) =<< checkExp e1
  e2' <- require (map Elem tl) =<< checkExp e2
  t' <- unifyExpTypes e1' e2'
  t'' <- checkAnnotation pos "result" t t'
  return $ BinOp op e1' e2' t'' pos

sequentially :: VarName vn =>
                TypeM vn a -> (a -> Dataflow vn -> TypeM vn b) -> TypeM vn b
sequentially m1 m2 = do
  (a, m1flow) <- collectDataflow m1
  (b, m2flow) <- collectDataflow $ m2 a m1flow
  occur $ usageOccurences m1flow `seqOccurences`
          usageOccurences m2flow
  return b

checkBinding :: (VarName vn, TypeBox ty) =>
                TaggedTupIdent ty vn -> TaggedType vn -> Dataflow vn
             -> TypeM vn (TypeM vn a -> TypeM vn a, TaggedTupIdent CompTypeBase vn)
checkBinding pat et dflow = do
  (pat', (scope, _)) <-
    runStateT (checkBinding' pat et) (id, [])
  return (\m -> sequentially (tell dflow) (const . const $ scope m), pat')
  where checkBinding' (Id (Ident name namet pos)) t = do
          t' <- lift $
                checkAnnotation (srclocOf pat)
                ("binding of variable " ++ textual (baseName name)) namet t
          let t'' = typeOf $ Var $ Ident name t' pos
          add $ Ident name t'' pos
          return $ Id $ Ident name t'' pos
        checkBinding' (TupId pats pos) (Elem (Tuple ts))
          | length pats == length ts = do
          pats' <- zipWithM checkBinding' pats ts
          return $ TupId pats' pos
        checkBinding' _ _ =
          lift $ bad $ InvalidPatternError (untagPattern errpat) (untagType et) $ srclocOf pat

        add ident = do
          bnd <- gets $ find (==ident) . snd
          case bnd of
            Nothing ->
              modify $ \(scope, names) -> (binding [ident] . scope,
                                           ident : names)
            Just (Ident name _ pos2) ->
              lift $ bad $ DupPatternError (baseName name) (srclocOf ident) pos2
        -- A pattern with known type box (unit) for error messages.
        errpat = rmTypes pat
        rmTypes (Id (Ident name _ pos)) = Id $ Ident name NoInfo pos
        rmTypes (TupId pats pos) = TupId (map rmTypes pats) pos

validApply :: [DeclTypeBase (ID vn)] -> [TaggedType vn] -> Bool
validApply expected got =
  length got == length expected &&
  all id (zipWith subtypeOf (map toDecl got) expected)

type Arg vn = (TaggedType vn, Dataflow vn, SrcLoc)

argType :: Arg vn -> TaggedType vn
argType (t, _, _) = t

checkArg :: (TypeBox ty, VarName vn) =>
            TaggedExp ty vn -> TypeM vn (TaggedExp CompTypeBase vn, Arg vn)
checkArg arg = do (arg', dflow) <- collectDataflow $ checkExp arg
                  return (arg', (typeOf arg', dflow, srclocOf arg'))

checkLambdaArg :: (TypeBox ty, VarName vn) =>
                  TaggedLambda ty vn -> [Arg vn]
               -> TypeM vn (TaggedLambda CompTypeBase vn, Arg vn)
checkLambdaArg lam args = do
  (lam', dflow) <- collectDataflow $ checkLambda lam args
  let lamt = lambdaType lam' $ map argType args
  return (lam', (lamt, dflow, srclocOf lam'))

checkFuncall :: VarName vn =>
                Maybe Name -> SrcLoc
             -> [DeclTypeBase (ID vn)] -> DeclTypeBase (ID vn) -> [Arg vn]
             -> TypeM vn ()
checkFuncall fname loc paramtypes _ args = do
  let argts = map argType args

  unless (validApply paramtypes argts) $
    bad $ ParameterMismatch fname loc
          (Right $ map untagType paramtypes) (map untagType argts)

  forM_ (zip paramtypes args) $ \(paramt, (t, dflow, argloc)) -> do
    maybeCheckOccurences $ usageOccurences dflow
    let occurs = [consumption (consumeArg (aliases t) paramt) argloc]
    occur $ usageOccurences dflow `seqOccurences` occurs
  where -- Check if an argument of the given type consumes anything.
        consumes (Array _ _ Unique _) = True
        consumes (Elem (Tuple ets)) = any consumes ets
        consumes _ = False

        consumeArg names (Elem (Tuple ets)) =
          mconcat $ map (consumeArg names) ets
        consumeArg names t
          | consumes t = names
          | otherwise  = S.empty

checkLambda :: (TypeBox ty, VarName vn) =>
               TaggedLambda ty vn -> [Arg vn] -> TypeM vn (TaggedLambda CompTypeBase vn)
checkLambda (AnonymFun params body ret pos) args = do
  (_, ret', params', body', _) <-
    checkFun (nameFromString "<anonymous>", ret, params, body, pos)
  case () of
    _ | length params' == length args -> do
          checkFuncall Nothing pos (map identType params') ret' args
          return $ AnonymFun params body' ret' pos
      | [(Elem (Tuple ets), _, _)] <- args,
        validApply (map identType params) ets -> do
          -- The function expects N parmeters, but the argument is a
          -- single N-tuple whose types match the parameters.
          -- Generate a shim to make it fit.
          tupparam <- newIdent "tup_shim" (Elem (Tuple $ map (fromDecl . identType) params)) pos
          let tupfun = AnonymFun [toParam tupparam] tuplet ret pos
              tuplet = LetPat (TupId (map (Id . fromParam) params) pos)
                              (Var tupparam) body' pos
          checkLambda tupfun args
      | otherwise -> bad $ TypeError pos $ "Anonymous function defined with " ++ show (length params) ++ " parameters, but expected to take " ++ show (length args) ++ " arguments."

checkLambda (CurryFun fname [] rettype pos) [arg]
  | "op ~" <- nameToString fname = do
  rettype' <- checkAnnotation pos "return" rettype $ argType arg
  var <- newIdent "x" (argType arg) pos
  let lam = AnonymFun [toParam var]
            (Negate (Var var) (argType arg) pos) (toDecl rettype') pos
  checkLambda lam [arg]

checkLambda (CurryFun opfun curryargexps rettype pos) args
  | Just op <- lookup (nameToString opfun) ops =
  checkPolyLambdaOp op curryargexps rettype args pos
  where ops = map (\op -> ("op " ++ opStr op, op)) [minBound..maxBound]

checkLambda (CurryFun fname curryargexps rettype pos) args = do
  (curryargexps', curryargs) <- unzip <$> mapM checkArg curryargexps
  bnd <- asks $ M.lookup fname . envFtable
  case bnd of
    Nothing -> bad $ UnknownFunctionError fname pos
    Just (rt, paramtypes) -> do
      rettype' <- checkAnnotation pos "return" rettype $ fromDecl rt
      case () of
        _ | [(tupt@(Elem (Tuple ets)), _, _)] <- args,
            validApply paramtypes ets -> do
              -- Same shimming as in the case for anonymous functions,
              -- although we don't have to worry about name shadowing
              -- here.
              let mkparam i t = newIdent ("param_" ++ show i) t pos
              params <- zipWithM mkparam [(0::Int)..] $ map fromDecl paramtypes
              tupparam <- newIdent "x" tupt pos
              let tuplet = LetPat (TupId (map Id params) pos) (Var tupparam) body pos
                  tupfun = AnonymFun [toParam tupparam] tuplet (toDecl rettype') pos
                  body = Apply fname (map Var params) rettype' pos
              checkLambda tupfun args
          | otherwise -> do
              case find (unique . snd) $ zip curryargexps paramtypes of
                Just (e, _) -> bad $ CurriedConsumption fname $ srclocOf e
                _           -> return ()
              checkFuncall Nothing pos paramtypes rt (curryargs++args)
              return $ CurryFun fname curryargexps' rettype' pos

checkPolyLambdaOp :: (TypeBox ty, VarName vn) =>
                     BinOp -> [TaggedExp ty vn] -> ty (ID vn) -> [Arg vn] -> SrcLoc
                  -> TypeM vn (TaggedLambda CompTypeBase vn)
checkPolyLambdaOp op curryargexps rettype args pos = do
  curryargexpts <- map typeOf <$> mapM checkExp curryargexps
  let argts = [ argt | (argt, _, _) <- args ]
  tp <- case curryargexpts ++ argts of
          [t1, t2] | t1 == t2 -> return t1
          [Elem (Tuple [t1,t2])] | t1 == t2 -> return t1 -- For autoshimming.
          l -> bad $ ParameterMismatch (Just fname) pos (Left 2) $ map untagType l
  xname <- new "x"
  yname <- new "y"
  let xident t = Ident xname t pos
      yident t = Ident yname t pos
  (x,y,params) <- case curryargexps of
                    [] -> return (Var $ xident (boxType tp),
                                  Var $ yident (boxType tp),
                                  [xident tp, yident tp])
                    [e] -> return (e,
                                   Var $ yident $ boxType tp,
                                   [yident tp])
                    (e1:e2:_) -> return (e1, e2, [])
  body <- binding params $ checkBinOp op x y rettype pos
  checkLambda (AnonymFun (map toParam params) body (toDecl $ typeOf body) pos) args
  where fname = nameFromString $ "op " ++ opStr op
