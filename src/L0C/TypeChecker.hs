{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import L0C.Renamer (tagProg', untagProg, untagExp, untagPattern)
import L0C.FreshNames

-- | Information about an error during type checking.  The 'Show'
-- instance for this type produces a human-readable description.
data TypeError vn = TypeError SrcLoc String
                  -- ^ A general error happened at the given position and
                  -- for the given reason.
                  | UnifyError (ExpBase Type vn) (ExpBase Type vn)
                  -- ^ Types of two expressions failed to unify.
                  | UnexpectedType (ExpBase Type vn) [Type]
                  -- ^ Expression of type was not one of the expected
                  -- types.
                  | ReturnTypeError SrcLoc Name Type Type
                  -- ^ The body of a function definition has a different
                  -- type than its declaration.
                  | DupDefinitionError Name SrcLoc SrcLoc
                  -- ^ Two functions have been defined with the same name.
                  | DupParamError Name vn SrcLoc
                  -- ^ Two function parameters share the same name.
                  | DupPatternError vn SrcLoc SrcLoc
                  -- ^ Two pattern variables share the same name.
                  | InvalidPatternError (TupIdentBase () vn) (ExpBase Type vn)
                  -- ^ The pattern is not compatible with the type.
                  | UnknownVariableError vn SrcLoc
                  -- ^ Unknown variable of the given name referenced at the given spot.
                  | UnknownFunctionError Name SrcLoc
                  -- ^ Unknown function of the given name called at the given spot.
                  | ParameterMismatch (Maybe Name) SrcLoc (Either Int [Type]) [Type]
                  -- ^ A function (possibly anonymous) was called with
                  -- invalid arguments.  The third argument is either the
                  -- number of parameters, or the specific types of
                  -- parameters accepted (sometimes, only the former can
                  -- be determined).
                  | UseAfterConsume vn SrcLoc SrcLoc
                  -- ^ A variable was attempted used after being
                  -- consumed.  The last location is the point of
                  -- consumption.
                  | IndexingError Int Int SrcLoc
                  -- ^ Too many indices provided.  The first integer is
                  -- the number of dimensions in the array being
                  -- indexed.
                  | BadAnnotation SrcLoc String Type Type
                  -- ^ One of the type annotations fails to match with the
                  -- derived type.  The string is a description of the
                  -- role of the type.  The last type is the new derivation.
                  | CurriedConsumption Name SrcLoc
                  -- ^ A function is being curried with an argument to be consumed.
                  | BadLetWithValue SrcLoc
                  -- ^ The new value for an array slice in let-with is aliased to the source.
                  | ReturnAliased Name vn SrcLoc
                  | UniqueReturnAliased Name SrcLoc

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
  show (InvalidPatternError pat e) =
    "Pattern " ++ ppTupId pat ++ " at " ++ locStr (srclocOf pat) ++
    " cannot match value of type " ++ ppType (typeOf e) ++ " at " ++ locStr (srclocOf e) ++ "."
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
  show (IndexingError dims got pos) =
    show got ++ " indices given, but type of expression at " ++ locStr pos ++
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

-- | Information about the aliasing of the value returned by an expression.
data Aliases vn = VarAlias (S.Set (ID vn))
                  -- ^ May alias these variables.
                | TupleAlias [Aliases vn]
                  -- ^ Aliasing information for specific components of a
                  -- tuple.
                  deriving (Show)

instance Monoid (Aliases vn) where
  mempty = VarAlias mempty
  VarAlias s1 `mappend` VarAlias s2 =
    VarAlias $ s1 `mappend` s2
  TupleAlias ass1 `mappend` TupleAlias ass2 =
    TupleAlias $ zipWith mappend ass1 ass2
  TupleAlias ass `mappend` as =
    TupleAlias (map (mappend as) ass)
  as `mappend` TupleAlias ass =
    TupleAlias (map (mappend as) ass)

-- | Remove a variable from the alias set (presumably because it has
-- gone out of scope).
unalias :: ID vn -> Aliases vn -> Aliases vn
unalias name (VarAlias names) = VarAlias $ name `S.delete` names
unalias name (TupleAlias alss) = TupleAlias $ map (unalias name) alss

-- | All the variables represented in this aliasing.  Guaranteed not
-- to contain duplicates.
aliased :: Aliases vn -> S.Set (ID vn)
aliased (VarAlias names) = names
aliased (TupleAlias ass) = mconcat $ map aliased ass

-- | Create an aliasing set given a list of names.
varAlias :: [ID vn] -> Aliases vn
varAlias = VarAlias . S.fromList

substituteAliases :: Aliases vn -> M.Map (ID vn) (S.Set (ID vn)) -> Aliases vn
substituteAliases (VarAlias names) m =
  VarAlias $ let substs = names `S.intersection` S.fromList (M.keys m)
             in (names S.\\ substs) `S.union`
                  S.unions (mapMaybe (`M.lookup` m) $ S.toList substs)
substituteAliases (TupleAlias alss) m =
  TupleAlias $ map (`substituteAliases` m) alss

-- | A tuple of a return type and a list of argument types.
type FunBinding = (Type, [Type])

data Binding vn = Bound Type (Aliases vn)
                | Consumed SrcLoc

data Usage = Consume SrcLoc
           | Observe SrcLoc
             deriving (Eq, Ord, Show)

data Occurence vn = Occurence { observed :: S.Set (ID vn)
                              , consumed :: S.Set (ID vn)
                              , location :: SrcLoc
                              }
             deriving (Eq, Ord, Show)

instance Located (Occurence vn) where
  locOf = locOf . location

observation :: S.Set (ID vn) -> SrcLoc -> Occurence vn
observation = flip Occurence S.empty

consumption :: S.Set (ID vn) -> SrcLoc -> Occurence vn
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

allConsumed :: Occurences vn -> S.Set (ID vn)
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
  , usageAliasing :: Aliases vn
  } deriving (Show)

instance Monoid (Dataflow vn) where
  mempty = Dataflow mempty mempty
  Dataflow o1 s1 `mappend` Dataflow o2 s2 =
    Dataflow (o1 ++ o2) (s1 <> s2)

-- | A pair of a variable table and a function table.  Type checking
-- happens with access to this environment.  The function table is
-- only initialised at the very beginning, but the variable table will
-- be extended during type-checking when let-expressions are
-- encountered.
data TypeEnv vn = TypeEnv { envVtable :: M.Map (ID vn) (Binding vn)
                          , envFtable :: M.Map Name FunBinding
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

bad :: TypeError vn -> TypeM vn a
bad = TypeM . lift . Left

liftEither :: Either (TypeError vn) a -> TypeM vn a
liftEither = either bad return

-- | Return a fresh, unique name.  The @VName@ is prepended to the
-- name.
new :: VarName vn => String -> TypeM vn (ID vn)
new k = state $ newName $ varName k Nothing

newIdent :: VarName vn =>
            String -> Type -> SrcLoc -> TypeM vn (TaggedIdent Type vn)
newIdent k t loc = Ident <$> new k <*> pure t <*> pure loc

-- | Proclaim that we have made read-only use of the given variable!
observe :: TaggedIdent Type vn -> TypeM vn ()
observe (Ident name _ loc) = do
  als <- aliases name
  tell $ mempty { usageOccurences = [observation (aliased als) loc]
                , usageAliasing = als }

-- | Proclaim that we have written to the given variable.
consume :: SrcLoc -> Aliases vn -> TypeM vn ()
consume loc als =
  tell $ mempty { usageOccurences = [consumption (aliased als) loc] }

alias :: Aliases vn -> TypeM vn ()
alias al = tell $ mempty { usageAliasing = al }

-- | Proclaim that we have written to the given variable, and mark
-- accesses to it and all of its aliases as invalid inside the given
-- computation.
consuming :: TaggedIdent Type vn -> TypeM vn a -> TypeM vn a
consuming (Ident name _ loc) m = do
  consume loc =<< aliases name
  local consume' m
  where consume' env =
          env { envVtable = M.insert name (Consumed loc) $ envVtable env }

collectAliases :: TypeM vn a -> TypeM vn (a, Aliases vn)
collectAliases m = pass $ do
  (x, usage) <- listen m
  return ((x, usageAliasing usage),
          const $ usage { usageAliasing = blank $ usageAliasing usage })
  where blank (VarAlias _) = VarAlias S.empty
        blank (TupleAlias alss) = TupleAlias $ map blank alss

collectDataflow :: TypeM vn a -> TypeM vn (a, Dataflow vn)
collectDataflow m = pass $ do
  (x, dataflow) <- listen m
  return ((x, dataflow), const mempty)

maybeCheckOccurences :: Occurences vn -> TypeM vn ()
maybeCheckOccurences us = do
  check <- asks envCheckOccurences
  when check $ liftEither $ checkOccurences us

alternative :: TypeM vn a -> TypeM vn b -> TypeM vn (a,b)
alternative m1 m2 = pass $ do
  (x, Dataflow occurs1 als1) <- listen m1
  (y, Dataflow occurs2 als2) <- listen m2
  maybeCheckOccurences occurs1
  maybeCheckOccurences occurs2
  let usage = Dataflow (occurs1 `altOccurences` occurs2) (als1 <> als2)
  return ((x, y), const usage)

aliases :: ID vn -> TypeM vn (Aliases vn)
aliases name = asks $ maybe name' reflexive . M.lookup name . envVtable
  where name' = varAlias [name]
        reflexive (Consumed _) = mempty
        reflexive (Bound _ als)
          | VarAlias _ <- als = als <> name'
          | otherwise         = als

binding' :: [TaggedIdent Type vn] -> TypeM vn a -> TypeM vn a
binding' = binding . (`zip` repeat mempty)

binding :: [(TaggedIdent Type vn, Aliases vn)] -> TypeM vn a -> TypeM vn a
binding bnds = check . local (`bindVars` bnds)
  where bindVars = foldl bindVar

        bindVar env (Ident name tp _, als) =
          let name' = varAlias [name]
              inedges = aliased als
              update k (Bound tp' als')
                | k `S.member` inedges = Bound tp' $ als'' <> name'
                | otherwise            = Bound tp' als''
                where als'' = unalias name als'
              update _ b = b
          in env { envVtable = M.insert name (Bound tp als) $
                               M.mapWithKey update $
                               envVtable env }

        -- Check whether the bound variables have been used correctly
        -- within their scope.
        check m = do
          (a, usages) <- collectOccurences m
          maybeCheckOccurences usages
          return a

        -- Collect and remove all occurences in @bnds@.  Also remove
        -- these names from aliasing information, since they are now
        -- out of scope.  We have to be careful to handle aliasing
        -- properly, however.
        collectOccurences m = pass $ do
          (x, usage) <- listen m
          let (relevant, rest) = split $ usageOccurences usage
              substs = M.fromList [ (identName k, aliased als) | (k, als) <- bnds ]
              newaliases = usageAliasing usage `substituteAliases` substs
          return ((x, relevant),
                  const $ usage { usageOccurences = rest
                                , usageAliasing = newaliases })
          where split = unzip .
                        map (\occ ->
                             let (obs1, obs2) = S.partition (`elem` names) $ observed occ
                                 (con1, con2) = S.partition (`elem` names) $ consumed occ
                             in (occ { observed = obs1, consumed = con1 },
                                 occ { observed = obs2, consumed = con2 }))
                names = map (identName . fst) bnds

lookupVar :: ID vn -> SrcLoc -> TypeM vn Type
lookupVar name pos = do
  bnd <- asks $ M.lookup name . envVtable
  case bnd of
    Nothing              -> bad $ UnknownVariableError (baseName name) pos
    Just (Bound t _)     -> return t
    Just (Consumed wloc) -> bad $ UseAfterConsume (baseName name) pos wloc

-- | Determine if two types are identical, ignoring uniqueness.
-- Causes a '(TypeError vn)' if they fail to match, and otherwise returns
-- one of them.
unifyExpTypes :: VarName vn => TaggedExp Type vn -> TaggedExp Type vn -> TypeM vn Type
unifyExpTypes e1 e2 =
  maybe (bad $ UnifyError (untagExp e1) (untagExp e2)) return $
  unifyKnownTypes (typeOf e1) (typeOf e2)

unifyElemTypes :: ElemType -> ElemType -> Maybe ElemType
unifyElemTypes Int Int = Just Int
unifyElemTypes Char Char = Just Char
unifyElemTypes Bool Bool = Just Bool
unifyElemTypes Real Real = Just Real
unifyElemTypes (Tuple ts1) (Tuple ts2)
  | length ts1 == length ts2 = do
  ts <- zipWithM unifyKnownTypes ts1 ts2
  Just $ Tuple ts
unifyElemTypes _ _ = Nothing

unifyKnownTypes :: Type -> Type -> Maybe Type
unifyKnownTypes (Elem t1) (Elem t2) = Elem <$> t1 `unifyElemTypes` t2
unifyKnownTypes (Array t1 ds1 u1) (Array t2 ds2 u2)
  | length ds1 == length ds2 = do
  t <- unifyElemTypes t1 t2
  Just $ Array t ds1 (u1 <> u2)
unifyKnownTypes _ _ = Nothing

-- | @checkAnnotation loc s t1 t2@ returns @t2@ if @t1@ contains no
-- type, and otherwise tries to unify them with 'unifyKnownTypes'.  If
-- this fails, a 'BadAnnotation' is raised.
checkAnnotation :: TypeBox tf =>
                   SrcLoc -> String -> tf -> Type -> TypeM vn Type
checkAnnotation loc desc t1 t2 =
  case unboxType t1 of
    Nothing -> return t2
    Just t1' -> case unifyKnownTypes t1' t2 of
                  Nothing -> bad $ BadAnnotation loc desc t1' t2
                  Just t  -> return t

-- | @require ts e@ causes a '(TypeError vn)' if @typeOf e@ does not unify
-- with one of the types in @ts@.  Otherwise, simply returns @e@.
-- This function is very useful in 'checkExp'.
require :: VarName vn => [Type] -> TaggedExp Type vn -> TypeM vn (TaggedExp Type vn)
require ts e
  | any (typeOf e `similarTo`) ts = return e
  | otherwise = bad $ UnexpectedType (untagExp e) ts

rowTypeM :: TaggedExp Type vn -> TypeM vn Type
rowTypeM e = maybe wrong return $ peelArray 1 $ typeOf e
  where wrong = bad $ TypeError (srclocOf e) $ "Type of expression is not array, but " ++ ppType (typeOf e) ++ "."

-- | Type check a program containing arbitrary type information,
-- yielding either a type error or a program with complete type
-- information.
checkProg :: (TypeBox tf, VarName vn) =>
             ProgBase tf vn -> Either (TypeError vn) (ProgBase Type vn)
checkProg = checkProg' True

-- | As 'checkProg', but don't check whether uniqueness constraints
-- are being upheld.  The uniqueness of types must still be correct.
checkProgNoUniqueness :: (VarName vn, TypeBox tf) =>
                         ProgBase tf vn -> Either (TypeError vn) (ProgBase Type vn)
checkProgNoUniqueness = checkProg' False

checkProg' :: (VarName vn, TypeBox ty) =>
              Bool -> ProgBase ty vn -> Either (TypeError vn) (ProgBase Type vn)
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
        let argtypes = map identType args -- Throw away argument names.
        in Right $ M.insert name (ret,argtypes,pos) ftable
    rmLoc (ret,args,_) = (ret,args)
    builtins = M.mapKeys nameFromString $
               M.fromList [("toReal", (Elem Real, [Elem Int], noLoc))
                          ,("trunc", (Elem Int, [Elem Real], noLoc))
                          ,("sqrt", (Elem Real, [Elem Real], noLoc))
                          ,("log", (Elem Real, [Elem Real], noLoc))
                          ,("exp", (Elem Real, [Elem Real], noLoc))
                          ,("op not", (Elem Bool, [Elem Bool], noLoc))]

checkFun :: (TypeBox tf, VarName vn) => TaggedFunDec tf vn -> TypeM vn (TaggedFunDec Type vn)
checkFun (fname, rettype, args, body, loc) = do
  args' <- checkArgs
  (body', dataflow) <-
    collectDataflow $ binding' args' $ checkExp body

  checkReturnAlias $ usageAliasing dataflow

  if typeOf body' `subtypeOf` rettype then
    return (fname, rettype, args, body', loc)
  else bad $ ReturnTypeError loc fname rettype $ typeOf body'

  where checkArgs = foldM expand [] args

        expand args' ident@(Ident pname _ _)
          | Just _ <- find ((==identName ident) . identName) args' =
            bad $ DupParamError fname (baseName pname) loc
          | otherwise =
            return $ ident : args'

        notAliasingParam names =
          forM_ args $ \arg ->
            when (not (unique $ typeOf arg) &&
                  identName arg `S.member` names) $
              bad $ ReturnAliased fname (baseName $ identName arg) loc

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

        returnAliasing :: Type -> Aliases vn -> [(Uniqueness, S.Set (ID vn))]
        returnAliasing (Elem (Tuple ets)) (TupleAlias alss) =
          concat $ zipWith returnAliasing ets alss
        returnAliasing t als = [(uniqueness t, aliased als)]


checkExp :: (TypeBox tf, VarName vn) => TaggedExp tf vn -> TypeM vn (TaggedExp Type vn)
checkExp e = do
  (e', als) <- collectAliases $ checkExp' e
  unless (basicType $ typeOf e') $
    tell $ mempty { usageAliasing = als }
  return e'

-- | Never call checkExp' directly!  Call checkExp!
checkExp' :: (TypeBox tf, VarName vn) => TaggedExp tf vn -> TypeM vn (TaggedExp Type vn)

checkExp' (Literal val pos) =
  Literal <$> checkLiteral pos val <*> pure pos

checkExp' (TupLit es pos) = do
  (es', als) <- unzip <$> mapM (collectAliases . checkExp) es
  let res = TupLit es' pos
  alias $ foldl extend (TupleAlias []) als
  return $ fromMaybe res (Literal <$> expToValue res <*> pure pos)
    where extend (TupleAlias alss) als = TupleAlias $ alss ++ [als]
          extend als1 als2             = TupleAlias [als1, als2]

checkExp' (ArrayLit es t loc) = do
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

checkExp' (BinOp op e1 e2 t pos) = checkBinOp op e1 e2 t pos

checkExp' (And e1 e2 pos) = do
  e1' <- require [Elem Bool] =<< checkExp e1
  e2' <- require [Elem Bool] =<< checkExp e2
  return $ And e1' e2' pos

checkExp' (Or e1 e2 pos) = do
  e1' <- require [Elem Bool] =<< checkExp e1
  e2' <- require [Elem Bool] =<< checkExp e2
  return $ Or e1' e2' pos

checkExp' (Not e pos) = do
  e' <- require [Elem Bool] =<< checkExp e
  return $ Not e' pos

checkExp' (Negate e t pos) = do
  e' <- require [Elem Int, Elem Real] =<< checkExp e
  t' <- checkAnnotation pos "result" t $ typeOf e'
  return $ Negate e' t' pos

checkExp' (If e1 e2 e3 t pos) = do
  e1' <- require [Elem Bool] =<< checkExp e1
  (e2', e3') <- checkExp e2 `alternative` checkExp e3
  bt <- checkAnnotation pos "result" t =<< unifyExpTypes e2' e3'
  return $ If e1' e2' e3' bt pos

checkExp' (Var ident) = do
  ident' <- checkIdent ident
  observe ident'
  return $ Var ident'

checkExp' (Apply fname args t pos)
  | "trace" <- nameToString fname =
  case args of
    [e] -> do
      e' <- checkExp e
      t' <- checkAnnotation pos "return" t $ typeOf e'
      return $ Apply fname [e'] t' pos
    _ -> bad $ TypeError pos "Trace function takes a single parameter"

checkExp' (Apply fname args t pos)
  | "assertZip" <- nameToString fname = do
  args' <- mapM checkExp args
  let argtps = map typeOf args'
  _ <- mapM (soac2ElemType pos) argtps
  t' <- checkAnnotation pos "return" t $ Elem Bool
  return $ Apply fname args' t' pos

checkExp' (Apply fname args rettype pos) = do
  bnd <- asks $ M.lookup fname . envFtable
  case bnd of
    Nothing -> bad $ UnknownFunctionError fname pos
    Just (ftype, paramtypes) -> do
      rettype' <- checkAnnotation pos "return" rettype ftype

      args' <- forM (zip args $ cycle paramtypes) $ \(arg, paramt) -> do
                 (arg', dflow) <- collectDataflow $ checkExp arg
                 maybeCheckOccurences $ usageOccurences dflow

                 let occurs = consumeArg (srclocOf arg) paramt $ usageAliasing dflow
                     als = maskAliasing rettype' $
                           VarAlias $ aliased $ usageAliasing dflow

                 tell $ mempty
                        { usageAliasing = als
                        , usageOccurences = usageOccurences dflow
                                            `seqOccurences` occurs }
                 return arg'

      checkApply (Just fname) paramtypes (map typeOf args') pos
      return $ Apply fname args' rettype' pos
  where -- Check if an argument of the given type consumes anything.
        consumes (Array _ _ Unique) = True
        consumes (Elem (Tuple ets)) = any consumes ets
        consumes _ = False

        -- Create usage information, given argument type and aliasing
        -- information.
        consumeArg loc (Elem (Tuple ets)) (TupleAlias alss) =
          concat $ zipWith (consumeArg loc) ets alss
        consumeArg loc t als
          | consumes t = [consumption (aliased als) loc]
          | otherwise = []

checkExp' (LetPat pat e body pos) = do
  (e', dataflow) <- collectDataflow $ checkExp e
  (scope, pat') <- checkBinding pat e' dataflow
  scope $ do
    body' <- checkExp body
    return $ LetPat pat' e' body' pos

checkExp' (LetWith (Ident dest destt destpos) src idxes ve body pos) = do
  src' <- checkIdent src
  idxes' <- mapM (require [Elem Int] <=< checkExp) idxes
  destt' <- checkAnnotation pos "source" destt $ identType src'
  let dest' = Ident dest destt' destpos

  unless (unique src' || basicType (typeOf src')) $
    bad $ TypeError pos $ "Source '" ++ textual (identName src) ++ "' is not unique"

  case peelArray (length idxes) (identType src') of
    Nothing -> bad $ IndexingError (arrayDims $ identType src') (length idxes) (srclocOf src)
    Just elemt ->
      sequentially (require [elemt] =<< checkExp ve) $ \ve' dflow -> do
        when (identName src `S.member` aliased (usageAliasing dflow)) $
          bad $ BadLetWithValue pos
        (scope, _) <- checkBinding (Id dest') (Var src') mempty
        body' <- consuming src' $ scope $ checkExp body
        return $ LetWith dest' src' idxes' ve' body' pos

checkExp' (Index ident idxes restype pos) = do
  ident' <- checkIdent ident
  observe ident'
  vt <- lookupVar (identName ident') pos
  case peelArray (length idxes) vt of
    Nothing -> bad $ IndexingError (arrayDims vt) (length idxes) pos
    Just et -> do
      restype' <- checkAnnotation pos "indexing result" restype et
      idxes' <- mapM (require [Elem Int] <=< checkExp) idxes
      return $ Index ident' idxes' restype' pos

checkExp' (Iota e pos) = do
  e' <- require [Elem Int] =<< checkExp e
  return $ Iota e' pos

checkExp' (Size e pos) = do
  e' <- checkExp e
  case typeOf e' of
    Array {} -> return $ Size e' pos
    _        -> bad $ TypeError pos "Argument to size must be array."

checkExp' (Replicate countexp valexp pos) = do
  countexp' <- require [Elem Int] =<< checkExp countexp
  valexp' <- checkExp valexp
  return $ Replicate countexp' valexp' pos

checkExp' (Reshape shapeexps arrexp pos) = do
  shapeexps' <- mapM (require [Elem Int] <=< checkExp) shapeexps
  arrexp' <- checkExp arrexp
  return (Reshape shapeexps' arrexp' pos)

checkExp' (Transpose arrexp pos) = do
  arrexp' <- checkExp arrexp
  when (arrayDims (typeOf arrexp') < 2) $
    bad $ TypeError pos "Argument to transpose does not have two dimensions."
  return $ Transpose arrexp' pos

checkExp' (Map fun arrexp intype pos) = do
  arrexp' <- checkExp arrexp
  case peelArray 1 $ typeOf arrexp' of
    Just et -> do
      fun' <- checkLambda fun [et]
      intype' <- checkAnnotation pos "input element" intype et
      return (Map fun' arrexp' intype' pos)
    _       -> bad $ TypeError (srclocOf arrexp) "Mapee expression does not return an array."

checkExp' (Reduce fun startexp arrexp intype pos) = do
  startexp' <- checkExp startexp
  arrexp' <- checkExp arrexp
  case peelArray 1 $ typeOf arrexp' of
    Just inelemt -> do
      inelemt' <- checkAnnotation pos "input element" intype inelemt
      fun' <- checkLambda fun [typeOf startexp', inelemt']
      when (typeOf startexp' /= typeOf fun') $
        bad $ TypeError pos $ "Accumulator is of type " ++ ppType (typeOf startexp') ++ ", but reduce function returns type " ++ ppType (typeOf fun') ++ "."
      return $ Reduce fun' startexp' arrexp' inelemt' pos
    _ -> bad $ TypeError (srclocOf arrexp) "Type of expression is not an array"

checkExp' (Zip arrexps pos) = do
  arrexps' <- mapM (checkExp . fst) arrexps
  inelemts <- mapM rowTypeM arrexps'
  inelemts' <- zipWithM (checkAnnotation pos "operand element") (map snd arrexps) inelemts
  return $ Zip (zip arrexps' inelemts') pos

checkExp' (Unzip e _ pos) = do
  e' <- checkExp e
  case peelArray 1 $ typeOf e' of
    Just (Elem (Tuple ts)) -> return $ Unzip e' ts pos
    _ -> bad $ TypeError pos $ "Argument to unzip is not an array of tuples, but " ++ ppType (typeOf e') ++ "."

checkExp' (Scan fun startexp arrexp intype pos) = do
  startexp' <- checkExp startexp
  arrexp' <- checkExp arrexp
  case peelArray 1 $ typeOf arrexp' of
    Just inelemt -> do
      intype' <- checkAnnotation pos "element" intype inelemt
      fun' <- checkLambda fun [intype', intype']
      when (typeOf startexp' /= typeOf fun') $
        bad $ TypeError pos $ "Initial value is of type " ++ ppType (typeOf startexp') ++ ", but scan function returns type " ++ ppType (typeOf fun') ++ "."
      when (intype' /= typeOf fun') $
        bad $ TypeError pos $ "Array element value is of type " ++ ppType intype' ++ ", but scan function returns type " ++ ppType (typeOf fun') ++ "."
      return $ Scan fun' startexp' arrexp' intype' pos
    _ -> bad $ TypeError (srclocOf arrexp) "Type of expression is not an array."

checkExp' (Filter fun arrexp rowtype pos) = do
  arrexp' <- checkExp arrexp
  inelemt <- rowTypeM arrexp'
  rowtype' <- checkAnnotation pos "row" rowtype inelemt
  fun' <- checkLambda fun [inelemt]
  when (typeOf fun' /= Elem Bool) $
    bad $ TypeError pos "Filter function does not return bool."
  return $ Filter fun' arrexp' rowtype' pos

checkExp' (Mapall fun arrexp pos) = do
  arrexp' <- checkExp arrexp
  fun' <- checkLambda fun [Elem $ elemType $ typeOf arrexp']
  return $ Mapall fun' arrexp' pos

checkExp' (Redomap redfun mapfun accexp arrexp intype pos) = do
  accexp' <- checkExp accexp
  arrexp' <- checkExp arrexp
  et <- rowTypeM arrexp'
  mapfun' <- checkLambda mapfun [et]
  redfun' <- checkLambda redfun [typeOf accexp', typeOf mapfun']
  _ <- require [typeOf redfun'] accexp'
  intype' <- checkAnnotation pos "input element" intype et
  return $ Redomap redfun' mapfun' accexp' arrexp' intype' pos

checkExp' (Split splitexp arrexp intype pos) = do
  splitexp' <- require [Elem Int] =<< checkExp splitexp
  arrexp' <- checkExp arrexp
  et <- rowTypeM arrexp'
  intype' <- checkAnnotation pos "element" intype et
  return $ Split splitexp' arrexp' intype' pos

checkExp' (Concat arr1exp arr2exp pos) = do
  arr1exp' <- checkExp arr1exp
  arr2exp' <- require [typeOf arr1exp'] =<< checkExp arr2exp
  _ <- rowTypeM arr2exp' -- Just check that it's an array.
  return $ Concat arr1exp' arr2exp' pos

checkExp' (Copy e pos) = do
  (e', _) <- collectAliases $ checkExp e
  return $ Copy e' pos

-- The loop body is checked twice to make sure any aliasing it
-- introduces is also checked.
checkExp' (DoLoop mergepat mergeexp (Ident loopvar _ _)
          boundexp loopbody letbody pos) = do
  ((boundexp', mergeexp'), mergeflow) <-
    collectDataflow $ do boundexp' <- require [Elem Int] =<< checkExp boundexp
                         mergeexp' <- checkExp mergeexp
                         return (boundexp', mergeexp')
  let mergetype = typeOf mergeexp'
      ibind = (Ident loopvar (Elem Int) pos, mempty)
      checkloop scope = collectDataflow $
                        scope $ binding [ibind] $
                        require [mergetype] =<< checkExp loopbody

  -- We need to check the loop body three times to ensure we have full
  -- aliasing information, and detect any usage collisions.
  (firstscope, mergepat') <- checkBinding mergepat mergeexp' mergeflow
  (loopbody', dataflow) <- checkloop firstscope

  (secondscope, _) <-
    checkBinding mergepat loopbody'
    dataflow { usageAliasing = maskAliasing (typeOf loopbody')
                               $ usageAliasing dataflow}
  (loopbody'', _) <- checkloop secondscope

  secondscope $ do
    letbody' <- checkExp letbody
    return $ DoLoop mergepat' mergeexp' (Ident loopvar (Elem Int) pos) boundexp' loopbody'' letbody' pos



----------------------------------------------
---- BEGIN Cosmin added SOAC2 combinators ----
----------------------------------------------
checkExp' (Map2 fun arrexp intype pos) = do
  arrexp' <- mapM checkExp arrexp
  ineltps <- mapM (soac2ElemType pos . typeOf) arrexp'
  ineltp  <- soac2ArgType pos "Map2" ineltps
  fun'    <- checkLambda fun ineltps
  intype' <- checkAnnotation pos "input element" intype ineltp
  return $ Map2 fun' arrexp' intype' pos

checkExp' (Reduce2 fun startexp arrexp intype pos) = do
  startexp' <- checkExp startexp
  arrexp'   <- mapM checkExp arrexp
  ineltps   <- mapM (soac2ElemType pos . typeOf) arrexp'
  ineltp    <- soac2ArgType pos "Reduce2" ineltps
  intype' <- checkAnnotation pos "input element" intype ineltp
  fun'    <- checkLambda fun $ case typeOf startexp' of
                                 Elem (Tuple ts) -> ts ++ ineltps
                                 t               -> t : ineltps
  when (typeOf startexp' /= typeOf fun') $
        bad $ TypeError pos $ "Accumulator is of type " ++ ppType (typeOf startexp') ++
                              ", but reduce function returns type " ++ ppType (typeOf fun') ++ "."
  return $ Reduce2 fun' startexp' arrexp' intype' pos

checkExp' (Scan2 fun startexp arrexp intype pos) = do
  startexp' <- checkExp startexp
  arrexp'   <- mapM checkExp arrexp

  --inelemt   <- soac2ElemType $ typeOf arrexp'
  ineltps   <- mapM (soac2ElemType pos . typeOf) arrexp'
  inelemt   <- soac2ArgType pos "Scan2" ineltps
  intype'   <- checkAnnotation pos "element" intype inelemt
  fun'      <- checkLambda fun (ineltps ++ ineltps)
  when (typeOf startexp' /= typeOf fun') $
    bad $ TypeError pos $ "Initial value is of type " ++ ppType (typeOf startexp') ++
                          ", but scan function returns type " ++ ppType (typeOf fun') ++ "."
  when (intype' /= typeOf fun') $
    bad $ TypeError pos $ "Array element value is of type " ++ ppType intype' ++
                          ", but scan function returns type " ++ ppType (typeOf fun') ++ "."
  return $ Scan2 fun' startexp' arrexp' intype' pos

checkExp' (Filter2 fun arrexp pos) = do
  arrexp' <- mapM checkExp arrexp
  ineltps   <- mapM (soac2ElemType pos . typeOf) arrexp'
  fun' <- checkLambda fun ineltps
  when (typeOf fun' /= Elem Bool) $
    bad $ TypeError pos "Filter function does not return bool."
  return $ Filter2 fun' arrexp' pos

checkExp' (Mapall2 fun arrexp pos) = do
  arrexp' <- mapM checkExp arrexp
  let arrtps = map typeOf arrexp'

  _ <- mapM (soac2ElemType pos) arrtps
  ineltps <- case arrtps of
               []   -> bad $ TypeError pos "Empty tuple given to Mapall2"
               [t]  -> return [Elem $ elemType t]
               t:ts -> let mindim = foldl min (arrayDims t) $ map arrayDims ts
                       in case mapM (peelArray mindim) $ t:ts of
                            Nothing  -> bad $ TypeError pos "mindim wrong in Mapall2"
                            Just ts' -> return ts'

  fun' <- checkLambda fun ineltps
  return $ Mapall2 fun' arrexp' pos

checkExp' (Redomap2 redfun mapfun accexp arrexp intype pos) = do
  accexp' <- checkExp accexp
  arrexp' <- mapM checkExp arrexp
  ets <- mapM (soac2ElemType pos . typeOf) arrexp'
  et <- soac2ArgType pos "Redomap2" ets
  mapfun' <- checkLambda mapfun ets
  let acct = case typeOf accexp' of
               Elem (Tuple ts) -> ts
               t               -> [t]
  redfun' <- checkLambda redfun $
             case typeOf mapfun' of
               Elem (Tuple ts) -> acct ++ ts
               t               -> acct ++ [t]
  _ <- require [typeOf redfun'] accexp'
  intype' <- checkAnnotation pos "input element" intype et
  return $ Redomap2 redfun' mapfun' accexp' arrexp' intype' pos

---------------------
--- SOAC2 HELPERS ---
---------------------

soac2ArgType :: SrcLoc -> String -> [Type] -> TypeM vn Type
soac2ArgType loc op [] = bad $ TypeError loc $ "Empty tuple given to " ++ op
soac2ArgType _ _ [et] = return et
soac2ArgType _ _ ets = return $ Elem $ Tuple ets

soac2ElemType :: SrcLoc -> Type -> TypeM vn Type
soac2ElemType loc tp@(Array {}) =
    getTupArrElemType loc tp
soac2ElemType loc (Elem (Tuple tps)) = do
    tps' <- mapM (getTupArrElemType loc) tps
    return $ Elem $ Tuple tps'
soac2ElemType loc tp =
    bad $ TypeError loc
                    ("In TypeChecker, soac2ElemType: "
                     ++" input type not a tuple/array: "++ppType tp)

getTupArrElemType :: SrcLoc -> Type -> TypeM vn Type
getTupArrElemType loc tp =
    case peelArray 1 tp of
        Just eltp ->
            if hasInnerTuple eltp
            then bad $ TypeError loc ("In TypeChecker, soac2, getTupArrElemType: "
                                      ++"array elem type has an inner tuple: "++ppType eltp)
            else return eltp
        _ -> bad $ TypeError loc
                             ("In TypeChecker, soac2, getTupArrElemType: "
                              ++" input type not an array: "++ppType tp)
    where
        hasInnerTuple :: Type -> Bool
        hasInnerTuple (Elem (Tuple {})) = True
        hasInnerTuple (Array etp _ _)   = hasInnerTuple $ Elem etp
        hasInnerTuple _                 = False

--------------------------------------
---- END Cosmin SOAC2 combinators ----
--------------------------------------

checkLiteral :: SrcLoc -> Value -> TypeM vn Value
checkLiteral _ (IntVal k) = return $ IntVal k
checkLiteral _ (RealVal x) = return $ RealVal x
checkLiteral _ (LogVal b) = return $ LogVal b
checkLiteral _ (CharVal c) = return $ CharVal c
checkLiteral loc (TupVal vals) = do
  vals' <- mapM (checkLiteral loc) vals
  return $ TupVal vals'
checkLiteral loc (ArrayVal arr rt) = do
  vals <- mapM (checkLiteral loc) (elems arr)
  case find ((/=rt) . typeOf) vals of
    Just wrong -> bad $ TypeError loc $ ppValue wrong ++ " is not of expected type " ++ ppType rt ++ "."
    _          -> return ()
  return $ ArrayVal (listArray (bounds arr) vals) rt

checkIdent :: (TypeBox ty, VarName vn) => TaggedIdent ty vn -> TypeM vn (TaggedIdent Type vn)
checkIdent (Ident name t pos) = do
  vt <- lookupVar name pos
  t' <- checkAnnotation pos ("variable " ++ textual (baseName name)) t vt
  return $ Ident name t' pos

checkBinOp :: (TypeBox tf, VarName vn) =>
              BinOp -> TaggedExp tf vn -> TaggedExp tf vn -> tf -> SrcLoc
           -> TypeM vn (TaggedExp Type vn)
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
              BinOp -> [ElemType]
           -> TaggedExp ty vn -> TaggedExp ty vn
           -> ty -> SrcLoc
           -> TypeM vn (TaggedExp Type vn)
checkRelOp op tl e1 e2 t pos = do
  e1' <- require (map Elem tl) =<< checkExp e1
  e2' <- require (map Elem tl) =<< checkExp e2
  _ <- unifyExpTypes e1' e2'
  t' <- checkAnnotation pos "result" t $ Elem Bool
  return $ BinOp op e1' e2' t' pos

checkPolyBinOp :: (TypeBox ty, VarName vn) =>
                  BinOp -> [ElemType]
               -> TaggedExp ty vn -> TaggedExp ty vn -> ty -> SrcLoc
               -> TypeM vn (TaggedExp Type vn)
checkPolyBinOp op tl e1 e2 t pos = do
  e1' <- require (map Elem tl) =<< checkExp e1
  e2' <- require (map Elem tl) =<< checkExp e2
  t' <- unifyExpTypes e1' e2'
  t'' <- checkAnnotation pos "result" t t'
  return $ BinOp op e1' e2' t'' pos

sequentially :: TypeM vn a -> (a -> Dataflow vn -> TypeM vn b) -> TypeM vn b
sequentially m1 m2 = do
  (a, m1flow) <- collectDataflow m1
  (b, m2flow) <- collectDataflow $ m2 a m1flow
  tell Dataflow {
           usageAliasing = usageAliasing m2flow
         , usageOccurences = usageOccurences m1flow `seqOccurences` usageOccurences m2flow
         }
  return b

checkBinding :: (VarName vn, TypeBox tf) =>
                TaggedTupIdent tf vn -> TaggedExp Type vn -> Dataflow vn
             -> TypeM vn (TypeM vn a -> TypeM vn a, TaggedTupIdent Type vn)
checkBinding pat e dflow = do
  (pat', (scope, _)) <-
    runStateT (checkBinding' pat (typeOf e) (usageAliasing dflow)) (id, [])
  return (\m -> sequentially (tell dflow) (const . const $ scope m), pat')
  where checkBinding' (Id (Ident name namet pos)) t a = do
          t' <- lift $
                checkAnnotation (srclocOf pat)
                ("binding of variable " ++ textual (baseName name)) namet t
          add (Ident name t' pos) a
          return $ Id $ Ident name t' pos
        checkBinding' (TupId pats pos) (Elem (Tuple ts)) a
          | length pats == length ts = do
          pats' <- sequence $ zipWith3 checkBinding' pats ts ass
          return $ TupId pats' pos
          where ass = case a of TupleAlias ass' -> ass' ++ repeat a
                                _               -> repeat a
        checkBinding' _ _ _ =
          lift $ bad $ InvalidPatternError (untagPattern errpat) (untagExp e)

        add ident a = do
          bnd <- gets $ find (==ident) . snd
          case bnd of
            Nothing ->
              modify $ \(scope, names) -> (binding [(ident, a)] . scope,
                                           ident : names)
            Just (Ident name _ pos2) ->
              lift $ bad $ DupPatternError (baseName name) (srclocOf ident) pos2
        -- A pattern with known type box (unit) for error messages.
        errpat = rmTypes pat
        rmTypes (Id (Ident name _ pos)) = Id $ Ident name () pos
        rmTypes (TupId pats pos) = TupId (map rmTypes pats) pos

validApply :: [Type] -> [Type] -> Bool
validApply expected got =
  length got == length expected && all id (zipWith subtypeOf got expected)

checkApply :: Maybe Name -> [Type] -> [Type] -> SrcLoc -> TypeM vn ()
checkApply fname expected got loc =
  unless (validApply expected got) $
  bad $ ParameterMismatch fname loc (Right expected) got

-- | If the return type is unique, we assume that the value does not
-- alias the parameters.
maskAliasing :: Type -> Aliases vn -> Aliases vn
maskAliasing (Elem (Tuple ets)) als =
  TupleAlias [ if unique et then mempty
               else maskAliasing et als'
               | (als', et) <- zip alss' ets ]
    where alss' = case als of (TupleAlias alss) -> alss
                              _                 -> repeat als
maskAliasing t als
  | unique t = mempty
  | otherwise = als

checkLambda :: (TypeBox ty, VarName vn) =>
               TaggedLambda ty vn -> [Type] -> TypeM vn (TaggedLambda Type vn)
checkLambda (AnonymFun params body ret pos) args = do
  (_, ret', params', body', _) <-
    checkFun (nameFromString "<anonymous>", ret, params, body, pos)
  case () of
    _ | length params' == length args -> do
          checkApply Nothing (map identType params') args pos
          return $ AnonymFun params body' ret' pos
      | [t@(Elem (Tuple args'))] <- args,
        validApply (map identType params) args',
        [Ident pname _ _] <- take 1 params ->
          -- The function expects N parmeters, but the argument is a
          -- single N-tuple whose types match the parameters.
          -- Generate a shim to make it fit.  We cleverly reuse the
          -- first parameter name, as it is guaranteed that it will
          -- not shadow anything.
          let tupparam = (Ident pname t pos)
              tupfun = AnonymFun [tupparam] tuplet ret pos
              tuplet = LetPat (TupId (map Id params) pos) (Var tupparam) body' pos
          in checkLambda tupfun args
      | otherwise -> bad $ TypeError pos $ "Anonymous function defined with " ++ show (length params) ++ " parameters, but expected to take " ++ show (length args) ++ " arguments."

checkLambda (CurryFun fname [] rettype pos) [arg]
  | "op ~" <- nameToString fname = do
  rettype' <- checkAnnotation pos "return" rettype arg
  var <- newIdent "x" arg pos
  checkLambda (AnonymFun [var] (Negate (Var var) arg pos) rettype' pos) [arg]

checkLambda (CurryFun opfun curryargexps rettype pos) args
  | Just op <- lookup (nameToString opfun) ops =
  checkPolyLambdaOp op curryargexps rettype args pos
  where ops = map (\op -> ("op " ++ opStr op, op)) [minBound..maxBound]

checkLambda (CurryFun fname curryargexps rettype pos) args = do
  curryargexps' <- mapM checkExp curryargexps
  let curryargexpts = map typeOf curryargexps'
  bnd <- asks $ M.lookup fname . envFtable
  case bnd of
    Nothing -> bad $ UnknownFunctionError fname pos
    Just (rt, paramtypes) -> do
      rettype' <- checkAnnotation pos "return" rettype rt
      case () of
        _ | [tupt@(Elem (Tuple ets))] <- args,
            validApply ets paramtypes -> do
              -- Same shimming as in the case for anonymous functions,
              -- although we don't have to worry about name shadowing
              -- here.
              let mkparam i t = newIdent ("param_" ++ show i) t pos
              params <- zipWithM mkparam [(0::Int)..] paramtypes
              tupparam <- newIdent "x" tupt pos
              let tuplet = LetPat (TupId (map Id params) pos) (Var tupparam) body pos
                  tupfun = AnonymFun [tupparam] tuplet rettype' pos
                  body = Apply fname (map Var params) rettype' pos
              checkLambda tupfun args
          | otherwise -> do
              case find (unique . snd) $ zip curryargexps paramtypes of
                Just (e, _) -> bad $ CurriedConsumption fname $ srclocOf e
                _           -> return ()
              checkApply Nothing paramtypes (curryargexpts++args) pos
              return $ CurryFun fname curryargexps' rettype' pos

checkPolyLambdaOp :: (TypeBox ty, VarName vn) =>
                     BinOp -> [TaggedExp ty vn] -> ty -> [Type] -> SrcLoc
                  -> TypeM vn (TaggedLambda Type vn)
checkPolyLambdaOp op curryargexps rettype args pos = do
  curryargexpts <- map typeOf <$> mapM checkExp curryargexps
  tp <- case curryargexpts ++ args of
          [t1, t2] | t1 == t2 -> return t1
          [Elem (Tuple [t1,t2])] | t1 == t2 -> return t1 -- For autoshimming.
          l -> bad $ ParameterMismatch (Just fname) pos (Left 2) l
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
  body <- binding' params $ checkBinOp op x y rettype pos
  checkLambda (AnonymFun params body (typeOf body) pos) args
  where fname = nameFromString $ "op " ++ opStr op
