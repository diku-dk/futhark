{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
-- | The type checker checks whether the program is type-consistent.
-- Whether type annotations are already present is irrelevant, but if
-- they are, the type checker will signal an error if they are wrong.
-- The program does not need to have any particular properties for the
-- type checker to function; in particular it does not need unique
-- names.
module Futhark.Representation.External.TypeChecker
  ( checkProg
  , checkProgNoUniqueness
  , checkClosedExp
  , checkOpenExp
  , TypeError)
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

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Prelude

import Futhark.Representation.External
import Futhark.Representation.External.Renamer
  (tagProg', tagExp, tagExp', tagType',
   untagProg, untagExp, untagPattern)
import Futhark.FreshNames hiding (newID, newName)
import qualified Futhark.FreshNames
import Futhark.TypeCheck.TypeError
-- | Information about an error during type checking.  The 'Show'
-- instance for this type produces a human-readable description.

type TypeError vn =
  GenTypeError
  vn
  (ExpBase CompTypeBase vn)
  (TypeBase Rank NoInfo ())
  (TupIdentBase NoInfo vn)

type TaggedIdent ty vn = IdentBase ty (ID vn)

type TaggedParam vn = ParamBase (ID vn)

type TaggedExp ty vn = ExpBase ty (ID vn)

type TaggedLambda ty vn = LambdaBase ty (ID vn)

type TaggedTupIdent ty vn = TupIdentBase ty (ID vn)

type TaggedFunDec ty vn = FunDecBase ty (ID vn)

type TaggedType vn = TypeBase Rank Names (ID vn)

type TaggedDeclType vn = DeclTypeBase (ID vn)

-- | A tuple of a return type and a list of argument types.
type FunBinding vn = (DeclTypeBase (ID vn), [DeclTypeBase (ID vn)])

data Binding vn = Bound (TaggedType vn)
                | WasConsumed SrcLoc

data Usage = Consumed SrcLoc
           | Observed SrcLoc
             deriving (Eq, Ord, Show)

data Occurence vn = Occurence { observed :: Names (ID vn)
                              , consumed :: Names (ID vn)
                              , location :: SrcLoc
                              }
             deriving (Eq, Show)

instance Located (Occurence vn) where
  locOf = locOf . location

observation :: Names (ID vn) -> SrcLoc -> Occurence vn
observation = flip Occurence HS.empty

consumption :: Names (ID vn) -> SrcLoc -> Occurence vn
consumption = Occurence HS.empty

nullOccurence :: Occurence vn -> Bool
nullOccurence occ = HS.null (observed occ) && HS.null (consumed occ)

type Occurences vn = [Occurence vn]

type UsageMap vn = HM.HashMap (ID vn) [Usage]

usageMap :: Occurences vn -> UsageMap vn
usageMap = foldl comb HM.empty
  where comb m (Occurence obs cons loc) =
          let m' = HS.foldl' (ins $ Observed loc) m obs
          in HS.foldl' (ins $ Consumed loc) m' cons
        ins v m k = HM.insertWith (++) k [v] m

combineOccurences :: ID vn -> Usage -> Usage -> Either (TypeError vn) Usage
combineOccurences _ (Observed loc) (Observed _) = Right $ Observed loc
combineOccurences name (Consumed wloc) (Observed rloc) =
  Left $ UseAfterConsume (baseName name) rloc wloc
combineOccurences name (Observed rloc) (Consumed wloc) =
  Left $ UseAfterConsume (baseName name) rloc wloc
combineOccurences name (Consumed loc1) (Consumed loc2) =
  Left $ UseAfterConsume (baseName name) (max loc1 loc2) (min loc1 loc2)

checkOccurences :: Occurences vn -> Either (TypeError vn) ()
checkOccurences = void . HM.traverseWithKey comb . usageMap
  where comb _    []     = Right ()
        comb name (u:us) = foldM_ (combineOccurences name) u us

allConsumed :: Occurences vn -> Names (ID vn)
allConsumed = HS.unions . map consumed

seqOccurences :: Occurences vn -> Occurences vn -> Occurences vn
seqOccurences occurs1 occurs2 =
  filter (not . nullOccurence) $ map filt occurs1 ++ occurs2
  where filt occ =
          occ { observed = observed occ `HS.difference` postcons }
        postcons = allConsumed occurs2

altOccurences :: Occurences vn -> Occurences vn -> Occurences vn
altOccurences occurs1 occurs2 =
  filter (not . nullOccurence) $ map filt occurs1 ++ occurs2
  where filt occ =
          occ { consumed = consumed occ `HS.difference` postcons
              , observed = observed occ `HS.difference` postcons }
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
data Scope vn = Scope { envVtable :: HM.HashMap (ID vn) (Binding vn)
                          , envFtable :: HM.HashMap Name (FunBinding vn)
                          , envCheckOccurences :: Bool
                          }

-- | The type checker runs in this monad.  The 'Either' monad is used
-- for error handling.
newtype TypeM vn a = TypeM (RWST
                            (Scope vn)            -- Reader
                            (Dataflow vn)           -- Writer
                            (NameSource (ID vn))    -- State
                            (Either (TypeError vn)) -- Inner monad
                            a)
  deriving (Monad, Functor, Applicative,
            MonadReader (Scope vn),
            MonadWriter (Dataflow vn),
            MonadState (NameSource (ID vn)))

runTypeM :: Scope vn -> NameSource (ID vn) -> TypeM vn a
         -> Either (TypeError vn) a
runTypeM env src (TypeM m) = fst <$> evalRWST m env src

bad :: VarName vn => TypeError vn -> TypeM vn a
bad = TypeM . lift . Left

newName :: VarName vn => ID vn -> TypeM vn (ID vn)
newName s = do src <- get
               let (s', src') = Futhark.FreshNames.newName src s
               put src'
               return s'

newFname :: VarName vn => String -> TypeM vn Name
newFname s = do s' <- newName $ varName s Nothing
                return $ nameFromString $ textual $ baseName s'

newID :: VarName vn => vn -> TypeM vn (ID vn)
newID s = newName $ ID (s, 0)

newIDFromString :: VarName vn => String -> TypeM vn (ID vn)
newIDFromString s = newID $ varName s Nothing

newIdent :: VarName vn =>
            String -> ty (ID vn) -> SrcLoc -> TypeM vn (IdentBase ty (ID vn))
newIdent s t loc = do
  s' <- newID $ varName s Nothing
  return $ Ident s' t loc

liftEither :: VarName vn => Either (TypeError vn) a -> TypeM vn a
liftEither = either bad return

occur :: VarName vn => Occurences vn -> TypeM vn ()
occur occurs = tell Dataflow { usageOccurences = occurs }

-- | Proclaim that we have made read-only use of the given variable.
-- No-op unless the variable is array-typed.
observe :: VarName vn => TaggedIdent CompTypeBase vn -> TypeM vn ()
observe (Ident nm t loc)
  | primType t = return ()
  | otherwise   = let als = nm `HS.insert` aliases t
                  in occur [observation als loc]

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
          env { envVtable = HM.insert name (WasConsumed loc) $ envVtable env }

collectDataflow :: VarName vn => TypeM vn a -> TypeM vn (a, Dataflow vn)
collectDataflow m = pass $ do
  (x, dataflow) <- listen m
  return ((x, dataflow), const mempty)

noDataflow :: VarName vn => TypeM vn a -> TypeM vn a
noDataflow = censor $ const mempty

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
unbinding = local (\env -> env { envVtable = HM.empty})

-- | Make all bindings nonunique.
noUnique :: VarName vn => TypeM vn a -> TypeM vn a
noUnique = local (\env -> env { envVtable = HM.map f $ envVtable env})
  where f (Bound t)         = Bound $ t `setUniqueness` Nonunique
        f (WasConsumed loc) = WasConsumed loc

binding :: VarName vn => [TaggedIdent CompTypeBase vn] -> TypeM vn a -> TypeM vn a
binding bnds = check . local (`bindVars` bnds)
  where bindVars :: Scope vn -> [TaggedIdent CompTypeBase vn] -> Scope vn
        bindVars = foldl bindVar

        bindVar :: Scope vn -> TaggedIdent CompTypeBase vn -> Scope vn
        bindVar env (Ident name tp _) =
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
                             let (obs1, obs2) = divide $ observed occ
                                 (con1, con2) = divide $ consumed occ
                             in (occ { observed = obs1, consumed = con1 },
                                 occ { observed = obs2, consumed = con2 }))
                names = HS.fromList $ map identName bnds
                divide s = (s `HS.intersection` names, s `HS.difference` names)

bindingParams :: VarName vn => [TaggedParam vn] -> TypeM vn a -> TypeM vn a
bindingParams params m =
  -- We need to bind both the identifiers themselves, as well as any
  -- presently non-bound shape annotations.
  binding (map fromParam params) $ do
    -- Figure out the not already bound shape annotations.
    dims <- liftM concat $ forM params $ \param ->
      liftM catMaybes $
      mapM (inspectDim $ srclocOf param) $
      arrayDims $ identType param
    binding dims m
  where inspectDim _ AnyDim =
          return Nothing
        inspectDim _ (ConstDim _) =
          return Nothing
        inspectDim loc (NamedDim name) =
          return $ Just $ Ident name (Prim $ IntType Int32) loc

lookupVar :: VarName vn => ID vn -> SrcLoc -> TypeM vn (TaggedType vn)
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
unifyTupleArrayElemTypes (PrimArrayElem bt1 als1) (PrimArrayElem bt2 als2)
  | bt1 == bt2 = Just $ PrimArrayElem bt1 $ als1 <> als2
  | otherwise  = Nothing
unifyTupleArrayElemTypes (ArrayArrayElem at1) (ArrayArrayElem at2) =
  ArrayArrayElem <$> unifyArrayTypes at1 at2
unifyTupleArrayElemTypes (TupleArrayElem ts1) (TupleArrayElem ts2) =
  TupleArrayElem <$> zipWithM unifyTupleArrayElemTypes ts1 ts2
unifyTupleArrayElemTypes _ _ =
  Nothing

-- | Determine if two types are identical, ignoring uniqueness.
-- Causes a '(TypeError vn)' if they fail to match, and otherwise returns
-- one of them.
unifyExpTypes :: VarName vn =>
                 TaggedExp CompTypeBase vn
              -> TaggedExp CompTypeBase vn
              -> TypeM vn (TaggedType vn)
unifyExpTypes e1 e2 =
  maybe (bad $ UnifyError e1' (toStructural t1) e2' (toStructural t2)) return $
  unifyTypes (typeOf e1) (typeOf e2)
  where e1' = untagExp e1
        t1  = toDecl $ typeOf e1'
        e2' = untagExp e2
        t2  = toDecl $ typeOf e2'

-- | @checkAnnotation loc s t1 t2@ returns @t2@ if @t1@ contains no
-- type, and otherwise tries to unify them with 'unifyTypes'.  If
-- this fails, a 'BadAnnotation' is raised.
checkAnnotation :: (VarName vn, TypeBox ty) =>
                   SrcLoc -> String -> ty (ID vn) -> TaggedType vn
                -> TypeM vn (TaggedType vn)
checkAnnotation loc desc t1 t2 =
  case unboxType t1 of
    Nothing -> return t2
    Just t1' -> case unifyTypes (t1' `setAliases` HS.empty) t2 of
                  Nothing -> bad $ BadAnnotation loc desc
                                   (toStructural t1')
                                   (toStructural t2)
                  Just t  -> return t

anyIntType :: [TaggedType vn]
anyIntType = map (Prim . IntType) [minBound .. maxBound]

anyFloatType :: [TaggedType vn]
anyFloatType = map (Prim . FloatType) [minBound .. maxBound]

anyNumberType :: [TaggedType vn]
anyNumberType = anyIntType ++ anyFloatType

-- | @require ts e@ causes a '(TypeError vn)' if @typeOf e@ does not unify
-- with one of the types in @ts@.  Otherwise, simply returns @e@.
-- This function is very useful in 'checkExp'.
require :: VarName vn => [TaggedType vn] -> TaggedExp CompTypeBase vn -> TypeM vn (TaggedExp CompTypeBase vn)
require ts e
  | any (typeOf e `similarTo`) ts = return e
  | otherwise = bad $ UnexpectedType (srclocOf e') e'
                      (toStructural $ typeOf e') $
                      map toStructural ts
  where e' = untagExp e

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
  let typeenv = Scope { envVtable = HM.empty
                        , envFtable = ftable
                        , envCheckOccurences = checkoccurs
                        }
  liftM (untagProg . Prog) $
          runTypeM typeenv src $ mapM (noDataflow . checkFun) $ progFunctions prog'
  where
    (prog', src) = tagProg' blankNameSource prog
    -- To build the ftable we loop through the list of function
    -- definitions.  In addition to the normal ftable information
    -- (name, return type, argument types), we also keep track of
    -- position information, in order to report both locations of
    -- duplicate function definitions.  The position information is
    -- removed at the end.
    buildFtable = HM.map rmLoc <$>
                  foldM expand (HM.map addLoc initialFtable)
                  (progFunctions prog')
    expand ftable (name,ret,args,_,pos)
      | Just (_,_,pos2) <- HM.lookup name ftable =
        Left $ DupDefinitionError name pos pos2
      | otherwise =
        let argtypes = map (toDecl . identType) args -- Throw away argument names.
        in Right $ HM.insert name (ret,argtypes,pos) ftable
    rmLoc (ret,args,_) = (ret,args)
    addLoc (t, ts) = (t, ts, noLoc)

initialFtable :: HM.HashMap Name (FunBinding vn)
initialFtable = HM.map addBuiltin builtInFunctions
  where addBuiltin (t, ts) = (Prim t, map Prim ts)

checkFun :: (TypeBox ty, VarName vn) =>
            TaggedFunDec ty vn -> TypeM vn (TaggedFunDec CompTypeBase vn)
checkFun (fname, rettype, params, body, loc) = do
  checkParams
  body' <- bindingParams params $ do
    checkRetType loc rettype
    checkExp body

  checkReturnAlias $ typeOf body'

  if toStructural (typeOf body') `subtypeOf` toStructural rettype then
    return (fname, rettype, params, body', loc)
  else bad $ ReturnTypeError loc fname (toStructural rettype) $
             toStructural $ typeOf body'

  where checkParams = do
          -- First find all normal parameters (checking for duplicates).
          normal_params <- foldM checkNormParams HS.empty params
          -- Then check shape annotations (where duplicates are OK, as
          -- long as it's not a duplicate of a normal parameter.)
          mapM_ (checkDimDecls normal_params) params

        checkNormParams knownparams (Ident pname _ _)
          | pname `HS.member` knownparams =
            bad $ DupParamError fname (baseName pname) loc
          | otherwise =
            return $ HS.insert pname knownparams

        checkDimDecls normal_params (Ident _ ptype _)
          | Just name <- find (`HS.member` normal_params) boundDims =
            bad $ DupParamError fname (baseName name) loc
          | otherwise =
            return ()
          where boundDims = mapMaybe boundDim $ arrayDims ptype
                boundDim (NamedDim name) = Just name
                boundDim _ = Nothing

        notAliasingParam names =
          forM_ params $ \p ->
            when (not (unique $ identType p) &&
                  identName p `HS.member` names) $
              bad $ ReturnAliased fname (baseName $ identName p) loc

        -- | Check that unique return values do not alias a
        -- non-consumed parameter.
        checkReturnAlias =
          foldM_ checkReturnAlias' HS.empty . returnAliasing rettype

        checkReturnAlias' seen (Unique, names)
          | any (`HS.member` HS.map snd seen) $ HS.toList names =
            bad $ UniqueReturnAliased fname loc
          | otherwise = do
            notAliasingParam names
            return $ seen `HS.union` tag Unique names
        checkReturnAlias' seen (Nonunique, names)
          | any (`HS.member` seen) $ HS.toList $ tag Unique names =
            bad $ UniqueReturnAliased fname loc
          | otherwise = return $ seen `HS.union` tag Nonunique names

        tag u = HS.map $ \name -> (u, name)

        returnAliasing (Tuple ets1) (Tuple ets2) =
          concat $ zipWith returnAliasing ets1 ets2
        returnAliasing expected got = [(uniqueness expected, aliases got)]

-- | Type-check a single expression without any calls to non-builtin
-- functions.  Free variables are permitted, as long as they are
-- present in the passed-in vtable.
checkOpenExp :: (TypeBox ty, VarName vn) =>
                HM.HashMap vn (CompTypeBase vn) -> ExpBase ty vn ->
                Either (TypeError vn) (ExpBase CompTypeBase vn)
checkOpenExp bnds e = untagExp <$> runTypeM env namesrc (checkExp e')
  where env = Scope { envFtable = initialFtable
                      , envCheckOccurences = True
                      , envVtable = vtable
                      }
        (e', src) = tagExp' blankNameSource e

        (vtable, namesrc) = foldl tagBnd (HM.empty, src) $
                            HS.toList $ freeNamesInExp e'
        tagBnd (m, src') k =
          case HM.lookup (baseName k) bnds of
            Nothing -> (m, src')
            Just t  -> let (t', src'') = tagType' src' t
                       in (HM.insert k (Bound t') m, src'')

-- | Type-check a single expression without any free variables or
-- calls to non-builtin functions.
checkClosedExp :: (TypeBox ty, VarName vn) => ExpBase ty vn ->
                  Either (TypeError vn) (ExpBase CompTypeBase vn)
checkClosedExp e = untagExp <$> runTypeM env src (checkExp e')
  where env = Scope { envFtable = initialFtable
                      , envCheckOccurences = True
                      , envVtable = HM.empty
                      }
        e' = tagExp e
        src = newNameSource $ freeNamesInExp e'

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
                  | Just elemt' <- elemt `unifyTypes` typeOf eleme =
                    return elemt'
                  | otherwise =
                    bad $ TypeError loc $ ppExp eleme ++ " is not of expected type " ++ ppType elemt ++ "."
            in foldM check (typeOf e) es''

  -- Unify that type with the one given for the array literal.
  t' <- checkAnnotation loc "array-element" t et

  let lit = ArrayLit es' t' loc
  return $ fromMaybe lit (Literal <$> expToValue lit <*> pure loc)

checkExp (BinOp op e1 e2 t pos) = checkBinOp op e1 e2 t pos

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
  e' <- require anyIntType =<< checkExp e
  return $ UnOp Abs e' loc

checkExp (UnOp Signum e loc) = do
  e' <- require anyIntType =<< checkExp e
  return $ UnOp Signum e' loc

checkExp (UnOp (ToFloat t) e loc) = do
  e' <- require anyNumberType =<< checkExp e
  return $ UnOp (ToFloat t) e' loc

checkExp (UnOp (ToInt t) e loc) = do
  e' <- require anyNumberType =<< checkExp e
  return $ UnOp (ToInt t) e' loc

checkExp (If e1 e2 e3 t pos) = do
  e1' <- require [Prim Bool] =<< checkExp e1
  ((e2', e3'), dflow) <- collectDataflow $ checkExp e2 `alternative` checkExp e3
  tell dflow
  t' <- checkAnnotation pos "branch result" t $
        addAliases (typeOf e2' `unifyUniqueness` typeOf e3')
        (`HS.difference` allConsumed (usageOccurences dflow))
  return $ If e1' e2' e3' t' pos

checkExp (Var ident) = do
  ident' <- checkIdent ident
  observe ident'
  return $ Var ident'

checkExp (Apply fname args t pos)
  | "trace" <- nameToString fname =
  case args of
    [(e, _)] -> do
      e'  <- checkExp e
      t'  <- checkAnnotation pos "return" t $ typeOf e'
      return $ Apply fname [(e', Observe)] t' pos
    _ -> bad $ TypeError pos "Trace function takes a single parameter"

checkExp (Apply fname args rettype loc) = do
  bnd <- asks $ HM.lookup fname . envFtable
  case bnd of
    Nothing -> bad $ UnknownFunctionError fname loc
    Just (ftype, paramtypes) -> do
      (args', argflows) <- unzip <$> mapM (checkArg . fst) args

      rettype' <- checkAnnotation loc "return" rettype $
                  returnType (removeShapeAnnotations ftype)
                  (map diet paramtypes) (map typeOf args')

      checkFuncall (Just fname) loc paramtypes ftype argflows

      return $ Apply fname (zip args' $ map diet paramtypes) rettype' loc

checkExp (LetPat pat e body pos) = do
  (e', dataflow) <- collectDataflow $ checkExp e
  (scope, pat') <- checkBinding pat (typeOf e') dataflow
  scope $ do
    body' <- checkExp body
    return $ LetPat pat' e' body' pos

checkExp (LetWith (Ident dest destt destpos) src idxes ve body pos) = do
  src' <- checkIdent src
  idxes' <- mapM (require [Prim $ IntType Int32] <=< checkExp) idxes
  destt' <- checkAnnotation pos "source" destt $ identType src' `setAliases` HS.empty
  let dest' = Ident dest destt' destpos

  unless (unique $ identType src') $
    bad $ TypeError pos $ "Source '" ++ textual (baseName $ identName src) ++
    "' has type " ++ ppType (identType src') ++ ", which is not unique"

  case peelArray (length idxes) (identType src') of
    Nothing -> bad $ IndexingError (baseName $ identName src)
                     (arrayRank $ identType src') (length idxes) (srclocOf src)
    Just elemt ->
      sequentially (require [elemt] =<< checkExp ve) $ \ve' _ -> do
        when (identName src `HS.member` aliases (typeOf ve')) $
          bad $ BadLetWithValue pos
        (scope, _) <- checkBinding (Id dest') destt' mempty
        body' <- consuming src' $ scope $ checkExp body
        return $ LetWith dest' src' idxes' ve' body' pos

checkExp (Index ident idxes pos) = do
  ident' <- checkIdent ident
  observe ident'
  vt <- lookupVar (identName ident') pos
  when (arrayRank vt < length idxes) $
    bad $ IndexingError (baseName $ identName ident)
          (arrayRank vt) (length idxes) pos
  idxes' <- mapM (require [Prim $ IntType Int32] <=< checkExp) idxes
  return $ Index ident' idxes' pos

checkExp (Iota e pos) = do
  e' <- require [Prim $ IntType Int32] =<< checkExp e
  return $ Iota e' pos

checkExp (Size i e pos) = do
  e' <- checkExp e
  case typeOf e' of
    Array {}
      | i >= 0 && i < arrayRank (typeOf e') ->
        return $ Size i e' pos
      | otherwise ->
        bad $ TypeError pos $ "Type " ++ ppType (typeOf e') ++ " has no dimension " ++ show i ++ "."
    _        -> bad $ TypeError pos "Argument to size must be array."

checkExp (Replicate countexp valexp pos) = do
  countexp' <- require [Prim $ IntType Int32] =<< checkExp countexp
  valexp' <- checkExp valexp
  return $ Replicate countexp' valexp' pos

checkExp (Reshape shapeexps arrexp pos) = do
  shapeexps' <- mapM (require [Prim $ IntType Int32] <=< checkExp) shapeexps
  arrexp' <- checkExp arrexp
  return (Reshape shapeexps' arrexp' pos)

checkExp (Rearrange perm arrexp pos) = do
  arrexp' <- checkExp arrexp
  let rank = arrayRank $ typeOf arrexp'
  when (length perm /= rank || sort perm /= [0..rank-1]) $
    bad $ PermutationError pos perm rank name
  return $ Rearrange perm arrexp' pos
  where name = case arrexp of Var v -> Just $ baseName $ identName v
                              _     -> Nothing

checkExp (Stripe strideexp arrexp loc) = do
  strideexp' <- require [Prim $ IntType Int32] =<< checkExp strideexp
  arrexp' <- checkExp arrexp
  _ <- rowTypeM arrexp' -- Just check that it's an array.
  return $ Stripe strideexp' arrexp' loc

checkExp (Unstripe strideexp arrexp loc) = do
  strideexp' <- require [Prim $ IntType Int32] =<< checkExp strideexp
  arrexp' <- checkExp arrexp
  _ <- rowTypeM arrexp' -- Just check that it's an array.
  return $ Unstripe strideexp' arrexp' loc

checkExp (Transpose k n arrexp pos) = do
  arrexp' <- checkExp arrexp
  when (arrayRank (typeOf arrexp') < reach + 1) $
    bad $ TypeError pos $ "Argument to transpose does not have " ++
          show (reach+1) ++ " dimensions."
  return $ Transpose k n arrexp' pos
  where reach = max k $ n + k

checkExp (Zip arrexps loc) = do
  arrexps' <- mapM (checkExp . fst) arrexps
  arrts <- forM arrexps' $ \arrexp -> do
    let arrt = typeOf arrexp
    when (arrayRank arrt < 1) $
      bad $ TypeError (srclocOf arrexp) $
      "Type of expression is not array, but " ++ ppType arrt ++ "."
    return arrt
  arrts' <- zipWithM (checkAnnotation loc "operand element") (map snd arrexps) arrts
  return $ Zip (zip arrexps' arrts') loc

checkExp (Unzip e _ pos) = do
  e' <- checkExp e
  case typeOf e' of
    Array (TupleArray ets shape u) ->
      let componentType et =
            arrayOf (tupleArrayElemToType et) shape u
      in return $ Unzip e' (map componentType ets) pos
    t ->
      bad $ TypeError pos $
      "Argument to unzip is not an array of tuples, but " ++
      ppType t ++ "."

checkExp (Map fun arrexp pos) = do
  (arrexp', arg) <- checkSOACArrayArg arrexp
  fun' <- checkLambda fun [arg]
  return (Map fun' arrexp' pos)

checkExp (ConcatMap fun arrexp arrexps pos) = do
  (arrexp', arg) <- checkArg arrexp
  (arrexps', _) <- unzip <$> mapM checkArg arrexps
  fun' <- checkLambda fun [arg]
  return $ ConcatMap fun' arrexp' arrexps' pos

checkExp (Reduce comm fun startexp arrexp pos) = do
  (startexp', startarg) <- checkArg startexp
  (arrexp', arrarg@(inrowt, _, _)) <- checkSOACArrayArg arrexp
  fun' <- checkLambda fun [startarg, arrarg]
  let redtype = lambdaType fun' [typeOf startexp', typeOf arrexp']
  unless (typeOf startexp' `subtypeOf` redtype) $
    bad $ TypeError pos $ "Initial value is of type " ++ ppType (typeOf startexp') ++ ", but reduce function returns type " ++ ppType redtype ++ "."
  unless (inrowt `subtypeOf` redtype) $
    bad $ TypeError pos $ "Array element value is of type " ++ ppType inrowt ++ ", but reduce function returns type " ++ ppType redtype ++ "."
  return $ Reduce comm fun' startexp' arrexp' pos

checkExp (Scan fun startexp arrexp pos) = do
  (startexp', startarg) <- checkArg startexp
  (arrexp', arrarg@(inrowt, _, _)) <- checkSOACArrayArg arrexp
  fun' <- checkLambda fun [startarg, arrarg]
  let scantype = lambdaType fun' [typeOf startexp', typeOf arrexp']
  unless (typeOf startexp' `subtypeOf` scantype) $
    bad $ TypeError pos $ "Initial value is of type " ++ ppType (typeOf startexp') ++ ", but scan function returns type " ++ ppType scantype ++ "."
  unless (inrowt `subtypeOf` scantype) $
    bad $ TypeError pos $ "Array element value is of type " ++ ppType inrowt ++ ", but scan function returns type " ++ ppType scantype ++ "."
  return $ Scan fun' startexp' arrexp' pos

checkExp (Filter fun arrexp pos) = do
  (arrexp', (rowelemt, argflow, argloc)) <- checkSOACArrayArg arrexp
  let nonunique_arg = (rowelemt `setUniqueness` Nonunique,
                       argflow, argloc)
  fun' <- checkLambda fun [nonunique_arg]
  when (lambdaType fun' [rowelemt] /= Prim Bool) $
    bad $ TypeError pos "Filter function does not return bool."

  return $ Filter fun' arrexp' pos

checkExp (Partition funs arrexp pos) = do
  (arrexp', (rowelemt, argflow, argloc)) <- checkSOACArrayArg arrexp
  let nonunique_arg = (rowelemt `setUniqueness` Nonunique,
                       argflow, argloc)
  funs' <- forM funs $ \fun -> do
    fun' <- checkLambda fun [nonunique_arg]
    when (lambdaType fun' [rowelemt] /= Prim Bool) $
      bad $ TypeError (srclocOf fun') "Partition function does not return bool."
    return fun'

  return $ Partition funs' arrexp' pos

checkExp (Redomap comm outerfun innerfun accexp arrexp pos) = do
  (accexp', accarg) <- checkArg accexp
  (arrexp', arrarg@(rt, _, _)) <- checkSOACArrayArg arrexp
  (outerfun', _) <- checkLambdaArg outerfun [accarg, accarg]
  innerfun' <- checkLambda innerfun [accarg, arrarg]
  let redtype = lambdaType innerfun' [typeOf accexp', rt]
  if argType accarg == redtype
  then return $ Redomap comm outerfun' innerfun' accexp' arrexp' pos
  else case redtype of
         Tuple (acctp:_) -> do
             _ <- require [acctp] accexp'
             return $ Redomap comm outerfun' innerfun' accexp' arrexp' pos
         _ -> bad $ TypeError pos "Redomap with illegal reduce type."

checkExp (Stream form lam@(AnonymFun lam_ps _ lam_rtp _) arr ii pos) = do
  let isArrayType arrtp =
        case arrtp of
          Array _ -> True
          _       -> False
  let lit_int0 = Literal (PrimValue $ IntValue $ Int32Value 0) pos
  [(_, intarg),(arr',arrarg)] <- mapM checkArg [lit_int0, arr]
  -- arr must have an array type
  unless (isArrayType $ typeOf arr') $
    bad $ TypeError pos "Stream with input array of non-array type."
  -- typecheck stream's lambdas
  (form', macctup) <-
    case form of
      MapLike o -> return (MapLike o, Nothing)
      RedLike o comm lam0 acc -> do
        (acc',accarg) <- checkArg acc
        lam0' <- checkLambda lam0 [accarg, accarg]
        let redtype = lambdaType lam0' [typeOf acc', typeOf acc']
        unless (redtype `subtypeOf` typeOf acc') $
            bad $ TypeError pos $ "Stream's reduce fun: Initial value is of type " ++
                  ppType (typeOf acc') ++ ", but reduce fun returns type "++ppType redtype++"."
        return (RedLike o comm lam0' acc', Just(acc',accarg))
      Sequential acc -> do
        (acc',accarg) <- checkArg acc
        return (Sequential acc', Just(acc',accarg))
  -- (i) properly check the lambda on its parameter and
  --(ii) make some fake arguments, which do not alias `arr', and
  --     check that aliases of `arr' are not used inside lam.
  let fakearg = (fromDecl $ addNames $ removeNames $ typeOf arr', mempty, srclocOf pos)
      (aas,faas) = case macctup of
                    Nothing        -> ([intarg, arrarg],        [intarg,fakearg]         )
                    Just(_,accarg) -> ([intarg, accarg, arrarg],[intarg, accarg, fakearg])
  lam' <- checkLambda lam aas
  (_, dflow)<- collectDataflow $ checkLambda lam faas
  let arr_aliasses = HS.toList $ aliases $ typeOf arr'
  let usages = usageMap $ usageOccurences dflow
  when (any (`HM.member` usages) arr_aliasses) $
     bad $ TypeError pos "Stream with input array used inside lambda."
  -- check that the result type of lambda matches the accumulator part
  _ <- case macctup of
        Just (acc',_) -> do
            let rtp' = lambdaType lam' [Prim $ IntType Int32, typeOf acc', typeOf acc']
            case rtp' of
                Tuple (acctp:_) ->
                     unless (typeOf acc' `subtypeOf` removeShapeAnnotations acctp) $
                        bad $ TypeError pos ("Stream with accumulator-type missmatch"++
                                             "or result arrays of non-array type.")
                _ -> unless (typeOf acc' `subtypeOf` removeShapeAnnotations rtp') $
                        bad $ TypeError pos "Stream with accumulator-type missmatch."
        Nothing -> return ()
  -- check outerdim of Lambda's streamed-in array params are NOT specified,
  -- and that return type inner dimens are all specified but not as other
  -- lambda parameters!
  (chunk,lam_arr_tp)<- case macctup of
                         Just _ -> case lam_ps of
                                     [ch,_,arrpar] -> return (identName ch, identType arrpar)
                                     _ -> bad $ TypeError pos "Stream's lambda should have three args."
                         Nothing-> case lam_ps of
                                     [ch,  arrpar] -> return (identName ch, identType arrpar)
                                     _ -> bad $ TypeError pos "Stream's lambda should have three args."
  let outer_dims = arrayDims lam_arr_tp
  _ <- case head outer_dims of
        AnyDim      -> return ()
        NamedDim _  -> return ()
        ConstDim _  -> bad $ TypeError pos ("Stream: outer dimension of stream should NOT"++
                                            " be specified since it is "++textual chunk++"by default.")
  _ <- case lam_rtp of
        Tuple res_tps -> do
            let res_arr_tps = tail res_tps
            if all isArrayType res_arr_tps
            then do let lam_params = HS.fromList $ map identName lam_ps
                        arr_iner_dims = concatMap (tail . arrayDims) res_arr_tps
                        boundDim (NamedDim name) = return $ Just name
                        boundDim (ConstDim _   ) = return Nothing
                        boundDim _               =
                            bad $ TypeError pos $ "Stream's lambda: inner dimensions of the"++
                                                  " streamed-result arrays MUST be specified!"
                    rtp_iner_syms <- catMaybes <$> mapM boundDim arr_iner_dims
                    case find (`HS.member` lam_params) rtp_iner_syms of
                      Just name -> bad $ TypeError pos $
                                          "Stream's lambda: " ++ textual (baseName name) ++
                                          " cannot specify a variant inner result shape"
                      _ -> return ()
            else bad $ TypeError pos "Stream with result arrays of non-array type."
        _ -> return ()-- means that no array is streamed out!
  -- finally return type-checked stream!
  return $ Stream form' lam' arr' ii pos

checkExp (Stream _ _ _ _ pos) =
  bad $ TypeError pos "Stream with lambda NOT an anonymous function!!!!"

checkExp (Split splitexps arrexp pos) = do
  splitexps' <- mapM (require [Prim $ IntType Int32] <=< checkExp) splitexps
  arrexp' <- checkExp arrexp
  _ <- rowTypeM arrexp' -- Just check that it's an array.
  return $ Split splitexps' arrexp' pos

checkExp (Concat arr1exp arr2exps pos) = do
  arr1exp'  <- checkExp arr1exp
  arr2exps' <- mapM (require [typeOf arr1exp'] <=< checkExp) arr2exps
  mapM_ rowTypeM arr2exps' -- Just check that it's an array.
  return $ Concat arr1exp' arr2exps' pos

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
checkExp (DoLoop mergepat mergeexp form loopbody letbody loc) = do
  -- First, check the bound and initial merge expression and throw
  -- away the dataflow.  The dataflow will be reconstructed later, but
  -- we need the result of this to synthesize the function.
  (mergeexp', bindExtra) <-
    noDataflow $ do
      mergeexp' <- checkExp mergeexp
      return $
        case form of
          For _ _ (Ident loopvar _ _) _ ->
            let iparam = Ident loopvar (Prim $ IntType Int32) loc
            in (mergeexp', [iparam])
          While _ ->
            (mergeexp', [])

  -- Check the loop body.
  (firstscope, mergepat') <- checkBinding mergepat (typeOf mergeexp') mempty
  (loopbody', form', extraargs, letExtra, boundExtra, extraparams, freeInForm) <-
    binding bindExtra $ noDataflow $
    case form of
      For dir lboundexp (Ident loopvar _ loopvarloc) uboundexp -> do
        lboundexp' <- require [Prim $ IntType Int32] =<< checkExp lboundexp
        uboundexp' <- require [Prim $ IntType Int32] =<< checkExp uboundexp
        firstscope $ do
          loopbody' <- checkExp loopbody
          let iparam = Ident loopvar (Prim $ IntType Int32) loc
          return (loopbody',
                  For dir lboundexp' (Ident loopvar (Prim $ IntType Int32) loopvarloc) uboundexp',
                  [(Literal (PrimValue $ IntValue $ Int32Value 0) loc, Observe)],
                  id,
                  HS.singleton iparam,
                  [iparam],
                  mempty)
      While condexp -> firstscope $ do
        (condexp', condflow) <-
          collectDataflow $ require [Prim Bool] =<< checkExp condexp
        (loopbody', bodyflow) <-
          collectDataflow $ checkExp loopbody
        occur $ usageOccurences condflow `seqOccurences`
                usageOccurences bodyflow
        cond <- newIdent "loop_cond" (Prim Bool) loc
        return (loopbody',
                While condexp',
                [(Var cond, Observe)],
                \inner -> LetPat (Id cond) condexp'
                          inner (srclocOf mergeexp),
                HS.empty,
                [toParam cond],
                freeInExp condexp')

  -- We can use the name generator in a slightly hacky way to generate
  -- a unique Name for the function.
  fname <- newFname "loop_fun"

  let rettype = vacuousShapeAnnotations $
                case map (toDecl . identType) $ patIdents mergepat' of
                  [t] -> t
                  ts  -> Tuple ts
      rettype' = removeShapeAnnotations $ fromDecl rettype

  merge <- newIdent "merge_val" rettype' $ srclocOf mergeexp'

  let boundnames = boundExtra `HS.union` patIdentSet mergepat'
      ununique ident =
        ident { identType = toDecl $ identType ident `setUniqueness` Nonunique }
      -- Find the free variables of the loop body.
      free = map ununique $ HS.toList $
             (freeInExp loopbody' <> freeInForm)
             `HS.difference` boundnames

      -- These are the parameters expected by the function: All of the
      -- free variables, followed by the merge value, followed by
      -- whatever needed by the loop form.
      params = map toParam free ++
               [toParam merge] ++
               extraparams
      bindfun env = env { envFtable = HM.insert fname
                                      (rettype,
                                       map (vacuousShapeAnnotations . identType) params) $
                                      envFtable env }

      -- The body of the function will be the loop body, but with all
      -- tails replaced with recursive calls.
      recurse e = Apply fname
                    ([(Var (fromParam k), diet (identType k)) | k <- free ] ++
                     [(e, diet $ typeOf e)] ++
                     extraargs)
                    rettype' (srclocOf e)
      funbody' = LetPat mergepat' (Var merge) (mapTails recurse id $ letExtra loopbody')
                 (srclocOf loopbody')

  (funcall, callflow) <- collectDataflow $ local bindfun $ do
    -- Check that the function is internally consistent.
    _ <- unbinding $ checkFun (fname, rettype, params, funbody', loc)
    -- Check the actual function call - we start by computing the
    -- bound and initial merge value, in case they use something
    -- consumed in the call.  This reintroduces the dataflow for
    -- boundexp and mergeexp that we previously threw away.
    let call = LetPat (Id merge) mergeexp'
               (LetPat mergepat' (Var merge)
                (letExtra
                 (Apply fname
                  ([(Var (fromParam k), diet (identType k)) | k <- free ] ++
                   [(Var merge, diet rettype)] ++
                   extraargs)
                  rettype' $ srclocOf mergeexp))
                (srclocOf mergeexp))
               (srclocOf mergeexp)
    checkExp call
  -- Now we just need to bind the result of the function call to the
  -- original merge pattern...
  (secondscope, _) <- checkBinding mergepat (typeOf funcall) callflow

  -- And then check the let-body.
  secondscope $ do
    letbody' <- checkExp letbody
    return $ DoLoop mergepat' mergeexp'
                    form'
                    loopbody' letbody' loc

checkSOACArrayArg :: (TypeBox ty, VarName vn) =>
                     TaggedExp ty vn -> TypeM vn (TaggedExp CompTypeBase vn, Arg vn)
checkSOACArrayArg e = do
  (e', (t, dflow, argloc)) <- checkArg e
  case peelArray 1 t of
    Nothing -> bad $ TypeError argloc "SOAC argument is not an array"
    Just rt -> return (e', (rt, dflow, argloc))

checkLiteral :: VarName vn => SrcLoc -> Value -> TypeM vn Value
checkLiteral _ (PrimValue bv) = return $ PrimValue bv
checkLiteral loc (TupValue vals) = do
  vals' <- mapM (checkLiteral loc) vals
  return $ TupValue vals'
checkLiteral loc (ArrayValue arr rt) = do
  vals <- mapM (checkLiteral loc) (elems arr)
  case find ((/=rt) . removeNames . valueType) vals of
    Just wrong -> bad $ TypeError loc $ ppValue wrong ++ " is not of expected type " ++ ppType rt ++ "."
    _          -> return ()
  return $ ArrayValue (listArray (bounds arr) vals) rt

checkIdent :: (TypeBox ty, VarName vn) =>
              TaggedIdent ty vn -> TypeM vn (TaggedIdent CompTypeBase vn)
checkIdent (Ident name t pos) = do
  vt <- lookupVar name pos
  t' <- checkAnnotation pos ("variable " ++ textual (baseName name)) t vt
  return $ Ident name t' pos

checkBinOp :: (TypeBox ty, VarName vn) =>
              BinOp -> TaggedExp ty vn -> TaggedExp ty vn -> ty (ID vn) -> SrcLoc
           -> TypeM vn (TaggedExp CompTypeBase vn)
checkBinOp Plus e1 e2 t pos = checkPolyBinOp Plus anyNumberType e1 e2 t pos
checkBinOp Minus e1 e2 t pos = checkPolyBinOp Minus anyNumberType e1 e2 t pos
checkBinOp Pow e1 e2 t pos = checkPolyBinOp Pow anyNumberType e1 e2 t pos
checkBinOp Times e1 e2 t pos = checkPolyBinOp Times anyNumberType e1 e2 t pos
checkBinOp Divide e1 e2 t pos = checkPolyBinOp Divide anyNumberType e1 e2 t pos
checkBinOp Mod e1 e2 t pos = checkPolyBinOp Mod anyIntType e1 e2 t pos
checkBinOp Quot e1 e2 t pos = checkPolyBinOp Quot anyIntType e1 e2 t pos
checkBinOp Rem e1 e2 t pos = checkPolyBinOp Rem anyIntType e1 e2 t pos
checkBinOp ShiftR e1 e2 t pos = checkPolyBinOp ShiftR anyIntType e1 e2 t pos
checkBinOp ZShiftR e1 e2 t pos = checkPolyBinOp ZShiftR anyIntType e1 e2 t pos
checkBinOp ShiftL e1 e2 t pos = checkPolyBinOp ShiftL anyIntType e1 e2 t pos
checkBinOp Band e1 e2 t pos = checkPolyBinOp Band anyIntType e1 e2 t pos
checkBinOp Xor e1 e2 t pos = checkPolyBinOp Xor anyIntType e1 e2 t pos
checkBinOp Bor e1 e2 t pos = checkPolyBinOp Bor anyIntType e1 e2 t pos
checkBinOp LogAnd e1 e2 t pos = checkPolyBinOp LogAnd [Prim Bool] e1 e2 t pos
checkBinOp LogOr e1 e2 t pos = checkPolyBinOp LogOr [Prim Bool] e1 e2 t pos
checkBinOp Equal e1 e2 t pos = checkRelOp Equal anyNumberType e1 e2 t pos
checkBinOp Less e1 e2 t pos = checkRelOp Less anyNumberType e1 e2 t pos
checkBinOp Leq e1 e2 t pos = checkRelOp Leq anyNumberType e1 e2 t pos
checkBinOp Greater e1 e2 t pos = checkRelOp Greater anyNumberType e1 e2 t pos
checkBinOp Geq e1 e2 t pos = checkRelOp Geq anyNumberType e1 e2 t pos

checkRelOp :: (TypeBox ty, VarName vn) =>
              BinOp -> [TaggedType vn]
           -> TaggedExp ty vn -> TaggedExp ty vn
           -> ty (ID vn) -> SrcLoc
           -> TypeM vn (TaggedExp CompTypeBase vn)
checkRelOp op tl e1 e2 t pos = do
  e1' <- require tl =<< checkExp e1
  e2' <- require tl =<< checkExp e2
  _ <- unifyExpTypes e1' e2'
  t' <- checkAnnotation pos (ppBinOp op ++ " result") t $ Prim Bool
  return $ BinOp op e1' e2' t' pos

checkPolyBinOp :: (TypeBox ty, VarName vn) =>
                  BinOp -> [TaggedType vn]
               -> TaggedExp ty vn -> TaggedExp ty vn -> ty (ID vn) -> SrcLoc
               -> TypeM vn (TaggedExp CompTypeBase vn)
checkPolyBinOp op tl e1 e2 t pos = do
  e1' <- require tl =<< checkExp e1
  e2' <- require tl =<< checkExp e2
  t' <- unifyExpTypes e1' e2'
  t'' <- checkAnnotation pos (ppBinOp op ++ " result") t t'
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
  (pat', idds) <-
    runStateT (checkBinding' pat et) []
  return (\m -> sequentially (tell dflow) (const . const $ binding idds m), pat')
  where checkBinding' (Id (Ident name namet pos)) t = do
          t' <- lift $
                checkAnnotation (srclocOf pat)
                ("binding of variable " ++ textual (baseName name)) namet t
          let t'' = typeOf $ Var $ Ident name t' pos
          add $ Ident name t'' pos
          return $ Id $ Ident name t'' pos
        checkBinding' (TupId pats pos) (Tuple ts)
          | length pats == length ts = do
          pats' <- zipWithM checkBinding' pats ts
          return $ TupId pats' pos
        checkBinding' (Wildcard wt loc) t = do
          t' <- lift $ checkAnnotation (srclocOf pat) "wildcard" wt t
          return $ Wildcard t' loc
        checkBinding' _ _ =
          lift $ bad $ InvalidPatternError
                       (untagPattern errpat) (toStructural et)
                       Nothing $ srclocOf pat

        add ident = do
          bnd <- gets $ find (==ident)
          case bnd of
            Nothing -> modify (ident:)
            Just (Ident name _ pos2) ->
              lift $ bad $ DupPatternError (baseName name) (srclocOf ident) pos2
        -- A pattern with known type box (NoInfo) for error messages.
        errpat = rmTypes pat
        rmTypes (Id (Ident name _ pos)) = Id $ Ident name NoInfo pos
        rmTypes (TupId pats pos) = TupId (map rmTypes pats) pos
        rmTypes (Wildcard _ loc) = Wildcard NoInfo loc

validApply :: VarName vn =>
              [DeclTypeBase (ID vn)] -> [TaggedType vn] -> Bool
validApply expected got =
  length got == length expected &&
  and (zipWith subtypeOf (map toStructural got) (map toStructural expected))

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
          (Right $ map toStructural paramtypes) (map toStructural argts)

  forM_ (zip (map diet paramtypes) args) $ \(d, (t, dflow, argloc)) -> do
    maybeCheckOccurences $ usageOccurences  dflow
    let occurs = consumeArg argloc t d
    occur $ usageOccurences dflow `seqOccurences` occurs

consumeArg :: SrcLoc -> TaggedType vn -> Diet -> [Occurence vn]
consumeArg loc (Tuple ets) (TupleDiet ds) =
  concat $ zipWith (consumeArg loc) ets ds
consumeArg loc at Consume = [consumption (aliases at) loc]
consumeArg loc at _       = [observation (aliases at) loc]

checkLambda :: (TypeBox ty, VarName vn) =>
               TaggedLambda ty vn -> [Arg vn] -> TypeM vn (TaggedLambda CompTypeBase vn)
checkLambda (AnonymFun params body ret pos) args =
  case () of
    _ | length params == length args -> do
          checkFuncall Nothing pos (map identType params) ret args
          (_, ret', params', body', _) <-
            noUnique $ checkFun (nameFromString "<anonymous>", ret, params, body, pos)
          return $ AnonymFun params' body' ret' pos
      | [(Tuple ets, _, _)] <- args,
        validApply (map identType params) ets -> do
          -- The function expects N parameters, but the argument is a
          -- single N-tuple whose types match the parameters.
          -- Generate a shim to make it fit.
          (_, ret', _, body', _) <-
            noUnique $ checkFun (nameFromString "<anonymous>", ret, params, body, pos)
          tupparam <- newIdent "tup_shim"
                      (Tuple $ map (fromDecl .
                                    removeShapeAnnotations .
                                    identType) params)
                      pos
          let tupfun = AnonymFun [toParam tupparam] tuplet ret pos
              tuplet = LetPat (TupId (map (Id . fromParam) params) pos)
                              (Var tupparam) body' pos
          _ <- checkLambda tupfun args
          return $ AnonymFun params body' ret' pos
      | otherwise -> bad $ TypeError pos $ "Anonymous function defined with " ++ show (length params) ++ " parameters, but expected to take " ++ show (length args) ++ " arguments."

checkLambda (CurryFun fname curryargexps rettype pos) args = do
  (curryargexps', curryargs) <- unzip <$> mapM checkArg curryargexps
  bnd <- asks $ HM.lookup fname . envFtable
  case bnd of
    Nothing -> bad $ UnknownFunctionError fname pos
    Just (rt, paramtypes) -> do
      rettype' <- checkAnnotation pos "return" rettype $
                  fromDecl $ removeShapeAnnotations rt
      let paramtypes' = map (fromDecl . removeShapeAnnotations) paramtypes
      case () of
        _ | [(Tuple ets, _, _)] <- args,
            validApply paramtypes ets -> do
              -- Same shimming as in the case for anonymous functions.
              let mkparam i t = newIdent ("param_" ++ show i) t pos
                  tupt = Tuple $ zipWith setUniqueness ets $
                         map uniqueness paramtypes
              params <- zipWithM mkparam [(0::Int)..] paramtypes'
              tupparam <- newIdent "x" (removeShapeAnnotations tupt) pos
              let tuplet = LetPat (TupId (map Id params) pos) (Var tupparam) body pos
                  tupfun = AnonymFun [toParam tupparam] tuplet
                           (vacuousShapeAnnotations rt) pos
                  body = Apply fname [(Var param, diet paramt) |
                                      (param, paramt) <- zip params paramtypes']
                         rettype' pos
              checkLambda tupfun args
          | otherwise -> do
              case find (unique . snd) $ zip curryargexps paramtypes of
                Just (e, _) -> bad $ CurriedConsumption fname $ srclocOf e
                _           -> return ()
              let mkparam i t = newIdent ("param_" ++ show i) t pos
              params <- zipWithM mkparam [(0::Int)..] $
                        drop (length curryargs) paramtypes'
              let fun = AnonymFun (map toParam params) body
                        (vacuousShapeAnnotations rt) pos
                  body = Apply fname (zip (curryargexps'++map Var params) $
                                      map diet paramtypes)
                         rettype' pos
              _ <- checkLambda fun args
              return $ CurryFun fname curryargexps' rettype' pos

checkLambda (UnOpFun unop paramtype rettype loc) [arg] = do
  var <- newIdent "x" (argType arg) loc
  binding [var] $ do
    e <- checkExp $ UnOp unop (Var var) loc
    paramtype' <- checkAnnotation loc "param" paramtype $ argType arg
    rettype' <- checkAnnotation loc "return" rettype $ typeOf e
    return $ UnOpFun unop paramtype' rettype' loc

checkLambda (UnOpFun unop _ _ loc) args =
  bad $ ParameterMismatch (Just $ nameFromString $ ppUnOp unop) loc (Left 1) $
  map (toStructural . argType) args

checkLambda (BinOpFun op _ _ rettype loc) args =
  checkPolyLambdaOp op [] rettype args loc

checkLambda (CurryBinOpLeft binop x paramtype rettype loc) [arg] = do
  x' <- checkExp x
  y <- newIdent "y" (argType arg) loc
  xvar <- newIdent "x" (typeOf x') loc
  binding [y, xvar] $ do
    e <- checkExp $ BinOp binop (Var $ untype xvar) (Var $ untype y) NoInfo loc
    rettype' <- checkAnnotation loc "return" rettype $ typeOf e
    paramtype' <- checkAnnotation loc "param" paramtype $ argType arg
    return $ CurryBinOpLeft binop x' paramtype' rettype' loc
  where untype (Ident name _ varloc) = Ident name NoInfo varloc

checkLambda (CurryBinOpLeft binop _ _ _ loc) args =
  bad $ ParameterMismatch (Just $ nameFromString $ ppBinOp binop) loc (Left 1) $
  map (toStructural . argType) args

checkLambda (CurryBinOpRight binop x paramtype rettype loc) [arg] = do
  x' <- checkExp x
  y <- newIdent "y" (argType arg) loc
  xvar <- newIdent "x" (typeOf x') loc
  binding [y, xvar] $ do
    e <- checkExp $ BinOp binop (Var $ untype y) (Var $ untype xvar) NoInfo loc
    rettype' <- checkAnnotation loc "return" rettype $ typeOf e
    paramtype' <- checkAnnotation loc "param" paramtype $ argType arg
    return $ CurryBinOpRight binop x' paramtype' rettype' loc
  where untype (Ident name _ varloc) = Ident name NoInfo varloc

checkLambda (CurryBinOpRight binop _ _ _ loc) args =
  bad $ ParameterMismatch (Just $ nameFromString $ ppBinOp binop) loc (Left 1) $
  map (toStructural . argType) args

checkPolyLambdaOp :: (TypeBox ty, VarName vn) =>
                     BinOp -> [TaggedExp ty vn] -> ty (ID vn) -> [Arg vn] -> SrcLoc
                  -> TypeM vn (TaggedLambda CompTypeBase vn)
checkPolyLambdaOp op curryargexps rettype args pos = do
  curryargexpts <- map typeOf <$> mapM checkExp curryargexps
  let argts = [ argt | (argt, _, _) <- args ]
  tp <- case curryargexpts ++ argts of
          [t1, t2] | t1 == t2 -> return t1
          [Tuple [t1,t2]] | t1 == t2 -> return t1 -- For autoshimming.
          l -> bad $ ParameterMismatch (Just fname) pos (Left 2) $ map toStructural l
  xname <- newIDFromString "x"
  yname <- newIDFromString "y"
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
  checkLambda
    (AnonymFun (map toParam params) body
     (vacuousShapeAnnotations $ toDecl $ typeOf body) pos)
    args
  where fname = nameFromString $ ppBinOp op

checkRetType :: VarName vn =>
                SrcLoc -> TaggedDeclType vn -> TypeM vn ()
checkRetType loc (Tuple ts) = mapM_ (checkRetType loc) ts
checkRetType _ (Prim _) = return ()
checkRetType loc (Array at) =
  checkArrayType loc at

checkArrayType :: VarName vn =>
                  SrcLoc
               -> DeclArrayTypeBase (ID vn)
               -> TypeM vn ()
checkArrayType loc (PrimArray _ ds _ _) =
  mapM_ (checkDim loc) $ shapeDims ds
checkArrayType loc (TupleArray cts ds _) = do
  mapM_ (checkDim loc) $ shapeDims ds
  mapM_ (checkTupleArrayElem loc) cts

checkTupleArrayElem :: VarName vn =>
                       SrcLoc
                    -> DeclTupleArrayElemTypeBase (ID vn)
                    -> TypeM vn ()
checkTupleArrayElem _ PrimArrayElem{} =
  return ()
checkTupleArrayElem loc (ArrayArrayElem at) =
  checkArrayType loc at
checkTupleArrayElem loc (TupleArrayElem cts) =
  mapM_ (checkTupleArrayElem loc) cts

checkDim :: VarName vn =>
            SrcLoc -> DimDecl (ID vn) -> TypeM vn ()
checkDim _ AnyDim =
  return ()
checkDim _ (ConstDim _) =
  return ()
checkDim loc (NamedDim name) = do
  t <- lookupVar name loc
  case t of
    Prim (IntType Int32) -> return ()
    _                    -> bad $ DimensionNotInteger loc $ baseName name
