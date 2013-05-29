{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module L0.TypeChecker ( checkProg
                      , checkProgNoUniqueness
                      , TypeError(..))
  where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.Array
import Data.List
import Data.Loc
import Data.Maybe
import qualified Data.Traversable as T

import qualified Data.Map as M
import qualified Data.Set as S

import L0.AbSyn
import L0.Traversals

-- | Information about an error during type checking.  The 'Show'
-- instance for this type produces a human-readable description.
data TypeError = TypeError SrcLoc String
               -- ^ A general error happened at the given position and
               -- for the given reason.
               | UnifyError (Exp Type) (Exp Type)
               -- ^ Types of two expressions failed to unify.
               | UnexpectedType (Exp Type) [Type]
               -- ^ Expression of type was not one of the expected
               -- types.
               | ReturnTypeError SrcLoc Name Type Type
               -- ^ The body of a function definition has a different
               -- type than its declaration.
               | DupDefinitionError Name SrcLoc SrcLoc
               -- ^ Two functions have been defined with the same name.
               | DupParamError Name Name SrcLoc
               -- ^ Two function parameters share the same name.
               | DupPatternError Name SrcLoc SrcLoc
               -- ^ Two pattern variables share the same name.
               | InvalidPatternError (TupIdent (Maybe Type)) (Exp Type)
               -- ^ The pattern is not compatible with the type.
               | UnknownVariableError Name SrcLoc
               -- ^ Unknown variable of the given name referenced at the given spot.
               | UnknownFunctionError Name SrcLoc
               -- ^ Unknown function of the given name called at the given spot.
               | ParameterMismatch (Maybe Name) SrcLoc (Either Int [Type]) [Type]
               -- ^ A function (possibly anonymous) was called with
               -- invalid arguments.  The third argument is either the
               -- number of parameters, or the specific types of
               -- parameters accepted (sometimes, only the former can
               -- be determined).
               | UseAfterConsume Name SrcLoc SrcLoc
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
               | FreeConsumption Name SrcLoc
               | ReturnAliased Name Name SrcLoc

instance Show TypeError where
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
    "Parameter " ++ nameToString paramname ++
    " mentioned multiple times in argument list of function " ++
    nameToString funname ++ " at " ++ locStr pos ++ "."
  show (DupPatternError name pos1 pos2) =
    "Variable " ++ nameToString name ++ " bound twice in tuple pattern; at " ++
    locStr pos1 ++ " and " ++ locStr pos2 ++ "."
  show (InvalidPatternError pat e) =
    "Pattern " ++ ppTupId pat ++ " at " ++ locStr (srclocOf pat) ++
    " cannot match value of type " ++ ppType (typeOf e) ++ " at " ++ locStr (srclocOf e) ++ "."
  show (UnknownVariableError name pos) =
    "Unknown variable " ++ nameToString name ++ " referenced at " ++ locStr pos ++ "."
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
    "Varible " ++ nameToString name ++ " used at " ++ locStr rloc ++
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
  show (FreeConsumption name loc) =
    "Variable " ++ nameToString name ++ " consumed at " ++
    locStr loc ++ ", but is free inside loop body."
  show (ReturnAliased fname name loc) =
    "Unique return value of function " ++ nameToString fname ++ " at " ++
    locStr loc ++ " is aliased to " ++ nameToString name ++ ", which is not consumed."

-- | A tuple of a return type and a list of argument types.
type FunBinding = (Type, [Type])

-- | Information about the aliasing of the value returned by an expression.
data Aliases = VarAlias (S.Set Name)
             -- ^ May alias these variables.
             | TupleAlias [Aliases]
             -- ^ Aliasing information for specific components of a
             -- tuple.
               deriving (Show)

instance Monoid Aliases where
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
unalias :: Name -> Aliases -> Aliases
unalias name (VarAlias names) = VarAlias $ name `S.delete` names
unalias name (TupleAlias alss) = TupleAlias $ map (unalias name) alss

-- | All the variables represented in this aliasing.  Guaranteed not
-- to contain duplicates.
aliased :: Aliases -> [Name]
aliased (VarAlias names) = S.toList names
aliased (TupleAlias ass) = nub $ concatMap aliased ass

-- | Create an aliasing set given a list of names.
varAlias :: [Name] -> Aliases
varAlias = VarAlias . S.fromList

-- | A pair of a variable table and a function table.  Type checking
-- happens with access to this environment.  The function table is
-- only initialised at the very beginning, but the variable table will
-- be extended during type-checking when let-expressions are
-- encountered.
data TypeEnv = TypeEnv { envVtable :: M.Map Name Type
                       , envFtable :: M.Map Name FunBinding
                       , envAliases :: M.Map Name Aliases
                       , envConsumed :: M.Map Name SrcLoc
                       , envCheckOccurences :: Bool
                       }

data Usage = Observed SrcLoc
           | Consumed SrcLoc
             deriving (Eq, Ord, Show)

type Usages = M.Map Name [Usage]

combineUsages :: Name -> Usage -> Usage -> Either TypeError Usage
combineUsages _ (Observed loc) (Observed _) = Right $ Observed loc
combineUsages name (Consumed wloc) (Observed rloc) =
  Left $ UseAfterConsume name rloc wloc
combineUsages name (Observed rloc) (Consumed wloc) =
  Left $ UseAfterConsume name rloc wloc
combineUsages name (Consumed loc1) (Consumed loc2) =
  Left $ UseAfterConsume name (max loc1 loc2) (min loc1 loc2)

checkUsages :: Usages -> Either TypeError Usages
checkUsages = T.sequence . M.mapWithKey comb
  where comb _    []     = Right []
        comb name (u:us) = (:[]) <$> foldM (combineUsages name) u us

seqUsages :: Usages -> Usages -> Usages
seqUsages = M.unionWith comb
  where comb pre post
          | Consumed noLoc `elem` post =
            filter (not . (==Observed noLoc)) pre ++ post
          | otherwise = pre ++ post

-- | The 'VarUsage' data structure is used to keep track of which
-- variables have been referenced inside an expression, as well as
-- which variables the resulting expression may possibly alias.
data Dataflow = Dataflow {
    usageOccurences :: Usages
  , usageAliasing :: Aliases
  } deriving (Show)

instance Monoid Dataflow where
  mempty = Dataflow mempty mempty
  Dataflow m1 s1 `mappend` Dataflow m2 s2 =
    Dataflow (M.unionWith (++) m1 m2) (s1 <> s2)

consumptions :: Dataflow -> M.Map Name [SrcLoc]
consumptions (Dataflow occurs _) = M.mapMaybe f occurs
  where f l = case concatMap isConsumption l of
                [] -> Nothing
                l' -> Just l'
        isConsumption (Consumed loc) = [loc]
        isConsumption _              = []

-- | The type checker runs in this monad.  Note that it has no mutable
-- state, but merely keeps track of current bindings in a 'TypeEnv'.
-- The 'Either' monad is used for error handling.
newtype TypeM a = TypeM (WriterT Dataflow (ReaderT TypeEnv (Either TypeError)) a)
  deriving (Monad, Functor, Applicative, MonadReader TypeEnv, MonadWriter Dataflow)

runTypeM :: TypeEnv -> TypeM a -> Either TypeError a
runTypeM env (TypeM m) = fst <$> runReaderT (runWriterT m) env

bad :: TypeError -> TypeM a
bad = TypeM . lift . lift . Left

liftEither :: Either TypeError a -> TypeM a
liftEither = either bad return

-- | Proclaim that we have made read-only use of the given variable!
observe :: Ident Type -> TypeM ()
observe (Ident name _ loc) = do
  als <- aliases name
  tell $ mempty { usageOccurences = M.singleton name [Observed loc]
                , usageAliasing = als }

alias :: Aliases -> TypeM ()
alias al = tell $ mempty { usageAliasing = al }

-- | Proclaim that we have written to the given variable.
consume :: SrcLoc -> Aliases -> TypeM ()
consume loc als =
  let occurs = M.fromList [ (name, [Consumed loc]) | name <- aliased als ]
  in tell $ mempty { usageOccurences = occurs }

-- | Proclaim that we have written to the given variable, and mark
-- accesses to it and all of its aliases as invalid inside the given
-- computation.
consuming :: SrcLoc -> Name -> TypeM a -> TypeM a
consuming loc name m = do
  consume loc =<< aliases name
  local consume' m
  where consume' env =
          env { envConsumed = M.insert name loc $ envConsumed env }

collectAliases :: TypeM a -> TypeM (a, Aliases)
collectAliases m = pass $ do
  (x, usage) <- listen m
  return ((x, usageAliasing usage),
          const $ usage { usageAliasing = blank $ usageAliasing usage })
  where blank (VarAlias _) = VarAlias S.empty
        blank (TupleAlias alss) = TupleAlias $ map blank alss

collectDataflow :: TypeM a -> TypeM (a, Dataflow)
collectDataflow m = pass $ do
  (x, dataflow) <- listen m
  return ((x, dataflow), const mempty)

checkOccurences :: Usages -> TypeM Usages
checkOccurences us = do
  check <- asks envCheckOccurences
  if check then liftEither $ checkUsages us
  else return us

alternative :: TypeM a -> TypeM b -> TypeM (a,b)
alternative m1 m2 = pass $ do
  (x, Dataflow occurs1 als1) <- listen m1
  (y, Dataflow occurs2 als2) <- listen m2
  occurs1' <- checkOccurences occurs1
  occurs2' <- checkOccurences occurs2
  let usage = Dataflow (M.unionWith max occurs1' occurs2') (als1 <> als2)
  return ((x, y), const usage)

aliasing :: Name -> Aliases -> TypeM a -> TypeM a
aliasing name als tm = do
  let name' = varAlias [name]
      outedges = M.insert name als
      -- Edges from everything in als to name:
      inedges m =  foldl (\m' v -> M.insertWith (<>) v name' m') m $ aliased als
  local (\env -> env { envAliases = inedges $ outedges $ envAliases env }) tm

aliases :: Name -> TypeM Aliases
aliases name = asks $ maybe name' reflexive . M.lookup name . envAliases
  where name' = varAlias [name]
        reflexive als | VarAlias _ <- als = als <> name'
                      | otherwise         = als

binding :: [Ident Type] -> TypeM a -> TypeM a
binding bnds = check bnds . local (`bindVars` bnds)
  where bindVars = foldl bindVar

        -- Since the type checker does not require unique names,
        -- we need to remove any existing information about
        -- variables of the same name.
        bindVar env (Ident name tp _) =
          env { envVtable = M.insert name tp $ envVtable env
              , envConsumed = M.delete name $ envConsumed env }

        -- Check whether the bound variables have been used correctly
        -- within their scope.
        check vars m = do
          (a, usages) <- collectOccurences (map identName vars) m
          void $ checkOccurences usages
          return a

        -- Collect and remove all occurences in @names@.  Also remove
        -- these names from aliasing information, since they are now
        -- out of scope.
        collectOccurences names m = pass $ do
          (x, usage) <- listen m
          let (relevant, rest) = split $ usageOccurences usage
              newaliases = foldr unalias (usageAliasing usage) names
          return ((x, relevant),
                  const $ usage { usageOccurences = rest
                                , usageAliasing = newaliases })
          where split = M.partitionWithKey $ const . (`elem` names)

lookupVar :: Name -> SrcLoc -> TypeM Type
lookupVar name pos = do
  bnd <- asks $ M.lookup name . envVtable
  consumed <- asks $ M.lookup name . envConsumed
  check <- asks envCheckOccurences
  case (bnd, consumed, check) of
    (_, Just wloc, True) -> bad $ UseAfterConsume name pos wloc
    (Nothing, _, _)      -> bad $ UnknownVariableError name pos
    (Just t, _, _)       -> return t

-- | Determine if two types are identical, ignoring uniqueness.
-- Causes a 'TypeError' if they fail to match, and otherwise returns
-- one of them.
unifyExpTypes :: Exp Type -> Exp Type -> TypeM Type
unifyExpTypes e1 e2 =
  maybe (bad $ UnifyError e1 e2) return $
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
checkAnnotation :: TypeBox tf => SrcLoc -> String -> tf -> Type -> TypeM Type
checkAnnotation loc desc t1 t2 =
  case unboxType t1 of
    Nothing -> return t2
    Just t1' -> case unifyKnownTypes t1' t2 of
                  Nothing -> bad $ BadAnnotation loc desc t1' t2
                  Just t  -> return t

-- | @require ts e@ causes a 'TypeError' if @typeOf e@ does not unify
-- with one of the types in @ts@.  Otherwise, simply returns @e@.
-- This function is very useful in 'checkExp'.
require :: [Type] -> Exp Type -> TypeM (Exp Type)
require ts e
  | any (typeOf e `similarTo`) ts = return e
  | otherwise = bad $ UnexpectedType e ts

rowTypeM :: Exp Type -> TypeM Type
rowTypeM e = maybe wrong return $ peelArray 1 $ typeOf e
  where wrong = bad $ TypeError (srclocOf e) $ "Type of expression is not array, but " ++ ppType (typeOf e) ++ "."

-- | Type check a program containing arbitrary type information,
-- yielding either a type error or a program with complete type
-- information.
checkProg :: TypeBox tf => Prog tf -> Either TypeError (Prog Type)
checkProg = checkProg' True

-- | As 'checkProg', but don't check whether uniqueness constraints
-- are being upheld.  The uniqueness of types must still be correct.
checkProgNoUniqueness :: TypeBox tf => Prog tf -> Either TypeError (Prog Type)
checkProgNoUniqueness = checkProg' False

checkProg' :: TypeBox tf => Bool -> Prog tf -> Either TypeError (Prog Type)
checkProg' checkoccurs prog = do
  ftable <- buildFtable
  let typeenv = TypeEnv { envVtable = M.empty
                        , envFtable = ftable
                        , envAliases = M.empty
                        , envConsumed = M.empty
                        , envCheckOccurences = checkoccurs
                        }
  runTypeM typeenv $ mapM checkFun prog
  where
    -- To build the ftable we loop through the list of function
    -- definitions.  In addition to the normal ftable information
    -- (name, return type, argument types), we also keep track of
    -- position information, in order to report both locations of
    -- duplicate function definitions.  The position information is
    -- removed at the end.
    buildFtable = M.map rmLoc <$> foldM expand builtins prog
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

checkFun :: TypeBox tf => FunDec tf -> TypeM (FunDec Type)
checkFun (fname, rettype, args, body, loc) = do
  args' <- checkArgs
  (body', dataflow) <-
    collectDataflow $ binding args' $ checkExp body
  when (unique rettype) $ checkReturnAlias dataflow
  if typeOf body' `subtypeOf` rettype then
    return (fname, rettype, args, body', loc)
  else bad $ ReturnTypeError loc fname rettype $ typeOf body'
  where checkArgs = foldM expand [] args

        expand args' ident@(Ident pname _ _)
          | Just _ <- find ((==identName ident) . identName) args' =
            bad $ DupParamError fname pname loc
          | otherwise =
            return $ ident : args'

        -- | Check that the unique return value does not alias a
        -- non-consumed parameter.
        checkReturnAlias dataflow =
          forM_ args $ \arg ->
            when (not (unique $ typeOf arg) &&
                  identName arg `elem` als) $
              bad $ ReturnAliased fname (identName arg) loc
            where als = aliased (usageAliasing dataflow)

checkExp :: TypeBox tf => Exp tf -> TypeM (Exp Type)
checkExp e = do
  (e', als) <- collectAliases $ checkExp' e
  unless (basicType $ typeOf e') $
    tell $ mempty { usageAliasing = als }
  return e'

-- | Never call checkExp' directly!  Call checkExp!
checkExp' :: TypeBox tf => Exp tf -> TypeM (Exp Type)

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
                    bad $ TypeError loc $ ppExp 0 eleme ++ " is not of expected type " ++ ppType elemt ++ "."
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
                 occurs1 <- checkOccurences $ usageOccurences dflow

                 let occurs2 = consumeArg (srclocOf arg) paramt $ usageAliasing dflow
                     als
                       -- If the return type is unique, we assume that
                       -- the value does not alias the parameters.
                       | unique ftype = mempty
                       | otherwise = varAlias $ aliased $ usageAliasing dflow

                 tell $ mempty
                        { usageAliasing = als
                        , usageOccurences = M.unionWith max occurs1 occurs2 }
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
          mconcat $ zipWith (consumeArg loc) ets alss
        consumeArg loc t als
          | consumes t =
            mconcat $ map (`M.singleton` [Consumed loc]) $ aliased als
          | otherwise = mempty

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
    bad $ TypeError pos "Source is not unique"

  case peelArray (length idxes) (identType src') of
    Nothing -> bad $ IndexingError (arrayDims $ identType src') (length idxes) (srclocOf src)
    Just elemt ->
      sequentially (require [elemt] =<< checkExp ve) $ \ve' dflow -> do
        when (identName src `elem` aliased (usageAliasing dflow)) $
          bad $ BadLetWithValue pos
        (scope, _) <- checkBinding (Id dest') (Var src') mempty
        body' <- consuming (srclocOf src) (identName src') $ scope $ checkExp body
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
      checkloop scope = collectDataflow $
                        scope $ binding [Ident loopvar (Elem Int) pos] $
                        require [mergetype] =<< checkExp loopbody
  (firstscope, mergepat') <- checkBinding mergepat mergeexp' mergeflow
  (_, dataflow) <- checkloop firstscope
  (secondscope, _) <- checkBinding mergepat mergeexp' dataflow
  (loopbody', secondflow) <- checkloop secondscope

  checkoccurs <- asks envCheckOccurences
  when checkoccurs $
    case M.toList $ free (patNames mergepat) secondflow of
      (v,loc:_):_ -> bad $ FreeConsumption v loc
      _ -> return ()

  secondscope $ do
    letbody' <- checkExp letbody
    return $ DoLoop mergepat' mergeexp' (Ident loopvar (Elem Int) pos) boundexp' loopbody' letbody' pos
  where free names = M.filterWithKey (\x _ -> not $ x `S.member` names) . consumptions

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

soac2ArgType :: SrcLoc -> String -> [Type] -> TypeM Type
soac2ArgType loc op [] = bad $ TypeError loc $ "Empty tuple given to " ++ op
soac2ArgType _ _ [et] = return et
soac2ArgType _ _ ets = return $ Elem $ Tuple ets

soac2ElemType :: SrcLoc -> Type -> TypeM Type
soac2ElemType loc tp@(Array {}) =
    getTupArrElemType loc tp
soac2ElemType loc (Elem (Tuple tps)) = do
    tps' <- mapM (getTupArrElemType loc) tps
    return $ Elem $ Tuple tps'
soac2ElemType loc tp =
    bad $ TypeError loc
                    ("In TypeChecker, soac2ElemType: "
                     ++" input type not a tuple/array: "++ppType tp)

getTupArrElemType :: SrcLoc -> Type -> TypeM Type
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

checkLiteral :: SrcLoc -> Value -> TypeM Value
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

checkIdent :: TypeBox ty => Ident ty -> TypeM (Ident Type)
checkIdent (Ident name t pos) = do
  vt <- lookupVar name pos
  t' <- checkAnnotation pos ("variable " ++ nameToString name) t vt
  return $ Ident name t' pos

checkBinOp :: TypeBox tf => BinOp -> Exp tf -> Exp tf -> tf -> SrcLoc
           -> TypeM (Exp Type)
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

checkRelOp :: TypeBox ty =>
              BinOp -> [ElemType] -> Exp ty -> Exp ty -> ty -> SrcLoc
           -> TypeM (Exp Type)
checkRelOp op tl e1 e2 t pos = do
  e1' <- require (map Elem tl) =<< checkExp e1
  e2' <- require (map Elem tl) =<< checkExp e2
  _ <- unifyExpTypes e1' e2'
  t' <- checkAnnotation pos "result" t $ Elem Bool
  return $ BinOp op e1' e2' t' pos

checkPolyBinOp :: TypeBox ty => BinOp -> [ElemType] -> Exp ty -> Exp ty -> ty -> SrcLoc
               -> TypeM (Exp Type)
checkPolyBinOp op tl e1 e2 t pos = do
  e1' <- require (map Elem tl) =<< checkExp e1
  e2' <- require (map Elem tl) =<< checkExp e2
  t' <- unifyExpTypes e1' e2'
  t'' <- checkAnnotation pos "result" t t'
  return $ BinOp op e1' e2' t'' pos

sequentially :: TypeM a -> (a -> Dataflow -> TypeM b) -> TypeM b
sequentially m1 m2 = do
  (a, m1flow) <- collectDataflow m1
  (b, m2flow) <- collectDataflow $ m2 a m1flow
  tell Dataflow {
           usageAliasing = usageAliasing m2flow
         , usageOccurences = usageOccurences m1flow `seqUsages` usageOccurences m2flow
         }
  return b

checkBinding :: TypeBox tf =>
                TupIdent tf -> Exp Type -> Dataflow
             -> TypeM (TypeM a -> TypeM a, TupIdent Type)
checkBinding pat e dflow = do
  (pat', (scope, _)) <-
    runStateT (checkBinding' pat (typeOf e) (usageAliasing dflow)) (id, [])
  return (\m -> sequentially (tell dflow) (const . const $ scope m), pat')
  where checkBinding' (Id (Ident name namet pos)) t a = do
          t' <- lift $ checkAnnotation (srclocOf pat) (nameToString name) namet t
          add (Ident name t' pos) a
          return $ Id $ Ident name t' pos
        checkBinding' (TupId pats pos) (Elem (Tuple ts)) a
          | length pats == length ts = do
          pats' <- sequence $ zipWith3 checkBinding' pats ts ass
          return $ TupId pats' pos
          where ass = case a of TupleAlias ass' -> ass' ++ repeat a
                                _               -> repeat a
        checkBinding' _ _ _ = lift $ bad $ InvalidPatternError errpat e

        add ident a = do
          bnd <- gets $ find (==ident) . snd
          case bnd of
            Nothing ->
              modify $ \(scope, names) ->
                (binding [ident] . aliasing (identName ident) a . scope,
                 ident : names)
            Just (Ident name _ pos2) ->
              lift $ bad $ DupPatternError name (srclocOf ident) pos2
        -- A pattern with known type box (Maybe) for error messages.
        errpat = rmTypes pat
        rmTypes (Id (Ident name t pos)) = Id $ Ident name (unboxType t) pos
        rmTypes (TupId pats pos) = TupId (map rmTypes pats) pos

validApply :: [Type] -> [Type] -> Bool
validApply expected got =
  length got == length expected && all id (zipWith subtypeOf got expected)

checkApply :: Maybe Name -> [Type] -> [Type] -> SrcLoc -> TypeM ()
checkApply fname expected got loc =
  unless (validApply expected got) $
  bad $ ParameterMismatch fname loc (Right expected) got

checkLambda :: TypeBox ty => Lambda ty -> [Type] -> TypeM (Lambda Type)
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
  checkLambda (AnonymFun [var] (Negate (Var var) arg pos) rettype' pos) [arg]
  where var = Ident (nameFromString "x") arg pos

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
            validApply ets paramtypes ->
              -- Same shimming as in the case for anonymous functions,
              -- although we don't have to worry about name shadowing
              -- here.
              let tupparam = Ident (nameFromString "x") tupt pos
                  tupfun = AnonymFun [tupparam] tuplet rettype' pos
                  params = zipWith mkparam [0..] paramtypes
                    where mkparam :: Int -> Type -> Ident Type
                          mkparam i t = Ident (nameFromString $ "param_" ++ show i) t pos
                  tuplet = LetPat (TupId (map Id params) pos) (Var tupparam) body pos
                  body = Apply fname (map Var params) rettype' pos
              in checkLambda tupfun args
          | otherwise -> do
              case find (unique . snd) $ zip curryargexps paramtypes of
                Just (e, _) -> bad $ CurriedConsumption fname $ srclocOf e
                _           -> return ()
              checkApply Nothing paramtypes (curryargexpts++args) pos
              return $ CurryFun fname curryargexps' rettype' pos

checkPolyLambdaOp :: TypeBox ty => BinOp -> [Exp ty] -> ty -> [Type] -> SrcLoc
                  -> TypeM (Lambda Type)
checkPolyLambdaOp op curryargexps rettype args pos = do
  curryargexpts <- map typeOf <$> mapM checkExp curryargexps
  tp <- case curryargexpts ++ args of
          [t1, t2] | t1 == t2 -> return t1
          [Elem (Tuple [t1,t2])] | t1 == t2 -> return t1 -- For autoshimming.
          l -> bad $ ParameterMismatch (Just fname) pos (Left 2) l
  (x,y,params) <- case curryargexps of
                    [] -> return (Var $ xident (boxType tp),
                                  Var $ yident (boxType tp),
                                  [xident tp, yident tp])
                    [e] -> return (e,
                                   Var $ yident $ boxType tp,
                                   [yident tp])
                    (e1:e2:_) -> return (e1, e2, [])
  body <- binding params $ checkBinOp op x y rettype pos
  checkLambda (AnonymFun params body (typeOf body) pos) args
  where fname = nameFromString $ "op" ++ ppBinOp op
        xident t = Ident (nameFromString "x") t pos
        yident t = Ident (nameFromString "y") t pos
