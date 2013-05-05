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

-- | Information about an error during type checking.  The 'Show'
-- instance for this type produces a human-readable description.
data TypeError = TypeError SrcLoc String
               -- ^ A general error happened at the given position and
               -- for the given reason.
               | UnifyError Type Type
               -- ^ Two types failed to unify.
               | ReturnTypeError SrcLoc String Type Type
               -- ^ The body of a function definition has a different
               -- type than its declaration.
               | DupDefinitionError String SrcLoc SrcLoc
               -- ^ Two functions have been defined with the same name.
               | DupParamError String String SrcLoc
               -- ^ Two function parameters share the same name.
               | DupPatternError String SrcLoc SrcLoc
               -- ^ Two pattern variables share the same name.
               | InvalidPatternError (TupIdent (Maybe Type)) Type
               -- ^ The pattern is not compatible with the type.
               | UnknownVariableError String SrcLoc
               -- ^ Unknown variable of the given name referenced at the given spot.
               | UnknownFunctionError String SrcLoc
               -- ^ Unknown function of the given name called at the given spot.
               | ParameterMismatch (Maybe String) SrcLoc (Either Int [Type]) [Type]
               -- ^ A function (possibly anonymous) was called with
               -- invalid arguments.  The third argument is either the
               -- number of parameters, or the specific types of
               -- parameters accepted (sometimes, only the former can
               -- be determined).
               | UseAfterConsume String SrcLoc SrcLoc
               -- ^ A variable was attempted used after being
               -- consumed.  The last location is the point of
               -- consumption.
               | IndexingError Int Int SrcLoc
               -- ^ Too many indices provided.  The first integer is
               -- the number of dimensions in the array being
               -- indexed.

instance Show TypeError where
  show (TypeError pos msg) =
    "Type error at " ++ locStr pos ++ ":\n" ++ msg
  show (UnifyError t1 t2) =
    "Cannot unify type " ++ ppType t1 ++ " from " ++ locStr (srclocOf t1) ++
    " with type " ++ ppType t2 ++ " from " ++ locStr (srclocOf t2)
  show (ReturnTypeError pos fname rettype bodytype) =
    "Declaration of function " ++ fname ++ " at " ++ locStr pos ++
    " declares return type " ++ ppType rettype ++ ", but body has type " ++
    ppType bodytype
  show (DupDefinitionError name pos1 pos2) =
    "Duplicate definition of function " ++ name ++ ".  Defined at " ++
    locStr pos1 ++ " and " ++ locStr pos2 ++ "."
  show (DupParamError funname paramname pos) =
    "Parameter " ++ paramname ++
    " mentioned multiple times in argument list of function " ++
    funname ++ " at " ++ locStr pos ++ "."
  show (DupPatternError name pos1 pos2) =
    "Variable " ++ name ++ " bound twice in tuple pattern; at " ++
    locStr pos1 ++ " and " ++ locStr pos2 ++ "."
  show (InvalidPatternError pat t) =
    "Pattern " ++ ppTupId pat ++ " at " ++ locStr (srclocOf pat) ++
    " cannot match value of type " ++ ppType t ++ " at " ++ locStr (srclocOf t) ++ "."
  show (UnknownVariableError name pos) =
    "Unknown variable " ++ name ++ " referenced at " ++ locStr pos ++ "."
  show (UnknownFunctionError fname pos) =
    "Unknown function " ++ fname ++ " called at " ++ locStr pos ++ "."
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
          fname' = maybe "anonymous function" ("function "++) fname
  show (UseAfterConsume name rloc wloc) =
    "Varible " ++ name ++ " used at " ++ locStr rloc ++
    ", but it was consumed at " ++ locStr wloc ++ ".  (Possibly through aliasing)"
  show (IndexingError dims got pos) =
    show got ++ " indices given, but type of expression at " ++ locStr pos ++ " has " ++ show dims ++ " dimension(s)."

-- | A tuple of a return type and a list of argument types.
type FunBinding = (Type, [Type])

-- | Information about the aliasing of the value returned by an expression.
data Aliases = VarAlias (S.Set String)
             -- ^ May alias these variables.
             | TupleAlias [Aliases]
             -- ^ Aliasing information for specific components of a
             -- tuple.

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

-- | All the variables represented in this aliasing.
aliased :: Aliases -> [String]
aliased (VarAlias names) = S.toList names
aliased (TupleAlias ass) = concatMap aliased ass

-- | A pair of a variable table and a function table.  Type checking
-- happens with access to this environment.  The function table is
-- only initialised at the very beginning, but the variable table will
-- be extended during type-checking when let-expressions are
-- encountered.
data TypeEnv = TypeEnv { envVtable :: M.Map String Type
                       , envFtable :: M.Map String FunBinding
                       , envAliases :: M.Map String Aliases
                       -- ^ Invariant: every variable must alias
                       -- itself.
                       , envConsumed :: M.Map String SrcLoc
                       , envCheckOccurences :: Bool
                       }

data Usage = Observed SrcLoc
           | Consumed SrcLoc
             deriving (Eq, Ord, Show)

type Usages = M.Map String [Usage]

combineUsages :: String -> Usage -> Usage -> Either TypeError Usage
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
  }

instance Monoid Dataflow where
  mempty = Dataflow mempty mempty
  Dataflow m1 s1 `mappend` Dataflow m2 s2 =
    Dataflow (M.unionWith (++) m1 m2) (s1 `mappend` s2)

-- | The type checker runs in this monad.  Note that it has no mutable
-- state, but merely keeps track of current bindings in a 'TypeEnv'.
-- The 'Either' monad is used for error handling.
newtype TypeM a = TypeM (WriterT Dataflow (ReaderT TypeEnv (Either TypeError)) a)
  deriving (Monad, Functor, MonadReader TypeEnv, MonadWriter Dataflow)

runTypeM :: TypeEnv -> TypeM a -> Either TypeError a
runTypeM env (TypeM m) = fst <$> runReaderT (runWriterT m) env

bad :: TypeError -> TypeM a
bad = TypeM . lift . lift . Left

liftEither :: Either TypeError a -> TypeM a
liftEither = either bad return

-- | Proclaim that we have made read-only use of the given variable!
observe :: Ident Type -> TypeM ()
observe (Ident name _ loc) =
  tell $ mempty { usageOccurences = M.singleton name [Observed loc]
                , usageAliasing = VarAlias (S.singleton name) }

alias :: Aliases -> TypeM ()
alias al = tell $ mempty { usageAliasing = al }

-- | Proclaim that we have written to the given variable.
consume :: SrcLoc -> String -> TypeM ()
consume loc name = do
  als <- aliases name
  tell $ mempty { usageOccurences =
                    M.fromList [ (name', [Consumed loc])
                                 | name' <- aliased als ] }

-- | Proclaim that we have written to the given variable, and mark
-- accesses to it and all of its aliases as invalid inside the given
-- computation.
consuming :: SrcLoc -> String -> TypeM a -> TypeM a
consuming loc name m = do
  consume loc name
  local consume' m
  where consume' env =
          env { envConsumed = M.insert name loc $ envConsumed env }

-- | Retrieve the variables referenced inside a computation.
collectOccurences :: [String] -> TypeM a -> TypeM (a, M.Map String [Usage])
collectOccurences names m = pass $ do
  (x, usage) <- listen m
  let (relevant, rest) = split $ usageOccurences usage
  return ((x, relevant), const $ usage { usageOccurences = rest})
    where split = M.partitionWithKey $ const . (`elem` names)

collectAliases :: TypeM a -> TypeM (a, Aliases)
collectAliases m = pass $ do
  (x, usage) <- listen m
  return ((x, usageAliasing usage),
          const $ usage { usageAliasing = mempty })

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
  let usage = Dataflow (M.unionWith max occurs1' occurs2') (als1 `mappend` als2)
  return ((x, y), const usage)

aliasing :: String -> Aliases -> TypeM a -> TypeM a
aliasing name als tm = do
  als' <- mconcat <$> mapM aliases (aliased als)
  let name' = VarAlias $ S.singleton name
      -- Edges from name to als' and to itself.
      outedges = M.insert name name' . M.insert name als'
      -- Edges from everything in als' to name:
      inedges m =  foldl (\m' v -> M.insert v name' m') m $ aliased als'
  local (\env -> env { envAliases = inedges $ outedges $ envAliases env }) tm

aliases :: String -> TypeM Aliases
aliases name = asks $ fromMaybe mempty . M.lookup name . envAliases

binding :: [Ident Type] -> TypeM a -> TypeM a
binding bnds = check bnds . local (`bindVars` bnds)
  where bindVars = foldl bindVar

        -- Since the type checker does not require unique names,
        -- we need to remove any existing information about
        -- variables of the same name.
        bindVar env (Ident name tp _) =
          env { envVtable = M.insert name tp $ envVtable env
              , envConsumed = M.delete name $ envConsumed env }

        check vars m = do
          (a, usages) <- collectOccurences (map identName vars) m
          void $ checkOccurences usages
          return a

lookupVar :: String -> SrcLoc -> TypeM Type
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
unifyKnownTypes :: Type -> Type -> TypeM Type
unifyKnownTypes (Int pos) (Int _) = return $ Int pos
unifyKnownTypes (Char pos) (Char _) = return $ Char pos
unifyKnownTypes (Bool pos) (Bool _) = return $ Bool pos
unifyKnownTypes (Real pos) (Real _) = return $ Real pos
unifyKnownTypes (Tuple ts1 u1 pos) (Tuple ts2 u2 _)
  | length ts1 == length ts2 = do
  ts <- zipWithM unifyKnownTypes ts1 ts2
  return $ Tuple ts (u1 <> u2) pos
unifyKnownTypes (Array t1 e u1 pos) (Array t2 _ u2 _) = do
  t <- unifyKnownTypes t1 t2
  return $ Array t e (u1 <> u2) pos
unifyKnownTypes t1 t2 = bad $ UnifyError t1 t2

-- | @unifyWithKnown t1 t2@ returns @t2@ if @t1@ contains no type, and
-- otherwise tries to unify them with 'unifyKnownTypes'.
unifyWithKnown :: TypeBox tf => tf -> Type -> TypeM Type
unifyWithKnown t1 t2 = case unboxType t1 of
                         Nothing -> return t2
                         Just t1' -> unifyKnownTypes t2 t1'

-- | @require ts e@ causes a 'TypeError' if @typeOf e@ does not unify
-- with one of the types in @ts@.  Otherwise, simply returns @e@.
-- This function is very useful in 'checkExp'.
require :: [Type] -> Exp Type -> TypeM (Exp Type)
require [] e = bad $ TypeError (srclocOf e) "Expression cannot have any type (probably a bug in the type checker)."
require ts e
  | any (typeOf e `similarTo`) ts = return e
  | otherwise =
    bad $ TypeError (srclocOf e) $ "Expression type must be one of " ++
          intercalate ", " (map ppType ts) ++ ", but is " ++ ppType (typeOf e) ++ "."

elemType :: Type -> TypeM Type
elemType (Array t _ _ _) = return t
elemType t = bad $ TypeError (srclocOf t) $ "Type of expression is not array, but " ++ ppType t ++ "."

propagateUniqueness :: Type -> Type
propagateUniqueness (Array et dim Nonunique loc) =
  Array et dim (uniqueness et) loc
propagateUniqueness (Tuple ts Nonunique loc) =
  Tuple ts (fromMaybe Nonunique $ find (==Unique) $ map uniqueness ts) loc
propagateUniqueness t = t

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
        let argtypes = map (propagateUniqueness . identType) args -- Throw away argument names.
        in Right $ M.insert name (propagateUniqueness ret,argtypes,pos) ftable
    rmLoc (ret,args,_) = (ret,args)
    builtins = M.fromList [("toReal", (Real noLoc, [Int noLoc], noLoc))
                          ,("trunc", (Int noLoc, [Real noLoc], noLoc))
                          ,("sqrt", (Real noLoc, [Real noLoc], noLoc))
                          ,("log", (Real noLoc, [Real noLoc], noLoc))
                          ,("exp", (Real noLoc, [Real noLoc], noLoc))
                          ,("op not", (Bool noLoc, [Bool noLoc], noLoc))]

checkFun :: TypeBox tf => FunDec tf -> TypeM (FunDec Type)
checkFun (fname, rettype, args, body, pos) = do
  args' <- checkArgs
  body' <- binding args' $ checkExp body
  let rettype' = propagateUniqueness rettype
      bodytype = propagateUniqueness $ typeOf body'
  if bodytype `subtypeOf` rettype' then
    return (fname, rettype', args, body', pos)
  else bad $ ReturnTypeError pos fname rettype' $ typeOf body'
  where checkArgs = foldM expand [] args
        expand args' ident@(Ident pname _ _)
          | Just _ <- find ((==identName ident) . identName) args' =
            bad $ DupParamError fname pname pos
          | otherwise =
            return $ ident : args'

checkExp :: TypeBox tf => Exp tf -> TypeM (Exp Type)
checkExp e = do
  (e', als) <- collectAliases $ checkExp' e
  unless (basicType $ typeOf e') $
    tell $ mempty { usageAliasing = als }
  return e'

-- | Never call checkExp' directly!  Call checkExp!
checkExp' :: TypeBox tf => Exp tf -> TypeM (Exp Type)

checkExp' (Literal val) =
  Literal <$> checkLiteral val

checkExp' (TupLit es pos) = do
  (es', als) <- unzip <$> mapM (collectAliases . checkExp) es
  let res = TupLit es' pos
  alias $ foldl extend (TupleAlias []) als
  return $ fromMaybe res (Literal <$> expToValue res)
    where extend (TupleAlias alss) als = TupleAlias $ alss ++ [als]
          extend als1 als2             = TupleAlias [als1, als2]

checkExp' (ArrayLit es t pos) = do
  es' <- mapM checkExp es
  -- Find the unified type of all subexpression types.
  et <- case map typeOf es' of
          [] -> bad $ TypeError pos "Empty array literal"
          e:ets' -> foldM unifyKnownTypes e ets'
  -- Unify that type with the one given for the array literal.
  t' <- t `unifyWithKnown` et
  let res = ArrayLit es' t' pos
  return $ fromMaybe res (Literal <$> expToValue res)

checkExp' (BinOp op e1 e2 t pos) = checkBinOp op e1 e2 t pos

checkExp' (And e1 e2 pos) = do
  e1' <- require [Bool pos] =<< checkExp e1
  e2' <- require [Bool pos] =<< checkExp e2
  return $ And e1' e2' pos

checkExp' (Or e1 e2 pos) = do
  e1' <- require [Bool pos] =<< checkExp e1
  e2' <- require [Bool pos] =<< checkExp e2
  return $ Or e1' e2' pos

checkExp' (Not e pos) = do
  e' <- require [Bool pos] =<< checkExp e
  return $ Not e' pos

checkExp' (Negate e t pos) = do
  e' <- require [Int pos, Real pos] =<< checkExp e
  t' <- t `unifyWithKnown` typeOf e'
  return $ Negate e' t' pos

checkExp' (If e1 e2 e3 t pos) = do
  e1' <- require [Bool pos] =<< checkExp e1
  (e2', e3') <- checkExp e2 `alternative` checkExp e3
  bt <- unifyWithKnown t =<< unifyKnownTypes (typeOf e2') (typeOf e3')
  return $ If e1' e2' e3' bt pos

checkExp' (Var ident) = do
  ident' <- checkIdent ident
  observe ident'
  return $ Var ident'

checkExp' (Apply "trace" args t pos) =
  case args of
    [e] -> do
      e' <- checkExp e
      t' <- t `unifyWithKnown` typeOf e'
      return $ Apply "trace" [e'] t' pos
    _ -> bad $ TypeError pos "Trace function takes a single parameter"

checkExp' (Apply "assertZip" args t pos) = do
  args' <- mapM checkExp args
  let argtps = map typeOf args'
  _ <- mapM soac2ElemType argtps
  t' <- t `unifyWithKnown` Bool pos -- (Tuple argtps Unique pos)
  return $ Apply "assertZip" args' t' pos

checkExp' (Apply fname args t pos) = do
  bnd <- asks $ M.lookup fname . envFtable
  case bnd of
    Nothing -> bad $ UnknownFunctionError fname pos
    Just (rettype, paramtypes) -> do
      rettype' <- t `unifyWithKnown` rettype
      args' <- forM (zip args $ cycle paramtypes) $ \(arg, paramt) ->
                 if unique paramt then do
                   (arg', dflow) <- collectDataflow $ checkExp arg
                   mapM_ (consume pos) $ aliased $ usageAliasing dflow
                   return arg'
                 else checkExp arg
      checkApply (Just fname) paramtypes (map typeOf args') pos
      return $ Apply fname args' rettype' pos

checkExp' (LetPat pat e body pos) = do
  (e', dataflow) <- collectDataflow $ checkExp e
  (scope, pat') <- checkBinding pat (typeOf e') dataflow
  scope $ do
    body' <- checkExp body
    return $ LetPat pat' e' body' pos

checkExp' (LetWith (Ident dest destt destpos) src idxes ve body pos) = do
  src' <- checkIdent src
  idxes' <- mapM (require [Int pos] <=< checkExp) idxes
  destt' <- destt `unifyWithKnown` identType src'
  let dest' = Ident dest destt' destpos

  unless (unique src' || basicType (typeOf src')) $
    bad $ TypeError pos "Source is not unique"

  case peelArray (length idxes) (identType src') of
    Nothing -> bad $ IndexingError (arrayDims $ identType src') (length idxes) (srclocOf src)
    Just elemt ->
      sequentially (require [elemt] =<< checkExp ve) $ \ve' -> do
        (scope, _) <- checkBinding (Id dest') destt' mempty
        body' <- consuming pos (identName src') $ scope $ checkExp body
        return $ LetWith dest' src' idxes' ve' body' pos

checkExp' (Index ident idxes intype restype pos) = do
  ident' <- checkIdent ident
  observe ident'
  vt <- lookupVar (identName ident') pos
  when (arrayDims vt < length idxes) $
    bad $ IndexingError (arrayDims vt) (length idxes) pos
  vet <- elemType vt
  intype' <- intype `unifyWithKnown` vet
  restype' <- restype `unifyWithKnown` stripArray (length idxes) vt
  idxes' <- mapM (require [Int pos] <=< checkExp) idxes
  return $ Index ident' idxes' intype' restype' pos

checkExp' (Iota e pos) = do
  e' <- require [Int pos] =<< checkExp e
  return $ Iota e' pos

checkExp' (Size e pos) = do
  e' <- checkExp e
  case typeOf e' of
    Array {} -> return $ Size e' pos
    _        -> bad $ TypeError pos "Argument to size must be array."

checkExp' (Replicate countexp valexp pos) = do
  countexp' <- require [Int pos] =<< checkExp countexp
  valexp' <- checkExp valexp
  return $ Replicate countexp' valexp' pos

checkExp' (Reshape shapeexps arrexp pos) = do
  shapeexps' <- mapM (require [Int pos] <=< checkExp) shapeexps
  arrexp' <- checkExp arrexp
  return (Reshape shapeexps' arrexp' pos)

checkExp' (Transpose arrexp pos) = do
  arrexp' <- checkExp arrexp
  when (arrayDims (typeOf arrexp') < 2) $
    bad $ TypeError pos "Argument to transpose does not have two dimensions."
  return $ Transpose arrexp' pos

checkExp' (Map fun arrexp intype outtype pos) = do
  arrexp' <- checkExp arrexp
  case typeOf arrexp' of
    Array et _ _ _ -> do
      fun' <- checkLambda fun [et]
      intype' <- intype `unifyWithKnown` et
      outtype' <- outtype `unifyWithKnown` typeOf fun'
      return (Map fun' arrexp' intype' outtype' pos)
    _       -> bad $ TypeError (srclocOf arrexp) "Mapee expression does not return an array."

checkExp' (Reduce fun startexp arrexp intype pos) = do
  startexp' <- checkExp startexp
  arrexp' <- checkExp arrexp
  case typeOf arrexp' of
    Array inelemt _ _ _ -> do
      inelemt' <- intype `unifyWithKnown` inelemt
      fun' <- checkLambda fun [typeOf startexp', inelemt']
      when (typeOf startexp' /= typeOf fun') $
        bad $ TypeError pos $ "Accumulator is of type " ++ ppType (typeOf startexp') ++ ", but reduce function returns type " ++ ppType (typeOf fun') ++ "."
      return $ Reduce fun' startexp' arrexp' inelemt' pos
    _ -> bad $ TypeError (srclocOf arrexp) "Type of expression is not an array"

checkExp' (Zip arrexps pos) = do
  arrexps' <- mapM (checkExp . fst) arrexps
  inelemts <- mapM (elemType . typeOf) arrexps'
  inelemts' <- zipWithM unifyWithKnown (map snd arrexps) inelemts
  return $ Zip (zip arrexps' inelemts') pos

checkExp' (Unzip e _ pos) = do
  e' <- checkExp e
  case typeOf e' of
    Array (Tuple ts _ _) _ _ _ -> return $ Unzip e' ts pos
    et -> bad $ TypeError pos $ "Argument to unzip is not an array of tuples, but " ++ ppType et ++ "."

checkExp' (Scan fun startexp arrexp intype pos) = do
  startexp' <- checkExp startexp
  arrexp' <- checkExp arrexp
  case typeOf arrexp' of
    Array inelemt _ _ _ -> do
      intype' <- intype `unifyWithKnown` inelemt
      fun' <- checkLambda fun [intype', intype']
      when (typeOf startexp' /= typeOf fun') $
        bad $ TypeError pos $ "Initial value is of type " ++ ppType (typeOf startexp') ++ ", but scan function returns type " ++ ppType (typeOf fun') ++ "."
      when (intype' /= typeOf fun') $
        bad $ TypeError pos $ "Array element value is of type " ++ ppType intype' ++ ", but scan function returns type " ++ ppType (typeOf fun') ++ "."
      return $ Scan fun' startexp' arrexp' intype' pos
    _ -> bad $ TypeError (srclocOf arrexp) "Type of expression is not an array."

checkExp' (Filter fun arrexp eltype pos) = do
  arrexp' <- checkExp arrexp
  inelemt <- elemType $ typeOf arrexp'
  eltype' <- eltype `unifyWithKnown` inelemt
  fun' <- checkLambda fun [inelemt]
  when (typeOf fun' /= Bool pos) $
    bad $ TypeError pos "Filter function does not return bool."
  return $ Filter fun' arrexp' eltype' pos

checkExp' (Mapall fun arrexp intype outtype pos) = do
  arrexp' <- checkExp arrexp
  intype' <- intype `unifyWithKnown` baseType (typeOf arrexp')
  fun' <- checkLambda fun [baseType intype']
  outtype' <- outtype `unifyWithKnown` typeOf fun'
  return $ Mapall fun' arrexp' intype' outtype' pos

checkExp' (Redomap redfun mapfun accexp arrexp intype outtype pos) = do
  accexp' <- checkExp accexp
  arrexp' <- checkExp arrexp
  et <- elemType $ typeOf arrexp'
  mapfun' <- checkLambda mapfun [et]
  redfun' <- checkLambda redfun [typeOf accexp', typeOf mapfun']
  _ <- unifyKnownTypes (typeOf redfun') (typeOf accexp')
  intype' <- intype `unifyWithKnown` et
  outtype' <- outtype `unifyWithKnown` typeOf redfun'
  return $ Redomap redfun' mapfun' accexp' arrexp' intype' outtype' pos

checkExp' (Split splitexp arrexp intype pos) = do
  splitexp' <- require [Int pos] =<< checkExp splitexp
  arrexp' <- checkExp arrexp
  et <- elemType $ typeOf arrexp'
  intype' <- intype `unifyWithKnown` et
  return $ Split splitexp' arrexp' intype' pos

checkExp' (Concat arr1exp arr2exp intype pos) = do
  arr1exp' <- checkExp arr1exp
  arr2exp' <- require [typeOf arr1exp'] =<< checkExp arr2exp
  et <- elemType $ typeOf arr2exp'
  intype' <- intype `unifyWithKnown` et
  return $ Concat arr1exp' arr2exp' intype' pos

checkExp' (Copy e pos) = do
  (e', _) <- collectAliases $ checkExp e
  return $ Copy e' pos

-- The loop body is checked twice to make sure any aliasing it
-- introduces is also checked.
checkExp' (DoLoop mergepat mergeexp (Ident loopvar _ _)
          boundexp loopbody letbody pos) = do
  (mergeexp', mergeflow) <- collectDataflow $ checkExp mergeexp
  let mergetype = typeOf mergeexp'
      checkloop scope = collectDataflow $
                        scope $ binding [Ident loopvar (Int pos) pos] $
                        require [mergetype] =<< checkExp loopbody
  (firstscope, mergepat') <- checkBinding mergepat mergetype mergeflow
  boundexp' <- require [Int pos] =<< checkExp boundexp
  (_, dataflow) <- checkloop firstscope
  (secondscope, _) <- checkBinding mergepat mergetype dataflow
  (loopbody', _) <- checkloop secondscope
  secondscope $ do
    letbody' <- checkExp letbody
    return $ DoLoop mergepat' mergeexp' (Ident loopvar (Int pos) pos) boundexp' loopbody' letbody' pos

----------------------------------------------
---- BEGIN Cosmin added SOAC2 combinators ----
----------------------------------------------
checkExp' (Map2 fun arrexp intype outtype pos) = do
  arrexp' <- mapM checkExp arrexp
  ineltps <- mapM (soac2ElemType . typeOf) arrexp'
  let ineltp = if length ineltps == 1 then head ineltps
               else Tuple ineltps Unique pos
  fun'    <- checkLambda fun [ineltp]
  intype' <- intype `unifyWithKnown` ineltp
  outtype'<- outtype `unifyWithKnown` typeOf fun'
  return $ Map2 fun' arrexp' intype' outtype' pos

checkExp' (Reduce2 fun startexp arrexp intype pos) = do
  startexp' <- checkExp startexp
  arrexp'   <- mapM checkExp arrexp
  ineltps   <- mapM (soac2ElemType . typeOf) arrexp'
  let ineltp = if length ineltps == 1 then head ineltps
               else Tuple ineltps Unique pos
  intype' <- intype `unifyWithKnown` ineltp
  fun'    <- checkLambda fun [typeOf startexp', intype']
  when (typeOf startexp' /= typeOf fun') $
        bad $ TypeError pos $ "Accumulator is of type " ++ ppType (typeOf startexp') ++
                              ", but reduce function returns type " ++ ppType (typeOf fun') ++ "."
  return $ Reduce2 fun' startexp' arrexp' intype' pos

checkExp' (Scan2 fun startexp arrexp intype pos) = do
  startexp' <- checkExp startexp
  arrexp'   <- mapM checkExp arrexp

  --inelemt   <- soac2ElemType $ typeOf arrexp'
  ineltps   <- mapM (soac2ElemType . typeOf) arrexp'
  let inelemt = if length ineltps == 1 then head ineltps
                else Tuple ineltps Unique pos
  intype'   <- intype `unifyWithKnown` inelemt
  fun'      <- checkLambda fun [intype', intype']
  when (typeOf startexp' /= typeOf fun') $
    bad $ TypeError pos $ "Initial value is of type " ++ ppType (typeOf startexp') ++
                          ", but scan function returns type " ++ ppType (typeOf fun') ++ "."
  when (intype' /= typeOf fun') $
    bad $ TypeError pos $ "Array element value is of type " ++ ppType intype' ++
                          ", but scan function returns type " ++ ppType (typeOf fun') ++ "."
  return $ Scan2 fun' startexp' arrexp' intype' pos

checkExp' (Filter2 fun arrexp eltype pos) = do
  arrexp' <- mapM checkExp arrexp
  --inelemt <- soac2ElemType $ typeOf arrexp'
  ineltps   <- mapM (soac2ElemType . typeOf) arrexp'
  let inelemt = if length ineltps == 1 then head ineltps
                else Tuple ineltps Unique pos
  eltype' <- eltype `unifyWithKnown` inelemt
  fun' <- checkLambda fun [inelemt]
  when (typeOf fun' /= Bool pos) $
    bad $ TypeError pos "Filter function does not return bool."
  return $ Filter2 fun' arrexp' eltype' pos

checkExp' (Mapall2 fun arrexp intype outtype pos) = do
  arrexp' <- mapM checkExp arrexp
  let arrtps = map typeOf arrexp'

  _ <- mapM soac2ElemType arrtps
  let mindim = foldl min
                     (arrayDims (head arrtps))
                     (map arrayDims (tail arrtps))
  let ineltps= map (stripArray mindim) arrtps
  let ineltp = if length ineltps == 1 then head ineltps
               else Tuple ineltps Unique pos

  intype' <- intype `unifyWithKnown` ineltp
  fun' <- checkLambda fun [intype']
  outtype' <- outtype `unifyWithKnown` typeOf fun'
  return $ Mapall2 fun' arrexp' intype' outtype' pos

checkExp' (Redomap2 redfun mapfun accexp arrexp intype outtype pos) = do
  accexp' <- checkExp accexp
  arrexp' <- mapM checkExp arrexp
  ets <- mapM (soac2ElemType . typeOf) arrexp'
  let et = if length ets == 1 then head ets
           else Tuple ets Unique pos
  mapfun' <- checkLambda mapfun [et]
  redfun' <- checkLambda redfun [typeOf accexp', typeOf mapfun']
  _ <- unifyKnownTypes (typeOf redfun') (typeOf accexp')
  intype' <- intype `unifyWithKnown` et
  outtype' <- outtype `unifyWithKnown` typeOf redfun'
  return $ Redomap2 redfun' mapfun' accexp' arrexp' intype' outtype' pos

---------------------
--- SOAC2 HELPERS ---
---------------------
soac2ElemType :: Type -> TypeM Type
soac2ElemType tp@(Array {}) =
    getTupArrElemType tp
soac2ElemType (Tuple tps u pos ) = do
    tps' <- mapM getTupArrElemType tps
    return $ Tuple tps' u pos
soac2ElemType tp =
    bad $ TypeError (SrcLoc (locOf tp))
                    ("In TypeChecker, soac2ElemType: "
                     ++" input type not a tuple/array: "++ppType tp)

getTupArrElemType :: Type -> TypeM Type
getTupArrElemType tp =
    case tp of
        Array eltp _ _ pos ->
            if hasInnerTuple eltp
            then bad $ TypeError pos ("In TypeChecker, soac2, getTupArrElemType: "
                                      ++"array elem type has an inner tuple: "++ppType eltp)
            else return eltp
        _ -> bad $ TypeError (SrcLoc (locOf tp))
                             ("In TypeChecker, soac2, getTupArrElemType: "
                              ++" input type not an array: "++ppType tp)
    where
        hasInnerTuple :: Type -> Bool
        hasInnerTuple (Tuple {}       ) = True
        hasInnerTuple (Array etp _ _ _) = hasInnerTuple etp
        hasInnerTuple _                 = False

--------------------------------------
---- END Cosmin SOAC2 combinators ----
--------------------------------------

checkLiteral :: Value -> TypeM Value
checkLiteral (IntVal k pos) = return $ IntVal k pos
checkLiteral (RealVal x pos) = return $ RealVal x pos
checkLiteral (LogVal b pos) = return $ LogVal b pos
checkLiteral (CharVal c pos) = return $ CharVal c pos
checkLiteral (TupVal vals pos) = do
  vals' <- mapM checkLiteral vals
  return $ TupVal vals' pos
checkLiteral (ArrayVal arr t pos) = do
  vals' <- mapM checkLiteral (elems arr)
  -- Find the unified type of all subexpression types.
  vt <- case map typeOf vals' of
          [] -> return t -- Permit empty array values, as they always
                         -- have a type.
          v:vts' -> foldM unifyKnownTypes v vts'
  -- Unify that type with the one given for the array literal.
  t' <- t `unifyKnownTypes` vt
  return $ ArrayVal (listArray (bounds arr) vals') t' pos

checkIdent :: TypeBox ty => Ident ty -> TypeM (Ident Type)
checkIdent (Ident name t pos) = do
  vt <- lookupVar name pos
  t' <- t `unifyWithKnown` vt
  return $ Ident name t' pos

checkBinOp :: TypeBox tf => BinOp -> Exp tf -> Exp tf -> tf -> SrcLoc
           -> TypeM (Exp Type)
checkBinOp Plus e1 e2 t pos = checkPolyBinOp Plus [Real pos, Int pos] e1 e2 t pos
checkBinOp Minus e1 e2 t pos = checkPolyBinOp Minus [Real pos, Int pos] e1 e2 t pos
checkBinOp Pow e1 e2 t pos = checkPolyBinOp Pow [Real pos, Int pos] e1 e2 t pos
checkBinOp Times e1 e2 t pos = checkPolyBinOp Times [Real pos, Int pos] e1 e2 t pos
checkBinOp Divide e1 e2 t pos = checkPolyBinOp Divide [Real pos, Int pos] e1 e2 t pos
checkBinOp Mod e1 e2 t pos = checkPolyBinOp Mod [Int pos] e1 e2 t pos
checkBinOp ShiftR e1 e2 t pos = checkPolyBinOp ShiftR [Int pos] e1 e2 t pos
checkBinOp ShiftL e1 e2 t pos = checkPolyBinOp ShiftL [Int pos] e1 e2 t pos
checkBinOp Band e1 e2 t pos = checkPolyBinOp Band [Int pos] e1 e2 t pos
checkBinOp Xor e1 e2 t pos = checkPolyBinOp Xor [Int pos] e1 e2 t pos
checkBinOp Bor e1 e2 t pos = checkPolyBinOp Bor [Int pos] e1 e2 t pos
checkBinOp LogAnd e1 e2 t pos = checkPolyBinOp LogAnd [Bool pos] e1 e2 t pos
checkBinOp LogOr e1 e2 t pos = checkPolyBinOp LogOr [Bool pos] e1 e2 t pos
checkBinOp Equal e1 e2 t pos = checkRelOp Equal [Int pos, Real pos] e1 e2 t pos
checkBinOp Less e1 e2 t pos = checkRelOp Less [Int pos, Real pos] e1 e2 t pos
checkBinOp Leq e1 e2 t pos = checkRelOp Leq [Int pos, Real pos] e1 e2 t pos

checkRelOp :: TypeBox ty =>
              BinOp -> [Type] -> Exp ty -> Exp ty -> ty -> SrcLoc
           -> TypeM (Exp Type)
checkRelOp op tl e1 e2 t pos = do
  e1' <- require tl =<< checkExp e1
  e2' <- require tl =<< checkExp e2
  _ <- unifyKnownTypes (typeOf e1') (typeOf e2')
  t' <- t `unifyWithKnown` Bool pos
  return $ BinOp op e1' e2' t' pos

checkPolyBinOp :: TypeBox ty => BinOp -> [Type] -> Exp ty -> Exp ty -> ty -> SrcLoc
               -> TypeM (Exp Type)
checkPolyBinOp op tl e1 e2 t pos = do
  e1' <- require tl =<< checkExp e1
  e2' <- require tl =<< checkExp e2
  t' <- unifyKnownTypes (typeOf e1') (typeOf e2')
  t'' <- t `unifyWithKnown` t'
  return $ BinOp op e1' e2' t'' pos

sequentially :: TypeM a -> (a -> TypeM b) -> TypeM b
sequentially m1 m2 = do
  (a, m1flow) <- collectDataflow m1
  (b, m2flow) <- collectDataflow $ m2 a
  tell Dataflow {
           usageAliasing = usageAliasing m1flow <> usageAliasing m2flow
         , usageOccurences = usageOccurences m1flow `seqUsages` usageOccurences m2flow
         }
  return b

checkBinding :: TypeBox tf =>
                TupIdent tf -> Type -> Dataflow
             -> TypeM (TypeM a -> TypeM a, TupIdent Type)
checkBinding pat vt dflow = do
  (pat', (scope, _)) <- runStateT (checkBinding' pat vt (usageAliasing dflow)) (id, [])
  return (\m -> sequentially (tell dflow) (const $ scope m), pat')
  where checkBinding' (Id (Ident name namet pos)) t a = do
          t' <- lift $ namet `unifyWithKnown` t
          add (Ident name t' pos) a
          return $ Id $ Ident name t' pos
        checkBinding' (TupId pats pos) (Tuple ts _ _) a
          | length pats == length ts = do
          pats' <- sequence $ zipWith3 checkBinding' pats ts ass
          return $ TupId pats' pos
          where ass = case a of TupleAlias ass' -> ass' ++ repeat a
                                _               -> repeat a
        checkBinding' _ _ _ = lift $ bad $ InvalidPatternError errpat vt

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

checkApply :: Maybe String -> [Type] -> [Type] -> SrcLoc -> TypeM ()
checkApply fname expected got loc =
  unless (validApply expected got) $
  bad $ ParameterMismatch fname loc (Right expected) got

checkLambda :: TypeBox ty => Lambda ty -> [Type] -> TypeM (Lambda Type)
checkLambda (AnonymFun params body ret pos) args = do
  (_, ret', params', body', _) <-
    checkFun ("<anonymous>", ret, params, body, pos)
  case () of
    _ | length params' == length args -> do
          checkApply Nothing (map identType params') args pos
          return $ AnonymFun params body' ret' pos
      | [t@(Tuple args' _ _)] <- args,
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

checkLambda (CurryFun "op ~" [] rettype pos) [arg] = do
  rettype' <- rettype `unifyWithKnown` arg
  checkLambda (AnonymFun [var] (Negate (Var var) arg pos) rettype' pos) [arg]
  where var = Ident "x" arg pos

checkLambda (CurryFun opfun curryargexps rettype pos) args
  | Just op <- lookup opfun ops =
  checkPolyLambdaOp op curryargexps rettype args pos
  where ops = map (\op -> ("op " ++ opStr op, op)) [minBound..maxBound]

checkLambda (CurryFun fname curryargexps rettype pos) args = do
  curryargexps' <- mapM checkExp curryargexps
  let curryargexpts = map typeOf curryargexps'
  bnd <- asks $ M.lookup fname . envFtable
  case bnd of
    Nothing -> bad $ UnknownFunctionError fname pos
    Just (rt, paramtypes) -> do
      rettype' <- rettype `unifyWithKnown` rt
      case () of
        _ | [tupt@(Tuple ets _ _)] <- args,
            validApply ets paramtypes ->
              -- Same shimming as in the case for anonymous functions,
              -- although we don't have to worry about name shadowing
              -- here.
              let tupparam = (Ident "x" tupt pos)
                  tupfun = AnonymFun [tupparam] tuplet rettype' pos
                  params = zipWith mkparam [0..] paramtypes
                    where mkparam :: Int -> Type -> Ident Type
                          mkparam i t = Ident ("param_" ++ show i) t pos
                  tuplet = LetPat (TupId (map Id params) pos) (Var tupparam) body pos
                  body = Apply fname (map Var params) rettype' pos
              in checkLambda tupfun args
          | otherwise -> do
              checkApply Nothing paramtypes (curryargexpts++args) pos
              return $ CurryFun fname curryargexps' rettype' pos

checkPolyLambdaOp :: TypeBox ty => BinOp -> [Exp ty] -> ty -> [Type] -> SrcLoc
                  -> TypeM (Lambda Type)
checkPolyLambdaOp op curryargexps rettype args pos = do
  curryargexpts <- map typeOf <$> mapM checkExp curryargexps
  tp <- case curryargexpts ++ args of
          [t1, t2] | t1 == t2 -> return t1
          [Tuple [t1,t2] _ _] | t1 == t2 -> return t1 -- For autoshimming.
          l -> bad $ ParameterMismatch (Just fname) pos (Left 2) l
  (x,y,params) <- case curryargexps of
                    [] -> return (Var (Ident "x" (boxType tp) pos),
                                  Var (Ident "y" (boxType tp) pos),
                                  [Ident "x" tp pos, Ident "y" tp pos])
                    [e] -> return (e,
                                   Var (Ident "y" (boxType tp) pos),
                                   [Ident "y" tp pos])
                    (e1:e2:_) -> return (e1, e2, [])
  body <- binding params $ checkBinOp op x y rettype pos
  checkLambda (AnonymFun params body (typeOf body) pos) args
  where fname = "op" ++ ppBinOp op
