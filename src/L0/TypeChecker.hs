{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module L0.TypeChecker ( checkProg
                      , TypeError(..))
  where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.List

import qualified Data.Map as M
import qualified Data.Set as S

import L0.AbSyn

-- | Special property related to a variable binding.
data BindingProp = MergeVar -- ^ A merge variable.
                 | NormalVar -- ^ A normal variable.
                   deriving (Eq)

data VarBinding = VarBinding { bndType :: Type
                             -- ^ The type of the variable.
                             , bndProp :: BindingProp
                             -- ^ If true, the binding is a merge variable.
                             }

-- | A tuple of a return type and a list of argument types.
type FunBinding = (Type, [Type])

-- | A pair of a variable table and a function table.  Type checking
-- happens with access to this environment.  The function table is
-- only initialised at the very beginning, but the variable table will
-- be extended during type-checking when let-expressions are
-- encountered.
data TypeEnv = TypeEnv { envVtable :: M.Map String VarBinding
                       , envFtable :: M.Map String FunBinding }

-- | Accumulated information generated during type checking.
data TypeAcc = TypeAcc { accSrcMergeVars :: S.Set String
                       -- ^ The set of merge variables read from.
                       , accDestMergeVars :: S.Set String
                       -- ^ The set of merge variables written to.
                       }

instance Monoid TypeAcc where
  (TypeAcc src1 dest1) `mappend` (TypeAcc src2 dest2) =
    TypeAcc (src1 <> src2) (dest1 <> dest2)
  mempty = TypeAcc mempty mempty

-- | Information about an error during type checking.  The 'Show'
-- instance for this type produces a human-readable description.
data TypeError = TypeError Pos String
               -- ^ A general error happened at the given position and
               -- for the given reason.
               | UnifyError Type Type
               -- ^ Two types failed to unify.
               | ReturnTypeError Pos String Type Type
               -- ^ The body of a function definition has a different
               -- type than its declaration.
               | DupDefinitionError String Pos Pos
               -- ^ Two functions have been defined with the same name.
               | DupParamError String String Pos
               -- ^ Two function parameters share the same name.
               | DupPatternError String Pos Pos
               -- ^ Two pattern variables share the same name.
               | InvalidPatternError TupIdent Type
               -- ^ The pattern is not compatible with the type.
               | UnknownVariableError String Pos
               -- ^ Unknown variable of the given name referenced at the given spot.
               | UnknownFunctionError String Pos
               -- ^ Unknown function of the given name called at the given spot.
               | ParameterMismatch String Pos (Either Int [Type]) [Type]
               -- ^ A known function was called with invalid
               -- arguments.  The third argument is either the number
               -- of parameters, or the specific types of parameters
               -- accepted (sometimes, only the former can be
               -- determined).
               | MergeVarNonBasicIndexing String Pos

instance Show TypeError where
  show (TypeError pos msg) =
    "Type error at " ++ posStr pos ++ ":\n" ++ msg
  show (UnifyError t1 t2) =
    "Cannot unify type " ++ ppType t1 ++ " from " ++ posStr (typePos t1) ++
    " with type " ++ ppType t2 ++ " from " ++ posStr (typePos t2)
  show (ReturnTypeError pos fname rettype bodytype) =
    "Declaration of function " ++ fname ++ " at " ++ posStr pos ++
    " declares return type " ++ ppType rettype ++ ", but body has type " ++
    ppType bodytype
  show (DupDefinitionError name pos1 pos2) =
    "Duplicate definition of function " ++ name ++ ".  Defined at " ++
    posStr pos1 ++ " and " ++ posStr pos2 ++ "."
  show (DupParamError funname paramname pos) =
    "Parameter " ++ paramname ++
    " mentioned multiple times in argument list of function " ++
    funname ++ " at " ++ posStr pos ++ "."
  show (DupPatternError name pos1 pos2) =
    "Variable " ++ name ++ " bound twice in tuple pattern; at " ++
    posStr pos1 ++ " and " ++ posStr pos2 ++ "."
  show (InvalidPatternError pat t) =
    "Pattern " ++ ppTupId pat ++ " at " ++ posStr (patPos pat) ++
    " cannot match value of type " ++ ppType t ++ " at " ++ posStr (typePos t) ++ "."
  show (UnknownVariableError name pos) =
    "Unknown variable " ++ name ++ " referenced at " ++ posStr pos ++ "."
  show (UnknownFunctionError fname pos) =
    "Unknown function " ++ fname ++ " called at " ++ posStr pos ++ "."
  show (ParameterMismatch fname pos expected got) =
    "In call of Function " ++ fname ++ " at position " ++ posStr pos ++
    ": expecting " ++ show nexpected ++ " argument(s) of type(s) " ++
     expected' ++ ", but got " ++ show ngot ++
    " arguments of types " ++ intercalate ", " (map ppType got) ++ "."
    where (nexpected, expected') =
            case expected of
              Left i -> (i, "(polymorphic)")
              Right ts -> (length ts, intercalate ", " $ map ppType ts)
          ngot = length got
  show (MergeVarNonBasicIndexing name pos) =
    "Merge variable " ++ name ++ " indexed at " ++ posStr pos ++
    " to non-base type, but also modified in body of let."

-- | The type checker runs in this monad.  Note that it has no mutable
-- state, but merely keeps track of current bindings in a 'TypeEnv'.
-- The 'Either' monad is used for error handling.
newtype TypeM a = TypeM (WriterT TypeAcc (ReaderT TypeEnv (Either TypeError)) a)
  deriving (Monad, Functor, MonadReader TypeEnv, MonadWriter TypeAcc)

runTypeM :: TypeEnv -> TypeM a -> Either TypeError a
runTypeM env (TypeM m) = runReaderT (fst <$> runWriterT m) env

bad :: TypeError -> TypeM a
bad = TypeM . lift . lift . Left

-- | Bind a name as a common (non-merge) variable.
bindVar :: TypeEnv -> Binding -> TypeEnv
bindVar env (name,tp) =
  env { envVtable = M.insert name (VarBinding tp NormalVar) $ envVtable env }

bindVars :: TypeEnv -> [Binding] -> TypeEnv
bindVars = foldl bindVar

binding :: [Binding] -> TypeM a -> TypeM a
binding bnds = local (`bindVars` bnds)

-- | 'unbinding names m' evaluates 'm' with the names in 'names'
-- unbound.
unbinding :: [String] -> TypeM a -> TypeM a
unbinding bnds = local (`unbindVars` bnds)
  where unbindVars = foldl unbindVar
        unbindVar env name = env { envVtable = M.delete name $ envVtable env }

-- | Rebind variables as merge variables while evaluating a 'TypeM'
-- action.
merging :: [String] -> Pos -> TypeM a -> TypeM a
merging [] _ m = m
merging (k:ks) pos m = do
  bnd <- lookupVar k pos
  let mkmerge = M.insert k bnd { bndProp = MergeVar }
  local (\env -> env { envVtable = mkmerge $ envVtable env }) $
        merging ks pos m

unmerging :: TypeM a -> TypeM a
unmerging = local unmerging'
  where unmerging' tenv = tenv { envVtable = M.map unmerge $ envVtable tenv }
        unmerge (VarBinding t _) = VarBinding t NormalVar

-- | The list of merge variables currently in scope.
mergeVars :: TypeM [String]
mergeVars = asks $ map fst . filter ((==MergeVar) . bndProp . snd) . M.toList . envVtable

lookupVar :: String -> Pos -> TypeM VarBinding
lookupVar name pos = do
  bnd <- asks $ M.lookup name . envVtable
  case bnd of Nothing   -> bad $ UnknownVariableError name pos
              Just bnd' -> return bnd'

lookupVarType :: String -> Pos -> TypeM Type
lookupVarType name pos = bndType <$> lookupVar name pos

collectSrcMergeVars :: TypeM a -> TypeM (a, S.Set String)
collectSrcMergeVars m = pass collect
  where collect = do (x,acc) <- listen m
                     return ((x, accSrcMergeVars acc),
                             const $ acc { accSrcMergeVars = S.empty})

collectDestMergeVars :: TypeM a -> TypeM (a, S.Set String)
collectDestMergeVars m = pass collect
  where collect = do (x,acc) <- listen m
                     return ((x, accDestMergeVars acc),
                             const $ acc { accDestMergeVars = S.empty})

-- | Determine if two types are identical.  Causes a 'TypeError' if
-- they fail to match, and otherwise returns one of them.
unifyKnownTypes :: Type -> Type -> TypeM Type
unifyKnownTypes (Int pos) (Int _) = return $ Int pos
unifyKnownTypes (Char pos) (Char _) = return $ Char pos
unifyKnownTypes (Bool pos) (Bool _) = return $ Bool pos
unifyKnownTypes (Real pos) (Real _) = return $ Real pos
unifyKnownTypes (Tuple ts1 pos) (Tuple ts2 _)
  | length ts1 == length ts2 = do
  ts <- zipWithM unifyKnownTypes ts1 ts2
  return $ Tuple ts pos
unifyKnownTypes (Array t1 e pos) (Array t2 _ _) = do
  t <- unifyKnownTypes t1 t2
  return $ Array t e pos
unifyKnownTypes t1 t2 = bad $ UnifyError t1 t2

-- | @unifyWithKnown t1 t2@ returns @t2@ if @t1@ contains no type, and
-- otherwise tries to unify them with 'unifyKnownTypes'.
unifyWithKnown :: TypeBox tf => tf Type -> Type -> TypeM Type
unifyWithKnown t1 t2 = case unboxType t1 of
                         Nothing -> return t2
                         Just t1' -> unifyKnownTypes t2 t1'

-- | @require ts (t, e)@ causes a 'TypeError' if @t@ does not unify
-- with one of the types in @ts@.  Otherwise, simply returns @(t, e)@.
-- This function is very useful in 'checkExp'.
require :: HasPosition v => [Type] -> (Type, v) -> TypeM (Type, v)
require [] (_,e) = bad $ TypeError (posOf e) "Expression cannot have any type (probably a bug in the type checker)."
require ts (et,e)
  | et `elem` ts = return (et,e)
  | otherwise = bad $ TypeError (posOf e) $ "Expression type must be one of " ++ intercalate ", " (map ppType ts) ++ "."

elemType :: Type -> TypeM Type
elemType (Array t _ _) = return t
elemType t = bad $ TypeError (typePos t) $ "Type of expression is not array, but " ++ ppType t ++ "."

-- | Type check a program containing arbitrary type information,
-- yielding either a type error or a program with complete type
-- information.
checkProg :: TypeBox tf => Prog tf -> Either TypeError (Prog Identity)
checkProg prog = do
  ftable <- buildFtable
  let typeenv = TypeEnv { envVtable = M.empty
                        , envFtable = ftable}
  runTypeM typeenv $ mapM checkFun prog
  where
    -- To build the ftable we loop through the list of function
    -- definitions.  In addition to the normal ftable information
    -- (name, return type, argument types), we also keep track of
    -- position information, in order to report both locations of
    -- duplicate function definitions.  The position information is
    -- removed at the end.
    buildFtable = M.map rmPos <$> foldM expand builtins prog
    expand ftable (name,ret,args,_,pos)
      | Just (_,_,pos2) <- M.lookup name ftable =
        Left $ DupDefinitionError name pos pos2
      | otherwise =
        let argtypes = map snd args -- Throw away argument names.
        in Right $ M.insert name (ret,argtypes,pos) ftable
    rmPos (ret,args,_) = (ret,args)
    builtins = M.fromList [("toReal", (Real (0,0), [Int (0,0)], (0,0)))
                          ,("sqrt", (Real (0,0), [Real (0,0)], (0,0)))
                          ,("log", (Real (0,0), [Real (0,0)], (0,0)))
                          ,("exp", (Real (0,0), [Real (0,0)], (0,0)))
                          ,("op not", (Bool (0,0), [Bool (0,0)], (0,0)))
                          ,("op ~", (Real (0,0), [Real (0,0)], (0,0)))]

checkFun :: TypeBox tf => FunDec tf -> TypeM (FunDec Identity)
checkFun (fname, rettype, args, body, pos) = do
  args' <- checkArgs
  (bodytype, body') <- binding args' $ checkExp body
  if bodytype == rettype then
    return (fname, rettype, args, body', pos)
  else bad $ ReturnTypeError pos fname rettype bodytype
  where checkArgs = foldM expand [] args
        expand args' (pname, tp)
          | Just _ <- lookup pname args' =
            bad $ DupParamError fname pname pos
          | otherwise =
            return $ (pname, tp) : args'

-- | Type-check an expression, but convert all merge variables to
-- normal variables first.
checkSubExp :: TypeBox tf => Exp tf -> TypeM (Type, Exp Identity)
checkSubExp = unmerging . checkExp

checkExp :: TypeBox tf => Exp tf -> TypeM (Type, Exp Identity)
checkExp (Literal val) = do
  (t, val') <- checkLiteral val
  return (t, Literal val')
checkExp (TupLit es t pos) = do
  (ets, es') <- unzip <$> mapM checkSubExp es
  t' <- t `unifyWithKnown` Tuple ets pos
  return (t', TupLit es' (boxType t') pos)
checkExp (ArrayLit es t pos) = do
  (ets, es') <- unzip <$> mapM checkSubExp es
  -- Find the unified type of all subexpression types.
  et <- case ets of
          [] -> bad $ TypeError pos "Empty array literal"
          e:ets' -> foldM unifyKnownTypes e ets'
  -- Unify that type with the one given for the array literal.
  t' <- t `unifyWithKnown` et
  return (Array t' Nothing pos, ArrayLit es' (boxType t') pos)
checkExp (BinOp op e1 e2 t pos) = checkBinOp op e1 e2 t pos
checkExp (And e1 e2 pos) = do
  (_, e1') <- require [Bool pos] =<< checkSubExp e1
  (_, e2') <- require [Bool pos] =<< checkSubExp e2
  return (Bool pos, And e1' e2' pos)
checkExp (Or e1 e2 pos) = do
  (_, e1') <- require [Bool pos] =<< checkSubExp e1
  (_, e2') <- require [Bool pos] =<< checkSubExp e2
  return (Bool pos, Or e1' e2' pos)
checkExp (Not e pos) = require [Bool pos] =<< checkSubExp e
checkExp (Negate e t pos) = do
  (et,e') <- require [Int pos, Real pos] =<< checkSubExp e
  t' <- t `unifyWithKnown` et
  return (t', Negate e' (boxType t') pos)
checkExp (If e1 e2 e3 t pos) = do
  (_,e1') <- require [Bool pos] =<< checkSubExp e1
  (t2,e2') <- checkExp e2
  (t3,e3') <- checkExp e3
  bt <- unifyWithKnown t =<< unifyKnownTypes t2 t3
  return (bt, If e1' e2' e3' (boxType bt) pos)
checkExp (Var name t pos) = do
  vt <- lookupVarType name pos
  t' <- t `unifyWithKnown` vt
  tell $ TypeAcc (S.singleton name) S.empty
  return (t', Var name (boxType t') pos)
checkExp (Apply fname args t pos) = do
  bnd <- asks $ M.lookup fname . envFtable
  case bnd of
    Nothing -> bad $ UnknownFunctionError fname pos
    Just (rettype, paramtypes) -> do
      rettype' <- t `unifyWithKnown` rettype
      (argtypes, args') <- unzip <$> mapM checkSubExp args
      if length argtypes == length paramtypes then do
        zipWithM_ unifyKnownTypes argtypes paramtypes
        return (rettype', Apply fname args' (boxType rettype') pos)
      else bad $ ParameterMismatch fname pos (Right paramtypes) argtypes
checkExp (LetPat pat e body pos) = do
  ((et, e'), srcmvars) <- collectSrcMergeVars $ checkSubExp e
  bnds <- checkPattern pat et
  mvs <- S.fromList <$> mergeVars
  binding bnds $
    if basicType et then do
      (bt, body') <- checkExp body
      return (bt, LetPat pat e' body' pos)
    else do
      ((bt, body'), destmvars) <- collectDestMergeVars $ checkExp body
      let srcmvars'  = mvs `S.intersection` srcmvars
          destmvars' = mvs `S.intersection` destmvars
      case S.toList $ srcmvars' `S.intersection` destmvars' of
        (v:_) -> bad $ MergeVarNonBasicIndexing v pos
        _     -> return ()
      tell $ TypeAcc srcmvars destmvars
      return (bt, LetPat pat e' body' pos)
checkExp (LetWith name e idxes ve body pos) = do
  (et, e') <- checkSubExp e
  -- We don't check whether name is a merge variable.  We might want
  -- to change this.
  tell $ TypeAcc S.empty (S.singleton name)
  case peelArray (length idxes) et of
    Nothing -> bad $ TypeError pos $ show (length idxes) ++ " indices given, but type of expression at " ++ posStr (posOf e) ++ " has " ++ show (arrayDims et) ++ " dimensions."
    Just elemt -> do
      (_, idxes') <- unzip <$> mapM (require [Int pos] <=< checkSubExp) idxes
      (_, ve') <- require [elemt] =<< checkSubExp ve
      (bt, body') <- local (`bindVar` (name, et)) $ checkExp body
      return (bt, LetWith name e' idxes' ve' body' pos)
checkExp (Index name idxes intype restype pos) = do
  vt <- lookupVarType name pos
  when (arrayDims vt < length idxes) $
    bad $ TypeError pos $ show (length idxes) ++ " indices given, but type of variable " ++ name ++ " has " ++ show (arrayDims vt) ++ " dimensions."
  intype' <- intype `unifyWithKnown` baseType vt
  restype' <- restype `unifyWithKnown` strip (length idxes) vt
  (_, idxes') <- unzip <$> mapM (require [Int pos] <=< checkSubExp) idxes
  tell $ TypeAcc (S.singleton name) S.empty
  return (restype', Index name idxes' (boxType intype') (boxType restype') pos)
  where strip 0 t = t
        strip n (Array t _ _) = strip (n-1) t
        strip _ t = t
checkExp (Iota e pos) = do
  (_, e') <- require [Int pos] =<< checkSubExp e
  return (Array (Int pos) Nothing pos, Iota e' pos)
checkExp (Replicate countexp valexp outtype pos) = do
  (_, countexp') <- require [Int pos] =<< checkSubExp countexp
  (valtype, valexp') <- checkSubExp valexp
  outtype' <- outtype `unifyWithKnown` Array valtype Nothing pos
  return (outtype', Replicate countexp' valexp' (boxType outtype') pos)
checkExp (Reshape shapeexps arrexp intype restype pos) = do
  (_, shapeexps') <- unzip <$> mapM (require [Int pos] <=< checkSubExp) shapeexps
  (arrt, arrexp') <- checkSubExp arrexp
  intype' <- intype `unifyWithKnown` arrt
  restype' <- restype `unifyWithKnown` build (length shapeexps') (baseType intype')
  return (restype', Reshape shapeexps' arrexp' (boxType intype') (boxType restype') pos)
  where build 0 t = t
        build n t = build (n-1) (Array t Nothing (typePos t))
checkExp (Transpose arrexp intype outtype pos) = do
  (arrt, arrexp') <- checkSubExp arrexp
  when (arrayDims arrt < 2) $
    bad $ TypeError pos "Argument to transpose does not have two dimensions."
  intype' <- intype `unifyWithKnown` arrt
  outtype' <- outtype `unifyWithKnown` intype'
  return (outtype', Transpose arrexp' (boxType intype') (boxType outtype') pos)
checkExp (Map fun arrexp intype outtype pos) = do
  (arrt, arrexp') <- checkSubExp arrexp
  intype' <- intype `unifyWithKnown` arrt
  case intype' of
    Array t e pos2 -> do
      (fun', funret) <- checkLambda fun [t]
      outtype' <- outtype `unifyWithKnown` Array funret e pos2
      return (outtype', Map fun' arrexp' (boxType intype') (boxType outtype') pos)
    _       -> bad $ TypeError (posOf arrexp) "Expression does not return an array."
checkExp (Reduce fun startexp arrexp intype pos) = do
  (acct, startexp') <- checkSubExp startexp
  (arrt, arrexp') <- checkSubExp arrexp
  intype' <- intype `unifyWithKnown` arrt
  case intype' of
    Array inelemt _ _ -> do
      (fun', funret) <- checkLambda fun [acct, inelemt]
      when (acct /= funret) $
        bad $ TypeError pos $ "Accumulator is of type " ++ ppType acct ++ ", but reduce function returns type " ++ ppType funret ++ "."
      return (funret, Reduce fun' startexp' arrexp' (boxType intype') pos)
    _ -> bad $ TypeError (posOf arrexp) "Type of expression is not an array"
checkExp (ZipWith fun arrexps intypes outtype pos) = do
  (arrts, arrexps') <- unzip <$> mapM checkSubExp arrexps
  intypes' <- case unboxType intypes of
                Nothing -> return arrts
                Just intypes' -> zipWithM unifyKnownTypes intypes' arrts
  (fun', funret) <- checkLambda fun =<< mapM elemType intypes'
  outtype' <- outtype `unifyWithKnown` Array funret Nothing pos
  return (outtype',
          ZipWith fun' arrexps' (boxType intypes') (boxType outtype') pos)
checkExp (Scan fun startexp arrexp intype pos) = do
  (startt, startexp') <- checkSubExp startexp
  (arrt, arrexp') <- checkSubExp arrexp
  intype' <- intype `unifyWithKnown` arrt
  case intype' of
    Array inelemt e pos2 -> do
      (fun', funret) <- checkLambda fun [inelemt, inelemt]
      when (startt /= funret) $
        bad $ TypeError pos $ "Initial value is of type " ++ ppType startt ++ ", but scan function returns type " ++ ppType funret ++ "."
      when (inelemt /= funret) $
        bad $ TypeError pos $ "Array element value is of type " ++ ppType inelemt ++ ", but scan function returns type " ++ ppType funret ++ "."
      return (Array funret e pos2, Scan fun' startexp' arrexp' (boxType intype') pos)
    _ -> bad $ TypeError (posOf arrexp) "Type of expression is not an array."
checkExp (Filter fun arrexp arrtype pos) = do
  (arrexpt, arrexp') <- checkSubExp arrexp
  arrtype' <- arrtype `unifyWithKnown` arrexpt
  inelemt <- elemType arrtype'
  (fun', funret) <- checkLambda fun [inelemt]
  when (funret /= Bool pos) $
    bad $ TypeError pos "Filter function does not return bool."
  return (arrtype', Filter fun' arrexp' (boxType arrtype') pos)
checkExp (Mapall fun arrexp intype outtype pos) = do
  (arrt, arrexp') <- checkSubExp arrexp
  intype' <- intype `unifyWithKnown` arrt
  (fun', funret) <- checkLambda fun [baseType intype']
  outtype' <- outtype `unifyWithKnown` array (arrayDims intype') funret
  return (outtype', Mapall fun' arrexp' (boxType intype') (boxType outtype') pos)
checkExp (Redomap redfun mapfun accexp arrexp inarr outarr pos) = do
  (acct, accexp') <- checkSubExp accexp
  (arrt, arrexp') <- checkSubExp arrexp
  et <- elemType arrt
  (mapfun', mapret) <- checkLambda mapfun [et]
  (redfun', redret) <- checkLambda redfun [acct, mapret]
  _ <- unifyKnownTypes redret acct
  inarr' <- inarr `unifyWithKnown` Array et Nothing pos
  outarr' <- outarr `unifyWithKnown` Array redret Nothing pos
  return (redret, Redomap redfun' mapfun' accexp' arrexp' (boxType inarr') (boxType outarr') pos)
checkExp (Split splitexp arrexp inarr pos) = do
  (_, splitexp') <- require [Int pos] =<< checkSubExp splitexp
  (arrt, arrexp') <- checkSubExp arrexp
  inarr' <- inarr `unifyWithKnown` arrt
  return (inarr', Split splitexp' arrexp' (boxType inarr') pos)
checkExp (Concat arr1exp arr2exp inarr pos) = do
  (arr1t, arr1exp') <- checkSubExp arr1exp
  (arrt, arr2exp') <- require [arr1t] =<< checkSubExp arr2exp
  inarr' <- inarr `unifyWithKnown` arrt
  return (inarr', Concat arr1exp' arr2exp' (boxType inarr') pos)
checkExp (Read t pos) =
  return (t, Read t pos)
checkExp (Write e t pos) = do
  (et, e') <- checkSubExp e
  t' <- t `unifyWithKnown` et
  return (t', Write e' (boxType t') pos)
checkExp (DoLoop loopvar boundexp body mergevars pos) = do
  (_, boundexp') <- require [Int pos] =<< checkSubExp boundexp
  merging mergevars pos $ binding [(loopvar, Int pos)] $ do
    ts <- mapM (`lookupVarType` pos) mergevars
    let bodytype = case ts of [t] -> t
                              _   -> Tuple ts pos
    (bodyt, body') <- require [bodytype] =<< checkExp body
    return (bodyt, DoLoop loopvar boundexp' body' mergevars pos)

checkLiteral :: Value -> TypeM (Type, Value)
checkLiteral (IntVal k pos) = return (Int pos, IntVal k pos)
checkLiteral (RealVal x pos) = return (Real pos, RealVal x pos)
checkLiteral (LogVal b pos) = return (Bool pos, LogVal b pos)
checkLiteral (CharVal c pos) = return (Char pos, CharVal c pos)
checkLiteral (TupVal vals pos) = do
  (ts, vals') <- unzip <$> mapM checkLiteral vals
  return (Tuple ts pos, TupVal vals' pos)
checkLiteral (ArrayVal vals t pos) = do
  (ts, vals') <- unzip <$> mapM checkLiteral vals
  -- Find the unified type of all subexpression types.
  vt <- case ts of
          [] -> bad $ TypeError pos "Empty array literal"
          v:vts' -> foldM unifyKnownTypes v vts'
  -- Unify that type with the one given for the array literal.
  t' <- t `unifyKnownTypes` Array vt Nothing pos
  return (t', ArrayVal vals' t' pos)

checkBinOp :: TypeBox tf => BinOp -> Exp tf -> Exp tf -> tf Type -> Pos
           -> TypeM (Type, Exp Identity)
checkBinOp Plus e1 e2 t pos = checkPolyBinOp Plus [Real pos, Int pos] e1 e2 t pos
checkBinOp Minus e1 e2 t pos = checkPolyBinOp Minus [Real pos, Int pos] e1 e2 t pos
checkBinOp Pow e1 e2 t pos = checkPolyBinOp Pow [Real pos, Int pos] e1 e2 t pos
checkBinOp Times e1 e2 t pos = checkPolyBinOp Times [Real pos, Int pos] e1 e2 t pos
checkBinOp Divide e1 e2 t pos = checkPolyBinOp Divide [Real pos, Int pos] e1 e2 t pos
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

checkRelOp :: TypeBox tf => BinOp
           -> [Type] -> Exp tf -> Exp tf -> tf Type -> Pos -> TypeM (Type, Exp Identity)
checkRelOp op tl e1 e2 t pos = do
  (t1,e1') <- require tl =<< checkSubExp e1
  (t2,e2') <- require tl =<< checkSubExp e2
  _ <- unifyKnownTypes t1 t2
  t' <- t `unifyWithKnown` Bool pos
  return (Bool pos, BinOp op e1' e2' (boxType t') pos)

checkPolyBinOp :: TypeBox tf =>
                  BinOp -> [Type] -> Exp tf -> Exp tf -> tf Type -> Pos
               -> TypeM (Type, Exp Identity)
checkPolyBinOp op tl e1 e2 t pos = do
  (t1, e1') <- require tl =<< checkSubExp e1
  (t2, e2') <- require tl =<< checkSubExp e2
  t' <- unifyKnownTypes t1 t2
  t'' <- t `unifyWithKnown` t'
  return (t'', BinOp op e1' e2' (boxType t'') pos)

checkPattern :: TupIdent -> Type -> TypeM [Binding]
checkPattern pat vt = map rmPos <$> execStateT (checkPattern' pat vt) []
  where checkPattern' (Id name pos) t = add name t pos
        checkPattern' (TupId pats _) (Tuple ts _)
          | length pats == length ts = zipWithM_ checkPattern' pats ts
        checkPattern' _ _ = lift $ bad $ InvalidPatternError pat vt

        add name t pos = do
          bnd <- gets $ lookup name
          case bnd of
            Nothing       -> modify ((name,(t,pos)):)
            Just (_,pos2) -> lift $ bad $ DupPatternError name pos pos2
        rmPos (name,(t,_)) = (name,t)

checkLambda :: TypeBox tf => Lambda tf -> [Type] -> TypeM (Lambda Identity, Type)
checkLambda (AnonymFun params body ret pos) args
  | length params == length args = do
  mvs <- mergeVars
  (_, ret', params', body', _) <-
    unbinding mvs $ checkFun ("<anonymous>", ret, params, body, pos)
  zipWithM_ unifyKnownTypes (map snd params') args
  return (AnonymFun params body' ret' pos, ret')
  | otherwise = bad $ TypeError pos $ "Anonymous function defined with " ++ show (length params) ++ " parameters, but expected to take " ++ show (length args) ++ " arguments."
checkLambda (CurryFun opfun curryargexps curryargts rettype pos) args
  | Just op <- lookup opfun ops =
  checkPolyLambdaOp op curryargexps curryargts rettype args pos
  where ops = map (\op -> ("op " ++ opStr op, op)) [minBound..maxBound]
checkLambda (CurryFun fname curryargexps curryargts rettype pos) args = do
  (curryargexpts, curryargexps') <- unzip <$> mapM checkSubExp curryargexps
  curryargts' <- case unboxType curryargts of
                   Nothing -> return curryargexpts
                   Just curryargts' -> zipWithM unifyKnownTypes curryargts' curryargexpts
  let args' = curryargts' ++ args
  bnd <- asks $ M.lookup fname . envFtable
  case bnd of
    Nothing -> bad $ UnknownFunctionError fname pos
    Just (rt, paramtypes) -> do
      when (length args' /= length paramtypes ||
            not (all (uncurry (==)) $ zip args' paramtypes)) $
        bad $ ParameterMismatch fname pos (Right paramtypes) args'
      rettype' <- rettype `unifyWithKnown` rt
      zipWithM_ unifyKnownTypes (curryargts'++args) paramtypes
      return (CurryFun fname curryargexps' (boxType curryargts') (boxType rettype') pos, rettype')

checkPolyLambdaOp :: (TypeBox tf) =>
                     BinOp -> [Exp tf] -> tf [Type] -> tf Type -> [Type] -> Pos
                  -> TypeM (Lambda Identity, Type)
checkPolyLambdaOp op curryargexps curryargts rettype args pos = do
  (curryargexpts, curryargexps') <- unzip <$> mapM checkSubExp curryargexps
  curryargts' <- case unboxType curryargts of
                   Nothing          -> return curryargexpts
                   Just curryargts' -> zipWithM unifyKnownTypes curryargts' curryargexpts
  tp <- case curryargts' ++ args of
          [t1, t2] | t1 == t2 -> return t1
          l -> bad $ ParameterMismatch fname pos (Left 2) l
  (x,y,params) <- case curryargexps' of
                    [] -> return (Var "x" (boxType tp) pos,
                                  Var "y" (boxType tp) pos,
                                  [("x", tp), ("y", tp)])
                    [e] -> return (e,
                                   Var "y" (boxType tp) pos,
                                   [("y", tp)])
                    (e1:e2:_) -> return (e1, e2, [])
  (fun, t) <- checkLambda (AnonymFun params (BinOp op x y (boxType tp) pos) tp pos)
              $ curryargts' ++ args
  t' <- rettype `unifyWithKnown` t
  return (fun, t')
  where fname = "op" ++ ppBinOp op
