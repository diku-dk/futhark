{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module L0.TypeChecker ( checkProg
                      , TypeError(..))
  where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Array
import Data.List
import Data.Loc

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
               | MergeVarNonBasicIndexing String SrcLoc

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
  show (MergeVarNonBasicIndexing name pos) =
    "Merge variable " ++ name ++ " indexed at " ++ locStr pos ++
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
bindVar :: TypeEnv -> Ident Type -> TypeEnv
bindVar env (Ident name tp _) =
  env { envVtable = M.insert name (VarBinding tp NormalVar) $ envVtable env }

bindVars :: TypeEnv -> [Ident Type] -> TypeEnv
bindVars = foldl bindVar

binding :: [Ident Type] -> TypeM a -> TypeM a
binding bnds = local (`bindVars` bnds)

-- | 'unbinding names m' evaluates 'm' with the names in 'names'
-- unbound.
unbinding :: [String] -> TypeM a -> TypeM a
unbinding bnds = local (`unbindVars` bnds)
  where unbindVars = foldl unbindVar
        unbindVar env name = env { envVtable = M.delete name $ envVtable env }

-- | Rebind variables as merge variables while evaluating a 'TypeM'
-- action.
merging :: [String] -> SrcLoc -> TypeM a -> TypeM a
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

lookupVar :: String -> SrcLoc -> TypeM VarBinding
lookupVar name pos = do
  bnd <- asks $ M.lookup name . envVtable
  case bnd of Nothing   -> bad $ UnknownVariableError name pos
              Just bnd' -> return bnd'

lookupVarType :: String -> SrcLoc -> TypeM Type
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
unifyWithKnown :: TypeBox tf => tf -> Type -> TypeM Type
unifyWithKnown t1 t2 = case unboxType t1 of
                         Nothing -> return t2
                         Just t1' -> unifyKnownTypes t2 t1'

-- | @require ts (t, e)@ causes a 'TypeError' if @t@ does not unify
-- with one of the types in @ts@.  Otherwise, simply returns @(t, e)@.
-- This function is very useful in 'checkExp'.
require :: Located v => [Type] -> (Type, v) -> TypeM (Type, v)
require [] (_,e) = bad $ TypeError (srclocOf e) "Expression cannot have any type (probably a bug in the type checker)."
require ts (et,e)
  | et `elem` ts = return (et,e)
  | otherwise =
    bad $ TypeError (srclocOf e) $ "Expression type must be one of " ++
          intercalate ", " (map ppType ts) ++ ", but is " ++ ppType et ++ "."

elemType :: Type -> TypeM Type
elemType (Array t _ _) = return t
elemType t = bad $ TypeError (srclocOf t) $ "Type of expression is not array, but " ++ ppType t ++ "."

-- | Type check a program containing arbitrary type information,
-- yielding either a type error or a program with complete type
-- information.
checkProg :: TypeBox tf => Prog tf -> Either TypeError (Prog Type)
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
    buildFtable = M.map rmLoc <$> foldM expand builtins prog
    expand ftable (name,ret,args,_,pos)
      | Just (_,_,pos2) <- M.lookup name ftable =
        Left $ DupDefinitionError name pos pos2
      | otherwise =
        let argtypes = map identType args -- Throw away argument names.
        in Right $ M.insert name (ret,argtypes,pos) ftable
    rmLoc (ret,args,_) = (ret,args)
    builtins = M.fromList [("toReal", (Real noLoc, [Int noLoc], noLoc))
                          ,("trunc", (Int noLoc, [Real noLoc], noLoc))
                          ,("sqrt", (Real noLoc, [Real noLoc], noLoc))
                          ,("log", (Real noLoc, [Real noLoc], noLoc))
                          ,("exp", (Real noLoc, [Real noLoc], noLoc))
                          ,("op not", (Bool noLoc, [Bool noLoc], noLoc))
                          ,("op ~", (Real noLoc, [Real noLoc], noLoc))]

checkFun :: TypeBox tf => FunDec tf -> TypeM (FunDec Type)
checkFun (fname, rettype, args, body, pos) = do
  args' <- checkArgs
  (bodytype, body') <- binding args' $ checkExp body
  if bodytype == rettype then
    return (fname, rettype, args, body', pos)
  else bad $ ReturnTypeError pos fname rettype bodytype
  where checkArgs = foldM expand [] args
        expand args' ident@(Ident pname _ _)
          | Just _ <- find ((==identName ident) . identName) args' =
            bad $ DupParamError fname pname pos
          | otherwise =
            return $ ident : args'

-- | Type-check an expression, but convert all merge variables to
-- normal variables first.
checkSubExp :: TypeBox tf => Exp tf -> TypeM (Type, Exp Type)
checkSubExp = unmerging . checkExp

checkExp :: TypeBox tf => Exp tf -> TypeM (Type, Exp Type)
checkExp (Literal val) = do
  (t, val') <- checkLiteral val
  return (t, Literal val')
checkExp (TupLit es pos) = do
  (ets, es') <- unzip <$> mapM checkSubExp es
  let t = Tuple ets pos
  return (t, TupLit es' pos)
checkExp (ArrayLit es t pos) = do
  (ets, es') <- unzip <$> mapM checkSubExp es
  -- Find the unified type of all subexpression types.
  et <- case ets of
          [] -> bad $ TypeError pos "Empty array literal"
          e:ets' -> foldM unifyKnownTypes e ets'
  -- Unify that type with the one given for the array literal.
  t' <- t `unifyWithKnown` et
  return (Array t' Nothing pos, ArrayLit es' t' pos)
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
  return (t', Negate e' t' pos)
checkExp (If e1 e2 e3 t pos) = do
  (_,e1') <- require [Bool pos] =<< checkSubExp e1
  (t2,e2') <- checkExp e2
  (t3,e3') <- checkExp e3
  bt <- unifyWithKnown t =<< unifyKnownTypes t2 t3
  return (bt, If e1' e2' e3' bt pos)
checkExp (Var ident) = do
  (t, ident') <- checkIdent ident
  return (t, Var ident')
checkExp (Apply fname args t pos) = do
  bnd <- asks $ M.lookup fname . envFtable
  case bnd of
    Nothing -> bad $ UnknownFunctionError fname pos
    Just (rettype, paramtypes) -> do
      rettype' <- t `unifyWithKnown` rettype
      (argtypes, args') <- unzip <$> mapM checkSubExp args
      if length argtypes == length paramtypes then do
        zipWithM_ unifyKnownTypes argtypes paramtypes
        return (rettype', Apply fname args' rettype' pos)
      else bad $ ParameterMismatch (Just fname) pos (Right paramtypes) argtypes
checkExp (LetPat pat e body pos) = do
  ((et, e'), srcmvars) <- collectSrcMergeVars $ checkSubExp e
  (bnds, pat') <- checkPattern pat et
  mvs <- S.fromList <$> mergeVars
  binding bnds $
    if basicType et then do
      (bt, body') <- checkExp body
      return (bt, LetPat pat' e' body' pos)
    else do
      ((bt, body'), destmvars) <- collectDestMergeVars $ checkExp body
      let srcmvars'  = mvs `S.intersection` srcmvars
          destmvars' = mvs `S.intersection` destmvars
      case S.toList $ srcmvars' `S.intersection` destmvars' of
        (v:_) -> bad $ MergeVarNonBasicIndexing v pos
        _     -> return ()
      tell $ TypeAcc srcmvars destmvars
      return (bt, LetPat pat' e' body' pos)
checkExp (LetWith (Ident name _ _) e idxes ve body pos) = do
  (et, e') <- checkSubExp e
  -- We don't check whether name is a merge variable.  We might want
  -- to change this.
  tell $ TypeAcc S.empty (S.singleton name)
  case peelArray (length idxes) et of
    Nothing -> bad $ TypeError pos $ show (length idxes) ++ " indices given, but type of expression at " ++ locStr (srclocOf e) ++ " has " ++ show (arrayDims et) ++ " dimensions."
    Just elemt -> do
      (_, idxes') <- unzip <$> mapM (require [Int pos] <=< checkSubExp) idxes
      (_, ve') <- require [elemt] =<< checkSubExp ve
      (bt, body') <- local (`bindVar` Ident name et pos) $ checkExp body
      return (bt, LetWith (Ident name et pos) e' idxes' ve' body' pos)
checkExp (Index ident idxes intype restype pos) = do
  ident' <- snd <$> checkIdent ident
  vt <- lookupVarType (identName ident') pos
  when (arrayDims vt < length idxes) $
    bad $ TypeError pos $ show (length idxes) ++ " indices given, but type of variable " ++ identName ident' ++ " has " ++ show (arrayDims vt) ++ " dimensions."
  vet <- elemType vt
  intype' <- intype `unifyWithKnown` vet
  restype' <- restype `unifyWithKnown` strip (length idxes) vt
  (_, idxes') <- unzip <$> mapM (require [Int pos] <=< checkSubExp) idxes
  tell $ TypeAcc (S.singleton $ identName ident') S.empty
  return (restype', Index ident' idxes' intype' restype' pos)
  where strip 0 t = t
        strip n (Array t _ _) = strip (n-1) t
        strip _ t = t
checkExp (Iota e pos) = do
  (_, e') <- require [Int pos] =<< checkSubExp e
  return (Array (Int pos) Nothing pos, Iota e' pos)
checkExp (Size e pos) = do
  (et, e') <- checkSubExp e
  case et of
    Array {} -> return (Int pos, Size e' pos)
    _        -> bad $ TypeError pos "Argument to size must be array."
checkExp (Replicate countexp valexp outtype pos) = do
  (_, countexp') <- require [Int pos] =<< checkSubExp countexp
  (valtype, valexp') <- checkSubExp valexp
  outtype' <- outtype `unifyWithKnown` valtype
  return (Array outtype' Nothing pos,
          Replicate countexp' valexp' outtype' pos)
checkExp (Reshape shapeexps arrexp intype restype pos) = do
  (_, shapeexps') <- unzip <$> mapM (require [Int pos] <=< checkSubExp) shapeexps
  (arrt, arrexp') <- checkSubExp arrexp
  intype' <- intype `unifyWithKnown` arrt
  restype' <- restype `unifyWithKnown` build (length shapeexps') (baseType intype')
  return (restype', Reshape shapeexps' arrexp' intype' restype' pos)
  where build 0 t = t
        build n t = build (n-1) (Array t Nothing (srclocOf t))
checkExp (Transpose arrexp intype outtype pos) = do
  (arrt, arrexp') <- checkSubExp arrexp
  when (arrayDims arrt < 2) $
    bad $ TypeError pos "Argument to transpose does not have two dimensions."
  intype' <- intype `unifyWithKnown` arrt
  outtype' <- outtype `unifyWithKnown` intype'
  return (outtype', Transpose arrexp' intype' outtype' pos)
checkExp (Map fun arrexp intype outtype pos) = do
  (arrt, arrexp') <- checkSubExp arrexp
  case arrt of
    Array et e _ -> do
      (fun', funret) <- checkLambda fun [et]
      intype' <- intype `unifyWithKnown` et
      outtype' <- outtype `unifyWithKnown` funret
      return (Array outtype' e pos, Map fun' arrexp' intype' outtype' pos)
    _       -> bad $ TypeError (srclocOf arrexp) "Expression does not return an array."
checkExp (Reduce fun startexp arrexp intype pos) = do
  (acct, startexp') <- checkSubExp startexp
  (arrt, arrexp') <- checkSubExp arrexp
  case arrt of
    Array inelemt _ _ -> do
      inelemt' <- intype `unifyWithKnown` inelemt
      (fun', funret) <- checkLambda fun [acct, inelemt']
      when (acct /= funret) $
        bad $ TypeError pos $ "Accumulator is of type " ++ ppType acct ++ ", but reduce function returns type " ++ ppType funret ++ "."
      return (funret, Reduce fun' startexp' arrexp' inelemt' pos)
    _ -> bad $ TypeError (srclocOf arrexp) "Type of expression is not an array"
checkExp (Zip arrexps pos) = do
  (arrts, arrexps') <- unzip <$> mapM (checkSubExp . fst) arrexps
  inelemts <- mapM elemType arrts
  inelemts' <- zipWithM unifyWithKnown (map snd arrexps) inelemts
  let outtype = Array (Tuple inelemts' pos) Nothing pos
  return (outtype, Zip (zip arrexps' inelemts') pos)
checkExp (Unzip e _ pos) = do
  (et, e') <- checkSubExp e
  case et of
    Array (Tuple ts _) _ _ -> do
      let outtypes = map (\t -> Array t Nothing pos) ts
      return (Tuple outtypes pos, Unzip e' ts pos)
    _ -> bad $ TypeError pos $ "Argument to unzip is not an array of tuples, but " ++ ppType et ++ "."
checkExp (Scan fun startexp arrexp intype pos) = do
  (startt, startexp') <- checkSubExp startexp
  (arrt, arrexp') <- checkSubExp arrexp
  case arrt of
    Array inelemt e pos2 -> do
      intype' <- intype `unifyWithKnown` inelemt
      (fun', funret) <- checkLambda fun [intype', intype']
      when (startt /= funret) $
        bad $ TypeError pos $ "Initial value is of type " ++ ppType startt ++ ", but scan function returns type " ++ ppType funret ++ "."
      when (intype' /= funret) $
        bad $ TypeError pos $ "Array element value is of type " ++ ppType intype' ++ ", but scan function returns type " ++ ppType funret ++ "."
      return (Array funret e pos2, Scan fun' startexp' arrexp' intype' pos)
    _ -> bad $ TypeError (srclocOf arrexp) "Type of expression is not an array."
checkExp (Filter fun arrexp eltype pos) = do
  (arrexpt, arrexp') <- checkSubExp arrexp
  inelemt <- elemType arrexpt
  eltype' <- eltype `unifyWithKnown` inelemt
  (fun', funret) <- checkLambda fun [inelemt]
  when (funret /= Bool pos) $
    bad $ TypeError pos "Filter function does not return bool."
  return (Array eltype' Nothing pos, Filter fun' arrexp' eltype' pos)
checkExp (Mapall fun arrexp intype outtype pos) = do
  (arrt, arrexp') <- checkSubExp arrexp
  intype' <- intype `unifyWithKnown` baseType arrt
  (fun', funret) <- checkLambda fun [baseType intype']
  outtype' <- outtype `unifyWithKnown` funret
  return (arrayType (arrayDims arrt) outtype',
          Mapall fun' arrexp' intype' outtype' pos)
checkExp (Redomap redfun mapfun accexp arrexp intype outtype pos) = do
  (acct, accexp') <- checkSubExp accexp
  (arrt, arrexp') <- checkSubExp arrexp
  et <- elemType arrt
  (mapfun', mapret) <- checkLambda mapfun [et]
  (redfun', redret) <- checkLambda redfun [acct, mapret]
  _ <- unifyKnownTypes redret acct
  intype' <- intype `unifyWithKnown` et
  outtype' <- outtype `unifyWithKnown` redret
  return (redret, Redomap redfun' mapfun' accexp' arrexp' intype' outtype' pos)
checkExp (Split splitexp arrexp inarr pos) = do
  (_, splitexp') <- require [Int pos] =<< checkSubExp splitexp
  (arrt, arrexp') <- checkSubExp arrexp
  inarr' <- inarr `unifyWithKnown` arrt
  return (inarr', Split splitexp' arrexp' inarr' pos)
checkExp (Concat arr1exp arr2exp inarr pos) = do
  (arr1t, arr1exp') <- checkSubExp arr1exp
  (arrt, arr2exp') <- require [arr1t] =<< checkSubExp arr2exp
  inarr' <- inarr `unifyWithKnown` arrt
  return (inarr', Concat arr1exp' arr2exp' inarr' pos)
checkExp (Read t pos) =
  return (t, Read t pos)
checkExp (Write e t pos) = do
  (et, e') <- checkSubExp e
  t' <- t `unifyWithKnown` et
  return (t', Write e' t' pos)
checkExp (DoLoop (Ident loopvar _ _) boundexp body mergevars pos) = do
  (mergetypes, mergevars') <- unzip <$> mapM checkIdent mergevars
  (_, boundexp') <- require [Int pos] =<< checkSubExp boundexp
  merging (map identName mergevars') pos $ binding [Ident loopvar (Int pos) pos] $ do
    let bodytype = case mergetypes of
                     [t] -> t
                     _   -> Tuple mergetypes pos
    (bodyt, body') <- require [bodytype] =<< checkExp body
    return (bodyt, DoLoop (Ident loopvar (Int pos) pos) boundexp' body' mergevars' pos)

checkLiteral :: Value -> TypeM (Type, Value)
checkLiteral (IntVal k pos) = return (Int pos, IntVal k pos)
checkLiteral (RealVal x pos) = return (Real pos, RealVal x pos)
checkLiteral (LogVal b pos) = return (Bool pos, LogVal b pos)
checkLiteral (CharVal c pos) = return (Char pos, CharVal c pos)
checkLiteral (TupVal vals pos) = do
  (ts, vals') <- unzip <$> mapM checkLiteral vals
  return (Tuple ts pos, TupVal vals' pos)
checkLiteral (ArrayVal arr t pos) = do
  (ts, vals') <- unzip <$> mapM checkLiteral (elems arr)
  -- Find the unified type of all subexpression types.
  vt <- case ts of
          [] -> return t -- Permit empty array values, as they always
                         -- have a type.
          v:vts' -> foldM unifyKnownTypes v vts'
  -- Unify that type with the one given for the array literal.
  t' <- t `unifyKnownTypes` vt
  return (Array vt Nothing pos, ArrayVal (listArray (bounds arr) vals') t' pos)

checkIdent :: TypeBox ty => Ident ty -> TypeM (Type, Ident Type)
checkIdent (Ident name t pos) = do
  vt <- lookupVarType name pos
  t' <- t `unifyWithKnown` vt
  tell $ TypeAcc (S.singleton name) S.empty
  return (t', Ident name t' pos)

checkBinOp :: TypeBox tf => BinOp -> Exp tf -> Exp tf -> tf -> SrcLoc
           -> TypeM (Type, Exp Type)
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

checkRelOp :: TypeBox ty =>
              BinOp -> [Type] -> Exp ty -> Exp ty -> ty -> SrcLoc
           -> TypeM (Type, Exp Type)
checkRelOp op tl e1 e2 t pos = do
  (t1,e1') <- require tl =<< checkSubExp e1
  (t2,e2') <- require tl =<< checkSubExp e2
  _ <- unifyKnownTypes t1 t2
  t' <- t `unifyWithKnown` Bool pos
  return (Bool pos, BinOp op e1' e2' t' pos)

checkPolyBinOp :: TypeBox ty => BinOp -> [Type] -> Exp ty -> Exp ty -> ty -> SrcLoc
               -> TypeM (Type, Exp Type)
checkPolyBinOp op tl e1 e2 t pos = do
  (t1, e1') <- require tl =<< checkSubExp e1
  (t2, e2') <- require tl =<< checkSubExp e2
  t' <- unifyKnownTypes t1 t2
  t'' <- t `unifyWithKnown` t'
  return (t'', BinOp op e1' e2' t'' pos)

checkPattern :: TypeBox tf =>
                TupIdent tf -> Type -> TypeM ([Ident Type], TupIdent Type)
checkPattern pat vt = do
  (pat', bnds) <- runStateT (checkPattern' pat vt) []
  return (bnds, pat')
  where checkPattern' (Id (Ident name namet pos)) t = do
          add name t pos
          t' <- lift $ namet `unifyWithKnown` t
          return $ Id $ Ident name t' pos
        checkPattern' (TupId pats pos) (Tuple ts _)
          | length pats == length ts = do
          pats' <- zipWithM checkPattern' pats ts
          return $ TupId pats' pos
        checkPattern' _ _ = lift $ bad $ InvalidPatternError errpat vt

        add name t pos = do
          bnd <- gets $ find ((==name) . identName)
          case bnd of
            Nothing               -> modify (Ident name t pos:)
            Just (Ident _ _ pos2) -> lift $ bad $ DupPatternError name pos pos2
        -- A pattern with known type box (Maybe) for error messages.
        errpat = rmTypes pat
        rmTypes (Id (Ident name _ pos)) = Id $ Ident name Nothing pos
        rmTypes (TupId pats pos) = TupId (map rmTypes pats) pos

checkLambda :: TypeBox ty => Lambda ty -> [Type] -> TypeM (Lambda Type, Type)
checkLambda (AnonymFun params body ret pos) args = do
  mvs <- mergeVars
  (_, ret', params', body', _) <-
    unbinding mvs $ checkFun ("<anonymous>", ret, params, body, pos)
  case () of
    _ | length params == length args -> do
          zipWithM_ unifyKnownTypes (map identType params') args
          return (AnonymFun params body' ret' pos, ret')
      | [t@(Tuple args' _)] <- args,
        args' == map identType params,
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
checkLambda (CurryFun opfun curryargexps rettype pos) args
  | Just op <- lookup opfun ops =
  checkPolyLambdaOp op curryargexps rettype args pos
  where ops = map (\op -> ("op " ++ opStr op, op)) [minBound..maxBound]
checkLambda (CurryFun fname curryargexps rettype pos) args = do
  (curryargexpts, curryargexps') <- unzip <$> mapM checkSubExp curryargexps
  let args' = curryargexpts ++ args
  bnd <- asks $ M.lookup fname . envFtable
  case bnd of
    Nothing -> bad $ UnknownFunctionError fname pos
    Just (rt, paramtypes) -> do
      rettype' <- rettype `unifyWithKnown` rt
      case () of
        _ | [tupt@(Tuple ets _)] <- args,
            ets == paramtypes ->
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
          | length args' /= length paramtypes ||
            not (all (uncurry (==)) $ zip args' paramtypes) ->
              bad $ ParameterMismatch (Just fname) pos (Right paramtypes) args'
          | otherwise -> do
              zipWithM_ unifyKnownTypes (curryargexpts++args) paramtypes
              return (CurryFun fname curryargexps' rettype' pos, rettype')

checkPolyLambdaOp :: TypeBox ty => BinOp -> [Exp ty] -> ty -> [Type] -> SrcLoc
                  -> TypeM (Lambda Type, Type)
checkPolyLambdaOp op curryargexps rettype args pos = do
  (curryargexpts, curryargexps') <- unzip <$> mapM checkSubExp curryargexps
  tp <- case curryargexpts ++ args of
          [t1, t2] | t1 == t2 -> return t1
          [Tuple [t1,t2] _] | t1 == t2 -> return t1 -- For autoshimming.
          l -> bad $ ParameterMismatch (Just fname) pos (Left 2) l
  (x,y,params) <- case curryargexps' of
                    [] -> return (Var (Ident "x" tp pos),
                                  Var (Ident "y" tp pos),
                                  [Ident "x" tp pos, Ident "y" tp pos])
                    [e] -> return (e,
                                   Var (Ident "y" tp pos),
                                   [Ident "y" tp pos])
                    (e1:e2:_) -> return (e1, e2, [])
  let body = BinOp op x y tp pos
  (fun, t) <- checkLambda (AnonymFun params body tp pos) args
  t' <- rettype `unifyWithKnown` t
  return (fun, t')
  where fname = "op" ++ ppBinOp op
