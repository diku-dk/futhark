{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module L0.TypeChecker ( checkProg
                      , TypeError(..))
  where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.List

import qualified Data.Map as M

import L0.AbSyn

data TypeEnv = TypeEnv { envVtable :: M.Map String Type
                       , envFtable :: M.Map String (Type, [Type]) }

data TypeError = TypeError Pos String
               | UnifyError Type Type
               | ReturnTypeError Pos String Type Type
               | DupDefinitionError String Pos Pos
               | DupParamError String String Pos
               | DupPatternError String Pos Pos
               | InvalidPatternError TupIdent Type
               | UnknownVariableError String Pos
               | UnknownFunctionError String Pos
               | ParameterMismatch String Pos [Type] [Type]

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
    intercalate ", " (map ppType expected) ++ ", but got " ++ show ngot ++
    " arguments of types " ++ intercalate ", " (map ppType got) ++ "."
    where nexpected = length expected
          ngot = length got

class TypeBox tf where
  unboxType :: tf t -> Maybe t
  boxType :: t -> tf t

instance TypeBox Maybe where
  unboxType = id
  boxType = Just

instance TypeBox Identity where
  unboxType = Just . runIdentity
  boxType = Identity

newtype TypeM a = TypeM (ReaderT TypeEnv (Either TypeError) a)
  deriving (Monad, Functor, MonadReader TypeEnv)

runTypeM :: TypeEnv -> TypeM a -> Either TypeError a
runTypeM env (TypeM m) = runReaderT m env

bad :: TypeError -> TypeM a
bad = TypeM . lift . Left

bindVar :: TypeEnv -> Binding -> TypeEnv
bindVar env (name,tp) =
  env { envVtable = M.insert name tp $ envVtable env }

bindVars :: TypeEnv -> [Binding] -> TypeEnv
bindVars = foldl bindVar

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

unifyWithKnown :: TypeBox tf => tf Type -> Type -> TypeM Type
unifyWithKnown t1 t2 = case unboxType t1 of
                         Nothing -> return t2
                         Just t1' -> unifyKnownTypes t2 t1'

require :: [Type] -> (Type, Exp Identity) -> TypeM (Type, Exp Identity)
require [] (_,e) = bad $ TypeError (expPos e) "Expression cannot have any type (probably a bug in the type checker)."
require ts (et,e)
  | et `elem` ts = return (et,e)
  | otherwise = bad $ TypeError (expPos e) $ "Expression type must be one of " ++ intercalate ", " (map ppType ts) ++ "."

elemType :: Type -> TypeM Type
elemType (Array t _ _) = return t
elemType t = bad $ TypeError (typePos t) $ "Type of expression is not array, but " ++ ppType t ++ "."

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
  (bodytype, body') <- local (`bindVars` args') $ checkExp body
  if bodytype == rettype then
    return (fname, rettype, args, body', pos)
  else bad $ ReturnTypeError pos fname rettype bodytype
  where checkArgs = foldM expand [] args
        expand args' (pname, tp)
          | Just _ <- lookup pname args' =
            bad $ DupParamError fname pname pos
          | otherwise =
            return $ (pname, tp) : args'

checkExp :: TypeBox tf => Exp tf -> TypeM (Type, Exp Identity)
checkExp (Literal val) = do
  (t, val') <- checkLiteral val
  return (t, Literal val')
checkExp (TupLit es t pos) = do
  (ets, es') <- unzip <$> mapM checkExp es
  t' <- t `unifyWithKnown` Tuple ets pos
  return (t', TupLit es' (boxType t') pos)
checkExp (ArrayLit es t pos) = do
  (ets, es') <- unzip <$> mapM checkExp es
  -- Find the unified type of all subexpression types.
  et <- case ets of
          [] -> bad $ TypeError pos "Empty array literal"
          e:ets' -> foldM unifyKnownTypes e ets'
  -- Unify that type with the one given for the array literal.
  t' <- t `unifyWithKnown` Array et Nothing pos
  return (t', ArrayLit es' (boxType t') pos)
checkExp (BinOp op e1 e2 t pos) = checkBinOp op e1 e2 t pos
checkExp (And e1 e2 pos) = do
  (_, e1') <- require [Bool pos] =<< checkExp e1
  (_, e2') <- require [Bool pos] =<< checkExp e2
  return (Bool pos, And e1' e2' pos)
checkExp (Or e1 e2 pos) = do
  (_, e1') <- require [Bool pos] =<< checkExp e1
  (_, e2') <- require [Bool pos] =<< checkExp e2
  return (Bool pos, Or e1' e2' pos)
checkExp (Not e pos) = require [Bool pos] =<< checkExp e
checkExp (Negate e t pos) = do
  (et,e') <- require [Int pos, Real pos] =<< checkExp e
  t' <- t `unifyWithKnown` et
  return (t', Negate e' (boxType t') pos)
checkExp (If e1 e2 e3 t pos) = do
  (_,e1') <- require [Bool pos] =<< checkExp e1
  (t2,e2') <- checkExp e2
  (t3,e3') <- checkExp e3
  bt <- unifyWithKnown t =<< unifyKnownTypes t2 t3
  return (bt, If e1' e2' e3' (boxType bt) pos)
checkExp (Var name t pos) = do
  bnd <- asks $ M.lookup name . envVtable
  case bnd of Nothing -> bad $ UnknownVariableError name pos
              Just vt -> do
                t' <- t `unifyWithKnown` vt
                return (t', Var name (boxType t') pos)
checkExp (Apply fname args t pos) = do
  bnd <- asks $ M.lookup fname . envFtable
  case bnd of
    Nothing -> bad $ UnknownFunctionError fname pos
    Just (rettype, paramtypes) -> do
      rettype' <- t `unifyWithKnown` rettype
      (argtypes, args') <- unzip <$> mapM checkExp args
      if length argtypes == length paramtypes then do
        zipWithM_ unifyKnownTypes argtypes paramtypes
        return (rettype', Apply fname args' (boxType rettype') pos)
      else bad $ ParameterMismatch fname pos paramtypes argtypes
checkExp (Let pat e Nothing Nothing body pos) = do
  (et, e') <- checkExp e
  bnds <- checkPattern pat et
  local (`bindVars` bnds) $ do
    (bt, body') <- checkExp body
    return (bt, Let pat e' Nothing Nothing body' pos)
checkExp (Let (TupId _ _) _ _ _ _ pos) =
  bad $ TypeError pos "Cannot use tuple pattern when using rebinding syntax"
checkExp (Let (Id name namepos) e (Just idxes) (Just ve) body pos) = do
  (et, e') <- checkExp e
  case indexArray (length idxes) et of
    Nothing -> bad $ TypeError pos $ show (length idxes) ++ " indices given, but type of expression at " ++ posStr (expPos e) ++ " has " ++ show (arrayDims et) ++ " dimensions."
    Just elemt -> do
      (_, idxes') <- unzip <$> mapM (require [Int pos] <=< checkExp) idxes
      (_, ve') <- require [elemt] =<< checkExp ve
      (bt, body') <- local (`bindVar` (name, et)) $ checkExp body
      return (bt, Let (Id name namepos) e' (Just idxes') (Just ve') body' pos)
checkExp (Let (Id _ _) _ _ _ _ pos) =
  bad $ TypeError pos "Index or element part missing (I think this should never happen)."
checkExp (Index name idxes intype restype pos) = do
  bnd <- asks $ M.lookup name . envVtable
  case bnd of
    Nothing -> bad $ UnknownVariableError name pos
    Just vt -> do
      when (arrayDims vt < length idxes) $
        bad $ TypeError pos $ show (length idxes) ++ " indices given, but type of variable " ++ name ++ " has " ++ show (arrayDims vt) ++ " dimensions."
      intype' <- intype `unifyWithKnown` baseType vt
      restype' <- restype `unifyWithKnown` strip (length idxes) vt
      (_, idxes') <- unzip <$> mapM (require [Int pos] <=< checkExp) idxes
      return (restype', Index name idxes' (boxType intype') (boxType restype') pos)
  where strip 0 t = t
        strip n (Array t _ _) = strip (n-1) t
        strip _ t = t
checkExp (Iota e pos) = do
  (_, e') <- require [Int pos] =<< checkExp e
  return (Array (Int pos) Nothing pos, Iota e' pos)
checkExp (Replicate countexp valexp outtype pos) = do
  (_, countexp') <- require [Int pos] =<< checkExp countexp
  (valtype, valexp') <- checkExp valexp
  outtype' <- outtype `unifyWithKnown` Array valtype Nothing pos
  return (outtype', Replicate countexp' valexp' (boxType outtype') pos)
checkExp (Reshape shapeexps arrexp intype restype pos) = do
  (_, shapeexps') <- unzip <$> mapM (require [Int pos] <=< checkExp) shapeexps
  (arrt, arrexp') <- checkExp arrexp
  intype' <- intype `unifyWithKnown` arrt
  restype' <- restype `unifyWithKnown` build (length shapeexps') (baseType intype')
  return (restype', Reshape shapeexps' arrexp' (boxType intype') (boxType restype') pos)
  where build 0 t = t
        build n t = build (n-1) (Array t Nothing (typePos t))
checkExp (Transpose arrexp intype outtype pos) = do
  (arrt, arrexp') <- checkExp arrexp
  when (arrayDims arrt < 2) $
    bad $ TypeError pos "Argument to transpose does not have two dimensions."
  intype' <- intype `unifyWithKnown` arrt
  outtype' <- outtype `unifyWithKnown` intype'
  return (outtype', Transpose arrexp' (boxType intype') (boxType outtype') pos)
checkExp (Map fun arrexp intype outtype pos) = do
  (arrt, arrexp') <- checkExp arrexp
  intype' <- intype `unifyWithKnown` arrt
  case intype' of
    Array t e pos2 -> do
      (fun', funret) <- checkLambda fun [t]
      outtype' <- outtype `unifyWithKnown` Array funret e pos2
      return (outtype', Map fun' arrexp' (boxType intype') (boxType outtype') pos)
    _       -> bad $ TypeError (expPos arrexp) "Expression does not return an array."
checkExp (Reduce fun startexp arrexp intype pos) = do
  (acct, startexp') <- checkExp startexp
  (arrt, arrexp') <- checkExp arrexp
  intype' <- intype `unifyWithKnown` arrt
  case intype' of
    Array inelemt _ _ -> do
      (fun', funret) <- checkLambda fun [acct, inelemt]
      when (acct /= funret) $
        bad $ TypeError pos $ "Accumulator is of type " ++ ppType acct ++ ", but reduce function returns type " ++ ppType funret ++ "."
      return (funret, Reduce fun' startexp' arrexp' (boxType intype') pos)
    _ -> bad $ TypeError (expPos arrexp) "Type of expression is not an array"
checkExp (ZipWith fun arrexps intypes outtype pos) = do
  (arrts, arrexps') <- unzip <$> mapM checkExp arrexps
  intypes' <- case unboxType intypes of
                Nothing -> return arrts
                Just intypes' -> zipWithM unifyKnownTypes intypes' arrts
  (fun', funret) <- checkLambda fun =<< mapM elemType intypes'
  outtype' <- outtype `unifyWithKnown` Array funret Nothing pos
  return (outtype',
          ZipWith fun' arrexps' (boxType intypes') (boxType outtype') pos)
checkExp (Scan fun startexp arrexp intype pos) = do
  (startt, startexp') <- checkExp startexp
  (arrt, arrexp') <- checkExp arrexp
  intype' <- intype `unifyWithKnown` arrt
  case intype' of
    Array inelemt e pos2 -> do
      (fun', funret) <- checkLambda fun [inelemt, inelemt]
      when (startt /= funret) $
        bad $ TypeError pos $ "Initial value is of type " ++ ppType startt ++ ", but scan function returns type " ++ ppType funret ++ "."
      when (inelemt /= funret) $
        bad $ TypeError pos $ "Array element value is of type " ++ ppType inelemt ++ ", but scan function returns type " ++ ppType funret ++ "."
      return (Array funret e pos2, Scan fun' startexp' arrexp' (boxType intype') pos)
    _ -> bad $ TypeError (expPos arrexp) "Type of expression is not an array."
checkExp (Filter fun arrexp arrtype pos) = do
  (arrexpt, arrexp') <- checkExp arrexp
  arrtype' <- arrtype `unifyWithKnown` arrexpt
  inelemt <- elemType arrtype'
  (fun', funret) <- checkLambda fun [inelemt]
  when (funret /= Bool pos) $
    bad $ TypeError pos "Filter function does not return bool."
  return (arrtype', Filter fun' arrexp' (boxType arrtype') pos)
checkExp (Mapall fun arrexp intype outtype pos) = do
  (arrt, arrexp') <- checkExp arrexp
  intype' <- intype `unifyWithKnown` arrt
  (fun', funret) <- checkLambda fun [baseType intype']
  outtype' <- outtype `unifyWithKnown` array (arrayDims intype') funret
  return (outtype', Mapall fun' arrexp' (boxType intype') (boxType outtype') pos)
checkExp (Redomap redfun mapfun accexp arrexp inarr outarr pos) = do
  (acct, accexp') <- checkExp accexp
  (arrt, arrexp') <- checkExp arrexp
  et <- elemType arrt
  (mapfun', mapret) <- checkLambda mapfun [et]
  (redfun', redret) <- checkLambda redfun [acct, mapret]
  _ <- unifyKnownTypes redret acct
  inarr' <- inarr `unifyWithKnown` Array et Nothing pos
  outarr' <- outarr `unifyWithKnown` Array redret Nothing pos
  return (redret, Redomap redfun' mapfun' accexp' arrexp' (boxType inarr') (boxType outarr') pos)
checkExp (Split splitexp arrexp inarr pos) = do
  (_, splitexp') <- require [Int pos] =<< checkExp splitexp
  (arrt, arrexp') <- checkExp arrexp
  inarr' <- inarr `unifyWithKnown` arrt
  return (inarr', Split splitexp' arrexp' (boxType inarr') pos)
checkExp (Concat arr1exp arr2exp inarr pos) = do
  (arr1t, arr1exp') <- checkExp arr1exp
  (arrt, arr2exp') <- require [arr1t] =<< checkExp arr2exp
  inarr' <- inarr `unifyWithKnown` arrt
  return (inarr', Concat arr1exp' arr2exp' (boxType inarr') pos)
checkExp (Read t pos) =
  return (t, Read t pos)
checkExp (Write e t pos) = do
  (et, e') <- checkExp e
  t' <- t `unifyWithKnown` et
  return (t', Write e' (boxType t') pos)
checkExp (DoLoop loopvar boundexp body mergevars pos) = do
  (_, boundexp') <- require [Int pos] =<< checkExp boundexp
  (bodyt, body') <- local (`bindVar` (loopvar, Int pos)) $ checkExp body
  forM_ mergevars $ \name -> do
    -- Just check if they exist.
    bnd <- asks $ M.lookup name . envVtable
    case bnd of Nothing -> bad $ UnknownVariableError name pos
                Just _  -> return ()
  return (bodyt, DoLoop loopvar boundexp' body' mergevars pos)

checkLiteral :: Value -> TypeM (Type, Value)
checkLiteral (IntVal k pos) = return (Int pos, IntVal k pos)
checkLiteral (RealVal x pos) = return (Real pos, RealVal x pos)
checkLiteral (LogVal b pos) = return (Bool pos, LogVal b pos)
checkLiteral (CharVal c pos) = return (Char pos, CharVal c pos)
checkLiteral (StringVal s pos) = return (Array (Char pos) Nothing pos, StringVal s pos)
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
  (t1,e1') <- require tl =<< checkExp e1
  (t2,e2') <- require tl =<< checkExp e2
  _ <- unifyKnownTypes t1 t2
  t' <- t `unifyWithKnown` Bool pos
  return (Bool pos, BinOp op e1' e2' (boxType t') pos)

checkPolyBinOp :: TypeBox tf =>
                  BinOp -> [Type] -> Exp tf -> Exp tf -> tf Type -> Pos
               -> TypeM (Type, Exp Identity)
checkPolyBinOp op tl e1 e2 t pos = do
  (t1, e1') <- require tl =<< checkExp e1
  (t2, e2') <- require tl =<< checkExp e2
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
  (_, ret', params', body', _) <- checkFun ("<anonymous>", ret, params, body, pos)
  zipWithM_ unifyKnownTypes (map snd params') args
  return (AnonymFun params body' ret' pos, ret')
  | otherwise = bad $ TypeError pos $ "Anonymous function defined with " ++ show (length params) ++ " parameters, but expected to take " ++ show (length args) ++ " arguments."
checkLambda (CurryFun opfun curryargexps curryargts rettype pos) args
  | Just op <- lookup opfun ops =
  checkPolyLambdaOp op curryargexps curryargts rettype args pos
  where ops = map (\op -> ("op " ++ opStr op, op)) [minBound..maxBound]
checkLambda (CurryFun fname curryargexps curryargts rettype pos) args = do
  (curryargexpts, curryargexps') <- unzip <$> mapM checkExp curryargexps
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
        bad $ ParameterMismatch fname pos paramtypes args'
      rettype' <- rettype `unifyWithKnown` rt
      zipWithM_ unifyKnownTypes (curryargts'++args) paramtypes
      return (CurryFun fname curryargexps' (boxType curryargts') (boxType rettype') pos, rettype')

checkPolyLambdaOp :: (TypeBox tf) =>
                     BinOp -> [Exp tf] -> tf [Type] -> tf Type -> [Type] -> Pos
                  -> TypeM (Lambda Identity, Type)
checkPolyLambdaOp op curryargexps curryargts rettype args pos = do
  (curryargexpts, curryargexps') <- unzip <$> mapM checkExp curryargexps
  curryargts' <- case unboxType curryargts of
                   Nothing          -> return curryargexpts
                   Just curryargts' -> zipWithM unifyKnownTypes curryargts' curryargexpts
  tp <- case curryargts' ++ args of
          [Real _, Real _] -> return $ Real pos
          [Int _, Int _] -> return $ Int pos
          l -> bad $ ParameterMismatch fname pos [Real pos, Real pos] l
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
