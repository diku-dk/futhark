{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module L0.TypeChecker ( checkProg
                      , TypeError(..)) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.List

import qualified Data.Map as M

import L0.AbSyn

data TypeEnv = TypeEnv { env_vtable :: M.Map String Type
                       , env_ftable :: M.Map String (Type, [Type]) }

data TypeError = TypeError Pos String
               | UnifyError Type Type
               | ReturnTypeError Pos String Type Type
               | DupDefinitionError String Pos Pos
               | DupParamError String String Pos
               | DupPatternError String Pos Pos
               | InvalidPatternError TupIdent Type
               | UnknownVariableError String Pos
               | UnknownFunctionError String Pos
               | ParameterCountMismatch String Int Int

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
  show (ParameterCountMismatch fname expected got) =
    "Function " ++ fname ++ " expects " ++ show expected ++ " arguments, but got " ++ show got ++ "."

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
  env { env_vtable = M.insert name tp $ env_vtable env }

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

unifyTypes :: TypeBox tf => tf Type -> tf Type -> TypeM (tf Type)
unifyTypes t1 t2 = case (unboxType t1, unboxType t2) of
                     (Just t1', Nothing) -> return $ boxType t1'
                     (Nothing, Just t2') -> return $ boxType t2'
                     (Nothing, Nothing) -> return t1
                     (Just t1', Just t2') -> boxType <$> unifyKnownTypes t1' t2'

unifyWithKnown :: TypeBox tf => tf Type -> Type -> TypeM Type
unifyWithKnown t1 t2 = case unboxType t1 of
                         Nothing -> return t2
                         Just t1' -> unifyKnownTypes t2 t1'

require :: [Type] -> (Type, Exp Identity) -> TypeM (Type, Exp Identity)
require [] (_,e) = bad $ TypeError (expPos e) "Expression cannot have any type (probably a bug in the type checker)."
require ts (et,e)
  | et `elem` ts = return (et,e)
  | otherwise = bad $ TypeError (expPos e) $ "Expression type must be one of " ++ intercalate ", " (map ppType ts) ++ "."

checkProg :: Prog Maybe -> Either TypeError (Prog Identity)
checkProg prog = do
  ftable <- buildFtable
  let typeenv = TypeEnv { env_vtable = M.empty
                        , env_ftable = ftable }
  runTypeM typeenv $ mapM checkFun prog
  where
    -- To build the ftable we loop through the list of function
    -- definitions.  In addition to the normal ftable information
    -- (name, return type, argument types), we also keep track of
    -- position information, in order to report both locations of
    -- duplicate function definitions.  The position information is
    -- removed at the end.
    buildFtable = M.map rmPos <$> foldM expand M.empty prog
    expand ftable (name,ret,args,_,pos)
      | Just (_,_,pos2) <- M.lookup name ftable =
        Left $ DupDefinitionError name pos pos2
      | otherwise =
        let argtypes = map snd args -- Throw away argument names.
        in Right $ M.insert name (ret,argtypes,pos) ftable
    rmPos (ret,args,_) = (ret,args)

checkFun :: FunDec Maybe -> TypeM (FunDec Identity)
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

checkExp :: Exp Maybe -> TypeM (Type, Exp Identity)
checkExp (NumInt k pos) = return (Int pos, NumInt k pos)
checkExp (NumReal x pos) = return (Real pos, NumReal x pos)
checkExp (Log b pos) = return (Bool pos, Log b pos)
checkExp (CharLit c pos) = return (Char pos, CharLit c pos)
checkExp (StringLit s pos) = return (Array (Char pos) Nothing pos, StringLit s pos)
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
  t' <- t `unifyWithKnown` et
  return (t', ArrayLit es' (boxType t') pos)
checkExp (Plus e1 e2 t pos) = checkPolyBinOp Plus [Real pos, Int pos] e1 e2 t pos
checkExp (Minus e1 e2 t pos) = checkPolyBinOp Minus [Real pos, Int pos] e1 e2 t pos
checkExp (Pow e1 e2 t pos) = checkPolyBinOp Pow [Real pos, Int pos] e1 e2 t pos
checkExp (Times e1 e2 t pos) = checkPolyBinOp Times [Real pos, Int pos] e1 e2 t pos
checkExp (Divide e1 e2 t pos) = checkPolyBinOp Divide [Real pos, Int pos] e1 e2 t pos
checkExp (ShiftR e1 e2 pos) = checkMonoBinOp ShiftR (Int pos) e1 e2 pos
checkExp (ShiftL e1 e2 pos) = checkMonoBinOp ShiftL (Int pos) e1 e2 pos
checkExp (Band e1 e2 pos) = checkMonoBinOp Band (Int pos) e1 e2 pos
checkExp (Xor e1 e2 pos) = checkMonoBinOp Xor (Int pos) e1 e2 pos
checkExp (Bor e1 e2 pos) = checkMonoBinOp Bor (Int pos) e1 e2 pos
checkExp (And e1 e2 pos) = checkMonoBinOp And (Bool pos) e1 e2 pos
checkExp (Or e1 e2 pos) = checkMonoBinOp Or (Bool pos) e1 e2 pos
checkExp (Equal e1 e2 pos) = checkRelOp Equal [Int pos, Real pos, Bool pos] e1 e2 pos
checkExp (Less e1 e2 pos) = checkRelOp Less [Int pos, Real pos, Bool pos] e1 e2 pos
checkExp (Leq e1 e2 pos) = checkRelOp Leq [Int pos, Real pos, Bool pos] e1 e2 pos
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
  bnd <- asks $ M.lookup name . env_vtable
  case bnd of Nothing -> bad $ UnknownVariableError name pos
              Just vt -> do
                t' <- t `unifyWithKnown` vt
                return (t', Var name (boxType t') pos)
checkExp (Apply fname args t pos) = do
  bnd <- asks $ M.lookup fname . env_ftable
  case bnd of
    Nothing -> bad $ UnknownFunctionError fname pos
    Just (rettype, paramtypes) -> do
      rettype' <- t `unifyWithKnown` rettype
      (argtypes, args') <- unzip <$> mapM checkExp args
      if length argtypes == length paramtypes then do
        zipWithM_ unifyKnownTypes argtypes paramtypes
        return (rettype', Apply fname args' (boxType rettype') pos)
      else bad $ ParameterCountMismatch fname (length paramtypes) (length argtypes)
checkExp (Let pat e Nothing Nothing body pos) = do
  (et, e') <- checkExp e
  bnds <- checkPattern pat et
  local (`bindVars` bnds) $ do
    (bt, body') <- checkExp body
    return (bt, Let pat e' Nothing Nothing body' pos)
checkExp (Let (Id name namepos) e (Just idxes) (Just ve) body pos) = do
  (et, e') <- checkExp e
  when (arrayDims et < length idxes) $
    bad $ TypeError pos $ show (length idxes) ++ " indices given, but type of expression at " ++ posStr (expPos e) ++ " has " ++ show (arrayDims et) ++ " dimensions."
  (_, idxes') <- unzip <$> mapM (require [Int pos] <=< checkExp) idxes
  (_, ve') <- require [baseType et] =<< checkExp ve
  local (`bindVar` (name, et)) $ do
    (bt, body') <- checkExp body
    return (bt, Let (Id name namepos) e' (Just idxes') (Just ve') body' pos)
checkExp (Let (TupId _ _) _ _ _ _ pos) =
  bad $ TypeError pos "Cannot use tuple pattern when using rebinding syntax"
checkExp (Index name idxes intype restype pos) = do
  bnd <- asks $ M.lookup name . env_vtable
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
checkExp (Replicate countexp valexp elemtype pos) = do
  (_, countexp') <- require [Int pos] =<< checkExp countexp
  (valtype, valexp') <- checkExp valexp
  elemtype' <- elemtype `unifyWithKnown` valtype
  return (elemtype', Replicate countexp' valexp' (boxType elemtype') pos)
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

checkMonoBinOp :: (Exp Identity -> Exp Identity -> Pos -> Exp Identity)
               -> Type -> Exp Maybe -> Exp Maybe -> Pos -> TypeM (Type, Exp Identity)
checkMonoBinOp op t e1 e2 = checkPolyBinOp (\e1' e2' _ pos' -> op e1' e2' pos') [t] e1 e2 Nothing

checkRelOp :: (Exp Identity -> Exp Identity -> Pos -> Exp Identity)
           -> [Type] -> Exp Maybe -> Exp Maybe -> Pos -> TypeM (Type, Exp Identity)
checkRelOp op tl e1 e2 pos = do
  (_, e) <- checkPolyBinOp (\e1' e2' _ pos' -> op e1' e2' pos') tl e1 e2 Nothing pos
  return (Bool pos, e)

checkPolyBinOp :: TypeBox tf =>
                  (Exp Identity -> Exp Identity -> Identity Type -> Pos -> Exp Identity)
               -> [Type] -> Exp Maybe -> Exp Maybe -> tf Type -> Pos -> TypeM (Type, Exp Identity)
checkPolyBinOp op tl e1 e2 t pos = do
  (t1,e1') <- require tl =<< checkExp e1
  (t2,e2') <- require tl =<< checkExp e2
  t' <- (t `unifyWithKnown`) =<< unifyKnownTypes t1 t2
  return (t', op e1' e2' (boxType t') pos)

checkPattern :: TupIdent -> Type -> TypeM [Binding]
checkPattern pat vt = map rmPos <$> execStateT (checkPattern' pat vt) []
  where checkPattern' (Id name pos) t = add name t pos
        checkPattern' (TupId pats _) (Tuple ts _)
          | length pats == length ts = zipWithM_ checkPattern' pats ts
        checkPattern' _ _ = lift $ bad $ InvalidPatternError pat vt

        add name t pos = do
          bnd <- gets $ lookup name
          case bnd of
            Nothing       -> modify $ ((name,(t,pos)):)
            Just (_,pos2) -> lift $ bad $ DupPatternError name pos pos2
        rmPos (name,(t,_)) = (name,t)
