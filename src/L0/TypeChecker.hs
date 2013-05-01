{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module L0.TypeChecker ( checkProg
                      , TypeError(..))
  where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.Array
import Data.List
import Data.Loc
import Data.Maybe
import Data.Monoid

import qualified Data.Map as M

import L0.AbSyn

-- | A tuple of a return type and a list of argument types.
type FunBinding = (Type, [Type])

-- | A pair of a variable table and a function table.  Type checking
-- happens with access to this environment.  The function table is
-- only initialised at the very beginning, but the variable table will
-- be extended during type-checking when let-expressions are
-- encountered.
data TypeEnv = TypeEnv { envVtable :: M.Map String Type
                       , envFtable :: M.Map String FunBinding }

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

-- | The type checker runs in this monad.  Note that it has no mutable
-- state, but merely keeps track of current bindings in a 'TypeEnv'.
-- The 'Either' monad is used for error handling.
newtype TypeM a = TypeM (ReaderT TypeEnv (Either TypeError) a)
  deriving (Monad, Functor, MonadReader TypeEnv)

runTypeM :: TypeEnv -> TypeM a -> Either TypeError a
runTypeM env (TypeM m) = runReaderT m env

bad :: TypeError -> TypeM a
bad = TypeM . lift . Left

-- | Bind a name as a common (non-merge) variable.
bindVar :: TypeEnv -> Ident Type -> TypeEnv
bindVar env (Ident name tp _) =
  env { envVtable = M.insert name tp $ envVtable env }

bindVars :: TypeEnv -> [Ident Type] -> TypeEnv
bindVars = foldl bindVar

binding :: [Ident Type] -> TypeM a -> TypeM a
binding bnds = local (`bindVars` bnds)

lookupVar :: String -> SrcLoc -> TypeM Type
lookupVar name pos = do
  bnd <- asks $ M.lookup name . envVtable
  case bnd of Nothing   -> bad $ UnknownVariableError name pos
              Just bnd' -> return bnd'

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

-- | @require ts (t, e)@ causes a 'TypeError' if @t@ does not unify
-- with one of the types in @ts@.  Otherwise, simply returns @(t, e)@.
-- This function is very useful in 'checkExp'.
require :: [Type] -> Exp Type -> TypeM (Exp Type)
require [] e = bad $ TypeError (srclocOf e) "Expression cannot have any type (probably a bug in the type checker)."
require ts e
  | any (expType e `similarTo`) ts = return e
  | otherwise =
    bad $ TypeError (srclocOf e) $ "Expression type must be one of " ++
          intercalate ", " (map ppType ts) ++ ", but is " ++ ppType (expType e) ++ "."

elemType :: Type -> TypeM Type
elemType (Array t _ _ _) = return t
elemType t = bad $ TypeError (srclocOf t) $ "Type of expression is not array, but " ++ ppType t ++ "."

uniqueness :: Type -> Uniqueness
uniqueness (Array _ _ u _) = u
uniqueness (Tuple _ u _) = u
uniqueness _ = Nonunique

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
  if expType body' `subtypeOf` rettype' then
    return (fname, rettype', args, body', pos)
  else bad $ ReturnTypeError pos fname rettype' $ expType body'
  where checkArgs = foldM expand [] args
        expand args' ident@(Ident pname _ _)
          | Just _ <- find ((==identName ident) . identName) args' =
            bad $ DupParamError fname pname pos
          | otherwise =
            return $ ident : args'

checkExp :: TypeBox tf => Exp tf -> TypeM (Exp Type)
checkExp (Literal val) =
  Literal <$> checkLiteral val
checkExp (TupLit es pos) = do
  es' <- mapM checkExp es
  let res = TupLit es' pos
  return $ fromMaybe res (Literal <$> expToValue res)
checkExp (ArrayLit es t pos) = do
  es' <- mapM checkExp es
  -- Find the unified type of all subexpression types.
  et <- case map expType es' of
          [] -> bad $ TypeError pos "Empty array literal"
          e:ets' -> foldM unifyKnownTypes e ets'
  -- Unify that type with the one given for the array literal.
  t' <- t `unifyWithKnown` et
  let res = ArrayLit es' t' pos
  return $ fromMaybe res (Literal <$> expToValue res)
checkExp (BinOp op e1 e2 t pos) = checkBinOp op e1 e2 t pos
checkExp (And e1 e2 pos) = do
  e1' <- require [Bool pos] =<< checkExp e1
  e2' <- require [Bool pos] =<< checkExp e2
  return $ And e1' e2' pos
checkExp (Or e1 e2 pos) = do
  e1' <- require [Bool pos] =<< checkExp e1
  e2' <- require [Bool pos] =<< checkExp e2
  return $ Or e1' e2' pos
checkExp (Not e pos) = do
  e' <- require [Bool pos] =<< checkExp e
  return $ Not e' pos
checkExp (Negate e t pos) = do
  e' <- require [Int pos, Real pos] =<< checkExp e
  t' <- t `unifyWithKnown` expType e'
  return $ Negate e' t' pos
checkExp (If e1 e2 e3 t pos) = do
  e1' <- require [Bool pos] =<< checkExp e1
  e2' <- checkExp e2
  e3' <- checkExp e3
  bt <- unifyWithKnown t =<< unifyKnownTypes (expType e2') (expType e3')
  return $ If e1' e2' e3' bt pos
checkExp (Var ident) = do
  ident' <- checkIdent ident
  return $ Var ident'
checkExp (Apply "trace" args t pos) =
  case args of
    [e] -> do
      e' <- checkExp e
      t' <- t `unifyWithKnown` expType e'
      return $ Apply "trace" [e'] t' pos
    _ -> bad $ TypeError pos "Trace function takes a single parameter"
checkExp (Apply "assertZip" args t pos) = do
  args' <- mapM checkExp args
  let argtps = map expType args'
  _ <- mapM soac2ElemType argtps
  t' <- t `unifyWithKnown` (Bool pos) --(Tuple argtps Unique pos)
  return $ Apply "assertZip" args' t' pos
checkExp (Apply fname args t pos) = do
  bnd <- asks $ M.lookup fname . envFtable
  case bnd of
    Nothing -> bad $ UnknownFunctionError fname pos
    Just (rettype, paramtypes) -> do
      rettype' <- t `unifyWithKnown` rettype
      args' <- mapM checkExp args
      let wrong = bad $ ParameterMismatch (Just fname) pos (Right paramtypes) (map expType args')
      if validApply paramtypes (map expType args') then
          return $ Apply fname args' rettype' pos
      else wrong
checkExp (LetPat pat e body pos) = do
  e' <- checkExp e
  (bnds, pat') <- checkPattern pat (expType e')
  binding bnds $ do
    body' <- checkExp body
    return $ LetPat pat' e' body' pos
checkExp (LetWith (Ident dest destt destpos) src idxes ve body pos) = do
  src' <- checkIdent src
  destt' <- destt `unifyWithKnown` identType src'
  let dest' = Ident dest destt' destpos
  case peelArray (length idxes) (identType src') of
    Nothing -> bad $ TypeError pos $ show (length idxes) ++ " indices given, but type of expression at " ++ locStr (srclocOf src) ++ " has " ++ show (arrayDims $ identType src') ++ " dimensions."
    Just elemt -> do
      idxes' <- mapM (require [Int pos] <=< checkExp) idxes
      ve' <- require [elemt] =<< checkExp ve
      body' <- local (`bindVar` dest') $ checkExp body
      return $ LetWith dest' src' idxes' ve' body' pos
checkExp (Index ident idxes intype restype pos) = do
  ident' <- checkIdent ident
  vt <- lookupVar (identName ident') pos
  when (arrayDims vt < length idxes) $
    bad $ TypeError pos $ show (length idxes) ++ " indices given, but type of variable " ++ identName ident' ++ " has " ++ show (arrayDims vt) ++ " dimensions."
  vet <- elemType vt
  intype' <- intype `unifyWithKnown` vet
  restype' <- restype `unifyWithKnown` stripArray (length idxes) vt
  idxes' <- mapM (require [Int pos] <=< checkExp) idxes
  return $ Index ident' idxes' intype' restype' pos
checkExp (Iota e pos) = do
  e' <- require [Int pos] =<< checkExp e
  return $ Iota e' pos
checkExp (Size e pos) = do
  e' <- checkExp e
  case expType e' of
    Array {} -> return $ Size e' pos
    _        -> bad $ TypeError pos "Argument to size must be array."
checkExp (Replicate countexp valexp pos) = do
  countexp' <- require [Int pos] =<< checkExp countexp
  valexp' <- checkExp valexp
  return $ Replicate countexp' valexp' pos
checkExp (Reshape shapeexps arrexp pos) = do
  shapeexps' <- mapM (require [Int pos] <=< checkExp) shapeexps
  arrexp' <- checkExp arrexp
  return (Reshape shapeexps' arrexp' pos)
checkExp (Transpose arrexp intype outtype pos) = do
  arrexp' <- checkExp arrexp
  when (arrayDims (expType arrexp') < 2) $
    bad $ TypeError pos "Argument to transpose does not have two dimensions."
  intype' <- intype `unifyWithKnown` expType arrexp'
  outtype' <- outtype `unifyWithKnown` intype'
  return $ Transpose arrexp' intype' outtype' pos
checkExp (Map fun arrexp intype outtype pos) = do
  arrexp' <- checkExp arrexp
  case expType arrexp' of
    Array et _ _ _ -> do
      fun' <- checkLambda fun [et]
      intype' <- intype `unifyWithKnown` et
      outtype' <- outtype `unifyWithKnown` lambdaType fun'
      return (Map fun' arrexp' intype' outtype' pos)
    _       -> bad $ TypeError (srclocOf arrexp) "Mapee expression does not return an array."
checkExp (Reduce fun startexp arrexp intype pos) = do
  startexp' <- checkExp startexp
  arrexp' <- checkExp arrexp
  case expType arrexp' of
    Array inelemt _ _ _ -> do
      inelemt' <- intype `unifyWithKnown` inelemt
      fun' <- checkLambda fun [expType startexp', inelemt']
      when (expType startexp' /= lambdaType fun') $
        bad $ TypeError pos $ "Accumulator is of type " ++ ppType (expType startexp') ++ ", but reduce function returns type " ++ ppType (lambdaType fun') ++ "."
      return $ Reduce fun' startexp' arrexp' inelemt' pos
    _ -> bad $ TypeError (srclocOf arrexp) "Type of expression is not an array"
checkExp (Zip arrexps pos) = do
  arrexps' <- mapM (checkExp . fst) arrexps
  inelemts <- mapM (elemType . expType) arrexps'
  inelemts' <- zipWithM unifyWithKnown (map snd arrexps) inelemts
  return $ Zip (zip arrexps' inelemts') pos
checkExp (Unzip e _ pos) = do
  e' <- checkExp e
  case expType e' of
    Array (Tuple ts _ _) _ _ _ -> return $ Unzip e' ts pos
    et -> bad $ TypeError pos $ "Argument to unzip is not an array of tuples, but " ++ ppType et ++ "."
checkExp (Scan fun startexp arrexp intype pos) = do
  startexp' <- checkExp startexp
  arrexp' <- checkExp arrexp
  case expType arrexp' of
    Array inelemt _ _ _ -> do
      intype' <- intype `unifyWithKnown` inelemt
      fun' <- checkLambda fun [intype', intype']
      when (expType startexp' /= lambdaType fun') $
        bad $ TypeError pos $ "Initial value is of type " ++ ppType (expType startexp') ++ ", but scan function returns type " ++ ppType (lambdaType fun') ++ "."
      when (intype' /= lambdaType fun') $
        bad $ TypeError pos $ "Array element value is of type " ++ ppType intype' ++ ", but scan function returns type " ++ ppType (lambdaType fun') ++ "."
      return $ Scan fun' startexp' arrexp' intype' pos
    _ -> bad $ TypeError (srclocOf arrexp) "Type of expression is not an array."
checkExp (Filter fun arrexp eltype pos) = do
  arrexp' <- checkExp arrexp
  inelemt <- elemType $ expType arrexp'
  eltype' <- eltype `unifyWithKnown` inelemt
  fun' <- checkLambda fun [inelemt]
  when (lambdaType fun' /= Bool pos) $
    bad $ TypeError pos "Filter function does not return bool."
  return $ Filter fun' arrexp' eltype' pos
checkExp (Mapall fun arrexp intype outtype pos) = do
  arrexp' <- checkExp arrexp
  intype' <- intype `unifyWithKnown` baseType (expType arrexp')
  fun' <- checkLambda fun [baseType intype']
  outtype' <- outtype `unifyWithKnown` lambdaType fun'
  return $ Mapall fun' arrexp' intype' outtype' pos
checkExp (Redomap redfun mapfun accexp arrexp intype outtype pos) = do
  accexp' <- checkExp accexp
  arrexp' <- checkExp arrexp
  et <- elemType $ expType arrexp'
  mapfun' <- checkLambda mapfun [et]
  redfun' <- checkLambda redfun [expType accexp', lambdaType mapfun']
  _ <- unifyKnownTypes (lambdaType redfun') (expType accexp')
  intype' <- intype `unifyWithKnown` et
  outtype' <- outtype `unifyWithKnown` lambdaType redfun'
  return $ Redomap redfun' mapfun' accexp' arrexp' intype' outtype' pos
checkExp (Split splitexp arrexp intype pos) = do
  splitexp' <- require [Int pos] =<< checkExp splitexp
  arrexp' <- checkExp arrexp
  et <- elemType $ expType arrexp'
  intype' <- intype `unifyWithKnown` et
  return $ Split splitexp' arrexp' intype' pos
checkExp (Concat arr1exp arr2exp intype pos) = do
  arr1exp' <- checkExp arr1exp
  arr2exp' <- require [expType arr1exp'] =<< checkExp arr2exp
  et <- elemType $ expType arr2exp'
  intype' <- intype `unifyWithKnown` et
  return $ Concat arr1exp' arr2exp' intype' pos
checkExp (Copy e pos) = do
  e' <- checkExp e
  return $ Copy e' pos
checkExp (DoLoop mergepat mergeexp (Ident loopvar _ _) boundexp loopbody letbody pos) = do
  mergeexp' <- checkExp mergeexp
  let mergetype = expType mergeexp'
  (bnds, mergepat') <- checkPattern mergepat mergetype
  boundexp' <- require [Int pos] =<< checkExp boundexp
  loopbody' <- binding (bnds++[Ident loopvar (Int pos) pos]) $
                 require [mergetype] =<< checkExp loopbody
  binding bnds $ do
    letbody' <- checkExp letbody
    return $ DoLoop mergepat' mergeexp' (Ident loopvar (Int pos) pos) boundexp' loopbody' letbody' pos

----------------------------------------------
---- BEGIN Cosmin added SOAC2 combinators ----
----------------------------------------------
checkExp (Map2 fun arrexp intype outtype pos) = do
  arrexp' <- mapM checkExp arrexp
  ineltps <- mapM (soac2ElemType . expType) arrexp'
  let ineltp = if length ineltps == 1 then head ineltps
               else Tuple ineltps Unique pos
  fun'    <- checkLambda fun [ineltp]
  intype' <- intype `unifyWithKnown` ineltp
  outtype'<- outtype `unifyWithKnown` lambdaType fun'
  return $ Map2 fun' arrexp' intype' outtype' pos

checkExp (Reduce2 fun startexp arrexp intype pos) = do
  startexp' <- checkExp startexp
  arrexp'   <- mapM checkExp arrexp
  ineltps   <- mapM (soac2ElemType . expType) arrexp'
  let ineltp = if length ineltps == 1 then head ineltps
               else Tuple ineltps Unique pos
  intype' <- intype `unifyWithKnown` ineltp
  fun'    <- checkLambda fun [expType startexp', intype']
  when (expType startexp' /= lambdaType fun') $
        bad $ TypeError pos $ "Accumulator is of type " ++ ppType (expType startexp') ++ 
                              ", but reduce function returns type " ++ ppType (lambdaType fun') ++ "."
  return $ Reduce2 fun' startexp' arrexp' intype' pos

checkExp (Scan2 fun startexp arrexp intype pos) = do
  startexp' <- checkExp startexp
  arrexp'   <- mapM checkExp arrexp

  --inelemt   <- soac2ElemType $ expType arrexp'
  ineltps   <- mapM (soac2ElemType . expType) arrexp'
  let inelemt = if length ineltps == 1 then head ineltps
                else Tuple ineltps Unique pos
  intype'   <- intype `unifyWithKnown` inelemt
  fun'      <- checkLambda fun [intype', intype']
  when (expType startexp' /= lambdaType fun') $
    bad $ TypeError pos $ "Initial value is of type " ++ ppType (expType startexp') ++ 
                          ", but scan function returns type " ++ ppType (lambdaType fun') ++ "."
  when (intype' /= lambdaType fun') $
    bad $ TypeError pos $ "Array element value is of type " ++ ppType intype' ++ 
                          ", but scan function returns type " ++ ppType (lambdaType fun') ++ "."
  return $ Scan2 fun' startexp' arrexp' intype' pos

checkExp (Filter2 fun arrexp eltype pos) = do
  arrexp' <- mapM checkExp arrexp
  --inelemt <- soac2ElemType $ expType arrexp'
  ineltps   <- mapM (soac2ElemType . expType) arrexp'
  let inelemt = if length ineltps == 1 then head ineltps
                else Tuple ineltps Unique pos
  eltype' <- eltype `unifyWithKnown` inelemt
  fun' <- checkLambda fun [inelemt]
  when (lambdaType fun' /= Bool pos) $
    bad $ TypeError pos "Filter function does not return bool."
  return $ Filter2 fun' arrexp' eltype' pos

checkExp (Mapall2 fun arrexp intype outtype pos) = do
  arrexp' <- mapM checkExp arrexp
  let arrtps = map expType arrexp'

  _ <- mapM soac2ElemType arrtps
  let mindim = foldl (\x y -> min x y) 
                     (arrayDims (head arrtps)) 
                     (map arrayDims (tail arrtps))
  let ineltps= map (\x -> stripArray mindim x) arrtps
  let ineltp = if length ineltps == 1 then head ineltps
               else Tuple ineltps Unique pos 

  intype' <- intype `unifyWithKnown` ineltp
  fun' <- checkLambda fun [intype']
  outtype' <- outtype `unifyWithKnown` lambdaType fun'
  return $ Mapall2 fun' arrexp' intype' outtype' pos


checkExp (Redomap2 redfun mapfun accexp arrexp intype outtype pos) = do
  accexp' <- checkExp accexp
  arrexp' <- mapM checkExp arrexp
  ets <- mapM (soac2ElemType . expType) arrexp'
  let et = if length ets == 1 then head ets
           else Tuple ets Unique pos
  mapfun' <- checkLambda mapfun [et]
  redfun' <- checkLambda redfun [expType accexp', lambdaType mapfun']
  _ <- unifyKnownTypes (lambdaType redfun') (expType accexp')
  intype' <- intype `unifyWithKnown` et
  outtype' <- outtype `unifyWithKnown` lambdaType redfun'
  return $ Redomap2 redfun' mapfun' accexp' arrexp' intype' outtype' pos

---------------------
--- SOAC2 HELPERS ---
---------------------
soac2ElemType :: Type -> TypeM Type
soac2ElemType tp@(Array _ _ _ _) = 
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
  vt <- case map valueType vals' of
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
  _ <- unifyKnownTypes (expType e1') (expType e2')
  t' <- t `unifyWithKnown` Bool pos
  return $ BinOp op e1' e2' t' pos

checkPolyBinOp :: TypeBox ty => BinOp -> [Type] -> Exp ty -> Exp ty -> ty -> SrcLoc
               -> TypeM (Exp Type)
checkPolyBinOp op tl e1 e2 t pos = do
  e1' <- require tl =<< checkExp e1
  e2' <- require tl =<< checkExp e2
  t' <- unifyKnownTypes (expType e1') (expType e2')
  t'' <- t `unifyWithKnown` t'
  return $ BinOp op e1' e2' t'' pos

checkPattern :: TypeBox tf =>
                TupIdent tf -> Type -> TypeM ([Ident Type], TupIdent Type)
checkPattern pat vt = do
  (pat', bnds) <- runStateT (checkPattern' pat vt) []
  return (bnds, pat')
  where checkPattern' (Id (Ident name namet pos)) t = do
          add name t pos
          t' <- lift $ namet `unifyWithKnown` t
          return $ Id $ Ident name t' pos
        checkPattern' (TupId pats pos) (Tuple ts _ _)
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
  let curryargexpts = map expType curryargexps'
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
              checkApply Nothing  paramtypes (curryargexpts++args) pos
              return $ CurryFun fname curryargexps' rettype' pos

checkPolyLambdaOp :: TypeBox ty => BinOp -> [Exp ty] -> ty -> [Type] -> SrcLoc
                  -> TypeM (Lambda Type)
checkPolyLambdaOp op curryargexps rettype args pos = do
  curryargexpts <- map expType <$> mapM checkExp curryargexps
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
  checkLambda (AnonymFun params body (expType body) pos) args
  where fname = "op" ++ ppBinOp op
