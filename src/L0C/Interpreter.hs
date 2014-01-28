{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A nonoptimising interpreter for L0.  It makes no assumptions of
-- the form of the input program, and in particular permits shadowing.
-- This interpreter should be considered the primary benchmark for
-- judging the correctness of a program, but note that it is not by
-- itself sufficient.  The interpreter does not perform in-place
-- updates like the native code generator, and bugs related to
-- uniqueness will therefore not be detected.  Of course, the type
-- checker should catch such error.
--
-- To run an L0 program, you would normally run the interpreter as
-- @'runFun' 'defaultEntryPoint' args prog@.
module L0C.Interpreter
  ( runFun
  , runFunNoTrace
  , Trace
  , InterpreterError(..) )
where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Error

import Data.Array
import Data.Bits
import Data.List
import Data.Loc
import qualified Data.HashMap.Strict as HM

import L0C.L0

-- | An error happened during execution, and this is why.
data InterpreterError = MissingEntryPoint Name
                      -- ^ The specified start function does not exist.
                      | InvalidFunctionArguments Name (Maybe [DeclType]) [Type]
                      -- ^ The arguments given to a function were mistyped.
                      | IndexOutOfBounds SrcLoc Int Int
                      -- ^ First @Int@ is array size, second is attempted index.
                      | NegativeIota SrcLoc Int
                      -- ^ Called @iota(n)@ where @n@ was negative.
                      | NegativeReplicate SrcLoc Int
                      -- ^ Called @replicate(n, x)@ where @n@ was negative.
                      | InvalidArrayShape SrcLoc [Int] [Int]
                      -- ^ First @Int@ is old shape, second is attempted new shape.
                      | ZipError SrcLoc [Int]
                      -- ^ The arguments to @zip@ were of different lengths.
                      | AssertFailed SrcLoc
                      -- ^ Assertion failed at this location.
                      | TypeError SrcLoc String
                      -- ^ Some value was of an unexpected type.

instance Show InterpreterError where
  show (MissingEntryPoint fname) =
    "Program entry point '" ++ nameToString fname ++ "' not defined."
  show (InvalidFunctionArguments fname Nothing got) =
    "Function '" ++ nameToString fname ++ "' did not expect argument(s) of type " ++
    intercalate ", " (map ppType got) ++ "."
  show (InvalidFunctionArguments fname (Just expected) got) =
    "Function '" ++ nameToString fname ++ "' expected argument(s) of type " ++
    intercalate ", " (map ppType expected) ++
    " but got argument(s) of type " ++
    intercalate ", " (map ppType got) ++ "."
  show (IndexOutOfBounds pos arrsz i) =
    "Array index " ++ show i ++ " out of bounds of array size " ++ show arrsz ++ " at " ++ locStr pos ++ "."
  show (NegativeIota pos n) =
    "Argument " ++ show n ++ " to iota at " ++ locStr pos ++ " is negative."
  show (NegativeReplicate pos n) =
    "Argument " ++ show n ++ " to replicate at " ++ locStr pos ++ " is negative."
  show (TypeError pos s) =
    "Type error at " ++ locStr pos ++ " in " ++ s ++ " during interpretation.  This implies a bug in the type checker."
  show (InvalidArrayShape pos shape newshape) =
    "Invalid array reshaping at " ++ locStr pos ++ ", from " ++ show shape ++ " to " ++ show newshape
  show (ZipError pos lengths) =
    "Array arguments to zip must have same length, but arguments at " ++
    locStr pos ++ " have lenghts " ++ intercalate ", " (map show lengths) ++ "."
  show (AssertFailed loc) =
    "Assertion failed at " ++ locStr loc ++ "."

instance Error InterpreterError where
  strMsg = TypeError noLoc

data L0Env = L0Env { envVtable  :: HM.HashMap VName Value
                   , envFtable  :: HM.HashMap Name ([Value] -> L0M Value)
                   }

-- | A list of places where @trace@ was called, alongside the
-- prettyprinted value that was passed to it.
type Trace = [(SrcLoc, String)]

newtype L0M a = L0M (ReaderT L0Env
                     (ErrorT InterpreterError
                      (Writer Trace)) a)
  deriving (MonadReader L0Env, MonadWriter Trace, Monad, Applicative, Functor)

runL0M :: L0M a -> L0Env -> (Either InterpreterError a, Trace)
runL0M (L0M m) env = runWriter $ runErrorT $ runReaderT m env

bad :: InterpreterError -> L0M a
bad = L0M . throwError

bindVar :: L0Env -> (Ident, Value) -> L0Env
bindVar env (Ident name _ _,val) =
  env { envVtable = HM.insert name val $ envVtable env }

bindVars :: L0Env -> [(Ident, Value)] -> L0Env
bindVars = foldl bindVar

binding :: [(Ident, Value)] -> L0M a -> L0M a
binding bnds = local (`bindVars` bnds)

lookupVar :: Ident -> L0M Value
lookupVar (Ident vname _ loc) = do
  val <- asks $ HM.lookup vname . envVtable
  case val of Just val' -> return val'
              Nothing   -> bad $ TypeError loc $ "lookupVar " ++ textual vname

lookupFun :: Name -> L0M ([Value] -> L0M Value)
lookupFun fname = do
  fun <- asks $ HM.lookup fname . envFtable
  case fun of Just fun' -> return fun'
              Nothing   -> bad $ TypeError noLoc $ "lookupFun " ++ textual fname

arrToList :: SrcLoc -> Value -> L0M [Value]
arrToList _ (ArrayVal l _) = return $ elems l
arrToList loc _ = bad $ TypeError loc "arrToList"

tupToList :: SrcLoc -> Value -> L0M [Value]
tupToList _ (TupVal l) = return l
tupToList loc _ = bad $ TypeError loc "tupToList"

untuple :: Value -> [Value]
untuple (TupVal vs) = vs
untuple v           = [v]

tuple :: [Value] -> Value
tuple [v] = v
tuple vs = TupVal vs

arrays :: Type -> [Value] -> Value
arrays (Elem (Tuple ts)) v =
  TupVal $ zipWith arrayVal (arrays' (map untuple v)) ts
  where arrays' = foldr (zipWith (:)) (replicate (length ts) [])
arrays rowtype vs = arrayVal vs rowtype

--------------------------------------------------
------- Interpreting an arbitrary function -------
--------------------------------------------------

-- |  @funFun name args prog@ invokes the @name@ function of program
-- @prog@, with the parameters bound in order to the values in @args@.
-- Returns either an error or the return value of @fun@.
-- Additionally, a list of all calls to the special built-in function
-- @trace@ is always returned.  This is useful for debugging.
--
-- Note that if 'prog' is not type-correct, you cannot be sure that
-- you'll get an error from the interpreter - it may just as well
-- silently return a wrong value.  You are, however, guaranteed that
-- the initial call to 'prog' is properly checked.
runFun :: Name -> [Value] -> Prog -> (Either InterpreterError Value, Trace)
runFun fname mainargs prog = do
  let ftable = foldl expand builtins $ progFunctions prog
      l0env = L0Env { envVtable = HM.empty
                    , envFtable = ftable
                    }
      runmain =
        case (funDecByName fname prog, HM.lookup fname ftable) of
          (Nothing, Nothing) -> bad $ MissingEntryPoint fname
          (Just (_,rettype,fparams,_,_), _)
            | map (toDecl . valueType) mainargs == map identType fparams ->
              evalExp (Apply fname [ (Literal arg noLoc,
                                      diet $ identType paramt) |
                                     (arg,paramt) <- zip mainargs fparams ]
                       (fromDecl rettype) noLoc)
            | otherwise ->
              bad $ InvalidFunctionArguments fname
                    (Just (map identType fparams))
                    (map (fromDecl . valueType) mainargs)
          (_ , Just fun) -> -- It's a builtin function, it'll
                            -- do its own error checking.
            fun mainargs
  runL0M runmain l0env
  where
    -- We assume that the program already passed the type checker, so
    -- we don't check for duplicate definitions.
    expand ftable (name,_,params,body,_) =
      let fun args = binding (zip (map fromParam params) args) $ evalExp body
      in HM.insert name fun ftable

-- | As 'runFun', but throws away the trace.
runFunNoTrace :: Name -> [Value] -> Prog -> Either InterpreterError Value
runFunNoTrace = ((.) . (.) . (.)) fst runFun -- I admit this is just for fun.

--------------------------------------------
--------------------------------------------
------------- BUILTIN FUNCTIONS ------------
--------------------------------------------
--------------------------------------------

builtins :: HM.HashMap Name ([Value] -> L0M Value)
builtins = HM.fromList $ map namify
           [("toReal", builtin "toReal")
           ,("trunc", builtin "trunc")
           ,("sqrt", builtin "sqrt")
           ,("log", builtin "log")
           ,("exp", builtin "exp")
           ,("op not", builtin "op not")
           ,("op ~", builtin "op ~")]
  where namify (k,v) = (nameFromString k, v)

builtin :: String -> [Value] -> L0M Value
builtin "toReal" [IntVal x] = return $ RealVal (fromIntegral x)
builtin "trunc" [RealVal x] = return $ IntVal (truncate x)
builtin "sqrt" [RealVal x] = return $ RealVal (sqrt x)
builtin "log" [RealVal x] = return $ RealVal (log x)
builtin "exp" [RealVal x] = return $ RealVal (exp x)
builtin "op not" [LogVal b] = return $ LogVal (not b)
builtin "op ~" [RealVal b] = return $ RealVal (-b)
builtin fname args =
  bad $ InvalidFunctionArguments (nameFromString fname) Nothing $
        map (fromDecl . valueType) args

--------------------------------------------
--------------------------------------------
--------------------------------------------
--------------------------------------------


evalExp :: Exp -> L0M Value

evalExp (Literal val _) = return val

evalExp (TupLit es _) =
  TupVal <$> mapM evalExp es

evalExp (ArrayLit es rt _) =
  arrayVal <$> mapM evalExp es <*> pure rt

evalExp (BinOp Plus e1 e2 (Elem Int) pos) = evalIntBinOp (+) e1 e2 pos
evalExp (BinOp Plus e1 e2 (Elem Real) pos) = evalRealBinOp (+) e1 e2 pos
evalExp (BinOp Minus e1 e2 (Elem Int) pos) = evalIntBinOp (-) e1 e2 pos
evalExp (BinOp Minus e1 e2 (Elem Real) pos) = evalRealBinOp (-) e1 e2 pos
evalExp (BinOp Pow e1 e2 (Elem Int) pos) = evalIntBinOp pow e1 e2 pos
  -- Haskell (^) cannot handle negative exponents, so check for that
  -- explicitly.
  where pow x y | y < 0, x == 0 = error "Negative exponential with zero base"
                | y < 0         = 1 `div` (x ^ (-y))
                | otherwise     = x ^ y
evalExp (BinOp Pow e1 e2 (Elem Real) pos) = evalRealBinOp (**) e1 e2 pos
evalExp (BinOp Times e1 e2 (Elem Int) pos) = evalIntBinOp (*) e1 e2 pos
evalExp (BinOp Times e1 e2 (Elem Real) pos) = evalRealBinOp (*) e1 e2 pos
evalExp (BinOp Divide e1 e2 (Elem Int) pos) = evalIntBinOp div e1 e2 pos
evalExp (BinOp Mod e1 e2 (Elem Int) pos) = evalIntBinOp mod e1 e2 pos
evalExp (BinOp Divide e1 e2 (Elem Real) pos) = evalRealBinOp (/) e1 e2 pos
evalExp (BinOp ShiftR e1 e2 _ pos) = evalIntBinOp shiftR e1 e2 pos
evalExp (BinOp ShiftL e1 e2 _ pos) = evalIntBinOp shiftL e1 e2 pos
evalExp (BinOp Band e1 e2 _ pos) = evalIntBinOp (.&.) e1 e2 pos
evalExp (BinOp Xor e1 e2 _ pos) = evalIntBinOp xor e1 e2 pos
evalExp (BinOp Bor e1 e2 _ pos) = evalIntBinOp (.|.) e1 e2 pos
evalExp (BinOp LogAnd e1 e2 _ pos) = evalBoolBinOp (&&) e1 e2 pos
evalExp (BinOp LogOr e1 e2 _ pos) = evalBoolBinOp (||) e1 e2 pos

evalExp (BinOp Equal e1 e2 _ _) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  return $ LogVal (v1==v2)

evalExp (BinOp Less e1 e2 _ _) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  return $ LogVal (v1<v2)

evalExp (BinOp Leq e1 e2 _ _) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  return $ LogVal (v1<=v2)

evalExp (BinOp _ _ _ _ pos) = bad $ TypeError pos "evalExp Binop"

evalExp (Not e pos) = do
  v <- evalExp e
  case v of LogVal b -> return $ LogVal (not b)
            _        -> bad $ TypeError pos "evalExp Not"

evalExp (Negate e _ pos) = do
  v <- evalExp e
  case v of IntVal x  -> return $ IntVal (-x)
            RealVal x -> return $ RealVal (-x)
            _         -> bad $ TypeError pos "evalExp Negate"

evalExp (If e1 e2 e3 _ pos) = do
  v <- evalExp e1
  case v of LogVal True  -> evalExp e2
            LogVal False -> evalExp e3
            _            -> bad $ TypeError pos "evalExp If"

evalExp (Var ident) =
  lookupVar ident

evalExp (Apply fname [(arg, _)] _ loc)
  | "trace" <- nameToString fname = do
  arg' <- evalExp arg
  tell [(loc, ppValue arg')]
  return arg'

evalExp (Apply fname args _ _) = do
  fun <- lookupFun fname
  args' <- mapM (evalExp . fst) args
  fun args'

evalExp (LetPat pat e body pos) = do
  v <- evalExp e
  case evalPattern pat v of
    Nothing   -> bad $ TypeError pos "evalExp Let pat"
    Just bnds -> local (`bindVars` bnds) $ evalExp body

evalExp (LetWith _ name src _ idxs ve body pos) = do
  v <- lookupVar src
  idxs' <- mapM evalExp idxs
  vev <- evalExp ve
  v' <- change v idxs' vev
  binding [(name, v')] $ evalExp body
  where change _ [] to = return to
        change (ArrayVal arr t) (IntVal i:rest) to
          | i >= 0 && i <= upper = do
            let x = arr ! i
            x' <- change x rest to
            return $ ArrayVal (arr // [(i, x')]) t
          | otherwise = bad $ IndexOutOfBounds pos (upper+1) i
          where upper = snd $ bounds arr
        change _ _ _ = bad $ TypeError pos "evalExp Let Id"

evalExp (Index _ ident _ idxs pos) = do
  v <- lookupVar ident
  idxs' <- mapM evalExp idxs
  foldM idx v idxs'
  where idx (ArrayVal arr _) (IntVal i)
          | i >= 0 && i <= upper = return $ arr ! i
          | otherwise             = bad $ IndexOutOfBounds pos (upper+1) i
          where upper = snd $ bounds arr
        idx _ _ = bad $ TypeError pos "evalExp Index"

evalExp (Iota e pos) = do
  v <- evalExp e
  case v of
    IntVal x
      | x >= 0    -> return $ arrayVal (map IntVal [0..x-1]) $ Elem Int
      | otherwise -> bad $ NegativeIota pos x
    _ -> bad $ TypeError pos "evalExp Iota"

evalExp (Size _ i e pos) = do
  v <- evalExp e
  case drop i $ arrayShape v of
    [] -> bad $ TypeError pos "evalExp Size"
    n:_ -> return $ IntVal n

evalExp (Replicate e1 e2 pos) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case v1 of
    IntVal x
      | x >= 0    -> return $ ArrayVal (listArray (0,x-1) $ repeat v2) $ valueType v2
      | otherwise -> bad $ NegativeReplicate pos x
    _   -> bad $ TypeError pos "evalExp Replicate"

evalExp re@(Reshape _ shapeexp arrexp pos) = do
  shape <- mapM (asInt <=< evalExp) shapeexp
  arr <- evalExp arrexp
  let rt = stripArray 1 $ typeOf re
      reshape (n:rest) vs
        | length vs `mod` n == 0 =
          arrayVal <$> mapM (reshape rest) (chunk (length vs `div` n) vs)
                   <*> pure rt
      reshape [] [v] = return v
      reshape _ _ = bad $ InvalidArrayShape pos (arrayShape arr) shape
  reshape shape $ flatten arr
  where flatten (ArrayVal arr _) = concatMap flatten $ elems arr
        flatten t = [t]
        chunk _ [] = []
        chunk i l = let (a,b) = splitAt i l
                    in a : chunk i b
        asInt (IntVal x) = return x
        asInt _ = bad $ TypeError pos "evalExp Reshape int"

evalExp (Transpose _ k n arrexp _) =
  transposeArray k n <$> evalExp arrexp

evalExp (Map fun e _ pos) = do
  vs <- arrToList pos =<< evalExp e
  vs' <- mapM (applyLambda fun . (:[])) vs
  return $ arrayVal vs' $ lambdaReturnType fun

evalExp (Reduce fun accexp arrexp _ loc) = do
  startacc <- evalExp accexp
  vs <- arrToList loc =<< evalExp arrexp
  let foldfun acc x = applyLambda fun [acc, x]
  foldM foldfun startacc vs

evalExp (Zip arrexps pos) = do
  arrs <- mapM ((arrToList pos <=< evalExp) . fst) arrexps
  let zipit ls
        | all null ls = return []
        | otherwise = case unzip <$> mapM split ls of
                        Just (hds, tls) -> do
                          ls' <- zipit tls
                          return $ TupVal hds : ls'
                        Nothing -> bad $ ZipError pos (map length arrs)
  arrayVal <$> zipit arrs <*> pure (Elem $ Tuple $ map snd arrexps)
  where split []     = Nothing
        split (x:xs) = Just (x, xs)

evalExp (Unzip e ts pos) = do
  arr <- mapM (tupToList pos) =<< arrToList pos =<< evalExp e
  return $ TupVal (zipWith arrayVal (transpose arr) ts)

-- scan * e {x1,..,xn} = {e*x1, e*x1*x2, ..., e*x1*x2*...*xn}
-- we can change this definition of scan if deemed not suitable
evalExp (Scan fun startexp arrexp _ pos) = do
  startval <- evalExp startexp
  vals <- arrToList pos =<< evalExp arrexp
  (acc, vals') <- foldM scanfun (startval, []) vals
  return $ arrayVal (reverse vals') $ valueType acc
    where scanfun (acc, l) x = do
            acc' <- applyLambda fun [acc, x]
            return (acc', acc' : l)

evalExp (Filter fun arrexp _ pos) = do
  vs <- filterM filt =<< arrToList pos =<< evalExp arrexp
  return $ arrayVal vs $ lambdaReturnType fun
  where filt x = do res <- applyLambda fun [x]
                    case res of (LogVal True) -> return True
                                _             -> return False

evalExp (Redomap redfun mapfun accexp arrexp _ pos) = do
  startacc <- evalExp accexp
  vs <- arrToList pos =<< evalExp arrexp
  vs' <- mapM (applyLambda mapfun . (:[])) vs
  let foldfun acc x = applyLambda redfun [acc, x]
  foldM foldfun startacc vs'

evalExp (Split _ splitexp arrexp intype pos) = do
  split <- evalExp splitexp
  vs <- arrToList pos =<< evalExp arrexp
  case split of
    IntVal i
      | i <= length vs ->
        let (bef,aft) = splitAt i vs
        in return $ TupVal [arrayVal bef intype, arrayVal aft intype]
      | otherwise        -> bad $ IndexOutOfBounds pos (length vs) i
    _ -> bad $ TypeError pos "evalExp Split"

evalExp (Concat _ arr1exp arr2exp pos) = do
  elems1 <- arrToList pos =<< evalExp arr1exp
  elems2 <- arrToList pos =<< evalExp arr2exp
  return $ arrayVal (elems1 ++ elems2) $ stripArray 1 $ typeOf arr1exp

evalExp (Copy e _) = evalExp e

evalExp (Assert e loc) = do
  v <- evalExp e
  case v of LogVal True -> return Checked
            _ -> bad $ AssertFailed loc

evalExp (Conjoin _ _) = return Checked

evalExp (DoLoop mergepat mergeexp loopvar boundexp loopbody letbody pos) = do
  bound <- evalExp boundexp
  mergestart <- evalExp mergeexp
  case bound of
    IntVal n -> do loopresult <- foldM iteration mergestart [0..n-1]
                   evalExp $ LetPat mergepat (Literal loopresult pos) letbody pos
    _ -> bad $ TypeError pos "evalExp DoLoop"
  where iteration mergeval i =
          binding [(loopvar, IntVal i)] $
            evalExp $ LetPat mergepat (Literal mergeval pos) loopbody pos

evalExp (MapT _ fun arrexps loc) = do
  vss <- mapM (arrToList loc <=< evalExp) arrexps
  vs' <- mapM (applyTupleLambda fun) $ transpose vss
  return $ arrays (fromDecl $ Elem $ Tuple ret) vs'
  where TupleLambda _ _ ret _ = fun

evalExp (ReduceT _ fun inputs loc) = do
  let (accexps, arrexps) = unzip inputs
  startaccs <- mapM evalExp accexps
  vss <- mapM (arrToList loc <=< evalExp) arrexps
  let foldfun acc x = applyTupleLambda fun $ untuple acc ++ x
  foldM foldfun (tuple startaccs) (transpose vss)

evalExp (ScanT _ fun inputs loc) = do
  let (accexps, arrexps) = unzip inputs
  startvals <- mapM evalExp accexps
  vss <- mapM (arrToList loc <=< evalExp) arrexps
  (acc, vals') <- foldM scanfun (tuple startvals, []) $ transpose vss
  return $ arrays (fromDecl $ valueType acc) $ reverse vals'
    where scanfun (acc, l) x = do
            acc' <- applyTupleLambda fun $ untuple acc ++ x
            return (acc', acc' : l)

evalExp e@(FilterT _ fun arrexp loc) = do
  vss <- mapM (arrToList loc <=< evalExp) arrexp
  vss' <- filterM filt $ transpose vss
  return $ arrays (typeOf e) $ map tuple vss'
  where filt x = do res <- applyTupleLambda fun x
                    case res of (TupVal [LogVal True]) -> return True
                                _                      -> return False

evalExp (RedomapT _ _ innerfun accexp arrexps loc) = do
  startaccs <- mapM evalExp accexp
  vss <- mapM (arrToList loc <=< evalExp) arrexps
  let foldfun acc x = applyTupleLambda innerfun $ untuple acc ++ x
  foldM foldfun (tuple startaccs) $ transpose vss

evalIntBinOp :: (Int -> Int -> Int) -> Exp -> Exp -> SrcLoc -> L0M Value
evalIntBinOp op e1 e2 loc = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case (v1, v2) of
    (IntVal x, IntVal y) -> return $ IntVal (op x y)
    _                    -> bad $ TypeError loc "evalIntBinOp"

evalRealBinOp :: (Double -> Double -> Double) -> Exp -> Exp -> SrcLoc -> L0M Value
evalRealBinOp op e1 e2 loc = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case (v1, v2) of
    (RealVal x, RealVal y) -> return $ RealVal (op x y)
    _                      -> bad $ TypeError loc $ "evalRealBinOp " ++ ppValue v1 ++ " " ++ ppValue v2

evalBoolBinOp :: (Bool -> Bool -> Bool) -> Exp -> Exp -> SrcLoc -> L0M Value
evalBoolBinOp op e1 e2 loc = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case (v1, v2) of
    (LogVal x, LogVal y) -> return $ LogVal (op x y)
    _                    -> bad $ TypeError loc $ "evalBoolBinOp " ++ ppValue v1 ++ " " ++ ppValue v2

evalPattern :: TupIdent -> Value -> Maybe [(Ident, Value)]
evalPattern (Id ident) v = Just [(ident, v)]
evalPattern (TupId pats _) (TupVal vs)
  | length pats == length vs =
    concat <$> zipWithM evalPattern pats vs
evalPattern (Wildcard _ _) _ = Just []
evalPattern _ _ = Nothing

applyLambda :: Lambda -> [Value] -> L0M Value
applyLambda (AnonymFun params body _ _) args =
  binding (zip (map fromParam params) args) $ evalExp body
applyLambda (CurryFun name curryargs _ _) args = do
  curryargs' <- mapM evalExp curryargs
  fun <- lookupFun name
  fun $ curryargs' ++ args

applyTupleLambda :: TupleLambda -> [Value] -> L0M Value
applyTupleLambda = applyLambda . tupleLambdaToLambda
