{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A nonoptimising interpreter for Futhark.  It makes no assumptions of
-- the form of the input program, and in particular permits shadowing.
-- This interpreter should be considered the primary benchmark for
-- judging the correctness of a program, but note that it is not by
-- itself sufficient.  The interpreter does not perform in-place
-- updates like the native code generator, and bugs related to
-- uniqueness will therefore not be detected.  Of course, the type
-- checker should catch such error.
--
-- To run an Futhark program, you would normally run the interpreter as
-- @'runFun' 'defaultEntryPoint' args prog@.
module Futhark.Interpreter
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

import Futhark.Representation.AST

-- | An error happened during execution, and this is why.
data InterpreterError = MissingEntryPoint Name
                      -- ^ The specified start function does not exist.
                      | InvalidFunctionArguments Name (Maybe [DeclType]) [DeclType]
                      -- ^ The arguments given to a function were mistyped.
                      | IndexOutOfBounds SrcLoc String Int Int
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
    intercalate ", " (map pretty got) ++ "."
  show (InvalidFunctionArguments fname (Just expected) got) =
    "Function '" ++ nameToString fname ++ "' expected argument(s) of type " ++
    intercalate ", " (map pretty expected) ++
    " but got argument(s) of type " ++
    intercalate ", " (map pretty got) ++ "."
  show (IndexOutOfBounds pos var arrsz i) =
    "Array index " ++ show i ++ " out of bounds in array '" ++
    var ++ "', of size " ++ show arrsz ++ " at " ++ locStr pos ++ "."
  show (NegativeIota pos n) =
    "Argument " ++ show n ++ " to iota at " ++ locStr pos ++ " is negative."
  show (NegativeReplicate pos n) =
    "Argument " ++ show n ++ " to replicate at " ++ locStr pos ++ " is negative."
  show (TypeError pos s) =
    "Type error during interpretation at " ++ locStr pos ++ " in " ++ s
  show (InvalidArrayShape pos shape newshape) =
    "Invalid array reshaping at " ++ locStr pos ++ ", from " ++ show shape ++ " to " ++ show newshape
  show (ZipError pos lengths) =
    "Array arguments to zip must have same length, but arguments at " ++
    locStr pos ++ " have lenghts " ++ intercalate ", " (map show lengths) ++ "."
  show (AssertFailed loc) =
    "Assertion failed at " ++ locStr loc ++ "."

instance Error InterpreterError where
  strMsg = TypeError noLoc

data FutharkEnv = FutharkEnv { envVtable :: HM.HashMap VName Value
                             , envFtable :: HM.HashMap Name ([Value] -> FutharkM [Value])
                             }

-- | A list of places where @trace@ was called, alongside the
-- prettyprinted value that was passed to it.
type Trace = [(SrcLoc, String)]

newtype FutharkM a = FutharkM (ReaderT FutharkEnv
                     (ErrorT InterpreterError
                      (Writer Trace)) a)
  deriving (MonadReader FutharkEnv, MonadWriter Trace, Monad, Applicative, Functor)

runFutharkM :: FutharkM a -> FutharkEnv -> (Either InterpreterError a, Trace)
runFutharkM (FutharkM m) env = runWriter $ runErrorT $ runReaderT m env

bad :: InterpreterError -> FutharkM a
bad = FutharkM . throwError

bindVar :: FutharkEnv -> (Ident, Value) -> FutharkEnv
bindVar env (Ident name _ _,val) =
  env { envVtable = HM.insert name val $ envVtable env }

bindVars :: FutharkEnv -> [(Ident, Value)] -> FutharkEnv
bindVars = foldl bindVar

binding :: [(Ident, Value)] -> FutharkM a -> FutharkM a
binding bnds m = local (`bindVars` bnds) $ checkPatSizes bnds >> m

lookupVar :: Ident -> FutharkM Value
lookupVar (Ident vname _ loc) = do
  val <- asks $ HM.lookup vname . envVtable
  case val of Just val' -> return val'
              Nothing   -> bad $ TypeError loc $ "lookupVar " ++ textual vname

lookupFun :: Name -> FutharkM ([Value] -> FutharkM [Value])
lookupFun fname = do
  fun <- asks $ HM.lookup fname . envFtable
  case fun of Just fun' -> return fun'
              Nothing   -> bad $ TypeError noLoc $ "lookupFun " ++ textual fname

arrToList :: SrcLoc -> Value -> FutharkM [Value]
arrToList _ (ArrayVal l _) = return $ elems l
arrToList loc _ = bad $ TypeError loc "arrToList"

arrays :: ArrayShape shape => [TypeBase shape] -> [[Value]] -> [Value]
arrays [rowtype] vs = [arrayVal (concat vs) rowtype]
arrays ts v =
  zipWith arrayVal (arrays' v) ts
  where arrays' = foldr (zipWith (:)) (replicate (length ts) [])

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
runFun :: Name -> [Value] -> Prog lore
       -> (Either InterpreterError [Value], Trace)
runFun fname mainargs prog = do
  let ftable = foldl expand builtins $ progFunctions prog
      futharkenv = FutharkEnv { envVtable = HM.empty
                    , envFtable = ftable
                    }
      argtypes = map ((`setUniqueness` Unique) . valueType) mainargs
      runmain =
        case (funDecByName fname prog, HM.lookup fname ftable) of
          (Nothing, Nothing) -> bad $ MissingEntryPoint fname
          (Just (FunDec _ _ fparams _ _), _)
            | length argtypes == length fparams &&
              and (zipWith subtypeOf argtypes $ map bindeeType fparams) ->
              evalFuncall fname [ Constant v noLoc | v <- mainargs ]
            | otherwise ->
              bad $ InvalidFunctionArguments fname
                    (Just (map (toDecl . bindeeType) fparams))
                    (map toDecl argtypes)
          (_ , Just fun) -> -- It's a builtin function, it'll
                            -- do its own error checking.
            fun mainargs
  runFutharkM runmain futharkenv
  where
    -- We assume that the program already passed the type checker, so
    -- we don't check for duplicate definitions.
    expand ftable (FunDec name _ params body _) =
      let fun args = binding (zip (map bindeeIdent params) args) $ evalBody body
      in HM.insert name fun ftable

-- | As 'runFun', but throws away the trace.
runFunNoTrace :: Name -> [Value] -> Prog lore -> Either InterpreterError [Value]
runFunNoTrace = ((.) . (.) . (.)) fst runFun -- I admit this is just for fun.

--------------------------------------------
--------------------------------------------
------------- BUILTIN FUNCTIONS ------------
--------------------------------------------
--------------------------------------------

builtins :: HM.HashMap Name ([Value] -> FutharkM [Value])
builtins = HM.fromList $ map namify
           [("toReal", builtin "toReal")
           ,("trunc", builtin "trunc")
           ,("sqrt", builtin "sqrt")
           ,("log", builtin "log")
           ,("exp", builtin "exp")
           ,("op not", builtin "op not")
           ,("op ~", builtin "op ~")]
  where namify (k,v) = (nameFromString k, v)

builtin :: String -> [Value] -> FutharkM [Value]
builtin "toReal" [BasicVal (IntVal x)] =
  return [BasicVal $ RealVal (fromIntegral x)]
builtin "trunc" [BasicVal (RealVal x)] =
  return [BasicVal $ IntVal (truncate x)]
builtin "sqrt" [BasicVal (RealVal x)] =
  return [BasicVal $ RealVal (sqrt x)]
builtin "log" [BasicVal (RealVal x)] =
  return [BasicVal $ RealVal (log x)]
builtin "exp" [BasicVal (RealVal x)] =
  return [BasicVal $ RealVal (exp x)]
builtin "op not" [BasicVal (LogVal b)] =
  return [BasicVal $ LogVal (not b)]
builtin "op ~" [BasicVal (RealVal b)] =
  return [BasicVal $ RealVal (-b)]
builtin fname args =
  bad $ InvalidFunctionArguments (nameFromString fname) Nothing $
        map (toDecl . valueType) args

single :: Value -> [Value]
single v = [v]

evalSubExp :: SubExp -> FutharkM Value
evalSubExp (Var ident)    = lookupVar ident
evalSubExp (Constant v _) = return v

evalBody :: Body lore -> FutharkM [Value]

evalBody (Body _ [] (Result _ es _)) =
  mapM evalSubExp es

evalBody (Body lore (Let pat _ e:bnds) res) = do
  v <- evalExp e
  binding (zip (patternIdents pat) v) $ evalBody $ Body lore bnds res

evalExp :: Exp lore -> FutharkM [Value]
evalExp (If e1 e2 e3 rettype pos) = do
  v <- evalSubExp e1
  vs <- case v of BasicVal (LogVal True)  -> evalBody e2
                  BasicVal (LogVal False) -> evalBody e3
                  _                       -> bad $ TypeError pos "evalExp If"
  return $ valueShapeContext rettype vs ++ vs
evalExp (Apply fname args _ loc)
  | "trace" <- nameToString fname = do
  vs <- mapM (evalSubExp . fst) args
  tell [(loc, pretty vs)]
  return vs
evalExp (Apply fname args rettype _) = do
  vs <- evalFuncall fname $ map fst args
  return $ valueShapeContext rettype vs ++ vs
evalExp (PrimOp op) = evalPrimOp op
evalExp (LoopOp op) = evalLoopOp op

evalPrimOp :: PrimOp lore -> FutharkM [Value]

evalPrimOp (SubExp se) =
  single <$> evalSubExp se

evalPrimOp (ArrayLit es rt _) =
  single <$> (arrayVal <$> mapM evalSubExp es <*> pure rt)

evalPrimOp (BinOp Plus e1 e2 (Basic Int) pos) = evalIntBinOp (+) e1 e2 pos
evalPrimOp (BinOp Plus e1 e2 (Basic Real) pos) = evalRealBinOp (+) e1 e2 pos
evalPrimOp (BinOp Minus e1 e2 (Basic Int) pos) = evalIntBinOp (-) e1 e2 pos
evalPrimOp (BinOp Minus e1 e2 (Basic Real) pos) = evalRealBinOp (-) e1 e2 pos
evalPrimOp (BinOp Pow e1 e2 (Basic Int) pos) = evalIntBinOp pow e1 e2 pos
  -- Haskell (^) cannot handle negative exponents, so check for that
  -- explicitly.
  where pow x y | y < 0, x == 0 = error "Negative exponential with zero base"
                | y < 0         = 1 `div` (x ^ (-y))
                | otherwise     = x ^ y
evalPrimOp (BinOp Pow e1 e2 (Basic Real) pos) = evalRealBinOp (**) e1 e2 pos
evalPrimOp (BinOp Times e1 e2 (Basic Int) pos) = evalIntBinOp (*) e1 e2 pos
evalPrimOp (BinOp Times e1 e2 (Basic Real) pos) = evalRealBinOp (*) e1 e2 pos
evalPrimOp (BinOp Divide e1 e2 (Basic Int) pos) = evalIntBinOp div e1 e2 pos
evalPrimOp (BinOp Mod e1 e2 (Basic Int) pos) = evalIntBinOp mod e1 e2 pos
evalPrimOp (BinOp Divide e1 e2 (Basic Real) pos) = evalRealBinOp (/) e1 e2 pos
evalPrimOp (BinOp ShiftR e1 e2 _ pos) = evalIntBinOp shiftR e1 e2 pos
evalPrimOp (BinOp ShiftL e1 e2 _ pos) = evalIntBinOp shiftL e1 e2 pos
evalPrimOp (BinOp Band e1 e2 _ pos) = evalIntBinOp (.&.) e1 e2 pos
evalPrimOp (BinOp Xor e1 e2 _ pos) = evalIntBinOp xor e1 e2 pos
evalPrimOp (BinOp Bor e1 e2 _ pos) = evalIntBinOp (.|.) e1 e2 pos
evalPrimOp (BinOp LogAnd e1 e2 _ pos) = evalBoolBinOp (&&) e1 e2 pos
evalPrimOp (BinOp LogOr e1 e2 _ pos) = evalBoolBinOp (||) e1 e2 pos

evalPrimOp (BinOp Equal e1 e2 _ _) = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  return [BasicVal $ LogVal (v1==v2)]

evalPrimOp (BinOp Less e1 e2 _ _) = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  return [BasicVal $ LogVal (v1<v2)]

evalPrimOp (BinOp Leq e1 e2 _ _) = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  return [BasicVal $ LogVal (v1<=v2)]

evalPrimOp (BinOp _ _ _ _ pos) = bad $ TypeError pos "evalPrimOp Binop"

evalPrimOp (Not e pos) = do
  v <- evalSubExp e
  case v of BasicVal (LogVal b) -> return [BasicVal $ LogVal (not b)]
            _                     -> bad $ TypeError pos "evalPrimOp Not"

evalPrimOp (Negate e pos) = do
  v <- evalSubExp e
  case v of BasicVal (IntVal x)  -> return [BasicVal $ IntVal (-x)]
            BasicVal (RealVal x) -> return [BasicVal $ RealVal (-x)]
            _                      -> bad $ TypeError pos "evalPrimOp Negate"



evalPrimOp (Index _ ident idxs pos) = do
  v <- lookupVar ident
  idxs' <- mapM evalSubExp idxs
  single <$> foldM idx v idxs'
  where idx (ArrayVal arr _) (BasicVal (IntVal i))
          | i >= 0 && i <= upper = return $ arr ! i
          | otherwise            =
            bad $ IndexOutOfBounds pos (textual $ identName ident) (upper+1) i
          where upper = snd $ bounds arr
        idx _ _ = bad $ TypeError pos "evalPrimOp Index"

evalPrimOp (Update _ src idxs ve loc) = do
  v <- lookupVar src
  idxs' <- mapM evalSubExp idxs
  vev <- evalSubExp ve
  single <$> change v idxs' vev
  where change _ [] to = return to
        change (ArrayVal arr t) (BasicVal (IntVal i):rest) to
          | i >= 0 && i <= upper = do
            let x = arr ! i
            x' <- change x rest to
            return $ ArrayVal (arr // [(i, x')]) t
          | otherwise = bad $ IndexOutOfBounds loc
                              (textual $ identName src) (upper+1) i
          where upper = snd $ bounds arr
        change _ _ _ = bad $ TypeError loc "evalBody Let Id"

evalPrimOp (Iota e pos) = do
  v <- evalSubExp e
  case v of
    BasicVal (IntVal x)
      | x >= 0    ->
        return [arrayVal (map (BasicVal . IntVal) [0..x-1])
                         (Basic Int :: DeclType)]
      | otherwise ->
        bad $ NegativeIota pos x
    _ -> bad $ TypeError pos "evalPrimOp Iota"

evalPrimOp (Replicate e1 e2 pos) = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  case v1 of
    BasicVal (IntVal x)
      | x >= 0    ->
        return [arrayVal (replicate x v2) $ valueType v2]
      | otherwise -> bad $ NegativeReplicate pos x
    _   -> bad $ TypeError pos "evalPrimOp Replicate"

evalPrimOp (Reshape _ shapeexp arrexp pos) = do
  shape <- mapM (asInt <=< evalSubExp) shapeexp
  arr <- evalSubExp arrexp
  let arrt = toDecl $ subExpType arrexp
      rt = arrayOf arrt (Rank $ length shapeexp-1) (uniqueness arrt)
      reshape (n:rest) vs
        | length vs `mod` n == 0 =
          arrayVal <$> mapM (reshape rest) (chunk (length vs `div` n) vs)
                   <*> pure rt
      reshape [] [v] = return v
      reshape _ _ = bad $ InvalidArrayShape pos (valueShape arr) shape
  single <$> reshape shape (flatten arr)
  where flatten (ArrayVal arr _) = concatMap flatten $ elems arr
        flatten t = [t]
        chunk _ [] = []
        chunk i l = let (a,b) = splitAt i l
                    in a : chunk i b
        asInt (BasicVal (IntVal x)) = return x
        asInt _ = bad $ TypeError pos "evalPrimOp Reshape asInt"

evalPrimOp (Rearrange _ perm arrexp _) =
  single <$> permuteArray perm <$> evalSubExp arrexp

evalPrimOp (Rotate _ perm arrexp _) =
  single <$> rotateArray perm <$> evalSubExp arrexp

evalPrimOp (Split _ splitexp arrexp _ pos) = do
  split <- evalSubExp splitexp
  vs <- arrToList pos =<< evalSubExp arrexp
  case split of
    BasicVal (IntVal i)
      | i <= length vs ->
        let (bef,aft) = splitAt i vs
        in return [arrayVal bef rt, arrayVal aft rt]
      | otherwise        -> bad $ IndexOutOfBounds pos (pretty arrexp) (length vs) i
    _ -> bad $ TypeError pos "evalPrimOp Split"
  where rt = rowType $ subExpType arrexp

evalPrimOp (Concat _ arr1exp arr2exp _ pos) = do
  elems1 <- arrToList pos =<< evalSubExp arr1exp
  elems2 <- arrToList pos =<< evalSubExp arr2exp
  return $ single $ arrayVal (elems1 ++ elems2) $ stripArray 1 $ subExpType arr1exp

evalPrimOp (Copy e _) = single <$> evalSubExp e

evalPrimOp (Assert e loc) = do
  v <- evalSubExp e
  case v of BasicVal (LogVal True) ->
              return [BasicVal Checked]
            _ ->
              bad $ AssertFailed loc

evalPrimOp (Conjoin _ _) = return [BasicVal Checked]

-- Alloc is not used in the interpreter, so just return whatever
evalPrimOp (Alloc se _) =
  single <$> evalSubExp se

evalLoopOp :: LoopOp lore -> FutharkM [Value]

evalLoopOp (DoLoop respat merge loopvar boundexp loopbody loc) = do
  bound <- evalSubExp boundexp
  mergestart <- mapM evalSubExp mergeexp
  case bound of
    BasicVal (IntVal n) -> do
      vs <- foldM iteration mergestart [0..n-1]
      binding (zip mergepat vs) $
        mapM lookupVar $ loopResult respat $ map fst merge
    _ -> bad $ TypeError loc "evalBody DoLoop"
  where (mergepat, mergeexp) = unzip merge
        iteration mergeval i =
          binding [(loopvar, BasicVal $ IntVal i)] $
            binding (zip mergepat mergeval) $
              evalBody loopbody

evalLoopOp (Map _ fun arrexps loc) = do
  vss <- mapM (arrToList loc <=< evalSubExp) arrexps
  vs' <- mapM (applyLambda fun) $ transpose vss
  return $ arrays (lambdaReturnType fun) vs'

evalLoopOp (Reduce _ fun inputs loc) = do
  let (accexps, arrexps) = unzip inputs
  startaccs <- mapM evalSubExp accexps
  vss <- mapM (arrToList loc <=< evalSubExp) arrexps
  let foldfun acc x = applyLambda fun $ acc ++ x
  foldM foldfun startaccs (transpose vss)

evalLoopOp (Scan _ fun inputs loc) = do
  let (accexps, arrexps) = unzip inputs
  startvals <- mapM evalSubExp accexps
  vss <- mapM (arrToList loc <=< evalSubExp) arrexps
  (acc, vals') <- foldM scanfun (startvals, []) $ transpose vss
  return $ arrays (map valueType acc) $ reverse vals'
    where scanfun (acc, l) x = do
            acc' <- applyLambda fun $ acc ++ x
            return (acc', acc' : l)

evalLoopOp e@(Filter _ fun arrexp loc) = do
  vss <- mapM (arrToList loc <=< evalSubExp) arrexp
  vss' <- filterM filt $ transpose vss
  return $ BasicVal (IntVal $ length vss') : arrays (loopOpType e) vss'
  where filt x = do
          res <- applyLambda fun x
          case res of [BasicVal (LogVal True)] -> return True
                      _                          -> return False

evalLoopOp (Redomap _ _ innerfun accexp arrexps loc) = do
  startaccs <- mapM evalSubExp accexp
  vss <- mapM (arrToList loc <=< evalSubExp) arrexps
  let foldfun acc x = applyLambda innerfun $ acc ++ x
  foldM foldfun startaccs $ transpose vss

evalFuncall :: Name -> [SubExp] -> FutharkM [Value]
evalFuncall fname args = do
  fun <- lookupFun fname
  args' <- mapM evalSubExp args
  fun args'

evalIntBinOp :: (Int -> Int -> Int) -> SubExp -> SubExp -> SrcLoc -> FutharkM [Value]
evalIntBinOp op e1 e2 loc = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  case (v1, v2) of
    (BasicVal (IntVal x), BasicVal (IntVal y)) ->
      return [BasicVal $ IntVal (op x y)]
    _ ->
      bad $ TypeError loc "evalIntBinOp"

evalRealBinOp :: (Double -> Double -> Double) -> SubExp -> SubExp -> SrcLoc -> FutharkM [Value]
evalRealBinOp op e1 e2 loc = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  case (v1, v2) of
    (BasicVal (RealVal x), BasicVal (RealVal y)) ->
      return [BasicVal $ RealVal (op x y)]
    _ ->
      bad $ TypeError loc $ "evalRealBinOp " ++ pretty v1 ++ " " ++ pretty v2

evalBoolBinOp :: (Bool -> Bool -> Bool) -> SubExp -> SubExp -> SrcLoc -> FutharkM [Value]
evalBoolBinOp op e1 e2 loc = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  case (v1, v2) of
    (BasicVal (LogVal x), BasicVal (LogVal y)) ->
      return [BasicVal $ LogVal (op x y)]
    _ ->
      bad $ TypeError loc $ "evalBoolBinOp " ++ pretty v1 ++ " " ++ pretty v2

applyLambda :: Lambda lore -> [Value] -> FutharkM [Value]
applyLambda (Lambda params body rettype loc) args =
  do v <- binding (zip params args) $ evalBody body
     checkReturnShapes loc rettype v
     return v

checkPatSizes :: [(Ident, Value)]-> FutharkM ()
checkPatSizes = mapM_ $ uncurry checkSize
  where checkSize var val = do
          let valshape = map value $ valueShape val
              varname = textual $ identName var
              vardims = arrayDims $ identType var
              loc = srclocOf var
          varshape <- mapM evalSubExp vardims
          when (varshape /= valshape) $
            bad $ TypeError loc $ "checkPatSizes:\n" ++
                                  varname ++ " is specified to have shape [" ++
                                  intercalate "," (zipWith ppDim vardims varshape) ++
                                  "], but is being bound to value of shape [" ++
                                  intercalate "," (map pretty valshape) ++ "]."

        ppDim (Constant v _) _ = pretty v
        ppDim e              v = pretty e ++ "=" ++ pretty v

checkReturnShapes :: SrcLoc -> [Type] -> [Value] -> FutharkM ()
checkReturnShapes loc = zipWithM_ checkShape
  where checkShape t val = do
          let valshape = map (BasicVal . IntVal) $ valueShape val
              retdims = arrayDims t
          varshape <- mapM evalSubExp retdims
          when (varshape /= valshape) $
            bad $ TypeError loc $ "checkReturnShapes:\n" ++
                                  "Return type specifies shape [" ++
                                  intercalate "," (zipWith ppDim retdims varshape) ++
                                  "], but returned value is of shape [" ++
                                  intercalate "," (map pretty valshape) ++ "]."

        ppDim (Constant v _) _ = pretty v
        ppDim e              v = pretty e ++ "=" ++ pretty v
