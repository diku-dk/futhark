{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
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
  , runFunWithShapes
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
import Data.Maybe

import Futhark.Representation.AST.Lore (Lore)
import Futhark.Representation.AST

-- | An error happened during execution, and this is why.
data InterpreterError lore =
      MissingEntryPoint Name
      -- ^ The specified start function does not exist.
    | InvalidFunctionArguments Name (Maybe [DeclType]) [DeclType]
      -- ^ The arguments given to a function were mistyped.
    | IndexOutOfBounds SrcLoc String Int Int
      -- ^ First @Int@ is array size, second is attempted index.
    | NegativeIota SrcLoc Int
      -- ^ Called @iota(n)@ where @n@ was negative.
    | NegativeReplicate SrcLoc Int
      -- ^ Called @replicate(n, x)@ where @n@ was negative.
    | InvalidArrayShape SrcLoc (Exp lore) [Int] [Int]
      -- ^ First @Int@ is old shape, second is attempted new shape.
    | ZipError SrcLoc [Int]
      -- ^ The arguments to @zip@ were of different lengths.
    | AssertFailed SrcLoc
      -- ^ Assertion failed at this location.
    | TypeError SrcLoc String
      -- ^ Some value was of an unexpected type.
    | DivisionByZero SrcLoc
      -- ^ Attempted to divide by zero.

instance PrettyLore lore => Show (InterpreterError lore) where
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
  show (InvalidArrayShape pos e shape newshape) =
    "Invalid array reshaping " ++ pretty e ++
    " at " ++ locStr pos ++
    ", from " ++ show shape ++ " to " ++ show newshape
  show (ZipError pos lengths) =
    "Array arguments to zip must have same length, but arguments at " ++
    locStr pos ++ " have lenghts " ++ intercalate ", " (map show lengths) ++ "."
  show (AssertFailed loc) =
    "Assertion failed at " ++ locStr loc ++ "."
  show (DivisionByZero loc) =
    "Division by zero at " ++ locStr loc ++ "."

instance Error (InterpreterError lore) where
  strMsg = TypeError noLoc

type FunTable lore = HM.HashMap Name ([Value] -> FutharkM lore [Value])

data FutharkEnv lore = FutharkEnv { envVtable :: HM.HashMap VName Value
                                  , envFtable :: FunTable lore
                                  }

-- | A list of places where @trace@ was called, alongside the
-- prettyprinted value that was passed to it.
type Trace = [(SrcLoc, String)]

newtype FutharkM lore a = FutharkM (ReaderT (FutharkEnv lore)
                                    (ErrorT (InterpreterError lore)
                                     (Writer Trace)) a)
  deriving (Monad, Applicative, Functor,
            MonadReader (FutharkEnv lore),
            MonadWriter Trace)

runFutharkM :: FutharkM lore a -> FutharkEnv lore
            -> (Either (InterpreterError lore) a, Trace)
runFutharkM (FutharkM m) env = runWriter $ runErrorT $ runReaderT m env

bad :: InterpreterError lore -> FutharkM lore a
bad = FutharkM . throwError

bindVar :: FutharkEnv lore -> (Ident, Value) -> FutharkEnv lore
bindVar env (Ident name _ _,val) =
  env { envVtable = HM.insert name val $ envVtable env }

bindVars :: FutharkEnv lore -> [(Ident, Value)] -> FutharkEnv lore
bindVars = foldl bindVar

binding :: [(Ident, Value)] -> FutharkM lore a -> FutharkM lore a
binding bnds m = local (`bindVars` bnds) $ checkPatSizes bnds >> m

lookupVar :: Ident -> FutharkM lore Value
lookupVar (Ident vname _ loc) = do
  val <- asks $ HM.lookup vname . envVtable
  case val of Just val' -> return val'
              Nothing   -> bad $ TypeError loc $ "lookupVar " ++ textual vname

lookupFun :: Name -> FutharkM lore ([Value] -> FutharkM lore [Value])
lookupFun fname = do
  fun <- asks $ HM.lookup fname . envFtable
  case fun of Just fun' -> return fun'
              Nothing   -> bad $ TypeError noLoc $ "lookupFun " ++ textual fname

arrToList :: SrcLoc -> Value -> FutharkM lore [Value]
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

-- |  @runFun name args prog@ invokes the @name@ function of program
-- @prog@, with the parameters bound in order to the values in @args@.
-- Returns either an error or the return value of @fun@.
-- Additionally, a list of all calls to the special built-in function
-- @trace@ is always returned.  This is useful for debugging.
--
-- Note that if 'prog' is not type-correct, you cannot be sure that
-- you'll get an error from the interpreter - it may just as well
-- silently return a wrong value.  You are, however, guaranteed that
-- the initial call to 'prog' is properly checked.
runFun :: Lore lore => Name -> [Value] -> Prog lore
       -> (Either (InterpreterError lore) [Value], Trace)
runFun fname mainargs prog = do
  let ftable = buildFunTable prog
      futharkenv = FutharkEnv { envVtable = HM.empty
                              , envFtable = ftable
                              }
  case (funDecByName fname prog, HM.lookup fname ftable) of
    (Nothing, Nothing) -> (Left $ MissingEntryPoint fname, mempty)
    (Just fundec, _) ->
      runThisFun fundec mainargs ftable
    (_ , Just fun) -> -- It's a builtin function, it'll do its own
                      -- error checking.
      runFutharkM (fun mainargs) futharkenv

-- | Like 'runFun', but prepends parameters corresponding to the
-- required shape context of the function being called.
runFunWithShapes :: Lore lore => Name -> [Value] -> Prog lore
                 -> (Either (InterpreterError lore) [Value], Trace)
runFunWithShapes fname valargs prog = do
  let ftable = buildFunTable prog
      futharkenv = FutharkEnv { envVtable = HM.empty
                              , envFtable = ftable
                              }
  case (funDecByName fname prog, HM.lookup fname ftable) of
    (Nothing, Nothing) -> (Left $ MissingEntryPoint fname, mempty)
    (Just fundec, _) ->
      let args' = shapes (funDecParams fundec) ++ valargs
      in runThisFun fundec args' ftable
    (_ , Just fun) -> -- It's a builtin function, it'll do its own
                      -- error checking.
      runFutharkM (fun valargs) futharkenv
  where shapes params =
          let (shapeparams, valparams) =
                splitAt (length params - length valargs) params
              shapemap = shapeMapping'
                         (map bindeeType valparams)
                         (map valueShape valargs)
          in map (BasicVal . IntVal . fromMaybe 0 .
                  flip HM.lookup shapemap .
                  bindeeName)
             shapeparams

-- | As 'runFun', but throws away the trace.
runFunNoTrace :: Lore lore => Name -> [Value] -> Prog lore
              -> Either (InterpreterError lore) [Value]
runFunNoTrace = ((.) . (.) . (.)) fst runFun -- I admit this is just for fun.

runThisFun :: Lore lore => FunDec lore -> [Value] -> FunTable lore
           -> (Either (InterpreterError lore) [Value], Trace)
runThisFun (FunDec fname _ fparams _ _) args ftable
  | length argtypes == length fparams,
    subtypesOf argtypes paramtypes =
    runFutharkM (evalFuncall fname args) futharkenv
  | otherwise =
    (Left $ InvalidFunctionArguments fname
     (Just (map (toDecl . bindeeType) fparams))
     (map toDecl argtypes),
     mempty)
  where argtypes = map (toDecl . (`setUniqueness` Unique) . valueType) args
        paramtypes = map (toDecl . bindeeType) fparams
        futharkenv = FutharkEnv { envVtable = HM.empty
                                , envFtable = ftable
                                }

buildFunTable :: Lore lore =>
                 Prog lore -> FunTable lore
buildFunTable = foldl expand builtins . progFunctions
  where -- We assume that the program already passed the type checker, so
        -- we don't check for duplicate definitions.
        expand ftable' (FunDec name _ params body _) =
          let fun funargs = binding (zip (map bindeeIdent params) funargs) $
                            evalBody body
          in HM.insert name fun ftable'

--------------------------------------------
--------------------------------------------
------------- BUILTIN FUNCTIONS ------------
--------------------------------------------
--------------------------------------------

builtins :: HM.HashMap Name ([Value] -> FutharkM lore [Value])
builtins = HM.fromList $ map namify
           [("toReal", builtin "toReal")
           ,("trunc", builtin "trunc")
           ,("sqrt", builtin "sqrt")
           ,("log", builtin "log")
           ,("exp", builtin "exp")
           ,("op not", builtin "op not")
           ,("op ~", builtin "op ~")]
  where namify (k,v) = (nameFromString k, v)

builtin :: String -> [Value] -> FutharkM lore [Value]
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

evalSubExp :: SubExp -> FutharkM lore Value
evalSubExp (Var ident)    = lookupVar ident
evalSubExp (Constant v _) = return $ BasicVal v

evalBody :: Lore lore => Body lore -> FutharkM lore [Value]

evalBody (Body _ [] (Result es _)) =
  mapM evalSubExp es

evalBody (Body lore (Let pat _ e:bnds) res) = do
  v <- evalExp e
  binding (zip (patternIdents pat) v) $ evalBody $ Body lore bnds res

evalExp :: Lore lore => Exp lore -> FutharkM lore [Value]
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
  args' <- mapM (evalSubExp . fst) args
  vs <- evalFuncall fname args'
  return $ valueShapeContext (retTypeValues rettype) vs ++ vs
evalExp (PrimOp op) = evalPrimOp op
evalExp (LoopOp op) = evalLoopOp op

evalPrimOp :: Lore lore => PrimOp lore -> FutharkM lore [Value]

evalPrimOp (SubExp se) =
  single <$> evalSubExp se

evalPrimOp (ArrayLit es rt _) =
  single <$> (arrayVal <$> mapM evalSubExp es <*> pure rt)

evalPrimOp (BinOp Plus e1 e2 (Basic Int) pos) = evalIntBinOp (+) e1 e2 pos
evalPrimOp (BinOp Plus e1 e2 (Basic Real) pos) = evalRealBinOp (+) e1 e2 pos
evalPrimOp (BinOp Minus e1 e2 (Basic Int) pos) = evalIntBinOp (-) e1 e2 pos
evalPrimOp (BinOp Minus e1 e2 (Basic Real) pos) = evalRealBinOp (-) e1 e2 pos
evalPrimOp (BinOp Pow e1 e2 (Basic Int) pos) = evalIntBinOpM pow e1 e2 pos
  -- Haskell (^) cannot handle negative exponents, so check for that
  -- explicitly.
  where pow x y | y < 0, x == 0 = bad $ DivisionByZero pos
                | y < 0         = return $ 1 `div` (x ^ (-y))
                | otherwise     = return $ x ^ y
evalPrimOp (BinOp Pow e1 e2 (Basic Real) pos) = evalRealBinOp (**) e1 e2 pos
evalPrimOp (BinOp Times e1 e2 (Basic Int) pos) = evalIntBinOp (*) e1 e2 pos
evalPrimOp (BinOp Times e1 e2 (Basic Real) pos) = evalRealBinOp (*) e1 e2 pos
evalPrimOp (BinOp Divide e1 e2 (Basic Int) pos) = evalIntBinOpM div' e1 e2 pos
  where div' _ 0 = bad $ DivisionByZero pos
        div' x y = return $ x `div` y
evalPrimOp (BinOp Mod e1 e2 (Basic Int) pos) = evalIntBinOpM mod' e1 e2 pos
  where mod' _ 0 = bad $ DivisionByZero pos
        mod' x y = return $ x `mod` y
evalPrimOp (BinOp Divide e1 e2 (Basic Real) pos) = evalRealBinOpM div' e1 e2 pos
  where div' _ 0 = bad $ DivisionByZero pos
        div' x y = return $ x / y
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

evalPrimOp e@(Reshape _ shapeexp arrexp pos) = do
  shape <- mapM (asInt <=< evalSubExp) shapeexp
  arr <- lookupVar arrexp
  let arrt = toDecl $ identType arrexp
      rt = arrayOf arrt (Rank $ length shapeexp-1) (uniqueness arrt)
      reshape (0:rest) [] =
        arrayVal <$> mapM (reshape rest) [] <*> pure rt
      reshape (n:rest) vs
        | n > 0, length vs `mod` n == 0 =
          arrayVal <$> mapM (reshape rest) (chunk (length vs `div` n) vs)
                   <*> pure rt
      reshape [] [v] = return v
      reshape [] [] =
        return $ arrayVal [] rt
      reshape _ _ = bad $ InvalidArrayShape pos (PrimOp e) (valueShape arr) shape
  single <$> reshape shape (flatten arr)
  where flatten (ArrayVal arr _) = concatMap flatten $ elems arr
        flatten t = [t]
        chunk _ [] = []
        chunk i l = let (a,b) = splitAt i l
                    in a : chunk i b
        asInt (BasicVal (IntVal x)) = return x
        asInt _ = bad $ TypeError pos "evalPrimOp Reshape asInt"

evalPrimOp (Rearrange _ perm arrexp _) =
  single <$> permuteArray perm <$> lookupVar arrexp

evalPrimOp (Rotate _ perm arrexp _) =
  single <$> rotateArray perm <$> lookupVar arrexp

evalPrimOp (Split _ splitexp arrexp _ pos) = do
  split <- evalSubExp splitexp
  vs <- arrToList pos =<< lookupVar arrexp
  case split of
    BasicVal (IntVal i)
      | i <= length vs ->
        let (bef,aft) = splitAt i vs
        in return [arrayVal bef rt, arrayVal aft rt]
      | otherwise        -> bad $ IndexOutOfBounds pos (pretty arrexp) (length vs) i
    _ -> bad $ TypeError pos "evalPrimOp Split"
  where rt = rowType $ identType arrexp

evalPrimOp (Concat _ arr1exp arr2exp _ pos) = do
  elems1 <- arrToList pos =<< lookupVar arr1exp
  elems2 <- arrToList pos =<< lookupVar arr2exp
  return $ single $ arrayVal (elems1 ++ elems2) $ stripArray 1 $ identType arr1exp

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

evalLoopOp :: forall lore . Lore lore => LoopOp lore -> FutharkM lore [Value]

evalLoopOp (DoLoop respat merge loopvar boundexp loopbody loc) = do
  bound <- evalSubExp boundexp
  mergestart <- mapM evalSubExp mergeexp
  case bound of
    BasicVal (IntVal n) -> do
      vs <- foldM iteration mergestart [0..n-1]
      binding (zip (map bindeeIdent mergepat) vs) $
        mapM lookupVar $
        loopResultContext (representative :: lore) respat mergepat ++ respat
    _ -> bad $ TypeError loc "evalBody DoLoop"
  where (mergepat, mergeexp) = unzip merge
        iteration mergeval i =
          binding [(loopvar, BasicVal $ IntVal i)] $
            binding (zip (map bindeeIdent mergepat) mergeval) $
              evalBody loopbody

evalLoopOp (Map _ fun arrexps loc) = do
  vss <- mapM (arrToList loc <=< lookupVar) arrexps
  vs' <- mapM (applyLambda fun) $ transpose vss
  return $ arrays (lambdaReturnType fun) vs'

evalLoopOp (Reduce _ fun inputs loc) = do
  let (accexps, arrexps) = unzip inputs
  startaccs <- mapM evalSubExp accexps
  vss <- mapM (arrToList loc <=< lookupVar) arrexps
  let foldfun acc x = applyLambda fun $ acc ++ x
  foldM foldfun startaccs (transpose vss)

evalLoopOp (Scan _ fun inputs loc) = do
  let (accexps, arrexps) = unzip inputs
  startvals <- mapM evalSubExp accexps
  vss <- mapM (arrToList loc <=< lookupVar) arrexps
  (acc, vals') <- foldM scanfun (startvals, []) $ transpose vss
  return $ arrays (map valueType acc) $ reverse vals'
    where scanfun (acc, l) x = do
            acc' <- applyLambda fun $ acc ++ x
            return (acc', acc' : l)

evalLoopOp (Filter _ fun arrexp loc) = do
  vss <- mapM (arrToList loc <=< lookupVar) arrexp
  vss' <- filterM filt $ transpose vss
  return $
    BasicVal (IntVal $ length vss') :
    arrays (filterType fun $ map identType arrexp) vss'
  where filt x = do
          res <- applyLambda fun x
          case res of [BasicVal (LogVal True)] -> return True
                      _                          -> return False

evalLoopOp (Redomap _ _ innerfun accexp arrexps loc) = do
  startaccs <- mapM evalSubExp accexp
  vss <- mapM (arrToList loc <=< lookupVar) arrexps
  let foldfun acc x = applyLambda innerfun $ acc ++ x
  foldM foldfun startaccs $ transpose vss

evalFuncall :: Name -> [Value] -> FutharkM lore [Value]
evalFuncall fname args = do
  fun <- lookupFun fname
  fun args

evalIntBinOp :: (Int -> Int -> Int) -> SubExp -> SubExp -> SrcLoc
             -> FutharkM lore [Value]
evalIntBinOp op = evalIntBinOpM $ \x y -> return $ op x y

evalIntBinOpM :: (Int -> Int -> FutharkM lore Int)
              -> SubExp
              -> SubExp
              -> SrcLoc
              -> FutharkM lore [Value]
evalIntBinOpM op e1 e2 loc = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  case (v1, v2) of
    (BasicVal (IntVal x), BasicVal (IntVal y)) -> do
      result <- op x y
      return [BasicVal $ IntVal result]
    _ ->
      bad $ TypeError loc "evalIntBinOpM"

evalRealBinOp :: (Double -> Double -> Double) -> SubExp -> SubExp -> SrcLoc
             -> FutharkM lore [Value]
evalRealBinOp op = evalRealBinOpM $ \x y -> return $ op x y

evalRealBinOpM :: (Double -> Double -> FutharkM lore Double)
               -> SubExp
               -> SubExp
               -> SrcLoc
               -> FutharkM lore [Value]
evalRealBinOpM op e1 e2 loc = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  case (v1, v2) of
    (BasicVal (RealVal x), BasicVal (RealVal y)) -> do
      result <- op x y
      return [BasicVal $ RealVal result]
    _ ->
      bad $ TypeError loc $ "evalRealBinOpM " ++ pretty v1 ++ " " ++ pretty v2

evalBoolBinOp :: (Bool -> Bool -> Bool) -> SubExp -> SubExp -> SrcLoc -> FutharkM lore [Value]
evalBoolBinOp op e1 e2 loc = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  case (v1, v2) of
    (BasicVal (LogVal x), BasicVal (LogVal y)) ->
      return [BasicVal $ LogVal (op x y)]
    _ ->
      bad $ TypeError loc $ "evalBoolBinOp " ++ pretty v1 ++ " " ++ pretty v2

applyLambda :: Lore lore => Lambda lore -> [Value] -> FutharkM lore [Value]
applyLambda (Lambda params body rettype loc) args =
  do v <- binding (zip params args) $ evalBody body
     checkReturnShapes loc rettype v
     return v

checkPatSizes :: [(Ident, Value)]-> FutharkM lore ()
checkPatSizes = mapM_ $ uncurry checkSize
  where checkSize var val = do
          let valshape = map (BasicVal . value) $ valueShape val
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

checkReturnShapes :: SrcLoc -> [Type] -> [Value] -> FutharkM lore ()
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
