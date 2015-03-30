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
import Control.Monad.Except

import Data.Array
import Data.Bits
import Data.List
import Data.Loc
import qualified Data.HashMap.Strict as HM
import Data.Maybe

import Prelude

import Futhark.Representation.AST.Lore (Lore)
import Futhark.Representation.AST
import Futhark.Util

-- | An error happened during execution, and this is why.
data InterpreterError lore =
      MissingEntryPoint Name
      -- ^ The specified start function does not exist.
    | InvalidFunctionArguments Name (Maybe [DeclType]) [DeclType]
      -- ^ The arguments given to a function were mistyped.
    | IndexOutOfBounds String [Int] [Int]
      -- ^ First @Int@ is array shape, second is attempted index.
    | SplitOutOfBounds String [Int] [Int]
      -- ^ First @[Int]@ is array shape, second is attempted split
      -- sizes.
    | NegativeIota Int
      -- ^ Called @iota(n)@ where @n@ was negative.
    | NegativeReplicate Int
      -- ^ Called @replicate(n, x)@ where @n@ was negative.
    | InvalidArrayShape (Exp lore) [Int] [Int]
      -- ^ First @Int@ is old shape, second is attempted new shape.
    | ZipError [Int]
      -- ^ The arguments to @zip@ were of different lengths.
    | AssertFailed SrcLoc
      -- ^ Assertion failed at this location.
    | TypeError String
      -- ^ Some value was of an unexpected type.
    | DivisionByZero
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
  show (IndexOutOfBounds var arrsz i) =
    "Array index " ++ show i ++ " out of bounds in array '" ++
    var ++ "', of size " ++ show arrsz ++ "."
  show (SplitOutOfBounds var arrsz sizes) =
    "Split not valid for sizes " ++ show sizes ++
    " on array '" ++ var ++ "', with shape " ++ show arrsz ++ "."
  show (NegativeIota n) =
    "Argument " ++ show n ++ " to iota at is negative."
  show (NegativeReplicate n) =
    "Argument " ++ show n ++ " to replicate is negative."
  show (TypeError s) =
    "Type error during interpretation: " ++ s
  show (InvalidArrayShape e shape newshape) =
    "Invalid array reshaping " ++ pretty e ++
    ", from " ++ show shape ++ " to " ++ show newshape
  show (ZipError lengths) =
    "Array arguments to zip must have same length, but arguments have lengths " ++ intercalate ", " (map show lengths) ++ "."
  show (AssertFailed loc) =
    "Assertion failed at " ++ locStr loc ++ "."
  show DivisionByZero =
    "Division by zero."

type FunTable lore = HM.HashMap Name ([Value] -> FutharkM lore [Value])

type VTable = HM.HashMap VName Value

data FutharkEnv lore = FutharkEnv { envVtable :: VTable
                                  , envFtable :: FunTable lore
                                  }

-- | A list of the prettyprinted values that were passed to @trace@
type Trace = [String]

newtype FutharkM lore a = FutharkM (ReaderT (FutharkEnv lore)
                                    (ExceptT (InterpreterError lore)
                                     (Writer Trace)) a)
  deriving (Monad, Applicative, Functor,
            MonadReader (FutharkEnv lore),
            MonadWriter Trace)

runFutharkM :: FutharkM lore a -> FutharkEnv lore
            -> (Either (InterpreterError lore) a, Trace)
runFutharkM (FutharkM m) env = runWriter $ runExceptT $ runReaderT m env

bad :: InterpreterError lore -> FutharkM lore a
bad = FutharkM . throwError

bindVar :: Bindage -> Value
        -> FutharkM lore Value

bindVar BindVar val =
  return val

bindVar (BindInPlace _ src is) val = do
  srcv <- lookupVar src
  is' <- mapM (asInt <=< evalSubExp) is
  case srcv of
    ArrayVal arr bt shape -> do
      flatidx <- indexArray (textual $ identName src) shape is'
      if length is' == length shape then
        case val of
          BasicVal bv ->
            return $ ArrayVal (arr // [(flatidx, bv)]) bt shape
          _ ->
            bad $ TypeError "bindVar BindInPlace, full indices given, but replacement value is not a basic value"
      else
        case val of
          ArrayVal valarr _ valshape ->
            let updates =
                  [ (flatidx + i, valarr ! i) | i <- [0..product valshape-1] ]
            in return $ ArrayVal (arr // updates) bt shape
          BasicVal _ ->
            bad $ TypeError "bindVar BindInPlace, incomplete indices given, but replacement value is not array"
    _ ->
      bad $ TypeError "bindVar BindInPlace, source is not array"
  where asInt (BasicVal (IntVal x)) = return x
        asInt _                     = bad $ TypeError "bindVar BindInPlace"

bindVars :: [(Ident, Bindage, Value)]
         -> FutharkM lore VTable
bindVars bnds = do
  let (idents, bindages, vals) = unzip3 bnds
  HM.fromList . zip (map identName idents) <$>
    zipWithM bindVar bindages vals

binding :: [(Ident, Bindage, Value)]
        -> FutharkM lore a
        -> FutharkM lore a
binding bnds m = do
  vtable <- bindVars bnds
  local (extendVtable vtable) $ do
    checkBoundShapes bnds
    m
  where extendVtable vtable env = env { envVtable = vtable <> envVtable env }

        checkBoundShapes = mapM_ checkShape
        checkShape (ident, BindVar, val) = do
          let valshape = map (BasicVal . value) $ valueShape val
              vardims = arrayDims $ identType ident
          varshape <- mapM evalSubExp vardims
          when (varshape /= valshape) $
            bad $ TypeError $
            "checkPatSizes:\n" ++
            pretty ident ++ " is specified to have shape [" ++
            intercalate "," (zipWith ppDim vardims varshape) ++
            "], but is being bound to value " ++ pretty val ++
            " of shape [" ++ intercalate "," (map pretty valshape) ++ "]."
        checkShape _ = return ()

        ppDim (Constant v) _ = pretty v
        ppDim e            v = pretty e ++ "=" ++ pretty v

lookupVar :: Ident -> FutharkM lore Value
lookupVar (Ident vname _) = do
  val <- asks $ HM.lookup vname . envVtable
  case val of Just val' -> return val'
              Nothing   -> bad $ TypeError $ "lookupVar " ++ textual vname

lookupFun :: Name -> FutharkM lore ([Value] -> FutharkM lore [Value])
lookupFun fname = do
  fun <- asks $ HM.lookup fname . envFtable
  case fun of Just fun' -> return fun'
              Nothing   -> bad $ TypeError $ "lookupFun " ++ textual fname

arrToList :: Value -> FutharkM lore [Value]
arrToList (ArrayVal l _ [_]) =
  return $ map BasicVal $ elems l
arrToList (ArrayVal l bt (_:rowshape)) =
  return [ ArrayVal (listArray (0,rowsize-1) vs) bt rowshape
         | vs <- chunk rowsize $ elems l ]
  where rowsize = product rowshape
arrToList _ = bad $ TypeError "arrToList"

arrayVal :: [Value] -> BasicType -> [Int] -> Value
arrayVal vs bt shape =
  ArrayVal (listArray (0,product shape-1) vs') bt shape
  where vs' = concatMap flatten vs
        flatten (BasicVal bv)      = [bv]
        flatten (ArrayVal arr _ _) = elems arr

arrays :: [Type] -> [[Value]] -> FutharkM lore [Value]
arrays ts vs = zipWithM arrays' ts vs'
  where vs' = case vs of
          [] -> replicate (length ts) []
          _  -> transpose vs
        arrays' rt r = do
          rowshape <- mapM (asInt <=< evalSubExp) $ arrayDims rt
          return $ arrayVal r (elemType rt) $ length r : rowshape
        asInt (BasicVal (IntVal x)) = return x
        asInt _                     = bad $ TypeError "bindVar BindInPlace"

indexArray :: String -> [Int] -> [Int]
           -> FutharkM lore Int
indexArray name shape is
  | and (zipWith (<=) is shape),
    all (0<=) is,
    length is <= length shape =
      let slicesizes = map product $ drop 1 $ tails shape
      in return $ sum $ zipWith (*) is slicesizes
 | otherwise =
      bad $ IndexOutOfBounds name shape is


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
                         (map fparamType valparams)
                         (map valueShape valargs)
          in map (BasicVal . IntVal . fromMaybe 0 .
                  flip HM.lookup shapemap .
                  fparamName)
             shapeparams

-- | As 'runFun', but throws away the trace.
runFunNoTrace :: Lore lore => Name -> [Value] -> Prog lore
              -> Either (InterpreterError lore) [Value]
runFunNoTrace = ((.) . (.) . (.)) fst runFun -- I admit this is just for fun.

runThisFun :: Lore lore => FunDec lore -> [Value] -> FunTable lore
           -> (Either (InterpreterError lore) [Value], Trace)
runThisFun (FunDec fname _ fparams _) args ftable
  | length argtypes == length fparams,
    subtypesOf argtypes paramtypes =
    runFutharkM (evalFuncall fname args) futharkenv
  | otherwise =
    (Left $ InvalidFunctionArguments fname
     (Just (map (toDecl . fparamType) fparams))
     (map toDecl argtypes),
     mempty)
  where argtypes = map (toDecl . (`setUniqueness` Unique) . valueType) args
        paramtypes = map (toDecl . fparamType) fparams
        futharkenv = FutharkEnv { envVtable = HM.empty
                                , envFtable = ftable
                                }

buildFunTable :: Lore lore =>
                 Prog lore -> FunTable lore
buildFunTable = foldl expand builtins . progFunctions
  where -- We assume that the program already passed the type checker, so
        -- we don't check for duplicate definitions.
        expand ftable' (FunDec name _ params body) =
          let fun funargs = binding (zip3 (map fparamIdent params) (repeat BindVar) funargs) $
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
evalSubExp (Var ident)  = lookupVar ident
evalSubExp (Constant v) = return $ BasicVal v

evalBody :: Lore lore => Body lore -> FutharkM lore [Value]

evalBody (Body _ [] (Result es)) =
  mapM evalSubExp es

evalBody (Body lore (Let pat _ e:bnds) res) = do
  v <- evalExp e
  binding (zip3
           (map patElemIdent patElems)
           (map patElemBindage patElems)
           v) $
    evalBody $ Body lore bnds res
  where patElems = patternElements pat

evalExp :: Lore lore => Exp lore -> FutharkM lore [Value]
evalExp (If e1 e2 e3 rettype) = do
  v <- evalSubExp e1
  vs <- case v of BasicVal (LogVal True)  -> evalBody e2
                  BasicVal (LogVal False) -> evalBody e3
                  _                       -> bad $ TypeError "evalExp If"
  return $ valueShapeContext rettype vs ++ vs
evalExp (Apply fname args _)
  | "trace" <- nameToString fname = do
  vs <- mapM (evalSubExp . fst) args
  tell [pretty vs]
  return vs
evalExp (Apply fname args rettype) = do
  args' <- mapM (evalSubExp . fst) args
  vs <- evalFuncall fname args'
  return $ valueShapeContext (retTypeValues rettype) vs ++ vs
evalExp (PrimOp op) = evalPrimOp op
evalExp (LoopOp op) = evalLoopOp op

evalPrimOp :: Lore lore => PrimOp lore -> FutharkM lore [Value]

evalPrimOp (SubExp se) =
  single <$> evalSubExp se

evalPrimOp (ArrayLit es rt) = do
  rowshape <- mapM (asInt <=< evalSubExp) $ arrayDims rt
  single <$> (arrayVal <$>
              mapM evalSubExp es <*>
              pure (elemType rt) <*>
              pure (length es : rowshape))
  where asInt (BasicVal (IntVal x)) = return x
        asInt _                     = bad $ TypeError "evalPrimOp ArrayLit asInt"

evalPrimOp (BinOp Plus e1 e2 Int) = evalIntBinOp (+) e1 e2
evalPrimOp (BinOp Plus e1 e2 Real) = evalRealBinOp (+) e1 e2
evalPrimOp (BinOp Minus e1 e2 Int) = evalIntBinOp (-) e1 e2
evalPrimOp (BinOp Minus e1 e2 Real) = evalRealBinOp (-) e1 e2
evalPrimOp (BinOp Pow e1 e2 Int) = evalIntBinOpM pow e1 e2
  -- Haskell (^) cannot handle negative exponents, so check for that
  -- explicitly.
  where pow x y | y < 0, x == 0 = bad DivisionByZero
                | y < 0         = return $ 1 `div` (x ^ (-y))
                | otherwise     = return $ x ^ y
evalPrimOp (BinOp Pow e1 e2 Real) = evalRealBinOp (**) e1 e2
evalPrimOp (BinOp Times e1 e2 Int) = evalIntBinOp (*) e1 e2
evalPrimOp (BinOp Times e1 e2 Real) = evalRealBinOp (*) e1 e2
evalPrimOp (BinOp Divide e1 e2 Int) = evalIntBinOpM div' e1 e2
  where div' _ 0 = bad DivisionByZero
        div' x y = return $ x `div` y
evalPrimOp (BinOp Mod e1 e2 Int) = evalIntBinOpM mod' e1 e2
  where mod' _ 0 = bad DivisionByZero
        mod' x y = return $ x `mod` y
evalPrimOp (BinOp Divide e1 e2 Real) = evalRealBinOpM div' e1 e2
  where div' _ 0 = bad  DivisionByZero
        div' x y = return $ x / y
evalPrimOp (BinOp ShiftR e1 e2 _) = evalIntBinOp shiftR e1 e2
evalPrimOp (BinOp ShiftL e1 e2 _) = evalIntBinOp shiftL e1 e2
evalPrimOp (BinOp Band e1 e2 _) = evalIntBinOp (.&.) e1 e2
evalPrimOp (BinOp Xor e1 e2 _) = evalIntBinOp xor e1 e2
evalPrimOp (BinOp Bor e1 e2 _) = evalIntBinOp (.|.) e1 e2
evalPrimOp (BinOp LogAnd e1 e2 _) = evalBoolBinOp (&&) e1 e2
evalPrimOp (BinOp LogOr e1 e2 _) = evalBoolBinOp (||) e1 e2

evalPrimOp (BinOp Equal e1 e2 _) = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  return [BasicVal $ LogVal (v1==v2)]

evalPrimOp (BinOp Less e1 e2 _) = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  return [BasicVal $ LogVal (v1<v2)]

evalPrimOp (BinOp Leq e1 e2 _) = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  return [BasicVal $ LogVal (v1<=v2)]

evalPrimOp (BinOp {}) = bad $ TypeError "evalPrimOp Binop"

evalPrimOp (Not e) = do
  v <- evalSubExp e
  case v of BasicVal (LogVal b) -> return [BasicVal $ LogVal (not b)]
            _                     -> bad $ TypeError "evalPrimOp Not"

evalPrimOp (Negate e) = do
  v <- evalSubExp e
  case v of BasicVal (IntVal x)  -> return [BasicVal $ IntVal (-x)]
            BasicVal (RealVal x) -> return [BasicVal $ RealVal (-x)]
            _                      -> bad $ TypeError "evalPrimOp Negate"

evalPrimOp (Index _ ident idxs) = do
  v <- lookupVar ident
  idxs' <- mapM (asInt <=< evalSubExp) idxs
  case v of
    ArrayVal arr bt shape -> do
      flatidx <- indexArray (textual $ identName ident) shape idxs'
      if length idxs' == length shape
        then return [BasicVal $ arr ! flatidx]
        else let resshape = drop (length idxs') shape
                 ressize  = product resshape
             in return [ArrayVal (listArray (0,ressize-1)
                                  [ arr ! (flatidx+i) | i <- [0..ressize-1] ])
                        bt resshape]
    _ -> bad $ TypeError "evalPrimOp Index: ident is not an array"
  where asInt (BasicVal (IntVal x)) = return x
        asInt _                     = bad $ TypeError "evalPrimOp Index asInt"

evalPrimOp (Iota e) = do
  v <- evalSubExp e
  case v of
    BasicVal (IntVal x)
      | x >= 0    ->
        return [ArrayVal (listArray (0,x-1) $ map IntVal [0..x-1])
                Int [x]]
      | otherwise ->
        bad $ NegativeIota x
    _ -> bad $ TypeError "evalPrimOp Iota"

evalPrimOp (Replicate e1 e2) = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  case v1 of
    BasicVal (IntVal x)
      | x >= 0    ->
        case v2 of
          BasicVal bv ->
            return [ArrayVal (listArray (0,x-1) (replicate x bv))
                    (basicValueType bv)
                    [x]]
          ArrayVal arr bt shape ->
            return [ArrayVal (listArray (0,x*product shape-1)
                              (concat $ replicate x $ elems arr))
                    bt (x:shape)]
      | otherwise -> bad $ NegativeReplicate x
    _   -> bad $ TypeError "evalPrimOp Replicate"

evalPrimOp (Scratch bt shape) = do
  shape' <- mapM (asInt <=< evalSubExp) shape
  let nelems = product shape'
      vals = replicate nelems v
  return [ArrayVal (listArray (0,nelems-1) vals) bt shape']
  where v = blankBasicValue bt
        asInt (BasicVal (IntVal x)) = return x
        asInt _                     = bad $ TypeError "evalPrimOp Scratch asInt"

evalPrimOp e@(Reshape _ shapeexp arrexp) = do
  shape <- mapM (asInt <=< evalSubExp) shapeexp
  arr <- lookupVar arrexp
  case arr of
    ArrayVal vs bt oldshape
      | product oldshape == product shape ->
        return $ single $ ArrayVal vs bt shape
      | otherwise ->
        bad $ InvalidArrayShape (PrimOp e) oldshape shape
    _ ->
      bad $ TypeError "Reshape given a non-array argument"
  where asInt (BasicVal (IntVal x)) = return x
        asInt _ = bad $ TypeError "evalPrimOp Reshape asInt"

evalPrimOp (Rearrange _ perm arrexp) =
  single <$> permuteArray perm <$> lookupVar arrexp

evalPrimOp (Split _ sizeexps arrexp) = do
  sizes <- mapM (asInt <=< evalSubExp) sizeexps
  arrval <- lookupVar arrexp
  case arrval of
    (ArrayVal arr bt shape@(outerdim:rowshape))
      | all (0<=) sizes && sum sizes <= outerdim ->
        let rowsize = product rowshape
        in return $ zipWith (\beg num -> ArrayVal (listArray (0,rowsize*num-1)
                                                   $ drop (rowsize*beg) (elems arr))
                                         bt (num:rowshape))
                    (scanl (+) 0 sizes) sizes
      | otherwise        -> bad $ SplitOutOfBounds (pretty arrexp) shape sizes
    _ -> bad $ TypeError "evalPrimOp Split"
  where asInt (BasicVal (IntVal x)) = return x
        asInt _ = bad $ TypeError "evalPrimOp Split asInt"

evalPrimOp (Concat _ arr1exp arr2exps _) = do
  arr1  <- lookupVar arr1exp
  arr2s <- mapM lookupVar arr2exps

  case arr1 of
    ArrayVal arr1' bt (outerdim1:rowshape1) -> do
        (res,resouter,resshape) <- foldM concatArrVals (arr1',outerdim1,rowshape1) arr2s
        return [ArrayVal res bt (resouter:resshape)]
    _ -> bad $ TypeError "evalPrimOp Concat"
  where
    concatArrVals (acc,outerdim,rowshape) (ArrayVal arr2 _ (outerdim2:rowshape2)) =
        if rowshape == rowshape2
        then let nelems = (outerdim+outerdim2) * product rowshape
             in return  ( listArray (0,nelems-1) (elems acc ++ elems arr2)
                        , outerdim+outerdim2
                        , rowshape
                        )
        else bad $ TypeError "irregular arguments to concat"
    concatArrVals _ _ = bad $ TypeError "evalPrimOp Concat"

evalPrimOp (Copy e) = single <$> evalSubExp e

evalPrimOp (Assert e loc) = do
  v <- evalSubExp e
  case v of BasicVal (LogVal True) ->
              return [BasicVal Checked]
            _ ->
              bad $ AssertFailed loc

evalPrimOp (Partition _ n flags arr) = do
  flags_elems <- arrToList =<< lookupVar flags
  arrv <- lookupVar arr
  arr_elems <- arrToList arrv
  partitions <- partitionArray flags_elems arr_elems
  return $
    map (BasicVal . IntVal . length) partitions ++
    [arrayVal (concat partitions) et (valueShape arrv)]
  where et = elemType $ identType arr
        partitionArray flagsv arrv =
          map reverse <$>
          foldM divide (replicate n []) (zip flagsv arrv)

        divide partitions (BasicVal (IntVal i),v)
          | i < 0 =
            bad $ TypeError $ "Partition key " ++ show i ++ " is negative"
          | i < n =
            return $ take i partitions ++ [v : (partitions!!i)] ++ drop (i+1) partitions
          | otherwise =
            return partitions

        divide _ (i,_) =
          bad $ TypeError $ "Partition key " ++ pretty i ++ " is not an integer."

-- Alloc is not used in the interpreter, so just return whatever
evalPrimOp (Alloc se) =
  single <$> evalSubExp se

evalLoopOp :: forall lore . Lore lore => LoopOp lore -> FutharkM lore [Value]

evalLoopOp (DoLoop respat merge (ForLoop loopvar boundexp) loopbody) = do
  bound <- evalSubExp boundexp
  mergestart <- mapM evalSubExp mergeexp
  case bound of
    BasicVal (IntVal n) -> do
      vs <- foldM iteration mergestart [0..n-1]
      binding (zip3 (map fparamIdent mergepat) (repeat BindVar) vs) $
        mapM lookupVar $
        loopResultContext (representative :: lore) respat mergepat ++ respat
    _ -> bad $ TypeError "evalBody DoLoop for"
  where (mergepat, mergeexp) = unzip merge
        iteration mergeval i =
          binding [(loopvar, BindVar, BasicVal $ IntVal i)] $
            binding (zip3 (map fparamIdent mergepat) (repeat BindVar) mergeval) $
              evalBody loopbody

evalLoopOp (DoLoop respat merge (WhileLoop cond) loopbody) = do
  mergestart <- mapM evalSubExp mergeexp
  iteration mergestart
  where (mergepat, mergeexp) = unzip merge
        iteration mergeval =
          binding (zip3 (map fparamIdent mergepat) (repeat BindVar) mergeval) $ do
            condv <- lookupVar cond
            case condv of
              BasicVal (LogVal False) ->
                mapM lookupVar $
                loopResultContext (representative :: lore) respat mergepat ++ respat
              BasicVal (LogVal True) ->
                iteration =<< evalBody loopbody
              _ ->
                bad $ TypeError "evalBody DoLoop while"

evalLoopOp (Map _ fun arrexps) = do
  vss <- mapM (arrToList <=< lookupVar) arrexps
  vss' <- mapM (applyLambda fun) $ transpose vss
  arrays (lambdaReturnType fun) vss'

evalLoopOp (ConcatMap _ fun inputs) = do
  inputs' <- mapM (mapM lookupVar) inputs
  vss <- mapM (mapM asArray <=< applyConcatMapLambda fun) inputs'
  innershapes <- mapM (mapM (asInt <=< evalSubExp) . arrayDims) $
                 lambdaReturnType fun
  let numTs = length $ lambdaReturnType fun
      emptyArray :: (Array Int BasicValue, Int)
      emptyArray = (listArray (0,-1) [], 0)
      concatArrays (arr1,n1) (arr2,n2) =
        (listArray (0,n1+n2-1) (elems arr1 ++ elems arr2), n1+n2)
      arrs = foldl (zipWith concatArrays) (replicate numTs emptyArray) vss
      (ctx,vs) = unzip
                 [ (BasicVal $ IntVal n,
                    ArrayVal arr (elemType t) (n:innershape))
                 | (innershape,(arr,n),t) <-
                      zip3 innershapes arrs $ lambdaReturnType fun ]
  return $ ctx ++ vs
  where asInt (BasicVal (IntVal x)) = return x
        asInt _                     = bad $ TypeError "evalLoopOp asInt"
        asArray (ArrayVal a _ (n:_)) = return (a, n)
        asArray _                    = bad $ TypeError "evalLoopOp asArray"

evalLoopOp (Reduce _ fun inputs) = do
  let (accexps, arrexps) = unzip inputs
  startaccs <- mapM evalSubExp accexps
  vss <- mapM (arrToList <=< lookupVar) arrexps
  let foldfun acc x = applyLambda fun $ acc ++ x
  foldM foldfun startaccs (transpose vss)

evalLoopOp (Scan _ fun inputs) = do
  let (accexps, arrexps) = unzip inputs
  startvals <- mapM evalSubExp accexps
  vss <- mapM (arrToList <=< lookupVar) arrexps
  (acc, vals') <- foldM scanfun (startvals, []) $ transpose vss
  arrays (map valueType acc) $ reverse vals'
    where scanfun (acc, l) x = do
            acc' <- applyLambda fun $ acc ++ x
            return (acc', acc' : l)

evalLoopOp (Redomap _ _ innerfun accexp arrexps) = do
  startaccs <- mapM evalSubExp accexp
  vss <- mapM (arrToList <=< lookupVar) arrexps
  if res_len == acc_len
  then foldM foldfun startaccs $ transpose vss
  else do let startaccs'= (startaccs, replicate (res_len - acc_len) [])
          (acc_res, arr_res) <- foldM foldfun' startaccs' $ transpose vss
          arr_res_fut <- arrays lam_ret_arr_tp $ transpose $ map reverse arr_res
          return $ acc_res ++ arr_res_fut
    where
        lam_ret_tp     = lambdaReturnType innerfun
        res_len        = length lam_ret_tp
        acc_len        = length accexp
        lam_ret_arr_tp = drop acc_len lam_ret_tp
        foldfun  acc x = applyLambda innerfun $ acc ++ x
        foldfun' (acc,arr) x = do
            res_lam <- applyLambda innerfun $ acc ++ x
            let res_acc = take acc_len res_lam
                res_arr = drop acc_len res_lam
                acc_arr = zipWith (:) res_arr arr
            return (res_acc, acc_arr)

evalLoopOp (Stream _ accs arrs elam) = do
  accvals <- mapM evalSubExp accs
  arrvals <- mapM lookupVar  arrs
  let (ExtLambda elam_params elam_body elam_rtp) = elam
  let fun funargs = binding (zip3 elam_params (repeat BindVar) funargs) $
                            evalBody elam_body
  -- get the outersize of the input array(s), and use it as chunk!
  let (ArrayVal _ _ (outersize:_)) = head arrvals
  let (chunkval, ival) = (BasicVal $ IntVal outersize, BasicVal $ IntVal 0)
  vs <- fun (chunkval:ival:accvals++arrvals)
  return $ valueShapeContext elam_rtp vs ++ vs

evalFuncall :: Name -> [Value] -> FutharkM lore [Value]
evalFuncall fname args = do
  fun <- lookupFun fname
  fun args

evalIntBinOp :: (Int -> Int -> Int) -> SubExp -> SubExp
             -> FutharkM lore [Value]
evalIntBinOp op = evalIntBinOpM $ \x y -> return $ op x y

evalIntBinOpM :: (Int -> Int -> FutharkM lore Int)
              -> SubExp
              -> SubExp
              -> FutharkM lore [Value]
evalIntBinOpM op e1 e2 = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  case (v1, v2) of
    (BasicVal (IntVal x), BasicVal (IntVal y)) -> do
      result <- op x y
      return [BasicVal $ IntVal result]
    _ ->
      bad $ TypeError "evalIntBinOpM"

evalRealBinOp :: (Double -> Double -> Double) -> SubExp -> SubExp
             -> FutharkM lore [Value]
evalRealBinOp op = evalRealBinOpM $ \x y -> return $ op x y

evalRealBinOpM :: (Double -> Double -> FutharkM lore Double)
               -> SubExp
               -> SubExp
               -> FutharkM lore [Value]
evalRealBinOpM op e1 e2 = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  case (v1, v2) of
    (BasicVal (RealVal x), BasicVal (RealVal y)) -> do
      result <- op x y
      return [BasicVal $ RealVal result]
    _ ->
      bad $ TypeError $ "evalRealBinOpM " ++ pretty v1 ++ " " ++ pretty v2

evalBoolBinOp :: (Bool -> Bool -> Bool) -> SubExp -> SubExp -> FutharkM lore [Value]
evalBoolBinOp op e1 e2 = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  case (v1, v2) of
    (BasicVal (LogVal x), BasicVal (LogVal y)) ->
      return [BasicVal $ LogVal (op x y)]
    _ ->
      bad $ TypeError $ "evalBoolBinOp " ++ pretty v1 ++ " " ++ pretty v2

applyLambda :: Lore lore => Lambda lore -> [Value] -> FutharkM lore [Value]
applyLambda (Lambda params body rettype) args =
  do v <- binding (zip3 params (repeat BindVar) args) $ evalBody body
     checkReturnShapes (staticShapes rettype) v
     return v

applyConcatMapLambda :: Lore lore => Lambda lore -> [Value] -> FutharkM lore [Value]
applyConcatMapLambda (Lambda params body rettype) valargs = do
  v <- binding (zip3 params (repeat BindVar) $ shapes ++ valargs) $
       evalBody body
  let rettype' = [ arrayOf t (ExtShape [Ext 0]) $ uniqueness t
                 | t <- staticShapes rettype ]
  checkReturnShapes rettype' v
  return v
  where shapes =
          let (shapeparams, valparams) =
                splitAt (length params - length valargs) params
              shapemap = shapeMapping'
                         (map identType valparams)
                         (map valueShape valargs)
          in map (BasicVal . IntVal . fromMaybe 0 .
                  flip HM.lookup shapemap .
                  identName)
             shapeparams

checkReturnShapes :: [ExtType] -> [Value] -> FutharkM lore ()
checkReturnShapes = zipWithM_ checkShape
  where checkShape t val = do
          let valshape = map (BasicVal . IntVal) $ valueShape val
              retdims = extShapeDims $ arrayShape t
              evalExtDim (Free se) = do v <- evalSubExp se
                                        return $ Just (se, v)
              evalExtDim (Ext _)   = return Nothing
              matches (Just (_, v1), v2) = v1 == v2
              matches (Nothing,      _)  = True
          retshape <- mapM evalExtDim retdims
          unless (all matches $ zip retshape valshape) $
            bad $ TypeError $
            "checkReturnShapes:\n" ++
            "Return type specifies shape [" ++
            intercalate "," (map ppDim retshape) ++
            "], but returned value is of shape [" ++
            intercalate "," (map pretty valshape) ++ "]."

        ppDim (Just (Constant v, _)) = pretty v
        ppDim (Just (Var e,      v)) = pretty e ++ "=" ++ pretty v
        ppDim Nothing                = "?"
