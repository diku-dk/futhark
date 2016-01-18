{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Word

import Prelude

import Futhark.Representation.SOACS
import Futhark.Util

-- | An error happened during execution, and this is why.
data InterpreterError =
      MissingEntryPoint Name
      -- ^ The specified start function does not exist.
    | InvalidFunctionArguments Name (Maybe [TypeBase Rank NoUniqueness]) [TypeBase Rank NoUniqueness]
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
    | InvalidArrayShape Exp [Int] [Int]
      -- ^ First @Int@ is old shape, second is attempted new shape.
    | ZipError [Int]
      -- ^ The arguments to @zip@ were of different lengths.
    | AssertFailed SrcLoc
      -- ^ Assertion failed at this location.
    | TypeError String
      -- ^ Some value was of an unexpected type.
    | DivisionByZero
      -- ^ Attempted to divide by zero.

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

type FunTable = HM.HashMap Name ([Value] -> FutharkM [Value])

type VTable = HM.HashMap VName Value

data FutharkEnv = FutharkEnv { envVtable :: VTable
                             , envFtable :: FunTable
                             }

-- | A list of the prettyprinted values that were passed to @trace@
type Trace = [String]

newtype FutharkM a = FutharkM (ReaderT FutharkEnv
                               (ExceptT InterpreterError
                                (Writer Trace)) a)
  deriving (Monad, Applicative, Functor,
            MonadReader FutharkEnv,
            MonadWriter Trace)

runFutharkM :: FutharkM a -> FutharkEnv
            -> (Either InterpreterError a, Trace)
runFutharkM (FutharkM m) env = runWriter $ runExceptT $ runReaderT m env

bad :: InterpreterError -> FutharkM a
bad = FutharkM . throwError

asRational :: String -> Value -> FutharkM Rational
asRational _ (PrimVal (FloatValue (Float32Value v))) = return $ toRational v
asRational _ (PrimVal (FloatValue (Float64Value v))) = return $ toRational v
asRational w _ = bad $ TypeError $ w ++ " asInteger"

asIntValue :: String -> Value -> FutharkM IntValue
asIntValue _ (PrimVal (IntValue v)) = return v
asIntValue w _ = bad $ TypeError $ w ++ " asIntValue"

asInt32 :: String -> Value -> FutharkM Int32
asInt32 _ (PrimVal (IntValue (Int32Value v))) = return v
asInt32 w _ = bad $ TypeError $ w ++ " asInt32"

asInt :: String -> Value -> FutharkM Int
asInt _ (PrimVal (IntValue (Int32Value v))) = return $ fromIntegral v
asInt w _ = bad $ TypeError $ w ++ " asInt"

bindVar :: Bindage -> Value
        -> FutharkM Value

bindVar BindVar val =
  return val

bindVar (BindInPlace _ src is) val = do
  srcv <- lookupVar src
  is' <- mapM (asInt "bindInPlace" <=< evalSubExp) is
  case srcv of
    ArrayVal arr bt shape -> do
      flatidx <- indexArray (textual src) shape is'
      if length is' == length shape then
        case val of
          PrimVal bv ->
            return $ ArrayVal (arr // [(flatidx, bv)]) bt shape
          _ ->
            bad $ TypeError "bindVar BindInPlace, full indices given, but replacement value is not a prim value"
      else
        case val of
          ArrayVal valarr _ valshape ->
            let updates =
                  [ (flatidx + i, valarr ! i) | i <- [0..product valshape-1] ]
            in return $ ArrayVal (arr // updates) bt shape
          PrimVal _ ->
            bad $ TypeError "bindVar BindInPlace, incomplete indices given, but replacement value is not array"
    _ ->
      bad $ TypeError "bindVar BindInPlace, source is not array"

bindVars :: [(Ident, Bindage, Value)]
         -> FutharkM VTable
bindVars bnds = do
  let (idents, bindages, vals) = unzip3 bnds
  HM.fromList . zip (map identName idents) <$>
    zipWithM bindVar bindages vals

binding :: [(Ident, Bindage, Value)]
        -> FutharkM a
        -> FutharkM a
binding bnds m = do
  vtable <- bindVars bnds
  local (extendVtable vtable) $ do
    checkBoundShapes bnds
    m
  where extendVtable vtable env = env { envVtable = vtable <> envVtable env }

        checkBoundShapes = mapM_ checkShape
        checkShape (ident, BindVar, val) = do
          let valshape = map (PrimVal . value) $ valueShape val
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

lookupVar :: VName -> FutharkM Value
lookupVar vname = do
  val <- asks $ HM.lookup vname . envVtable
  case val of Just val' -> return val'
              Nothing   -> bad $ TypeError $ "lookupVar " ++ textual vname

lookupFun :: Name -> FutharkM ([Value] -> FutharkM [Value])
lookupFun fname = do
  fun <- asks $ HM.lookup fname . envFtable
  case fun of Just fun' -> return fun'
              Nothing   -> bad $ TypeError $ "lookupFun " ++ textual fname

arrToList :: Value -> FutharkM [Value]
arrToList (ArrayVal l _ [_]) =
  return $ map PrimVal $ elems l
arrToList (ArrayVal l bt (_:rowshape)) =
  return [ ArrayVal (listArray (0,rowsize-1) vs) bt rowshape
         | vs <- chunk rowsize $ elems l ]
  where rowsize = product rowshape
arrToList _ = bad $ TypeError "arrToList"

arrayVal :: [Value] -> PrimType -> [Int] -> Value
arrayVal vs bt shape =
  ArrayVal (listArray (0,product shape-1) vs') bt shape
  where vs' = concatMap flatten vs
        flatten (PrimVal bv)      = [bv]
        flatten (ArrayVal arr _ _) = elems arr

arrays :: [Type] -> [[Value]] -> FutharkM [Value]
arrays ts vs = zipWithM arrays' ts vs'
  where vs' = case vs of
          [] -> replicate (length ts) []
          _  -> transpose vs
        arrays' rt r = do
          rowshape <- mapM (asInt32 "arrays" <=< evalSubExp) $ arrayDims rt
          return $ arrayVal r (elemType rt) $ length r : map fromIntegral rowshape

soacArrays :: SubExp -> [VName] -> FutharkM [[Value]]
soacArrays w [] = do
  w' <- asInt32 "soacArrays" =<< evalSubExp w
  return $ genericReplicate w' []
soacArrays _ names = transpose <$> mapM (arrToList <=< lookupVar) names

indexArray :: String -> [Int] -> [Int]
           -> FutharkM Int
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
runFun :: Name -> [Value] -> Prog
       -> (Either InterpreterError [Value], Trace)
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
runFunWithShapes :: Name -> [Value] -> Prog
                 -> (Either InterpreterError [Value], Trace)
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
                         (map paramType valparams)
                         (map valueShape valargs)
          in map (PrimVal . intvalue Int32 . fromIntegral . fromMaybe 0 .
                  flip HM.lookup shapemap .
                  paramName)
             shapeparams

-- | As 'runFun', but throws away the trace.
runFunNoTrace :: Name -> [Value] -> Prog
              -> Either InterpreterError [Value]
runFunNoTrace = ((.) . (.) . (.)) fst runFun -- I admit this is just for fun.

runThisFun :: FunDec -> [Value] -> FunTable
           -> (Either InterpreterError [Value], Trace)
runThisFun (FunDec fname _ fparams _) args ftable
  | argtypes == paramtypes =
    runFutharkM (evalFuncall fname args) futharkenv
  | otherwise =
    (Left $ InvalidFunctionArguments fname
     (Just paramtypes)
     argtypes,
     mempty)
  where argtypes = map (rankShaped . valueType) args
        paramtypes = map (rankShaped . paramType) fparams
        futharkenv = FutharkEnv { envVtable = HM.empty
                                , envFtable = ftable
                                }

buildFunTable :: Prog -> FunTable
buildFunTable = foldl expand builtins . progFunctions
  where -- We assume that the program already passed the type checker, so
        -- we don't check for duplicate definitions.
        expand ftable' (FunDec name _ params body) =
          let fun funargs = binding (zip3 (map paramIdent params) (repeat BindVar) funargs) $
                            evalBody body
          in HM.insert name fun ftable'

--------------------------------------------
--------------------------------------------
------------- BUILTIN FUNCTIONS ------------
--------------------------------------------
--------------------------------------------

builtins :: HM.HashMap Name ([Value] -> FutharkM [Value])
builtins = HM.fromList $ map namify
           [("trunc32", builtin "trunc32")
           ,("sqrt32", builtin "sqrt32")
           ,("log32", builtin "log32")
           ,("exp32", builtin "exp32")

           ,("trunc64", builtin "trunc64")
           ,("sqrt64", builtin "sqrt64")
           ,("log64", builtin "log64")
           ,("exp64", builtin "exp64")]
  where namify (k,v) = (nameFromString k, v)

builtin :: String -> [Value] -> FutharkM [Value]
builtin "trunc32" [PrimVal (FloatValue (Float32Value x))] =
  return [PrimVal $ IntValue $ Int32Value $ truncate x]
builtin "sqrt32" [PrimVal (FloatValue (Float32Value x))] =
  return [PrimVal $ FloatValue $ Float32Value $ sqrt x]
builtin "log32" [PrimVal (FloatValue (Float32Value x))] =
  return [PrimVal $ FloatValue $ Float32Value $ log x]
builtin "exp32" [PrimVal (FloatValue (Float32Value x))] =
  return [PrimVal $ FloatValue $ Float32Value $ exp x]
builtin "trunc64" [PrimVal (FloatValue (Float64Value x))] =
  return [PrimVal $ IntValue $ Int32Value $ truncate x]
builtin "sqrt64" [PrimVal (FloatValue (Float64Value x))] =
  return [PrimVal $ FloatValue $ Float64Value $ sqrt x]
builtin "log64" [PrimVal (FloatValue (Float64Value x))] =
  return [PrimVal $ FloatValue $ Float64Value $ log x]
builtin "exp64" [PrimVal (FloatValue (Float64Value x))] =
  return [PrimVal $ FloatValue $ Float64Value $ exp x]
builtin fname args =
  bad $ InvalidFunctionArguments (nameFromString fname) Nothing $
        map (rankShaped . valueType) args

single :: Value -> [Value]
single v = [v]

evalSubExp :: SubExp -> FutharkM Value
evalSubExp (Var ident)  = lookupVar ident
evalSubExp (Constant v) = return $ PrimVal v

evalBody :: Body -> FutharkM [Value]

evalBody (Body _ [] es) =
  mapM evalSubExp es

evalBody (Body () (Let pat _ e:bnds) res) = do
  v <- evalExp e
  binding (zip3
           (map patElemIdent patElems)
           (map patElemBindage patElems)
           v) $
    evalBody $ Body () bnds res
  where patElems = patternElements pat

evalExp :: Exp -> FutharkM [Value]
evalExp (If e1 e2 e3 rettype) = do
  v <- evalSubExp e1
  vs <- case v of PrimVal (BoolValue True)  -> evalBody e2
                  PrimVal (BoolValue False) -> evalBody e3
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
evalExp (Op op) = evalSOAC op

evalPrimOp :: PrimOp -> FutharkM [Value]

evalPrimOp (SubExp se) =
  single <$> evalSubExp se

evalPrimOp (ArrayLit es rt) = do
  rowshape <- mapM (asInt "evalPrimOp ArrayLit" <=< evalSubExp) $ arrayDims rt
  single <$> (arrayVal <$>
              mapM evalSubExp es <*>
              pure (elemType rt) <*>
              pure (length es : rowshape))

evalPrimOp (BinOp Add{} e1 e2) = evalIntBinOp (+) e1 e2
evalPrimOp (BinOp FAdd{} e1 e2) = evalFloatBinOp (+) e1 e2
evalPrimOp (BinOp Sub{} e1 e2) = evalIntBinOp (-) e1 e2
evalPrimOp (BinOp FSub{} e1 e2) = evalFloatBinOp (-) e1 e2
evalPrimOp (BinOp SPow{} e1 e2) = evalIntBinOpM pow e1 e2
  -- Haskell (^) cannot handle negative exponents, so check for that
  -- explicitly.
  where pow x y | y < 0, x == 0 = bad DivisionByZero
                | y < 0         = return $ 1 `div` (x ^ (-y))
                | otherwise     = return $ x ^ y
evalPrimOp (BinOp FPow{} e1 e2) = evalFloatBinOp (**) e1 e2
evalPrimOp (BinOp Mul{} e1 e2) = evalIntBinOp (*) e1 e2
evalPrimOp (BinOp FMul{} e1 e2) = evalFloatBinOp (*) e1 e2
evalPrimOp (BinOp SDiv{} e1 e2) = evalIntBinOpM div' e1 e2
  where div' _ 0 = bad DivisionByZero
        div' x y = return $ x `div` y
evalPrimOp (BinOp SMod{} e1 e2) = evalIntBinOpM mod' e1 e2
  where mod' _ 0 = bad DivisionByZero
        mod' x y = return $ x `mod` y
evalPrimOp (BinOp SQuot{} e1 e2) = evalIntBinOpM quot' e1 e2
  where quot' _ 0 = bad DivisionByZero
        quot' x y = return $ x `quot` y
evalPrimOp (BinOp SRem{} e1 e2) = evalIntBinOpM rem' e1 e2
  where rem' _ 0 = bad DivisionByZero
        rem' x y = return $ x `rem` y
evalPrimOp (BinOp UDiv{} _ _ ) = bad $ TypeError "UDiv not implemented in interpreter yet."
evalPrimOp (BinOp UMod{} _ _ ) = bad $ TypeError "UDiv not implemented in interpreter yet."
evalPrimOp (BinOp FDiv{} e1 e2) = evalFloatBinOpM div' e1 e2
  where div' _ 0 = bad  DivisionByZero
        div' x y = return $ x / y
evalPrimOp (BinOp Shl{} e1 e2) = evalIntBinOp shiftL' e1 e2
  where shiftL' x = shiftL x . fromIntegral
evalPrimOp (BinOp AShr{} e1 e2) = evalIntBinOp shiftR' e1 e2
  where shiftR' x = shiftR x . fromIntegral
evalPrimOp (BinOp LShr{} _ _ ) = bad $ TypeError "LShr not implemented in interpreter yet."
evalPrimOp (BinOp And{} e1 e2) = evalIntBinOp (.&.) e1 e2
evalPrimOp (BinOp Xor{} e1 e2) = evalIntBinOp xor e1 e2
evalPrimOp (BinOp Or{} e1 e2) = evalIntBinOp (.|.) e1 e2
evalPrimOp (BinOp LogAnd e1 e2) = evalBoolBinOp (&&) e1 e2
evalPrimOp (BinOp LogOr e1 e2) = evalBoolBinOp (||) e1 e2

evalPrimOp (CmpOp CmpEq e1 e2) = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  return [PrimVal $ BoolValue (v1==v2)]

evalPrimOp (CmpOp CmpSlt{} e1 e2) = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  return [PrimVal $ BoolValue (v1<v2)]

evalPrimOp (CmpOp CmpSle{} e1 e2) = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  return [PrimVal $ BoolValue (v1<=v2)]

evalPrimOp (CmpOp FCmpLt{} e1 e2) = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  return [PrimVal $ BoolValue (v1<v2)]

evalPrimOp (CmpOp FCmpLe{} e1 e2) = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  return [PrimVal $ BoolValue (v1<=v2)]

evalPrimOp (CmpOp CmpUlt{} _ _) =
  bad $ TypeError "CmpUlt not implemented in interpreter yet."

evalPrimOp (CmpOp CmpUle{} _ _) =
  bad $ TypeError "CmpUlt not implemented in interpreter yet."

evalPrimOp (ConvOp (Trunc _ to) x) =
  -- Zero-extension with a smaller type is the same as truncation.
  single <$> PrimVal <$> intvalue to <$>
  valueInt <$> (asIntValue "Trunc" =<< evalSubExp x)

evalPrimOp (ConvOp (ZExt _ to) x) =
  single <$> PrimVal <$> IntValue <$>
  (zeroExtend <$> (asIntValue "ZExt" =<< evalSubExp x) <*> pure to)

evalPrimOp (ConvOp (SExt _ to) x) =
  single <$> PrimVal <$> IntValue <$>
  (signExtend <$> (asIntValue "SExt" =<< evalSubExp x) <*> pure to)

evalPrimOp (ConvOp (FPTrunc _ to) x) =
  single <$> PrimVal <$> FloatValue <$>
  floatvalue to <$> (asRational "FPTrunc" =<< evalSubExp x)

evalPrimOp (ConvOp (FPExt _ to) x) =
  single <$> PrimVal <$> FloatValue <$>
  floatvalue to <$> (asRational "FPExt" =<< evalSubExp x)

evalPrimOp (ConvOp (FPToSI _ t) x) =
  single <$> PrimVal <$>
  intvalue t <$> truncate <$> (asRational "FPToSI" =<< evalSubExp x)

evalPrimOp (ConvOp (FPToUI _ t) x) =
  single <$> PrimVal <$> intvalue t <$> toInteger <$>
  (truncate <$> (asRational "FPToSI" =<< evalSubExp x) :: FutharkM Word64)

evalPrimOp (ConvOp (UIToFP _ t) x) =
  single <$> PrimVal <$> FloatValue <$>
  (uintToFloat <$> (asIntValue "UIToFP" =<< evalSubExp x) <*> pure t)

evalPrimOp (ConvOp (SIToFP _ t) x) =
  single <$> PrimVal <$> FloatValue <$>
  (intToFloat <$> (asIntValue "SIToFP" =<< evalSubExp x) <*> pure t)

evalPrimOp (UnOp Not e) = do
  v <- evalSubExp e
  case v of PrimVal (BoolValue b) -> return [PrimVal $ BoolValue (not b)]
            _                     -> bad $ TypeError "evalPrimOp Not"

evalPrimOp (UnOp Complement{} e) = do
  v <- evalSubExp e
  case v of PrimVal x -> single <$> PrimVal <$> intUnOp (return . complement) x
            _         -> bad $ TypeError "evalPrimOp Not"

evalPrimOp (UnOp Abs{} e) = do
  v <- evalSubExp e
  case v of PrimVal x -> single <$> PrimVal <$> numUnOp (return . abs) x
            _         -> bad $ TypeError "evalPrimOp Abs"

evalPrimOp (UnOp FAbs{} e) = do
  v <- evalSubExp e
  case v of PrimVal x -> single <$> PrimVal <$> numUnOp (return . abs) x
            _         -> bad $ TypeError "evalPrimOp FAbs"

evalPrimOp (UnOp Signum{} e) = do
  v <- evalSubExp e
  case v of PrimVal x -> single <$> PrimVal <$> intUnOp (return . signum) x
            _         -> bad $ TypeError "evalPrimOp Signum"

evalPrimOp (Index _ ident idxs) = do
  v <- lookupVar ident
  idxs' <- mapM (asInt "Index" <=< evalSubExp) idxs
  case v of
    ArrayVal arr bt shape -> do
      flatidx <- indexArray (textual ident) shape idxs'
      if length idxs' == length shape
        then return [PrimVal $ arr ! flatidx]
        else let resshape = drop (length idxs') shape
                 ressize  = product resshape
             in return [ArrayVal (listArray (0,ressize-1)
                                  [ arr ! (flatidx+i) | i <- [0..ressize-1] ])
                        bt resshape]
    _ -> bad $ TypeError "evalPrimOp Index: ident is not an array"

evalPrimOp (Iota e) = do
  v <- evalSubExp e
  case v of
    PrimVal (IntValue (Int32Value x))
      | x >= 0    ->
        return [ArrayVal (listArray (0,fromIntegral x-1) $ map value [0..x-1])
                int32 [fromIntegral x]]
      | otherwise ->
        bad $ NegativeIota $ fromIntegral x
    _ -> bad $ TypeError "evalPrimOp Iota"

evalPrimOp (Replicate e1 e2) = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  case v1 of
    PrimVal (IntValue (Int32Value x))
      | x >= 0    ->
        case v2 of
          PrimVal bv ->
            return [ArrayVal (listArray (0,fromIntegral x-1) (genericReplicate x bv))
                    (primValueType bv)
                    [fromIntegral x]]
          ArrayVal arr bt shape ->
            return [ArrayVal (listArray (0,fromIntegral x*product shape-1)
                              (concat $ genericReplicate x $ elems arr))
                    bt (fromIntegral x:shape)]
      | otherwise -> bad $ NegativeReplicate $ fromIntegral x
    _   -> bad $ TypeError "evalPrimOp Replicate"

evalPrimOp (Scratch bt shape) = do
  shape' <- mapM (asInt "evalPrimOp Scratch" <=< evalSubExp) shape
  let nelems = product shape'
      vals = genericReplicate nelems v
  return [ArrayVal (listArray (0,fromIntegral nelems-1) vals) bt shape']
  where v = blankPrimValue bt

evalPrimOp e@(Reshape _ shapeexp arrexp) = do
  shape <- mapM (asInt "evalPrimOp Reshape" <=< evalSubExp) $ newDims shapeexp
  arr <- lookupVar arrexp
  case arr of
    ArrayVal vs bt oldshape
      | product oldshape == product shape ->
        return $ single $ ArrayVal vs bt shape
      | otherwise ->
        bad $ InvalidArrayShape (PrimOp e) oldshape shape
    _ ->
      bad $ TypeError "Reshape given a non-array argument"

evalPrimOp (Rearrange _ perm arrexp) =
  single <$> permuteArray perm <$> lookupVar arrexp

evalPrimOp (Stripe _ stride arrexp) =
  single <$> (stripeArray <$> (asInt "evalPrimOp Stripe" =<< evalSubExp stride) <*> lookupVar arrexp)

evalPrimOp (Unstripe _ stride arrexp) =
  single <$> (unstripeArray <$> (asInt "evalPrimOp Unstripe" =<< evalSubExp stride) <*> lookupVar arrexp)

evalPrimOp (Split _ sizeexps arrexp) = do
  sizes <- mapM (asInt "evalPrimOp Split" <=< evalSubExp) sizeexps
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

evalPrimOp (Copy v) = single <$> lookupVar v

evalPrimOp (Assert e loc) = do
  v <- evalSubExp e
  case v of PrimVal (BoolValue True) ->
              return [PrimVal Checked]
            _ ->
              bad $ AssertFailed loc

evalPrimOp (Partition _ n flags arrs) = do
  flags_elems <- arrToList =<< lookupVar flags
  arrvs <- mapM lookupVar arrs
  let ets = map (elemType . valueType) arrvs
  arrs_elems <- mapM arrToList arrvs
  partitions <- mapM (partitionArray flags_elems) arrs_elems
  return $
    case partitions of
      [] ->
        replicate n $ PrimVal $ intvalue Int32 0
      first_part:_ ->
        map (PrimVal . intvalue Int32 . genericLength) first_part ++
        [arrayVal (concat part) et (valueShape arrv) |
         (part,et,arrv) <- zip3 partitions ets arrvs]
  where partitionArray flagsv arrv =
          map reverse <$>
          foldM divide (replicate n []) (zip flagsv arrv)

        divide partitions (PrimVal (IntValue (Int32Value i)), v)
          | i' < 0 =
            bad $ TypeError $ "Partition key " ++ show i ++ " is negative"
          | i' < n =
            return $ genericTake i partitions ++ [v : (partitions!!i')] ++ genericDrop (i'+1) partitions
          | otherwise =
            return partitions
          where i' = fromIntegral i

        divide _ (i,_) =
          bad $ TypeError $ "Partition key " ++ pretty i ++ " is not an integer."

evalLoopOp :: LoopOp -> FutharkM [Value]

evalLoopOp (DoLoop respat merge (ForLoop loopvar boundexp) loopbody) = do
  bound <- evalSubExp boundexp
  mergestart <- mapM evalSubExp mergeexp
  case bound of
    PrimVal (IntValue (Int32Value n)) -> do
      vs <- foldM iteration mergestart [0..n-1]
      binding (zip3 (map paramIdent mergepat) (repeat BindVar) vs) $
        mapM lookupVar $
        loopResultContext (representative :: SOACS) respat mergepat ++ respat
    _ -> bad $ TypeError "evalBody DoLoop for"
  where (mergepat, mergeexp) = unzip merge
        iteration mergeval i =
          binding [(Ident loopvar $ Prim int32, BindVar, PrimVal $ value i)] $
            binding (zip3 (map paramIdent mergepat) (repeat BindVar) mergeval) $
              evalBody loopbody

evalLoopOp (DoLoop respat merge (WhileLoop cond) loopbody) = do
  mergestart <- mapM evalSubExp mergeexp
  iteration mergestart
  where (mergepat, mergeexp) = unzip merge
        iteration mergeval =
          binding (zip3 (map paramIdent mergepat) (repeat BindVar) mergeval) $ do
            condv <- lookupVar cond
            case condv of
              PrimVal (BoolValue False) ->
                mapM lookupVar $
                loopResultContext (representative :: SOACS) respat mergepat ++
                respat
              PrimVal (BoolValue True) ->
                iteration =<< evalBody loopbody
              _ ->
                bad $ TypeError "evalBody DoLoop while"

evalSOAC :: SOAC SOACS -> FutharkM [Value]

evalSOAC (Stream _ _ form elam arrs _) = do
  let accs = getStreamAccums form
  accvals <- mapM evalSubExp accs
  arrvals <- mapM lookupVar  arrs
  let ExtLambda i elam_params elam_body elam_rtp = elam
      bind_i = (Ident i (Prim int32),
                BindVar,
                PrimVal $ intvalue Int32 0)
  let fun funargs = binding (bind_i :
                             zip3 (map paramIdent elam_params)
                                  (repeat BindVar)
                                  funargs) $
                    evalBody elam_body
  -- get the outersize of the input array(s), and use it as chunk!
  let (ArrayVal _ _ (outersize:_)) = head arrvals
  let chunkval = PrimVal $ intvalue Int32 $ fromIntegral outersize
  vs <- fun (chunkval:accvals++arrvals)
  return $ valueShapeContext elam_rtp vs ++ vs

evalSOAC (Map _ w fun arrexps) = do
  vss' <- zipWithM (applyLambda fun) [0..] =<< soacArrays w arrexps
  arrays (lambdaReturnType fun) vss'

evalSOAC (ConcatMap _ _ fun inputs) = do
  inputs' <- mapM (mapM lookupVar) inputs
  vss <- mapM (mapM asArray <=< applyConcatMapLambda fun) inputs'
  innershapes <- mapM (mapM (asInt "evalPrimOp ConcatMap" <=< evalSubExp) . arrayDims) $
                 lambdaReturnType fun
  let numTs = length $ lambdaReturnType fun
      emptyArray :: (Array Int PrimValue, Int)
      emptyArray = (listArray (0,-1) [], 0)
      concatArrays (arr1,n1) (arr2,n2) =
        (listArray (0,n1+n2-1) (elems arr1 ++ elems arr2), n1+n2)
      arrs = foldl (zipWith concatArrays) (replicate numTs emptyArray) vss
      (ctx,vs) = unzip
                 [ (PrimVal $ intvalue Int32 $ fromIntegral n,
                    ArrayVal arr (elemType t) (n:innershape))
                 | (innershape,(arr,n),t) <-
                      zip3 innershapes arrs $ lambdaReturnType fun ]
  return $ ctx ++ vs
  where asArray (ArrayVal a _ (n:_)) = return (a, n)
        asArray _                    = bad $ TypeError "evalSOAC asArray"

evalSOAC (Reduce _ w _ fun inputs) = do
  let (accexps, arrexps) = unzip inputs
  startaccs <- mapM evalSubExp accexps
  let foldfun acc (i, x) = applyLambda fun i $ acc ++ x
  foldM foldfun startaccs =<< (zip [0..] <$> soacArrays w arrexps)

evalSOAC (Scan _ w fun inputs) = do
  let (accexps, arrexps) = unzip inputs
  startvals <- mapM evalSubExp accexps
  (acc, vals') <- foldM scanfun (startvals, []) =<<
                  (zip [0..] <$> soacArrays w arrexps)
  arrays (map valueType acc) $ reverse vals'
    where scanfun (acc, l) (i,x) = do
            acc' <- applyLambda fun i $ acc ++ x
            return (acc', acc' : l)

evalSOAC (Redomap _ w _ _ innerfun accexp arrexps) = do
  startaccs <- mapM evalSubExp accexp
  if res_len == acc_len
  then foldM foldfun startaccs =<< (zip [0..] <$> soacArrays w arrexps)
  else do let startaccs'= (startaccs, replicate (res_len - acc_len) [])
          (acc_res, arr_res) <- foldM foldfun' startaccs' =<<
                                (zip [0..] <$> soacArrays w arrexps)
          arr_res_fut <- arrays lam_ret_arr_tp $ transpose $ map reverse arr_res
          return $ acc_res ++ arr_res_fut
    where
        lam_ret_tp     = lambdaReturnType innerfun
        res_len        = length lam_ret_tp
        acc_len        = length accexp
        lam_ret_arr_tp = drop acc_len lam_ret_tp
        foldfun  acc (i,x) = applyLambda innerfun i $ acc ++ x
        foldfun' (acc,arr) (i,x) = do
            res_lam <- applyLambda innerfun i $ acc ++ x
            let res_acc = take acc_len res_lam
                res_arr = drop acc_len res_lam
                acc_arr = zipWith (:) res_arr arr
            return (res_acc, acc_arr)

evalFuncall :: Name -> [Value] -> FutharkM [Value]
evalFuncall fname args = do
  fun <- lookupFun fname
  fun args

evalIntBinOp :: (forall int. (Integral int, Bits int) => int -> int -> int)
             -> SubExp -> SubExp
             -> FutharkM [Value]
evalIntBinOp op = evalIntBinOpM $ \x y -> return $ op x y

evalIntBinOpM :: (forall int. (Integral int, Bits int) => int -> int -> FutharkM int)
              -> SubExp
              -> SubExp
              -> FutharkM [Value]
evalIntBinOpM op e1 e2 = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  case (v1, v2) of
    (PrimVal x, PrimVal y) -> do
      result <- intBinOp op x y
      return [PrimVal result]
    _ ->
      bad $ TypeError "evalIntBinOpM"

evalFloatBinOp :: (forall float. (Eq float, Floating float) =>
                   float -> float -> float)
               -> SubExp -> SubExp
               -> FutharkM [Value]
evalFloatBinOp op = evalFloatBinOpM $ \x y -> return $ op x y

evalFloatBinOpM :: (forall float. (Eq float, Floating float) =>
                    float -> float -> FutharkM float)
                -> SubExp
                -> SubExp
                -> FutharkM [Value]
evalFloatBinOpM op e1 e2 = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  case (v1, v2) of
    (PrimVal x, PrimVal y) -> do
      result <- floatBinOp op x y
      return [PrimVal result]
    _ ->
      bad $ TypeError "evalFloatBinOpM"

evalBoolBinOp :: (Bool -> Bool -> Bool) -> SubExp -> SubExp -> FutharkM [Value]
evalBoolBinOp op e1 e2 = do
  v1 <- evalSubExp e1
  v2 <- evalSubExp e2
  case (v1, v2) of
    (PrimVal (BoolValue x), PrimVal (BoolValue y)) ->
      return [PrimVal $ BoolValue (op x y)]
    _ ->
      bad $ TypeError $ "evalBoolBinOp " ++ pretty v1 ++ " " ++ pretty v2

applyLambda :: Lambda -> Int32 -> [Value] -> FutharkM [Value]
applyLambda (Lambda i params body rettype) j args = do
  v <- binding (bind_i : zip3 (map paramIdent params) (repeat BindVar) args) $
       evalBody body
  checkReturnShapes (staticShapes rettype) v
  return v
  where bind_i = (Ident i $ Prim int32,
                 BindVar,
                 PrimVal $ value j)

applyConcatMapLambda :: Lambda -> [Value] -> FutharkM [Value]
applyConcatMapLambda (Lambda i params body rettype) valargs = do
  v <- binding (bind_i : zip3 (map paramIdent params) (repeat BindVar) (shapes ++ valargs)) $
       evalBody body
  let rettype' = [ arrayOf t (ExtShape [Ext 0]) NoUniqueness
                 | t <- staticShapes rettype ]
  checkReturnShapes rettype' v
  return v
  where bind_i = (Ident i $ Prim int32,
                  BindVar,
                  PrimVal $ intvalue Int32 0)

        shapes =
          let (shapeparams, valparams) =
                splitAt (length params - length valargs) params
              shapemap = shapeMapping'
                         (map paramType valparams)
                         (map valueShape valargs)
          in map (PrimVal . intvalue Int32 . fromIntegral . fromMaybe 0 .
                  flip HM.lookup shapemap .
                  paramName)
             shapeparams

checkReturnShapes :: [TypeBase ExtShape u] -> [Value] -> FutharkM ()
checkReturnShapes = zipWithM_ checkShape
  where checkShape t val = do
          let valshape = map (PrimVal . intvalue Int32 . fromIntegral) $ valueShape val
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
