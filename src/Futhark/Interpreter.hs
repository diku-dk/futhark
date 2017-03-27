{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A nonoptimising interpreter for Futhark.  It makes no assumptions of
-- the form of the input program, and in particular permits shadowing.
-- This interpreter should be considered the primary benchmark for
-- judging the correctness of a program, but note that it is not by
-- itself sufficient.  The interpreter does not perform in-place
-- updates like the native code generator, and bugs related to
-- uniqueness will therefore not be detected.  Of course, the type
-- checker should catch such error.
module Futhark.Interpreter
  ( runFun
  , runFunWithShapes
  , InterpreterError(..) )
where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except

import Data.Array
import Data.List
import Data.Loc
import qualified Data.Map.Strict as M
import Data.Maybe

import Prelude

import Futhark.Representation.SOACS
import Futhark.Util

-- | An error happened during execution, and this is why.
data InterpreterError =
      MissingEntryPoint Name
      -- ^ The specified start function does not exist.
    | InvalidFunctionArguments Name (Maybe [TypeBase Rank NoUniqueness]) [TypeBase Rank NoUniqueness]
      -- ^ The arguments given to a function were mistyped.
    | IndexOutOfBounds String [Int] (Slice Int)
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
  show (IndexOutOfBounds var arrsz slice) =
    "Array index " ++ pretty slice ++ " out of bounds in array '" ++
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

type FunTable = M.Map Name ([Value] -> FutharkM [Value])

type VTable = M.Map VName Value

data FutharkEnv = FutharkEnv { envVtable :: VTable
                             , envFtable :: FunTable
                             }

newtype FutharkM a = FutharkM (ReaderT FutharkEnv
                               (Except InterpreterError) a)
  deriving (Monad, Applicative, Functor,
            MonadReader FutharkEnv)

runFutharkM :: FutharkM a -> FutharkEnv
            -> Either InterpreterError a
runFutharkM (FutharkM m) env = runExcept $ runReaderT m env

bad :: InterpreterError -> FutharkM a
bad = FutharkM . throwError

asPrimitive :: String -> Value -> FutharkM PrimValue
asPrimitive _ (PrimVal v) = return v
asPrimitive w _ = bad $ TypeError $ w ++ " asPrimitive"

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

bindVar (BindInPlace _ src slice) val = do
  srcv <- lookupVar src
  slice' <- mapM evalDimIndex slice
  case srcv of
    ArrayVal arr bt shape -> do
      is <- indexArray (pretty src) slice' shape
      case (is, val) of
        ([i], PrimVal bv) ->
          return $ ArrayVal (arr // [(i, bv)]) bt shape
        (_, ArrayVal valarr _ _) ->
          let updates = zip is $ elems valarr
          in return $ ArrayVal (arr // updates) bt shape
        (_, PrimVal _) ->
          bad $ TypeError "bindVar BindInPlace, incomplete indices given, but replacement value is not array"
    _ ->
      bad $ TypeError "bindVar BindInPlace, source is not array"

bindVars :: [(Ident, Bindage, Value)]
         -> FutharkM VTable
bindVars bnds = do
  let (idents, bindages, vals) = unzip3 bnds
  M.fromList . zip (map identName idents) <$>
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
  val <- asks $ M.lookup vname . envVtable
  case val of Just val' -> return val'
              Nothing   -> bad $ TypeError $ "lookupVar " ++ pretty vname

lookupFun :: Name -> FutharkM ([Value] -> FutharkM [Value])
lookupFun fname = do
  fun <- asks $ M.lookup fname . envFtable
  case fun of Just fun' -> return fun'
              Nothing   -> bad $ TypeError $ "lookupFun " ++ pretty fname

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

indexArray :: String -> Slice Int -> [Int] -> FutharkM [Int]
indexArray name slice shape
  | length slice == length shape,
    is <- flatSlice slice shape,
    all (<product shape) is,
    all (0<=) is =
      return is
 | otherwise =
      bad $ IndexOutOfBounds name shape slice


--------------------------------------------------
------- Interpreting an arbitrary function -------
--------------------------------------------------

-- |  @runFun name args prog@ invokes the @name@ function of program
-- @prog@, with the parameters bound in order to the values in @args@.
-- Returns either an error or the return value of @fun@.
--
-- Note that if 'prog' is not type-correct, you cannot be sure that
-- you'll get an error from the interpreter - it may just as well
-- silently return a wrong value.  You are, however, guaranteed that
-- the initial call to 'prog' is properly checked.
runFun :: Name -> [Value] -> Prog
       -> Either InterpreterError [Value]
runFun fname mainargs prog = do
  let ftable = buildFunTable prog
      futharkenv = FutharkEnv { envVtable = M.empty
                              , envFtable = ftable
                              }
  case (funDefByName fname prog, M.lookup fname ftable) of
    (Nothing, Nothing) -> Left $ MissingEntryPoint fname
    (Just fundec, _) ->
      runThisFun fundec mainargs ftable
    (_ , Just fun) -> -- It's a builtin function, it'll do its own
                      -- error checking.
      runFutharkM (fun mainargs) futharkenv

-- | Like 'runFun', but prepends parameters corresponding to the
-- required shape context of the function being called.
runFunWithShapes :: Name -> [Value] -> Prog
                 -> Either InterpreterError [Value]
runFunWithShapes fname valargs prog = do
  let ftable = buildFunTable prog
      futharkenv = FutharkEnv { envVtable = M.empty
                              , envFtable = ftable
                              }
  case (funDefByName fname prog, M.lookup fname ftable) of
    (Nothing, Nothing) -> Left $ MissingEntryPoint fname
    (Just fundec, _) ->
      let args' = shapes (funDefParams fundec) ++ valargs
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
          in map (PrimVal . IntValue . Int32Value . fromIntegral . fromMaybe 0 .
                  flip M.lookup shapemap .
                  paramName)
             shapeparams

runThisFun :: FunDef -> [Value] -> FunTable
           -> Either InterpreterError [Value]
runThisFun (FunDef _ fname _ fparams _) args ftable
  | argtypes == paramtypes =
    runFutharkM (evalFuncall fname args) futharkenv
  | otherwise =
    Left $ InvalidFunctionArguments fname
    (Just paramtypes)
    argtypes
  where argtypes = map (rankShaped . valueType) args
        paramtypes = map (rankShaped . paramType) fparams
        futharkenv = FutharkEnv { envVtable = M.empty
                                , envFtable = ftable
                                }

buildFunTable :: Prog -> FunTable
buildFunTable = foldl expand builtins . progFunctions
  where -- We assume that the program already passed the type checker, so
        -- we don't check for duplicate definitions.
        expand ftable' (FunDef _ name _ params body) =
          let fun funargs = binding (zip3 (map paramIdent params) (repeat BindVar) funargs) $
                            evalBody body
          in M.insert name fun ftable'

--------------------------------------------
--------------------------------------------
------------- BUILTIN FUNCTIONS ------------
--------------------------------------------
--------------------------------------------

builtins :: M.Map Name ([Value] -> FutharkM [Value])
builtins = M.fromList $ map namify
           [("sqrt32", builtin "sqrt32")
           ,("log32", builtin "log32")
           ,("exp32", builtin "exp32")
           ,("cos32", builtin "cos32")
           ,("sin32", builtin "sin32")
           ,("acos32", builtin "acos32")
           ,("asin32", builtin "asin32")
           ,("atan32", builtin "atan32")
           ,("atan2_32", builtin "atan2_32")
           ,("isinf32", builtin "isinf32")
           ,("isnan32", builtin "isnan32")

           ,("sqrt64", builtin "sqrt64")
           ,("log64", builtin "log64")
           ,("exp64", builtin "exp64")
           ,("cos64", builtin "cos64")
           ,("sin64", builtin "sin64")
           ,("acos64", builtin "acos64")
           ,("asin64", builtin "asin64")
           ,("atan64", builtin "atan64")
           ,("atan2_64", builtin "atan2_64")
           ,("isinf64", builtin "isinf64")
           ,("isnan64", builtin "isnan64")
           ]
  where namify (k,v) = (nameFromString k, v)

builtin :: String -> [Value] -> FutharkM [Value]
builtin "sqrt32" [PrimVal (FloatValue (Float32Value x))] =
  return [PrimVal $ FloatValue $ Float32Value $ sqrt x]
builtin "log32" [PrimVal (FloatValue (Float32Value x))] =
  return [PrimVal $ FloatValue $ Float32Value $ log x]
builtin "exp32" [PrimVal (FloatValue (Float32Value x))] =
  return [PrimVal $ FloatValue $ Float32Value $ exp x]
builtin "cos32" [PrimVal (FloatValue (Float32Value x))] =
  return [PrimVal $ FloatValue $ Float32Value $ cos x]
builtin "sin32" [PrimVal (FloatValue (Float32Value x))] =
  return [PrimVal $ FloatValue $ Float32Value $ sin x]
builtin "acos32" [PrimVal (FloatValue (Float32Value x))] =
  return [PrimVal $ FloatValue $ Float32Value $ acos x]
builtin "asin32" [PrimVal (FloatValue (Float32Value x))] =
  return [PrimVal $ FloatValue $ Float32Value $ asin x]
builtin "atan32" [PrimVal (FloatValue (Float32Value x))] =
  return [PrimVal $ FloatValue $ Float32Value $ atan x]
builtin "atan2_32" [PrimVal (FloatValue (Float32Value x)),
                    PrimVal (FloatValue (Float32Value y))] =
  return [PrimVal $ FloatValue $ Float32Value $ atan2 x y]
builtin "isnan32" [PrimVal (FloatValue (Float32Value x))] =
  return [PrimVal $ BoolValue $ isNaN x]
builtin "isinf32" [PrimVal (FloatValue (Float32Value x))] =
  return [PrimVal $ BoolValue $ isInfinite x]
builtin "sqrt64" [PrimVal (FloatValue (Float64Value x))] =
  return [PrimVal $ FloatValue $ Float64Value $ sqrt x]
builtin "log64" [PrimVal (FloatValue (Float64Value x))] =
  return [PrimVal $ FloatValue $ Float64Value $ log x]
builtin "exp64" [PrimVal (FloatValue (Float64Value x))] =
  return [PrimVal $ FloatValue $ Float64Value $ exp x]
builtin "cos64" [PrimVal (FloatValue (Float64Value x))] =
  return [PrimVal $ FloatValue $ Float64Value $ cos x]
builtin "sin64" [PrimVal (FloatValue (Float64Value x))] =
  return [PrimVal $ FloatValue $ Float64Value $ sin x]
builtin "acos64" [PrimVal (FloatValue (Float64Value x))] =
  return [PrimVal $ FloatValue $ Float64Value $ acos x]
builtin "asin64" [PrimVal (FloatValue (Float64Value x))] =
  return [PrimVal $ FloatValue $ Float64Value $ asin x]
builtin "atan64" [PrimVal (FloatValue (Float64Value x))] =
  return [PrimVal $ FloatValue $ Float64Value $ atan x]
builtin "atan2_64" [PrimVal (FloatValue (Float64Value x)),
                    PrimVal (FloatValue (Float64Value y))] =
  return [PrimVal $ FloatValue $ Float64Value $ atan2 x y]
builtin "isnan64" [PrimVal (FloatValue (Float64Value x))] =
  return [PrimVal $ BoolValue $ isNaN x]
builtin "isinf64" [PrimVal (FloatValue (Float64Value x))] =
  return [PrimVal $ BoolValue $ isInfinite x]
builtin fname args =
  bad $ InvalidFunctionArguments (nameFromString fname) Nothing $
        map (rankShaped . valueType) args

single :: Value -> [Value]
single v = [v]

evalSubExp :: SubExp -> FutharkM Value
evalSubExp (Var ident)  = lookupVar ident
evalSubExp (Constant v) = return $ PrimVal v

evalDimIndex :: DimIndex SubExp -> FutharkM (DimIndex Int)
evalDimIndex (DimFix d) =
  DimFix <$> (asInt "evalDimIndex" =<< evalSubExp d)
evalDimIndex (DimSlice d n s) =
  DimSlice
  <$> (asInt "evalDimIndex" =<< evalSubExp d)
  <*> (asInt "evalDimIndex" =<< evalSubExp n)
  <*> (asInt "evalDimIndex" =<< evalSubExp s)

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
evalExp (Apply fname args rettype) = do
  args' <- mapM (evalSubExp . fst) args
  vs <- evalFuncall fname args'
  return $ valueShapeContext (retTypeValues rettype) vs ++ vs
evalExp (BasicOp op) = evalBasicOp op

evalExp (DoLoop ctxmerge valmerge (ForLoop loopvar it boundexp) loopbody) = do
  bound <- evalSubExp boundexp
  mergestart <- mapM evalSubExp mergeexp
  case bound of
    PrimVal (IntValue bound_iv) -> do
      let n = valueIntegral bound_iv
      vs <- foldM iteration mergestart [0::Integer .. n-1]
      binding (zip3 (map paramIdent mergepat) (repeat BindVar) vs) $
        mapM (lookupVar . paramName) $
        loopResultContext (map fst ctxmerge) (map fst valmerge) ++ map fst valmerge
    _ -> bad $ TypeError "evalBody DoLoop for"
  where merge = ctxmerge ++ valmerge
        (mergepat, mergeexp) = unzip merge
        iteration mergeval i =
          binding [(Ident loopvar $ Prim $ IntType it,
                    BindVar,
                    PrimVal $ IntValue $ intValue it i)] $
            binding (zip3 (map paramIdent mergepat) (repeat BindVar) mergeval) $
              evalBody loopbody

evalExp (DoLoop ctxmerge valmerge (WhileLoop cond) loopbody) = do
  mergestart <- mapM evalSubExp mergeexp
  iteration mergestart
  where merge = ctxmerge ++ valmerge
        (mergepat, mergeexp) = unzip merge
        iteration mergeval =
          binding (zip3 (map paramIdent mergepat) (repeat BindVar) mergeval) $ do
            condv <- lookupVar cond
            case condv of
              PrimVal (BoolValue False) ->
                mapM (lookupVar . paramName) $
                loopResultContext (map fst ctxmerge) (map fst valmerge) ++ map fst valmerge
              PrimVal (BoolValue True) ->
                iteration =<< evalBody loopbody
              _ ->
                bad $ TypeError "evalBody DoLoop while"

evalExp (Op op) = evalSOAC op

evalBasicOp :: BasicOp -> FutharkM [Value]

evalBasicOp (SubExp se) =
  single <$> evalSubExp se

evalBasicOp (Opaque se) =
  single <$> evalSubExp se

evalBasicOp (ArrayLit es rt) = do
  rowshape <- mapM (asInt "evalBasicOp ArrayLit" <=< evalSubExp) $ arrayDims rt
  single <$> (arrayVal <$>
              mapM evalSubExp es <*>
              pure (elemType rt) <*>
              pure (length es : rowshape))

evalBasicOp binop@(BinOp op e1 e2) = do
  v1 <- asPrimitive "BinOp" =<< evalSubExp e1
  v2 <- asPrimitive "BinOp" =<< evalSubExp e2
  case doBinOp op v1 v2 of
    Just v -> return [PrimVal v]
    Nothing -> bad $ TypeError $ "Cannot BinOp: " ++ unwords [pretty binop, pretty v1, pretty v2]


evalBasicOp e@(CmpOp cmp e1 e2) = do
  v1 <- asPrimitive "CmpOp" =<< evalSubExp e1
  v2 <- asPrimitive "CmpOp" =<< evalSubExp e2
  case doCmpOp cmp v1 v2 of
    Just b -> return [PrimVal $ BoolValue b]
    Nothing -> bad $ TypeError $ "Cannot compare: " ++ unwords [pretty e, pretty v1, pretty v2]

evalBasicOp e@(ConvOp op x) = do
  v <- asPrimitive "ConvOp" =<< evalSubExp x
  case doConvOp op v of
    Just v' -> return [PrimVal v']
    Nothing -> bad $ TypeError $ "Cannot convert: " ++ unwords [pretty e, pretty v]

evalBasicOp unop@(UnOp op e) = do
  v <- asPrimitive "UnOp" =<< evalSubExp e
  case doUnOp op v of
    Just v' -> return [PrimVal v']
    Nothing -> bad $ TypeError $ "Cannot UnOp: " ++ unwords [pretty unop, pretty v]

evalBasicOp (Index _ ident slice) = do
  v <- lookupVar ident
  slice' <- mapM evalDimIndex slice
  pure <$> indexArrayValue v slice'

evalBasicOp (Iota e x s et) = do
  v1 <- evalSubExp e
  v2 <- evalSubExp x
  v3 <- evalSubExp s
  case (v1, v2, v3) of
    (PrimVal (IntValue (Int32Value e')),
     PrimVal (IntValue x'),
     PrimVal (IntValue s'))
      | e' >= 0 ->
        let x'' = valueIntegral x'
            s'' = valueIntegral s'
        in return [ArrayVal (listArray (0,fromIntegral $ e'-1) $
                             map (IntValue . intValue et)
                             [x'',x''+s''..x''+(toInteger e'-1)*s''])
                   (IntType et) [fromIntegral e']]
      | otherwise ->
        bad $ NegativeIota $ valueIntegral x'
    _ -> bad $ TypeError "evalBasicOp Iota"

evalBasicOp (Replicate (Shape ds) e2) = do
  ds' <- mapM (asInt32 "Replicate" <=< evalSubExp) ds
  let n = product ds'
  v2 <- evalSubExp e2
  case find (<0) ds' of
    Just x ->
      bad $ NegativeReplicate $ fromIntegral x
    Nothing ->
      case v2 of
        PrimVal bv ->
          return [ArrayVal (listArray (0,fromIntegral n-1) (genericReplicate n bv))
                  (primValueType bv) $
                  map fromIntegral ds']
        ArrayVal arr bt shape ->
          return [ArrayVal (listArray (0,fromIntegral n*product shape-1)
                            (concat $ genericReplicate n $ elems arr))
                  bt $ map fromIntegral ds'++shape]

evalBasicOp (Scratch bt shape) = do
  shape' <- mapM (asInt "evalBasicOp Scratch" <=< evalSubExp) shape
  let nelems = product shape'
      vals = genericReplicate nelems v
  return [ArrayVal (listArray (0,fromIntegral nelems-1) vals) bt shape']
  where v = blankPrimValue bt

evalBasicOp e@(Reshape _ shapeexp arrexp) = do
  shape <- mapM (asInt "evalBasicOp Reshape" <=< evalSubExp) $ newDims shapeexp
  arr <- lookupVar arrexp
  case arr of
    ArrayVal vs bt oldshape
      | product oldshape == product shape ->
        return $ single $ ArrayVal vs bt shape
      | otherwise ->
        bad $ InvalidArrayShape (BasicOp e) oldshape shape
    _ ->
      bad $ TypeError "Reshape given a non-array argument"

evalBasicOp (Rearrange _ perm arrexp) =
  single . permuteArray perm <$> lookupVar arrexp

evalBasicOp (Rotate _ offsets arrexp) = do
  offsets' <- mapM (asInt "evalBasicOp rotate" <=< evalSubExp) offsets
  single . rotateArray offsets' <$> lookupVar arrexp

evalBasicOp (Split _ i sizeexps arrexp) = do
  sizes <- mapM (asInt "evalBasicOp Split" <=< evalSubExp) sizeexps
  arr <- lookupVar arrexp
  return $ splitArray i sizes arr

evalBasicOp (Concat _ i arr1exp arr2exps _) = do
  arr1  <- lookupVar arr1exp
  arr2s <- mapM lookupVar arr2exps
  return [foldl (concatArrays i) arr1 arr2s]

evalBasicOp (Copy v) = single <$> lookupVar v

evalBasicOp (Manifest _ v) = single <$> lookupVar v

evalBasicOp (Assert e loc) = do
  v <- evalSubExp e
  case v of PrimVal (BoolValue True) ->
              return [PrimVal Checked]
            _ ->
              bad $ AssertFailed loc

evalBasicOp (Partition _ n flags arrs) = do
  flags_elems <- arrToList =<< lookupVar flags
  arrvs <- mapM lookupVar arrs
  let ets = map (elemType . valueType) arrvs
  arrs_elems <- mapM arrToList arrvs
  partitions <- mapM (partitionArray flags_elems) arrs_elems
  return $
    case partitions of
      [] ->
        replicate n $ PrimVal $ IntValue $ Int32Value 0
      first_part:_ ->
        map (PrimVal . IntValue . Int32Value . genericLength) first_part ++
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


evalSOAC :: SOAC SOACS -> FutharkM [Value]

evalSOAC (Stream _ w form elam arrs) = do
  let accs = getStreamAccums form
  accvals <- mapM evalSubExp accs
  arrvals <- mapM lookupVar  arrs
  let ExtLambda elam_params elam_body elam_rtp = elam
  let fun funargs = binding (zip3 (map paramIdent elam_params)
                                  (repeat BindVar)
                                  funargs) $
                    evalBody elam_body
  -- get the outersize of the input array(s), and use it as chunk!
  chunkval <- evalSubExp w
  vs <- fun (chunkval:accvals++arrvals)
  return $ valueShapeContext elam_rtp vs ++ vs

evalSOAC (Map _ w fun arrexps) = do
  vss' <- mapM (applyLambda fun) =<< soacArrays w arrexps
  arrays (lambdaReturnType fun) vss'

evalSOAC (Reduce _ w _ fun inputs) = do
  let (accexps, arrexps) = unzip inputs
  startaccs <- mapM evalSubExp accexps
  let foldfun acc x = applyLambda fun $ acc ++ x
  foldM foldfun startaccs =<< soacArrays w arrexps

evalSOAC (Scan _ w fun inputs) = do
  let (accexps, arrexps) = unzip inputs
  startvals <- mapM evalSubExp accexps
  (acc, vals') <- foldM scanfun (startvals, []) =<<
                  soacArrays w arrexps
  arrays (map valueType acc) $ reverse vals'
    where scanfun (acc, l) x = do
            acc' <- applyLambda fun $ acc ++ x
            return (acc', acc' : l)

evalSOAC (Redomap cs w _ redfun foldfun accexp arrexps) = do
  -- SO LAZY: redomap is scanomap, after which we index the last elements.
  w' <- asInt "evalBasicOp Redomap" =<< evalSubExp w
  vs <- evalSOAC $  Scanomap cs w redfun foldfun accexp arrexps
  let (acc_arrs, arrs) = splitAt (length accexp) vs
  accs <- if w' == 0
          then mapM evalSubExp accexp
          else forM acc_arrs $ \acc_arr ->
                 indexArrayValue acc_arr $ DimFix (w' - 1) :
                 map (unitSlice 0) (drop 1 $ valueShape acc_arr)
  return $ accs++arrs

evalSOAC (Scanomap _ w _ innerfun accexp arrexps) = do
  startaccs <- mapM evalSubExp accexp
  if res_len == acc_len
  then do (acc, vals) <- foldM foldfun (startaccs, []) =<< soacArrays w arrexps
          arrays (map valueType acc) $ reverse vals
  else do let startaccs'= (startaccs, [], replicate (res_len - acc_len) [])
          (acc_res, vals,  arr_res) <- foldM foldfun' startaccs' =<<
                                       soacArrays w arrexps
          vals' <- arrays (map valueType acc_res) $ reverse vals
          arr_res_fut <- arrays lam_ret_arr_tp $ transpose $ map reverse arr_res
          return $ vals' ++ arr_res_fut
    where
        lam_ret_tp     = lambdaReturnType innerfun
        res_len        = length lam_ret_tp
        acc_len        = length accexp
        lam_ret_arr_tp = drop acc_len lam_ret_tp
        foldfun  (acc, l) x = do
            acc' <- applyLambda innerfun $ acc ++ x
            return (acc', acc':l)
        foldfun' (acc, l, arr) x = do
            res_lam <- applyLambda innerfun $ acc ++ x
            let res_acc = take acc_len res_lam
                res_arr = drop acc_len res_lam
                acc_arr = zipWith (:) res_arr arr
            return (res_acc, res_acc:l, acc_arr)

evalSOAC (Write _cs len lam ivs as) = do

  let valInt :: Value -> FutharkM Int
      valInt (PrimVal (IntValue (Int32Value l))) = return $ fromIntegral l
      valInt _ = bad $ TypeError "evalSOAC Write: Wrong type for length"

  len' <- valInt =<< evalSubExp len

  as' <- mapM (lookupVar . snd) as

  -- Calculate all indexes and values.
  ivs' <- soacArrays len ivs
  ivs'' <- mapM (applyLambda lam) ivs'

  let ivsLen = length (lambdaReturnType lam) `div` 2
      is = transpose $ map (take ivsLen) ivs''
      vs = transpose $ map (drop ivsLen) ivs''
  is' <- mapM (mapM valInt) is

  (aArrs, aPrimTypes, aShapes) <-
    unzip3 <$> mapM (toArrayVal "evalSOAC Write: Wrong type for 'array' array") as'

  let handleIteration :: [Array Int PrimValue] -> Int -> FutharkM [Array Int PrimValue]
      handleIteration arrs iter = do
        let updatess =
              [ if idx < 0 || idx >= length (elems a)
                then []
                else case val of
                  PrimVal pval -> [(idx, pval)]
                  ArrayVal arr _ _ ->
                    zip [idx * fromIntegral (length (elems arr))..] (elems arr)
              | (i, v, a) <- zip3 is' vs arrs,
                let idx = i !! iter
                    val = v !! iter
              ]
        return [ arr // updates
               | (arr, updates) <- zip arrs updatess
               ]

  ress <- foldM handleIteration aArrs [0..fromIntegral len' - 1]
  return $ zipWith3 ArrayVal ress aPrimTypes aShapes

toArrayVal :: String -> Value -> FutharkM (Array Int PrimValue, PrimType, [Int])
toArrayVal err v = case v of
  ArrayVal a b c -> return (a, b, c)
  _ -> bad $ TypeError err

indexArrayValue :: Value -> Slice Int -> FutharkM Value
indexArrayValue (ArrayVal arr bt shape) slice =
  return $
  case (sliceDims slice, flatSlice slice shape) of
    ([], [i]) ->
      PrimVal $ arr ! i
    (ds, is) ->
      ArrayVal (listArray (0,product ds-1) [ arr ! i | i <- is ]) bt ds
indexArrayValue _ _ = bad $ TypeError "indexArrayValue: argument is not an array"

evalFuncall :: Name -> [Value] -> FutharkM [Value]
evalFuncall fname args = do
  fun <- lookupFun fname
  fun args

applyLambda :: Lambda -> [Value] -> FutharkM [Value]
applyLambda (Lambda params body rettype) args = do
  v <- binding (zip3 (map paramIdent params) (repeat BindVar) args) $
       evalBody body
  checkReturnShapes (staticShapes rettype) v
  return v

checkReturnShapes :: [TypeBase ExtShape u] -> [Value] -> FutharkM ()
checkReturnShapes = zipWithM_ checkShape
  where checkShape t val = do
          let valshape = map (PrimVal . IntValue . Int32Value . fromIntegral) $ valueShape val
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
