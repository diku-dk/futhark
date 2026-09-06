-- | Interpreter for the IR.
--
-- Running an IR program requires specifying the name of an entry point along
-- with values for the entry point parameters. Top level statements
-- ('progConsts') are evaluated first, and then the specified entry point is
-- invoked with the provided values.
--
-- The goal of this interpreter is not performance, but operational clarity.
-- Hence you should not expect programs to run fast at all.
module Futhark.IR.Run (runSOACS, runGPU) where

import Data.Map qualified as M
import Data.Text qualified as T
import Data.Vector.Storable qualified as SVec
import Futhark.Data qualified as V
import Futhark.IR
import Futhark.IR.GPU (GPU)
import Futhark.IR.SOACS (SOAC (Screma), SOACS)
import Language.Futhark.Primitive qualified as P

data Val
  = PrimVal PrimValue
  | ArrayValue [Int] PrimType [PrimValue]

data DimSelection
  = Fixed Int
  | Selected [Int]

type Env = M.Map VName Val

type FunEnv = M.Map Name (FunDef SOACS)

type InterpM a = Either T.Text a

evalBody :: FunEnv -> Env -> Body SOACS -> InterpM [Val]
evalBody funs env (Body _ stms res) = do
  env' <- foldStms env (stmsToList stms)
  mapM (evalSubExp env' . resSubExp) res
  where
    foldStms e [] = pure e
    foldStms e (s : ss) = evalStm funs e s >>= \e' -> foldStms e' ss

-- Evaluate the expression then bind the pattern names to its results
evalStm :: FunEnv -> Env -> Stm SOACS -> InterpM Env
evalStm funs env (Let pat _ e) = do
  vals <- evalExp funs env e
  let names = map patElemName $ patElems pat
  pure $ M.union (M.fromList $ zip names vals) env

-- Produce one Val per pattern element the expression is expected to bind.
evalExp :: FunEnv -> Env -> Exp SOACS -> InterpM [Val]
evalExp _ env (BasicOp op) = evalBasicOp env op
evalExp funs env (Match ses cases defaultBody _) = do
  values <- mapM (\se -> evalSubExp env se >>= expectPrimVal) ses
  evalBody funs env $ selectCase values cases
  where
    selectCase values (Case patterns body : remaining)
      | matches patterns values = body
      | otherwise = selectCase values remaining
    selectCase _ [] = defaultBody

    matches patterns values =
      length patterns == length values
        && and (zipWith matchesValue patterns values)

    matchesValue Nothing _ = True
    matchesValue (Just expected) actual = expected == actual
evalExp funs env (Loop merge (ForLoop iterator intType boundExp) body) = do
  initialValues <- mapM (evalSubExp env . snd) merge
  boundValue <- evalSubExp env boundExp >>= expectPrimVal
  bound <- expectInt boundValue
  runIterations 0 bound initialValues
  where
    mergeNames = map (paramName . fst) merge

    runIterations iteration bound currentValues
      | iteration >= bound =
          pure currentValues
      | otherwise = do
          let iteratorValue =
                PrimVal $ IntValue $ P.intValue intType iteration
              loopBindings =
                M.fromList $
                  (iterator, iteratorValue)
                    : zip mergeNames currentValues
              iterationEnv =
                M.union loopBindings env

          nextValues <- evalBody funs iterationEnv body

          if length nextValues /= length mergeNames
            then Left "loop result count mismatch"
            else runIterations (iteration + 1) bound nextValues
evalExp funs env (Loop merge (WhileLoop condition) body) = do
  initialValues <- mapM (evalSubExp env . snd) merge
  runWhile initialValues
  where
    mergeNames = map (paramName . fst) merge

    runWhile currentValues = do
      let loopEnv =
            M.union
              (M.fromList $ zip mergeNames currentValues)
              env

      conditionValue <- evalSubExp loopEnv (Var condition)

      case conditionValue of
        PrimVal (BoolValue False) ->
          pure currentValues
        PrimVal (BoolValue True) -> do
          nextValues <- evalBody funs loopEnv body
          if length nextValues /= length mergeNames
            then Left "loop result count mismatch"
            else runWhile nextValues
        _ ->
          Left "while-loop condition is not boolean"
evalExp funs env (Apply fname args _ _) = do
  callee <-
    maybe
      (Left $ "function not found: " <> prettyText fname)
      pure
      (M.lookup fname funs)
  argVals <- mapM (evalSubExp env . fst) args
  let params = map paramName $ funDefParams callee
  if length params /= length argVals
    then Left "function argument count mismatch"
    else
      let bindings = M.fromList $ zip params argVals
          calleeEnv = M.union bindings env
       in evalBody funs calleeEnv (funDefBody callee)
evalExp _ env (Op soac) = evalSOAC env soac -- map/reduction/scan
evalExp _ _ WithAcc {} = Left "WithAcc not implemented yet"

evalSubExp :: Env -> SubExp -> InterpM Val
evalSubExp _ (Constant pv) = pure $ PrimVal pv
evalSubExp env (Var v) =
  maybe (Left $ "unbound variable: " <> prettyText v) pure $ M.lookup v env

expectPrimVal :: Val -> InterpM PrimValue
expectPrimVal (PrimVal pv) = pure pv
expectPrimVal (ArrayValue _ _ _) = Left "expected a primitive value"

expectInt :: PrimValue -> InterpM Int
expectInt (IntValue i) = pure $ P.valueIntegral i
expectInt _ = Left "expected an integer value"

expectI32 :: PrimValue -> InterpM Int32
expectI32 (IntValue (Int32Value v)) = pure v
expectI32 _ = Left "expected an i32 value"

evalBasicOp :: Env -> BasicOp -> InterpM [Val]
evalBasicOp env (SubExp se) = pure <$> evalSubExp env se
evalBasicOp env (BinOp op x y) = do
  xv <- expectPrimVal =<< evalSubExp env x
  yv <- expectPrimVal =<< evalSubExp env y
  case P.doBinOp op xv yv of
    Just result -> pure [PrimVal result]
    Nothing -> Left "invalid binary operation"
evalBasicOp env (UnOp op x) = do
  xv <- expectPrimVal =<< evalSubExp env x
  case P.doUnOp op xv of
    Just result -> pure [PrimVal result]
    Nothing -> Left "invalid unary operation"
evalBasicOp env (CmpOp op x y) = do
  xv <- expectPrimVal =<< evalSubExp env x
  yv <- expectPrimVal =<< evalSubExp env y
  case P.doCmpOp op xv yv of
    Just result -> pure [PrimVal $ BoolValue result]
    Nothing -> Left "invalid comparison operation"
evalBasicOp env (ConvOp op x) = do
  xv <- expectPrimVal =<< evalSubExp env x
  case P.doConvOp op xv of
    Just result -> pure [PrimVal result]
    Nothing -> Left "invalid conversion operation"
evalBasicOp env (ArrayLit elements (Prim elementType)) = do
  values <- mapM (evalSubExp env) elements
  primitiveValues <- mapM expectPrimVal values
  pure [ArrayValue [length elements] elementType primitiveValues]
evalBasicOp _ (ArrayVal values elementType) =
  pure [ArrayValue [length values] elementType values]
evalBasicOp env (Assert condition _) = do
  conditionValue <- evalSubExp env condition >>= expectPrimVal
  case conditionValue of
    BoolValue True -> pure [PrimVal UnitValue]
    BoolValue False -> Left "assertion failed"
    _ -> Left "assert condition is not boolean"
evalBasicOp env (Index arrayName slice) = do
  array <-
    maybe (Left $ "unbound array: " <> prettyText arrayName) pure $
      M.lookup arrayName env
  case array of
    ArrayValue shape elementType values ->
      indexArray env shape elementType values slice
    PrimVal _ ->
      Left "cannot index a primitive value"
evalBasicOp env (Reshape arrayName reshape) = do
  array <-
    maybe (Left $ "unbound array: " <> prettyText arrayName) pure $
      M.lookup arrayName env
  dimensions <-
    mapM
      (\subExp -> evalSubExp env subExp >>= expectPrimVal >>= expectInt)
      (shapeDims $ newShape reshape)
  case array of
    ArrayValue _ elementType values
      | product dimensions == length values ->
          pure [ArrayValue dimensions elementType values]
      | otherwise ->
          Left "reshape element count mismatch"
    PrimVal _ -> Left "cannot reshape a primitive value"
evalBasicOp env (Opaque OpaqueNil se) =
  pure <$> evalSubExp env se
evalBasicOp env (Opaque (OpaqueTrace _) se) =
  pure <$> evalSubExp env se -- Perhaps include IO to print here?
evalBasicOp env (Manifest arrayName _) =
  case M.lookup arrayName env of
    Just array@ArrayValue {} -> pure [array]
    Just PrimVal {} -> Left "cannot manifest a primitive value"
    Nothing -> Left $ "unbound array: " <> prettyText arrayName
evalBasicOp env (Iota countSubExp strideSubExp startSubExp intType) = do
  count <- evalSubExp env countSubExp >>= expectPrimVal >>= expectInt
  stride <- evalSubExp env strideSubExp >>= expectPrimVal >>= expectInt
  start <- evalSubExp env startSubExp >>= expectPrimVal >>= expectInt

  if count < 0
    then Left "iota length cannot be negative"
    else
      pure
        [ ArrayValue
            [count]
            (IntType intType)
            [ IntValue $ P.intValue intType (start + i * stride)
            | i <- [0 .. count - 1]
            ]
        ]
evalBasicOp _ _ = Left "basic operation not implemented yet"

indexArray :: Env -> [Int] -> PrimType -> [PrimValue] -> Slice SubExp -> InterpM [Val]
indexArray env shape elementType values (Slice dimensions)
  | length shape /= length dimensions = Left "slice dimensions do not match array dimensions"
  | otherwise = do
      selections <- mapM evalDimension dimensions
      mapM_ checkSelectionBounds $ zip shape selections

      let resultShape = [length indices | Selected indices <- selections]
          coordinates = sequence $ map selectionIndices selections
          selectedValues = map (values !!) $ map (linearIndex shape) coordinates
      case resultShape of
        [] -> case selectedValues of
          [val] -> pure [PrimVal val]
          _ -> Left "invalid scalar index result"
        _ -> pure [ArrayValue resultShape elementType selectedValues]
  where
    evalDimension (DimFix indexExp) =
      Fixed <$> evalInt indexExp
    evalDimension (DimSlice startExp countExp strideExp) = do
      start <- evalInt startExp
      count <- evalInt countExp
      stride <- evalInt strideExp
      if count < 0
        then Left "slice length cannot be negative"
        else
          pure $
            Selected
              [start + position * stride | position <- [0 .. count - 1]]

    evalInt subExp =
      evalSubExp env subExp >>= expectPrimVal >>= expectInt

    selectionIndices (Fixed index) = [index]
    selectionIndices (Selected indices) = indices

    checkSelectionBounds (dimension, Fixed index) =
      checkIndex dimension index
    checkSelectionBounds (dimension, Selected indices) =
      mapM_ (checkIndex dimension) indices

    checkIndex dimension index
      | index < 0 || index >= dimension =
          Left "array index out of bounds"
      | otherwise =
          pure ()

linearIndex :: [Int] -> [Int] -> Int
linearIndex shape indices =
  foldl (\acc (dimSize, index) -> acc * dimSize + index) 0 $ zip shape indices

evalSOAC :: Env -> SOAC SOACS -> InterpM [Val]
evalSOAC _ Screma {} = Left "Screma not implemented yet"
evalSOAC _ _ = Left "SOAC not implemented yet"

_evalLambda :: FunEnv -> Env -> Lambda SOACS -> [Val] -> InterpM [Val]
_evalLambda funs env (Lambda ps _ body) args = do
  evalBody funs (M.union (M.fromList $ zip (map paramName ps) args) env) body

-- | Run a program in the SOACS IR.
runSOACS :: Prog SOACS -> Name -> [V.Value] -> Either T.Text [V.Value]
runSOACS prog entry inputs = do
  let funs = M.fromList [(funDefName fun, fun) | fun <- progFuns prog]
  constsEnv <- foldConsts funs mempty (stmsToList (progConsts prog)) -- top-level consts
  fun <- findEntry prog entry
  convertedInputs <- mapM fromValue inputs
  let shapeArgs = concatMap fst convertedInputs
      valueArgs = map snd convertedInputs
      argVals = shapeArgs <> valueArgs
      params = map paramName $ funDefParams fun
  if length params /= length argVals
    then Left "entry point argument count mismatch"
    else do
      let env = M.union (M.fromList $ zip params argVals) constsEnv
      results <- evalBody funs env (funDefBody fun)
      case reverse results of
        result : _ -> pure <$> toValue result
        [] -> Left "entry point returned no values"
  where
    foldConsts _ e [] = pure e
    foldConsts funs e (s : ss) = evalStm funs e s >>= \e' -> foldConsts funs e' ss

findEntry :: Prog SOACS -> Name -> Either T.Text (FunDef SOACS)
findEntry prog name =
  maybe
    (Left $ "entry point not found: " <> prettyText name)
    Right
    $ lookup
      name
      [ (entryName, fun)
      | fun <- progFuns prog,
        Just (entryName, _, _, _) <- [funDefEntryPoint fun]
      ]

fromValue :: V.Value -> InterpM ([Val], Val)
fromValue (V.I32Value shape values)
  | SVec.null shape,
    [v] <- SVec.toList values =
      pure ([], PrimVal $ IntValue $ Int32Value v)
  | otherwise =
      let shapeValues =
            map
              (PrimVal . IntValue . Int64Value . fromIntegral)
              (SVec.toList shape)
          array =
            ArrayValue
              (SVec.toList shape)
              (IntType Int32)
              (map (IntValue . Int32Value) $ SVec.toList values)
       in pure (shapeValues, array)
fromValue (V.BoolValue shape values)
  | SVec.null shape,
    [v] <- SVec.toList values =
      pure ([], PrimVal $ BoolValue v)
fromValue _ =
  Left "only scalar i32 and bool values are currently supported"

toValue :: Val -> InterpM V.Value
toValue (PrimVal (IntValue (Int32Value v))) =
  pure $ V.I32Value SVec.empty (SVec.singleton v)
toValue (PrimVal (BoolValue v)) =
  pure $ V.BoolValue SVec.empty (SVec.singleton v)
toValue (ArrayValue shape (IntType Int32) values) = do
  values' <- mapM expectI32 values
  pure $
    V.I32Value
      (SVec.fromList shape)
      (SVec.fromList values')
toValue _ =
  Left "only scalar i32 and bool values are currently supported"

-- | Run a program in the GPU IR.
runGPU :: Prog GPU -> Name -> [V.Value] -> Either T.Text [V.Value]
runGPU = undefined
