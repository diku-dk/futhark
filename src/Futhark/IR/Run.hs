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

import Control.Monad (foldM, zipWithM, zipWithM_, (>=>))
import Control.Monad.Error.Class
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Data.Int qualified as I
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector.Storable qualified as SVec
import Data.Vector.Storable.Mutable qualified as MSVec
import Foreign.Storable (Storable)
import Futhark.Data qualified as V
import Futhark.IR
import Futhark.IR.GPU
  ( GPU,
    HostOp (..),
    SizeClass (..),
    SizeOp (..),
  )
import Futhark.IR.SOACS (HistOp (..), Reduce (..), SOAC (FlatMap, Hist, Screma, Stream), SOACS, Scan (..), ScremaForm (..), flatMapNonuniform)
import Futhark.IR.SegOp qualified as Seg
import Futhark.Util (showText)
import Language.Futhark.Primitive qualified as P
import Numeric.Half qualified as H

data Val
  = PrimVal PrimValue
  | ArrayValue [Int] PrimType ArrayValues
  | AccValue Accumulator

-- Operator receives (indices <> old values <> new values).
newtype AccOp = AccOp ([Val] -> IO (Either T.Text [Val]))

data Accumulator = Accumulator
  { accShape :: [Int],
    accArrays :: [Val],
    accOp :: Maybe AccOp
  }

data ArrayValues
  = I8ArrayValues (MSVec.IOVector I.Int8)
  | I16ArrayValues (MSVec.IOVector I.Int16)
  | I32ArrayValues (MSVec.IOVector I.Int32)
  | I64ArrayValues (MSVec.IOVector I.Int64)
  | F16ArrayValues (MSVec.IOVector H.Half)
  | F32ArrayValues (MSVec.IOVector Float)
  | F64ArrayValues (MSVec.IOVector Double)
  | BoolArrayValues (MSVec.IOVector Bool)
  | UnitArrayValues (MSVec.IOVector ())

data DimSelection
  = Fixed Int
  | Selected [Int]

data KernelResultValue
  = KernelValue Val
  | KernelTile [(Int, Int)] Val
  | KernelRegTile [(Int, Int, Int)] Val

type Env = M.Map VName Val

type FunEnv rep = M.Map Name (FunDef rep)

type OpEvaluator rep =
  FunEnv rep -> Env -> Op rep -> InterpM rep [Val]

newtype InterpEnv rep = InterpEnv
  { interpOpEvaluator :: OpEvaluator rep
  }

newtype InterpM rep a = InterpM
  { unInterpM :: ReaderT (InterpEnv rep) (ExceptT T.Text IO) a
  }
  deriving
    (Functor, Applicative, Monad, MonadReader (InterpEnv rep), MonadError T.Text, MonadIO)

interpError :: T.Text -> InterpM rep a
interpError = throwError

newArrayValue :: [Int] -> PrimType -> [PrimValue] -> InterpM rep Val
newArrayValue shape element_type values
  | length values /= product shape =
      interpError "invalid array storage"
  | otherwise = ArrayValue shape element_type <$> newValues element_type
  where
    newValues (IntType Int8) = I8ArrayValues <$> newPrimVector expectInt8
    newValues (IntType Int16) = I16ArrayValues <$> newPrimVector expectInt16
    newValues (IntType Int32) = I32ArrayValues <$> newPrimVector expectInt32
    newValues (IntType Int64) = I64ArrayValues <$> newPrimVector expectInt64
    newValues (FloatType Float16) = F16ArrayValues <$> newPrimVector expectFloat16
    newValues (FloatType Float32) = F32ArrayValues <$> newPrimVector expectFloat32
    newValues (FloatType Float64) = F64ArrayValues <$> newPrimVector expectFloat64
    newValues Bool = BoolArrayValues <$> newPrimVector expectBool
    newValues Unit = UnitArrayValues <$> newPrimVector expectUnit

    newPrimVector unwrap = do
      elements <- mapM unwrap values
      liftIO $ do
        vector <- MSVec.new (length elements)
        zipWithM_ (MSVec.write vector) [0 ..] elements
        pure vector

    expectInt8 (IntValue (Int8Value element)) = pure element
    expectInt8 _ = interpError "expected an i8 value"
    expectInt16 (IntValue (Int16Value element)) = pure element
    expectInt16 _ = interpError "expected an i16 value"
    expectInt32 (IntValue (Int32Value element)) = pure element
    expectInt32 _ = interpError "expected an i32 value"
    expectInt64 (IntValue (Int64Value element)) = pure element
    expectInt64 _ = interpError "expected an i64 value"
    expectFloat16 (FloatValue (Float16Value element)) = pure element
    expectFloat16 _ = interpError "expected an f16 value"
    expectFloat32 (FloatValue (Float32Value element)) = pure element
    expectFloat32 _ = interpError "expected an f32 value"
    expectFloat64 (FloatValue (Float64Value element)) = pure element
    expectFloat64 _ = interpError "expected an f64 value"
    expectBool (BoolValue element) = pure element
    expectBool _ = interpError "expected a bool value"
    expectUnit UnitValue = pure ()
    expectUnit _ = interpError "expected a unit value"

arrayValues :: ArrayValues -> InterpM rep [PrimValue]
arrayValues values =
  mapM (readArrayValue values) [0 .. arrayValuesLength values - 1]

arrayValuesLength :: ArrayValues -> Int
arrayValuesLength (I8ArrayValues values) = MSVec.length values
arrayValuesLength (I16ArrayValues values) = MSVec.length values
arrayValuesLength (I32ArrayValues values) = MSVec.length values
arrayValuesLength (I64ArrayValues values) = MSVec.length values
arrayValuesLength (F16ArrayValues values) = MSVec.length values
arrayValuesLength (F32ArrayValues values) = MSVec.length values
arrayValuesLength (F64ArrayValues values) = MSVec.length values
arrayValuesLength (BoolArrayValues values) = MSVec.length values
arrayValuesLength (UnitArrayValues values) = MSVec.length values

readArrayValue :: ArrayValues -> Int -> InterpM rep PrimValue
readArrayValue vector index
  | index < 0 || index >= arrayValuesLength vector =
      interpError "array index out of bounds"
readArrayValue (I8ArrayValues values) index =
  IntValue . Int8Value <$> liftIO (MSVec.read values index)
readArrayValue (I16ArrayValues values) index =
  IntValue . Int16Value <$> liftIO (MSVec.read values index)
readArrayValue (I32ArrayValues values) index =
  IntValue . Int32Value <$> liftIO (MSVec.read values index)
readArrayValue (I64ArrayValues values) index =
  IntValue . Int64Value <$> liftIO (MSVec.read values index)
readArrayValue (F16ArrayValues values) index =
  FloatValue . Float16Value <$> liftIO (MSVec.read values index)
readArrayValue (F32ArrayValues values) index =
  FloatValue . Float32Value <$> liftIO (MSVec.read values index)
readArrayValue (F64ArrayValues values) index =
  FloatValue . Float64Value <$> liftIO (MSVec.read values index)
readArrayValue (BoolArrayValues values) index =
  BoolValue <$> liftIO (MSVec.read values index)
readArrayValue (UnitArrayValues values) index =
  UnitValue <$ liftIO (MSVec.read values index)

writeArrayValue :: ArrayValues -> Int -> PrimValue -> InterpM rep ()
writeArrayValue (I8ArrayValues values) index (IntValue (Int8Value element)) =
  liftIO $ MSVec.write values index element
writeArrayValue (I16ArrayValues values) index (IntValue (Int16Value element)) =
  liftIO $ MSVec.write values index element
writeArrayValue (I32ArrayValues values) index (IntValue (Int32Value element)) =
  liftIO $ MSVec.write values index element
writeArrayValue (I64ArrayValues values) index (IntValue (Int64Value element)) =
  liftIO $ MSVec.write values index element
writeArrayValue (F16ArrayValues values) index (FloatValue (Float16Value element)) =
  liftIO $ MSVec.write values index element
writeArrayValue (F32ArrayValues values) index (FloatValue (Float32Value element)) =
  liftIO $ MSVec.write values index element
writeArrayValue (F64ArrayValues values) index (FloatValue (Float64Value element)) =
  liftIO $ MSVec.write values index element
writeArrayValue (BoolArrayValues values) index (BoolValue element) =
  liftIO $ MSVec.write values index element
writeArrayValue (UnitArrayValues values) index UnitValue =
  liftIO $ MSVec.write values index ()
writeArrayValue _ _ _ =
  interpError "array element type mismatch"

cloneArrayValues :: ArrayValues -> IO ArrayValues
cloneArrayValues (I8ArrayValues values) = I8ArrayValues <$> MSVec.clone values
cloneArrayValues (I16ArrayValues values) = I16ArrayValues <$> MSVec.clone values
cloneArrayValues (I32ArrayValues values) = I32ArrayValues <$> MSVec.clone values
cloneArrayValues (I64ArrayValues values) = I64ArrayValues <$> MSVec.clone values
cloneArrayValues (F16ArrayValues values) = F16ArrayValues <$> MSVec.clone values
cloneArrayValues (F32ArrayValues values) = F32ArrayValues <$> MSVec.clone values
cloneArrayValues (F64ArrayValues values) = F64ArrayValues <$> MSVec.clone values
cloneArrayValues (BoolArrayValues values) = BoolArrayValues <$> MSVec.clone values
cloneArrayValues (UnitArrayValues values) = UnitArrayValues <$> MSVec.clone values

sliceArrayValues :: Int -> Int -> ArrayValues -> ArrayValues
sliceArrayValues offset count (I8ArrayValues values) =
  I8ArrayValues $ MSVec.slice offset count values
sliceArrayValues offset count (I16ArrayValues values) =
  I16ArrayValues $ MSVec.slice offset count values
sliceArrayValues offset count (I32ArrayValues values) =
  I32ArrayValues $ MSVec.slice offset count values
sliceArrayValues offset count (I64ArrayValues values) =
  I64ArrayValues $ MSVec.slice offset count values
sliceArrayValues offset count (F16ArrayValues values) =
  F16ArrayValues $ MSVec.slice offset count values
sliceArrayValues offset count (F32ArrayValues values) =
  F32ArrayValues $ MSVec.slice offset count values
sliceArrayValues offset count (F64ArrayValues values) =
  F64ArrayValues $ MSVec.slice offset count values
sliceArrayValues offset count (BoolArrayValues values) =
  BoolArrayValues $ MSVec.slice offset count values
sliceArrayValues offset count (UnitArrayValues values) =
  UnitArrayValues $ MSVec.slice offset count values

evalStms :: FunEnv rep -> Env -> Stms rep -> InterpM rep Env
evalStms funs env stms =
  foldM (evalStm funs) env (stmsToList stms)

evalBody :: FunEnv rep -> Env -> Body rep -> InterpM rep [Val]
evalBody funs env (Body _ stms results) = do
  env' <- evalStms funs env stms
  mapM (evalSubExp env' . resSubExp) results

evalKernelBody ::
  FunEnv rep -> Env -> Seg.KernelBody rep -> InterpM rep [KernelResultValue]
evalKernelBody funs env (Body _ stms results) = do
  env' <- evalStms funs env stms
  mapM (evalKernelResult env') results
  where
    evalKernelResult env' (Seg.Returns _ _ result) =
      KernelValue <$> evalSubExp env' result
    evalKernelResult env' (Seg.TileReturns _ dimensions tile) =
      KernelTile <$> mapM (evalPair env') dimensions <*> evalSubExp env' (Var tile)
    evalKernelResult env' (Seg.RegTileReturns _ dimensions tile) =
      KernelRegTile <$> mapM (evalTriple env') dimensions <*> evalSubExp env' (Var tile)

    evalPair env' (size, tile_size) =
      (,) <$> evalInt env' size <*> evalInt env' tile_size
    evalTriple env' (size, block_tile, reg_tile) =
      (,,)
        <$> evalInt env' size
        <*> evalInt env' block_tile
        <*> evalInt env' reg_tile
    evalInt env' sub_exp =
      evalSubExp env' sub_exp >>= expectPrimVal >>= expectInt

-- Evaluate the expression then bind the pattern names to its results
evalStm :: FunEnv rep -> Env -> Stm rep -> InterpM rep Env
evalStm funs env (Let pat aux expression) = do
  vals <-
    evalExp funs env expression `catchError` \err ->
      interpError $ addProvenance (stmAuxLoc aux) err

  let names = map patElemName $ patElems pat
  pure $ M.union (M.fromList $ zip names vals) env

addProvenance :: Provenance -> T.Text -> T.Text
addProvenance (Provenance locations location) message
  | location == mempty = message
  | otherwise =
      message
        <> "\n"
        <> prettyStacktrace
          0
          (map locText $ location : reverse locations)

-- Produce one Val per pattern element the expression is expected to bind.
evalExp :: FunEnv rep -> Env -> Exp rep -> InterpM rep [Val]
evalExp _ env (BasicOp op) = evalBasicOp env op
evalExp funs env (Match ses cases default_body _) = do
  values <- mapM (evalSubExp env >=> expectPrimVal) ses
  evalBody funs env $ selectCase values cases
  where
    selectCase values (Case patterns body : remaining)
      | matches patterns values = body
      | otherwise = selectCase values remaining
    selectCase _ [] = default_body

    matches patterns values =
      length patterns == length values
        && and (zipWith matchesValue patterns values)

    matchesValue Nothing _ = True
    matchesValue (Just expected) actual = expected == actual
evalExp funs env (Loop merge (ForLoop iterator int_type bound_exp) body) = do
  initial_values <- mapM (evalSubExp env . snd) merge
  bound_value <- evalSubExp env bound_exp >>= expectPrimVal
  bound <- expectInt bound_value
  runIterations 0 bound initial_values
  where
    merge_names = map (paramName . fst) merge

    runIterations iteration bound current_values
      | iteration >= bound =
          pure current_values
      | otherwise = do
          let iterator_value =
                PrimVal $ IntValue $ P.intValue int_type iteration
              loop_bindings =
                M.fromList $
                  (iterator, iterator_value)
                    : zip merge_names current_values
              iteration_env =
                M.union loop_bindings env

          next_values <- evalBody funs iteration_env body

          if length next_values /= length merge_names
            then interpError "loop result count mismatch"
            else runIterations (iteration + 1) bound next_values
evalExp funs env (Loop merge (WhileLoop condition) body) = do
  initial_values <- mapM (evalSubExp env . snd) merge
  runWhile initial_values
  where
    merge_names = map (paramName . fst) merge

    runWhile current_values = do
      let loop_env =
            M.union
              (M.fromList $ zip merge_names current_values)
              env

      condition_value <- evalSubExp loop_env (Var condition)

      case condition_value of
        PrimVal (BoolValue False) ->
          pure current_values
        PrimVal (BoolValue True) -> do
          next_values <- evalBody funs loop_env body
          if length next_values /= length merge_names
            then interpError "loop result count mismatch"
            else runWhile next_values
        _ ->
          interpError "while-loop condition is not boolean"
evalExp funs env (Apply fname args _ _) = do
  arg_vals <- mapM (evalSubExp env . fst) args

  case M.lookup fname funs of
    Just callee -> do
      let params = map paramName $ funDefParams callee
      if length params /= length arg_vals
        then interpError "function argument count mismatch"
        else
          let bindings = M.fromList $ zip params arg_vals
              callee_env = M.union bindings env
           in evalBody funs callee_env (funDefBody callee)
    Nothing ->
      evalPrimitiveFunction fname arg_vals
evalExp funs env (Op op) = do
  eval_op <- asks interpOpEvaluator
  eval_op funs env op -- map/reduction/scan
evalExp funs env (WithAcc inputs lambda) =
  evalWithAcc funs env inputs lambda

evalPrimitiveFunction ::
  Name ->
  [Val] ->
  InterpM rep [Val]
evalPrimitiveFunction fname args =
  case M.lookup (nameToText fname) P.primFuns of
    Nothing ->
      interpError $ "function not found: " <> prettyText fname
    Just (parameter_types, result_type, function) -> do
      values <- mapM expectPrimVal args

      if map P.primValueType values /= parameter_types
        then interpError "primitive function argument type mismatch"
        else case function values of
          Just result
            | P.primValueType result == result_type ->
                pure [PrimVal result]
            | otherwise ->
                interpError "primitive function result type mismatch"
          Nothing ->
            interpError $
              "invalid arguments to primitive function: "
                <> prettyText fname

evalWithAcc ::
  FunEnv rep ->
  Env ->
  [WithAccInput rep] ->
  Lambda rep ->
  InterpM rep [Val]
evalWithAcc funs env inputs lambda = do
  evaluated_inputs <- mapM evaluateInput inputs

  let accumulator_count = length inputs
      (certificate_params, accumulator_params) =
        splitAt accumulator_count $ lambdaParams lambda

  if length certificate_params /= accumulator_count
    || length accumulator_params /= accumulator_count
    then interpError "WithAcc lambda parameter count mismatch"
    else do
      interp_env <- ask
      let accumulators = map (mkAccumulator interp_env) evaluated_inputs
          -- Certificates are bound to their accumulator so that zero-iteration
          -- maps can recover it from the result type 'Acc c ...'.
          bindings =
            M.fromList $
              zip (map paramName certificate_params) accumulators
                <> zip (map paramName accumulator_params) accumulators
          lambda_env = M.union bindings env

      results <- evalBody funs lambda_env $ lambdaBody lambda

      let (accumulator_results, ordinary_results) =
            splitAt accumulator_count results

      if length accumulator_results /= accumulator_count
        then interpError "WithAcc lambda returned too few accumulators"
        else do
          mapM_ validateAccumulatorResult accumulator_results
          pure $
            concatMap (\(_, arrays, _) -> arrays) evaluated_inputs
              <> ordinary_results
  where
    mkAccumulator interp_env (index_shape, arrays, operator) =
      AccValue $
        Accumulator index_shape arrays (mkAccOp interp_env <$> operator)

    mkAccOp interp_env (operator_lambda, _) = AccOp $ \args ->
      runExceptT $
        runReaderT
          (unInterpM $ evalLambda funs env operator_lambda args)
          interp_env

    evaluateInput (Shape dimension_exps, array_names, operator) = do
      index_shape <-
        mapM
          (\dimension -> evalSubExp env dimension >>= expectPrimVal >>= expectInt)
          dimension_exps

      if any (< 0) index_shape
        then interpError "WithAcc index-space dimensions cannot be negative"
        else do
          arrays <- mapM lookupArray array_names
          mapM_ (validateArray index_shape) arrays
          pure (index_shape, arrays, operator)

    lookupArray name =
      case M.lookup name env of
        Just array@ArrayValue {} ->
          pure array
        Just _ ->
          interpError "WithAcc input must be an array"
        Nothing ->
          interpError $ "unbound WithAcc input: " <> prettyText name

    validateArray index_shape (ArrayValue shape _ values)
      | index_shape /= take (length index_shape) shape =
          interpError "WithAcc input array does not match index space"
      | arrayValuesLength values /= product shape =
          interpError "invalid WithAcc input array storage"
      | otherwise =
          pure ()
    validateArray _ _ =
      interpError "WithAcc input must be an array"

    validateAccumulatorResult AccValue {} = pure ()
    validateAccumulatorResult _ =
      interpError "WithAcc lambda did not return an accumulator"

evalSubExp :: Env -> SubExp -> InterpM rep Val
evalSubExp _ (Constant pv) = pure $ PrimVal pv
evalSubExp env (Var v) =
  maybe (interpError $ "unbound variable: " <> prettyText v) pure $ M.lookup v env

expectPrimVal :: Val -> InterpM rep PrimValue
expectPrimVal (PrimVal pv) = pure pv
expectPrimVal ArrayValue {} = interpError "expected a primitive value"
expectPrimVal AccValue {} =
  interpError "expected a primitive value"

expectInt :: PrimValue -> InterpM rep Int
expectInt (IntValue i) = pure $ P.valueIntegral i
expectInt _ = interpError "expected an integer value"

safeBinOp :: BinOp -> Bool
safeBinOp (UDiv _ Safe) = True
safeBinOp (SDiv _ Safe) = True
safeBinOp (UMod _ Safe) = True
safeBinOp (SMod _ Safe) = True
safeBinOp (SQuot _ Safe) = True
safeBinOp (SRem _ Safe) = True
safeBinOp _ = False

evalBasicOp :: Env -> BasicOp -> InterpM rep [Val]
evalBasicOp env (SubExp se) = pure <$> evalSubExp env se
evalBasicOp env (BinOp op x y) = do
  xv <- expectPrimVal =<< evalSubExp env x
  yv <- expectPrimVal =<< evalSubExp env y
  case P.doBinOp op xv yv of
    Just result -> pure [PrimVal result]
    Nothing
      -- handle failed safe integer operations with a dummy result to allow assertions
      -- to produe the intended error (tests/slice4.fut)
      | safeBinOp op ->
          pure [PrimVal $ P.blankPrimValue $ P.binOpType op]
      | otherwise ->
          interpError "invalid binary operation"
evalBasicOp env (UnOp op x) = do
  xv <- expectPrimVal =<< evalSubExp env x
  case P.doUnOp op xv of
    Just result -> pure [PrimVal result]
    Nothing -> interpError "invalid unary operation"
evalBasicOp env (CmpOp op x y) = do
  xv <- expectPrimVal =<< evalSubExp env x
  yv <- expectPrimVal =<< evalSubExp env y
  case P.doCmpOp op xv yv of
    Just result -> pure [PrimVal $ BoolValue result]
    Nothing -> interpError "invalid comparison operation"
evalBasicOp env (ConvOp op x) = do
  xv <- expectPrimVal =<< evalSubExp env x
  case P.doConvOp op xv of
    Just result -> pure [PrimVal result]
    Nothing -> interpError "invalid conversion operation"
evalBasicOp env (ArrayLit elements (Prim element_type)) = do
  values <- mapM (evalSubExp env) elements
  primitive_values <- mapM expectPrimVal values
  pure <$> newArrayValue [length elements] element_type primitive_values
evalBasicOp env (ArrayLit elements (Array element_type (Shape row_shape_exps) _)) = do
  expected_row_shape <-
    mapM
      ( \dimension ->
          evalSubExp env dimension >>= expectPrimVal >>= expectInt
      )
      row_shape_exps

  if any (< 0) expected_row_shape
    then interpError "array literal dimensions cannot be negative"
    else do
      rows <- mapM (evalSubExp env) elements
      row_values <- mapM (expectRow expected_row_shape element_type) rows
      result <-
        newArrayValue
          (length elements : expected_row_shape)
          element_type
          (concat row_values)
      pure [result]
  where
    expectRow
      expected_shape
      expected_type
      (ArrayValue actual_shape actual_type values)
        | actual_shape /= expected_shape =
            interpError "array literal row shape mismatch"
        | actual_type /= expected_type =
            interpError "array literal row element type mismatch"
        | arrayValuesLength values /= product actual_shape =
            interpError "invalid array literal row storage"
        | otherwise =
            arrayValues values
    expectRow _ _ PrimVal {} =
      interpError "expected an array-valued row"
    expectRow _ _ AccValue {} =
      interpError "expected an array-valued row"
evalBasicOp _ (ArrayLit _ Acc {}) =
  interpError "accumulator array literals are not implemented"
evalBasicOp _ (ArrayLit _ Mem {}) =
  interpError "memory array literals are unsupported in SOACS"
evalBasicOp _ (ArrayVal values element_type) =
  pure <$> newArrayValue [length values] element_type values
evalBasicOp env (Assert condition message) = do
  condition_value <- evalSubExp env condition >>= expectPrimVal
  case condition_value of
    BoolValue True -> pure [PrimVal UnitValue]
    BoolValue False -> interpError =<< evalErrorMsg env message
    _ -> interpError "assert condition is not boolean"
evalBasicOp env (Index array_name slice) = do
  array <-
    maybe (interpError $ "unbound array: " <> prettyText array_name) pure $
      M.lookup array_name env
  case array of
    ArrayValue shape element_type values ->
      indexArray env shape element_type values slice
    PrimVal _ ->
      interpError "cannot index a primitive value"
    AccValue _ ->
      interpError "cannot index an accumulator value"
evalBasicOp env (Reshape array_name reshape) = do
  array <-
    maybe (interpError $ "unbound array: " <> prettyText array_name) pure $
      M.lookup array_name env

  case array of
    ArrayValue old_shape element_type values ->
      case reshapeKind reshape of
        ReshapeCoerce ->
          pure [ArrayValue old_shape element_type values]
        ReshapeArbitrary -> do
          dimensions <-
            mapM
              (evalSubExp env >=> expectPrimVal >=> expectInt)
              (shapeDims $ newShape reshape)

          if product dimensions == arrayValuesLength values
            then pure [ArrayValue dimensions element_type values]
            else interpError "reshape element count mismatch"
    PrimVal _ ->
      interpError "cannot reshape a primitive value"
    AccValue _ ->
      interpError "cannot reshape an accumulator value"
evalBasicOp env (Opaque OpaqueNil se) =
  pure <$> evalSubExp env se
evalBasicOp env (Opaque (OpaqueTrace t) se) = do
  liftIO $ TIO.putStrLn t
  pure <$> evalSubExp env se
evalBasicOp env (Manifest array_name _) =
  case M.lookup array_name env of
    Just (ArrayValue shape element_type values) -> do
      values' <- liftIO $ cloneArrayValues values
      pure [ArrayValue shape element_type values']
    Just PrimVal {} -> interpError "cannot manifest a primitive value"
    Just AccValue {} -> interpError "cannot manifest an accumulator value"
    Nothing -> interpError $ "unbound array: " <> prettyText array_name
evalBasicOp env (Iota count_sub_exp start_sub_exp stride_sub_exp int_type) = do
  count <- evalSubExp env count_sub_exp >>= expectPrimVal >>= expectInt
  stride <- evalSubExp env stride_sub_exp >>= expectPrimVal >>= expectInt
  start <- evalSubExp env start_sub_exp >>= expectPrimVal >>= expectInt

  if count < 0
    then interpError "iota length cannot be negative"
    else do
      values <-
        newArrayValue
          [count]
          (IntType int_type)
          [ IntValue $ P.intValue int_type (start + i * stride)
          | i <- [0 .. count - 1]
          ]
      pure
        [values]
evalBasicOp env (Replicate (Shape shape_exps) val_exp) = do
  dimensions <- mapM (\dim -> evalSubExp env dim >>= expectPrimVal >>= expectInt) shape_exps
  if any (< 0) dimensions
    then interpError " replicate dimensions cannot be negative"
    else do
      val <- evalSubExp env val_exp
      let copies = product dimensions

      case (dimensions, val) of
        ([], ArrayValue shape element_type values) -> do
          values' <- liftIO $ cloneArrayValues values
          pure [ArrayValue shape element_type values']
        ([], _) -> pure [val]
        (_, PrimVal primitive_value) ->
          pure <$> newArrayValue dimensions (P.primValueType primitive_value) (replicate copies primitive_value)
        (_, ArrayValue old_shape element_type values) -> do
          primitive_values <- arrayValues values
          pure <$> newArrayValue (dimensions <> old_shape) element_type (concat $ replicate copies primitive_values)
        (_, AccValue {}) ->
          interpError "cannot replicate an accumulator value"
evalBasicOp env (Rearrange array_name permutation) = do
  array <-
    maybe (interpError $ "unbound array: " <> prettyText array_name) pure $
      M.lookup array_name env
  case array of
    ArrayValue old_shape element_type values
      | not $ validPermutation (length old_shape) permutation -> interpError "invalid rearrange permutation"
      | otherwise -> do
          let new_shape = map (old_shape !!) permutation
              new_coordinates =
                sequence [[0 .. dimension - 1] | dimension <- new_shape]
              oldCoordinate new_coordinate =
                [new_coordinate !! position | position <- inversePermutation permutation]
          new_values <-
            mapM
              (readArrayValue values . linearIndex old_shape . oldCoordinate)
              new_coordinates
          pure <$> newArrayValue new_shape element_type new_values
    PrimVal _ -> interpError "cannot rearrange a primitive value"
    AccValue _ -> interpError "cannot rearrange an accumulator value"
evalBasicOp env (Concat concat_dim array_names result_size_exp) = do
  arrays <- mapM lookupArray $ NE.toList array_names
  declared_size <-
    evalSubExp env result_size_exp >>= expectPrimVal >>= expectInt

  case arrays of
    [] ->
      interpError "concat requires at least one array"
    first_array@(first_shape, element_type, _) : remaining
      | concat_dim < 0 || concat_dim >= length first_shape ->
          interpError "concat dimension out of bounds"
      | not $ all (compatible first_array) remaining ->
          interpError "concat array shapes or element types do not match"
      | declared_size /= actualSize arrays ->
          interpError "concat result size mismatch"
      | otherwise -> do
          let result_shape =
                replaceAt concat_dim declared_size first_shape
              coordinates =
                sequence [[0 .. size - 1] | size <- result_shape]

          result_values <- mapM (valueAt arrays) coordinates
          pure <$> newArrayValue result_shape element_type result_values
  where
    lookupArray name =
      case M.lookup name env of
        Just (ArrayValue shape element_type values) ->
          pure (shape, element_type, values)
        Just PrimVal {} ->
          interpError "cannot concatenate a primitive value"
        Just AccValue {} ->
          interpError "cannot concatenate an accumulator value"
        Nothing ->
          interpError $ "unbound array: " <> prettyText name

    compatible (first_shape, first_type, _) (shape, element_type, _) =
      first_type == element_type
        && length first_shape == length shape
        && removeAt concat_dim first_shape == removeAt concat_dim shape

    actualSize =
      sum . map (\(shape, _, _) -> shape !! concat_dim)

    valueAt arrays coordinate = do
      let concat_index = coordinate !! concat_dim
      (source_shape, source_values, local_index) <-
        findSource concat_index arrays

      let source_coordinate =
            replaceAt concat_dim local_index coordinate
          offset =
            linearIndex source_shape source_coordinate

      readArrayValue source_values offset

    findSource _ [] =
      interpError "invalid concat coordinate"
    findSource index ((shape, _, values) : arrays)
      | index < size =
          pure (shape, values, index)
      | otherwise =
          findSource (index - size) arrays
      where
        size = shape !! concat_dim
evalBasicOp env (Update _ array_name slice value_exp) = do
  array <-
    maybe (interpError $ "unbound array: " <> prettyText array_name) pure $
      M.lookup array_name env

  replacement <- evalSubExp env value_exp

  case array of
    PrimVal _ ->
      interpError "cannot update a primitive value"
    AccValue _ ->
      interpError "cannot update an accumulator value"
    ArrayValue shape element_type values -> do
      (slice_shape, coordinates) <- resolveSlice env shape slice
      replacement_values <-
        updateValues element_type slice_shape replacement

      let offsets = map (linearIndex shape) coordinates

      mapM_
        (uncurry $ writeArrayValue values)
        (zip offsets replacement_values)

      pure [ArrayValue shape element_type values]
evalBasicOp env (FlatIndex array_name flat_slice) = do
  array <-
    maybe
      (interpError $ "unbound array: " <> prettyText array_name)
      pure
      (M.lookup array_name env)

  (result_shape, offsets) <- evalFlatSlice env flat_slice

  case array of
    ArrayValue [_] element_type values
      | not $ all (validOffset values) offsets ->
          interpError "flat index out of bounds"
      | otherwise -> do
          let count = product result_shape
              view offset =
                pure
                  [ ArrayValue result_shape element_type $
                      sliceArrayValues offset count values
                  ]
          case result_shape of
            [] ->
              case offsets of
                [offset] -> do
                  primitive_value <- readArrayValue values offset
                  pure [PrimVal primitive_value]
                _ -> interpError "invalid scalar flat index"
            _ ->
              case offsets of
                [] -> view 0
                offset : _
                  | offsets == [offset .. offset + count - 1] ->
                      view offset
                _ -> do
                  selected_values <- mapM (readArrayValue values) offsets
                  pure <$> newArrayValue result_shape element_type selected_values
    ArrayValue {} ->
      interpError "flat index source must be one-dimensional"
    PrimVal {} ->
      interpError "cannot flat-index a primitive value"
    AccValue {} ->
      interpError "cannot flat-index an accumulator value"
  where
    validOffset values offset =
      offset >= 0 && offset < arrayValuesLength values
evalBasicOp env (FlatUpdate source_name flat_slice replacement_name) = do
  source <-
    maybe
      (interpError $ "unbound array: " <> prettyText source_name)
      pure
      (M.lookup source_name env)
  replacement <-
    maybe
      (interpError $ "unbound replacement: " <> prettyText source_name)
      pure
      (M.lookup replacement_name env)
  (replacement_shape, offsets) <- evalFlatSlice env flat_slice
  case source of
    ArrayValue source_shape@[_] source_type source_values -> do
      replacement_values <-
        valuesForReplacement source_type replacement_shape replacement

      if not $ all (validOffset source_values) offsets
        then interpError "flat update out of bounds"
        else do
          mapM_
            (uncurry $ writeArrayValue source_values)
            (zip offsets replacement_values)

          pure [ArrayValue source_shape source_type source_values]
    ArrayValue {} ->
      interpError "flat update source must be one-dimensional"
    PrimVal {} ->
      interpError "cannot flat-update a primitive value"
    AccValue {} -> interpError "cannot flat-update an accumulator value"
  where
    validOffset values offset =
      offset >= 0 && offset < arrayValuesLength values

    valuesForReplacement expected_type [] (PrimVal val)
      | P.primValueType val == expected_type =
          pure [val]
      | otherwise =
          interpError "flat update element type mismatch"
    valuesForReplacement
      expected_type
      expected_shape
      (ArrayValue actual_shape actual_type values)
        | actual_shape /= expected_shape =
            interpError "flat update replacement shape mismatch"
        | actual_type /= expected_type =
            interpError "flat update element type mismatch"
        | otherwise =
            arrayValues values
    valuesForReplacement _ _ _ =
      interpError "invalid flat update replacement"
evalBasicOp env (Scratch element_type dimension_exps) = do
  dimensions <-
    mapM (\dimension_exp -> evalSubExp env dimension_exp >>= expectPrimVal >>= expectInt) dimension_exps
  if any (< 0) dimensions
    then interpError "scratch dimensions cannot be negative"
    else
      let element_count = product dimensions
          blank_value = P.blankPrimValue element_type
       in pure <$> newArrayValue dimensions element_type (replicate element_count blank_value)
evalBasicOp env (UserParam _ default_sub_exp) =
  pure <$> evalSubExp env default_sub_exp
evalBasicOp env (UpdateAcc safety accumulator_name index_exps value_exps) =
  evalUpdateAcc env safety accumulator_name index_exps value_exps

evalUpdateAcc ::
  Env ->
  Safety ->
  VName ->
  [SubExp] ->
  [SubExp] ->
  InterpM rep [Val]
evalUpdateAcc env safety accumulator_name index_exps value_exps = do
  accumulator <-
    maybe
      (interpError $ "unbound accumulator: " <> prettyText accumulator_name)
      pure
      (M.lookup accumulator_name env)

  indices <-
    mapM
      (\index_exp -> evalSubExp env index_exp >>= expectPrimVal >>= expectInt)
      index_exps
  values <- mapM (evalSubExp env) value_exps

  case accumulator of
    AccValue acc -> do
      updateAccumulator acc indices values
      pure [accumulator]
    _ ->
      interpError "UpdateAcc argument is not an accumulator"
  where
    updateAccumulator acc indices new_values
      | length indices /= length (accShape acc) =
          interpError "accumulator update index rank mismatch"
      | length new_values /= length (accArrays acc) =
          interpError "accumulator update value count mismatch"
      | not $ indicesInBounds (accShape acc) indices =
          case safety of
            Safe -> pure ()
            Unsafe -> interpError "unsafe accumulator update out of bounds"
      | otherwise = do
          replacement_values <-
            case accOp acc of
              Nothing -> pure new_values
              Just (AccOp operator) -> do
                old_values <-
                  mapM (readAccumulatorElement indices) $ accArrays acc
                liftIO (operator (map int64Val indices <> old_values <> new_values))
                  >>= either throwError pure

          if length replacement_values /= length (accArrays acc)
            then interpError "accumulator operator result count mismatch"
            else
              zipWithM_
                (writeAccumulatorElementInPlace indices)
                (accArrays acc)
                replacement_values

    indicesInBounds shape indices =
      and $ zipWith (\size index -> index >= 0 && index < size) shape indices

evalErrorMsg :: Env -> ErrorMsg SubExp -> InterpM rep T.Text
evalErrorMsg env (ErrorMsg parts) = do
  evaluatedParts <- mapM evalPart parts
  pure $ T.concat ("Error " : evaluatedParts)
  where
    evalPart (ErrorString text) = pure text
    evalPart (ErrorVal expected_type sub_exp) = do
      val <- evalSubExp env sub_exp >>= expectPrimVal
      if P.primValueType val == expected_type
        then pure $ renderErrorValue val
        else interpError "assert error-message value type mismatch"

renderErrorValue :: PrimValue -> T.Text
renderErrorValue (IntValue val) =
  showText (P.valueIntegral val :: Integer)
renderErrorValue (FloatValue (Float16Value val)) =
  showText val
renderErrorValue (FloatValue (Float32Value val)) =
  showText val
renderErrorValue (FloatValue (Float64Value val)) =
  showText val
renderErrorValue (BoolValue True) = "true"
renderErrorValue (BoolValue False) = "false"
renderErrorValue UnitValue = "()"

evalFlatSlice :: Env -> FlatSlice SubExp -> InterpM rep ([Int], [Int])
evalFlatSlice env (FlatSlice offset_exp dimensions) = do
  offset <- evalInt offset_exp
  evaluated_dimensions <- mapM evalDimension dimensions

  let result_shape = map fst evaluated_dimensions
      strides = map snd evaluated_dimensions
      coordinates = sequence [[0 .. size - 1] | size <- result_shape]
      offsets = [offset + sum (zipWith (*) coordinate strides) | coordinate <- coordinates]
  if any (< 0) result_shape
    then interpError "flat slice dimensions cannot be negative"
    else pure (result_shape, offsets)
  where
    evalInt sub_exp = evalSubExp env sub_exp >>= expectPrimVal >>= expectInt

    evalDimension (FlatDimIndex size_exp stride_exp) = do
      size <- evalInt size_exp
      stride <- evalInt stride_exp
      pure (size, stride)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt index val xs =
  take index xs <> [val] <> drop (index + 1) xs

removeAt :: Int -> [a] -> [a]
removeAt index xs =
  take index xs <> drop (index + 1) xs

validPermutation :: Int -> [Int] -> Bool
validPermutation rank permutation =
  L.sort permutation == [0 .. rank - 1]

inversePermutation :: [Int] -> [Int]
inversePermutation permutation =
  map snd $ L.sortOn fst $ zip permutation [0 ..]

updateValues ::
  PrimType ->
  [Int] ->
  Val ->
  InterpM rep [PrimValue]
updateValues element_type slc_shape replacement =
  case replacement of
    PrimVal primitive_value
      | slc_shape /= [] ->
          interpError "cannot use a scalar to update a non-scalar slice"
      | P.primValueType primitive_value /= element_type ->
          interpError "update element type mismatch"
      | otherwise ->
          pure [primitive_value]
    ArrayValue replacement_shape replacement_type replacement_values
      | replacement_type /= element_type ->
          interpError "update element type mismatch"
      | replacement_shape /= slc_shape ->
          interpError "update value shape does not match slice shape"
      | otherwise ->
          arrayValues replacement_values
    AccValue {} -> interpError "cannot use an accumulator as an update value"

resolveSlice ::
  Env ->
  [Int] ->
  Slice SubExp ->
  InterpM rep ([Int], [[Int]])
resolveSlice env shape (Slice dimensions)
  | length shape /= length dimensions =
      interpError "slice dimensions do not match array dimensions"
  | otherwise = do
      selections <- mapM evalDimension dimensions
      mapM_ checkSelectionBounds $ zip shape selections

      let result_shape =
            [length indices | Selected indices <- selections]
          coordinates =
            mapM selectionIndices selections

      pure (result_shape, coordinates)
  where
    evalDimension (DimFix index_exp) =
      Fixed <$> evalInt index_exp
    evalDimension (DimSlice start_exp count_exp stride_exp) = do
      start <- evalInt start_exp
      count <- evalInt count_exp
      stride <- evalInt stride_exp

      if count < 0
        then interpError "slice length cannot be negative"
        else
          pure $
            Selected
              [start + position * stride | position <- [0 .. count - 1]]

    evalInt sub_exp =
      evalSubExp env sub_exp >>= expectPrimVal >>= expectInt

    selectionIndices (Fixed index) = [index]
    selectionIndices (Selected indices) = indices

    checkSelectionBounds (dimension, Fixed index) =
      checkIndex dimension index
    checkSelectionBounds (dimension, Selected indices) =
      mapM_ (checkIndex dimension) indices

    checkIndex dimension index
      | index < 0 || index >= dimension =
          interpError "array index out of bounds"
      | otherwise =
          pure ()

indexArray ::
  Env ->
  [Int] ->
  PrimType ->
  ArrayValues ->
  Slice SubExp ->
  InterpM rep [Val]
indexArray env shape element_type values slice@(Slice dimensions)
  | length shape /= length dimensions =
      interpError "slice dimensions do not match array dimensions"
  | otherwise = do
      selections <-
        zipWithM
          evalDimension
          (zip shape $ tail $ scanr (*) 1 shape)
          dimensions
      let offset = sum $ map fst selections
          axes = concatMap snd selections
          result_shape = map fst axes
          count = product result_shape
          expected_strides = tail $ scanr (*) 1 result_shape
          contiguous =
            and $
              zipWith
                (\(size, stride) expected -> size <= 1 || stride == expected)
                axes
                expected_strides
          view start =
            pure
              [ ArrayValue result_shape element_type $
                  sliceArrayValues start count values
              ]
      case result_shape of
        [] -> pure . PrimVal <$> readArrayValue values offset
        _
          | count == 0 -> view 0
          | contiguous -> view offset
          | otherwise -> do
              (_, coordinates) <- resolveSlice env shape slice
              selected_values <-
                mapM (readArrayValue values . linearIndex shape) coordinates
              pure <$> newArrayValue result_shape element_type selected_values
  where
    evalInt sub_exp =
      evalSubExp env sub_exp >>= expectPrimVal >>= expectInt

    evalDimension (dimension, source_stride) (DimFix index_exp) = do
      index <- evalInt index_exp
      checkIndex dimension index
      pure (index * source_stride, [])
    evalDimension (dimension, source_stride) (DimSlice start_exp count_exp stride_exp) = do
      start <- evalInt start_exp
      count <- evalInt count_exp
      stride <- evalInt stride_exp
      if count < 0
        then interpError "slice length cannot be negative"
        else do
          if count == 0
            then pure ()
            else do
              checkIndex dimension start
              checkIndex dimension (start + (count - 1) * stride)
          pure (start * source_stride, [(count, stride * source_stride)])

    checkIndex dimension index
      | index < 0 || index >= dimension =
          interpError "array index out of bounds"
      | otherwise = pure ()

linearIndex :: [Int] -> [Int] -> Int
linearIndex shape indices =
  foldl (\acc (dim_size, index) -> acc * dim_size + index) 0 $ zip shape indices

evalSOAC :: FunEnv rep -> Env -> SOAC rep -> InterpM rep [Val]
evalSOAC funs env (Screma width_exp input_names form) =
  evalScrema funs env width_exp input_names form
evalSOAC funs env (Stream width_exp input_names initial_accumulators lambda) =
  evalStream funs env width_exp input_names initial_accumulators lambda
evalSOAC funs env (Hist width_exp input_names hist_ops lambda) = evalHist funs env width_exp input_names hist_ops lambda
evalSOAC funs env (FlatMap width_exp input_names lambda) = evalFlatMap funs env width_exp input_names lambda
evalSOAC _ _ _ = interpError "SOAC not implemented yet"

evalFlatMap ::
  FunEnv rep ->
  Env ->
  SubExp ->
  [VName] ->
  ExtLambda rep ->
  InterpM rep [Val]
evalFlatMap funs env width_exp input_names lambda = do
  width <- evalSubExp env width_exp >>= expectPrimVal >>= expectInt
  if width < 0
    then interpError "FlatMap width cannot be negative"
    else do
      inputs <- mapM (lookupSoacInput env) input_names
      mapM_ (validateSoacInput width) inputs

      if length inputs /= length (lambdaParams lambda)
        then interpError "FlatMap input count does not match lambda parameters"
        else do
          rows <- mapM (runIteration inputs) [0 .. width - 1]
          let sizes = map fst rows
              value_rows = map snd rows
              offsets = init $ scanl (+) 0 sizes
              total_size = sum sizes
              return_types = drop 1 $ lambdaReturnType lambda
              columns
                | null value_rows = replicate (length return_types) []
                | otherwise = L.transpose value_rows
          values <-
            zipWithM
              (collectFlatMapOutput env sizes total_size)
              return_types
              columns
          let flags = concatMap segmentFlags sizes
          sizes_array <- newArrayValue [width] int64_type $ map int64Prim sizes
          flags_array <- newArrayValue [total_size] Bool $ map BoolValue flags
          offsets_array <- newArrayValue [width] int64_type $ map int64Prim offsets

          pure $
            [int64Val total_size, sizes_array, flags_array, offsets_array]
              <> values
  where
    runIteration inputs index = do
      input_rows <- mapM (rowAt index) inputs
      results <- evalLambda funs env lambda input_rows

      case results of
        size_value : values -> do
          size <- expectPrimVal size_value >>= expectInt
          if size < 0
            then interpError "FlatMap segment size cannot be negative"
            else pure (size, values)
        [] ->
          interpError "FlatMap lambda returned no segment size"

    segmentFlags size
      | size > 0 = True : replicate (size - 1) False
      | otherwise = []

    int64_type = IntType Int64
    int64Prim = IntValue . Int64Value . fromIntegral

evalHist ::
  FunEnv rep ->
  Env ->
  SubExp ->
  [VName] ->
  [HistOp rep] ->
  Lambda rep ->
  InterpM rep [Val]
evalHist funs env width_exp input_names hist_ops bucket_lambda = do
  width <- evalSubExp env width_exp >>= expectPrimVal >>= expectInt

  if width < 0
    then interpError "Hist width cannot be negative"
    else do
      inputs <- mapM (lookupSoacInput env) input_names
      mapM_ (validateSoacInput width) inputs

      initial_histograms <- mapM initialHistogramsFor hist_ops
      final_histograms <-
        foldM
          (runIteration inputs)
          initial_histograms
          [0 .. width - 1]

      pure $ concat final_histograms
  where
    index_counts = map (shapeRank . histShape) hist_ops
    value_counts = map (length . histDest) hist_ops

    initialHistogramsFor hist_op =
      mapM lookupHistogram $ histDest hist_op

    lookupHistogram name =
      case M.lookup name env of
        Just histogram@ArrayValue {} -> pure histogram
        Just PrimVal {} -> interpError "Hist destination must be an array"
        Just AccValue {} -> interpError "Hist destination must be an array"
        Nothing -> interpError $ "unbound Hist destination: " <> prettyText name

    runIteration inputs histograms iteration = do
      input_rows <- mapM (rowAt iteration) inputs
      bucket_results <- evalLambda funs env bucket_lambda input_rows

      (index_groups, remaining) <- splitGroups index_counts bucket_results
      (value_groups, extra) <- splitGroups value_counts remaining

      index_groups' <-
        mapM
          (mapM (expectPrimVal >=> expectInt))
          index_groups

      if null extra
        then
          if length hist_ops /= length index_groups'
            || length hist_ops /= length value_groups
            || length hist_ops /= length histograms
            then interpError "Hist operation count mismatch"
            else
              mapM
                ( \(hist_operation, indices, value_and_histograms) ->
                    updateHistogram hist_operation indices value_and_histograms
                )
                (zip3 hist_ops index_groups' (zip value_groups histograms))
        else interpError "Hist bucket lambda returned too many values"

    updateHistogram hist_operation indices (values, histograms)
      | length histograms /= length (histDest hist_operation) =
          interpError "Hist destination count mismatch"
      | not (inBounds indices histograms) =
          pure histograms
      | otherwise = do
          old_bins <- mapM (readHistogramBin indices) histograms
          new_bins <-
            evalLambda funs env (histOp hist_operation) (old_bins <> values)

          if length new_bins /= length histograms
            then interpError "Hist operator result count mismatch"
            else zipWithM (writeHistogramBin indices) histograms new_bins

    inBounds indices histograms =
      case histograms of
        [] -> False
        ArrayValue shape _ _ : _ ->
          length indices <= length shape
            && and (zipWith validIndex indices shape)
        _ -> False

    validIndex index dimension =
      index >= 0 && index < dimension

evalStream :: FunEnv rep -> Env -> SubExp -> [VName] -> [SubExp] -> Lambda rep -> InterpM rep [Val]
evalStream funs env width_exp input_names initial_accumulators lambda = do
  width_value <- evalSubExp env width_exp >>= expectPrimVal
  width <- expectInt width_value

  if width < 0
    then interpError "Stream width cannot be negative"
    else do
      inputs <- mapM (lookupSoacInput env) input_names
      mapM_ (validateSoacInput width) inputs
      accumulators <- mapM (evalSubExp env) initial_accumulators

      let chunk_size = PrimVal $ IntValue $ Int64Value $ fromIntegral width
          lambda_args = chunk_size : accumulators <> inputs
      evalLambda funs env lambda lambda_args

evalScrema :: FunEnv rep -> Env -> SubExp -> [VName] -> ScremaForm rep -> InterpM rep [Val]
evalScrema funs env width_exp input_names (ScremaForm pre_lambda scans reductions post_lambda) = do
  width <- evalSubExp env width_exp >>= expectPrimVal >>= expectInt
  if width < 0
    then interpError "Screma width cannot be negative"
    else do
      inputs <- mapM (lookupSoacInput env) input_names
      mapM_ (validateSoacInput width) inputs

      if length inputs /= length (lambdaParams pre_lambda)
        then interpError "Screma input count does not match lambda parameters"
        else do
          initial_scan_states <- mapM (mapM (evalSubExp env) . scanNeutral) scans
          initial_reduction_states <- mapM (mapM (evalSubExp env) . redNeutral) reductions
          (_, final_reduction_states, reversed_output_rows) <-
            foldM
              (runIteration inputs)
              (initial_scan_states, initial_reduction_states, [])
              [0 .. width - 1]
          collected_outputs <-
            collectScremaOutputs
              env
              width
              (lambdaReturnType post_lambda)
              (reverse reversed_output_rows)

          pure $ concat final_reduction_states <> collected_outputs
  where
    scan_sizes =
      map (length . scanNeutral) scans
    reduction_sizes =
      map (length . redNeutral) reductions

    runIteration
      inputs
      (scan_states, reduction_states, output_rows)
      index = do
        input_rows <- mapM (rowAt index) inputs
        pre_results <- evalLambda funs env pre_lambda input_rows
        (scan_contributions, after_scans) <- splitGroups scan_sizes pre_results
        (reduction_contributions, map_values) <- splitGroups reduction_sizes after_scans
        next_scan_states <- updateScanStates funs env scans scan_states scan_contributions
        next_reduction_states <- updateReductionStates funs env reductions reduction_states reduction_contributions
        post_results <- evalLambda funs env post_lambda (concat next_scan_states <> map_values)
        pure (next_scan_states, next_reduction_states, post_results : output_rows)

int64Val :: Int -> Val
int64Val = PrimVal . IntValue . Int64Value . fromIntegral

evalSegSpace :: FunEnv rep -> M.Map VName Val -> Seg.SegSpace -> Seg.KernelBody rep -> InterpM rep ([Int], [([Int], [KernelResultValue])])
evalSegSpace funs env space@(Seg.SegSpace _ dimensions) body = do
  sizes <- mapM (evalSubExp env . snd >=> expectPrimVal >=> expectInt) dimensions
  if any (< 0) sizes
    then interpError "negative SegSpace dimension"
    else do
      let coordinates = sequence [[0 .. size - 1] | size <- sizes]
      rows <- mapM (runWorker sizes) coordinates
      pure (sizes, rows)
  where
    runWorker sizes coordinate = do
      let worker_env = segSpaceEnv env space sizes coordinate
      values <- evalKernelBody funs worker_env body
      pure (coordinate, values)

segSpaceEnv :: Env -> Seg.SegSpace -> [Int] -> [Int] -> Env
segSpaceEnv env (Seg.SegSpace flat dimensions) shape coordinate =
  M.union bindings env
  where
    bindings =
      M.fromList $
        (flat, int64Val $ linearIndex shape coordinate)
          : zip (map fst dimensions) (map int64Val coordinate)

evalShape :: Env -> Shape -> InterpM rep [Int]
evalShape env =
  mapM
    ( \dimension ->
        evalSubExp env dimension >>= expectPrimVal >>= expectInt
    )
    . shapeDims

expectKernelValue :: KernelResultValue -> InterpM rep Val
expectKernelValue (KernelValue result_value) = pure result_value
expectKernelValue KernelTile {} =
  interpError "TileReturns cannot be used as a segmented contribution"
expectKernelValue KernelRegTile {} =
  interpError "RegTileReturns cannot be used as a segmented contribution"

segmentRows :: Int -> Int -> [a] -> [[a]]
segmentRows segment_count segment_width =
  go segment_count
  where
    go remaining rows
      | remaining <= 0 = []
      | otherwise =
          let (segment, rest) = splitAt segment_width rows
           in segment : go (remaining - 1) rest

splitSegContributions ::
  [Seg.SegBinOp rep] ->
  [Val] ->
  InterpM rep [[Val]]
splitSegContributions operators values = do
  let sizes = map (length . Seg.segBinOpNeutral) operators
  (groups, extra) <- splitGroups sizes values
  if null extra
    then pure groups
    else interpError "segmented operator received too many values"

initialSegBinOp ::
  Env ->
  Seg.SegBinOp rep ->
  InterpM rep [Val]
initialSegBinOp env operator = do
  neutral <- mapM (evalSubExp env) $ Seg.segBinOpNeutral operator
  vector_shape <- evalShape env $ Seg.segBinOpShape operator

  if null vector_shape
    then pure neutral
    else
      collectOutputs
        env
        vector_shape
        (lambdaReturnType $ Seg.segBinOpLambda operator)
        (replicate (product vector_shape) neutral)

applySegBinOp ::
  FunEnv rep ->
  Env ->
  Seg.SegBinOp rep ->
  [Val] ->
  [Val] ->
  InterpM rep [Val]
applySegBinOp funs env operator state contribution = do
  vector_shape <- evalShape env $ Seg.segBinOpShape operator
  let operator_lambda = Seg.segBinOpLambda operator

  if length state /= length contribution
    then interpError "segmented operator argument count mismatch"
    else
      if null vector_shape
        then evalLambda funs env operator_lambda $ state <> contribution
        else do
          let coordinates =
                sequence [[0 .. size - 1] | size <- vector_shape]

          result_rows <-
            mapM
              ( \coordinate -> do
                  state_elements <-
                    mapM (readAccumulatorElement coordinate) state
                  contribution_elements <-
                    mapM
                      (readAccumulatorElement coordinate)
                      contribution
                  evalLambda
                    funs
                    env
                    operator_lambda
                    (state_elements <> contribution_elements)
              )
              coordinates

          collectOutputs
            env
            vector_shape
            (lambdaReturnType operator_lambda)
            result_rows

initialSegStates ::
  Env ->
  [Seg.SegBinOp rep] ->
  InterpM rep [[Val]]
initialSegStates env =
  mapM $ initialSegBinOp env

updateSegStates ::
  FunEnv rep ->
  Env ->
  [Seg.SegBinOp rep] ->
  [[Val]] ->
  [[Val]] ->
  InterpM rep [[Val]]
updateSegStates funs env operators states contributions
  | length operators /= length states
      || length operators /= length contributions =
      interpError "segmented operator count mismatch"
  | otherwise =
      zipWithM
        ( \operator (state, contribution) ->
            applySegBinOp funs env operator state contribution
        )
        operators
        (zip states contributions)

evalSegOp ::
  FunEnv rep -> Env -> Seg.SegOp level rep -> InterpM rep [Val]
evalSegOp funs env (Seg.SegMap _ space types body) = do
  (shape, indexed_rows) <- evalSegSpace funs env space body
  collectSegOutputs env shape types (bodyResult body) indexed_rows
evalSegOp
  funs
  env
  (Seg.SegRed _ space types body operators) = do
    (shape, indexed_rows) <- evalSegSpace funs env space body

    case shape of
      [] ->
        interpError "SegRed requires a nonempty index space"
      _ -> do
        let segment_shape = init shape
            segment_width = last shape
            segment_count = product segment_shape
            reduction_count = Seg.segBinOpResults operators
            reduction_types = take reduction_count types
            map_types = drop reduction_count types
            reduction_result_rows =
              map (take reduction_count . snd) indexed_rows

        reduction_rows <-
          mapM (mapM expectKernelValue) reduction_result_rows

        let segments =
              segmentRows
                segment_count
                segment_width
                reduction_rows

        reduced_rows <- mapM reduceSegment segments

        reduction_outputs <-
          collectOutputs
            env
            segment_shape
            reduction_types
            reduced_rows

        map_outputs <-
          collectSegOutputs
            env
            shape
            map_types
            (drop reduction_count $ bodyResult body)
            [ (coordinate, drop reduction_count values)
            | (coordinate, values) <- indexed_rows
            ]

        pure $ reduction_outputs <> map_outputs
    where
      reduceSegment rows = do
        initial_states <- initialSegStates env operators
        final_states <- foldM reduceRow initial_states rows
        pure $ concat final_states

      reduceRow states values = do
        contributions <-
          splitSegContributions operators values
        updateSegStates funs env operators states contributions
evalSegOp
  funs
  env
  (Seg.SegScan _ space _ body operators post_operator) = do
    (shape, indexed_rows) <- evalSegSpace funs env space body
    worker_rows <- mapM evaluateWorker indexed_rows

    case shape of
      [] ->
        interpError "SegScan requires a nonempty index space"
      _ -> do
        let segment_shape = init shape
            segment_width = last shape
            segment_count = product segment_shape
            segments =
              segmentRows
                segment_count
                segment_width
                worker_rows

        output_rows <- concat <$> mapM (scanSegment shape) segments

        collectOutputs
          env
          shape
          (lambdaReturnType post_lambda)
          output_rows
    where
      evaluateWorker (coordinate, values) = do
        values' <- mapM expectKernelValue values
        pure (coordinate, values')

      contribution_count = Seg.segBinOpResults operators
      post_lambda =
        Seg.segPostOpLambda post_operator
      scanSegment shape rows = do
        initial_states <- initialSegStates env operators
        (_, reversed_outputs) <-
          foldM
            (scanRow shape)
            (initial_states, [])
            rows
        pure $ reverse reversed_outputs

      scanRow shape (states, outputs) (coordinate, values) = do
        let (contribution_values, map_values) =
              splitAt contribution_count values

        contributions <-
          splitSegContributions operators contribution_values

        next_states <-
          updateSegStates
            funs
            env
            operators
            states
            contributions

        post_results <-
          evalLambda
            funs
            (segSpaceEnv env space shape coordinate)
            post_lambda
            (concat next_states <> map_values)

        pure (next_states, post_results : outputs)
evalSegOp
  funs
  env
  (Seg.SegHist _ space _ body operators) = do
    (shape, indexed_rows) <- evalSegSpace funs env space body

    case shape of
      [] ->
        interpError "SegHist requires a nonempty index space"
      _ -> do
        initial_histograms <- mapM initialHistogramsFor operators

        final_histograms <-
          foldM
            updateFromWorker
            initial_histograms
            indexed_rows

        pure $ concat final_histograms
    where
      index_counts =
        map (shapeRank . Seg.histShape) operators

      value_counts =
        map (length . Seg.histDest) operators

      initialHistogramsFor operator =
        mapM lookupHistogram $ Seg.histDest operator

      lookupHistogram name =
        case M.lookup name env of
          Just histogram@ArrayValue {} ->
            pure histogram
          Just PrimVal {} ->
            interpError "SegHist destination must be an array"
          Just AccValue {} ->
            interpError "SegHist destination must be an array"
          Nothing ->
            interpError $
              "unbound SegHist destination: " <> prettyText name

      updateFromWorker histograms (coordinate, worker_values) = do
        let segment_indices = init coordinate

        worker_values' <- mapM expectKernelValue worker_values

        (index_groups, remaining) <-
          splitGroups index_counts worker_values'
        (value_groups, extra) <-
          splitGroups value_counts remaining

        if not $ null extra
          then
            interpError "SegHist kernel body returned too many values"
          else do
            evaluated_indices <-
              mapM
                (mapM (expectPrimVal >=> expectInt))
                index_groups

            if length operators /= length evaluated_indices
              || length operators /= length value_groups
              || length operators /= length histograms
              then
                interpError "SegHist operation count mismatch"
              else
                mapM
                  ( \(operator, bucket_indices, values_and_histograms) ->
                      updateHistogram
                        segment_indices
                        operator
                        bucket_indices
                        values_and_histograms
                  )
                  ( zip3
                      operators
                      evaluated_indices
                      (zip value_groups histograms)
                  )

      updateHistogram
        segment_indices
        operator
        bucket_indices
        (new_values, histograms)
          | length histograms /= length (Seg.histDest operator) =
              interpError "SegHist destination count mismatch"
          | not $ indicesInBounds full_indices histograms =
              pure histograms
          | otherwise = do
              vector_shape <- evalShape env $ Seg.histOpShape operator
              old_values <- mapM (readHistogramBin full_indices) histograms

              replacement_values <-
                if null vector_shape
                  then
                    evalLambda
                      funs
                      env
                      (Seg.histOp operator)
                      (old_values <> new_values)
                  else do
                    let coordinates =
                          sequence [[0 .. size - 1] | size <- vector_shape]

                    result_rows <-
                      mapM
                        ( \coordinate -> do
                            old_elements <-
                              mapM (readAccumulatorElement coordinate) old_values
                            new_elements <-
                              mapM (readAccumulatorElement coordinate) new_values
                            evalLambda
                              funs
                              env
                              (Seg.histOp operator)
                              (old_elements <> new_elements)
                        )
                        coordinates

                    collectOutputs
                      env
                      vector_shape
                      (lambdaReturnType $ Seg.histOp operator)
                      result_rows

              if length replacement_values /= length histograms
                then
                  interpError "SegHist operator result count mismatch"
                else
                  zipWithM
                    (writeHistogramBin full_indices)
                    histograms
                    replacement_values
          where
            full_indices =
              segment_indices <> bucket_indices

      indicesInBounds indices =
        all $ histogramIndicesInBounds indices

      histogramIndicesInBounds
        indices
        (ArrayValue histogram_shape _ _) =
          length indices <= length histogram_shape
            && and
              (zipWith validIndex indices histogram_shape)
      histogramIndicesInBounds _ _ =
        False

      validIndex index dimension =
        index >= 0 && index < dimension

splitGroups :: [Int] -> [a] -> InterpM rep ([[a]], [a])
splitGroups [] values =
  pure ([], values)
splitGroups (size : sizes) values
  | length group /= size =
      interpError "Screma lambda returned too few values"
  | otherwise = do
      (groups, remaining) <- splitGroups sizes rest
      pure (group : groups, remaining)
  where
    (group, rest) = splitAt size values

evalLambda ::
  FunEnv rep ->
  Env ->
  GLambda rep return_type ->
  [Val] ->
  InterpM rep [Val]
evalLambda funs env (Lambda params return_types body) args
  | length params /= length args =
      interpError "lambda argument count mismatch"
  | otherwise = do
      let bindings =
            M.fromList $ zip (map paramName params) args
          lambda_env =
            M.union bindings env

      results <- evalBody funs lambda_env body

      if length results /= length return_types
        then interpError "lambda result count mismatch"
        else pure results

updateScanStates ::
  FunEnv rep ->
  Env ->
  [Scan rep] ->
  [[Val]] ->
  [[Val]] ->
  InterpM rep [[Val]]
updateScanStates funs env scans states contributions
  | length scans /= length states
      || length scans /= length contributions =
      interpError "Screma scan state count mismatch"
  | otherwise =
      zipWithM updateOne scans (zip states contributions)
  where
    updateOne scan (state, contribution) = do
      next <-
        evalLambda
          funs
          env
          (scanLambda scan)
          (state <> contribution)

      if length next /= length state
        then interpError "scan result count mismatch"
        else pure next

updateReductionStates ::
  FunEnv rep ->
  Env ->
  [Reduce rep] ->
  [[Val]] ->
  [[Val]] ->
  InterpM rep [[Val]]
updateReductionStates funs env reductions states contributions
  | length reductions /= length states
      || length reductions /= length contributions =
      interpError "Screma reduction state count mismatch"
  | otherwise =
      zipWithM updateOne reductions (zip states contributions)
  where
    updateOne reduction (state, contribution) = do
      next <-
        evalLambda
          funs
          env
          (redLambda reduction)
          (state <> contribution)

      if length next /= length state
        then interpError "reduction result count mismatch"
        else pure next

lookupSoacInput :: Env -> VName -> InterpM rep Val
lookupSoacInput env name =
  case M.lookup name env of
    Just array@ArrayValue {} ->
      pure array
    Just val@AccValue {} ->
      pure val
    Just PrimVal {} ->
      interpError "Screma input must be an array"
    Nothing ->
      interpError $ "unbound Screma input: " <> prettyText name

validateSoacInput :: Int -> Val -> InterpM rep ()
validateSoacInput width (ArrayValue shape _ values) =
  case shape of
    outer_size : _
      | outer_size /= width ->
          interpError "Screma input outer size mismatch"
      | arrayValuesLength values /= product shape ->
          interpError "invalid Screma input storage"
      | otherwise ->
          pure ()
    [] ->
      interpError "Screma input must have positive rank"
validateSoacInput _ PrimVal {} =
  interpError "Screma input must be an array"
validateSoacInput _ AccValue {} = pure ()

rowAt :: Int -> Val -> InterpM rep Val
rowAt index (ArrayValue (_ : row_shape) element_type values)
  | null row_shape =
      PrimVal <$> readArrayValue values index
  | otherwise = do
      let row_size = product row_shape
          offset = index * row_size
      row_values <-
        mapM (readArrayValue values) [offset .. offset + row_size - 1]
      newArrayValue row_shape element_type row_values
rowAt _ accumulator@AccValue {} =
  pure accumulator
rowAt _ _ =
  interpError "cannot extract a row from this value"

readAccumulatorElement :: [Int] -> Val -> InterpM rep Val
readAccumulatorElement indices (ArrayValue shape element_type values)
  | length indices > length shape =
      interpError "accumulator index rank exceeds array rank"
  | arrayValuesLength values /= product shape =
      interpError "invalid accumulator backing-array storage"
  | otherwise = do
      let index_rank = length indices
          index_shape = take index_rank shape
          element_shape = drop index_rank shape
          element_size = product element_shape
          offset = linearIndex index_shape indices * element_size
      element_values <- mapM (readArrayValue values) [offset .. offset + element_size - 1]
      case element_shape of
        [] ->
          case element_values of
            [val] -> pure $ PrimVal val
            _ -> interpError "invalid scalar accumulator element"
        _ -> newArrayValue element_shape element_type element_values
readAccumulatorElement _ _ =
  interpError "accumulator backing value must be an array"

writeAccumulatorElementInPlace :: [Int] -> Val -> Val -> InterpM rep ()
writeAccumulatorElementInPlace
  indices
  (ArrayValue shape element_type values)
  replacement
    | length indices > length shape =
        interpError "accumulator index rank exceeds array rank"
    | otherwise = do
        let index_rank = length indices
            index_shape = take index_rank shape
            element_shape = drop index_rank shape
            element_size = product element_shape
            offset = linearIndex index_shape indices * element_size

        replacement_values <-
          updateValues element_type element_shape replacement

        mapM_
          (uncurry $ writeArrayValue values)
          (zip [offset ..] replacement_values)
writeAccumulatorElementInPlace _ _ _ =
  interpError "accumulator backing value must be an array"

readHistogramBin :: [Int] -> Val -> InterpM rep Val
readHistogramBin indices (ArrayValue shape element_type values) = do
  let rank = length indices
      bin_shape = drop rank shape
      bin_size = product bin_shape
      offset = linearIndex (take rank shape) indices * bin_size
  bin_values <- mapM (readArrayValue values) [offset .. offset + bin_size - 1]
  case bin_shape of
    [] ->
      case bin_values of
        [val] -> pure $ PrimVal val
        _ -> interpError "invalid scalar Hist bin"
    _ ->
      if length bin_values == bin_size
        then newArrayValue bin_shape element_type bin_values
        else interpError "invalid Hist bin storage"
readHistogramBin _ PrimVal {} =
  interpError "Hist destination must be an array"
readHistogramBin _ AccValue {} =
  interpError "Hist destination must be an array"

writeHistogramBin :: [Int] -> Val -> Val -> InterpM rep Val
writeHistogramBin
  indices
  (ArrayValue shape element_type values)
  replacement = do
    let rank = length indices
        bin_shape = drop rank shape
        bin_size = product bin_shape
        offset = linearIndex (take rank shape) indices * bin_size

    replacement_values <-
      updateValues element_type bin_shape replacement

    if length replacement_values /= bin_size
      then interpError "invalid Hist operator result storage"
      else do
        mapM_
          (uncurry $ writeArrayValue values)
          (zip [offset ..] replacement_values)

        pure $ ArrayValue shape element_type values
writeHistogramBin _ PrimVal {} _ =
  interpError "Hist destination must be an array"
writeHistogramBin _ AccValue {} _ =
  interpError "Hist destination must be an array"

collectFlatMapOutput ::
  Env ->
  [Int] ->
  Int ->
  ExtType ->
  [Val] ->
  InterpM rep Val
collectFlatMapOutput env sizes total_size result_type rows
  | flatMapNonuniform result_type =
      collectNonuniform
  | otherwise =
      collectUniform
  where
    collectNonuniform = do
      arrays <- zipWithM expectSegment sizes rows

      case arrays of
        [] -> do
          (element_type, row_shape) <- emptyArrayType True
          newArrayValue (total_size : row_shape) element_type []
        (row_shape, element_type, values) : remaining
          | not $ all (sameArray row_shape element_type) remaining ->
              interpError "inconsistent FlatMap segment results"
          | otherwise ->
              newArrayValue
                (total_size : row_shape)
                element_type
                (values <> concatMap third remaining)

    collectUniform =
      case result_type of
        Prim expected_type -> do
          values <- mapM expectPrimitive rows
          if all ((== expected_type) . P.primValueType) values
            then newArrayValue [length rows] expected_type values
            else interpError "FlatMap uniform result type mismatch"
        Array expected_type _ _ ->
          case rows of
            [] -> do
              (_, row_shape) <- emptyArrayType False
              newArrayValue (0 : row_shape) expected_type []
            _ -> do
              arrays <- mapM expectArray rows
              case arrays of
                [] ->
                  interpError "internal empty FlatMap output"
                (row_shape, element_type, values) : remaining
                  | element_type /= expected_type ->
                      interpError "FlatMap uniform result type mismatch"
                  | not $ all (sameArray row_shape element_type) remaining ->
                      interpError "inconsistent FlatMap uniform results"
                  | otherwise ->
                      newArrayValue
                        (length rows : row_shape)
                        element_type
                        (values <> concatMap third remaining)
        Acc {} ->
          interpError "FlatMap accumulator outputs are unsupported"
        Mem {} ->
          interpError "FlatMap memory outputs are unsupported"

    expectSegment expected_size (ArrayValue (size : row_shape) element_type values)
      | size /= expected_size =
          interpError "FlatMap segment size does not match returned size"
      | arrayValuesLength values /= product (size : row_shape) =
          interpError "invalid FlatMap segment storage"
      | otherwise = do
          primitive_values <- arrayValues values
          pure (row_shape, element_type, primitive_values)
    expectSegment _ _ =
      interpError "nonuniform FlatMap result must be an array"

    expectPrimitive (PrimVal val) = pure val
    expectPrimitive ArrayValue {} =
      interpError "expected primitive FlatMap result"
    expectPrimitive AccValue {} =
      interpError "expected primitive FlatMap result"

    expectArray (ArrayValue shape element_type values)
      | arrayValuesLength values == product shape = do
          primitive_values <- arrayValues values
          pure (shape, element_type, primitive_values)
      | otherwise =
          interpError "invalid FlatMap result storage"
    expectArray PrimVal {} =
      interpError "expected array-valued FlatMap result"
    expectArray AccValue {} =
      interpError "expected array-valued FlatMap result"

    sameArray shape element_type (other_shape, other_type, _) =
      shape == other_shape
        && element_type == other_type

    third (_, _, values) = values

    emptyArrayType drop_existential =
      case result_type of
        Array element_type (Shape dimensions) _ -> do
          let dimensions'
                | drop_existential = drop 1 dimensions
                | otherwise = dimensions
          shape <- mapM evalFreeDimension dimensions'
          pure (element_type, shape)
        _ ->
          interpError "nonuniform FlatMap result must be an array"

    evalFreeDimension (Free dimension) =
      evalSubExp env dimension >>= expectPrimVal >>= expectInt
    evalFreeDimension (Ext _) =
      interpError "unexpected existential FlatMap result dimension"

collectOutputs ::
  Env ->
  [Int] ->
  [Type] ->
  [[Val]] ->
  InterpM rep [Val]
collectOutputs env outer_shape return_types iteration_results
  | any ((/= length return_types) . length) iteration_results =
      interpError "inconsistent output count"
  | otherwise =
      zipWithM collectOne return_types columns
  where
    columns
      | null iteration_results =
          replicate (length return_types) []
      | otherwise =
          L.transpose iteration_results

    collectOne (Prim expected_type) rows = do
      values <- mapM expectPrimitive rows
      if not $ all ((== expected_type) . P.primValueType) values
        then interpError "primitive output type mismatch"
        else case (outer_shape, values) of
          ([], [val]) ->
            pure $ PrimVal val
          ([], _) ->
            interpError "invalid scalar output count"
          _ ->
            newArrayValue outer_shape expected_type values
    collectOne (Array expected_type annotated_shape _) [] = do
      row_shape <-
        mapM
          ( \dimension ->
              evalSubExp env dimension >>= expectPrimVal >>= expectInt
          )
          (shapeDims annotated_shape)
      newArrayValue (outer_shape <> row_shape) expected_type []
    collectOne (Array expected_type _ _) rows = do
      evaluated_rows <- mapM expectArray rows
      case evaluated_rows of
        [] ->
          interpError "internal empty output"
        (first_shape, first_type, first_values) : remaining
          | first_type /= expected_type ->
              interpError "array output type mismatch"
          | not $ all (sameRow first_shape first_type) remaining ->
              interpError "inconsistent array output rows"
          | otherwise ->
              newArrayValue
                (outer_shape <> first_shape)
                expected_type
                (first_values <> concatMap third remaining)
    collectOne (Acc certificate _ _) [] =
      case M.lookup certificate env of
        Just accumulator@AccValue {} -> pure accumulator
        _ -> interpError "accumulator output has no backing storage"
    collectOne Acc {} rows@(row : _) =
      row <$ mapM_ expectAccumulator rows
    collectOne Mem {} _ =
      interpError "memory outputs are unsupported"

    expectPrimitive (PrimVal val) = pure val
    expectPrimitive _ = interpError "expected primitive output"

    expectArray (ArrayValue shape element_type values)
      | arrayValuesLength values == product shape = do
          primitive_values <- arrayValues values
          pure (shape, element_type, primitive_values)
      | otherwise =
          interpError "invalid array output storage"
    expectArray _ = interpError "expected array output"

    expectAccumulator AccValue {} = pure ()
    expectAccumulator _ = interpError "expected accumulator output"

    sameRow expected_shape expected_type (shape, element_type, values) =
      shape == expected_shape
        && element_type == expected_type
        && length values == product shape

    third (_, _, values) = values

collectScremaOutputs :: Env -> Int -> [Type] -> [[Val]] -> InterpM rep [Val]
collectScremaOutputs env width = collectOutputs env [width]

collectSegOutputs ::
  Env ->
  [Int] ->
  [Type] ->
  [Seg.KernelResult] ->
  [([Int], [KernelResultValue])] ->
  InterpM rep [Val]
collectSegOutputs env worker_shape return_types result_specs worker_rows
  | length return_types /= length result_specs =
      interpError "segmented result type count mismatch"
  | any ((/= length return_types) . length . snd) worker_rows =
      interpError "inconsistent segmented output count"
  | null worker_rows =
      zipWithM collectEmpty return_types result_specs
  | otherwise =
      zipWithM collectOne return_types columns
  where
    columns =
      [ [(coordinate, values !! result_index) | (coordinate, values) <- worker_rows]
      | result_index <- [0 .. length return_types - 1]
      ]

    collectEmpty result_type result_spec = do
      output_shape <- resultShape result_spec
      collectOutputs env output_shape [result_type] [] >>= expectOne

    collectOne result_type rows =
      case rows of
        [] -> interpError "internal empty segmented output"
        (_, KernelValue {}) : _ -> do
          values <- mapM (expectOrdinary . snd) rows
          collectOutputs env worker_shape [result_type] (map pure values)
            >>= expectOne
        (_, KernelTile dimensions _) : _ -> do
          let output_shape = map fst dimensions
              tile_sizes = map snd dimensions
          validateTileDimensions output_shape tile_sizes
          collectTiled
            result_type
            output_shape
            rows
            (tileSourceIndices tile_sizes)
        (_, KernelRegTile dimensions _) : _ -> do
          let output_shape = map firstOf3 dimensions
              block_sizes = map secondOf3 dimensions
              reg_sizes = map thirdOf3 dimensions
          validateTileDimensions output_shape block_sizes
          validateTileDimensions output_shape reg_sizes
          collectTiled
            result_type
            output_shape
            rows
            (regTileSourceIndices block_sizes reg_sizes)

    collectTiled result_type output_shape rows source_indices = do
      let output_coordinates =
            sequence [[0 .. size - 1] | size <- output_shape]
      values <- mapM (tiledValueAt rows source_indices) output_coordinates
      collectOutputs env output_shape [result_type] (map pure values)
        >>= expectOne

    tiledValueAt rows source_indices output_coordinate = do
      let (worker_coordinate, source_coordinate) =
            source_indices output_coordinate
      result <-
        maybe
          (interpError "missing worker for tiled result")
          pure
          (lookup worker_coordinate rows)
      tile <- case result of
        KernelTile _ tile_value -> pure tile_value
        KernelRegTile _ tile_value -> pure tile_value
        KernelValue {} -> interpError "inconsistent segmented result layout"
      readAccumulatorElement source_coordinate tile

    tileSourceIndices tile_sizes output_coordinate =
      ( zipWith div output_coordinate tile_sizes,
        zipWith mod output_coordinate tile_sizes
      )

    regTileSourceIndices block_sizes reg_sizes output_coordinate =
      (workers, block_indices <> reg_indices)
      where
        tile_sizes = zipWith (*) block_sizes reg_sizes
        workers = zipWith div output_coordinate tile_sizes
        within_tiles = zipWith mod output_coordinate tile_sizes
        block_indices = zipWith div within_tiles reg_sizes
        reg_indices = zipWith mod within_tiles reg_sizes

    resultShape Seg.Returns {} = pure worker_shape
    resultShape (Seg.TileReturns _ dimensions _) =
      mapM (evalInt . fst) dimensions
    resultShape (Seg.RegTileReturns _ dimensions _) =
      mapM (evalInt . firstOf3) dimensions

    validateTileDimensions output_shape tile_sizes
      | any (< 0) output_shape =
          interpError "negative tiled result dimension"
      | any (<= 0) tile_sizes =
          interpError "nonpositive tile size"
      | otherwise = pure ()

    evalInt sub_exp =
      evalSubExp env sub_exp >>= expectPrimVal >>= expectInt

    expectOrdinary (KernelValue result_value) = pure result_value
    expectOrdinary _ = interpError "inconsistent segmented result layout"

    expectOne [result_value] = pure result_value
    expectOne _ = interpError "invalid segmented output count"

    firstOf3 (first, _, _) = first
    secondOf3 (_, second, _) = second
    thirdOf3 (_, _, third) = third

-- | Run a program in the IR specified by rep.
runProgram ::
  (Typed (FParamInfo rep)) =>
  OpEvaluator rep ->
  Prog rep ->
  Name ->
  [V.Value] ->
  IO (Either T.Text [V.Value])
runProgram eval_op prog entry inputs = runExceptT $ flip runReaderT initial_interp_env . unInterpM $ do
  let funs = M.fromList [(funDefName fun, fun) | fun <- progFuns prog]
  consts_env <- foldConsts funs mempty (stmsToList (progConsts prog)) -- top-level consts
  fun <- findEntry prog entry
  converted_inputs <- mapM fromValue inputs
  let params = funDefParams fun
      value_count = length converted_inputs
      context_count = length params - value_count
  if context_count < 0
    then interpError "entry point argument count mismatch"
    else do
      let (context_params, value_params) = splitAt context_count params
      shape_bindings <-
        foldM bindInputShape mempty $ zip value_params converted_inputs
      context_args <- mapM (lookupContextArg shape_bindings) context_params
      let value_args = map snd converted_inputs
          arg_vals = context_args <> value_args
          env =
            M.union
              (M.fromList $ zip (map paramName params) arg_vals)
              consts_env
      results <- evalBody funs env (funDefBody fun)
      result_signedness <- entryResultSignedness prog fun
      let result_context_count = length results - length result_signedness
      if result_context_count < 0
        then interpError "entry point returned too few values"
        else
          zipWithM
            toValue
            result_signedness
            (drop result_context_count results)
  where
    initial_interp_env =
      InterpEnv
        { interpOpEvaluator = eval_op
        }

    foldConsts _ e [] = pure e
    foldConsts funs e (s : ss) = evalStm funs e s >>= \e' -> foldConsts funs e' ss

    bindInputShape bindings (param, (actual_dims, _))
      | length expected_dims /= length actual_dims =
          interpError "entry point input rank mismatch"
      | otherwise =
          foldM bindDimension bindings $ zip expected_dims actual_dims
      where
        expected_dims = arrayDims $ paramType param

    bindDimension bindings (Var name, actual_dim) =
      case M.lookup name bindings of
        Nothing -> pure $ M.insert name actual_dim bindings
        Just expected_dim
          | sameDimension expected_dim actual_dim -> pure bindings
          | otherwise -> interpError "entry point input shape mismatch"
    bindDimension bindings (Constant expected, PrimVal actual)
      | expected == actual = pure bindings
      | otherwise = interpError "entry point input shape mismatch"
    bindDimension _ _ =
      interpError "invalid entry point input dimension"

    lookupContextArg bindings param =
      maybe
        (interpError "unbound entry point shape parameter")
        pure
        (M.lookup (paramName param) bindings)

    sameDimension (PrimVal x) (PrimVal y) = x == y
    sameDimension _ _ = False

entryResultSignedness :: Prog rep -> FunDef rep -> InterpM rep [Signedness]
entryResultSignedness prog fun =
  case funDefEntryPoint fun of
    Just (_, _, result, _) ->
      entryPointTypeSignedness (progTypes prog) $ entryResultType result
    Nothing ->
      interpError "function is not an entry point"
  where
    entryPointTypeSignedness _ (TypeTransparent value_type) =
      pure [valueTypeSignedness value_type]
    entryPointTypeSignedness types@(OpaqueTypes opaque_types) (TypeOpaque name) =
      case lookup name opaque_types of
        Nothing -> interpError $ "unknown opaque type: " <> prettyText name
        Just (opaque_type, _) -> opaqueTypeSignedness types opaque_type

    opaqueTypeSignedness _ (OpaqueArray _ _ value_types) =
      pure $ map valueTypeSignedness value_types
    opaqueTypeSignedness types (OpaqueRecordArray _ _ fields) =
      concat <$> mapM (entryPointTypeSignedness types . snd) fields
    opaqueTypeSignedness _ (OpaqueRecord []) =
      pure [Signed]
    opaqueTypeSignedness types (OpaqueRecord fields) =
      concat <$> mapM (entryPointTypeSignedness types . snd) fields
    opaqueTypeSignedness _ (OpaqueSum value_types _) =
      pure $ map valueTypeSignedness value_types

    valueTypeSignedness (ValueType signedness _ _) = signedness

findEntry :: Prog rep -> Name -> InterpM rep (FunDef rep)
findEntry prog name =
  maybe
    (interpError $ "entry point not found: " <> prettyText name)
    pure
    $ lookup
      name
      [ (entry_name, fun)
      | fun <- progFuns prog,
        Just (entry_name, _, _, _) <- [funDefEntryPoint fun]
      ]

fromValue :: V.Value -> InterpM rep ([Val], Val)
fromValue (V.I8Value shape values) =
  fromPrimitiveVector shape (IntType Int8) (IntValue . Int8Value) values
fromValue (V.I16Value shape values) =
  fromPrimitiveVector shape (IntType Int16) (IntValue . Int16Value) values
fromValue (V.I32Value shape values) =
  fromPrimitiveVector shape (IntType Int32) (IntValue . Int32Value) values
fromValue (V.I64Value shape values) =
  fromPrimitiveVector shape (IntType Int64) (IntValue . Int64Value) values
fromValue (V.U8Value shape values) =
  fromPrimitiveVector shape (IntType Int8) (IntValue . Int8Value . fromIntegral) values
fromValue (V.U16Value shape values) =
  fromPrimitiveVector shape (IntType Int16) (IntValue . Int16Value . fromIntegral) values
fromValue (V.U32Value shape values) =
  fromPrimitiveVector shape (IntType Int32) (IntValue . Int32Value . fromIntegral) values
fromValue (V.U64Value shape values) =
  fromPrimitiveVector shape (IntType Int64) (IntValue . Int64Value . fromIntegral) values
fromValue (V.F16Value shape values) =
  fromPrimitiveVector shape (FloatType Float16) (FloatValue . Float16Value) values
fromValue (V.F32Value shape values) =
  fromPrimitiveVector shape (FloatType Float32) (FloatValue . Float32Value) values
fromValue (V.F64Value shape values) =
  fromPrimitiveVector shape (FloatType Float64) (FloatValue . Float64Value) values
fromValue (V.BoolValue shape values) =
  fromPrimitiveVector shape Bool BoolValue values

fromPrimitiveVector ::
  (Storable a) =>
  SVec.Vector Int ->
  PrimType ->
  (a -> PrimValue) ->
  SVec.Vector a ->
  InterpM rep ([Val], Val)
fromPrimitiveVector shape element_type wrap values
  | any (< 0) dimensions =
      interpError "input array dimensions cannot be negative"
  | null dimensions =
      case primitive_values of
        [primitive_value] -> pure ([], PrimVal primitive_value)
        _ -> interpError "invalid scalar input storage"
  | length primitive_values /= product dimensions =
      interpError "invalid input array storage"
  | otherwise = do
      array <- newArrayValue dimensions element_type primitive_values
      pure
        ( map
            (PrimVal . IntValue . Int64Value . fromIntegral)
            dimensions,
          array
        )
  where
    dimensions = SVec.toList shape
    primitive_values = map wrap $ SVec.toList values

toValue :: Signedness -> Val -> InterpM rep V.Value
toValue signedness (PrimVal primitive_value) =
  toPrimitiveValue signedness [] (P.primValueType primitive_value) [primitive_value]
toValue signedness (ArrayValue shape element_type values) = do
  primitive_values <- arrayValues values
  toPrimitiveValue signedness shape element_type primitive_values
toValue _ AccValue {} =
  interpError "accumulators cannot be represented as external values"

toPrimitiveValue :: Signedness -> [Int] -> PrimType -> [PrimValue] -> InterpM rep V.Value
toPrimitiveValue Signed shape (IntType Int8) values =
  V.I8Value (shapeVector shape) . SVec.fromList <$> mapM expectInt8 values
  where
    expectInt8 (IntValue (Int8Value element)) = pure element
    expectInt8 _ = interpError "expected an i8 value"
toPrimitiveValue Signed shape (IntType Int16) values =
  V.I16Value (shapeVector shape) . SVec.fromList <$> mapM expectInt16 values
  where
    expectInt16 (IntValue (Int16Value element)) = pure element
    expectInt16 _ = interpError "expected an i16 value"
toPrimitiveValue Signed shape (IntType Int32) values =
  V.I32Value (shapeVector shape) . SVec.fromList <$> mapM expectInt32 values
  where
    expectInt32 (IntValue (Int32Value element)) = pure element
    expectInt32 _ = interpError "expected an i32 value"
toPrimitiveValue Signed shape (IntType Int64) values =
  V.I64Value (shapeVector shape) . SVec.fromList <$> mapM expectInt64 values
  where
    expectInt64 (IntValue (Int64Value element)) = pure element
    expectInt64 _ = interpError "expected an i64 value"
toPrimitiveValue Unsigned shape (IntType Int8) values =
  V.U8Value (shapeVector shape) . SVec.fromList <$> mapM expectUInt8 values
  where
    expectUInt8 (IntValue (Int8Value element)) = pure $ fromIntegral element
    expectUInt8 _ = interpError "expected a u8 value"
toPrimitiveValue Unsigned shape (IntType Int16) values =
  V.U16Value (shapeVector shape) . SVec.fromList <$> mapM expectUInt16 values
  where
    expectUInt16 (IntValue (Int16Value element)) = pure $ fromIntegral element
    expectUInt16 _ = interpError "expected a u16 value"
toPrimitiveValue Unsigned shape (IntType Int32) values =
  V.U32Value (shapeVector shape) . SVec.fromList <$> mapM expectUInt32 values
  where
    expectUInt32 (IntValue (Int32Value element)) = pure $ fromIntegral element
    expectUInt32 _ = interpError "expected a u32 value"
toPrimitiveValue Unsigned shape (IntType Int64) values =
  V.U64Value (shapeVector shape) . SVec.fromList <$> mapM expectUInt64 values
  where
    expectUInt64 (IntValue (Int64Value element)) = pure $ fromIntegral element
    expectUInt64 _ = interpError "expected a u64 value"
toPrimitiveValue _ shape (FloatType Float16) values =
  V.F16Value (shapeVector shape) . SVec.fromList <$> mapM expectFloat16 values
  where
    expectFloat16 (FloatValue (Float16Value element)) = pure element
    expectFloat16 _ = interpError "expected an f16 value"
toPrimitiveValue _ shape (FloatType Float32) values =
  V.F32Value (shapeVector shape) . SVec.fromList <$> mapM expectFloat32 values
  where
    expectFloat32 (FloatValue (Float32Value element)) = pure element
    expectFloat32 _ = interpError "expected an f32 value"
toPrimitiveValue _ shape (FloatType Float64) values =
  V.F64Value (shapeVector shape) . SVec.fromList <$> mapM expectFloat64 values
  where
    expectFloat64 (FloatValue (Float64Value element)) = pure element
    expectFloat64 _ = interpError "expected an f64 value"
toPrimitiveValue _ shape Bool values =
  V.BoolValue (shapeVector shape) . SVec.fromList <$> mapM expectBool values
  where
    expectBool (BoolValue element) = pure element
    expectBool _ = interpError "expected a bool value"
toPrimitiveValue _ _ Unit _ =
  interpError "unit values cannot be represented as external values"

shapeVector :: [Int] -> SVec.Vector Int
shapeVector = SVec.fromList

-- | Run a program in the SOAC IR
runSOACS ::
  Prog SOACS ->
  Name ->
  [V.Value] ->
  IO (Either T.Text [V.Value])
runSOACS =
  runProgram evalSOAC

evalGPUOp :: OpEvaluator GPU
evalGPUOp funs env (SegOp op) = evalSegOp funs env op
evalGPUOp _ env (SizeOp op) = evalSizeOp env op
evalGPUOp funs env (OtherOp op) = evalSOAC funs env op
evalGPUOp funs env (GPUBody types body) = do
  values <- evalBody funs env body
  collectOutputs env [1] types [values]

sizeValue :: SizeClass -> Int
sizeValue (SizeThreshold _ (Just n)) = fromIntegral n
sizeValue SizeThreadBlock = 256
sizeValue SizeGrid = 65535
sizeValue SizeTile = 32
sizeValue SizeRegTile = 4
sizeValue SizeSharedMemory = 48 * 1024
sizeValue SizeRegisters = 65536
sizeValue SizeCache = 4 * 1024 * 1024
sizeValue _ = 32768

evalSizeOp :: Env -> SizeOp -> InterpM rep [Val]
evalSizeOp _ (GetSize _ cls) =
  pure [int64Val $ sizeValue cls]
evalSizeOp _ (GetSizeMax cls) =
  pure [int64Val $ sizeValue cls]
evalSizeOp env (CmpSizeLe _ cls x) = do
  limit <- evalSubExp env x >>= expectPrimVal >>= expectInt
  pure [PrimVal $ BoolValue $ sizeValue cls <= limit]
evalSizeOp env (CalcNumBlocks width_exp _ block_size_exp) = do
  width <- evalSubExp env width_exp >>= expectPrimVal >>= expectInt
  block_size <- evalSubExp env block_size_exp >>= expectPrimVal >>= expectInt
  if block_size <= 0
    then interpError "thread-block size must be positive"
    else
      pure
        [ int64Val $
            max 1 $
              min (sizeValue SizeGrid) $
                (width + block_size - 1) `div` block_size
        ]

-- | Run a program in the GPU IR.
runGPU :: Prog GPU -> Name -> [V.Value] -> IO (Either T.Text [V.Value])
runGPU = runProgram evalGPUOp
