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

import Control.Monad (foldM, forM, when, zipWithM, zipWithM_, (>=>))
import Control.Monad.Error.Class
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader, ReaderT, asks, local, runReaderT)
import Data.Int qualified as I
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
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
import Futhark.IR.SOACS (HistOp (..), Reduce (..), SOAC (..), SOACS, Scan (..), ScremaForm (..), flatMapNonuniform)
import Futhark.IR.SegOp qualified as Seg
import Futhark.Util (showText)
import Language.Futhark.Primitive qualified as P
import Numeric.Half qualified as H

data Val
  = PrimVal PrimValue
  | ArrayValue [Int] ArrayValues
  | AccValue Accumulator

-- The operator of an accumulator is looked up in 'interpAccOps' by
-- its certificate.
data Accumulator = Accumulator
  { accCert :: VName,
    accShape :: [Int],
    accArrays :: [Val]
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

arrayValuesType :: ArrayValues -> PrimType
arrayValuesType I8ArrayValues {} = IntType Int8
arrayValuesType I16ArrayValues {} = IntType Int16
arrayValuesType I32ArrayValues {} = IntType Int32
arrayValuesType I64ArrayValues {} = IntType Int64
arrayValuesType F16ArrayValues {} = FloatType Float16
arrayValuesType F32ArrayValues {} = FloatType Float32
arrayValuesType F64ArrayValues {} = FloatType Float64
arrayValuesType BoolArrayValues {} = Bool
arrayValuesType UnitArrayValues {} = Unit

data KernelResultValue
  = KernelValue Val
  | KernelTile [(Int, Int)] Val
  | KernelRegTile [(Int, Int, Int)] Val

type Env = M.Map VName Val

type FunEnv rep = M.Map Name (FunDef rep)

type OpEvaluator rep =
  FunEnv rep -> Env -> Op rep -> InterpM rep [Val]

data InterpEnv rep = InterpEnv
  { interpOpEvaluator :: OpEvaluator rep,
    -- | Accumulator operators, keyed by certificate, together with the
    -- environments of the 'WithAcc' that defined them. The operator
    -- receives (indices <> old values <> new values).
    interpAccOps :: M.Map VName (FunEnv rep, Env, Lambda rep)
  }

newtype InterpM rep a = InterpM
  { unInterpM :: ReaderT (InterpEnv rep) (ExceptT T.Text IO) a
  }
  deriving
    (Functor, Applicative, Monad, MonadReader (InterpEnv rep), MonadError T.Text, MonadIO)

interpError :: T.Text -> InterpM rep a
interpError = throwError

newArrayValue :: [Int] -> PrimType -> [PrimValue] -> InterpM rep Val
newArrayValue shape element_type values =
  ArrayValue shape <$> newValues element_type
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

    newPrimVector unwrap = liftIO $ do
      vector <- MSVec.new (length values)
      zipWithM_ (MSVec.write vector) [0 ..] (map unwrap values)
      pure vector

    expectInt8 (IntValue (Int8Value element)) = element
    expectInt8 _ = error "expected an i8 value"
    expectInt16 (IntValue (Int16Value element)) = element
    expectInt16 _ = error "expected an i16 value"
    expectInt32 (IntValue (Int32Value element)) = element
    expectInt32 _ = error "expected an i32 value"
    expectInt64 (IntValue (Int64Value element)) = element
    expectInt64 _ = error "expected an i64 value"
    expectFloat16 (FloatValue (Float16Value element)) = element
    expectFloat16 _ = error "expected an f16 value"
    expectFloat32 (FloatValue (Float32Value element)) = element
    expectFloat32 _ = error "expected an f32 value"
    expectFloat64 (FloatValue (Float64Value element)) = element
    expectFloat64 _ = error "expected an f64 value"
    expectBool (BoolValue element) = element
    expectBool _ = error "expected a bool value"
    expectUnit UnitValue = ()
    expectUnit _ = error "expected a unit value"

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
  error "array element type mismatch"

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
      KernelTile <$> mapM (evalPair env') dimensions <*> evalVar env' tile
    evalKernelResult env' (Seg.RegTileReturns _ dimensions tile) =
      KernelRegTile <$> mapM (evalTriple env') dimensions <*> evalVar env' tile

    evalPair env' (size, tile_size) =
      (,) <$> evalInt env' size <*> evalInt env' tile_size
    evalTriple env' (size, block_tile, reg_tile) =
      (,,)
        <$> evalInt env' size
        <*> evalInt env' block_tile
        <*> evalInt env' reg_tile

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

    matches patterns values = and $ zipWith matchesValue patterns values

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
          runIterations (iteration + 1) bound next_values
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

      condition_value <- evalVar loop_env condition

      case condition_value of
        PrimVal (BoolValue False) ->
          pure current_values
        PrimVal (BoolValue True) ->
          runWhile =<< evalBody funs loop_env body
        _ ->
          error "while-loop condition is not boolean"
evalExp funs env (Apply fname args _ _) = do
  arg_vals <- mapM (evalSubExp env . fst) args

  case M.lookup fname funs of
    Just callee ->
      let bindings = M.fromList $ zip (map paramName $ funDefParams callee) arg_vals
       in evalBody funs (M.union bindings env) (funDefBody callee)
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
      error $ "function not found: " <> prettyString fname
    Just (_, _, function) -> do
      values <- mapM expectPrimVal args
      case function values of
        Just result -> pure [PrimVal result]
        Nothing ->
          error $ "invalid arguments to primitive function: " <> prettyString fname

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
      certificates = map paramName certificate_params
      accumulators = zipWith mkAccumulator certificates evaluated_inputs
      operators =
        M.fromList . mapMaybe accOperator $ zip certificates evaluated_inputs
      -- Certificates are bound to their accumulator so that zero-iteration
      -- maps can recover it from the result type 'Acc c ...'.
      bindings =
        M.fromList $
          zip certificates accumulators
            <> zip (map paramName accumulator_params) accumulators

  results <-
    local (withAccOps operators) $
      evalBody funs (M.union bindings env) (lambdaBody lambda)
  pure $
    concatMap (\(_, arrays, _) -> arrays) evaluated_inputs
      <> drop accumulator_count results
  where
    mkAccumulator certificate (index_shape, arrays, _) =
      AccValue $ Accumulator certificate index_shape arrays

    accOperator (certificate, (_, _, Just (operator_lambda, _))) =
      Just (certificate, (funs, env, operator_lambda))
    accOperator _ = Nothing

    withAccOps new_operators interp_env =
      interp_env {interpAccOps = new_operators <> interpAccOps interp_env}

    evaluateInput (shape, array_names, operator) = do
      index_shape <- evalShape env shape
      arrays <- mapM (evalVar env) array_names
      pure (index_shape, arrays, operator)

evalVar :: Env -> VName -> InterpM rep Val
evalVar env v =
  maybe (error $ "unbound variable: " <> prettyString v) pure $ M.lookup v env

evalSubExp :: Env -> SubExp -> InterpM rep Val
evalSubExp _ (Constant pv) = pure $ PrimVal pv
evalSubExp env (Var v) = evalVar env v

expectPrimVal :: Val -> InterpM rep PrimValue
expectPrimVal (PrimVal pv) = pure pv
expectPrimVal _ = error "expected a primitive value"

expectInt :: PrimValue -> InterpM rep Int
expectInt (IntValue i) = pure $ P.valueIntegral i
expectInt _ = error "expected an integer value"

evalInt :: Env -> SubExp -> InterpM rep Int
evalInt env = evalSubExp env >=> expectPrimVal >=> expectInt

-- Safe division-like operations yield zero on a zero divisor, like the code generators.
evalBinOp :: BinOp -> PrimValue -> PrimValue -> Maybe PrimValue
evalBinOp op x y
  | safeDivision op, P.zeroIsh y = Just $ P.blankPrimValue $ P.binOpType op
  | otherwise = P.doBinOp op x y
  where
    safeDivision (UDiv _ Safe) = True
    safeDivision (UCeilDiv _ Safe) = True
    safeDivision (SDiv _ Safe) = True
    safeDivision (SCeilDiv _ Safe) = True
    safeDivision (UMod _ Safe) = True
    safeDivision (SMod _ Safe) = True
    safeDivision (SQuot _ Safe) = True
    safeDivision (SRem _ Safe) = True
    safeDivision _ = False

evalBasicOp :: Env -> BasicOp -> InterpM rep [Val]
evalBasicOp env (SubExp se) = pure <$> evalSubExp env se
evalBasicOp env (BinOp op x y) = do
  xv <- expectPrimVal =<< evalSubExp env x
  yv <- expectPrimVal =<< evalSubExp env y
  case evalBinOp op xv yv of
    Just result -> pure [PrimVal result]
    Nothing -> interpError "invalid binary operation"
evalBasicOp env (UnOp op x) = do
  xv <- expectPrimVal =<< evalSubExp env x
  case P.doUnOp op xv of
    Just result -> pure [PrimVal result]
    Nothing -> error "invalid unary operation"
evalBasicOp env (CmpOp op x y) = do
  xv <- expectPrimVal =<< evalSubExp env x
  yv <- expectPrimVal =<< evalSubExp env y
  case P.doCmpOp op xv yv of
    Just result -> pure [PrimVal $ BoolValue result]
    Nothing -> error "invalid comparison operation"
evalBasicOp env (ConvOp op x) = do
  xv <- expectPrimVal =<< evalSubExp env x
  case P.doConvOp op xv of
    Just result -> pure [PrimVal result]
    Nothing -> error "invalid conversion operation"
evalBasicOp env (ArrayLit elements (Prim element_type)) = do
  values <- mapM (evalSubExp env) elements
  primitive_values <- mapM expectPrimVal values
  pure <$> newArrayValue [length elements] element_type primitive_values
evalBasicOp env (ArrayLit elements (Array element_type row_shape_exps _)) = do
  row_shape <- evalShape env row_shape_exps
  rows <- mapM (evalSubExp env) elements
  row_values <- mapM valElements rows
  pure
    <$> newArrayValue
      (length elements : row_shape)
      element_type
      (concat row_values)
evalBasicOp _ (ArrayLit _ Acc {}) =
  error "accumulator array literals are not implemented"
evalBasicOp _ (ArrayLit _ Mem {}) =
  error "memory array literals are unsupported"
evalBasicOp _ (ArrayVal values element_type) =
  pure <$> newArrayValue [length values] element_type values
evalBasicOp env (Assert condition message) = do
  condition_value <- evalSubExp env condition >>= expectPrimVal
  case condition_value of
    BoolValue True -> pure [PrimVal UnitValue]
    BoolValue False -> interpError =<< evalErrorMsg env message
    _ -> error "assert condition is not boolean"
evalBasicOp env (Index array_name slice_exp) = do
  array <- evalVar env array_name
  slice <- evalSlice env slice_exp
  case array of
    ArrayValue shape values ->
      pure <$> indexArray shape values slice
    _ ->
      error "cannot index a non-array value"
evalBasicOp env (Reshape array_name reshape) = do
  array <- evalVar env array_name
  dimensions <- evalShape env $ newShape reshape
  case array of
    ArrayValue old_shape values ->
      case reshapeKind reshape of
        ReshapeCoerce
          | dimensions == old_shape ->
              pure [ArrayValue dimensions values]
          | otherwise ->
              error "coercion to a different shape"
        ReshapeArbitrary
          | product dimensions == arrayValuesLength values ->
              pure [ArrayValue dimensions values]
          | otherwise ->
              error "reshape element count mismatch"
    _ ->
      error "cannot reshape a non-array value"
evalBasicOp env (Opaque OpaqueNil se) =
  pure <$> evalSubExp env se
evalBasicOp env (Opaque (OpaqueTrace t) se) = do
  liftIO $ TIO.putStrLn t
  pure <$> evalSubExp env se
evalBasicOp env (Manifest array_name _) = do
  array <- evalVar env array_name
  case array of
    ArrayValue shape values ->
      pure . ArrayValue shape <$> liftIO (cloneArrayValues values)
    _ ->
      error "cannot manifest a non-array value"
evalBasicOp env (Iota count_sub_exp start_sub_exp stride_sub_exp int_type) = do
  count <- evalInt env count_sub_exp
  stride <- evalInt env stride_sub_exp
  start <- evalInt env start_sub_exp

  if count < 0
    then interpError "iota length cannot be negative"
    else do
      let values = do
            i <- [0 .. count - 1]
            pure $ IntValue $ P.intValue int_type $ start + i * stride
      pure <$> newArrayValue [count] (IntType int_type) values
evalBasicOp env (Replicate (Shape shape_exps) val_exp) = do
  dimensions <- mapM (evalInt env) shape_exps
  if any (< 0) dimensions
    then interpError " replicate dimensions cannot be negative"
    else do
      val <- evalSubExp env val_exp
      let copies = product dimensions

      case (dimensions, val) of
        ([], ArrayValue shape values) ->
          pure . ArrayValue shape <$> liftIO (cloneArrayValues values)
        ([], _) -> pure [val]
        (_, PrimVal primitive_value) ->
          pure <$> newArrayValue dimensions (P.primValueType primitive_value) (replicate copies primitive_value)
        (_, ArrayValue old_shape values) -> do
          primitive_values <- arrayValues values
          pure <$> newArrayValue (dimensions <> old_shape) (arrayValuesType values) (concat $ replicate copies primitive_values)
        (_, AccValue {}) ->
          error "cannot replicate an accumulator value"
evalBasicOp env (Rearrange array_name permutation) = do
  array <- evalVar env array_name
  case array of
    ArrayValue old_shape values -> do
      let new_shape = rearrangeShape permutation old_shape
          oldCoordinate = rearrangeShape $ rearrangeInverse permutation
      new_values <-
        mapM
          (readArrayValue values . linearIndex old_shape . oldCoordinate)
          (allCoordinates new_shape)
      pure <$> newArrayValue new_shape (arrayValuesType values) new_values
    _ -> error "cannot rearrange a non-array value"
evalBasicOp env (Concat concat_dim array_names result_size_exp) = do
  arrays <- mapM lookupArray $ NE.toList array_names
  declared_size <- evalInt env result_size_exp

  case arrays of
    [] ->
      error "concat requires at least one array"
    (first_shape, first_values) : _
      | declared_size /= actualSize arrays ->
          error "concat result size mismatch"
      | otherwise -> do
          let result_shape =
                replaceAt concat_dim declared_size first_shape

          result_values <- mapM (valueAt arrays) $ allCoordinates result_shape
          pure <$> newArrayValue result_shape (arrayValuesType first_values) result_values
  where
    lookupArray name = do
      array <- evalVar env name
      case array of
        ArrayValue shape values -> pure (shape, values)
        _ -> error "cannot concatenate a non-array value"

    actualSize =
      sum . map ((!! concat_dim) . fst)

    valueAt arrays coordinate = do
      let concat_index = coordinate !! concat_dim
          (source_shape, source_values, local_index) =
            findSource concat_index arrays
          source_coordinate =
            replaceAt concat_dim local_index coordinate
          offset =
            linearIndex source_shape source_coordinate

      readArrayValue source_values offset

    findSource _ [] =
      error "invalid concat coordinate"
    findSource index ((shape, values) : arrays)
      | index < size =
          (shape, values, index)
      | otherwise =
          findSource (index - size) arrays
      where
        size = shape !! concat_dim
evalBasicOp env (Update safety array_name slice_exp value_exp) = do
  array <- evalVar env array_name
  slice <- evalSlice env slice_exp
  replacement <- evalSubExp env value_exp

  case array of
    ArrayValue shape values -> do
      in_bounds <-
        case safety of
          Safe -> pure $ sliceInBounds shape slice
          Unsafe -> True <$ checkSlice shape slice
      when in_bounds $ do
        replacement_values <- valElements replacement
        zipWithM_
          (writeArrayValue values)
          (map (linearIndex shape) $ sliceCoordinates slice)
          replacement_values
      pure [array]
    _ ->
      error "cannot update a non-array value"
evalBasicOp env (FlatIndex array_name flat_slice) = do
  array <- evalVar env array_name
  (result_shape, offsets) <- evalFlatSlice env flat_slice

  case array of
    ArrayValue _ values -> do
      selected_values <- mapM (readArrayValue values) offsets
      case (result_shape, selected_values) of
        ([], [primitive_value]) -> pure [PrimVal primitive_value]
        ([], _) -> error "invalid scalar flat index"
        _ -> pure <$> newArrayValue result_shape (arrayValuesType values) selected_values
    _ ->
      error "cannot flat-index a non-array value"
evalBasicOp env (FlatUpdate source_name flat_slice replacement_name) = do
  source <- evalVar env source_name
  replacement <- evalVar env replacement_name
  (_, offsets) <- evalFlatSlice env flat_slice
  case source of
    ArrayValue _ source_values
      | not $ all (validOffset source_values) offsets ->
          interpError "flat update out of bounds"
      | otherwise -> do
          replacement_values <- valElements replacement
          zipWithM_ (writeArrayValue source_values) offsets replacement_values
          pure [source]
    _ ->
      error "cannot flat-update a non-array value"
  where
    validOffset values offset =
      offset >= 0 && offset < arrayValuesLength values
evalBasicOp env (Scratch element_type dimension_exps) = do
  dimensions <- mapM (evalInt env) dimension_exps
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
  accumulator <- evalVar env accumulator_name
  indices <- mapM (evalInt env) index_exps
  values <- mapM (evalSubExp env) value_exps

  case accumulator of
    AccValue acc -> do
      updateAccumulator acc indices values
      pure [accumulator]
    _ ->
      error "UpdateAcc argument is not an accumulator"
  where
    updateAccumulator acc indices new_values
      | not $ indicesInBounds (accShape acc) indices =
          case safety of
            Safe -> pure ()
            Unsafe -> interpError "unsafe accumulator update out of bounds"
      | otherwise = do
          operator <- asks $ M.lookup (accCert acc) . interpAccOps
          replacement_values <-
            case operator of
              Nothing -> pure new_values
              Just (operator_funs, operator_env, operator_lambda) -> do
                old_values <-
                  mapM (readAccumulatorElement indices) $ accArrays acc
                evalLambda operator_funs operator_env operator_lambda $
                  map int64Val indices <> old_values <> new_values

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
    evalPart (ErrorVal _ sub_exp) =
      renderErrorValue <$> (evalSubExp env sub_exp >>= expectPrimVal)

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
  offset <- evalInt env offset_exp
  evaluated_dimensions <- mapM evalDimension dimensions

  let result_shape = map fst evaluated_dimensions
      strides = map snd evaluated_dimensions
      offsets = map ((offset +) . sum . zipWith (*) strides) $ allCoordinates result_shape
  if any (< 0) result_shape
    then interpError "flat slice dimensions cannot be negative"
    else pure (result_shape, offsets)
  where
    evalDimension (FlatDimIndex size_exp stride_exp) = do
      size <- evalInt env size_exp
      stride <- evalInt env stride_exp
      pure (size, stride)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt index val xs =
  take index xs <> [val] <> drop (index + 1) xs

-- The elements of a value in row-major order.
valElements :: Val -> InterpM rep [PrimValue]
valElements (PrimVal primitive_value) = pure [primitive_value]
valElements (ArrayValue _ values) = arrayValues values
valElements AccValue {} = error "accumulators have no elements"

-- All coordinates of an array with the given shape, in row-major order.
allCoordinates :: [Int] -> [[Int]]
allCoordinates = mapM $ \size -> [0 .. size - 1]

evalSlice :: Env -> Slice SubExp -> InterpM rep (Slice Int)
evalSlice env = traverse $ evalInt env

sliceInBounds :: [Int] -> Slice Int -> Bool
sliceInBounds shape (Slice dimensions) =
  and $ zipWith dimensionInBounds shape dimensions
  where
    dimensionInBounds size (DimFix index) =
      validIndex size index
    dimensionInBounds size (DimSlice start count stride) =
      count == 0
        || (count > 0 && validIndex size start && validIndex size (start + (count - 1) * stride))

    validIndex size index = index >= 0 && index < size

checkSlice :: [Int] -> Slice Int -> InterpM rep ()
checkSlice shape slice
  | any (< 0) $ sliceDims slice =
      interpError "slice length cannot be negative"
  | not $ sliceInBounds shape slice =
      interpError "array index out of bounds"
  | otherwise =
      pure ()

-- The coordinates in the source array selected by a slice, in row-major order
-- of the result.
sliceCoordinates :: Slice Int -> [[Int]]
sliceCoordinates slice =
  map (fixSlice slice) $ allCoordinates $ sliceDims slice

indexArray :: [Int] -> ArrayValues -> Slice Int -> InterpM rep Val
indexArray shape values slice = do
  checkSlice shape slice
  selected_values <-
    mapM (readArrayValue values . linearIndex shape) $ sliceCoordinates slice
  case (sliceDims slice, selected_values) of
    ([], [primitive_value]) -> pure $ PrimVal primitive_value
    ([], _) -> error "invalid scalar index"
    (result_shape, _) ->
      newArrayValue result_shape (arrayValuesType values) selected_values

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
evalSOAC _ _ JVP {} = error "evalSOAC: JVP is not implemented"
evalSOAC _ _ VJP {} = error "evalSOAC: VJP is not implemented"
evalSOAC _ _ WithVJP {} = error "evalSOAC: WithVJP is not implemented"

evalFlatMap ::
  FunEnv rep ->
  Env ->
  SubExp ->
  [VName] ->
  ExtLambda rep ->
  InterpM rep [Val]
evalFlatMap funs env width_exp input_names lambda = do
  width <- evalInt env width_exp
  inputs <- mapM (evalVar env) input_names
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
      (collectFlatMapOutput env total_size)
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
          error "FlatMap lambda returned no segment size"

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
  width <- evalInt env width_exp
  inputs <- mapM (evalVar env) input_names
  initial_histograms <- mapM (mapM (evalVar env) . histDest) hist_ops
  final_histograms <-
    foldM
      (runIteration inputs)
      initial_histograms
      [0 .. width - 1]

  pure $ concat final_histograms
  where
    index_counts = map (shapeRank . histShape) hist_ops
    value_counts = map (length . histDest) hist_ops

    runIteration inputs histograms iteration = do
      input_rows <- mapM (rowAt iteration) inputs
      bucket_results <- evalLambda funs env bucket_lambda input_rows

      let (index_groups, remaining) = splitGroups index_counts bucket_results
          (value_groups, _) = splitGroups value_counts remaining

      index_groups' <-
        mapM
          (mapM (expectPrimVal >=> expectInt))
          index_groups

      forM (zip3 hist_ops index_groups' (zip value_groups histograms)) $
        \(hist_operation, indices, value_and_histograms) ->
          updateHistogram hist_operation indices value_and_histograms

    updateHistogram hist_operation indices (values, histograms)
      | not (inBounds indices histograms) =
          pure histograms
      | otherwise = do
          old_bins <- mapM (readHistogramBin indices) histograms
          new_bins <-
            evalLambda funs env (histOp hist_operation) (old_bins <> values)
          zipWithM (writeHistogramBin indices) histograms new_bins

    inBounds indices histograms =
      case histograms of
        [] -> False
        ArrayValue shape _ : _ ->
          length indices <= length shape
            && and (zipWith validIndex indices shape)
        _ -> False

    validIndex index dimension =
      index >= 0 && index < dimension

evalStream :: FunEnv rep -> Env -> SubExp -> [VName] -> [SubExp] -> Lambda rep -> InterpM rep [Val]
evalStream funs env width_exp input_names initial_accumulators lambda = do
  width <- evalSubExp env width_exp
  inputs <- mapM (evalVar env) input_names
  accumulators <- mapM (evalSubExp env) initial_accumulators
  evalLambda funs env lambda $ width : accumulators <> inputs

evalScrema :: FunEnv rep -> Env -> SubExp -> [VName] -> ScremaForm rep -> InterpM rep [Val]
evalScrema funs env width_exp input_names (ScremaForm pre_lambda scans reductions post_lambda) = do
  width <- evalInt env width_exp
  inputs <- mapM (evalVar env) input_names
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
        let (scan_contributions, after_scans) = splitGroups scan_sizes pre_results
            (reduction_contributions, map_values) = splitGroups reduction_sizes after_scans
        next_scan_states <- updateScanStates funs env scans scan_states scan_contributions
        next_reduction_states <- updateReductionStates funs env reductions reduction_states reduction_contributions
        post_results <- evalLambda funs env post_lambda (concat next_scan_states <> map_values)
        pure (next_scan_states, next_reduction_states, post_results : output_rows)

int64Val :: Int -> Val
int64Val = PrimVal . IntValue . Int64Value . fromIntegral

evalSegSpace :: FunEnv rep -> M.Map VName Val -> Seg.SegSpace -> Seg.KernelBody rep -> InterpM rep ([Int], [([Int], [KernelResultValue])])
evalSegSpace funs env space@(Seg.SegSpace _ dimensions) body = do
  sizes <- mapM (evalInt env . snd) dimensions
  rows <- mapM (runWorker sizes) $ allCoordinates sizes
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
evalShape env = mapM (evalInt env) . shapeDims

expectKernelValue :: KernelResultValue -> InterpM rep Val
expectKernelValue (KernelValue result_value) = pure result_value
expectKernelValue KernelTile {} =
  error "TileReturns cannot be used as a segmented contribution"
expectKernelValue KernelRegTile {} =
  error "RegTileReturns cannot be used as a segmented contribution"

segmentRows :: Int -> Int -> [a] -> [[a]]
segmentRows segment_count segment_width =
  go segment_count
  where
    go remaining rows
      | remaining <= 0 = []
      | otherwise =
          let (segment, rest) = splitAt segment_width rows
           in segment : go (remaining - 1) rest

splitSegContributions :: [Seg.SegBinOp rep] -> [Val] -> [[Val]]
splitSegContributions operators =
  fst . splitGroups (map (length . Seg.segBinOpNeutral) operators)

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

  if null vector_shape
    then evalLambda funs env operator_lambda $ state <> contribution
    else do
      result_rows <-
        forM (allCoordinates vector_shape) $ \coordinate -> do
          state_elements <-
            mapM (readAccumulatorElement coordinate) state
          contribution_elements <-
            mapM (readAccumulatorElement coordinate) contribution
          evalLambda
            funs
            env
            operator_lambda
            (state_elements <> contribution_elements)

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
updateSegStates funs env operators states contributions =
  forM (zip3 operators states contributions) $ \(operator, state, contribution) ->
    applySegBinOp funs env operator state contribution

evalSegOp ::
  FunEnv rep -> Env -> Seg.SegOp level rep -> InterpM rep [Val]
evalSegOp funs env (Seg.SegMap _ space types body) =
  evalSegMap funs env space types body
evalSegOp funs env (Seg.SegRed _ space types body operators) =
  evalSegRed funs env space types body operators
evalSegOp funs env (Seg.SegScan _ space _ body operators post_operator) =
  evalSegScan funs env space body operators post_operator
evalSegOp funs env (Seg.SegHist _ space _ body operators) =
  evalSegHist funs env space body operators

evalSegMap ::
  FunEnv rep ->
  Env ->
  Seg.SegSpace ->
  [Type] ->
  Seg.KernelBody rep ->
  InterpM rep [Val]
evalSegMap funs env space types body = do
  (shape, indexed_rows) <- evalSegSpace funs env space body
  collectSegOutputs env shape types (bodyResult body) indexed_rows

evalSegRed ::
  FunEnv rep ->
  Env ->
  Seg.SegSpace ->
  [Type] ->
  Seg.KernelBody rep ->
  [Seg.SegBinOp rep] ->
  InterpM rep [Val]
evalSegRed funs env space types body operators = do
  (shape, indexed_rows) <- evalSegSpace funs env space body

  case shape of
    [] ->
      error "SegRed requires a nonempty index space"
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
          (map (fmap $ drop reduction_count) indexed_rows)

      pure $ reduction_outputs <> map_outputs
  where
    reduceSegment rows = do
      initial_states <- initialSegStates env operators
      final_states <- foldM reduceRow initial_states rows
      pure $ concat final_states

    reduceRow states values =
      updateSegStates funs env operators states $
        splitSegContributions operators values

evalSegScan ::
  FunEnv rep ->
  Env ->
  Seg.SegSpace ->
  Seg.KernelBody rep ->
  [Seg.SegBinOp rep] ->
  Seg.SegPostOp rep ->
  InterpM rep [Val]
evalSegScan funs env space body operators post_operator = do
  (shape, indexed_rows) <- evalSegSpace funs env space body
  worker_rows <- mapM evaluateWorker indexed_rows

  case shape of
    [] ->
      error "SegScan requires a nonempty index space"
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

      next_states <-
        updateSegStates
          funs
          env
          operators
          states
          (splitSegContributions operators contribution_values)

      post_results <-
        evalLambda
          funs
          (segSpaceEnv env space shape coordinate)
          post_lambda
          (concat next_states <> map_values)

      pure (next_states, post_results : outputs)

evalSegHist ::
  FunEnv rep ->
  Env ->
  Seg.SegSpace ->
  Seg.KernelBody rep ->
  [Seg.HistOp rep] ->
  InterpM rep [Val]
evalSegHist funs env space body operators = do
  (shape, indexed_rows) <- evalSegSpace funs env space body

  case shape of
    [] ->
      error "SegHist requires a nonempty index space"
    _ -> do
      initial_histograms <-
        mapM (mapM (evalVar env) . Seg.histDest) operators

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

    updateFromWorker histograms (coordinate, worker_values) = do
      let segment_indices = init coordinate

      worker_values' <- mapM expectKernelValue worker_values

      let (index_groups, remaining) =
            splitGroups index_counts worker_values'
          (value_groups, _) =
            splitGroups value_counts remaining

      evaluated_indices <-
        mapM
          (mapM (expectPrimVal >=> expectInt))
          index_groups

      forM (zip3 operators evaluated_indices (zip value_groups histograms)) $
        \(operator, bucket_indices, values_and_histograms) ->
          updateHistogram
            segment_indices
            operator
            bucket_indices
            values_and_histograms

    updateHistogram
      segment_indices
      operator
      bucket_indices
      (new_values, histograms)
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
                  result_rows <-
                    forM (allCoordinates vector_shape) $ \coordinate -> do
                      old_elements <-
                        mapM (readAccumulatorElement coordinate) old_values
                      new_elements <-
                        mapM (readAccumulatorElement coordinate) new_values
                      evalLambda
                        funs
                        env
                        (Seg.histOp operator)
                        (old_elements <> new_elements)

                  collectOutputs
                    env
                    vector_shape
                    (lambdaReturnType $ Seg.histOp operator)
                    result_rows

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
      (ArrayValue histogram_shape _) =
        length indices <= length histogram_shape
          && and
            (zipWith validIndex indices histogram_shape)
    histogramIndicesInBounds _ _ =
      False

    validIndex index dimension =
      index >= 0 && index < dimension

splitGroups :: [Int] -> [a] -> ([[a]], [a])
splitGroups [] values = ([], values)
splitGroups (size : sizes) values =
  let (group, rest) = splitAt size values
      (groups, remaining) = splitGroups sizes rest
   in (group : groups, remaining)

evalLambda ::
  FunEnv rep ->
  Env ->
  GLambda rep return_type ->
  [Val] ->
  InterpM rep [Val]
evalLambda funs env (Lambda params _ body) args =
  evalBody funs (M.union (M.fromList $ zip (map paramName params) args) env) body

updateScanStates ::
  FunEnv rep ->
  Env ->
  [Scan rep] ->
  [[Val]] ->
  [[Val]] ->
  InterpM rep [[Val]]
updateScanStates funs env scans states contributions =
  zipWithM updateOne scans (zip states contributions)
  where
    updateOne scan (state, contribution) =
      evalLambda funs env (scanLambda scan) (state <> contribution)

updateReductionStates ::
  FunEnv rep ->
  Env ->
  [Reduce rep] ->
  [[Val]] ->
  [[Val]] ->
  InterpM rep [[Val]]
updateReductionStates funs env reductions states contributions =
  zipWithM updateOne reductions (zip states contributions)
  where
    updateOne reduction (state, contribution) =
      evalLambda funs env (redLambda reduction) (state <> contribution)

rowAt :: Int -> Val -> InterpM rep Val
rowAt index (ArrayValue (_ : row_shape) values)
  | null row_shape =
      PrimVal <$> readArrayValue values index
  | otherwise = do
      let row_size = product row_shape
          offset = index * row_size
      row_values <-
        mapM (readArrayValue values) [offset .. offset + row_size - 1]
      newArrayValue row_shape (arrayValuesType values) row_values
rowAt _ accumulator@AccValue {} =
  pure accumulator
rowAt _ _ =
  error "cannot extract a row from this value"

readAccumulatorElement :: [Int] -> Val -> InterpM rep Val
readAccumulatorElement indices (ArrayValue shape values) = do
  let index_rank = length indices
      element_shape = drop index_rank shape
      element_size = product element_shape
      offset = linearIndex (take index_rank shape) indices * element_size
  case element_shape of
    [] -> PrimVal <$> readArrayValue values offset
    _ -> do
      element_values <- mapM (readArrayValue values) [offset .. offset + element_size - 1]
      newArrayValue element_shape (arrayValuesType values) element_values
readAccumulatorElement _ _ =
  error "accumulator backing value must be an array"

writeAccumulatorElementInPlace :: [Int] -> Val -> Val -> InterpM rep ()
writeAccumulatorElementInPlace indices (ArrayValue shape values) replacement = do
  let index_rank = length indices
      element_size = product $ drop index_rank shape
      offset = linearIndex (take index_rank shape) indices * element_size
  replacement_values <- valElements replacement
  zipWithM_ (writeArrayValue values) [offset ..] replacement_values
writeAccumulatorElementInPlace _ _ _ =
  error "accumulator backing value must be an array"

readHistogramBin :: [Int] -> Val -> InterpM rep Val
readHistogramBin indices (ArrayValue shape values) = do
  let rank = length indices
      bin_shape = drop rank shape
      bin_size = product bin_shape
      offset = linearIndex (take rank shape) indices * bin_size
  case bin_shape of
    [] -> PrimVal <$> readArrayValue values offset
    _ -> do
      bin_values <- mapM (readArrayValue values) [offset .. offset + bin_size - 1]
      newArrayValue bin_shape (arrayValuesType values) bin_values
readHistogramBin _ _ =
  error "Hist destination must be an array"

writeHistogramBin :: [Int] -> Val -> Val -> InterpM rep Val
writeHistogramBin indices histogram@(ArrayValue shape values) replacement = do
  let rank = length indices
      bin_size = product $ drop rank shape
      offset = linearIndex (take rank shape) indices * bin_size
  replacement_values <- valElements replacement
  zipWithM_ (writeArrayValue values) [offset ..] replacement_values
  pure histogram
writeHistogramBin _ _ _ =
  error "Hist destination must be an array"

collectFlatMapOutput ::
  Env ->
  Int ->
  ExtType ->
  [Val] ->
  InterpM rep Val
collectFlatMapOutput env total_size result_type rows
  | flatMapNonuniform result_type =
      case rows of
        ArrayValue (_ : row_shape) values : _ -> do
          element_values <- concat <$> mapM valElements rows
          newArrayValue (total_size : row_shape) (arrayValuesType values) element_values
        _ -> do
          (element_type, row_shape) <- emptyArrayType True
          newArrayValue (total_size : row_shape) element_type []
  | otherwise =
      case result_type of
        Prim element_type ->
          newArrayValue [length rows] element_type =<< mapM expectPrimVal rows
        Array element_type _ _ ->
          case rows of
            ArrayValue row_shape _ : _ -> do
              element_values <- concat <$> mapM valElements rows
              newArrayValue (length rows : row_shape) element_type element_values
            _ -> do
              (_, row_shape) <- emptyArrayType False
              newArrayValue (0 : row_shape) element_type []
        Acc {} ->
          error "FlatMap accumulator outputs are unsupported"
        Mem {} ->
          error "FlatMap memory outputs are unsupported"
  where
    emptyArrayType drop_existential =
      case result_type of
        Array element_type (Shape dimensions) _ -> do
          let dimensions'
                | drop_existential = drop 1 dimensions
                | otherwise = dimensions
          shape <- mapM evalFreeDimension dimensions'
          pure (element_type, shape)
        _ ->
          error "nonuniform FlatMap result must be an array"

    evalFreeDimension (Free dimension) =
      evalInt env dimension
    evalFreeDimension (Ext _) =
      error "unexpected existential FlatMap result dimension"

collectOutputs ::
  Env ->
  [Int] ->
  [Type] ->
  [[Val]] ->
  InterpM rep [Val]
collectOutputs env outer_shape return_types iteration_results =
  zipWithM collectOne return_types columns
  where
    columns
      | null iteration_results =
          replicate (length return_types) []
      | otherwise =
          L.transpose iteration_results

    collectOne (Prim element_type) rows = do
      values <- mapM expectPrimVal rows
      case (outer_shape, values) of
        ([], [val]) -> pure $ PrimVal val
        ([], _) -> error "invalid scalar output count"
        _ -> newArrayValue outer_shape element_type values
    collectOne (Array element_type annotated_shape _) [] = do
      row_shape <- evalShape env annotated_shape
      newArrayValue (outer_shape <> row_shape) element_type []
    collectOne (Array element_type _ _) rows@(ArrayValue row_shape _ : _) = do
      element_values <- concat <$> mapM valElements rows
      newArrayValue (outer_shape <> row_shape) element_type element_values
    collectOne Array {} _ =
      error "expected array output"
    collectOne (Acc certificate _ _) [] =
      expectAccValue =<< evalVar env certificate
    collectOne Acc {} (row : _) =
      pure row
    collectOne Mem {} _ =
      error "memory outputs are unsupported"

    expectAccValue accumulator@AccValue {} = pure accumulator
    expectAccValue _ = error "accumulator output has no backing storage"

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
  | null worker_rows =
      zipWithM collectEmpty return_types result_specs
  | otherwise =
      zipWithM collectOne return_types columns
  where
    columns = do
      result_index <- [0 .. length return_types - 1]
      pure $ map (fmap (!! result_index)) worker_rows

    collectEmpty result_type result_spec = do
      output_shape <- resultShape result_spec
      collectOutputs env output_shape [result_type] [] >>= expectOne

    collectOne result_type rows =
      case rows of
        [] -> error "internal empty segmented output"
        (_, KernelValue {}) : _ -> do
          values <- mapM (expectOrdinary . snd) rows
          collectOutputs env worker_shape [result_type] (map pure values)
            >>= expectOne
        (_, KernelTile dimensions _) : _ -> do
          let output_shape = map fst dimensions
              tile_sizes = map snd dimensions
          collectTiled
            result_type
            output_shape
            rows
            (tileSourceIndices tile_sizes)
        (_, KernelRegTile dimensions _) : _ -> do
          let output_shape = map firstOf3 dimensions
              block_sizes = map secondOf3 dimensions
              reg_sizes = map thirdOf3 dimensions
          collectTiled
            result_type
            output_shape
            rows
            (regTileSourceIndices block_sizes reg_sizes)

    collectTiled result_type output_shape rows source_indices = do
      values <-
        mapM (tiledValueAt rows source_indices) $ allCoordinates output_shape
      collectOutputs env output_shape [result_type] (map pure values)
        >>= expectOne

    tiledValueAt rows source_indices output_coordinate = do
      let (worker_coordinate, source_coordinate) =
            source_indices output_coordinate
          tile = case lookup worker_coordinate rows of
            Just (KernelTile _ tile_value) -> tile_value
            Just (KernelRegTile _ tile_value) -> tile_value
            _ -> error "inconsistent segmented result layout"
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
      mapM (evalInt env . fst) dimensions
    resultShape (Seg.RegTileReturns _ dimensions _) =
      mapM (evalInt env . firstOf3) dimensions

    expectOrdinary (KernelValue result_value) = pure result_value
    expectOrdinary _ = error "inconsistent segmented result layout"

    expectOne [result_value] = pure result_value
    expectOne _ = error "invalid segmented output count"

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
      zipWithM
        toValue
        result_signedness
        (drop (length results - length result_signedness) results)
  where
    initial_interp_env =
      InterpEnv
        { interpOpEvaluator = eval_op,
          interpAccOps = mempty
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
      error "invalid entry point input dimension"

    lookupContextArg bindings param =
      maybe
        (error "unbound entry point shape parameter")
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
      error "function is not an entry point"
  where
    entryPointTypeSignedness _ (TypeTransparent value_type) =
      pure [valueTypeSignedness value_type]
    entryPointTypeSignedness types@(OpaqueTypes opaque_types) (TypeOpaque name) =
      case lookup name opaque_types of
        Nothing -> error $ "unknown opaque type: " <> prettyString name
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
toValue signedness (ArrayValue shape values) = do
  primitive_values <- arrayValues values
  toPrimitiveValue signedness shape (arrayValuesType values) primitive_values
toValue _ AccValue {} =
  error "accumulators cannot be represented as external values"

toPrimitiveValue :: Signedness -> [Int] -> PrimType -> [PrimValue] -> InterpM rep V.Value
toPrimitiveValue Signed shape (IntType Int8) values =
  pure $ V.I8Value (shapeVector shape) $ SVec.fromList $ map expectInt8 values
  where
    expectInt8 (IntValue (Int8Value element)) = element
    expectInt8 _ = error "expected an i8 value"
toPrimitiveValue Signed shape (IntType Int16) values =
  pure $ V.I16Value (shapeVector shape) $ SVec.fromList $ map expectInt16 values
  where
    expectInt16 (IntValue (Int16Value element)) = element
    expectInt16 _ = error "expected an i16 value"
toPrimitiveValue Signed shape (IntType Int32) values =
  pure $ V.I32Value (shapeVector shape) $ SVec.fromList $ map expectInt32 values
  where
    expectInt32 (IntValue (Int32Value element)) = element
    expectInt32 _ = error "expected an i32 value"
toPrimitiveValue Signed shape (IntType Int64) values =
  pure $ V.I64Value (shapeVector shape) $ SVec.fromList $ map expectInt64 values
  where
    expectInt64 (IntValue (Int64Value element)) = element
    expectInt64 _ = error "expected an i64 value"
toPrimitiveValue Unsigned shape (IntType Int8) values =
  pure $ V.U8Value (shapeVector shape) $ SVec.fromList $ map expectUInt8 values
  where
    expectUInt8 (IntValue (Int8Value element)) = fromIntegral element
    expectUInt8 _ = error "expected a u8 value"
toPrimitiveValue Unsigned shape (IntType Int16) values =
  pure $ V.U16Value (shapeVector shape) $ SVec.fromList $ map expectUInt16 values
  where
    expectUInt16 (IntValue (Int16Value element)) = fromIntegral element
    expectUInt16 _ = error "expected a u16 value"
toPrimitiveValue Unsigned shape (IntType Int32) values =
  pure $ V.U32Value (shapeVector shape) $ SVec.fromList $ map expectUInt32 values
  where
    expectUInt32 (IntValue (Int32Value element)) = fromIntegral element
    expectUInt32 _ = error "expected a u32 value"
toPrimitiveValue Unsigned shape (IntType Int64) values =
  pure $ V.U64Value (shapeVector shape) $ SVec.fromList $ map expectUInt64 values
  where
    expectUInt64 (IntValue (Int64Value element)) = fromIntegral element
    expectUInt64 _ = error "expected a u64 value"
toPrimitiveValue _ shape (FloatType Float16) values =
  pure $ V.F16Value (shapeVector shape) $ SVec.fromList $ map expectFloat16 values
  where
    expectFloat16 (FloatValue (Float16Value element)) = element
    expectFloat16 _ = error "expected an f16 value"
toPrimitiveValue _ shape (FloatType Float32) values =
  pure $ V.F32Value (shapeVector shape) $ SVec.fromList $ map expectFloat32 values
  where
    expectFloat32 (FloatValue (Float32Value element)) = element
    expectFloat32 _ = error "expected an f32 value"
toPrimitiveValue _ shape (FloatType Float64) values =
  pure $ V.F64Value (shapeVector shape) $ SVec.fromList $ map expectFloat64 values
  where
    expectFloat64 (FloatValue (Float64Value element)) = element
    expectFloat64 _ = error "expected an f64 value"
toPrimitiveValue _ shape Bool values =
  pure $ V.BoolValue (shapeVector shape) $ SVec.fromList $ map expectBool values
  where
    expectBool (BoolValue element) = element
    expectBool _ = error "expected a bool value"
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
  limit <- evalInt env x
  pure [PrimVal $ BoolValue $ sizeValue cls <= limit]
evalSizeOp env (CalcNumBlocks width_exp _ block_size_exp) = do
  width <- evalInt env width_exp
  block_size <- evalInt env block_size_exp
  if block_size <= 0
    then error "thread-block size must be positive"
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
