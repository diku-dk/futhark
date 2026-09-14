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

import Control.Monad (foldM, zipWithM, (>=>))
import Control.Monad.Error.Class
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Vector.Mutable qualified as MV
import Data.Vector.Storable qualified as SVec
import Foreign.Storable (Storable)
import Futhark.Data qualified as V
import Futhark.IR
import Futhark.IR.GPU (GPU)
import Futhark.IR.SOACS (HistOp (..), Reduce (..), SOAC (FlatMap, Hist, Screma, Stream), SOACS, Scan (..), ScremaForm (..), flatMapNonuniform)
import Language.Futhark.Primitive qualified as P

data Val
  = PrimVal PrimValue
  | ArrayValue [Int] PrimType (MV.IOVector PrimValue)
  | AccValue [AccUpdate]

data AccUpdate = AccUpdate Safety [Int] [Val]

data DimSelection
  = Fixed Int
  | Selected [Int]

type Env = M.Map VName Val

type FunEnv rep = M.Map Name (FunDef rep)

type OpEvaluator rep = 
  FunEnv rep -> Env -> Op rep -> InterpM [Val]

newtype InterpM a = InterpM
  { unInterpM :: ExceptT T.Text IO a
  }
  deriving
    (Functor, Applicative, Monad, MonadError T.Text, MonadIO)

interpError :: T.Text -> InterpM a
interpError = throwError

newArrayValue :: [Int] -> PrimType -> [PrimValue] -> InterpM Val
newArrayValue shape element_type values
  | length values /= product shape =
      interpError "invalid array storage"
  | otherwise = do
      vector <- liftIO $ MV.new (length values)
      liftIO $
        mapM_
          (uncurry (MV.write vector))
          (zip [0 ..] values)
      pure $ ArrayValue shape element_type vector

arrayValues :: MV.IOVector PrimValue -> InterpM [PrimValue]
arrayValues vector =
  liftIO $
    mapM (MV.read vector) [0 .. MV.length vector - 1]

readArrayValue ::
  MV.IOVector PrimValue ->
  Int ->
  InterpM PrimValue
readArrayValue vector index
  | index < 0 || index >= MV.length vector =
      interpError "array index out of bounds"
  | otherwise =
      liftIO $ MV.read vector index

evalBody :: OpEvaluator rep -> FunEnv rep -> Env -> Body rep -> InterpM [Val]
evalBody eval_op funs env (Body _ stms res) = do
  env' <- foldStms env (stmsToList stms)
  mapM (evalSubExp env' . resSubExp) res
  where
    foldStms e [] = pure e
    foldStms e (s : ss) = evalStm eval_op funs e s >>= \e' -> foldStms e' ss

-- Evaluate the expression then bind the pattern names to its results
evalStm :: OpEvaluator rep -> FunEnv rep -> Env -> Stm rep -> InterpM Env
evalStm eval_op funs env (Let pat _ e) = do
  vals <- evalExp eval_op funs env e
  let names = map patElemName $ patElems pat
  pure $ M.union (M.fromList $ zip names vals) env

-- Produce one Val per pattern element the expression is expected to bind.
evalExp :: OpEvaluator rep -> FunEnv rep -> Env -> Exp rep -> InterpM [Val]
evalExp _ _ env (BasicOp op) = evalBasicOp env op
evalExp eval_op funs env (Match ses cases default_body _) = do
  values <- mapM (\se -> evalSubExp env se >>= expectPrimVal) ses
  evalBody eval_op funs env $ selectCase values cases
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
evalExp eval_op funs env (Loop merge (ForLoop iterator int_type bound_exp) body) = do
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

          next_values <- evalBody eval_op funs iteration_env body

          if length next_values /= length merge_names
            then interpError "loop result count mismatch"
            else runIterations (iteration + 1) bound next_values
evalExp eval_op funs env (Loop merge (WhileLoop condition) body) = do
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
          next_values <- evalBody eval_op funs loop_env body
          if length next_values /= length merge_names
            then interpError "loop result count mismatch"
            else runWhile next_values
        _ ->
          interpError "while-loop condition is not boolean"
evalExp eval_op funs env (Apply fname args _ _) = do
  callee <-
    maybe
      (interpError $ "function not found: " <> prettyText fname)
      pure
      (M.lookup fname funs)
  arg_vals <- mapM (evalSubExp env . fst) args
  let params = map paramName $ funDefParams callee
  if length params /= length arg_vals
    then interpError "function argument count mismatch"
    else
      let bindings = M.fromList $ zip params arg_vals
          callee_env = M.union bindings env
       in evalBody eval_op funs callee_env (funDefBody callee)
evalExp eval_op funs env (Op op) = eval_op funs env op -- map/reduction/scan
evalExp eval_op funs env (WithAcc inputs lambda) =
  evalWithAcc eval_op funs env inputs lambda

evalWithAcc ::
  OpEvaluator rep ->
  FunEnv rep ->
  Env ->
  [WithAccInput rep] ->
  Lambda rep ->
  InterpM [Val]
evalWithAcc eval_op funs env inputs lambda = do
  evaluated_inputs <- mapM evaluateInput inputs

  let accumulator_count = length inputs
      (certificate_params, accumulator_params) =
        splitAt accumulator_count $ lambdaParams lambda

  if length certificate_params /= accumulator_count
    || length accumulator_params /= accumulator_count
    then interpError "WithAcc lambda parameter count mismatch"
    else do
      let certificates =
            replicate accumulator_count $ PrimVal UnitValue
          accumulators =
            replicate accumulator_count $ AccValue []
          bindings =
            M.fromList $
              zip
                (map paramName certificate_params <> map paramName accumulator_params)
                (certificates <> accumulators)
          lambda_env = M.union bindings env

      results <- evalBody eval_op funs lambda_env $ lambdaBody lambda

      let (accumulator_results, ordinary_results) =
            splitAt accumulator_count results

      if length accumulator_results /= accumulator_count
        then interpError "WithAcc lambda returned too few accumulators"
        else do
          updated_arrays <-
            concat
              <$> zipWithM
                applyAccumulator
                evaluated_inputs
                accumulator_results

          pure $ updated_arrays <> ordinary_results
  where
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
      | MV.length values /= product shape =
          interpError "invalid WithAcc input array storage"
      | otherwise =
          pure ()
    validateArray _ _ =
      interpError "WithAcc input must be an array"

    applyAccumulator
      (index_shape, initial_arrays, operator)
      (AccValue updates) =
        foldM
          (applyUpdateLog index_shape operator)
          initial_arrays
          updates
    applyAccumulator _ _ =
      interpError "WithAcc lambda did not return an accumulator"

    applyUpdateLog
      index_shape
      operator
      arrays
      (AccUpdate safety indices new_values)
        | length indices /= length index_shape =
            interpError "accumulator update index rank mismatch"
        | length new_values /= length arrays =
            interpError "accumulator update value count mismatch"
        | not (indicesInBounds index_shape indices) =
            case safety of
              Safe -> pure arrays
              Unsafe -> interpError "unsafe accumulator update out of bounds"
        | otherwise = do
            old_values <-
              mapM (readAccumulatorElement indices) arrays

            replacement_values <-
              case operator of
                Nothing ->
                  pure new_values
                Just (operator_lambda, _) ->
                  evalLambda
                    eval_op
                    funs
                    env
                    operator_lambda
                    (map int64Val indices <> old_values <> new_values)

            if length replacement_values /= length arrays
              then interpError "accumulator operator result count mismatch"
              else
                zipWithM
                  (writeAccumulatorElement indices)
                  arrays
                  replacement_values

    indicesInBounds shape indices =
      and $ zipWith (\size index -> index >= 0 && index < size) shape indices

    int64Val =
      PrimVal . IntValue . Int64Value . fromIntegral

evalSubExp :: Env -> SubExp -> InterpM Val
evalSubExp _ (Constant pv) = pure $ PrimVal pv
evalSubExp env (Var v) =
  maybe (interpError $ "unbound variable: " <> prettyText v) pure $ M.lookup v env

expectPrimVal :: Val -> InterpM PrimValue
expectPrimVal (PrimVal pv) = pure pv
expectPrimVal ArrayValue {} = interpError "expected a primitive value"
expectPrimVal AccValue {} =
  interpError "expected a primitive value"

expectInt :: PrimValue -> InterpM Int
expectInt (IntValue i) = pure $ P.valueIntegral i
expectInt _ = interpError "expected an integer value"

evalBasicOp :: Env -> BasicOp -> InterpM [Val]
evalBasicOp env (SubExp se) = pure <$> evalSubExp env se
evalBasicOp env (BinOp op x y) = do
  xv <- expectPrimVal =<< evalSubExp env x
  yv <- expectPrimVal =<< evalSubExp env y
  case P.doBinOp op xv yv of
    Just result -> pure [PrimVal result]
    Nothing -> interpError "invalid binary operation"
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
        | MV.length values /= product actual_shape =
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
evalBasicOp env (Assert condition _) = do
  condition_value <- evalSubExp env condition >>= expectPrimVal
  case condition_value of
    BoolValue True -> pure [PrimVal UnitValue]
    BoolValue False -> interpError "assertion failed"
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
  dimensions <-
    mapM
      (\sub_exp -> evalSubExp env sub_exp >>= expectPrimVal >>= expectInt)
      (shapeDims $ newShape reshape)
  case array of
    ArrayValue _ element_type values
      | product dimensions == MV.length values ->
          pure [ArrayValue dimensions element_type values]
      | otherwise ->
          interpError "reshape element count mismatch"
    PrimVal _ -> interpError "cannot reshape a primitive value"
    AccValue _ -> interpError "cannot reshape an accumulator value"
evalBasicOp env (Opaque OpaqueNil se) =
  pure <$> evalSubExp env se
evalBasicOp env (Opaque (OpaqueTrace _) se) =
  pure <$> evalSubExp env se -- Perhaps include IO to print here?
evalBasicOp env (Manifest array_name _) =
  case M.lookup array_name env of
    Just (ArrayValue shape element_type values) -> do
      values' <- liftIO $ MV.clone values
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
          primitive_values <- arrayValues values
          let new_values = [primitive_values !! linearIndex old_shape (oldCoordinate coordinate) | coordinate <- new_coordinates]
           in pure <$> newArrayValue new_shape element_type new_values
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
      (slice_shp, coordinates) <- resolveSlice env shape slice

      replacement_values <-
        updateValues element_type slice_shp replacement

      let offsets = map (linearIndex shape) coordinates

      liftIO $
        mapM_
          (uncurry (MV.write values))
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
          selected_values <- mapM (readArrayValue values) offsets
          case result_shape of
            [] ->
              case selected_values of
                [primitive_value] -> pure [PrimVal primitive_value]
                _ -> interpError "invalid scalar flat index"
            _ -> pure <$> newArrayValue result_shape element_type selected_values
    ArrayValue {} ->
      interpError "flat index source must be one-dimensional"
    PrimVal {} ->
      interpError "cannot flat-index a primitive value"
    AccValue {} ->
      interpError "cannot flat-index an accumulator value"
  where
    validOffset values offset =
      offset >= 0 && offset < MV.length values
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
          liftIO $
            mapM_
              (uncurry $ MV.write source_values)
              (zip offsets replacement_values)
          pure [ArrayValue source_shape source_type source_values]
    ArrayValue {} ->
      interpError "flat update source must be one-dimensional"
    PrimVal {} ->
      interpError "cannot flat-update a primitive value"
    AccValue {} -> interpError "cannot flat-update an accumulator value"
  where
    validOffset values offset =
      offset >= 0 && offset < MV.length values

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
evalBasicOp env (UpdateAcc safety accumulator_name index_exps value_exps) = do
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
    AccValue updates ->
      pure [AccValue $ updates <> [AccUpdate safety indices values]]
    _ ->
      interpError "UpdateAcc argument is not an accumulator"

evalFlatSlice :: Env -> FlatSlice SubExp -> InterpM ([Int], [Int])
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
  InterpM [PrimValue]
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
  InterpM ([Int], [[Int]])
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
  MV.IOVector PrimValue ->
  Slice SubExp ->
  InterpM [Val]
indexArray env shape element_type values slice = do
  (result_shape, coordinates) <- resolveSlice env shape slice
  selected_values <-
    mapM
      (readArrayValue values . linearIndex shape)
      coordinates

  case result_shape of
    [] ->
      case selected_values of
        [val] -> pure [PrimVal val]
        _ -> interpError "invalid scalar index result"
    _ ->
      pure <$> newArrayValue result_shape element_type selected_values

linearIndex :: [Int] -> [Int] -> Int
linearIndex shape indices =
  foldl (\acc (dim_size, index) -> acc * dim_size + index) 0 $ zip shape indices

evalSOAC :: OpEvaluator rep -> FunEnv rep -> Env -> SOAC rep -> InterpM [Val]
evalSOAC eval_soac_op funs env (Screma width_exp input_names form) =
  evalScrema eval_soac_op funs env width_exp input_names form
evalSOAC eval_soac_op funs env (Stream width_exp input_names initial_accumulators lambda) =
  evalStream eval_soac_op funs env width_exp input_names initial_accumulators lambda
evalSOAC eval_soac_op funs env (Hist width_exp input_names hist_ops lambda) = evalHist eval_soac_op funs env width_exp input_names hist_ops lambda
evalSOAC eval_soac_op funs env (FlatMap width_exp input_names lambda) = evalFlatMap eval_soac_op funs env width_exp input_names lambda
evalSOAC _ _ _ _ = interpError "SOAC not implemented yet"

evalFlatMap ::
  OpEvaluator rep ->
  FunEnv rep ->
  Env ->
  SubExp ->
  [VName] ->
  ExtLambda rep ->
  InterpM [Val]
evalFlatMap eval_op funs env width_exp input_names lambda = do
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
          let flags = L.foldl' markSegmentStart (replicate total_size False) (zip offsets sizes)
          sizes_array <- newArrayValue [width] int64_type $ map int64Prim sizes
          flags_array <- newArrayValue [total_size] Bool $ map BoolValue flags
          offsets_array <- newArrayValue [width] int64_type $ map int64Prim offsets

          pure $
            [int64Val total_size, sizes_array, flags_array, offsets_array]
              <> values
  where
    runIteration inputs index = do
      input_rows <- mapM (rowAt index) inputs
      results <- evalLambda eval_op funs env lambda input_rows

      case results of
        size_value : values -> do
          size <- expectPrimVal size_value >>= expectInt
          if size < 0
            then interpError "FlatMap segment size cannot be negative"
            else pure (size, values)
        [] ->
          interpError "FlatMap lambda returned no segment size"

    markSegmentStart flags (offset, size)
      | size > 0 = replaceAt offset True flags
      | otherwise = flags

    int64_type = IntType Int64
    int64Prim = IntValue . Int64Value . fromIntegral
    int64Val = PrimVal . int64Prim

evalHist ::
  OpEvaluator rep ->
  FunEnv rep ->
  Env ->
  SubExp ->
  [VName] ->
  [HistOp rep] ->
  Lambda rep ->
  InterpM [Val]
evalHist eval_op funs env width_exp input_names hist_ops bucket_lambda = do
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
      bucket_results <- evalLambda eval_op funs env bucket_lambda input_rows

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
            evalLambda eval_op funs env (histOp hist_operation) (old_bins <> values)

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

evalStream :: OpEvaluator rep -> FunEnv rep -> Env -> SubExp -> [VName] -> [SubExp] -> Lambda rep -> InterpM [Val]
evalStream eval_rep funs env width_exp input_names initial_accumulators lambda = do
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
      evalLambda eval_rep funs env lambda lambda_args

evalScrema :: OpEvaluator rep -> FunEnv rep -> Env -> SubExp -> [VName] -> ScremaForm rep -> InterpM [Val]
evalScrema eval_op funs env width_exp input_names (ScremaForm pre_lambda scans reductions post_lambda) = do
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

          outputs <- prependAccumulatorInputs inputs collected_outputs
          pure $ concat final_reduction_states <> outputs
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
        pre_results <- evalLambda eval_op funs env pre_lambda input_rows
        (scan_contributions, after_scans) <- splitGroups scan_sizes pre_results
        (reduction_contributions, map_values) <- splitGroups reduction_sizes after_scans
        next_scan_states <- updateScanStates eval_op funs env scans scan_states scan_contributions
        next_reduction_states <- updateReductionStates eval_op funs env reductions reduction_states reduction_contributions
        post_results <- evalLambda eval_op funs env post_lambda (concat next_scan_states <> map_values)
        pure (next_scan_states, next_reduction_states, post_results : output_rows)

prependAccumulatorInputs :: [Val] -> [Val] -> InterpM [Val]
prependAccumulatorInputs inputs =
  go [updates | AccValue updates <- inputs]
  where
    go [] outputs = pure outputs
    go remaining [] =
      if null remaining
        then pure []
        else interpError "Screma dropped an accumulator result"
    go (initial : remaining) (AccValue updates : outputs) =
      (AccValue (initial <> updates) :)
        <$> go remaining outputs
    go remaining (output : outputs) =
      (output :) <$> go remaining outputs

splitGroups :: [Int] -> [a] -> InterpM ([[a]], [a])
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
  OpEvaluator rep ->
  FunEnv rep ->
  Env ->
  GLambda rep return_type ->
  [Val] ->
  InterpM [Val]
evalLambda eval_op funs env (Lambda params return_types body) args
  | length params /= length args =
      interpError "lambda argument count mismatch"
  | otherwise = do
      let bindings =
            M.fromList $ zip (map paramName params) args
          lambda_env =
            M.union bindings env

      results <- evalBody eval_op funs lambda_env body

      if length results /= length return_types
        then interpError "lambda result count mismatch"
        else pure results

updateScanStates ::
  OpEvaluator rep ->
  FunEnv rep ->
  Env ->
  [Scan rep] ->
  [[Val]] ->
  [[Val]] ->
  InterpM [[Val]]
updateScanStates eval_op funs env scans states contributions
  | length scans /= length states
      || length scans /= length contributions =
      interpError "Screma scan state count mismatch"
  | otherwise =
      zipWithM updateOne scans (zip states contributions)
  where
    updateOne scan (state, contribution) = do
      next <-
        evalLambda
          eval_op
          funs
          env
          (scanLambda scan)
          (state <> contribution)

      if length next /= length state
        then interpError "scan result count mismatch"
        else pure next

updateReductionStates ::
  OpEvaluator rep ->
  FunEnv rep ->
  Env ->
  [Reduce rep] ->
  [[Val]] ->
  [[Val]] ->
  InterpM [[Val]]
updateReductionStates eval_op funs env reductions states contributions
  | length reductions /= length states
      || length reductions /= length contributions =
      interpError "Screma reduction state count mismatch"
  | otherwise =
      zipWithM updateOne reductions (zip states contributions)
  where
    updateOne reduction (state, contribution) = do
      next <-
        evalLambda
          eval_op
          funs
          env
          (redLambda reduction)
          (state <> contribution)

      if length next /= length state
        then interpError "reduction result count mismatch"
        else pure next

lookupSoacInput :: Env -> VName -> InterpM Val
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

validateSoacInput :: Int -> Val -> InterpM ()
validateSoacInput width (ArrayValue shape _ values) =
  case shape of
    outer_size : _
      | outer_size /= width ->
          interpError "Screma input outer size mismatch"
      | MV.length values /= product shape ->
          interpError "invalid Screma input storage"
      | otherwise ->
          pure ()
    [] ->
      interpError "Screma input must have positive rank"
validateSoacInput _ PrimVal {} =
  interpError "Screma input must be an array"
validateSoacInput _ AccValue {} = pure ()

rowAt :: Int -> Val -> InterpM Val
rowAt index (ArrayValue (_ : row_shape) element_type values)
  | null row_shape =
      PrimVal <$> readArrayValue values index
  | otherwise = do
      let row_size = product row_shape
          offset = index * row_size
      row_values <-
        mapM (readArrayValue values) [offset .. offset + row_size - 1]
      newArrayValue row_shape element_type row_values
rowAt _ AccValue {} =
  pure $ AccValue []
rowAt _ _ =
  interpError "cannot extract a row from this value"

readAccumulatorElement :: [Int] -> Val -> InterpM Val
readAccumulatorElement indices (ArrayValue shape element_type values)
  | length indices > length shape =
      interpError "accumulator index rank exceeds array rank"
  | MV.length values /= product shape =
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

writeAccumulatorElement :: [Int] -> Val -> Val -> InterpM Val
writeAccumulatorElement
  indices
  array@(ArrayValue shape element_type values)
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

        liftIO $
          mapM_
            (uncurry $ MV.write values)
            (zip [offset ..] replacement_values)

        pure array
writeAccumulatorElement _ _ _ =
  interpError "accumulator backing value must be an array"

readHistogramBin :: [Int] -> Val -> InterpM Val
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

writeHistogramBin :: [Int] -> Val -> Val -> InterpM Val
writeHistogramBin
  indices
  histogram@(ArrayValue shape element_type values)
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
        liftIO $
          mapM_
            (uncurry $ MV.write values)
            (zip [offset ..] replacement_values)

        pure histogram
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
  InterpM Val
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
      | MV.length values /= product (size : row_shape) =
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
      | MV.length values == product shape = do
          primitive_values <- arrayValues values
          pure (shape, element_type, primitive_values)
      | otherwise =
          interpError "invalid FlatMap result storage"
    expectArray PrimVal {} =
      interpError "expected array-valued FlatMap result"
    expectArray AccValue {} =
      interpError "expected array-valued FlatMap result"

    sameArray shape element_type (other_shape, other_type, values) =
      shape == other_shape
        && element_type == other_type
        && length values == product other_shape

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

collectScremaOutputs ::
  Env ->
  Int ->
  [Type] ->
  [[Val]] ->
  InterpM [Val]
collectScremaOutputs env width return_types iteration_results
  | any ((/= length return_types) . length) iteration_results =
      interpError "inconsistent Screma output count"
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

      if all ((== expected_type) . P.primValueType) values
        then newArrayValue [width] expected_type values
        else interpError "Screma primitive output type mismatch"
    collectOne (Array expected_type annotated_shape _) [] = do
      row_shape <-
        mapM
          ( \dimension ->
              evalSubExp env dimension >>= expectPrimVal >>= expectInt
          )
          (shapeDims annotated_shape)

      newArrayValue (width : row_shape) expected_type []
    collectOne (Array expected_type _ _) rows = do
      evaluated_rows <- mapM expectArray rows

      case evaluated_rows of
        [] ->
          interpError "internal empty Screma output"
        (first_shape, first_type, first_values) : remaining
          | first_type /= expected_type ->
              interpError "Screma array output type mismatch"
          | not $ all (sameRow first_shape first_type) remaining ->
              interpError "inconsistent Screma array output rows"
          | otherwise ->
              newArrayValue
                (width : first_shape)
                expected_type
                (first_values <> concatMap third remaining)
    collectOne Acc {} rows =
      AccValue . concat <$> mapM expectAccumulator rows
    collectOne Mem {} _ =
      interpError "Screma memory outputs are unsupported" -- This should never happen?
    expectPrimitive (PrimVal val) =
      pure val
    expectPrimitive ArrayValue {} =
      interpError "expected primitive Screma output"
    expectPrimitive AccValue {} =
      interpError "expected primitive Screma output"

    expectArray (ArrayValue shape element_type values)
      | MV.length values == product shape = do
          primitive_values <- arrayValues values
          pure (shape, element_type, primitive_values)
      | otherwise =
          interpError "invalid Screma output row storage"
    expectArray PrimVal {} =
      interpError "expected array-valued Screma output"
    expectArray AccValue {} =
      interpError "expected array-valued Screma output"
    expectAccumulator (AccValue updates) =
      pure updates
    expectAccumulator _ =
      interpError "expected accumulator-valued Screma output"

    sameRow expected_shape expected_type (shape, element_type, values) =
      shape == expected_shape
        && element_type == expected_type
        && length values == product shape

    third (_, _, values) = values

-- | Run a program in the IR specified by rep.
runProgram :: OpEvaluator rep -> Prog rep -> Name -> [V.Value] -> IO (Either T.Text [V.Value])
runProgram eval_op prog entry inputs = runExceptT . unInterpM $ do
  let funs = M.fromList [(funDefName fun, fun) | fun <- progFuns prog]
  consts_env <- foldConsts funs mempty (stmsToList (progConsts prog)) -- top-level consts
  fun <- findEntry prog entry
  converted_inputs <- mapM fromValue inputs
  let shape_args = concatMap fst converted_inputs
      value_args = map snd converted_inputs
      arg_vals = shape_args <> value_args
      params = map paramName $ funDefParams fun
  if length params /= length arg_vals
    then interpError "entry point argument count mismatch"
    else do
      let env = M.union (M.fromList $ zip params arg_vals) consts_env
      results <- evalBody eval_op funs env (funDefBody fun)
      mapM toValue results
  where
    foldConsts _ e [] = pure e
    foldConsts funs e (s : ss) = evalStm eval_op funs e s >>= \e' -> foldConsts funs e' ss

findEntry :: Prog rep -> Name -> InterpM (FunDef rep)
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

fromValue :: V.Value -> InterpM ([Val], Val)
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
  InterpM ([Val], Val)
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

toValue :: Val -> InterpM V.Value
toValue (PrimVal primitive_value) =
  toPrimitiveValue [] (P.primValueType primitive_value) [primitive_value]
toValue (ArrayValue shape element_type values) = do
  primitive_values <- arrayValues values
  toPrimitiveValue shape element_type primitive_values
toValue AccValue {} =
  interpError "accumulators cannot be represented as external values"

toPrimitiveValue :: [Int] -> PrimType -> [PrimValue] -> InterpM V.Value
toPrimitiveValue shape (IntType Int8) values =
  V.I8Value (shapeVector shape) . SVec.fromList <$> mapM expectInt8 values
  where
    expectInt8 (IntValue (Int8Value element)) = pure element
    expectInt8 _ = interpError "expected an i8 value"
toPrimitiveValue shape (IntType Int16) values =
  V.I16Value (shapeVector shape) . SVec.fromList <$> mapM expectInt16 values
  where
    expectInt16 (IntValue (Int16Value element)) = pure element
    expectInt16 _ = interpError "expected an i16 value"
toPrimitiveValue shape (IntType Int32) values =
  V.I32Value (shapeVector shape) . SVec.fromList <$> mapM expectInt32 values
  where
    expectInt32 (IntValue (Int32Value element)) = pure element
    expectInt32 _ = interpError "expected an i32 value"
toPrimitiveValue shape (IntType Int64) values =
  V.I64Value (shapeVector shape) . SVec.fromList <$> mapM expectInt64 values
  where
    expectInt64 (IntValue (Int64Value element)) = pure element
    expectInt64 _ = interpError "expected an i64 value"
toPrimitiveValue shape (FloatType Float16) values =
  V.F16Value (shapeVector shape) . SVec.fromList <$> mapM expectFloat16 values
  where
    expectFloat16 (FloatValue (Float16Value element)) = pure element
    expectFloat16 _ = interpError "expected an f16 value"
toPrimitiveValue shape (FloatType Float32) values =
  V.F32Value (shapeVector shape) . SVec.fromList <$> mapM expectFloat32 values
  where
    expectFloat32 (FloatValue (Float32Value element)) = pure element
    expectFloat32 _ = interpError "expected an f32 value"
toPrimitiveValue shape (FloatType Float64) values =
  V.F64Value (shapeVector shape) . SVec.fromList <$> mapM expectFloat64 values
  where
    expectFloat64 (FloatValue (Float64Value element)) = pure element
    expectFloat64 _ = interpError "expected an f64 value"
toPrimitiveValue shape Bool values =
  V.BoolValue (shapeVector shape) . SVec.fromList <$> mapM expectBool values
  where
    expectBool (BoolValue element) = pure element
    expectBool _ = interpError "expected a bool value"
toPrimitiveValue _ Unit _ =
  interpError "unit values cannot be represented as external values"

shapeVector :: [Int] -> SVec.Vector Int
shapeVector = SVec.fromList

evalSOACSOp :: OpEvaluator SOACS
evalSOACSOp funs env soac =
  evalSOAC evalSOACSOp funs env soac

-- | Run a program in the SOAC IR
runSOACS ::
  Prog SOACS ->
  Name ->
  [V.Value] ->
  IO (Either T.Text [V.Value])
runSOACS =
  runProgram evalSOACSOp

-- | Run a program in the GPU IR.
runGPU :: Prog GPU -> Name -> [V.Value] -> IO (Either T.Text [V.Value])
runGPU = undefined
