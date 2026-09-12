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

import Control.Monad (foldM, zipWithM)
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

type FunEnv = M.Map Name (FunDef SOACS)

newtype InterpM a = InterpM
  { unInterpM :: ExceptT T.Text IO a
  }
  deriving
    (Functor, Applicative, Monad, MonadError T.Text, MonadIO)

interpError :: T.Text -> InterpM a
interpError = throwError

newArrayValue :: [Int] -> PrimType -> [PrimValue] -> InterpM Val
newArrayValue shape elementType values
  | length values /= product shape =
      interpError "invalid array storage"
  | otherwise = do
      vector <- liftIO $ MV.new (length values)
      liftIO $
        mapM_
          (\(index, val) -> MV.write vector index val)
          (zip [0 ..] values)
      pure $ ArrayValue shape elementType vector

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
            then interpError "loop result count mismatch"
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
            then interpError "loop result count mismatch"
            else runWhile nextValues
        _ ->
          interpError "while-loop condition is not boolean"
evalExp funs env (Apply fname args _ _) = do
  callee <-
    maybe
      (interpError $ "function not found: " <> prettyText fname)
      pure
      (M.lookup fname funs)
  argVals <- mapM (evalSubExp env . fst) args
  let params = map paramName $ funDefParams callee
  if length params /= length argVals
    then interpError "function argument count mismatch"
    else
      let bindings = M.fromList $ zip params argVals
          calleeEnv = M.union bindings env
       in evalBody funs calleeEnv (funDefBody callee)
evalExp funs env (Op soac) = evalSOAC funs env soac -- map/reduction/scan
evalExp funs env (WithAcc inputs lambda) =
  evalWithAcc funs env inputs lambda

evalWithAcc ::
  FunEnv ->
  Env ->
  [WithAccInput SOACS] ->
  Lambda SOACS ->
  InterpM [Val]
evalWithAcc funs env inputs lambda = do
  evaluatedInputs <- mapM evaluateInput inputs

  let accumulatorCount = length inputs
      (certificateParams, accumulatorParams) =
        splitAt accumulatorCount $ lambdaParams lambda

  if length certificateParams /= accumulatorCount
    || length accumulatorParams /= accumulatorCount
    then interpError "WithAcc lambda parameter count mismatch"
    else do
      let certificates =
            replicate accumulatorCount $ PrimVal UnitValue
          accumulators =
            replicate accumulatorCount $ AccValue []
          bindings =
            M.fromList $
              zip
                (map paramName certificateParams <> map paramName accumulatorParams)
                (certificates <> accumulators)
          lambdaEnv = M.union bindings env

      results <- evalBody funs lambdaEnv $ lambdaBody lambda

      let (accumulatorResults, ordinaryResults) =
            splitAt accumulatorCount results

      if length accumulatorResults /= accumulatorCount
        then interpError "WithAcc lambda returned too few accumulators"
        else do
          updatedArrays <-
            concat
              <$> zipWithM
                applyAccumulator
                evaluatedInputs
                accumulatorResults

          pure $ updatedArrays <> ordinaryResults
  where
    evaluateInput (Shape dimensionExps, arrayNames, operator) = do
      indexShape <-
        mapM
          (\dimension -> evalSubExp env dimension >>= expectPrimVal >>= expectInt)
          dimensionExps

      if any (< 0) indexShape
        then interpError "WithAcc index-space dimensions cannot be negative"
        else do
          arrays <- mapM lookupArray arrayNames
          mapM_ (validateArray indexShape) arrays
          pure (indexShape, arrays, operator)

    lookupArray name =
      case M.lookup name env of
        Just array@ArrayValue {} ->
          pure array
        Just _ ->
          interpError "WithAcc input must be an array"
        Nothing ->
          interpError $ "unbound WithAcc input: " <> prettyText name

    validateArray indexShape (ArrayValue shape _ values)
      | indexShape /= take (length indexShape) shape =
          interpError "WithAcc input array does not match index space"
      | MV.length values /= product shape =
          interpError "invalid WithAcc input array storage"
      | otherwise =
          pure ()
    validateArray _ _ =
      interpError "WithAcc input must be an array"

    applyAccumulator
      (indexShape, initialArrays, operator)
      (AccValue updates) =
        foldM
          (applyUpdateLog indexShape operator)
          initialArrays
          updates
    applyAccumulator _ _ =
      interpError "WithAcc lambda did not return an accumulator"

    applyUpdateLog
      indexShape
      operator
      arrays
      (AccUpdate safety indices newValues)
        | length indices /= length indexShape =
            interpError "accumulator update index rank mismatch"
        | length newValues /= length arrays =
            interpError "accumulator update value count mismatch"
        | not (indicesInBounds indexShape indices) =
            case safety of
              Safe -> pure arrays
              Unsafe -> interpError "unsafe accumulator update out of bounds"
        | otherwise = do
            oldValues <-
              mapM (readAccumulatorElement indices) arrays

            replacementValues <-
              case operator of
                Nothing ->
                  pure newValues
                Just (operatorLambda, _) ->
                  evalLambda
                    funs
                    env
                    operatorLambda
                    (map int64Val indices <> oldValues <> newValues)

            if length replacementValues /= length arrays
              then interpError "accumulator operator result count mismatch"
              else
                zipWithM
                  (writeAccumulatorElement indices)
                  arrays
                  replacementValues

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
expectPrimVal (ArrayValue _ _ _) = interpError "expected a primitive value"
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
evalBasicOp env (ArrayLit elements (Prim elementType)) = do
  values <- mapM (evalSubExp env) elements
  primitiveValues <- mapM expectPrimVal values
  pure <$> newArrayValue [length elements] elementType primitiveValues
evalBasicOp env (ArrayLit elements (Array elementType (Shape rowShapeExps) _)) = do
  expectedRowShape <-
    mapM
      ( \dimension ->
          evalSubExp env dimension >>= expectPrimVal >>= expectInt
      )
      rowShapeExps

  if any (< 0) expectedRowShape
    then interpError "array literal dimensions cannot be negative"
    else do
      rows <- mapM (evalSubExp env) elements
      rowValues <- mapM (expectRow expectedRowShape elementType) rows
      result <-
        newArrayValue
          (length elements : expectedRowShape)
          elementType
          (concat rowValues)
      pure [result]
  where
    expectRow
      expectedShape
      expectedType
      (ArrayValue actualShape actualType values)
        | actualShape /= expectedShape =
            interpError "array literal row shape mismatch"
        | actualType /= expectedType =
            interpError "array literal row element type mismatch"
        | MV.length values /= product actualShape =
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
evalBasicOp _ (ArrayVal values elementType) =
  pure <$> newArrayValue [length values] elementType values
evalBasicOp env (Assert condition _) = do
  conditionValue <- evalSubExp env condition >>= expectPrimVal
  case conditionValue of
    BoolValue True -> pure [PrimVal UnitValue]
    BoolValue False -> interpError "assertion failed"
    _ -> interpError "assert condition is not boolean"
evalBasicOp env (Index arrayName slice) = do
  array <-
    maybe (interpError $ "unbound array: " <> prettyText arrayName) pure $
      M.lookup arrayName env
  case array of
    ArrayValue shape elementType values ->
      indexArray env shape elementType values slice
    PrimVal _ ->
      interpError "cannot index a primitive value"
    AccValue _ ->
      interpError "cannot index an accumulator value"
evalBasicOp env (Reshape arrayName reshape) = do
  array <-
    maybe (interpError $ "unbound array: " <> prettyText arrayName) pure $
      M.lookup arrayName env
  dimensions <-
    mapM
      (\subExp -> evalSubExp env subExp >>= expectPrimVal >>= expectInt)
      (shapeDims $ newShape reshape)
  case array of
    ArrayValue _ elementType values
      | product dimensions == MV.length values ->
          pure [ArrayValue dimensions elementType values]
      | otherwise ->
          interpError "reshape element count mismatch"
    PrimVal _ -> interpError "cannot reshape a primitive value"
    AccValue _ -> interpError "cannot reshape an accumulator value"
evalBasicOp env (Opaque OpaqueNil se) =
  pure <$> evalSubExp env se
evalBasicOp env (Opaque (OpaqueTrace _) se) =
  pure <$> evalSubExp env se -- Perhaps include IO to print here?
evalBasicOp env (Manifest arrayName _) =
  case M.lookup arrayName env of
    Just array@ArrayValue {} -> pure [array]
    Just PrimVal {} -> interpError "cannot manifest a primitive value"
    Just AccValue {} -> interpError "cannot manifest an accumulator value"
    Nothing -> interpError $ "unbound array: " <> prettyText arrayName
evalBasicOp env (Iota countSubExp startSubExp strideSubExp intType) = do
  count <- evalSubExp env countSubExp >>= expectPrimVal >>= expectInt
  stride <- evalSubExp env strideSubExp >>= expectPrimVal >>= expectInt
  start <- evalSubExp env startSubExp >>= expectPrimVal >>= expectInt

  if count < 0
    then interpError "iota length cannot be negative"
    else do
      values <-
        newArrayValue
          [count]
          (IntType intType)
          [ IntValue $ P.intValue intType (start + i * stride)
          | i <- [0 .. count - 1]
          ]
      pure
        [values]
evalBasicOp env (Replicate (Shape shapeExps) valExp) = do
  dimensions <- mapM (\dim -> evalSubExp env dim >>= expectPrimVal >>= expectInt) shapeExps
  if any (< 0) dimensions
    then interpError " replicate dimensions cannot be negative"
    else do
      val <- evalSubExp env valExp
      let copies = product dimensions

      case (dimensions, val) of
        ([], _) -> pure [val]
        (_, PrimVal primitiveValue) ->
          pure <$> newArrayValue dimensions (P.primValueType primitiveValue) (replicate copies primitiveValue)
        (_, ArrayValue oldShape elementType values) -> do
          primitiveValues <- arrayValues values
          pure <$> newArrayValue (dimensions <> oldShape) elementType (concat $ replicate copies primitiveValues)
        (_, AccValue {}) ->
          interpError "cannot replicate an accumulator value"
evalBasicOp env (Rearrange arrayName permutation) = do
  array <-
    maybe (interpError $ "unbound array: " <> prettyText arrayName) pure $
      M.lookup arrayName env
  case array of
    ArrayValue oldShape elementType values
      | not $ validPermutation (length oldShape) permutation -> interpError "invalid rearrange permutation"
      | otherwise -> do
          let newShape = map (oldShape !!) permutation
              newCoordinates =
                sequence [[0 .. dimension - 1] | dimension <- newShape]
              oldCoordinate newCoordinate =
                [newCoordinate !! position | position <- inversePermutation permutation]
          primitiveValues <- arrayValues values
          let newValues = [primitiveValues !! linearIndex oldShape (oldCoordinate coordinate) | coordinate <- newCoordinates]
           in pure <$> newArrayValue newShape elementType newValues
    PrimVal _ -> interpError "cannot rearrange a primitive value"
    AccValue _ -> interpError "cannot rearrange an accumulator value"
evalBasicOp env (Concat concatDim arrayNames resultSizeExp) = do
  arrays <- mapM lookupArray $ NE.toList arrayNames
  declaredSize <-
    evalSubExp env resultSizeExp >>= expectPrimVal >>= expectInt

  case arrays of
    [] ->
      interpError "concat requires at least one array"
    firstArray@(firstShape, elementType, _) : remaining
      | concatDim < 0 || concatDim >= length firstShape ->
          interpError "concat dimension out of bounds"
      | not $ all (compatible firstArray) remaining ->
          interpError "concat array shapes or element types do not match"
      | declaredSize /= actualSize arrays ->
          interpError "concat result size mismatch"
      | otherwise -> do
          let resultShape =
                replaceAt concatDim declaredSize firstShape
              coordinates =
                sequence [[0 .. size - 1] | size <- resultShape]

          resultValues <- mapM (valueAt arrays) coordinates
          pure <$> newArrayValue resultShape elementType resultValues
  where
    lookupArray name =
      case M.lookup name env of
        Just (ArrayValue shape elementType values) ->
          pure (shape, elementType, values)
        Just PrimVal {} ->
          interpError "cannot concatenate a primitive value"
        Just AccValue {} ->
          interpError "cannot concatenate an accumulator value"
        Nothing ->
          interpError $ "unbound array: " <> prettyText name

    compatible (firstShape, firstType, _) (shape, elementType, _) =
      firstType == elementType
        && length firstShape == length shape
        && removeAt concatDim firstShape == removeAt concatDim shape

    actualSize =
      sum . map (\(shape, _, _) -> shape !! concatDim)

    valueAt arrays coordinate = do
      let concatIndex = coordinate !! concatDim
      (sourceShape, sourceValues, localIndex) <-
        findSource concatIndex arrays

      let sourceCoordinate =
            replaceAt concatDim localIndex coordinate
          offset =
            linearIndex sourceShape sourceCoordinate

      readArrayValue sourceValues offset

    findSource _ [] =
      interpError "invalid concat coordinate"
    findSource index ((shape, _, values) : arrays)
      | index < size =
          pure (shape, values, index)
      | otherwise =
          findSource (index - size) arrays
      where
        size = shape !! concatDim
evalBasicOp env (Update _ arrayName slice valueExp) = do
  array <-
    maybe (interpError $ "unbound array: " <> prettyText arrayName) pure $
      M.lookup arrayName env

  replacement <- evalSubExp env valueExp

  case array of
    PrimVal _ ->
      interpError "cannot update a primitive value"
    AccValue _ ->
      interpError "cannot update an accumulator value"
    ArrayValue shape elementType oldValues -> do
      (sliceShp, coordinates) <- resolveSlice env shape slice

      replacementValues <-
        updateValues elementType sliceShp replacement
      oldPrimitives <- arrayValues oldValues

      let offsets =
            map (linearIndex shape) coordinates
          newValues =
            L.foldl'
              ( \values (offset, newValue) ->
                  replaceAt offset newValue values
              )
              oldPrimitives
              (zip offsets replacementValues)

      pure <$> newArrayValue shape elementType newValues
evalBasicOp env (FlatIndex arrayName flatSlice) = do
  array <-
    maybe
      (interpError $ "unbound array: " <> prettyText arrayName)
      pure
      (M.lookup arrayName env)

  (resultShape, offsets) <- evalFlatSlice env flatSlice

  case array of
    ArrayValue [_] elementType values
      | any (not . validOffset values) offsets ->
          interpError "flat index out of bounds"
      | otherwise -> do
          selectedValues <- mapM (readArrayValue values) offsets
          case resultShape of
            [] ->
              case selectedValues of
                [primitiveValue] -> pure [PrimVal primitiveValue]
                _ -> interpError "invalid scalar flat index"
            _ -> pure <$> newArrayValue resultShape elementType selectedValues
    ArrayValue _ _ _ ->
      interpError "flat index source must be one-dimensional"
    PrimVal {} ->
      interpError "cannot flat-index a primitive value"
    AccValue {} ->
      interpError "cannot flat-index an accumulator value"
  where
    validOffset values offset =
      offset >= 0 && offset < MV.length values
evalBasicOp env (FlatUpdate sourceName flatSlice replacementName) = do
  source <-
    maybe
      (interpError $ "unbound array: " <> prettyText sourceName)
      pure
      (M.lookup sourceName env)
  replacement <-
    maybe
      (interpError $ "unbound replacement: " <> prettyText sourceName)
      pure
      (M.lookup replacementName env)
  (replacementShape, offsets) <- evalFlatSlice env flatSlice
  case source of
    ArrayValue sourceShape@[_] sourceType sourceValues -> do
      replacementValues <-
        valuesForReplacement sourceType replacementShape replacement

      if any (not . validOffset sourceValues) offsets
        then interpError "flat update out of bounds"
        else do
          sourcePrimitiveValues <- arrayValues sourceValues
          let newValues =
                foldl applyUpdate sourcePrimitiveValues $
                  zip offsets replacementValues
          pure <$> newArrayValue sourceShape sourceType newValues
    ArrayValue _ _ _ ->
      interpError "flat update source must be one-dimensional"
    PrimVal {} ->
      interpError "cannot flat-update a primitive value"
    AccValue {} -> interpError "cannot flat-update an accumulator value"
  where
    validOffset values offset =
      offset >= 0 && offset < MV.length values

    applyUpdate values (offset, val) =
      replaceAt offset val values

    valuesForReplacement expectedType [] (PrimVal val)
      | P.primValueType val == expectedType =
          pure [val]
      | otherwise =
          interpError "flat update element type mismatch"
    valuesForReplacement
      expectedType
      expectedShape
      (ArrayValue actualShape actualType values)
        | actualShape /= expectedShape =
            interpError "flat update replacement shape mismatch"
        | actualType /= expectedType =
            interpError "flat update element type mismatch"
        | otherwise =
            arrayValues values
    valuesForReplacement _ _ _ =
      interpError "invalid flat update replacement"
evalBasicOp env (Scratch elementType dimensionExps) = do
  dimensions <-
    mapM (\dimensionExp -> evalSubExp env dimensionExp >>= expectPrimVal >>= expectInt) dimensionExps
  if any (< 0) dimensions
    then interpError "scratch dimensions cannot be negative"
    else
      let elementCount = product dimensions
          blankValue = P.blankPrimValue elementType
       in pure <$> newArrayValue dimensions elementType (replicate elementCount blankValue)
evalBasicOp env (UserParam _ defaultSubExp) =
  pure <$> evalSubExp env defaultSubExp
evalBasicOp env (UpdateAcc safety accumulatorName indexExps valueExps) = do
  accumulator <-
    maybe
      (interpError $ "unbound accumulator: " <> prettyText accumulatorName)
      pure
      (M.lookup accumulatorName env)

  indices <-
    mapM
      (\indexExp -> evalSubExp env indexExp >>= expectPrimVal >>= expectInt)
      indexExps
  values <- mapM (evalSubExp env) valueExps

  case accumulator of
    AccValue updates ->
      pure [AccValue $ updates <> [AccUpdate safety indices values]]
    _ ->
      interpError "UpdateAcc argument is not an accumulator"

evalFlatSlice :: Env -> FlatSlice SubExp -> InterpM ([Int], [Int])
evalFlatSlice env (FlatSlice offsetExp dimensions) = do
  offset <- evalInt offsetExp
  evaluatedDimensions <- mapM evalDimension dimensions

  let resultShape = map fst evaluatedDimensions
      strides = map snd evaluatedDimensions
      coordinates = sequence [[0 .. size - 1] | size <- resultShape]
      offsets = [offset + sum (zipWith (*) coordinate strides) | coordinate <- coordinates]
  if any (< 0) resultShape
    then interpError "flat slice dimensions cannot be negative"
    else pure (resultShape, offsets)
  where
    evalInt subExp = evalSubExp env subExp >>= expectPrimVal >>= expectInt

    evalDimension (FlatDimIndex sizeExp strideExp) = do
      size <- evalInt sizeExp
      stride <- evalInt strideExp
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
updateValues elementType slcShape replacement =
  case replacement of
    PrimVal primitiveValue
      | slcShape /= [] ->
          interpError "cannot use a scalar to update a non-scalar slice"
      | P.primValueType primitiveValue /= elementType ->
          interpError "update element type mismatch"
      | otherwise ->
          pure [primitiveValue]
    ArrayValue replacementShape replacementType replacementValues
      | replacementType /= elementType ->
          interpError "update element type mismatch"
      | replacementShape /= slcShape ->
          interpError "update value shape does not match slice shape"
      | otherwise ->
          arrayValues replacementValues
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

      let resultShape =
            [length indices | Selected indices <- selections]
          coordinates =
            sequence $ map selectionIndices selections

      pure (resultShape, coordinates)
  where
    evalDimension (DimFix indexExp) =
      Fixed <$> evalInt indexExp
    evalDimension (DimSlice startExp countExp strideExp) = do
      start <- evalInt startExp
      count <- evalInt countExp
      stride <- evalInt strideExp

      if count < 0
        then interpError "slice length cannot be negative"
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
indexArray env shape elementType values slice = do
  (resultShape, coordinates) <- resolveSlice env shape slice
  selectedValues <-
    mapM
      (readArrayValue values . linearIndex shape)
      coordinates

  case resultShape of
    [] ->
      case selectedValues of
        [val] -> pure [PrimVal val]
        _ -> interpError "invalid scalar index result"
    _ ->
      pure <$> newArrayValue resultShape elementType selectedValues

linearIndex :: [Int] -> [Int] -> Int
linearIndex shape indices =
  foldl (\acc (dimSize, index) -> acc * dimSize + index) 0 $ zip shape indices

evalSOAC :: FunEnv -> Env -> SOAC SOACS -> InterpM [Val]
evalSOAC funs env (Screma widthExp inputNames form) =
  evalScrema funs env widthExp inputNames form
evalSOAC funs env (Stream widthExp inputNames initialAccumulators lambda) =
  evalStream funs env widthExp inputNames initialAccumulators lambda
evalSOAC funs env (Hist widthExp inputNames histOps lambda) = evalHist funs env widthExp inputNames histOps lambda
evalSOAC funs env (FlatMap widthExp inputNames lambda) = evalFlatMap funs env widthExp inputNames lambda
evalSOAC _ _ _ = interpError "SOAC not implemented yet"

evalFlatMap ::
  FunEnv ->
  Env ->
  SubExp ->
  [VName] ->
  ExtLambda SOACS ->
  InterpM [Val]
evalFlatMap funs env widthExp inputNames lambda = do
  width <- evalSubExp env widthExp >>= expectPrimVal >>= expectInt
  if width < 0
    then interpError "FlatMap width cannot be negative"
    else do
      inputs <- mapM (lookupSoacInput env) inputNames
      mapM_ (validateSoacInput width) inputs

      if length inputs /= length (lambdaParams lambda)
        then interpError "FlatMap input count does not match lambda parameters"
        else do
          rows <- mapM (runIteration inputs) [0 .. width - 1]
          let sizes = map fst rows
              valueRows = map snd rows
              offsets = init $ scanl (+) 0 sizes
              totalSize = sum sizes
              returnTypes = drop 1 $ lambdaReturnType lambda
              columns
                | null valueRows = replicate (length returnTypes) []
                | otherwise = L.transpose valueRows
          values <-
            zipWithM
              (collectFlatMapOutput env sizes totalSize)
              returnTypes
              columns
          let flags = L.foldl' markSegmentStart (replicate totalSize False) (zip offsets sizes)
          sizesArray <- newArrayValue [width] int64Type $ map int64Prim sizes
          flagsArray <- newArrayValue [totalSize] Bool $ map BoolValue flags
          offsetsArray <- newArrayValue [width] int64Type $ map int64Prim offsets

          pure $
            [int64Val totalSize, sizesArray, flagsArray, offsetsArray]
              <> values
  where
    runIteration inputs index = do
      inputRows <- mapM (rowAt index) inputs
      results <- evalLambda funs env lambda inputRows

      case results of
        sizeValue : values -> do
          size <- expectPrimVal sizeValue >>= expectInt
          if size < 0
            then interpError "FlatMap segment size cannot be negative"
            else pure (size, values)
        [] ->
          interpError "FlatMap lambda returned no segment size"

    markSegmentStart flags (offset, size)
      | size > 0 = replaceAt offset True flags
      | otherwise = flags

    int64Type = IntType Int64
    int64Prim = IntValue . Int64Value . fromIntegral
    int64Val = PrimVal . int64Prim

evalHist ::
  FunEnv ->
  Env ->
  SubExp ->
  [VName] ->
  [HistOp SOACS] ->
  Lambda SOACS ->
  InterpM [Val]
evalHist funs env widthExp inputNames histOps bucketLambda = do
  width <- evalSubExp env widthExp >>= expectPrimVal >>= expectInt

  if width < 0
    then interpError "Hist width cannot be negative"
    else do
      inputs <- mapM (lookupSoacInput env) inputNames
      mapM_ (validateSoacInput width) inputs

      initialHistograms <- mapM initialHistogramsFor histOps
      finalHistograms <-
        foldM
          (runIteration inputs)
          initialHistograms
          [0 .. width - 1]

      pure $ concat finalHistograms
  where
    indexCounts = map (shapeRank . histShape) histOps
    valueCounts = map (length . histDest) histOps

    initialHistogramsFor histOp =
      mapM lookupHistogram $ histDest histOp

    lookupHistogram name =
      case M.lookup name env of
        Just histogram@ArrayValue {} -> pure histogram
        Just PrimVal {} -> interpError "Hist destination must be an array"
        Just AccValue {} -> interpError "Hist destination must be an array"
        Nothing -> interpError $ "unbound Hist destination: " <> prettyText name

    runIteration inputs histograms iteration = do
      inputRows <- mapM (rowAt iteration) inputs
      bucketResults <- evalLambda funs env bucketLambda inputRows

      (indexGroups, remaining) <- splitGroups indexCounts bucketResults
      (valueGroups, extra) <- splitGroups valueCounts remaining

      indexGroups' <-
        mapM
          (mapM (\val -> expectPrimVal val >>= expectInt))
          indexGroups

      if null extra
        then
          if length histOps /= length indexGroups'
            || length histOps /= length valueGroups
            || length histOps /= length histograms
            then interpError "Hist operation count mismatch"
            else
              mapM
                ( \(histOperation, indices, valueAndHistograms) ->
                    updateHistogram histOperation indices valueAndHistograms
                )
                (zip3 histOps indexGroups' (zip valueGroups histograms))
        else interpError "Hist bucket lambda returned too many values"

    updateHistogram histOperation indices (values, histograms)
      | length histograms /= length (histDest histOperation) =
          interpError "Hist destination count mismatch"
      | not (inBounds indices histograms) =
          pure histograms
      | otherwise = do
          oldBins <- mapM (readHistogramBin indices) histograms
          newBins <-
            evalLambda funs env (histOp histOperation) (oldBins <> values)

          if length newBins /= length histograms
            then interpError "Hist operator result count mismatch"
            else zipWithM (writeHistogramBin indices) histograms newBins

    inBounds indices histograms =
      case histograms of
        [] -> False
        ArrayValue shape _ _ : _ ->
          length indices <= length shape
            && and (zipWith validIndex indices shape)
        _ -> False

    validIndex index dimension =
      index >= 0 && index < dimension

evalStream :: FunEnv -> Env -> SubExp -> [VName] -> [SubExp] -> Lambda SOACS -> InterpM [Val]
evalStream funs env widthExp inputNames initialAccumulators lambda = do
  widthValue <- evalSubExp env widthExp >>= expectPrimVal
  width <- expectInt widthValue

  if width < 0
    then interpError "Stream width cannot be negative"
    else do
      inputs <- mapM (lookupSoacInput env) inputNames
      mapM_ (validateSoacInput width) inputs
      accumulators <- mapM (evalSubExp env) initialAccumulators

      let chunkSize = PrimVal $ IntValue $ Int64Value $ fromIntegral width
          lambdaArgs = chunkSize : accumulators <> inputs
      evalLambda funs env lambda lambdaArgs

evalScrema :: FunEnv -> Env -> SubExp -> [VName] -> ScremaForm SOACS -> InterpM [Val]
evalScrema funs env widthExp inputNames (ScremaForm preLambda scans reductions postLambda) = do
  width <- evalSubExp env widthExp >>= expectPrimVal >>= expectInt
  if width < 0
    then interpError "Screma width cannot be negative"
    else do
      inputs <- mapM (lookupSoacInput env) inputNames
      mapM_ (validateSoacInput width) inputs

      if length inputs /= length (lambdaParams preLambda)
        then interpError "Screma input count does not match lambda parameters"
        else do
          initialScanStates <- mapM (mapM (evalSubExp env) . scanNeutral) scans
          initialReductionStates <- mapM (mapM (evalSubExp env) . redNeutral) reductions
          (_, finalReductionStates, reversedOutputRows) <-
            foldM
              (runIteration inputs)
              (initialScanStates, initialReductionStates, [])
              [0 .. width - 1]
          collectedOutputs <-
            collectScremaOutputs
              env
              width
              (lambdaReturnType postLambda)
              (reverse reversedOutputRows)

          outputs <- prependAccumulatorInputs inputs collectedOutputs
          pure $ concat finalReductionStates <> outputs
  where
    scanSizes =
      map (length . scanNeutral) scans
    reductionSizes =
      map (length . redNeutral) reductions

    runIteration
      inputs
      (scanStates, reductionStates, outputRows)
      index = do
        inputRows <- mapM (rowAt index) inputs
        preResults <- evalLambda funs env preLambda inputRows
        (scanContributions, afterScans) <- splitGroups scanSizes preResults
        (reductionContributions, mapValues) <- splitGroups reductionSizes afterScans
        nextScanStates <- updateScanStates funs env scans scanStates scanContributions
        nextReductionStates <- updateReductionStates funs env reductions reductionStates reductionContributions
        postResults <- evalLambda funs env postLambda (concat nextScanStates <> mapValues)
        pure (nextScanStates, nextReductionStates, postResults : outputRows)

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
  FunEnv ->
  Env ->
  GLambda SOACS returnType ->
  [Val] ->
  InterpM [Val]
evalLambda funs env (Lambda params returnTypes body) args
  | length params /= length args =
      interpError "lambda argument count mismatch"
  | otherwise = do
      let bindings =
            M.fromList $ zip (map paramName params) args
          lambdaEnv =
            M.union bindings env

      results <- evalBody funs lambdaEnv body

      if length results /= length returnTypes
        then interpError "lambda result count mismatch"
        else pure results

updateScanStates ::
  FunEnv ->
  Env ->
  [Scan SOACS] ->
  [[Val]] ->
  [[Val]] ->
  InterpM [[Val]]
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
  FunEnv ->
  Env ->
  [Reduce SOACS] ->
  [[Val]] ->
  [[Val]] ->
  InterpM [[Val]]
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
    outerSize : _
      | outerSize /= width ->
          interpError "Screma input outer size mismatch"
      | MV.length values /= product shape ->
          interpError "invalid Screma input storage"
      | otherwise ->
          pure ()
    [] ->
      interpError "Screma input must have positive rank"
validateSoacInput _ PrimVal {} =
  interpError "Screma input must be an array"
validateSoacInput _ AccValue {} =
  interpError "Screma input must be an array"

rowAt :: Int -> Val -> InterpM Val
rowAt index (ArrayValue (_ : rowShape) elementType values)
  | null rowShape =
      PrimVal <$> readArrayValue values index
  | otherwise = do
      let rowSize = product rowShape
          offset = index * rowSize
      rowValues <-
        mapM (readArrayValue values) [offset .. offset + rowSize - 1]
      newArrayValue rowShape elementType rowValues
rowAt _ AccValue {} =
  pure $ AccValue []
rowAt _ _ =
  interpError "cannot extract a row from this value"

readAccumulatorElement :: [Int] -> Val -> InterpM Val
readAccumulatorElement indices (ArrayValue shape elementType values)
  | length indices > length shape =
      interpError "accumulator index rank exceeds array rank"
  | MV.length values /= product shape =
      interpError "invalid accumulator backing-array storage"
  | otherwise = do
      let indexRank = length indices
          indexShape = take indexRank shape
          elementShape = drop indexRank shape
          elementSize = product elementShape
          offset = linearIndex indexShape indices * elementSize
      elementValues <- mapM (readArrayValue values) [offset .. offset + elementSize - 1]
      case elementShape of
        [] ->
          case elementValues of
            [val] -> pure $ PrimVal val
            _ -> interpError "invalid scalar accumulator element"
        _ -> newArrayValue elementShape elementType elementValues
readAccumulatorElement _ _ =
  interpError "accumulator backing value must be an array"

writeAccumulatorElement :: [Int] -> Val -> Val -> InterpM Val
writeAccumulatorElement
  indices
  (ArrayValue shape elementType oldValues)
  replacement
    | length indices > length shape =
        interpError "accumulator index rank exceeds array rank"
    | otherwise = do
        let indexRank = length indices
            indexShape = take indexRank shape
            elementShape = drop indexRank shape
            elementSize = product elementShape
            offset = linearIndex indexShape indices * elementSize

        replacementValues <-
          updateValues elementType elementShape replacement
        oldPrimitiveValues <- arrayValues oldValues
        newArrayValue
          shape
          elementType
          ( take offset oldPrimitiveValues
              <> replacementValues
              <> drop (offset + elementSize) oldPrimitiveValues
          )
writeAccumulatorElement _ _ _ =
  interpError "accumulator backing value must be an array"

readHistogramBin :: [Int] -> Val -> InterpM Val
readHistogramBin indices (ArrayValue shape elementType values) = do
  let rank = length indices
      binShape = drop rank shape
      binSize = product binShape
      offset = linearIndex (take rank shape) indices * binSize
  binValues <- mapM (readArrayValue values) [offset .. offset + binSize - 1]
  case binShape of
    [] ->
      case binValues of
        [val] -> pure $ PrimVal val
        _ -> interpError "invalid scalar Hist bin"
    _ ->
      if length binValues == binSize
        then newArrayValue binShape elementType binValues
        else interpError "invalid Hist bin storage"
readHistogramBin _ PrimVal {} =
  interpError "Hist destination must be an array"
readHistogramBin _ AccValue {} =
  interpError "Hist destination must be an array"

writeHistogramBin :: [Int] -> Val -> Val -> InterpM Val
writeHistogramBin indices histogram@(ArrayValue shape elementType oldValues) replacement = do
  let rank = length indices
      binShape = drop rank shape
      binSize = product binShape
      offset = linearIndex (take rank shape) indices * binSize

  replacementValues <- updateValues elementType binShape replacement

  if length replacementValues /= binSize
    then interpError "invalid Hist operator result storage"
    else case histogram of
      ArrayValue _ _ _ -> do
        oldPrimitiveValues <- arrayValues oldValues
        newArrayValue shape elementType $
          take offset oldPrimitiveValues
            <> replacementValues
            <> drop (offset + binSize) oldPrimitiveValues
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
collectFlatMapOutput env sizes totalSize resultType rows
  | flatMapNonuniform resultType =
      collectNonuniform
  | otherwise =
      collectUniform
  where
    collectNonuniform = do
      arrays <- zipWithM expectSegment sizes rows

      case arrays of
        [] -> do
          (elementType, rowShape) <- emptyArrayType True
          newArrayValue (totalSize : rowShape) elementType []
        (rowShape, elementType, values) : remaining
          | not $ all (sameArray rowShape elementType) remaining ->
              interpError "inconsistent FlatMap segment results"
          | otherwise ->
              newArrayValue
                (totalSize : rowShape)
                elementType
                (values <> concatMap third remaining)

    collectUniform =
      case resultType of
        Prim expectedType -> do
          values <- mapM expectPrimitive rows
          if all ((== expectedType) . P.primValueType) values
            then newArrayValue [length rows] expectedType values
            else interpError "FlatMap uniform result type mismatch"
        Array expectedType _ _ ->
          case rows of
            [] -> do
              (_, rowShape) <- emptyArrayType False
              newArrayValue (0 : rowShape) expectedType []
            _ -> do
              arrays <- mapM expectArray rows
              case arrays of
                [] ->
                  interpError "internal empty FlatMap output"
                (rowShape, elementType, values) : remaining
                  | elementType /= expectedType ->
                      interpError "FlatMap uniform result type mismatch"
                  | not $ all (sameArray rowShape elementType) remaining ->
                      interpError "inconsistent FlatMap uniform results"
                  | otherwise ->
                      newArrayValue
                        (length rows : rowShape)
                        elementType
                        (values <> concatMap third remaining)
        Acc {} ->
          interpError "FlatMap accumulator outputs are unsupported"
        Mem {} ->
          interpError "FlatMap memory outputs are unsupported"

    expectSegment expectedSize (ArrayValue (size : rowShape) elementType values)
      | size /= expectedSize =
          interpError "FlatMap segment size does not match returned size"
      | MV.length values /= product (size : rowShape) =
          interpError "invalid FlatMap segment storage"
      | otherwise = do
          primitiveValues <- arrayValues values
          pure (rowShape, elementType, primitiveValues)
    expectSegment _ _ =
      interpError "nonuniform FlatMap result must be an array"

    expectPrimitive (PrimVal val) = pure val
    expectPrimitive ArrayValue {} =
      interpError "expected primitive FlatMap result"
    expectPrimitive AccValue {} =
      interpError "expected primitive FlatMap result"

    expectArray (ArrayValue shape elementType values)
      | MV.length values == product shape = do
          primitiveValues <- arrayValues values
          pure (shape, elementType, primitiveValues)
      | otherwise =
          interpError "invalid FlatMap result storage"
    expectArray PrimVal {} =
      interpError "expected array-valued FlatMap result"
    expectArray AccValue {} =
      interpError "expected array-valued FlatMap result"

    sameArray shape elementType (otherShape, otherType, values) =
      shape == otherShape
        && elementType == otherType
        && length values == product otherShape

    third (_, _, values) = values

    emptyArrayType dropExistential =
      case resultType of
        Array elementType (Shape dimensions) _ -> do
          let dimensions'
                | dropExistential = drop 1 dimensions
                | otherwise = dimensions
          shape <- mapM evalFreeDimension dimensions'
          pure (elementType, shape)
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
collectScremaOutputs env width returnTypes iterationResults
  | any ((/= length returnTypes) . length) iterationResults =
      interpError "inconsistent Screma output count"
  | otherwise =
      zipWithM collectOne returnTypes columns
  where
    columns
      | null iterationResults =
          replicate (length returnTypes) []
      | otherwise =
          L.transpose iterationResults

    collectOne (Prim expectedType) rows = do
      values <- mapM expectPrimitive rows

      if all ((== expectedType) . P.primValueType) values
        then newArrayValue [width] expectedType values
        else interpError "Screma primitive output type mismatch"
    collectOne (Array expectedType annotatedShape _) [] = do
      rowShape <-
        mapM
          ( \dimension ->
              evalSubExp env dimension >>= expectPrimVal >>= expectInt
          )
          (shapeDims annotatedShape)

      newArrayValue (width : rowShape) expectedType []
    collectOne (Array expectedType _ _) rows = do
      evaluatedRows <- mapM expectArray rows

      case evaluatedRows of
        [] ->
          interpError "internal empty Screma output"
        (firstShape, firstType, firstValues) : remaining
          | firstType /= expectedType ->
              interpError "Screma array output type mismatch"
          | not $ all (sameRow firstShape firstType) remaining ->
              interpError "inconsistent Screma array output rows"
          | otherwise ->
              newArrayValue
                (width : firstShape)
                expectedType
                (firstValues <> concatMap third remaining)
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

    expectArray (ArrayValue shape elementType values)
      | MV.length values == product shape = do
          primitiveValues <- arrayValues values
          pure (shape, elementType, primitiveValues)
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

    sameRow expectedShape expectedType (shape, elementType, values) =
      shape == expectedShape
        && elementType == expectedType
        && length values == product shape

    third (_, _, values) = values

-- | Run a program in the SOACS IR.
runSOACS :: Prog SOACS -> Name -> [V.Value] -> IO (Either T.Text [V.Value])
runSOACS prog entry inputs = runExceptT . unInterpM $ do
  let funs = M.fromList [(funDefName fun, fun) | fun <- progFuns prog]
  constsEnv <- foldConsts funs mempty (stmsToList (progConsts prog)) -- top-level consts
  fun <- findEntry prog entry
  convertedInputs <- mapM fromValue inputs
  let shapeArgs = concatMap fst convertedInputs
      valueArgs = map snd convertedInputs
      argVals = shapeArgs <> valueArgs
      params = map paramName $ funDefParams fun
  if length params /= length argVals
    then interpError "entry point argument count mismatch"
    else do
      let env = M.union (M.fromList $ zip params argVals) constsEnv
      results <- evalBody funs env (funDefBody fun)
      mapM toValue results
  where
    foldConsts _ e [] = pure e
    foldConsts funs e (s : ss) = evalStm funs e s >>= \e' -> foldConsts funs e' ss

findEntry :: Prog SOACS -> Name -> InterpM (FunDef SOACS)
findEntry prog name =
  maybe
    (interpError $ "entry point not found: " <> prettyText name)
    pure
    $ lookup
      name
      [ (entryName, fun)
      | fun <- progFuns prog,
        Just (entryName, _, _, _) <- [funDefEntryPoint fun]
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
fromPrimitiveVector shape elementType wrap values
  | any (< 0) dimensions =
      interpError "input array dimensions cannot be negative"
  | null dimensions =
      case primitiveValues of
        [primitiveValue] -> pure ([], PrimVal primitiveValue)
        _ -> interpError "invalid scalar input storage"
  | length primitiveValues /= product dimensions =
      interpError "invalid input array storage"
  | otherwise = do
      array <- newArrayValue dimensions elementType primitiveValues
      pure
        ( map
            (PrimVal . IntValue . Int64Value . fromIntegral)
            dimensions,
          array
        )
  where
    dimensions = SVec.toList shape
    primitiveValues = map wrap $ SVec.toList values

toValue :: Val -> InterpM V.Value
toValue (PrimVal primitiveValue) =
  toPrimitiveValue [] (P.primValueType primitiveValue) [primitiveValue]
toValue (ArrayValue shape elementType values) = do
  primitiveValues <- arrayValues values
  toPrimitiveValue shape elementType primitiveValues
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

-- | Run a program in the GPU IR.
runGPU :: Prog GPU -> Name -> [V.Value] -> IO (Either T.Text [V.Value])
runGPU = undefined
