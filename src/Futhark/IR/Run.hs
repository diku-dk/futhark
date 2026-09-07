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
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Vector.Storable qualified as SVec
import Foreign.Storable (Storable)
import Futhark.Data qualified as V
import Futhark.IR
import Futhark.IR.GPU (GPU)
import Futhark.IR.SOACS (Reduce (..), SOAC (Screma), SOACS, Scan (..), ScremaForm (..))
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
evalExp funs env (Op soac) = evalSOAC funs env soac -- map/reduction/scan
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
evalBasicOp env (ArrayLit elements (Array elementType (Shape rowShapeExps) _)) = do
  expectedRowShape <-
    mapM
      ( \dimension ->
          evalSubExp env dimension >>= expectPrimVal >>= expectInt
      )
      rowShapeExps

  if any (< 0) expectedRowShape
    then Left "array literal dimensions cannot be negative"
    else do
      rows <- mapM (evalSubExp env) elements
      rowValues <- mapM (expectRow expectedRowShape elementType) rows

      pure
        [ ArrayValue
            (length elements : expectedRowShape)
            elementType
            (concat rowValues)
        ]
  where
    expectRow
      expectedShape
      expectedType
      (ArrayValue actualShape actualType values)
        | actualShape /= expectedShape =
            Left "array literal row shape mismatch"
        | actualType /= expectedType =
            Left "array literal row element type mismatch"
        | length values /= product actualShape =
            Left "invalid array literal row storage"
        | otherwise =
            pure values
    expectRow _ _ PrimVal {} =
      Left "expected an array-valued row"
evalBasicOp _ (ArrayLit _ Acc {}) =
  Left "accumulator array literals are not implemented"
evalBasicOp _ (ArrayLit _ Mem {}) =
  Left "memory array literals are unsupported in SOACS"
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
evalBasicOp env (Iota countSubExp startSubExp strideSubExp intType) = do
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
evalBasicOp env (Replicate (Shape shapeExps) valExp) = do
  dimensions <- mapM (\dim -> evalSubExp env dim >>= expectPrimVal >>= expectInt) shapeExps
  if any (< 0) dimensions
    then Left " replicate dimensions cannot be negative"
    else do
      val <- evalSubExp env valExp
      let copies = product dimensions

      case (dimensions, val) of
        ([], _) -> pure [val]
        (_, PrimVal primitiveValue) ->
          pure [ArrayValue dimensions (P.primValueType primitiveValue) (replicate copies primitiveValue)]
        (_, ArrayValue oldShape elementType values) ->
          pure [ArrayValue (dimensions <> oldShape) elementType (concat $ replicate copies values)]
evalBasicOp env (Rearrange arrayName permutation) = do
  array <-
    maybe (Left $ "unbound array: " <> prettyText arrayName) pure $
      M.lookup arrayName env
  case array of
    ArrayValue oldShape elementType values
      | not $ validPermutation (length oldShape) permutation -> Left "invalid rearrange permutation"
      | otherwise ->
          let newShape = map (oldShape !!) permutation
              newCoordinates =
                sequence [[0 .. dimension - 1] | dimension <- newShape]
              oldCoordinate newCoordinate =
                [newCoordinate !! position | position <- inversePermutation permutation]
              newValues = [values !! linearIndex oldShape (oldCoordinate coordinate) | coordinate <- newCoordinates]
           in pure [ArrayValue newShape elementType newValues]
    PrimVal _ -> Left "cannot rearrange a primitive value"
evalBasicOp env (Concat concatDim arrayNames resultSizeExp) = do
  arrays <- mapM lookupArray $ NE.toList arrayNames
  declaredSize <-
    evalSubExp env resultSizeExp >>= expectPrimVal >>= expectInt

  case arrays of
    [] ->
      Left "concat requires at least one array"
    firstArray@(firstShape, elementType, _) : remaining
      | concatDim < 0 || concatDim >= length firstShape ->
          Left "concat dimension out of bounds"
      | not $ all (compatible firstArray) remaining ->
          Left "concat array shapes or element types do not match"
      | declaredSize /= actualSize arrays ->
          Left "concat result size mismatch"
      | otherwise -> do
          let resultShape =
                replaceAt concatDim declaredSize firstShape
              coordinates =
                sequence [[0 .. size - 1] | size <- resultShape]

          resultValues <- mapM (valueAt arrays) coordinates
          pure [ArrayValue resultShape elementType resultValues]
  where
    lookupArray name =
      case M.lookup name env of
        Just (ArrayValue shape elementType values) ->
          pure (shape, elementType, values)
        Just PrimVal {} ->
          Left "cannot concatenate a primitive value"
        Nothing ->
          Left $ "unbound array: " <> prettyText name

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

      pure $ sourceValues !! offset

    findSource _ [] =
      Left "invalid concat coordinate"
    findSource index ((shape, _, values) : arrays)
      | index < size =
          pure (shape, values, index)
      | otherwise =
          findSource (index - size) arrays
      where
        size = shape !! concatDim
evalBasicOp env (Update _ arrayName slice valueExp) = do
  array <-
    maybe (Left $ "unbound array: " <> prettyText arrayName) pure $
      M.lookup arrayName env

  replacement <- evalSubExp env valueExp

  case array of
    PrimVal _ ->
      Left "cannot update a primitive value"
    ArrayValue shape elementType oldValues -> do
      (sliceShp, coordinates) <- resolveSlice env shape slice

      replacementValues <-
        updateValues elementType sliceShp replacement

      let offsets =
            map (linearIndex shape) coordinates
          newValues =
            L.foldl'
              ( \values (offset, newValue) ->
                  replaceAt offset newValue values
              )
              oldValues
              (zip offsets replacementValues)

      pure [ArrayValue shape elementType newValues]
evalBasicOp env (FlatIndex arrayName flatSlice) = do
  array <-
    maybe
      (Left $ "unbound array: " <> prettyText arrayName)
      pure
      (M.lookup arrayName env)

  (resultShape, offsets) <- evalFlatSlice env flatSlice

  case array of
    ArrayValue [_] elementType values
      | any (not . validOffset values) offsets ->
          Left "flat index out of bounds"
      | null resultShape ->
          case offsets of
            [offset] -> pure [PrimVal $ values !! offset]
            _ -> Left "invalid scalar flat index"
      | otherwise ->
          pure
            [ ArrayValue
                resultShape
                elementType
                [values !! offset | offset <- offsets]
            ]
    ArrayValue _ _ _ ->
      Left "flat index source must be one-dimensional"
    PrimVal {} ->
      Left "cannot flat-index a primitive value"
  where
    validOffset values offset =
      offset >= 0 && offset < length values
evalBasicOp env (FlatUpdate sourceName flatSlice replacementName) = do
  source <-
    maybe
      (Left $ "unbound array: " <> prettyText sourceName)
      pure
      (M.lookup sourceName env)
  replacement <-
    maybe
      (Left $ "unbound replacement: " <> prettyText sourceName)
      pure
      (M.lookup replacementName env)
  (replacementShape, offsets) <- evalFlatSlice env flatSlice
  case source of
    ArrayValue sourceShape@[_] sourceType sourceValues -> do
      replacementValues <-
        valuesForReplacement sourceType replacementShape replacement

      if any (not . validOffset sourceValues) offsets
        then Left "flat update out of bounds"
        else
          pure
            [ ArrayValue
                sourceShape
                sourceType
                ( foldl applyUpdate sourceValues $
                    zip offsets replacementValues
                )
            ]
    ArrayValue _ _ _ ->
      Left "flat update source must be one-dimensional"
    PrimVal {} ->
      Left "cannot flat-update a primitive value"
  where
    validOffset values offset =
      offset >= 0 && offset < length values

    applyUpdate values (offset, val) =
      replaceAt offset val values

    valuesForReplacement expectedType [] (PrimVal val)
      | P.primValueType val == expectedType =
          pure [val]
      | otherwise =
          Left "flat update element type mismatch"
    valuesForReplacement
      expectedType
      expectedShape
      (ArrayValue actualShape actualType values)
        | actualShape /= expectedShape =
            Left "flat update replacement shape mismatch"
        | actualType /= expectedType =
            Left "flat update element type mismatch"
        | otherwise =
            pure values
    valuesForReplacement _ _ _ =
      Left "invalid flat update replacement"
evalBasicOp env (Scratch elementType dimensionExps) = do
  dimensions <-
    mapM (\dimensionExp -> evalSubExp env dimensionExp >>= expectPrimVal >>= expectInt) dimensionExps
  if any (< 0) dimensions
    then Left "scratch dimensions cannot be negative"
    else
      let elementCount = product dimensions
          blankValue = P.blankPrimValue elementType
       in pure [ArrayValue dimensions elementType (replicate elementCount blankValue)]
evalBasicOp env (UserParam _ defaultSubExp) =
  pure <$> evalSubExp env defaultSubExp
evalBasicOp _ _ = Left "basic operation not implemented yet"

evalFlatSlice :: Env -> FlatSlice SubExp -> InterpM ([Int], [Int])
evalFlatSlice env (FlatSlice offsetExp dimensions) = do
  offset <- evalInt offsetExp
  evaluatedDimensions <- mapM evalDimension dimensions

  let resultShape = map fst evaluatedDimensions
      strides = map snd evaluatedDimensions
      coordinates = sequence [[0 .. size - 1] | size <- resultShape]
      offsets = [offset + sum (zipWith (*) coordinate strides) | coordinate <- coordinates]
  if any (< 0) resultShape
    then Left "flat slice dimensions cannot be negative"
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
          Left "cannot use a scalar to update a non-scalar slice"
      | P.primValueType primitiveValue /= elementType ->
          Left "update element type mismatch"
      | otherwise ->
          pure [primitiveValue]
    ArrayValue replacementShape replacementType replacementValues
      | replacementType /= elementType ->
          Left "update element type mismatch"
      | replacementShape /= slcShape ->
          Left "update value shape does not match slice shape"
      | otherwise ->
          pure replacementValues

resolveSlice ::
  Env ->
  [Int] ->
  Slice SubExp ->
  InterpM ([Int], [[Int]])
resolveSlice env shape (Slice dimensions)
  | length shape /= length dimensions =
      Left "slice dimensions do not match array dimensions"
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

indexArray ::
  Env ->
  [Int] ->
  PrimType ->
  [PrimValue] ->
  Slice SubExp ->
  InterpM [Val]
indexArray env shape elementType values slice = do
  (resultShape, coordinates) <- resolveSlice env shape slice

  let selectedValues =
        [values !! linearIndex shape coordinate | coordinate <- coordinates]

  case resultShape of
    [] ->
      case selectedValues of
        [val] ->
          pure [PrimVal val]
        _ ->
          Left "invalid scalar index result"
    _ ->
      pure [ArrayValue resultShape elementType selectedValues]

linearIndex :: [Int] -> [Int] -> Int
linearIndex shape indices =
  foldl (\acc (dimSize, index) -> acc * dimSize + index) 0 $ zip shape indices

evalSOAC :: FunEnv -> Env -> SOAC SOACS -> InterpM [Val]
evalSOAC funs env (Screma widthExp inputNames form) =
  evalScrema funs env widthExp inputNames form
evalSOAC _ _ _ = Left "SOAC not implemented yet"

evalScrema :: FunEnv -> Env -> SubExp -> [VName] -> ScremaForm SOACS -> InterpM [Val]
evalScrema funs env widthExp inputNames (ScremaForm preLambda scans reductions postLambda) = do
  width <- evalSubExp env widthExp >>= expectPrimVal >>= expectInt
  if width < 0
    then Left "Screma width cannot be negative"
    else do
      inputs <- mapM (lookupScremaInput env) inputNames
      mapM_ (validateScremaInput width) inputs

      if length inputs /= length (lambdaParams preLambda)
        then Left "Screma input count does not match lambda parameters"
        else do
          initialScanStates <- mapM (mapM (evalSubExp env) . scanNeutral) scans
          initialReductionStates <- mapM (mapM (evalSubExp env) . redNeutral) reductions
          (_, finalReductionStates, reversedOutputRows) <-
            foldM
              (runIteration inputs)
              (initialScanStates, initialReductionStates, [])
              [0 .. width - 1]
          outputs <-
            collectScremaOutputs
              env
              width
              (lambdaReturnType postLambda)
              (reverse reversedOutputRows)
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

splitGroups :: [Int] -> [a] -> InterpM ([[a]], [a])
splitGroups [] values =
  pure ([], values)
splitGroups (size : sizes) values
  | length group /= size =
      Left "Screma lambda returned too few values"
  | otherwise = do
      (groups, remaining) <- splitGroups sizes rest
      pure (group : groups, remaining)
  where
    (group, rest) = splitAt size values

evalLambda :: FunEnv -> Env -> Lambda SOACS -> [Val] -> InterpM [Val]
evalLambda funs env (Lambda ps returnTypes body) args
  | length ps /= length args = Left "lambda argument count mismatch"
  | otherwise = do
      let bindings = M.fromList $ zip (map paramName ps) args
          lambdaEnv = M.union bindings env
      results <- evalBody funs lambdaEnv body

      if length results /= length returnTypes
        then Left "lambda result count mismatch"
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
      Left "Screma scan state count mismatch"
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
        then Left "scan result count mismatch"
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
      Left "Screma reduction state count mismatch"
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
        then Left "reduction result count mismatch"
        else pure next

lookupScremaInput :: Env -> VName -> InterpM Val
lookupScremaInput env name =
  case M.lookup name env of
    Just array@ArrayValue {} ->
      pure array
    Just PrimVal {} ->
      Left "Screma input must be an array"
    Nothing ->
      Left $ "unbound Screma input: " <> prettyText name

validateScremaInput :: Int -> Val -> InterpM ()
validateScremaInput width (ArrayValue shape _ values) =
  case shape of
    outerSize : _
      | outerSize /= width ->
          Left "Screma input outer size mismatch"
      | length values /= product shape ->
          Left "invalid Screma input storage"
      | otherwise ->
          pure ()
    [] ->
      Left "Screma input must have positive rank"
validateScremaInput _ PrimVal {} =
  Left "Screma input must be an array"

rowAt :: Int -> Val -> InterpM Val
rowAt index (ArrayValue (_ : rowShape) elementType values)
  | null rowShape =
      case drop index values of
        val : _ -> pure $ PrimVal val
        [] -> Left "Screma input index out of bounds"
  | otherwise =
      let rowSize = product rowShape
          offset = index * rowSize
          rowValues = take rowSize $ drop offset values
       in if length rowValues /= rowSize
            then Left "invalid Screma input row"
            else
              pure $
                ArrayValue rowShape elementType rowValues
rowAt _ _ =
  Left "cannot extract a row from this value"

collectScremaOutputs ::
  Env ->
  Int ->
  [Type] ->
  [[Val]] ->
  InterpM [Val]
collectScremaOutputs env width returnTypes iterationResults
  | any ((/= length returnTypes) . length) iterationResults =
      Left "inconsistent Screma output count"
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
        then pure $ ArrayValue [width] expectedType values
        else Left "Screma primitive output type mismatch"
    collectOne (Array expectedType annotatedShape _) [] = do
      rowShape <-
        mapM
          ( \dimension ->
              evalSubExp env dimension >>= expectPrimVal >>= expectInt
          )
          (shapeDims annotatedShape)

      pure $ ArrayValue (width : rowShape) expectedType []
    collectOne (Array expectedType _ _) rows = do
      evaluatedRows <- mapM expectArray rows

      case evaluatedRows of
        [] ->
          Left "internal empty Screma output"
        (firstShape, firstType, firstValues) : remaining
          | firstType /= expectedType ->
              Left "Screma array output type mismatch"
          | not $ all (sameRow firstShape firstType) remaining ->
              Left "inconsistent Screma array output rows"
          | otherwise ->
              pure $
                ArrayValue
                  (width : firstShape)
                  expectedType
                  (firstValues <> concatMap third remaining)
    collectOne Acc {} _ =
      Left "Screma accumulator outputs are unsupported" -- This should never happen?
    collectOne Mem {} _ =
      Left "Screma memory outputs are unsupported" -- This should never happen?
    expectPrimitive (PrimVal val) =
      pure val
    expectPrimitive ArrayValue {} =
      Left "expected primitive Screma output"

    expectArray (ArrayValue shape elementType values)
      | length values == product shape =
          pure (shape, elementType, values)
      | otherwise =
          Left "invalid Screma output row storage"
    expectArray PrimVal {} =
      Left "expected array-valued Screma output"

    sameRow expectedShape expectedType (shape, elementType, values) =
      shape == expectedShape
        && elementType == expectedType
        && length values == product shape

    third (_, _, values) = values

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
      mapM toValue results
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
      Left "input array dimensions cannot be negative"
  | null dimensions =
      case primitiveValues of
        [primitiveValue] -> pure ([], PrimVal primitiveValue)
        _ -> Left "invalid scalar input storage"
  | length primitiveValues /= product dimensions =
      Left "invalid input array storage"
  | otherwise =
      pure
        ( map (PrimVal . IntValue . Int64Value . fromIntegral) dimensions,
          ArrayValue dimensions elementType primitiveValues
        )
  where
    dimensions = SVec.toList shape
    primitiveValues = map wrap $ SVec.toList values

toValue :: Val -> InterpM V.Value
toValue (PrimVal primitiveValue) =
  toPrimitiveValue [] (P.primValueType primitiveValue) [primitiveValue]
toValue (ArrayValue shape elementType values) =
  toPrimitiveValue shape elementType values

toPrimitiveValue :: [Int] -> PrimType -> [PrimValue] -> InterpM V.Value
toPrimitiveValue shape (IntType Int8) values =
  V.I8Value (shapeVector shape) . SVec.fromList <$> mapM expectInt8 values
  where
    expectInt8 (IntValue (Int8Value element)) = pure element
    expectInt8 _ = Left "expected an i8 value"
toPrimitiveValue shape (IntType Int16) values =
  V.I16Value (shapeVector shape) . SVec.fromList <$> mapM expectInt16 values
  where
    expectInt16 (IntValue (Int16Value element)) = pure element
    expectInt16 _ = Left "expected an i16 value"
toPrimitiveValue shape (IntType Int32) values =
  V.I32Value (shapeVector shape) . SVec.fromList <$> mapM expectInt32 values
  where
    expectInt32 (IntValue (Int32Value element)) = pure element
    expectInt32 _ = Left "expected an i32 value"
toPrimitiveValue shape (IntType Int64) values =
  V.I64Value (shapeVector shape) . SVec.fromList <$> mapM expectInt64 values
  where
    expectInt64 (IntValue (Int64Value element)) = pure element
    expectInt64 _ = Left "expected an i64 value"
toPrimitiveValue shape (FloatType Float16) values =
  V.F16Value (shapeVector shape) . SVec.fromList <$> mapM expectFloat16 values
  where
    expectFloat16 (FloatValue (Float16Value element)) = pure element
    expectFloat16 _ = Left "expected an f16 value"
toPrimitiveValue shape (FloatType Float32) values =
  V.F32Value (shapeVector shape) . SVec.fromList <$> mapM expectFloat32 values
  where
    expectFloat32 (FloatValue (Float32Value element)) = pure element
    expectFloat32 _ = Left "expected an f32 value"
toPrimitiveValue shape (FloatType Float64) values =
  V.F64Value (shapeVector shape) . SVec.fromList <$> mapM expectFloat64 values
  where
    expectFloat64 (FloatValue (Float64Value element)) = pure element
    expectFloat64 _ = Left "expected an f64 value"
toPrimitiveValue shape Bool values =
  V.BoolValue (shapeVector shape) . SVec.fromList <$> mapM expectBool values
  where
    expectBool (BoolValue element) = pure element
    expectBool _ = Left "expected a bool value"
toPrimitiveValue _ Unit _ =
  Left "unit values cannot be represented as external values"

shapeVector :: [Int] -> SVec.Vector Int
shapeVector = SVec.fromList

-- | Run a program in the GPU IR.
runGPU :: Prog GPU -> Name -> [V.Value] -> Either T.Text [V.Value]
runGPU = undefined
