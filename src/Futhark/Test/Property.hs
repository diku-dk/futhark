{-# LANGUAGE LambdaCase #-}

module Futhark.Test.Property
  ( runPBT,
    PBTConfig (..),
    PBTPhase (..),
    PropSpec (..),
    PBTOutput,
    PBTFailure,
    extractPropSpecsFromServer,
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except
import Data.Either (fromRight)
import Data.IORef
import Data.Int
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text qualified as T
import Data.Word (Word64)
import Futhark.Data
import Futhark.Server
import Futhark.Server.Values qualified as FSV
import Futhark.Test.Values
  ( RandomConfiguration (..),
    initialRandomConfiguration,
    randomValue,
  )
import Futhark.Util (showText)
import System.FilePath (dropExtension)
import System.Random (StdGen, mkStdGen, random)
import System.Random.Stateful (IOGenM (..), applyIOGen, newIOGenM)

data PBTConfig = PBTConfig
  { -- | Number of test cases to run for each property.
    configNumTests :: Int32,
    -- | Maximum size argument to pass to generators and shrinkers (unless
    -- overridden by PropSpec).
    configMaxSize :: Int64,
    -- | Seed to use for the random number generator (can be overridden by PropSpec).
    configSeed :: Word64,
    -- | Number of shrinking attempts to make on userdefined shrinker before
    -- giving up and reporting the current candidate as the final
    -- counterexample.
    configShrinkTries :: Int
  }
  deriving (Show, Eq)

data PropSpec = PropSpec
  { -- | The property entry point to test.
    psProp :: T.Text,
    -- | Optional generator entry point to use instead of the default
    -- auto-generator.
    psGen :: Maybe T.Text,
    -- | Optional shrinker entry point to use instead of the default
    -- auto-shrinker.
    psShrink :: Maybe T.Text,
    -- | Optional size argument to pass to the generator and shrinker phases
    -- (overrides 'configMaxSize').
    psSize :: Maybe Int64
  }
  deriving (Show, Eq)

data PBTPhase = PBTPhase
  { -- | Property being tested (same for generator and shrinker phases).
    activeTest :: Maybe EntryName,
    -- | Current entrypoint being called (property, generator, or shrinker).
    phase :: Maybe EntryName,
    -- | Property or generator (only for auto).
    shrinkWith :: Maybe EntryName,
    -- | Size argument for generator and shrinker phases.
    phaseSize :: Maybe Int64,
    -- | Seed argument for generator and shrinker phases.
    phaseSeed :: Maybe Word64,
    -- | Random value argument for shrinker phase (used for user-defined
    -- shrinker, otherwise 'Nothing').
    phaseRandom :: Maybe Word64
  }
  deriving (Show, Eq)

type PBTOutput = Maybe T.Text

type PBTFailure = T.Text

-- | The Haskell-level random number generator that we use for generating seeds
-- passed to generators.
type PBTGen = IOGenM StdGen

genWord64 :: (MonadIO m) => PBTGen -> m Word64
genWord64 = applyIOGen random

peekWord64 :: (MonadIO m) => PBTGen -> m Word64
peekWord64 (IOGenM ref) = liftIO $ do
  pureGen <- readIORef ref
  let (value, _nextGen) = random pureGen
  pure value

lookupArgText :: T.Text -> [T.Text] -> Maybe T.Text
lookupArgText name = lookupArgWith name Just

lookupArgRead :: (Read a) => T.Text -> [T.Text] -> Maybe a
lookupArgRead name = lookupArgWith name readMaybeText

lookupArgWith :: T.Text -> (T.Text -> Maybe a) -> [T.Text] -> Maybe a
lookupArgWith name f = msum . map (stripCall name >=> f)

stripCall :: T.Text -> T.Text -> Maybe T.Text
stripCall name t =
  let s = T.strip t
   in if s == name
        then Just ""
        else T.stripSuffix ")" <=< T.stripPrefix (name <> "(") $ s

readMaybeText :: (Read a) => T.Text -> Maybe a
readMaybeText t =
  case reads (T.unpack t) of
    [(x, "")] -> Just x
    _ -> Nothing

parsePropSpec :: T.Text -> T.Text -> Maybe PropSpec
parsePropSpec entry attr = do
  argsText <- stripCall "prop" attr
  let args = map T.strip $ T.splitOn "," argsText
  pure
    PropSpec
      { psProp = entry,
        psGen = lookupArgText "gen" args,
        psShrink = lookupArgText "shrink" args,
        psSize = fromInteger <$> lookupArgRead "size" args
      }

atMostOnePropAttr :: (MonadFail m) => T.Text -> [T.Text] -> m [PropSpec]
atMostOnePropAttr entry attrs =
  case mapMaybe (parsePropSpec entry) attrs of
    [] -> pure []
    [spec] -> pure [spec]
    _ ->
      fail $
        "Entry point '"
          <> T.unpack entry
          <> "' has more than one #[prop(...)] attribute."

getInputTypes :: Server -> EntryName -> IO [TypeName]
getInputTypes srv entry =
  fmap (map inputType) $
    cmdErrorHandlerE ("Failed to get input types for " <> entry <> ": ") $
      cmdInputs srv entry

getOutputType :: Server -> EntryName -> IO TypeName
getOutputType s entry =
  fmap outputType $
    cmdErrorHandlerE ("Failed to get output types for " <> entry <> ": ") $
      cmdOutput s entry

validateGenTypes :: Server -> EntryName -> Maybe EntryName -> IO (Maybe PBTFailure)
validateGenTypes _srv _propName Nothing = pure Nothing -- TODO: should have more logic to see if we can actually generate anything
validateGenTypes srv propName (Just genName) = fmap (either Just (const Nothing)) . runExceptT $ do
  -- find expected input types for generator
  genIns <- liftIO $ getInputTypes srv genName
  case genIns of
    [sizeTy, seedTy] -> do
      unless (sizeTy == "i64" && seedTy == "u64") $
        throwE $
          "Generator input type mismatch.\nGenerator "
            <> genName
            <> " takes size: "
            <> sizeTy
            <> " and seed: "
            <> seedTy
            <> "\nExpected size and seed to be i64 and u64, got: size type="
            <> sizeTy
            <> " and seed type="
            <> seedTy
    [] ->
      throwE $
        "Generator " <> genName <> " has no inputs? Expected 2 (size and seed)."
    tys ->
      throwE $
        "Generator " <> genName <> " has " <> showText (length tys) <> " inputs; expected 2 (size and seed)."

  -- check expected output type for generator matches property input type
  propTy <-
    liftIO (getSingleInputType srv propName) >>= \case
      Left err -> throwE $ showText err
      Right ty -> pure ty

  genOut <- liftIO $ getOutputType srv genName

  let isMatch = propTy == genOut

  unless isMatch $
    throwE $
      "Generator output type mismatch.\nProperty "
        <> propName
        <> " expects: "
        <> propTy
        <> "\nGenerator "
        <> genName
        <> " produces: "
        <> showText genOut
        <> "\nExpected generator output type to equal the property input type."

validateShrinkTypes :: Server -> EntryName -> EntryName -> IO (Maybe PBTFailure)
validateShrinkTypes srv propName shrinkName = fmap (either Just (const Nothing)) . runExceptT $ do
  propTy <-
    liftIO (getSingleInputType srv propName) >>= \case
      Left err -> throwE $ showText err
      Right ty -> pure ty

  shrinkIns <- liftIO $ getInputTypes srv shrinkName

  -- validate input
  case shrinkIns of
    [xTy, randTy] -> do
      let errors =
            [ "Property " <> propName <> " expects " <> propTy <> " but shrinker " <> shrinkName <> " takes " <> xTy
            | xTy /= propTy
            ]
              <> [ "Shrinker " <> shrinkName <> " takes random value " <> randTy <> " but expected u64"
                 | randTy /= "u64"
                 ]

      unless (null errors) $
        throwE $
          "Shrinker Mismatch:\n" <> T.intercalate "\n---\n" errors
    tys ->
      throwE $
        "Shrinker input arity mismatch.\n shrinker "
          <> shrinkName
          <> " inputs: "
          <> showText tys
          <> "\nExpected exactly 2 inputs: ("
          <> propTy
          <> ", u64)."

  -- validate output
  shrinkOut <- liftIO $ getOutputType srv shrinkName
  let isMatch = propTy == shrinkOut

  unless isMatch $
    throwE $
      "Shrinker output mismatch.\nProperty "
        <> propName
        <> " expects: "
        <> propTy
        <> "\nShrinker "
        <> shrinkName
        <> " returns: "
        <> showText shrinkOut
        <> "\nExpected shrinker output to equal the property input type."

validatePropTypes :: Server -> EntryName -> IO (Maybe PBTFailure)
validatePropTypes srv propName = fmap (either Just (const Nothing)) . runExceptT $ do
  ins <- liftIO $ getInputTypes srv propName
  case ins of
    [_] -> pure ()
    [] -> throwE $ "Property " <> propName <> " has no inputs? Expected 1."
    _ -> throwE $ "Property " <> propName <> " has " <> showText (length ins) <> " inputs; expected 1."

  out <- liftIO $ getOutputType srv propName
  case out of
    "bool" -> pure ()
    ty -> throwE $ "Property " <> propName <> " output must be bool, got: " <> ty

validateOneSpec :: Server -> [EntryName] -> PropSpec -> IO (Maybe PBTFailure)
validateOneSpec srv eps spec = do
  let prop = psProp spec

  fmap (either Just (const Nothing)) . runExceptT $ do
    unless (prop `elem` eps) $
      throwE $
        "Property entry point not found: " <> prop

    liftIO (validatePropTypes srv prop) >>= maybe (pure ()) throwE

    genName <- case psGen spec of
      Nothing ->
        pure Nothing
      Just g
        | g `notElem` eps ->
            throwE $ "Generator is not a server entry point: " <> g
      Just g ->
        pure $ Just g

    liftIO (validateGenTypes srv prop genName) >>= maybe (pure ()) throwE

    case psShrink spec of
      Nothing -> pure ()
      Just sh -> do
        unless (sh `elem` eps) $
          throwE $
            "Shrinker is not a server entry point: " <> sh
        liftIO (validateShrinkTypes srv prop sh) >>= maybe (pure ()) throwE

extractPropSpecsFromServer :: Server -> IO [PropSpec]
extractPropSpecsFromServer srv = do
  epsE <- cmdEntryPoints srv
  eps <- either (error . show) pure epsE
  concat <$> mapM getOne eps
  where
    getOne entry = do
      attrsE <- cmdAttributes srv entry
      attrs <- either (fail . show) pure attrsE
      atMostOnePropAttr entry attrs

-- | Generate a candidate automatically.
automaticGenerator :: Server -> EntryName -> TypeName -> Int64 -> PBTGen -> IO (Maybe PBTFailure)
automaticGenerator srv candidate genTy size rng = do
  kRes <- cmdErrorHandlerE "automaticGenerator cmdKind failed: " $ cmdKind srv genTy
  case kRes of
    Record -> do
      resFields <- cmdFields srv genTy
      case resFields of
        Right fields -> do
          let fieldVarNames = [candidate <> "_$compositeVal" <> fieldName fld | fld <- fields]

          results <- forM (zip fieldVarNames fields) $ \(fVarName, fld) ->
            automaticGenerator srv fVarName (fieldType fld) size rng

          case sequence results of
            Just err -> pure $ Just $ T.unlines err
            Nothing -> do
              freeVars srv [candidate]
              cmdErrorHandlerM ("Failed to pack record " <> candidate <> ": ") $
                cmdNew srv candidate genTy fieldVarNames

              freeVars srv fieldVarNames
              pure Nothing
        Left err -> pure $ Just $ showText err
    Array -> do
      (dimCount, baseTy) <- getArrayDimsAndBase srv genTy
      baseKind <- cmdErrorHandlerE "automaticGenerator base cmdKind failed: " $ cmdKind srv baseTy
      case baseKind of
        Record -> do
          resFields <- cmdFields srv baseTy
          case resFields of
            Right fields -> do
              let fieldVarNames = [candidate <> "_$compositeArr_" <> fieldName fld | fld <- fields]

              results <- forM (zip fieldVarNames fields) $ \(fVarName, fld) -> do
                (innerDimCount, innerBaseTy) <- getArrayDimsAndBase srv (fieldType fld)
                let totalDimCount = dimCount + innerDimCount
                    fArrTy = T.replicate totalDimCount "[]" <> innerBaseTy
                automaticGenerator srv fVarName fArrTy size rng

              case sequence results of
                Just err -> pure $ Just $ T.unlines err
                Nothing -> do
                  freeVars srv [candidate]
                  cmdErrorHandlerM ("Failed to zip array of records " <> candidate <> ": ") $
                    cmdZip srv candidate genTy fieldVarNames

                  freeVars srv fieldVarNames
                  pure Nothing
            Left err -> pure $ Just $ showText err
        _ -> do
          let shapeList = replicate dimCount size
          seed <- genWord64 rng
          case makeFutPrimitiveValue baseTy shapeList seed of
            Right val -> do
              freeVars srv [candidate]
              putRes <- FSV.putValue srv candidate val
              case putRes of
                Nothing -> pure Nothing
                Just err -> pure $ Just $ showText err
            Left err -> pure $ Just err
    _ -> do
      seed <- genWord64 rng
      case makeFutPrimitiveValue genTy [] seed of
        Right val -> do
          freeVars srv [candidate]
          putRes <- FSV.putValue srv candidate val
          case putRes of
            Nothing -> pure Nothing
            Just err -> pure $ Just $ showText err
        Left _ -> do
          let genName = "gen_" <> genTy
              szVar = candidate <> "_$size"
              sdVar = candidate <> "_$seed"

          putVal srv szVar size
          putVal srv sdVar seed

          callFreeIns srv genName candidate [szVar, sdVar]

-- | Recursively unwrap arrays using the Futhark server to handle type aliases dynamically.
getArrayDimsAndBase :: Server -> TypeName -> IO (Int, TypeName)
getArrayDimsAndBase srv t
  | "[]" `T.isPrefixOf` t = do
      (d, base) <- getArrayDimsAndBase srv (T.drop 2 t)
      pure (d + 1, base)
  | otherwise = do
      kRes <- cmdKind srv t
      case kRes of
        Right Array -> do
          etRes <- cmdElemtype srv t
          case etRes of
            Right et -> do
              (d, base) <- getArrayDimsAndBase srv et
              pure (d + 1, base)
            Left _ -> pure (0, t) -- Fallback if server fails to resolve elemtype
        _ -> pure (0, t)

makeFutPrimitiveValue :: TypeName -> [Int64] -> Word64 -> Either PBTFailure Value
makeFutPrimitiveValue ty shape seed =
  case ty of
    "i8" -> Right $ randomValue cfg (ValueType shape' I8) seed
    "i16" -> Right $ randomValue cfg (ValueType shape' I16) seed
    "i32" -> Right $ randomValue cfg (ValueType shape' I32) seed
    "i64" -> Right $ randomValue cfg (ValueType shape' I64) seed
    "u8" -> Right $ randomValue cfg (ValueType shape' U8) seed
    "u16" -> Right $ randomValue cfg (ValueType shape' U16) seed
    "u32" -> Right $ randomValue cfg (ValueType shape' U32) seed
    "u64" -> Right $ randomValue cfg (ValueType shape' U64) seed
    "f16" -> Right $ randomValue cfg (ValueType shape' F16) seed
    "f32" -> Right $ randomValue cfg (ValueType shape' F32) seed
    "f64" -> Right $ randomValue cfg (ValueType shape' F64) seed
    "bool" -> Right $ randomValue cfg (ValueType shape' Bool) seed
    _ -> Left ("Automatic generation not implemented for: " <> ty)
  where
    shape' = map fromIntegral shape
    cfg = initialRandomConfiguration {f32Range = (-1, 1)}

runOne :: PropSpec -> PBTConfig -> Server -> IORef PBTPhase -> FilePath -> IO (Either PBTFailure PBTOutput)
runOne s config srv entryNameRef program = runExceptT $ do
  rng <- newIOGenM $ mkStdGen $ fromIntegral $ configSeed config
  let loop i
        | i >= numTests = pure Nothing
        | otherwise = check (i + 1)
      check i = do
        seed <- peekWord64 rng
        let runUpdate ph =
              liftIO $
                updatePhase
                  (Just propName)
                  (Just ph)
                  Nothing
                  (Just size)
                  (Just seed)
                  Nothing
                  entryNameRef

        generatorCandidateM <- liftIO $ generatorPhase rng
        maybe (pure ()) throwE generatorCandidateM

        runUpdate propName

        okE <-
          liftIO $ withCallKeepIns srv propName serverOk [serverIn] $ getVal srv
        ok <- case okE of
          Right b -> pure b
          Left err -> do
            liftIO $
              cmdErrorHandlerM "cmdStore failed to store when property crashed" $
                cmdStore srv propertyFileName [serverIn]
            valuePPrint <- liftIO pPrintPhase
            throwE $
              "Property "
                <> propName
                <> " failed with candidate="
                <> valuePPrint
                <> " with error: "
                <> err
        if ok
          then loop i
          else do
            let failmsg =
                  "PBT FAIL: "
                    <> propName
                    <> " size="
                    <> showText size
                    <> " seed="
                    <> showText seed
                    <> " after "
                    <> showText i
                    <> " tests\n"

            shrinkRes <- case psShrink s of
              Nothing ->
                liftIO $
                  autoShrinkLoop
                    srv
                    propName
                    genName
                    serverIn
                    size
                    seed
                    rng
                    serverSize
                    serverSeed
                    entryNameRef
              Just sh -> do
                userShrinkRes <-
                  liftIO $
                    shrinkLoop
                      srv
                      propName
                      serverIn
                      sh
                      rng
                      (configShrinkTries config)
                      entryNameRef

                case userShrinkRes of
                  Right Nothing ->
                    pure (Right Nothing)
                  Right (Just err) -> do
                    autoRes <-
                      liftIO $
                        autoShrinkLoop
                          srv
                          propName
                          genName
                          serverIn
                          size
                          seed
                          rng
                          serverSize
                          serverSeed
                          entryNameRef

                    pure $
                      Right $
                        Just $
                          "User shrinker failed: "
                            <> err
                            <> "\nAttempted auto-shrinker fallback."
                            <> formatAutoShrinkResult autoRes
                  Left err -> do
                    autoRes <-
                      liftIO $
                        autoShrinkLoop
                          srv
                          propName
                          genName
                          serverIn
                          size
                          seed
                          rng
                          serverSize
                          serverSeed
                          entryNameRef

                    pure $
                      Right $
                        Just $
                          "User shrinker failed: "
                            <> err
                            <> "\nAttempted auto-shrinker fallback."
                            <> formatAutoShrinkResult autoRes

            case shrinkRes of
              Left err -> do
                runUpdate "prettyPrint"
                counterLog <- liftIO pPrintPhase
                liftIO $ cmdErrorHandlerM "cmdStore failed to store when shrinker crashed" $ cmdStore srv propertyFileName [serverIn]
                pure $
                  Just $
                    failmsg
                      <> "Shrinking failed: "
                      <> err
                      <> "\nCounterexample: "
                      <> counterLog
              Right (Just note) -> do
                runUpdate "prettyPrint"
                counterLog <- liftIO pPrintPhase
                pure $
                  Just $
                    failmsg
                      <> "Shrinking note: "
                      <> note
                      <> "\nCounterexample: "
                      <> counterLog
              Right Nothing -> do
                runUpdate "prettyPrint"
                counterLog <- liftIO pPrintPhase
                liftIO $
                  cmdErrorHandlerM "cmdStore failed to store shrinker result" $
                    cmdStore srv propertyFileName [serverIn]
                pure $ Just $ failmsg <> "Counterexample: " <> counterLog
  loop 0
  where
    propName = psProp s
    genName = psGen s
    size = fromMaybe (configMaxSize config) (psSize s)
    numTests = configNumTests config
    serverSize = "runPBT_size"
    serverSeed = "runPBT_seed"
    serverIn = "runPBT_input"
    serverOk = "runPBT_ok"
    propertyFileName =
      dropExtension program <> "_" <> T.unpack propName <> ".counterexample"

    generatorPhase rng = do
      let runUpdate ph seed =
            liftIO $
              updatePhase
                (Just propName)
                (Just ph)
                Nothing
                (Just size)
                (Just seed)
                Nothing
                entryNameRef
      case genName of
        Nothing -> fmap (either Just (const Nothing)) . runExceptT $ do
          seed <- peekWord64 rng
          runUpdate "Auto Generator" seed
          liftIO $ freeVars srv [serverIn]
          propType <-
            liftIO (getSingleInputType srv propName) >>= \case
              Left err -> throwE $ showText err
              Right ty -> pure ty

          errM <- liftIO $ automaticGenerator srv serverIn propType size rng
          maybe (pure ()) (throwE . ("Haskell generator failed: " <>)) errM
        Just gn -> fmap (either Just (const Nothing)) . runExceptT $ do
          seed <- genWord64 rng
          runUpdate "User Generator" seed
          liftIO $ putVal srv serverSize size
          liftIO $ putVal srv serverSeed seed

          let genOut = outName gn

          ExceptT $ withFreedVars srv [genOut] $ runExceptT $ do
            liftIO (callFreeIns srv gn genOut [serverSize, serverSeed]) >>= \case
              Nothing -> pure ()
              Just err -> do
                liftIO $ putVal srv serverSize size >> putVal srv serverSeed seed
                liftIO $
                  cmdErrorHandlerM "cmdStore failed to store generator error: " $
                    cmdStore srv propertyFileName [serverSize, serverSeed]
                liftIO $ freeVars srv [serverSize, serverSeed]
                throwE $
                  "User Generator "
                    <> gn
                    <> " failed with size="
                    <> showText size
                    <> " and seed="
                    <> showText seed
                    <> " with error: "
                    <> err

            liftIO $ renameVar srv serverIn genOut

    pPrintPhase = do
      inputTypes <- getInputTypes srv propName
      case inputTypes of
        [] -> pure "Could not retrieve input type for counterexample."
        ty0 : _ -> prettyVar srv serverIn ty0

formatAutoShrinkResult :: Either PBTFailure PBTOutput -> T.Text
formatAutoShrinkResult autoRes =
  case autoRes of
    Right Nothing ->
      "\nAuto-shrinker fallback completed."
    Right (Just msg) ->
      "\nAuto-shrinker fallback completed with note: " <> msg
    Left autoErr ->
      "\nAuto-shrinker fallback also failed: " <> autoErr

updatePhase ::
  Maybe EntryName ->
  Maybe EntryName ->
  Maybe EntryName ->
  Maybe Int64 ->
  Maybe Word64 ->
  Maybe Word64 ->
  IORef PBTPhase ->
  IO ()
updatePhase propName phase activeTest size seed rand phaseRef =
  writeIORef phaseRef $
    PBTPhase
      { activeTest = propName,
        phase = phase,
        shrinkWith = activeTest,
        phaseSize = size,
        phaseSeed = seed,
        phaseRandom = rand
      }

autoShrinkLoop ::
  Server ->
  EntryName ->
  Maybe EntryName ->
  VarName ->
  Int64 ->
  Word64 ->
  PBTGen ->
  VarName ->
  VarName ->
  IORef PBTPhase ->
  IO (Either PBTFailure PBTOutput)
autoShrinkLoop srv propName genName vCounterExample size seed rng serverSize serverSeed phaseRef = runExceptT $ do
  let loop i
        | i <= 1 = pure Nothing
        | otherwise = do
            let newSize = i - 1

            errM <- liftIO $ generatorPhase newSize
            maybe (pure ()) (throwE . ("Auto-shrinker generator failed: " <>)) errM

            liftIO $ autoShrinkUpdatePhase (Right $ Just propName)

            ok <-
              either (throwE . ("Property " <>)) pure <=< liftIO $
                withCallKeepIns srv propName vOk [vCandidate] $
                  getVal srv

            liftIO $ autoShrinkUpdatePhase (Left Nothing)

            case ok of
              True -> do
                liftIO $ freeVars srv [vCandidate]
                loop newSize
              False -> do
                liftIO $
                  freeOnException srv [vCounterExample] $
                    renameVar srv vCounterExample vCandidate
                loop newSize
  loop size
  where
    vCandidate = "auto_shrink_candidate"
    vOk = "auto_shrink_ok"
    autoShrinkUpdatePhase active =
      liftIO $
        updatePhase
          (Just propName)
          (Just "autoShrinkLoop")
          (fromRight (Just "Auto Generator") active)
          (Just size)
          (Just seed)
          Nothing
          phaseRef
    generatorPhase size' = do
      fmap (either Just (const Nothing)) . runExceptT $
        case genName of
          Nothing -> do
            propertyType <- do
              liftIO (getSingleInputType srv propName) >>= \case
                Left err -> throwE $ showText err
                Right ty -> pure ty
            liftIO $ freeVars srv [serverSize, serverSeed]
            runExceptT $ do
              errM <- liftIO $ automaticGenerator srv vCandidate propertyType size' rng
              maybe (pure ()) (throwE . ("Haskell auto generator failed: " <>)) errM
          Just gn -> do
            liftIO $ putVal srv serverSize size' >> putVal srv serverSeed seed
            runExceptT $ do
              errM <- liftIO $ callFreeIns srv gn vCandidate [serverSize, serverSeed]
              maybe (pure ()) (\err -> throwE $ "User Generator " <> gn <> " failed: " <> err) errM

data Step
  = -- | Property still failed with the new candidate
    AcceptedShrink
  | -- | Property passed with the new candidate
    NotAcceptedShrink
  | -- | Shrinker error (including property failure in shrinker)
    ErrorInShrink T.Text
  deriving (Eq, Show)

shrinkLoop :: Server -> EntryName -> VarName -> EntryName -> PBTGen -> Int -> IORef PBTPhase -> IO (Either PBTFailure PBTOutput)
shrinkLoop srv propName counterExample shrinkName rng numTries phaseRef = runExceptT $ do
  -- 1. Setup Phase
  oldRef <- liftIO $ readIORef phaseRef
  let size = fromMaybe 0 (phaseSize oldRef)
      shrinkUpdatePhase activeTest =
        liftIO $
          updatePhase
            (Just propName)
            (Just shrinkName)
            activeTest
            (Just size)
            Nothing
            Nothing
            phaseRef

  shrinkUpdatePhase Nothing

  let loop (acc :: Int) (totalCounter :: Int)
        | acc >= numTries = pure Nothing
        | otherwise = do
            random_value <- genWord64 rng
            stepResultE <- liftIO $ oneStep size random_value
            stepResult <-
              either
                ( \err ->
                    throwE $
                      "Error in shrinker "
                        <> shrinkName
                        <> " with random="
                        <> showText random_value
                        <> ": "
                        <> err
                )
                pure
                stepResultE

            case stepResult of
              AcceptedShrink ->
                loop 0 (totalCounter + 1) -- Reset trials on success
              NotAcceptedShrink ->
                loop (acc + 1) (totalCounter + 1) -- Increment trials
              ErrorInShrink err ->
                throwE $ "Error in shrinker: " <> err
  loop 0 0
  where
    oneStep size (val :: Word64) = do
      let vCandidate = "shrink_candidate"
          vOk = "shrink_ok"
          vRandomValue = "shrink_random"

      let shrinkUpdatePhase activeTest randomNum =
            updatePhase
              (Just propName)
              (Just shrinkName)
              activeTest
              (Just size)
              Nothing
              randomNum
              phaseRef

      freeVars srv [vRandomValue]
      putVal srv vRandomValue val

      shrinkUpdatePhase Nothing $ Just val

      withFreedVar srv vCandidate $ runExceptT $ do
        errE <- liftIO $ callKeepIns srv shrinkName vCandidate [counterExample, vRandomValue]
        liftIO $ freeVars srv [vRandomValue]
        maybe (pure ()) (throwE . ((shrinkName <> " has ") <>)) errE

        liftIO $ shrinkUpdatePhase Nothing $ Just $ fromIntegral val

        ok <-
          either (throwE . ("Property " <>)) pure <=< liftIO $
            withCallKeepIns srv propName vOk [vCandidate] $
              getVal srv

        liftIO $ shrinkUpdatePhase Nothing $ Just $ fromIntegral val

        case ok of
          True -> pure NotAcceptedShrink
          False -> do
            liftIO $ renameVar srv counterExample vCandidate
            pure AcceptedShrink

-- | Name of out-variable for this entry point.
outName :: EntryName -> VarName
outName = (<> "_out")

putVal :: (PutValue1 a) => Server -> T.Text -> a -> IO ()
putVal s name x = do
  let v = putValue1 x
  cmdErrorHandlerM ("putValue failed for " <> name <> ": ") $ FSV.putValue s name v

freeVars :: Server -> [VarName] -> IO ()
freeVars s vs = do
  mFail <- cmdFree s vs
  case mFail of
    Nothing -> pure ()
    Just err
      | any (T.isPrefixOf "Unknown variable:") (failureMsg err) -> pure ()
      | otherwise ->
          fail $ "cmdFree failed for " <> show vs <> ": " <> show (failureMsg err)

callFreeIns :: Server -> EntryName -> VarName -> [VarName] -> IO (Maybe PBTFailure)
callFreeIns s entry out ins = do
  freeVars s [out]
  r <- cmdCall s entry out ins
  freeVars s ins
  either (pure . handleRuntimeError entry) (const $ pure Nothing) r

isRuntimeError :: CmdFailure -> Bool
isRuntimeError failure = any ("runtime: " `T.isPrefixOf`) (failureLog failure)

handleRuntimeError :: EntryName -> CmdFailure -> Maybe PBTFailure
handleRuntimeError entry err
  | isRuntimeError err = Just $ "Runtime error: " <> showText (failureMsg err)
  | otherwise =
      fail . T.unpack $
        "Fatal Error on "
          <> entry
          <> ": "
          <> showText (failureMsg err)

callKeepIns :: Server -> EntryName -> VarName -> [VarName] -> IO (Maybe PBTFailure)
callKeepIns s entry out ins = do
  freeVars s [out]
  r <- cmdCall s entry out ins
  either (pure . handleRuntimeError entry) (const $ pure Nothing) r

freeOnException :: Server -> [VarName] -> IO a -> IO a
freeOnException srv vs action =
  action `onException` freeVars srv vs

withCallKeepIns :: Server -> EntryName -> VarName -> [VarName] -> (VarName -> IO a) -> IO (Either PBTFailure a)
withCallKeepIns srv entry out ins k = do
  errM <- callKeepIns srv entry out ins
  maybe
    (Right <$> (k out `finally` freeVars srv [out]))
    (pure . Left . ((entry <> " has ") <>))
    errM

withFreedVars :: Server -> [VarName] -> IO a -> IO a
withFreedVars srv vs action =
  action `finally` freeVars srv vs

-- | Bracket a single temporary var name.
withFreedVar :: Server -> VarName -> IO a -> IO a
withFreedVar srv v = withFreedVars srv [v]

renameVar :: Server -> VarName -> VarName -> IO ()
renameVar srv new old = do
  freeVars srv [new]
  cmdErrorHandlerM "cmdRename failed: " $ cmdRename srv old new

cmdErrorHandlerM :: T.Text -> IO (Maybe CmdFailure) -> IO ()
cmdErrorHandlerM msg action = action >>= maybe (pure ()) (fail . format)
  where
    format err = T.unpack msg <> show err

cmdErrorHandlerE :: T.Text -> IO (Either CmdFailure a) -> IO a
cmdErrorHandlerE msg action = action >>= either (fail . format) pure
  where
    format err = T.unpack msg <> show err

getVal :: (GetValue a) => Server -> VarName -> IO a
getVal srv name = do
  v <- getDataVal srv name
  case getValue v of
    Just b -> pure b
    Nothing -> fail $ "Expected " <> T.unpack name <> " to decode, got: " <> T.unpack (valueText v)

getDataVal :: Server -> VarName -> IO Value
getDataVal s name = do
  r <- FSV.getValue s name
  case r of
    Left msg -> fail $ "getValue failed for " <> T.unpack name <> ": " <> T.unpack msg
    Right v -> pure v

prettyVar :: Server -> VarName -> TypeName -> IO T.Text
prettyVar srv v ty = do
  kRes <- cmdErrorHandlerE "cmdKind failed: " $ cmdKind srv ty
  case kRes of
    Record -> do
      fieldsRes <- cmdFields srv ty
      case fieldsRes of
        Right fieldLines -> do
          let fnames = map fieldName fieldLines
              ftypes = map fieldType fieldLines
              isTuple = all (\(n, i :: Int) -> n == showText i) (zip fnames [0 ..])

          rendered <- forM (zip fnames ftypes) $ \(fname, fty) -> do
            let tmp = v <> "_proj_" <> fname
            sField <- withFreedVar srv tmp $ do
              cmdErrorHandlerM "project failed: " $ cmdProject srv tmp v fname
              prettyVar srv tmp fty
            pure $ if isTuple then sField else fname <> " = " <> sField

          pure $
            if isTuple
              then "(" <> T.intercalate ", " rendered <> ")"
              else "{" <> T.intercalate ", " rendered <> "}"
        Left _ -> pure "<error: record fields missing>"
    Array -> do
      valRes <- FSV.getValue srv v
      case valRes of
        Right val -> pure (valueText val)
        Left _ -> do
          dims <- cmdErrorHandlerE "cmdShape failed: " $ cmdShape srv v
          baseTy <- getBaseType ty
          buildNested v baseTy dims []
    _ -> fallbackValue
  where
    fallbackValue = do
      valRes <- FSV.getValue srv v
      case valRes of
        Right val -> pure (valueText val)
        Left _ -> pure ("<opaque:" <> ty <> ">")

    getBaseType :: TypeName -> IO TypeName
    getBaseType t = do
      k <- cmdErrorHandlerE "cmdKind failed: " $ cmdKind srv t
      case k of
        Array -> do
          et <- cmdErrorHandlerE "cmdElemtype failed: " $ cmdElemtype srv t
          getBaseType et
        _ -> pure t

    buildNested :: VarName -> TypeName -> [Int] -> [Int] -> IO T.Text
    buildNested varName baseType dims currentPath =
      case dims of
        [] -> do
          let pathStr = T.intercalate "_" (map showText currentPath)
              tmpElem = varName <> "_elem_" <> pathStr

          withFreedVar srv tmpElem $ do
            cmdErrorHandlerM "index failed: " $ cmdIndex srv tmpElem varName currentPath
            prettyVar srv tmpElem baseType
        (d : restDims) -> do
          elements <- forM [0 .. d - 1] $ \idx -> do
            buildNested varName baseType restDims (currentPath ++ [idx])

          pure $ "[" <> T.intercalate ", " elements <> "]"

getSingleInputType :: Server -> EntryName -> IO (Either PBTFailure TypeName)
getSingleInputType srv ep = do
  tys <- getInputTypes srv ep
  case tys of
    [ty] -> pure $ Right ty
    [] -> pure $ Left $ T.pack $ "Entrypoint " <> T.unpack ep <> " has no inputs (expected 1)."
    _ -> pure $ Left $ T.pack $ "Entrypoint " <> T.unpack ep <> " has >1 input (expected 1): " <> show tys

runPBT :: PBTConfig -> Server -> [PropSpec] -> IORef PBTPhase -> FilePath -> IO [Either PBTFailure PBTOutput]
runPBT config srv specs entryNameRef program = do
  eps <- cmdErrorHandlerE "Failed to get entry points: " $ cmdEntryPoints srv -- error should not be reached by the user
  forM specs $ \spec -> do
    validation <- validateOneSpec srv eps spec
    maybe (runOne spec config srv entryNameRef program) (pure . Left) validation
