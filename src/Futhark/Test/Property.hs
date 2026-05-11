{-# LANGUAGE LambdaCase #-}

module Futhark.Test.Property
  ( runPBT,
    PBTConfig (..),
    PBTPhase (..),
    PropSpec (..),
    lookupArgRead,
    lookupArgText,
    stripCall,
    PBTOutput,
    PBTFailure,
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Char (chr)
import Data.IORef
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Vector.Storable qualified as SV
import Data.Word (Word16, Word32, Word64, Word8)
import Futhark.Data
import Futhark.Server
import Futhark.Server.Values qualified as FSV
import Futhark.Util (showText)
import Numeric.Half (Half)
import System.Random (mkStdGen, random, randomIO, randoms)

data PBTConfig = PBTConfig
  { configNumTests :: Int32,
    configMaxSize :: Int64,
    configSeed :: Maybe Int32,
    configShrinkTries :: Int
  }
  deriving (Show, Eq)

data PropSpec = PropSpec
  { psProp :: T.Text,
    psGen :: Maybe T.Text,
    psShrink :: Maybe T.Text,
    psSize :: Maybe Int64,
    psPPrint :: Maybe T.Text
  }
  deriving (Show, Eq)

data PBTPhase = PBTPhase
  { activeTest :: Maybe EntryName, -- property being tested (same for generator and shrinker phases)
    phase :: Maybe EntryName, -- current entrypoint being called (property, generator, or shrinker)
    shrinkWith :: Maybe EntryName, -- property or generator (only for auto)
    phaseSize :: Maybe Int64,
    phaseSeed :: Maybe Int32,
    phaseRandom :: Maybe Int32
  }
  deriving (Show, Eq)

type PBTOutput = Maybe T.Text

type PBTFailure = T.Text

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

runPBT :: PBTConfig -> Server -> [PropSpec] -> IORef PBTPhase -> IO [Either PBTFailure PBTOutput]
runPBT config srv specs entryNameRef = do
  eps <- cmdErrorHandlerE "Failed to get entry points: " $ cmdEntryPoints srv -- error should not be reached by the user
  forM specs $ \spec -> do
    validation <- validateOneSpec srv eps spec
    maybe (runOne spec config srv entryNameRef) (pure . Left) validation

validateOneSpec :: Server -> [EntryName] -> PropSpec -> IO (Maybe PBTFailure)
validateOneSpec srv eps spec = do
  let prop = psProp spec

  result <- runExceptT $ do
    unless (prop `elem` eps) $
      throwE $
        "Property entry point not found: " <> prop

    liftIO (validatePropTypes srv prop) >>= maybe (pure ()) throwE

    genName <- case psGen spec of
      Nothing ->
        pure "$no_gen$" -- placeholder name for entrypoint that user cannot write.
        -- not the most elegant solution but allows us to reuse most of the logic.
      Just g
        | g `notElem` eps ->
            throwE $ "Generator is not a server entry point: " <> g
      Just g ->
        pure g

    liftIO (validateGenTypes srv prop genName) >>= maybe (pure ()) throwE

    case psShrink spec of
      Nothing -> pure ()
      Just "auto" -> pure ()
      Just sh -> do
        unless (sh `elem` eps) $
          throwE $
            "Shrinker is not a server entry point: " <> sh
        liftIO (validateShrinkTypes srv prop sh) >>= maybe (pure ()) throwE

    case psPPrint spec of
      Nothing -> pure ()
      Just pp -> do
        unless (pp `elem` eps) $
          throwE $
            "Pretty-printer is not a server entry point: " <> pp
        liftIO (validatePPrintTypes srv prop pp) >>= maybe (pure ()) throwE

  pure $ either Just (const Nothing) result

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

getInputTypes :: Server -> EntryName -> IO [TypeName]
getInputTypes srv entry = do
  ins <-
    cmdErrorHandlerE ("Failed to get input types for " <> entry <> ": ") $
      cmdInputs srv entry
  pure $ map inputType ins

getOutputType :: Server -> EntryName -> IO TypeName
getOutputType s entry = do
  out <-
    cmdErrorHandlerE ("Failed to get output types for " <> entry <> ": ") $
      cmdOutput s entry
  pure $ outputType out

haskellFutGenerator :: Server -> EntryName -> TypeName -> Int64 -> Int32 -> IO (Maybe PBTFailure)
haskellFutGenerator srv candidate genTy size seed = do
  resFields <- cmdFields srv genTy
  case resFields of
    -- Composite type
    Right fields | not (null fields) -> do
      let fieldVarNames = [candidate <> "_$compositeVal" <> fieldName fld | fld <- fields]

      -- Generate each field recursively
      results <- forM (zip3 [0 ..] fieldVarNames fields) $ \(i, fVarName, fld) -> do
        let branchGen = mkStdGen (fromIntegral seed + fromIntegral i)
        let (pseudoRandom, _) = random branchGen
        haskellFutGenerator srv fVarName (fieldType fld) size pseudoRandom

      case sequence results of
        Just err -> pure $ Just $ T.unlines err
        Nothing -> do
          freeVars srv [candidate]
          cmdErrorHandlerM ("Failed to pack record " <> candidate <> ": ") $
            cmdNew srv candidate genTy fieldVarNames

          freeVars srv fieldVarNames
          pure Nothing

    -- Primitive or Array
    _ -> do
      let (dims, baseTy) = getFutBaseType genTy
      let shapeList = replicate (length dims) size
      case makeFutPrimitiveValue baseTy shapeList seed of
        Left err -> pure $ Just err
        Right val -> do
          freeVars srv [candidate]
          putRes <- FSV.putValue srv candidate val
          case putRes of
            Nothing -> pure Nothing
            Just err -> pure $ Just $ T.pack $ show err

getFutBaseType :: T.Text -> ([Int64], T.Text)
getFutBaseType t
  | "[]" `T.isPrefixOf` t = let (ds, base) = getFutBaseType (T.drop 2 t) in (0 : ds, base)
  | otherwise = ([], t)

makeFutPrimitiveValue :: TypeName -> [Int64] -> Int32 -> Either PBTFailure Value
makeFutPrimitiveValue ty shapeList seed =
  let gen = mkStdGen (fromIntegral seed)
      shape = SV.fromList $ map fromIntegral shapeList
      totalElems = fromIntegral $ product shapeList
   in case ty of
        "i8" -> Right $ I8Value shape $ SV.fromList $ take totalElems (randoms gen)
        "i16" -> Right $ I16Value shape $ SV.fromList $ take totalElems (randoms gen)
        "i32" -> Right $ I32Value shape $ SV.fromList $ take totalElems (randoms gen)
        "i64" -> Right $ I64Value shape $ SV.fromList $ take totalElems (randoms gen)
        "u8" -> Right $ U8Value shape $ SV.fromList $ take totalElems (randoms gen)
        "u16" -> Right $ U16Value shape $ SV.fromList $ take totalElems (randoms gen)
        "u32" -> Right $ U32Value shape $ SV.fromList $ take totalElems (randoms gen)
        "u64" -> Right $ U64Value shape $ SV.fromList $ take totalElems (randoms gen)
        "f16" -> Right $ F16Value shape $ SV.fromList $ take totalElems $ map (realToFrac :: Float -> Half) (randoms gen)
        "f32" -> Right $ F32Value shape $ SV.fromList $ take totalElems (randoms gen)
        "f64" -> Right $ F64Value shape $ SV.fromList $ take totalElems (randoms gen)
        "bool" -> Right $ BoolValue shape $ SV.fromList $ take totalElems (randoms gen)
        _ -> Left ("Batch generation not implemented for: " <> ty)

validateGenTypes :: Server -> EntryName -> EntryName -> IO (Maybe PBTFailure)
validateGenTypes _ _ "$no_gen$" = pure Nothing -- should have more logic to see if we can actually generate anything
validateGenTypes srv propName genName = fmap (either Just (const Nothing)) . runExceptT $ do
  -- find expected input types for generator
  genIns <- liftIO $ getInputTypes srv genName
  case genIns of
    [sizeTy, seedTy] -> do
      unless (sizeTy == "i64" && seedTy == "i32") $
        throwE $
          "Generator input type mismatch.\nGenerator "
            <> genName
            <> " takes size: "
            <> sizeTy
            <> " and seed: "
            <> seedTy
            <> "\nExpected size and seed to be i64 and i32, got: size type="
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

  isMatch <- liftIO $ outsMatchType srv propTy genOut

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
            concat
              [ [ "Property " <> propName <> " expects " <> propTy <> " but shrinker " <> shrinkName <> " takes " <> xTy
                | xTy /= propTy
                ],
                [ "Shrinker " <> shrinkName <> " takes random value " <> randTy <> " but expected i32"
                | randTy /= "i32"
                ]
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
          <> ", i32)."

  -- validate output
  shrinkOut <- liftIO $ getOutputType srv shrinkName
  isMatch <- liftIO $ outsMatchType srv propTy shrinkOut

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

validatePPrintTypes :: Server -> EntryName -> EntryName -> IO (Maybe PBTFailure)
validatePPrintTypes srv propName ppName = fmap (either Just (const Nothing)) . runExceptT $ do
  propTy <-
    liftIO (getSingleInputType srv propName) >>= \case
      Left err -> throwE $ showText err
      Right ty -> pure ty

  ppIns <- liftIO $ getInputTypes srv ppName

  case ppIns of
    [xTy] -> do
      unless (xTy == propTy) $
        throwE $
          "Pretty-printer input type mismatch.\nProperty "
            <> propName
            <> " expects: "
            <> propTy
            <> "\nPretty-printer "
            <> ppName
            <> " takes: "
            <> xTy
            <> "\nExpected pretty-printer input to be exactly the property input type."
    tys ->
      throwE $
        "Pretty-printer input arity mismatch.\nProperty "
          <> propName
          <> " expects: "
          <> propTy
          <> "\nPretty-printer "
          <> ppName
          <> " takes: "
          <> showText tys
          <> "\nExpected exactly 1 input with property input type."

  ppOut <- liftIO $ getOutputType srv ppName

  unless (ppOut == "[]u8") $
    throwE $
      "Pretty-printer output mismatch.\nPretty-printer "
        <> ppName
        <> " returns: "
        <> ppOut
        <> "\nExpected pretty-printer output to be []u8."

runOne :: PropSpec -> PBTConfig -> Server -> IORef PBTPhase -> IO (Either PBTFailure PBTOutput)
runOne s config srv entryNameRef = do
  let propName = psProp s
      -- genName = fromMaybe (error "missing generator") (psGen s)
      genName = fromMaybe "$no_gen$" (psGen s) -- placeholder to allow attribute to not specify a generator.
      size = fromMaybe (configMaxSize config) (psSize s)
      numTests = configNumTests config
      serverSize = "runPBT_size"
      serverSeed = "runPBT_seed"
      serverIn = "runPBT_input"
      serverOk = "runPBT_ok"

  runExceptT $ do
    let loop i
          | i >= numTests = pure Nothing
          | otherwise = do
              randomValue <- liftIO (randomIO :: IO Int32)
              let seed = fromMaybe randomValue (configSeed config)
              let runUpdate ph = liftIO $ updatePhase (Just propName) (Just ph) Nothing (Just size) (Just seed) Nothing entryNameRef

              runUpdate genName
              generatorCandidateE <-
                if genName == "$no_gen$"
                  then do
                    liftIO $ freeVars srv [serverIn]
                    propType <-
                      liftIO (getSingleInputType srv propName) >>= \case
                        Left err -> throwE $ showText err
                        Right ty -> pure ty

                    errM <- liftIO $ haskellFutGenerator srv serverIn propType size seed
                    maybe (pure $ Right ()) (throwE . ("Haskell generator failed: " <>)) errM
                  else do liftIO $ generatorPhase genName serverSize serverSeed serverIn size seed
              either throwE pure generatorCandidateE

              runUpdate propName

              okE <- liftIO $
                withCallKeepIns srv propName serverOk [serverIn] $
                  \vOk -> getVal srv vOk
              ok <- case okE of
                Right b -> pure b
                Left err -> do
                  valuePPrint <- liftIO $ pPrintPhase propName serverIn size seed
                  throwE $
                    "Property "
                      <> propName
                      <> " failed with candidate="
                      <> valuePPrint
                      <> " with error: "
                      <> err

              if ok
                then loop (i + 1)
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
                    Just "auto" ->
                      liftIO $ autoShrinkLoop srv propName genName serverIn size seed entryNameRef
                    Just sh -> do
                      userShrinkRes <-
                        liftIO $
                          shrinkLoop srv propName serverIn sh seed (configShrinkTries config) entryNameRef

                      case userShrinkRes of
                        Right Nothing ->
                          pure (Right Nothing)
                        Right (Just err) -> do
                          autoRes <-
                            liftIO $
                              autoShrinkLoop srv propName genName serverIn size seed entryNameRef

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
                              autoShrinkLoop srv propName genName serverIn size seed entryNameRef

                          pure $
                            Right $
                              Just $
                                "User shrinker failed: "
                                  <> err
                                  <> "\nAttempted auto-shrinker fallback."
                                  <> formatAutoShrinkResult autoRes
                    Nothing ->
                      pure (Right Nothing)

                  case shrinkRes of
                    Left err -> do
                      runUpdate "prettyPrint"
                      counterLog <- liftIO $ pPrintPhase propName serverIn size seed
                      pure $
                        Just $
                          failmsg
                            <> "Shrinking failed: "
                            <> err
                            <> "\nCounterexample: "
                            <> counterLog
                    Right (Just note) -> do
                      runUpdate "prettyPrint"
                      counterLog <- liftIO $ pPrintPhase propName serverIn size seed
                      pure $
                        Just $
                          failmsg
                            <> "Shrinking note: "
                            <> note
                            <> "\nCounterexample: "
                            <> counterLog
                    Right Nothing -> do
                      runUpdate "prettyPrint"
                      counterLog <- liftIO $ pPrintPhase propName serverIn size seed
                      pure $ Just (failmsg <> "Counterexample: " <> counterLog)
    loop 0
  where
    generatorPhase genName serverSize serverSeed serverIn size seed = do
      insE <- cmdInputs srv genName
      either
        (fail . show)
        (\_ -> putVal srv serverSize size >> putVal srv serverSeed seed)
        insE
      let genOut = outName genName
      withFreedVars srv [genOut] $ runExceptT $ do
        liftIO (callFreeIns srv genName genOut [serverSize, serverSeed]) >>= \case
          Nothing -> pure ()
          Just err ->
            throwE $
              "Generator "
                <> genName
                <> " failed with size="
                <> showText size
                <> " and seed="
                <> showText seed
                <> " with error: "
                <> err
        liftIO $ renameVar srv serverIn genOut

    pPrintPhase propName serverIn size seed = do
      pPrintWithFallback srv s propName serverIn size seed

formatAutoShrinkResult :: Either PBTFailure PBTOutput -> T.Text
formatAutoShrinkResult autoRes =
  case autoRes of
    Right Nothing ->
      "\nAuto-shrinker fallback completed."
    Right (Just msg) ->
      "\nAuto-shrinker fallback completed with note: " <> msg
    Left autoErr ->
      "\nAuto-shrinker fallback also failed: " <> autoErr

updatePhase :: Maybe EntryName -> Maybe EntryName -> Maybe EntryName -> Maybe Int64 -> Maybe Int32 -> Maybe Int32 -> IORef PBTPhase -> IO ()
updatePhase propName phase activeTest size seed randomValue phaseRef =
  writeIORef phaseRef $
    PBTPhase
      { activeTest = propName,
        phase = phase,
        shrinkWith = activeTest,
        phaseSize = size,
        phaseSeed = seed,
        phaseRandom = randomValue
      }

autoShrinkLoop :: Server -> EntryName -> EntryName -> VarName -> Int64 -> Int32 -> IORef PBTPhase -> IO (Either PBTFailure PBTOutput)
autoShrinkLoop srv propName genName vCounterExample size seed phaseRef = runExceptT $ do
  let autoShrinkUpdatePhase activeTest =
        liftIO $ updatePhase (Just propName) (Just "autoShrinkLoop") activeTest (Just size) (Just seed) Nothing phaseRef

      vCandidate = "qc_try"
      vOk = "qc_ok"
      serverSize = "qc_size"
      serverSeed = "qc_seed"

  -- 1. Validate property input arity using our Either-to-ExceptT logic
  liftIO (getInputTypes srv propName) >>= \case
    [_] -> pure ()
    [] -> throwE $ "Property " <> propName <> " has no inputs?"
    tys -> throwE $ "Property " <> propName <> " has >1 input: " <> showText tys

  -- 2. The Loop
  let loop i
        | i <= 1 = pure Nothing
        | otherwise = do
            let newSize = i - 1

            liftIO $ putVal srv serverSize newSize >> putVal srv serverSeed seed

            -- Note: Since withFreedVars manages server-side resources,
            -- we lift the whole block into IO then handle the result.
            res <- liftIO $ withFreedVars srv [vCandidate] $ runExceptT $ do
              liftIO $ autoShrinkUpdatePhase (Just genName)
              errM <-
                if genName == "$no_gen$"
                  then do
                    propertyType <-
                      liftIO (getSingleInputType srv propName) >>= \case
                        Left err -> throwE $ showText err
                        Right ty -> pure ty
                    liftIO $ freeVars srv [serverSize, serverSeed]
                    errM <- liftIO $ haskellFutGenerator srv vCandidate propertyType newSize seed
                    maybe (pure Nothing) (throwE . ("Haskell generator failed for auto shrinking: " <>)) errM
                  else liftIO $ callFreeIns srv genName vCandidate [serverSize, serverSeed]
              maybe (pure ()) (throwE . (("Generator failed: " <> genName <> " has ") <>)) errM

              liftIO $ autoShrinkUpdatePhase (Just propName)
              okE <- liftIO $
                withCallKeepIns srv propName vOk [vCandidate] $
                  \vOk' -> getVal srv vOk'
              ok <- either (throwE . ("Property " <>)) pure okE

              liftIO $ autoShrinkUpdatePhase Nothing

              if ok
                then pure Nothing -- Shrink didn't find a smaller failure
                else do
                  _ <-
                    liftIO $
                      freeOnException srv [vCounterExample] $
                        renameVar srv vCounterExample vCandidate
                  pure (Just newSize) -- Found a smaller failing size!
            case res of
              Left err -> throwE err
              Right Nothing -> loop (i - 1)
              Right (Just s) -> loop s
  loop size

data Step
  = AcceptedShrink -- Property still failed with the new candidate
  | NotAcceptedShrink -- Property passed with the new candidate
  | ErrorInShrink T.Text -- shrinker error (including property failure in shrinker)
  deriving (Eq, Show)

shrinkLoop :: Server -> EntryName -> VarName -> EntryName -> Int32 -> Int -> IORef PBTPhase -> IO (Either PBTFailure PBTOutput)
shrinkLoop srv propName counterExample shrinkName seed numTries phaseRef = runExceptT $ do
  -- 1. Setup Phase
  oldRef <- liftIO $ readIORef phaseRef
  let size = fromMaybe 0 (phaseSize oldRef)
      shrinkUpdatePhase activeTest = liftIO $ updatePhase (Just propName) (Just shrinkName) activeTest (Just size) (Just seed) Nothing phaseRef

  shrinkUpdatePhase Nothing

  let loop (pseudoRandom :: Int32) (acc :: Int) (totalCounter :: Int)
        | acc >= numTries = pure Nothing
        | otherwise = do
            stepResultE <- liftIO $ oneStep size pseudoRandom
            stepResult <-
              either
                ( \err ->
                    throwE $
                      "Error in shrinker "
                        <> shrinkName
                        <> " with random="
                        <> showText pseudoRandom
                        <> ": "
                        <> err
                )
                pure
                stepResultE

            let branchGen = mkStdGen (fromIntegral pseudoRandom)
            let (pseudoRandom', _) = random branchGen

            case stepResult of
              AcceptedShrink ->
                loop pseudoRandom' 0 (totalCounter + 1) -- Reset trials on success
              NotAcceptedShrink ->
                loop pseudoRandom' (acc + 1) (totalCounter + 1) -- Increment trials
              ErrorInShrink err ->
                throwE $ "Error in shrinker: " <> err
  loop seed 0 0
  where
    oneStep size randomValue = do
      let vOk = "qc_ok"
          vRandomValue = "qc_random"

      let shrinkUpdatePhase activeTest randomNum = updatePhase (Just propName) (Just shrinkName) activeTest (Just size) (Just seed) randomNum phaseRef

      freeVars srv [vRandomValue]
      putVal srv vRandomValue randomValue

      let shrinkerCandidate = outName shrinkName
      shrinkUpdatePhase Nothing $ Just randomValue

      withFreedVar srv shrinkerCandidate $ runExceptT $ do
        errE <- liftIO $ callKeepIns srv shrinkName shrinkerCandidate [counterExample, vRandomValue]
        liftIO $ freeVars srv [vRandomValue]
        maybe (pure ()) (throwE . ((shrinkName <> " has ") <>)) errE

        liftIO $ shrinkUpdatePhase Nothing $ Just randomValue

        okE <- liftIO $
          withCallKeepIns srv propName vOk [shrinkerCandidate] $
            \vOk' -> getVal srv vOk'
        ok <- either (throwE . ("Property " <>)) pure okE

        liftIO $ shrinkUpdatePhase Nothing $ Just randomValue

        case ok of
          True -> pure NotAcceptedShrink
          False -> do
            liftIO $ renameVar srv counterExample shrinkerCandidate
            pure AcceptedShrink

pPrintWithFallback :: Server -> PropSpec -> EntryName -> VarName -> Int64 -> Int32 -> IO T.Text
pPrintWithFallback srv spec propName serverIn size seed = do
  inputTypes <- getInputTypes srv propName
  case inputTypes of
    [] ->
      pure "Could not retrieve input type for counterexample."
    ty0 : _ -> do
      case psPPrint spec of
        Nothing ->
          prettyVar srv serverIn ty0
        Just futPPrint -> do
          let prettyOuts = outName futPPrint

          userPrinterE <- withFreedVars srv [prettyOuts] $ runExceptT $ do
            errM <- liftIO $ callKeepIns srv futPPrint prettyOuts [serverIn]
            maybe
              (pure ())
              ( throwE
                  . ( ( "Pretty printer "
                          <> futPPrint
                          <> " failed with size="
                          <> showText size
                          <> " and seed="
                          <> showText seed
                          <> " with error: "
                      )
                        <>
                    )
              )
              errM

            valE <- liftIO $ FSV.getValue srv prettyOuts
            case valE of
              Left err ->
                throwE $ "getValue failed for pretty-printer output: " <> err
              Right (U8Value _ bytes) ->
                pure $ T.pack [chr (fromIntegral b) | b <- SV.toList bytes]
              Right v ->
                throwE $
                  "Pretty printer "
                    <> futPPrint
                    <> " returned non-u8 value: "
                    <> valueText v

          case userPrinterE of
            Right rendered ->
              pure rendered
            Left ppErr -> do
              runnerPrinterE <- try (prettyVar srv serverIn ty0) :: IO (Either SomeException T.Text)
              case runnerPrinterE of
                Right fallbackRendered ->
                  pure $
                    "Pretty-printer failed: "
                      <> ppErr
                      <> "\nFallback counterexample: "
                      <> fallbackRendered
                Left runnerErr ->
                  pure $
                    "Counterexample found, but printing failed.\n"
                      <> "Pretty-printer error: "
                      <> ppErr
                      <> "\nRunner-printer error: "
                      <> showText (show runnerErr)

-- | Name of out-variable for this entry point.
outName :: EntryName -> VarName
outName = (<> "_out")

putVal :: (PutValue1 a) => Server -> T.Text -> a -> IO ()
putVal s name x = do
  let v = putValue1 x
  -- freeVars s [name]
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
  fieldsRes <- cmdFields srv ty
  case fieldsRes of
    Right fieldLines -> do
      -- convert from Field to text and type pairs

      let fnames = map fieldName fieldLines
          ftypes = map fieldType fieldLines
          isTuple =
            and
              [ fname == showText i
              | (fname, i) <- zip fnames [0 .. (length fnames - 1)]
              ]

      rendered <- forM (zip fnames ftypes) $ \(fname, fty) -> do
        let tmp =
              if isTuple
                then v <> "_tup_" <> fname
                else v <> "_rec_" <> fname

        sField <- withFreedVar srv tmp $ do
          cmdErrorHandlerM ("cmdProject failed for field " <> fname <> ": ") $ cmdProject srv tmp v fname

          prettyVar srv tmp fty

        if isTuple
          then pure sField
          else pure (fname <> " = " <> sField)

      if isTuple
        then pure $ "(" <> T.intercalate ", " rendered <> ")"
        else do
          pure $ "{" <> T.intercalate ", " rendered <> "}"
    Left _notARecord -> do
      valRes <- FSV.getValue srv v
      case valRes of
        Right val -> pure (valueText val)
        Left _opaqueOrFailed -> pure ("<opaque:" <> ty <> ">")

getSingleInputType :: Server -> EntryName -> IO (Either PBTFailure TypeName)
getSingleInputType srv ep = do
  tys <- getInputTypes srv ep
  case tys of
    [ty] -> pure $ Right ty
    [] -> pure $ Left $ T.pack $ "Entrypoint " <> T.unpack ep <> " has no inputs (expected 1)."
    _ -> pure $ Left $ T.pack $ "Entrypoint " <> T.unpack ep <> " has >1 input (expected 1): " <> show tys

outsMatchType :: Server -> TypeName -> TypeName -> IO Bool
outsMatchType _ propTy out =
  pure (out == propTy)
