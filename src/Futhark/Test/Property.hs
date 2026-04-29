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
import Data.Int (Int32, Int64)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Vector.Storable qualified as SV
import Futhark.Data
import Futhark.Server
import Futhark.Server.Values qualified as FSV
import Futhark.Util (showText)
import System.IO.Temp (withSystemTempFile)
import System.Random (mkStdGen, random, randomIO)

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
    phaseTactic :: Maybe Int32
    -- , phaseValue :: Maybe T.Text
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
stripCall name =
  T.stripSuffix ")" <=< T.stripPrefix (name <> "(") . T.strip

readMaybeText :: (Read a) => T.Text -> Maybe a
readMaybeText t =
  case reads (T.unpack t) of
    [(x, "")] -> Just x
    _ -> Nothing

runPBT :: PBTConfig -> Server -> [PropSpec] -> IORef PBTPhase -> IO [Either PBTFailure PBTOutput]
runPBT config srv specs entryNameRef =
  withSystemTempFile "pbt-scratch" $ \scratchBin _scratchHandle -> do
    eps <- cmdErrorHandlerE "Failed to get entry points: " $ cmdEntryPoints srv -- error should not be reached by the user
    forM specs $ \spec -> do
      validation <- validateOneSpec srv eps spec
      maybe (runOne spec config scratchBin srv entryNameRef) (pure . Left) validation

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
        throwE $ "No generator specified for " <> prop
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

validateGenTypes :: Server -> EntryName -> EntryName -> IO (Maybe PBTFailure)
validateGenTypes srv propName genName = fmap (either Just (const Nothing)) . runExceptT $ do
  propTy <-
    liftIO (getSingleInputType srv propName) >>= \case
      Left err -> throwE $ showText err
      Right ty -> pure ty

  genOut <- liftIO $ getOutputType srv genName

  isMatch <- liftIO $ outsMatchType srv propTy genOut

  unless isMatch $
    throwE $
      "Generator type mismatch.\nProperty "
        <> propName
        <> " expects: "
        <> propTy
        <> "\nGenerator "
        <> genName
        <> " produces: "
        <> showText genOut
        <> "\nExpected generator outputs to equal the property input type "
        <> "(possibly split for tuples/records)."

validateShrinkTypes :: Server -> EntryName -> EntryName -> IO (Maybe PBTFailure)
validateShrinkTypes srv propName shrinkName = fmap (either Just (const Nothing)) . runExceptT $ do
  propTy <-
    liftIO (getSingleInputType srv propName) >>= \case
      Left err -> throwE $ showText err
      Right ty -> pure ty

  shrinkIns <- liftIO $ getInputTypes srv shrinkName

  case shrinkIns of
    [xTy, randTy] -> do
      unless (xTy == propTy) $
        throwE $
          "Shrinker input mismatch.\n property "
            <> propName
            <> " expects: "
            <> propTy
            <> "\n shrinker "
            <> shrinkName
            <> " takes x: "
            <> xTy
            <> "\nExpected shrinker's first input to be exactly the property input type."

      unless (randTy == "i32") $
        throwE $
          "Shrinker random-value type mismatch.\n shrinker "
            <> shrinkName
            <> " takes random value: "
            <> randTy
            <> "\nExpected random value to be i32."
    tys ->
      throwE $
        "Shrinker input arity mismatch.\n shrinker "
          <> shrinkName
          <> " inputs: "
          <> showText tys
          <> "\nExpected exactly 2 inputs: ("
          <> propTy
          <> ", i32)."

  shrinkOut <- liftIO $ getOutputType srv shrinkName
  isMatch <- liftIO $ outsMatchType srv propTy shrinkOut

  unless isMatch $
    throwE $
      "Shrinker output mismatch.\n property "
        <> propName
        <> " expects: "
        <> propTy
        <> "\n shrinker "
        <> shrinkName
        <> " returns: "
        <> showText shrinkOut
        <> "\nExpected shrinker output to equal the property input type."

runOne :: PropSpec -> PBTConfig -> FilePath -> Server -> IORef PBTPhase -> IO (Either PBTFailure PBTOutput)
runOne s config scratchBin srv entryNameRef = do
  let propName = psProp s
      genName = fromMaybe (error "missing generator") (psGen s)
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

              runUpdate propName

              checkIO $ sendGenInputs srv serverSize serverSeed genName size seed

              runUpdate genName
              genOut <- liftIO $ generatorPhase genName serverSize serverSeed serverIn

              runUpdate propName

              ok <- liftIO $ withCallKeepIns srv propName serverOk [serverIn] $ \_ ->
                getVal srv serverOk

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
                    Just "auto" -> liftIO $ autoShrinkLoop scratchBin srv propName genName serverIn size seed genOut entryNameRef
                    Just sh -> liftIO $ shrinkLoop scratchBin srv propName serverIn sh seed (configShrinkTries config) entryNameRef
                    Nothing -> pure (Right Nothing)

                  case shrinkRes of
                    Left err -> throwE $ failmsg <> "Shrink failed: " <> err
                    Right (Just err) -> throwE $ failmsg <> "Shrink failed: " <> err
                    Right Nothing -> do
                      runUpdate "prettyPrint"
                      counterLog <- liftIO $ pPrintPhase propName serverIn
                      pure $ Just (failmsg <> counterLog)
    loop 0
  where
    generatorPhase genName serverSize serverSeed serverIn = do
      let genOut = outName genName
      withFreedVars srv [genOut] $ do
        _ <- callFreeIns srv genName genOut [serverSize, serverSeed]
        copyVar scratchBin srv serverIn genOut
        pure genOut

    pPrintPhase propName serverIn = do
      inputTypes <- getInputTypes srv propName
      case inputTypes of
        (ty0 : _) -> do
          prettyOut <- case psPPrint s of
            Just futPPrint -> do
              let prettyOuts = outName futPPrint
              withFreedVars srv [prettyOuts] $ do
                _ <- callFreeIns srv futPPrint prettyOuts [serverIn]
                valE <- FSV.getValue srv prettyOuts
                case valE of
                  Left err -> fail $ "getValue failed: " <> show err -- this is server error, not user error, so we can just fail
                  Right (U8Value _ bytes) -> pure $ T.pack [chr (fromIntegral b) | b <- SV.toList bytes]
                  Right _ -> pure $ T.pack "pretty printer returned non-u8 value" -- this is user error
            Nothing -> do
              prettyOut <- prettyVar srv serverIn ty0
              pure $ T.pack prettyOut

          pure $ "Minimal counterexample: " <> prettyOut
        _ -> pure "Could not retrieve input types for counterexample log."

-- Bridges IO (Maybe Error) into our ExceptT block
checkIO :: IO (Maybe e) -> ExceptT e IO ()
checkIO act = liftIO act >>= maybe (pure ()) throwE

updatePhase :: Maybe EntryName -> Maybe EntryName -> Maybe EntryName -> Maybe Int64 -> Maybe Int32 -> Maybe Int32 -> IORef PBTPhase -> IO ()
updatePhase propName phase activeTest size seed tactic phaseRef =
  writeIORef phaseRef $
    PBTPhase
      { activeTest = propName,
        phase = phase,
        shrinkWith = activeTest,
        phaseSize = size,
        phaseSeed = seed,
        phaseTactic = tactic
      }

autoShrinkLoop :: FilePath -> Server -> EntryName -> EntryName -> VarName -> Int64 -> Int32 -> VarName -> IORef PBTPhase -> IO (Either PBTFailure PBTOutput)
autoShrinkLoop scratchBin srv propName genName vIn size seed genOut phaseRef = runExceptT $ do
  let autoShrinkUpdatePhase activeTest =
        liftIO $ updatePhase (Just propName) (Just "autoShrinkLoop") activeTest (Just size) (Just seed) Nothing phaseRef

      vCand = "qc_try"
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

            -- Use checkIO for the generator inputs
            checkIO $ sendGenInputs srv serverSize serverSeed genName newSize seed

            -- Note: Since withFreedVars manages server-side resources,
            -- we lift the whole block into IO then handle the result.
            res <- liftIO $ withFreedVars srv [genOut, vCand] $ do
              autoShrinkUpdatePhase (Just genName)
              _ <- callFreeIns srv genName genOut [serverSize, serverSeed]

              _ <-
                freeOnException srv [vCand] $
                  copyVar scratchBin srv vCand genOut

              autoShrinkUpdatePhase (Just propName)
              ok <- withCallKeepIns srv propName vOk [vCand] $ \_ ->
                getVal srv vOk

              autoShrinkUpdatePhase Nothing

              if ok
                then pure Nothing -- Shrink didn't find a smaller failure
                else do
                  _ <-
                    freeOnException srv [vIn] $
                      copyVar scratchBin srv vIn vCand
                  pure (Just newSize) -- Found a smaller failing size!
            case res of
              Nothing -> loop (i - 1)
              Just s -> loop s
  loop size

data Step
  = AcceptedShrink -- Property still failed with the new candidate
  | NotAcceptedShrink -- Property passed with the new candidate
  | ErrorInShrink T.Text -- shrinker error (including property failure in shrinker)
  deriving (Eq, Show)

shrinkLoop :: FilePath -> Server -> EntryName -> VarName -> EntryName -> Int32 -> Int -> IORef PBTPhase -> IO (Either PBTFailure PBTOutput)
shrinkLoop scratchBin srv propName vIn shrinkName seed numTries phaseRef = runExceptT $ do
  -- 1. Setup Phase
  oldRef <- liftIO $ readIORef phaseRef
  let size = fromMaybe 0 (phaseSize oldRef)
      shrinkUpdatePhase activeTest = liftIO $ updatePhase (Just propName) (Just shrinkName) activeTest (Just size) (Just seed) Nothing phaseRef

  shrinkUpdatePhase Nothing

  -- 2. Validate Types
  liftIO (getSingleInputType srv propName) >>= \case
    Left err -> throwE $ showText err
    Right _ -> pure ()

  seedTy <-
    liftIO (getInputTypes srv shrinkName) >>= \case
      [_, sTy] -> pure sTy
      [] -> throwE $ "Shrinker " <> shrinkName <> " has no inputs?"
      [_] -> throwE $ "Shrinker " <> shrinkName <> " has 1 input; expected 2 (x, random value)."
      tys -> throwE $ "Shrinker " <> shrinkName <> " has " <> showText (length tys) <> " inputs; expected 2."

  unless (seedTy == "i32") $
    throwE $
      "Shrinker " <> shrinkName <> " random value must be i32, got: " <> seedTy

  let loop (pseudoRandom :: Int32) (acc :: Int) (totalCounter :: Int)
        | acc >= numTries = pure Nothing
        | otherwise = do
            stepResult <- liftIO $ oneStep size pseudoRandom

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
    oneStep size tactic = do
      -- Keeping the constants as requested
      let vTry = "qc_try"
          vOk = "qc_ok"
          vTactic = "qc_tactic"
          vInRetyped = vIn <> "_shrinktyped"

      let shrinkUpdatePhase activeTest = updatePhase (Just propName) (Just shrinkName) activeTest (Just size) (Just seed) Nothing phaseRef

      freeVars srv [vTactic]
      putVal srv vTactic tactic

      _ <-
        freeOnException srv [vInRetyped] $
          copyVar scratchBin srv vInRetyped vIn

      let shrinkOuts = outName shrinkName
      shrinkUpdatePhase Nothing

      withFreedVars srv [shrinkOuts, vTry] $ do
        _ <- callFreeIns srv shrinkName shrinkOuts [vInRetyped, vTactic]
        _ <- freeOnException srv [vTry] $ copyVar scratchBin srv vTry shrinkOuts

        shrinkUpdatePhase Nothing
        ok <- withCallKeepIns srv propName vOk [vTry] $ \_ -> getVal srv vOk
        shrinkUpdatePhase Nothing

        case ok of
          True -> pure NotAcceptedShrink
          False -> do
            freeVars srv [vIn]
            _ <- copyVar scratchBin srv vIn vTry
            pure AcceptedShrink

sendGenInputs :: Server -> VarName -> VarName -> VarName -> Int64 -> Int32 -> IO (Maybe PBTFailure)
sendGenInputs srv sizeName seedName genName size seed = do
  insE <- cmdInputs srv genName
  case insE of
    -- Our fault since this should not happen for the user
    Left err -> fail $ show err
    Right ins' -> case map inputType ins' of
      [sizeTy, seedTy]
        | sizeTy == "i64" && seedTy == "i32" -> do
            putVal srv sizeName size
            putVal srv seedName seed
            pure Nothing
        | otherwise ->
            pure . Just $ T.pack $ "Expected size and seed to be i64 and i32, got: size type=" <> T.unpack sizeTy <> " and seed type=" <> T.unpack seedTy
      tys ->
        pure . Just $ T.pack $ "Expected generator to have exactly two inputs, got types: " <> show tys

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

callFreeIns :: Server -> EntryName -> VarName -> [VarName] -> IO [VarName]
callFreeIns s entry out ins = do
  freeVars s [out]
  r <- cmdCall s entry out ins
  freeVars s ins
  either (\err -> fail $ T.unpack entry <> ": " <> show (failureMsg err)) pure r

callKeepIns :: Server -> EntryName -> VarName -> [VarName] -> IO [VarName]
callKeepIns s entry out ins = do
  freeVars s [out]
  r <- cmdCall s entry out ins
  either (\err -> fail $ T.unpack entry <> ": " <> show (failureMsg err)) pure r

freeOnException :: Server -> [VarName] -> IO a -> IO a
freeOnException srv vs action =
  action `onException` freeVars srv vs

withCallKeepIns :: Server -> EntryName -> VarName -> [VarName] -> (VarName -> IO a) -> IO a
withCallKeepIns srv entry out ins k = do
  _ <- callKeepIns srv entry out ins
  k out `finally` freeVars srv [out]

withFreedVars :: Server -> [VarName] -> IO a -> IO a
withFreedVars srv vs action =
  action `finally` freeVars srv vs

-- | Bracket a single temporary var name.
withFreedVar :: Server -> VarName -> IO a -> IO a
withFreedVar srv v = withFreedVars srv [v]

copyVar :: FilePath -> Server -> VarName -> VarName -> IO ()
copyVar scratchBin srv outVar inVar = do
  typ <- cmdErrorHandlerE "cmdType failed: " $ cmdType srv inVar
  cmdErrorHandlerM "cmdStore failed: " $ cmdStore srv scratchBin [inVar]
  freeVars srv [outVar]
  cmdErrorHandlerM "cmdRestore failed: " $ cmdRestore srv scratchBin [(outVar, typ)]

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

prettyVar :: Server -> VarName -> TypeName -> IO String
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
          else pure (T.unpack fname <> " = " <> sField)

      if isTuple
        then pure $ "(" <> intercalate ", " rendered <> ")"
        else do
          pure $ "{" <> intercalate ", " rendered <> "}"
    Left _notARecord -> do
      valRes <- FSV.getValue srv v
      case valRes of
        Right val -> pure (prettyLeaf val)
        Left _opaqueOrFailed -> pure ("<opaque:" <> T.unpack ty <> ">")

prettyLeaf :: Value -> String
prettyLeaf = T.unpack . valueText

getSingleInputType :: Server -> EntryName -> IO (Either PBTFailure TypeName)
getSingleInputType srv ep = do
  tys <- getInputTypes srv ep
  case tys of
    [ty] -> pure $ Right ty
    [] -> pure $ Left $ T.pack $ "Entrypoint " <> T.unpack ep <> " has no inputs (expected 1)."
    _ -> pure $ Left $ T.pack $ "Entrypoint " <> T.unpack ep <> " has >1 input (expected 1): " <> show tys

outsMatchType :: Server -> TypeName -> TypeName -> IO Bool
outsMatchType srv propTy out
  | out == propTy = pure True
  | otherwise = do
      res <- cmdFields srv propTy
      pure $ either (const False) (\fs -> [out] == map fieldType fs) res
