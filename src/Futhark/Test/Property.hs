{-# LANGUAGE LambdaCase #-}

module Futhark.Test.Property
  (
    runPBT,
    PBTConfig (..),
    PBTPhase (..),
    PropSpec (..),
    lookupArgRead,
    lookupArgText,
    stripCall,
    PBTOutput,
    PBTFailure
  )
where
import Control.Exception
import Control.Monad
import Data.Char (chr)
import Data.Int (Int32, Int64, Int8)
import Data.IORef
import Data.List (intercalate)
import Data.Maybe (maybe, fromMaybe)
import Data.Text qualified as T
import Data.Vector.Storable qualified as SV
import Futhark.Data
import Futhark.Server
import Futhark.Server.Values qualified as FSV
import Futhark.Util (showText)
import System.Random (randomIO)
import System.IO.Temp (withSystemTempFile)
import Futhark.IR.Mem.LMAD (iota)

data PBTConfig = PBTConfig
  { configNumTests :: Int32,
    configMaxSize :: Int64,
    configSeed :: Maybe Int32
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

data PBTPhase
  = PBTPhase
    { activeTest :: Maybe EntryName  -- property being tested (same for generator and shrinker phases)
    , phase :: Maybe EntryName -- current entrypoint being called (property, generator, or shrinker)
    , shrinkWith :: Maybe EntryName -- property or generator (only for auto)
    , phaseSize :: Maybe Int64
    , phaseSeed :: Maybe Int32
    , phaseTactic :: Maybe Int32
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
    epsE <- cmdEntryPoints srv
    case epsE of
      Left err -> fail $ show err -- cannot be reached by user
      Right eps ->
        forM specs $ \spec -> do
          validation <- validateOneSpec srv eps spec
          maybe (runOne spec config scratchBin srv entryNameRef) (pure . Left) validation

validateOneSpec :: Server -> [EntryName] -> PropSpec -> IO (Maybe PBTFailure)
validateOneSpec srv eps spec = do
  let prop = psProp spec
  if prop `notElem` eps
  then pure . Just $
    "Property entry point not found: " <> prop
  else do
    propErr <- validatePropTypes srv prop
    case propErr of
      Just err ->
        pure $ Just err
      Nothing -> do
        let genNameE = case psGen spec of
              Just g | g `elem` eps ->
                Right g
              Just g ->
                Left $
                  "Generator is not a server entry point: " <> g
              Nothing ->
                Left $ "No generator specified for " <> prop
        case genNameE of
          Left err ->
            pure $ Just err -- user error
          Right genName -> do
            genErr <- validateGenTypes srv prop genName
            case genErr of
              Just err ->
                pure $ Just err
              Nothing ->
                case psShrink spec of
                  Just "auto" ->
                    pure $ Nothing
                  Just sh | sh `elem` eps -> do
                    shrinkErr <- validateShrinkTypes srv prop sh
                    case shrinkErr of
                      Just err -> pure $ Just err
                      Nothing -> pure $ Nothing
                  Just sh ->
                    pure . Just $
                      "Shrinker is not a server entry point: " <> sh
                  Nothing ->
                    pure $ Nothing

validatePropTypes :: Server -> EntryName -> IO (Maybe PBTFailure)
validatePropTypes srv propName = do
  -- Exactly one input
  ins <- getInputTypes srv propName
  validInputType <- case ins of
    [_] -> pure Nothing
    [] -> pure $ Just $ T.pack $ "Property " <> T.unpack propName <> " has no inputs? Expected 1."
    ins' -> pure $ Just $ T.pack $ "Property " <> T.unpack propName <> " has " <> show (length ins') <> " inputs; expected 1: " <> show ins'

  case validInputType of
    Just err -> pure $ Just err
    Nothing -> do
      -- Exactly one output, and it must be bool
      outs <- getOutputTypes srv propName
      case outs of
        ["bool"] -> pure Nothing
        [] -> pure $ Just $ T.pack $ "Property " <> T.unpack propName <> " has no outputs? Expected 1 bool."
        [ty] -> pure $ Just $ T.pack $ "Property " <> T.unpack propName <> " output must be bool, got: " <> T.unpack ty
        ins' -> pure $ Just $ T.pack $ "Property " <> T.unpack propName <> " has " <> show (length ins') <> " outputs; expected 1 bool: " <> show ins'

getInputTypes :: Server -> EntryName -> IO [TypeName]
getInputTypes srv entry = do
  ins <- cmdInputs srv entry
  case ins of
    Left err -> fail $ show err -- cannot be reached by user
    Right ins' -> pure $ map inputType ins'

getOutputTypes :: Server -> EntryName -> IO [TypeName]
getOutputTypes s entry = do
  outs <- cmdOutputs s entry
  case outs of
    Left err -> fail $ show err
    Right outs' -> pure $ map outputType outs'

validateGenTypes :: Server -> EntryName -> EntryName -> IO (Maybe PBTFailure)
validateGenTypes srv propName genName = do
  propTyE <- getSingleInputType srv propName
  case propTyE of
    Left err -> pure $ Just $ T.pack $ "getInputTypes failed for " <> T.unpack propName <> ": " <> show err
    Right propTy -> do
      genOuts <- getOutputTypes srv genName
      ok <- outsMatchType srv propTy genOuts
      pure $ case ok of
        True -> Nothing
        False -> Just $ T.pack $
          "Generator type mismatch.\n"
            <> "  property "
            <> T.unpack propName
            <> " expects: "
            <> T.unpack propTy
            <> "\n"
            <> "  generator "
            <> T.unpack genName
            <> " produces: "
            <> show genOuts
            <> "\n"
            <> "Expected generator outputs to equal the property input type (possibly split for tuples/records)."

validateShrinkTypes :: Server -> EntryName -> EntryName -> IO (Maybe PBTFailure)
validateShrinkTypes srv propName shrinkName = do
  propTyE <- getSingleInputType srv propName
  case propTyE of
    Left err -> pure $ Just $ T.pack $ "getInputTypes failed for " <> T.unpack propName <> ": " <> show err
    Right propTy -> do

      -- 1. Check Input Types
      shrinkIns <- getInputTypes srv shrinkName
      case shrinkIns of
        [xTy, tacTy] -> 
          if xTy /= propTy 
          then pure . Just $ T.pack $ 
              "Shrinker input mismatch.\n property " <> T.unpack propName <> 
              " expects: " <> T.unpack propTy <> "\n shrinker " <> 
              T.unpack shrinkName <> " takes x: " <> T.unpack xTy <> 
              "\nExpected shrinker's first input to be exactly the property input type."
          else if tacTy /= "i32"
          then pure . Just $ T.pack $
              "Shrinker tactic type mismatch.\n shrinker " <> T.unpack shrinkName <> 
              " takes tactic: " <> T.unpack tacTy <> "\nExpected tactic to be i32."
          else validateOutputs propTy
        
        tys -> pure . Just $ T.pack $
              "Shrinker input arity mismatch.\n shrinker " <> T.unpack shrinkName <> 
              " inputs: " <> show tys <> "\nExpected exactly 2 inputs: (" <> 
              T.unpack propTy <> ", i32)."
  where
    validateOutputs propTy = do
      shrinkOuts <- getOutputTypes srv shrinkName
      case shrinkOuts of
        [] -> pure . Just $ T.pack $ "Shrinker " <> T.unpack shrinkName <> " has no outputs? Expected (testType, bool)."
        shrinkOuts' -> do
          let doneTy = last shrinkOuts'
              yOuts  = init shrinkOuts'

          if doneTy /= "i8"
          then pure . Just $ T.pack $
               "Shrinker output mismatch.\n shrinker " <> T.unpack shrinkName <> 
               " last output type: " <> T.unpack doneTy <> 
               "\nExpected last output to be bool (done flag)."
          else do
            ok <- outsMatchType srv propTy yOuts
            pure $
              if not ok
              then Just $ T.pack $
                  "Shrinker output mismatch.\n property " <> T.unpack propName <> 
                  " expects: " <> T.unpack propTy <> "\n shrinker " <> 
                  T.unpack shrinkName <> " returns y parts: " <> show yOuts <> 
                  " and done: bool\nExpected shrinker's y outputs to equal the property input type."
              else Nothing

runOne :: PropSpec -> PBTConfig -> FilePath -> Server -> IORef PBTPhase -> IO (Either PBTFailure PBTOutput)
runOne s config scratchBin srv entryNameRef = do
  let propName = psProp s
      genName = fromMaybe (error "missing generator") (psGen s)
      size = fromMaybe (configMaxSize config) (psSize s)
      serverSize = "runPBT_size"
      serverSeed = "runPBT_seed"
      serverIn = "runPBT_input"
      serverOk = "runPBT_ok"

      loop i
        | i >= configNumTests config = pure $ Right Nothing
        | otherwise = do
            randomValue <- randomIO :: IO Int32
            let runOneUpdatePhase phase = 
                  updatePhase (Just propName) phase Nothing (Just size) (Just seed) Nothing entryNameRef
                seed = fromMaybe randomValue (configSeed config)
            runOneUpdatePhase $ Just propName

            -- Setup and run Property
            genInputsRes <- sendGenInputs srv serverSize serverSeed genName size seed
            case (genInputsRes) of
              Just err -> pure $ Left err -- Two ways it can fail, ours would stop program, their will be reported back
              Nothing -> do
                runOneUpdatePhase $ Just genName
                genOuts <- generatorPhase genName serverSize serverSeed serverIn propName
                runOneUpdatePhase $ Just propName
                -- property phase
                ok <- withCallKeepIns srv propName [serverOk] [serverIn] $ \_ ->
                  getVal srv serverOk -- can only fail if we have messed up (get value in server is our responsibility)

                if ok
                  then loop (i + 1)
                  else withFreedVar srv serverIn $ do -- only fails if we have messed up
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
                    -- Shrinking phase
                    shrinkOutput <- case psShrink s of
                      Just "auto" -> do
                        autoShrinkLoop scratchBin srv propName genName serverIn size seed genOuts entryNameRef
                      Just shrinkName -> do
                        shrinkLoop scratchBin srv propName serverIn shrinkName entryNameRef
                      Nothing -> pure $ Right Nothing
                    
                    case shrinkOutput of
                      Left err -> pure $ Left $ failmsg <> "Shrink failed: " <> err
                      Right (Just err) -> pure $ Left $ failmsg <> "Shrink failed: " <> err
                      Right Nothing -> do
                        runOneUpdatePhase $ Just "prettyPrint"
                        counterLog <- pPrintPhase propName serverIn
                        pure $ Right $ Just $ failmsg <> counterLog
  loop 0
  where
    generatorPhase genName serverSize serverSeed serverIn propName = do
      genOuts <- allocOuts srv genName
      withFreedVars srv genOuts $ do
        _ <- callFreeIns srv genName genOuts [serverSize, serverSeed]
        _ <-
          freeOnException srv [serverIn] $
            useInputTypeToPack scratchBin srv serverIn propName genOuts
        pure genOuts

    pPrintPhase propName serverIn = do
      inputTypes <- getInputTypes srv propName
      case inputTypes of
        (ty0 : _) -> do
          prettyOut <- case psPPrint s of
            Just futPPrint -> do
              prettyOuts <- allocOuts srv futPPrint
              withFreedVars srv prettyOuts $ do
                _ <- callFreeIns srv futPPrint prettyOuts [serverIn]
                valE <- FSV.getValue srv (head prettyOuts)
                case valE of
                  Left err -> pure $ T.pack $ "getValue failed: " <> show err
                  Right (U8Value _ bytes) -> pure $ T.pack [chr (fromIntegral b) | b <- SV.toList bytes]
                  Right _ -> pure $ T.pack $ "pretty printer returned non-u8 value"
            Nothing -> do
              prettyOut <- prettyVar srv serverIn ty0
              pure $ T.pack prettyOut

          pure $ "Minimal counterexample: " <> prettyOut
        _ -> pure "Could not retrieve input types for counterexample log."

updatePhase :: Maybe EntryName -> Maybe EntryName -> Maybe EntryName -> Maybe Int64 -> Maybe Int32 -> Maybe Int32 -> IORef PBTPhase -> IO ()
updatePhase propName phase activeTest size seed tactic phaseRef =
  writeIORef phaseRef $ PBTPhase
      { activeTest = propName
      , phase = phase
      , shrinkWith = activeTest
      , phaseSize = size
      , phaseSeed = seed
      , phaseTactic = tactic
      }

autoShrinkLoop :: FilePath -> Server -> EntryName -> EntryName -> VarName -> Int64 -> Int32 -> [VarName] -> IORef PBTPhase -> IO (Either PBTFailure PBTOutput)
autoShrinkLoop scratchBin srv propName genName vIn size seed genOuts phaseRef = do
  let autoShrinkUpdatePhase activeTest = updatePhase (Just propName) (Just "autoShrinkLoop") activeTest (Just size) (Just seed) Nothing phaseRef

  let vCand = "qc_try"
      vOk = "qc_ok"
      serverSize = "qc_size"
      serverSeed = "qc_seed"

  propInTys <- getInputTypes srv propName
  propTyE <- pure $ case propInTys of
    [ty] -> Right ty
    [] -> Left $ "Property " <> T.unpack propName <> " has no inputs?" -- user error
    tys -> Left $ "Property " <> T.unpack propName <> " has >1 input: " <> show tys -- user error
  case propTyE of
    Left err -> pure . Left $ T.pack err -- the only failure left is user error not us
    Right propTy -> do
      let loop :: Int64 -> IO (Either PBTFailure PBTOutput)
          loop i
            | i <= 1 =
                pure $ Right Nothing
            | otherwise = do
                let newSize = i - 1

                genOutputs <- sendGenInputs srv serverSize serverSeed genName newSize seed
                case genOutputs of
                  Just err -> pure . Left $ T.pack $ "Failed to send inputs to generator in autoShrinkLoop: " <> T.unpack err -- only error left is user error, we already check generator inputs before starting loop
                  Nothing -> do
                    res <- withFreedVars srv (genOuts ++ [vCand]) $ do
                      -- withFreedVar srv vCand $ do
                        autoShrinkUpdatePhase $ Just genName

                        _ <- callFreeIns srv genName genOuts [serverSize, serverSeed]
                        _ <-
                          freeOnException srv [vCand] $
                            packType scratchBin srv vCand propTy genOuts -- can only fail if we messed up
                        autoShrinkUpdatePhase $ Just propName

                        ok <- withCallKeepIns srv propName [vOk] [vCand] $ \_ ->
                          getVal srv vOk
                        autoShrinkUpdatePhase Nothing

                        if ok
                        then pure Nothing
                        else do
                          _ <-
                            freeOnException srv [vIn] $
                              packType scratchBin srv vIn propTy [vCand]
                          pure $ Just newSize

                    case res of
                      Nothing -> loop (i - 1)
                      Just s -> loop s
      loop size

data Step
  = AcceptedSame -- overwrite vIn, keep tactic
  | AcceptedInc -- overwrite vIn, tactic++
  | PropPassed -- do not overwrite vIn, tactic++
  | StopShrinking -- stop, do not overwrite vIn
  | ErrorInShrink T.Text -- shrinker error (including property failure in shrinker)
  deriving (Eq, Show)

shrinkLoop :: FilePath -> Server -> EntryName -> VarName -> EntryName -> IORef PBTPhase-> IO (Either PBTFailure PBTOutput)
shrinkLoop scratchBin srv propName vIn shrinkName phaseRef = do  
  oldRef <- readIORef phaseRef
  size <- pure $ fromMaybe 0 (phaseSize oldRef)
  seed <- pure $ fromMaybe 0 (phaseSeed oldRef)
  let shrinkUpdatePhase activeTest = updatePhase (Just propName) (Just shrinkName) activeTest (Just size) (Just seed) Nothing phaseRef
  -- updatePhase shrinkName Nothing size seedø
  shrinkUpdatePhase Nothing

  propTyE <- getSingleInputType srv propName
  case propTyE of
    Left err -> pure . Left $ T.pack $ show err
    Right propTy -> do
      shrinkInTys <- getInputTypes srv shrinkName
      shrinkInE <- pure $ case shrinkInTys of
        [shrinkXTy, tacticTy] -> Right (shrinkXTy, tacticTy)
        []  -> Left $ "Shrinker " <> shrinkName <> " has no inputs?"
        [_] -> Left $ "Shrinker " <> shrinkName <> " has 1 input; expected 2 (x,tactic)."
        tys -> Left $ "Shrinker " <> shrinkName <> " has " <> T.pack (show (length tys)) <> " inputs; expected 2 (x,tactic): " <> T.pack (show tys)

      case shrinkInE of
        Left err -> pure $ Left err -- can only be user error
        Right (shrinkXTy, tacticTy) -> do
          case  (tacticTy /= "i32") of
              -- tactic is explicitly Int32 to resolve ambiguity
            True -> pure . Left $ T.pack $ "Shrinker " <> T.unpack shrinkName <> " tactic must be i32, got: " <> T.unpack tacticTy -- this is user error (tactic is wrong type)
            False -> do
              let loop (tactic :: Int32) = do
                    r <- oneStep shrinkXTy propTy size seed tactic
                    case r of
                      AcceptedSame -> loop 0
                      AcceptedInc -> loop (tactic + 1)
                      PropPassed -> loop (tactic + 1)
                      StopShrinking -> pure $ Right Nothing
                      ErrorInShrink err -> pure . Left $ T.pack $ "Error in shrink: " <> T.unpack err
              loop (0)
  where
    oneStep shrinkXTy propTy size seed tactic = do
      let vTry = "qc_try"
          vOk = "qc_ok"
          vTactic = "qc_tactic"
          vInRetyped = vIn <> "_shrinktyped"
      -- put tactic into server var (will be freed by callFreeIns below)
      freeVars srv [vTactic]
      putVal srv vTactic tactic

      -- ensure vIn is of shrinker’s expected x type
      _ <-
        freeOnException srv [vInRetyped] $
          packType scratchBin srv (vInRetyped) shrinkXTy [vIn]

      -- call shrinker
      shrinkOuts <- allocOuts srv shrinkName
      let shrinkUpdatePhase activeTest = updatePhase (Just propName) (Just shrinkName) activeTest (Just size) (Just seed) Nothing phaseRef

      shrinkUpdatePhase Nothing
      withFreedVars srv shrinkOuts $ do
        _ <- callFreeIns srv shrinkName shrinkOuts [vInRetyped, vTactic]

        -- Convention: last output is status:i8, the rest are y parts.
        let (yParts, statusVar) = (init shrinkOuts, last shrinkOuts)
        -- status must be i8
        statusTys <- getOutputTypes srv shrinkName
        let statusTy = last statusTys
        case (statusTy /= "i8") of
          True -> pure . ErrorInShrink $
            "Shrinker "
              <> shrinkName
              <> " last output must be i8 status, got: "
              <> statusTy
          False -> do 
            status <- (getVal srv statusVar :: IO Int8)
            -- Build vTry (candidate y) and make sure it is freed at the end of this step.
            withFreedVar srv vTry $ do
              _ <- freeOnException srv [vTry] $ packType scratchBin srv vTry propTy yParts

              -- Evaluate property on y (keep inputs alive; vTry freed by withFreedVar scope anyway)
              shrinkUpdatePhase Nothing
              ok <- withCallKeepIns srv propName [vOk] [vTry] $ \_ -> getVal srv vOk
              shrinkUpdatePhase Nothing

              if ok
              then
                pure $ if status == 2
                  then StopShrinking
                  -- property passed => ignore status; do not overwrite vIn; tactic++
                  else PropPassed
              else
                -- property failed => follow status
                case status of
                  0 ->
                    freeVars srv [vIn]
                      >> packType scratchBin srv vIn propTy [vTry]
                      >> pure AcceptedSame
                  1 ->
                    freeVars srv [vIn]
                      >> packType scratchBin srv vIn propTy [vTry]
                      >> pure AcceptedInc
                  -- stop; do not overwrite vIn (keep last failing)
                  2 -> pure StopShrinking
                  _ -> pure . ErrorInShrink $ T.pack $ "Shrinker " <> T.unpack shrinkName <> " returned invalid status: " <> show status

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

--- | Allocate output variables for a generator entry point.
-- needed because eg. tuples return multiple outputs, and we need to know how many to allocate. We name them like genName_out0, genName_out1, etc.
-- Calls cmdOutputs to find how many outputs the entrypoint has and allocates that many variables in the server, returning their names.
allocOuts :: Server -> EntryName -> IO [VarName]
allocOuts srv entry = do
  outsE <- cmdOutputs srv entry
  case outsE of
    Left err -> fail $ ("cmdOutputs failed for " <> T.unpack entry <> ": " <> show err) -- can only fail if something is very wrong, we already check if generator is an entry
    Right outs -> pure $ [entry <> "_out" <> T.pack (show i) | (i, _) <- zip [(0 :: Int) ..] outs]

isCompositeLike :: Server -> VarName -> TypeName -> IO Bool
isCompositeLike srv var ty = do
  -- 1. Check the 'kind' of the current type
  typeNameE <- cmdKind srv ty
  typeName <- either (\e -> fail ("kind command failed for " <> T.unpack var <> ": " <> show e)) pure typeNameE -- should only fail because we have messed up
  -- T.putStrLn $ "Checking type: " <> T.unpack ty <> " (Kind: " <> T.unpack currentKind <> ")"

  case typeName of
    Array -> do
      elemTyE <- cmdElemtype srv ty
      case elemTyE of
        Right et -> do
          -- T.putStrLn $ "  -> Found nested element type: " <> T.unpack et
          isCompositeLike srv var et
        Left e -> fail $ "elemtype command failed for " <> T.unpack ty <> ": " <> show e -- should only fail due to bad logic
    Record -> pure True
    _ -> pure False

putVal :: (PutValue1 a) => Server -> T.Text -> a -> IO ()
putVal s name x = do
  let v = putValue1 x
  mFail <- FSV.putValue s name v
  case mFail of
    Nothing -> pure ()
    Just err -> fail $ "putValue failed for " <> T.unpack name <> ": " <> show err

freeVars :: Server -> [VarName] -> IO ()
freeVars s vs = do
  mFail <- cmdFree s vs
  case mFail of
    Nothing -> pure ()
    Just err
      | any (T.isPrefixOf "Unknown variable:") (failureMsg err) -> pure ()
      | otherwise ->
          fail $ "cmdFree failed for " <> show vs <> ": " <> show (failureMsg err)

callFreeIns :: Server -> EntryName -> [VarName] -> [VarName] -> IO [VarName]
callFreeIns s entry outs ins = do
  freeVars s outs
  r <- cmdCall s entry outs ins
  freeVars s ins
  case r of
    Left err -> fail $ T.unpack entry <> ": " <> show (failureMsg err)
    Right okLines -> pure okLines

callKeepIns :: Server -> EntryName -> [VarName] -> [VarName] -> IO [VarName]
callKeepIns s entry outs ins = do
  freeVars s outs
  r <- cmdCall s entry outs ins
  case r of
    Left err -> fail $ T.unpack entry <> ": " <> show (failureMsg err)
    Right okLines -> pure okLines

freeOnException :: Server -> [VarName] -> IO a -> IO a
freeOnException srv vs action =
  action `onException` freeVars srv vs

withCallFreeIns :: Server -> EntryName -> [VarName] -> [VarName] -> ([VarName] -> IO a) -> IO a
withCallFreeIns srv entry outs ins k = do
  _ <- callFreeIns srv entry outs ins
  k outs `finally` freeVars srv outs

withCallKeepIns :: Server -> EntryName -> [VarName] -> [VarName] -> ([VarName] -> IO a) -> IO a
withCallKeepIns srv entry outs ins k = do
  _ <- callKeepIns srv entry outs ins
  k outs `finally` freeVars srv outs

withFreedVars :: Server -> [VarName] -> IO a -> IO a
withFreedVars srv vs action =
  action `finally` freeVars srv vs

-- | Bracket a single temporary var name.
withFreedVar :: Server -> VarName -> IO a -> IO a
withFreedVar srv v = withFreedVars srv [v]

packType :: FilePath -> Server -> VarName -> TypeName -> [VarName] -> IO (VarName)
packType scratchBin srv outVar typ componentVars = do
  mustRepack <- isCompositeLike srv outVar typ
  case mustRepack of
    True -> do
      -- store the (possibly split) representation
      m1 <- cmdStore srv scratchBin componentVars
      case m1 of
        Just err -> fail $ "cmdStore failed: " <> show err -- user can't do anyting here, clearly our fault
        Nothing -> do
          -- make destination reusable
          freeVars srv [outVar]
          -- restore as the requested type/name
          m2 <- cmdRestore srv scratchBin [(outVar, typ)]
          case m2 of
            Just err -> fail $ "cmdRestore failed: " <> show err -- user can't do anyting here, clearly our fault
            Nothing -> pure outVar
    False -> do
      -- already exactly one var, and not composite-like => just reuse it
      val <- getDataVal srv (head componentVars) -- sanity check: can we get the value?
      freeVars srv [outVar]
      _ <- FSV.putValue srv outVar val
      pure outVar

useInputTypeToPack :: FilePath -> Server -> VarName -> EntryName -> [VarName] -> IO (Either PBTFailure VarName)
useInputTypeToPack scratchBin srv var propName componentVars = do
  insPE <- cmdInputs srv propName
  case insPE of
    Left err -> pure $ Left $ T.pack $ "cmdInputs failed for " <> T.unpack propName <> ": " <> show err
    Right ins -> do
      case ins of
        [inp] -> Right <$> packType scratchBin srv var (inputType inp) componentVars
        [] -> pure . Left $ "Expected property " <> propName <> " to have exactly one input, got none."
        tys -> pure . Left $ "Expected property " <> propName <> " to have exactly one input, got: " <> (T.pack $ show (map inputType tys))

getVal :: (GetValue a) => Server -> VarName -> IO a
getVal s name = do
  v <- getDataVal s name
  case getValue v of
    Just b -> pure b
    -- expected type of a
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
          ftypes = map (fieldType) fieldLines
          isTuple =
            and
              [ fname == T.pack (show i)
              | (fname, i) <- zip fnames [0 .. (length fnames - 1)]
              ]

      rendered <- forM (zip fnames ftypes) $ \(fname, fty) -> do
        let tmp =
              if isTuple
                then v <> "_tup_" <> fname
                else v <> "_rec_" <> fname

        sField <- withFreedVar srv tmp $ do
          mFail <- cmdProject srv tmp v fname
          case mFail of
            Just err -> fail $ "cmdProject failed for field " <> T.unpack fname <> ": " <> show err
            Nothing -> pure ()

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

outsMatchType :: Server -> TypeName -> [TypeName] -> IO Bool
outsMatchType srv propTy outs = do
  if outs == [propTy]
    then pure True
    else do
      fieldsE <- cmdFields srv propTy
      case fieldsE of
        Left _ -> pure False
        Right fs ->
          let ftys = map fieldType fs
           in pure (outs == ftys)
