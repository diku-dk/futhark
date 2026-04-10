{-# LANGUAGE LambdaCase #-}

module Futhark.Test.Property
  (
    runPBT,
    isPropertyInputOutput,
    PBTConfig (..),
    PBTPhase (..),
    PropSpec (..),
    lookupArgRead,
    lookupArgText,
    stripCall
  )
where

import Control.Exception
import Control.Monad
import Data.Char (chr)
import Data.Int (Int32, Int64, Int8)
import Data.IORef
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Vector.Storable qualified as SV
import Futhark.Data
import Futhark.Server
import Futhark.Server.Values qualified as FSV
import Futhark.Test.Spec
import Futhark.Util (showText)
import System.Random (randomIO)
import System.IO.Temp (withSystemTempFile)

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

-- iroef = simple_gen, propname, seed, null, null
-- generator

-- iroef = propname, propname, seed, null, value
-- prop 

-- loop


-- Enten: 
-- iroef = shrink_simple, propname, seed, tactic, value
-- shrinker

-- iroef = shrink_simple: propname, propname, seed, tactic, value, shirnkprop true
-- prop

--Eller:
-- ioref = shrink_auto, propname, seed, tactic, value
-- generator

-- iroef = shrink_auto propname, propname, seed, tactic, value, shirnkprop true
-- prop


-- iroef = null, null, null, null, null


-- endloop
-- print

-- shrinker: Shrinkeren er gået galt eller propertien i shrinkeren er gået galt
-- auto_shrinker: generatoren er gået galt eller propertien i generatoen er gået galt

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

runPBT :: PBTConfig -> Server -> [PropSpec] -> IORef PBTPhase -> IO [Maybe T.Text]
runPBT config srv specs entryNameRef = do
  withSystemTempFile "pbt-scratch" $ \scratchBin _scratchHandle -> do
    epsE <- cmdEntryPoints srv
    entrypoints <- case epsE of
      Left err -> fail $ "cmdEntryPoints failed: " <> show err
      Right xs -> pure xs

    forM_ specs $ \s -> do
      let prop = psProp s
      unless (prop `elem` entrypoints) $
        fail $
          "Property is not a server entry point: " <> T.unpack prop
      validatePropTypes srv prop
      gen <- case psGen s of
        Just g | g `elem` entrypoints -> pure g
        Just g -> fail $ "Generator is not a server entry point: " <> T.unpack g
        Nothing -> fail $ "No generator specified for " <> T.unpack prop
      validateGenTypes srv prop gen
      case psShrink s of
        Just sh | sh `elem` entrypoints -> validateShrinkTypes srv prop sh
        Just "auto" -> pure ()
        Just sh -> fail $ "Shrinker is not a server entry point: " <> T.unpack sh
        Nothing -> pure ()

    -- 3. Execute tests and concatenate result lists
    results <- forM specs $ \s -> runOne s config scratchBin srv entryNameRef
    pure $ results

runOne :: PropSpec -> PBTConfig -> FilePath -> Server -> IORef PBTPhase -> IO (Maybe T.Text)
runOne s config scratchBin srv entryNameRef = do
  
  let propName = psProp s
      genName = fromMaybe (error "missing generator") (psGen s)
      sizeBase = fromMaybe (configMaxSize config) (psSize s)
      serverSize = "runPBT_size"
      serverSeed = "runPBT_seed"
      serverIn = "runPBT_input"
      serverOk = "runPBT_ok"

  -- write active test is propName to entryNameRef for logging purposes

  let loop i
        | i >= configNumTests config = pure $ Nothing
        | otherwise = do
            randomValue <- randomIO :: IO Int32
            let seed = case configSeed config of
                  Just s -> s
                  Nothing -> do
                    randomValue

            writeIORef entryNameRef $ PBTPhase
              { activeTest = Just propName
              , phase = Nothing
              , shrinkWith = Nothing
              , phaseSize = Just sizeBase
              , phaseSeed = Just seed
              , phaseTactic = Nothing
              }

            -- Setup and run Property
            sendGenInputs srv serverSize serverSeed genName sizeBase seed
            genOuts <- allocOuts srv genName

            writeIORef entryNameRef $ PBTPhase
              { activeTest = Just propName
              , phase = Just genName
              , shrinkWith = Nothing
              , phaseSize = Just sizeBase
              , phaseSeed = Just seed
              , phaseTactic = Nothing
              }

            withFreedVars srv genOuts $ do
              _ <- callFreeIns srv genName genOuts [serverSize, serverSeed]
              _ <-
                freeOnException srv [serverIn] $
                  useInputTypeToPack scratchBin srv serverIn propName genOuts
              pure ()

            writeIORef entryNameRef $ PBTPhase
              { activeTest = Just propName
              , phase = Just propName
              , shrinkWith = Nothing
              , phaseSize = Just sizeBase
              , phaseSeed = Just seed
              , phaseTactic = Nothing
              }

            ok <- withCallKeepIns srv propName [serverOk] [serverIn] $ \_ ->
              getVal srv serverOk

            if ok
              then loop (i + 1)
              else withFreedVar srv serverIn $ do
                let failmsg =
                      "PBT FAIL: "
                        <> propName
                        <> " size="
                        <> showText sizeBase
                        <> " seed="
                        <> showText seed
                        <> " after "
                        <> showText i
                        <> " tests\n"

                -- Collect Shrinking Logs
                case psShrink s of
                  Just "auto" -> do
                    writeIORef entryNameRef $ PBTPhase
                      { activeTest = Just propName
                      , phase = Just "autoShrinkLoop"
                      , shrinkWith = Just genName
                      , phaseSize = Just sizeBase
                      , phaseSeed = Just seed
                      , phaseTactic = Nothing
                      }
                    autoShrinkLoop scratchBin srv propName genName serverIn sizeBase seed genOuts entryNameRef
                  Just shrinkName -> do
                    writeIORef entryNameRef $ PBTPhase
                      { activeTest = Just propName
                      , phase = Just shrinkName
                      , shrinkWith = Nothing
                      , phaseSize = Just sizeBase
                      , phaseSeed = Just seed
                      , phaseTactic = Nothing
                      }
                    shrinkLoop scratchBin srv propName serverIn shrinkName entryNameRef
                  Nothing -> pure ()

                -- Collect Counterexample Log
                inputTypesE <- getInputTypes srv propName

                -- pretty pritner
                writeIORef entryNameRef $ PBTPhase
                  { activeTest = Just propName
                  , phase = Just "prettyPrint"
                  , shrinkWith = Nothing
                  , phaseSize = Nothing
                  , phaseSeed = Nothing
                  , phaseTactic = Nothing
                  }

                counterLog <- case inputTypesE of
                  Right (ty0 : _) -> do
                    prettyOut <- case psPPrint s of
                      Just futPPrint -> do
                        prettyOuts <- allocOuts srv futPPrint
                        withFreedVars srv prettyOuts $ do
                          _ <- callFreeIns srv futPPrint prettyOuts [serverIn]
                          valE <- FSV.getValue srv (head prettyOuts)
                          case valE of
                            Left err -> fail $ "getValue failed: " <> show err
                            Right (U8Value _ bytes) -> pure [chr (fromIntegral b) | b <- SV.toList bytes]
                            Right _ -> fail "pretty printer returned non-u8 value"
                      Nothing -> prettyVar srv serverIn ty0

                    pure $ "Minimal counterexample: " <> T.pack prettyOut
                  _ -> pure "Could not retrieve input types for counterexample log."

                pure $ Just $ failmsg <> counterLog
  loop 0

validatePropTypes :: Server -> EntryName -> IO ()
validatePropTypes srv propName = do
  -- Exactly one input
  insE <- getInputTypes srv propName
  ins <- either (fail . show) pure insE
  case ins of
    [_] -> pure ()
    [] -> fail $ "Property " <> T.unpack propName <> " has no inputs? Expected 1."
    tys -> fail $ "Property " <> T.unpack propName <> " has " <> show (length tys) <> " inputs; expected 1: " <> show tys

  -- Exactly one output, and it must be bool
  outsE <- getOutputTypes srv propName
  outs <- either (fail . show) pure outsE
  case outs of
    ["bool"] -> pure ()
    [] -> fail $ "Property " <> T.unpack propName <> " has no outputs? Expected 1 bool."
    [ty] -> fail $ "Property " <> T.unpack propName <> " output must be bool, got: " <> T.unpack ty
    tys -> fail $ "Property " <> T.unpack propName <> " has " <> show (length tys) <> " outputs; expected 1 bool: " <> show tys

getInputTypes :: Server -> EntryName -> IO (Either CmdFailure [TypeName])
getInputTypes srv entry = do
  r <- cmdInputs srv entry
  pure $ fmap (map inputType) r

getOutputTypes :: Server -> EntryName -> IO (Either CmdFailure [TypeName])
getOutputTypes s entry = do
  outs <- cmdOutputs s entry
  pure $ fmap (map outputType) outs

autoShrinkLoop :: FilePath -> Server -> EntryName -> EntryName -> VarName -> Int64 -> Int32 -> [VarName] -> IORef PBTPhase -> IO ()
autoShrinkLoop scratchBin srv propName genName vIn size seed genOuts phaseRef = do
  let vCand = "qc_try"
      vOk = "qc_ok"
      serverSize = "qc_size"
      serverSeed = "qc_seed"

  -- Property must have exactly one input
  propInTys <- either (fail . show) pure =<< getInputTypes srv propName
  propTy <- case propInTys of
    [ty] -> pure ty
    [] -> fail $ "Property " <> T.unpack propName <> " has no inputs?"
    tys -> fail $ "Property " <> T.unpack propName <> " has >1 input: " <> show tys

  let loop :: Int64 -> IO ()
      loop i
        | i <= 1 =
            pure ()
        | otherwise = do
            let newSize = i - 1

            sendGenInputs srv serverSize serverSeed genName newSize seed

            res <- withFreedVars srv genOuts $
              withFreedVar srv vCand $ do

                writeIORef phaseRef $ PBTPhase
                  { activeTest = Just propName
                  , phase = Just "autoShrinkLoop"
                  , shrinkWith = Just genName
                  , phaseSize = Just newSize
                  , phaseSeed = Just seed
                  , phaseTactic = Nothing
                  }

                _ <- callFreeIns srv genName genOuts [serverSize, serverSeed]
                _ <-
                  freeOnException srv [vCand] $
                    packType scratchBin srv vCand propTy genOuts

                writeIORef phaseRef $ PBTPhase
                  { activeTest = Just propName
                  , phase = Just "autoShrinkLoop"
                  , shrinkWith = Just propName
                  , phaseSize = Just newSize
                  , phaseSeed = Just seed
                  , phaseTactic = Nothing
                  }

                ok <- withCallKeepIns srv propName [vOk] [vCand] $ \_ ->
                  getVal srv vOk

                writeIORef phaseRef $ PBTPhase
                  { activeTest = Just propName
                  , phase = Just "autoShrinkLoop"
                  , shrinkWith = Nothing
                  , phaseSize = Just newSize
                  , phaseSeed = Just seed
                  , phaseTactic = Nothing
                  }

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
  deriving (Eq, Show)

shrinkLoop :: FilePath -> Server -> EntryName -> VarName -> EntryName -> IORef PBTPhase-> IO ()
shrinkLoop scratchBin srv propName vIn shrinkName phaseRef = do
  let vTry = "qc_try"
      vOk = "qc_ok"
      vTactic = "qc_tactic"
      vInRetyped = vIn <> "_shrinktyped"
  
  oldRef <- readIORef phaseRef
  size <- pure $ fromMaybe 0 (phaseSize oldRef)
  seed <- pure $ fromMaybe 0 (phaseSeed oldRef)

  writeIORef phaseRef $ PBTPhase
              { activeTest = Just propName
              , phase = Just shrinkName
              , shrinkWith = Nothing
              , phaseSize = Just size
              , phaseSeed = Just seed
              , phaseTactic = Nothing
              }

  propTy <- getSingleInputType srv propName
  shrinkInTys <- either (fail . show) pure =<< getInputTypes srv shrinkName
  -- (shrinkXTy, _) <- case shrinkInTys of
  --   [tyX, "i32"] -> pure (tyX, "i32")
  --   tys -> fail $ "Shrinker " <> T.unpack shrinkName <> " expected (x, i32), got: " <> show tys
  (shrinkXTy, tacticTy) <- case shrinkInTys of
    [tyX, tyTac] -> pure (tyX, tyTac)
    [] -> fail $ "Shrinker " <> T.unpack shrinkName <> " has no inputs?"
    [_] -> fail $ "Shrinker " <> T.unpack shrinkName <> " has 1 input; expected 2 (x,tactic)."
    tys -> fail $ "Shrinker " <> T.unpack shrinkName <> " has " <> show (length tys) <> " inputs; expected 2."

  when (tacticTy /= "i32") $
    fail $
      "Shrinker " <> T.unpack shrinkName <> " tactic must be i32, got: " <> T.unpack tacticTy
  -- tactic is explicitly Int32 to resolve ambiguity

  let oneStep :: Int32 -> IO Step
      oneStep tactic = do
        -- put tactic into server var (will be freed by callFreeIns below)
        freeVars srv [vTactic]
        putVal srv vTactic tactic

        -- ensure vIn is of shrinker’s expected x type
        _ <-
          freeOnException srv [vInRetyped] $
            packType scratchBin srv (vInRetyped) shrinkXTy [vIn]

        -- call shrinker
        shrinkOuts <- allocOuts srv shrinkName
        
        writeIORef phaseRef $ PBTPhase
              { activeTest = Just propName
              , phase = Just shrinkName
              , shrinkWith = Nothing
              , phaseSize = Just size
              , phaseSeed = Just seed
              , phaseTactic = Nothing
              }
        withFreedVars srv shrinkOuts $ do
          _ <- callFreeIns srv shrinkName shrinkOuts [vInRetyped, vTactic]

          -- Convention: last output is status:i8, the rest are y parts.
          -- let (yParts, statusVar) = (init shrinkOuts, last shrinkOuts)
          let (yParts, statusVar) =
                case reverse shrinkOuts of
                  (s : rs) -> (reverse rs, s)
                  [] -> error "impossible: shrinkOuts checked non-empty"

          -- status must be i8
          statusTyE <- getOutputTypes srv shrinkName
          statusTys <- either (fail . show) pure statusTyE
          let statusTy = last statusTys
          when (statusTy /= "i8") $
            fail $
              "Shrinker "
                <> T.unpack shrinkName
                <> " last output must be i8 status, got: "
                <> T.unpack statusTy

          status <- (getVal srv statusVar :: IO Int8)

          -- Build vTry (candidate y) and make sure it is freed at the end of this step.
          withFreedVar srv vTry $ do
            _ <- freeOnException srv [vTry] $ packType scratchBin srv vTry propTy yParts

            -- Evaluate property on y (keep inputs alive; vTry freed by withFreedVar scope anyway)
            writeIORef phaseRef $ PBTPhase
              { activeTest = Just propName
              , phase = Just shrinkName
              , shrinkWith = Just propName
              , phaseSize = Just size
              , phaseSeed = Just seed
              , phaseTactic = Nothing
              }
            ok <- withCallKeepIns srv propName [vOk] [vTry] $ \_ -> getVal srv vOk
            writeIORef phaseRef $ PBTPhase
              { activeTest = Just propName
              , phase = Just shrinkName
              , shrinkWith = Nothing
              , phaseSize = Just size
              , phaseSeed = Just seed
              , phaseTactic = Nothing
              }

            if ok
              then
                pure $
                  if status == 2
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
                  _ -> fail $ "Shrinker " <> T.unpack shrinkName <> " returned invalid status: " <> show status

  let loop (tactic :: Int32) = do
        r <- oneStep tactic
        case r of
          AcceptedSame -> loop 0
          AcceptedInc -> loop (tactic + 1)
          PropPassed -> loop (tactic + 1)
          StopShrinking -> pure ()

  loop (0)

validateGenTypes :: Server -> EntryName -> EntryName -> IO ()
validateGenTypes srv propName genName = do
  propTy <- getSingleInputType srv propName

  genOutsE <- getOutputTypes srv genName
  genOuts <- either (fail . show) pure genOutsE
  ok <- outsMatchType srv propTy genOuts
  unless ok $
    fail $
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

validateShrinkTypes :: Server -> EntryName -> EntryName -> IO ()
validateShrinkTypes srv propName shrinkName = do
  propTy <- getSingleInputType srv propName

  -- Inputs: (x, tactic)
  shrinkInsE <- getInputTypes srv shrinkName
  shrinkIns <- either (fail . show) pure shrinkInsE
  case shrinkIns of
    [xTy, tacTy] -> do
      unless (xTy == propTy) $
        fail $
          "Shrinker input mismatch.\n"
            <> "  property "
            <> T.unpack propName
            <> " expects: "
            <> T.unpack propTy
            <> "\n"
            <> "  shrinker  "
            <> T.unpack shrinkName
            <> " takes x: "
            <> T.unpack xTy
            <> "\n"
            <> "Expected shrinker's first input to be exactly the property input type."
      unless (tacTy == "i32") $
        fail $
          "Shrinker tactic type mismatch.\n"
            <> "  shrinker "
            <> T.unpack shrinkName
            <> " takes tactic: "
            <> T.unpack tacTy
            <> "\n"
            <> "Expected tactic to be i32."
    _ ->
      fail $
        "Shrinker input arity mismatch.\n"
          <> "  shrinker "
          <> T.unpack shrinkName
          <> " inputs: "
          <> show shrinkIns
          <> "\n"
          <> "Expected exactly 2 inputs: ("
          <> T.unpack propTy
          <> ", i32)."

  -- Outputs: (y, done)
  shrinkOutsE <- getOutputTypes srv shrinkName
  shrinkOuts <- either (fail . show) pure shrinkOutsE
  when (null shrinkOuts) $
    fail $
      "Shrinker " <> T.unpack shrinkName <> " has no outputs? Expected (testType, bool)."

  let doneTy = last shrinkOuts
      yOuts = init shrinkOuts

  unless (doneTy == "i8") $
    fail $
      "Shrinker output mismatch.\n"
        <> "  shrinker "
        <> T.unpack shrinkName
        <> " last output type: "
        <> T.unpack doneTy
        <> "\n"
        <> "Expected last output to be bool (done flag)."

  ok <- outsMatchType srv propTy yOuts
  unless ok $
    fail $
      "Shrinker output mismatch.\n"
        <> "  property "
        <> T.unpack propName
        <> " expects: "
        <> T.unpack propTy
        <> "\n"
        <> "  shrinker  "
        <> T.unpack shrinkName
        <> " returns y parts: "
        <> show yOuts
        <> " and done: bool\n"
        <> "Expected shrinker's y outputs to equal the property input type (possibly split for tuples/records)."

sendGenInputs :: Server -> VarName -> VarName -> VarName -> Int64 -> Int32 -> IO ()
sendGenInputs srv sizeName seedName genName size seed = do
  insE <- cmdInputs srv genName
  ins <- either (\err -> fail ("cmdInputs failed for " <> T.unpack genName <> ": " <> show err)) pure insE

  case map inputType ins of
    [sizeTy, seedTy]
      | sizeTy == "i64" && seedTy == "i32" -> do
          putVal srv sizeName size
          putVal srv seedName seed
      | otherwise ->
          fail ("Expected both size and seed to be i64 and i32, got: " <> T.unpack sizeTy <> ", " <> T.unpack seedTy)
    tys ->
      fail ("Expected generator to have exactly two inputs, got types: " <> show tys)

--- | Allocate output variables for a generator entry point.
-- needed because eg. tuples return multiple outputs, and we need to know how many to allocate. We name them like genName_out0, genName_out1, etc.
-- Calls cmdOutputs to find how many outputs the entrypoint has and allocates that many variables in the server, returning their names.
allocOuts :: Server -> VarName -> IO [VarName]
allocOuts srv entry = do
  outsE <- cmdOutputs srv entry
  outs <- either (\e -> fail ("cmdOutputs failed for " <> T.unpack entry <> ": " <> show e)) pure outsE
  pure [entry <> "_out" <> T.pack (show i) | (i, _) <- zip [(0 :: Int) ..] outs]

isCompositeLike :: Server -> VarName -> TypeName -> IO Bool
isCompositeLike srv var ty = do
  -- 1. Check the 'kind' of the current type
  typeNameE <- cmdKind srv ty
  typeName <- either (\e -> fail ("kind command failed for " <> T.unpack var <> ": " <> show e)) pure typeNameE

  -- T.putStrLn $ "Checking type: " <> T.unpack ty <> " (Kind: " <> T.unpack currentKind <> ")"

  case typeName of
    Array -> do
      elemTyE <- cmdElemtype srv ty
      case elemTyE of
        Right et -> do
          -- T.putStrLn $ "  -> Found nested element type: " <> T.unpack et
          isCompositeLike srv var et
        Left e -> fail ("elemtype command failed for " <> T.unpack ty <> ": " <> show e)
    Record -> pure True
    _ -> pure False

putVal :: (PutValue1 a) => Server -> T.Text -> a -> IO ()
putVal s name x = do
  let v = putValue1 x
  mFail <- FSV.putValue s name v
  case mFail of
    Nothing -> pure ()
    Just err -> fail $ "putValue failed for " <> T.unpack name <> ": " <> show err

freeVars :: Server -> [T.Text] -> IO ()
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

packType :: FilePath -> Server -> VarName -> TypeName -> [VarName] -> IO VarName
packType scratchBin srv outVar typ componentVars = do
  compLike <- isCompositeLike srv outVar typ
  -- must repack if type is composite like
  let mustRepack = compLike

  -- T.putStrLn $ "Packing type " <> T.unpack typ <> " from components " <> show componentVars <> " with mustRepack = " <> show mustRepack <> " (composite-like: " <> show comp <> ", array of composite-like: " <> show isArrComp <> ")"

  if mustRepack
    then do
      -- store the (possibly split) representation
      m1 <- cmdStore srv scratchBin componentVars
      case m1 of
        Just err -> fail ("cmdStore failed: " <> show err)
        Nothing -> pure ()

      -- make destination reusable
      freeVars srv [outVar]

      -- restore as the requested type/name
      m2 <- cmdRestore srv scratchBin [(outVar, typ)]
      case m2 of
        Just err -> fail ("cmdRestore failed: " <> show err)
        Nothing -> pure ()

      pure outVar
    else do
      -- already exactly one var, and not composite-like => just reuse it
      val <- getDataVal srv (head componentVars) -- sanity check: can we get the value?
      freeVars srv [outVar]
      _ <- FSV.putValue srv outVar val
      pure outVar

useInputTypeToPack :: FilePath -> Server -> VarName -> EntryName -> [VarName] -> IO VarName
useInputTypeToPack scratchBin srv var propName componentVars = do
  insP <- either (fail . show) pure =<< cmdInputs srv propName
  inputType <- case insP of
    [inp] -> pure (inputType inp) -- this is the important fix
    [] -> fail $ "Expected property " <> T.unpack propName <> " to have exactly one input, got none."
    tys -> fail $ "Expected property " <> T.unpack propName <> " to have exactly one input, got: " <> show (map inputType tys)

  packType scratchBin srv var inputType componentVars

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

getSingleInputType :: Server -> EntryName -> IO TypeName
getSingleInputType srv ep = do
  tysE <- getInputTypes srv ep
  tys <- either (fail . show) pure tysE
  case tys of
    [ty] -> pure ty
    [] -> fail $ "Entrypoint " <> T.unpack ep <> " has no inputs (expected 1)."
    _ -> fail $ "Entrypoint " <> T.unpack ep <> " has >1 input (expected 1): " <> show tys

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


-- Helper fuctions for checking property comments and attributes match

isPropertyPlaceholderRun :: TestRun -> Bool
isPropertyPlaceholderRun (TestRun _ (GenValues []) (Succeeds Nothing) _ _) = True
isPropertyPlaceholderRun _ = False

isPropertyInputOutput :: InputOutputs -> Bool
isPropertyInputOutput io =
  any isPropertyPlaceholderRun (iosTestRuns io)
