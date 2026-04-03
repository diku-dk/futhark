{-# LANGUAGE LambdaCase #-}

-- | @futhark test@
module Futhark.CLI.Test (main) where

import Control.Applicative.Lift (Errors, Lift (..), failure, runErrors)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.Except (ExceptT (..), MonadError, runExceptT, withExceptT)
import Control.Monad.Except qualified as E
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Data.ByteString qualified as SBS
import Data.ByteString.Lazy qualified as LBS
import Data.Char (chr)
import Data.Int (Int32, Int64, Int8)
import Data.List (delete, intercalate, partition)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Time.Clock.System (SystemTime (..), getSystemTime)
import Data.Vector.Storable qualified as SV
import Futhark.Analysis.Metrics.Type
import Futhark.Data
import Futhark.Server
import Futhark.Server.Values qualified as FSV
import Futhark.Test
import Futhark.Util (atMostChars, fancyTerminal, showText)
import Futhark.Util.Options
import Futhark.Util.Pretty (annotate, bgColor, bold, hardline, pretty, putDoc, vsep)
import Futhark.Util.Table
import GHC.Stack (HasCallStack)
import Language.LSP.Protocol.Lens (HasChange (change))
import System.Console.ANSI (clearFromCursorToScreenEnd, clearLine, cursorUpLine)
import System.Console.Terminal.Size qualified as Terminal
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp (withSystemTempFile)
import System.Process.ByteString (readProcessWithExitCode)
import Text.Regex.TDFA

--- Test execution

-- The use of [T.Text] here is somewhat kludgy. We use it to track how
-- many errors have occurred during testing of a single program (which
-- may have multiple entry points). This should really not be done at
-- the monadic level - a test failing should be handled explicitly.
type TestM = ExceptT [T.Text] IO

extractPropSpecsFromServer :: Server -> IO [PropSpec]
extractPropSpecsFromServer srv = do
  epsE <- cmdEntryPoints srv
  eps <- either (error . show) pure epsE
  concat <$> mapM getOne eps
  where
    getOne entry = do
      attrsE <- cmdAttributes srv entry
      attrs <- either (error . show) pure attrsE
      pure $ mapMaybe (parsePropSpec entry) attrs

parsePropSpec :: T.Text -> T.Text -> Maybe PropSpec
parsePropSpec entry attr = do
  argsText <- stripCall "prop" attr
  let args = map T.strip $ T.splitOn "," argsText
  pure
    PropSpec
      { psProp = entry,
        psGen = lookupArgText "gen" args,
        psShrink = lookupArgText "shrink" args,
        psSize = fmap fromInteger $ lookupArgRead "size" args,
        psSeed = fmap fromInteger $ lookupArgRead "seed" args,
        psPPrint = lookupArgText "pprint" args
      }

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

-- Taken from transformers-0.5.5.0.
eitherToErrors :: Either e a -> Errors e a
eitherToErrors = either failure Pure

throwError :: (MonadError [e] m) => e -> m a
throwError e = E.throwError [e]

runTestM :: TestM () -> IO TestResult
runTestM = fmap (either Failure $ const Success) . runExceptT

liftExcept :: ExceptT T.Text IO a -> TestM a
liftExcept = either (E.throwError . pure) pure <=< liftIO . runExceptT

context :: T.Text -> TestM a -> TestM a
context s = withExceptT $
  \case
    [] -> []
    (e : es') -> (s <> ":\n" <> e) : es'

context1 :: (Monad m) => T.Text -> ExceptT T.Text m a -> ExceptT T.Text m a
context1 s = withExceptT $ \e -> s <> ":\n" <> e

accErrors :: [TestM a] -> TestM [a]
accErrors tests = do
  eithers <- lift $ mapM runExceptT tests
  let errors = traverse eitherToErrors eithers
  ExceptT $ pure $ runErrors errors

accErrors_ :: [TestM a] -> TestM ()
accErrors_ = void . accErrors

data TestResult
  = Success
  | Failure [T.Text]
  deriving (Eq, Show)

pureTestResults :: IO [TestResult] -> TestM ()
pureTestResults m = do
  errs <- foldr collectErrors mempty <$> liftIO m
  unless (null errs) $ E.throwError $ concat errs
  where
    collectErrors Success errs = errs
    collectErrors (Failure err) errs = err : errs

-- | The longest we are willing to wait for a test, in microseconds.
timeout :: Int
timeout = 5 * 60 * 1000000

withProgramServer :: FilePath -> FilePath -> [String] -> (Server -> IO [TestResult]) -> TestM ()
withProgramServer program runner extra_options f = do
  -- Explicitly prefixing the current directory is necessary for
  -- readProcessWithExitCode to find the binary when binOutputf has
  -- no path component.
  let binOutputf = dropExtension program
      binpath = "." </> binOutputf

      (to_run, to_run_args)
        | null runner = (binpath, extra_options)
        | otherwise = (runner, binpath : extra_options)

      prog_ctx =
        "Running " <> T.pack (unwords $ binpath : extra_options)

  context prog_ctx . pureTestResults . liftIO $
    withServer (futharkServerCfg to_run to_run_args) $ \server ->
      race (threadDelay timeout) (f server) >>= \case
        Left _ -> do
          abortServer server
          fail $ "test timeout after " <> show timeout <> " microseconds"
        Right r -> pure r

data TestMode
  = -- | Only type check.
    TypeCheck
  | -- | Only compile (do not run).
    Compile
  | -- | Only internalise (do not run).
    Internalise
  | -- | Test compiled code.
    Compiled
  | -- | Test interpreted code.
    Interpreted
  | -- | Perform structure tests.
    Structure
  deriving (Eq, Show)

data TestCase = TestCase
  { _testCaseMode :: TestMode,
    testCaseProgram :: FilePath,
    testCaseTest :: ProgramTest,
    _testCasePrograms :: ProgConfig,
    pbtConfig :: PBTConfig
  }
  deriving (Show)

instance Eq TestCase where
  x == y = testCaseProgram x == testCaseProgram y

instance Ord TestCase where
  x `compare` y = testCaseProgram x `compare` testCaseProgram y

data RunResult
  = ErrorResult T.Text
  | SuccessResult [Value]

progNotFound :: T.Text -> T.Text
progNotFound s = s <> ": command not found"

optimisedProgramMetrics :: ProgConfig -> StructurePipeline -> FilePath -> TestM AstMetrics
optimisedProgramMetrics programs pipeline program =
  case pipeline of
    SOACSPipeline ->
      check ["-s"]
    GpuPipeline ->
      check ["--gpu"]
    MCPipeline ->
      check ["--mc"]
    SeqMemPipeline ->
      check ["--seq-mem"]
    GpuMemPipeline ->
      check ["--gpu-mem"]
    MCMemPipeline ->
      check ["--mc-mem"]
    NoPipeline ->
      check []
  where
    check opt = do
      futhark <- liftIO $ maybe getExecutablePath pure $ configFuthark programs
      let opts = ["dev"] ++ opt ++ ["--metrics", program]
      (code, output, err) <- liftIO $ readProcessWithExitCode futhark opts ""
      let output' = T.decodeUtf8 output
      case code of
        ExitSuccess
          | [(m, [])] <- reads $ T.unpack output' -> pure m
          | otherwise -> throwError $ "Could not read metrics output:\n" <> output'
        ExitFailure 127 -> throwError $ progNotFound $ T.pack futhark
        ExitFailure _ -> throwError $ T.decodeUtf8 err

testMetrics :: ProgConfig -> FilePath -> StructureTest -> TestM ()
testMetrics programs program (StructureTest pipeline (AstMetrics expected)) =
  context "Checking metrics" $ do
    actual <- optimisedProgramMetrics programs pipeline program
    accErrors_ $ map (ok actual) $ M.toList expected
  where
    maybePipeline :: StructurePipeline -> T.Text
    maybePipeline SOACSPipeline = "(soacs) "
    maybePipeline GpuPipeline = "(gpu) "
    maybePipeline MCPipeline = "(mc) "
    maybePipeline SeqMemPipeline = "(seq-mem) "
    maybePipeline GpuMemPipeline = "(gpu-mem) "
    maybePipeline MCMemPipeline = "(mc-mem) "
    maybePipeline NoPipeline = " "

    ok (AstMetrics metrics) (name, expected_occurences) =
      case M.lookup name metrics of
        Nothing
          | expected_occurences > 0 ->
              throwError $
                name
                  <> maybePipeline pipeline
                  <> "should have occurred "
                  <> showText expected_occurences
                  <> " times, but did not occur at all in optimised program."
        Just actual_occurences
          | expected_occurences /= actual_occurences ->
              throwError $
                name
                  <> maybePipeline pipeline
                  <> "should have occurred "
                  <> showText expected_occurences
                  <> " times, but occurred "
                  <> showText actual_occurences
                  <> " times."
        _ -> pure ()

testWarnings :: [WarningTest] -> SBS.ByteString -> TestM ()
testWarnings warnings futerr = accErrors_ $ map testWarning warnings
  where
    testWarning (ExpectedWarning regex_s regex)
      | not (match regex $ T.unpack $ T.decodeUtf8 futerr) =
          throwError $
            "Expected warning:\n  "
              <> regex_s
              <> "\nGot warnings:\n  "
              <> T.decodeUtf8 futerr
      | otherwise = pure ()

runInterpretedEntry :: FutharkExe -> FilePath -> InputOutputs -> TestM ()
runInterpretedEntry (FutharkExe futhark) program (InputOutputs entry run_cases) =
  let dir = takeDirectory program
      runInterpretedCase run@(TestRun _ inputValues _ index _) =
        unless (any (`elem` runTags run) ["compiled", "script"]) $
          context ("Entry point: " <> entry <> "; dataset: " <> runDescription run) $ do
            input <- T.unlines . map valueText <$> getValues (FutharkExe futhark) dir inputValues
            expectedResult' <- getExpectedResult (FutharkExe futhark) program entry run
            (code, output, err) <-
              liftIO $
                readProcessWithExitCode futhark ["run", "-e", T.unpack entry, program] $
                  T.encodeUtf8 input
            case code of
              ExitFailure 127 ->
                throwError $ progNotFound $ T.pack futhark
              _ ->
                liftExcept $
                  compareResult entry index program expectedResult'
                    =<< runResult program code output err
   in accErrors_ $ map runInterpretedCase run_cases

runTestCase :: TestCase -> TestM ()
runTestCase (TestCase mode program testcase progs pbtConfig) = do
  futhark <- liftIO $ maybe getExecutablePath pure $ configFuthark progs
  let checkctx =
        mconcat
          [ "Type-checking with '",
            T.pack futhark,
            " check ",
            T.pack program,
            "'"
          ]
  case testAction testcase of
    CompileTimeFailure expected_error ->
      unless (mode `elem` [Structure, Internalise]) . context checkctx $ do
        (code, _, err) <-
          liftIO $ readProcessWithExitCode futhark ["check", program] ""
        case code of
          ExitSuccess -> throwError "Expected failure\n"
          ExitFailure 127 -> throwError $ progNotFound $ T.pack futhark
          ExitFailure 1 -> throwError $ T.decodeUtf8 err
          ExitFailure _ -> liftExcept $ checkError expected_error $ T.decodeUtf8 err
    RunCases {}
      | mode == TypeCheck -> do
          let options = ["check", program] ++ configExtraCompilerOptions progs
          context checkctx $ do
            (code, _, err) <- liftIO $ readProcessWithExitCode futhark options ""
            case code of
              ExitSuccess -> pure ()
              ExitFailure 127 -> throwError $ progNotFound $ T.pack futhark
              ExitFailure _ -> throwError $ T.decodeUtf8 err
      | mode == Internalise -> do
          let options = ["dev", program] ++ configExtraCompilerOptions progs
          context checkctx $ do
            (code, _, err) <- liftIO $ readProcessWithExitCode futhark options ""
            case code of
              ExitSuccess -> pure ()
              ExitFailure 127 -> throwError $ progNotFound $ T.pack futhark
              ExitFailure _ -> throwError $ T.decodeUtf8 err
    RunCases ios structures warnings -> do
      -- Compile up-front and reuse same executable for several entry points.
      let backend = configBackend progs
          extra_compiler_options = configExtraCompilerOptions progs

      when (mode `elem` [Compiled, Interpreted]) $
        context "Generating reference outputs" $
          -- We probably get the concurrency at the test program level,
          -- so force just one data set at a time here.
          withExceptT pure $
            ensureReferenceOutput (Just 1) (FutharkExe futhark) "c" program ios

      when (mode == Structure) $
        mapM_ (testMetrics progs program) structures

      when (mode `elem` [Compile, Compiled]) $
        context ("Compiling with --backend=" <> T.pack backend) $ do
          compileTestProgram extra_compiler_options (FutharkExe futhark) backend program warnings
          unless (mode == Compile) $ do
            (tuning_opts, _) <-
              liftIO $ determineTuning (configTuning progs) program
            let extra_options =
                  determineCache (configCacheExt progs) program
                    ++ tuning_opts
                    ++ configExtraOptions progs
                runner = configRunner progs
            context "Running compiled program" $
              withProgramServer program runner extra_options $ \server -> do
                let run = runCompiledEntry (FutharkExe futhark) server program
                propSpecs <- extractPropSpecsFromServer server

                let (propIOs, normalIOs) = partition isPropertyInputOutput ios
                normal_test_result <- concat <$> mapM run normalIOs

                let requestedNames = requestedPropertyNames propIOs
                let diagnostics = propertyDiagnostics requestedNames propSpecs

                if null diagnostics
                  then do
                    let verifiedProps = filter (\p -> psProp p `elem` requestedNames) propSpecs
                    propResults <- runPBT pbtConfig server verifiedProps
                    pure $ normal_test_result ++ propResults ++ diagnostics
                  else
                    pure $ normal_test_result ++ diagnostics

      when (mode == Interpreted) $
        context "Interpreting" $
          accErrors_ $
            map (runInterpretedEntry (FutharkExe futhark) program) ios

liftCommand ::
  (MonadError T.Text m, MonadIO m) =>
  IO (Maybe CmdFailure) ->
  m ()
liftCommand m = do
  r <- liftIO m
  case r of
    Just (CmdFailure _ err) -> E.throwError $ T.unlines err
    Nothing -> pure ()

runCompiledEntry :: FutharkExe -> Server -> FilePath -> InputOutputs -> IO [TestResult]
runCompiledEntry futhark server program (InputOutputs entry run_cases) = do
  output_types <- cmdOutputs server entry
  input_types <- cmdInputs server entry
  case (,) <$> output_types <*> input_types of
    Left (CmdFailure _ err) ->
      pure [Failure err]
    Right (output_types', input_types') -> do
      let outs = ["out" <> showText i | i <- [0 .. length output_types' - 1]]
          ins = ["in" <> showText i | i <- [0 .. length input_types' - 1]]
          onRes = either (Failure . pure) (const Success)
      mapM (fmap onRes . runCompiledCase input_types' outs ins) run_cases
  where
    dir = takeDirectory program

    runCompiledCase input_types outs ins run = runExceptT $ do
      let TestRun _ input_spec _ index _ = run
          case_ctx =
            "Entry point: "
              <> entry
              <> "; dataset: "
              <> runDescription run

      context1 case_ctx $ do
        expected <- getExpectedResult futhark program entry run

        valuesAsVars server (zip ins (map inputType input_types)) futhark dir input_spec

        call_r <- liftIO $ cmdCall server entry outs ins
        liftCommand $ cmdFree server ins

        res <- case call_r of
          Left (CmdFailure _ err) ->
            pure $ ErrorResult $ T.unlines err
          Right _ ->
            SuccessResult
              <$> readResults server outs
              <* liftCommand (cmdFree server outs)

        compareResult entry index program expected res

checkError :: (MonadError T.Text m) => ExpectedError -> T.Text -> m ()
checkError (ThisError regex_s regex) err
  | not (match regex $ T.unpack err) =
      E.throwError $
        "Expected error:\n  "
          <> regex_s
          <> "\nGot error:\n"
          <> T.unlines (map ("  " <>) (T.lines err))
checkError _ _ =
  pure ()

runResult ::
  (MonadIO m, MonadError T.Text m) =>
  FilePath ->
  ExitCode ->
  SBS.ByteString ->
  SBS.ByteString ->
  m RunResult
runResult program ExitSuccess stdout_s _ =
  case valuesFromByteString "stdout" $ LBS.fromStrict stdout_s of
    Left e -> do
      let actualf = program `addExtension` "actual"
      liftIO $ SBS.writeFile actualf stdout_s
      E.throwError $ T.pack e <> "\n(See " <> T.pack actualf <> ")"
    Right vs -> pure $ SuccessResult vs
runResult _ (ExitFailure _) _ stderr_s =
  pure $ ErrorResult $ T.decodeUtf8 stderr_s

compileTestProgram :: [String] -> FutharkExe -> String -> FilePath -> [WarningTest] -> TestM ()
compileTestProgram extra_options futhark backend program warnings = do
  (_, futerr) <-
    withExceptT pure $
      compileProgram ("--server" : extra_options) futhark backend program
  testWarnings warnings futerr

compareResult ::
  (MonadIO m, MonadError T.Text m) =>
  T.Text ->
  Int ->
  FilePath ->
  ExpectedResult [Value] ->
  RunResult ->
  m ()
compareResult _ _ _ (Succeeds Nothing) SuccessResult {} =
  pure ()
compareResult entry index program (Succeeds (Just expected_vs)) (SuccessResult actual_vs) =
  checkResult
    (program <.> T.unpack entry <.> show index)
    expected_vs
    actual_vs
compareResult _ _ _ (RunTimeFailure expectedError) (ErrorResult actualError) =
  checkError expectedError actualError
compareResult _ _ _ (Succeeds _) (ErrorResult err) =
  E.throwError $ "Function failed with error:\n" <> err
compareResult _ _ _ (RunTimeFailure f) (SuccessResult _) =
  E.throwError $ "Program succeeded, but expected failure:\n  " <> showText f

---
--- Test manager
---

data TestStatus = TestStatus
  { testStatusRemain :: [TestCase],
    testStatusRun :: [TestCase],
    testStatusTotal :: Int,
    testStatusFail :: Int,
    testStatusPass :: Int,
    testStatusRuns :: Int,
    testStatusRunsRemain :: Int,
    testStatusRunPass :: Int,
    testStatusRunFail :: Int
  }

catching :: IO TestResult -> IO TestResult
catching m = m `catch` save
  where
    save :: SomeException -> IO TestResult
    save e = pure $ Failure [showText e]

doTest :: TestCase -> IO TestResult
doTest = catching . runTestM . runTestCase

makeTestCase :: TestConfig -> TestMode -> (FilePath, ProgramTest) -> TestCase
makeTestCase config mode (file, spec) =
  excludeCases config $ TestCase mode file spec (configPrograms config) pbtcfg
  where
    pbtcfg =
      PBTConfig
        { configNumTests = configNumTests $ configPBTConfig config,
          configMaxSize = configMaxSize $ configPBTConfig config,
          configBaseSeed = configBaseSeed $ configPBTConfig config
        }

data ReportMsg
  = TestStarted TestCase
  | TestDone TestCase TestResult

runTest :: MVar TestCase -> MVar ReportMsg -> IO ()
runTest testmvar resmvar = forever $ do
  test <- takeMVar testmvar
  putMVar resmvar $ TestStarted test
  res <- doTest test
  putMVar resmvar $ TestDone test res

excludedTest :: TestConfig -> TestCase -> Bool
excludedTest config =
  any (`elem` configExclude config) . testTags . testCaseTest

-- | Exclude those test cases that have tags we do not wish to run or
-- exclude based on entry point name.
excludeCases :: TestConfig -> TestCase -> TestCase
excludeCases config tcase =
  tcase {testCaseTest = onTest $ testCaseTest tcase}
  where
    onTest (ProgramTest desc tags action) =
      ProgramTest desc tags $ onAction action
    onAction (RunCases ios stest wtest) =
      RunCases (map onIOs $ filter relevantEntry ios) stest wtest
    onAction action = action
    onIOs (InputOutputs entry runs) =
      InputOutputs entry $ filter (not . any excluded . runTags) runs
    relevantEntry (InputOutputs entry _) =
      maybe True (== T.unpack entry) (configEntryPoint config)
    excluded = (`elem` configExclude config)

putStatusTable :: TestStatus -> IO ()
putStatusTable ts = hPutTable stdout rows 1
  where
    rows =
      [ [mkEntry "" mempty, passed, failed, mkEntry "remaining" mempty],
        map (`mkEntry` mempty) ["programs", passedProgs, failedProgs, remainProgs'],
        map (`mkEntry` mempty) ["runs", passedRuns, failedRuns, remainRuns']
      ]
    passed = mkEntry "passed" $ color Green
    failed = mkEntry "failed" $ color Red
    passedProgs = show $ testStatusPass ts
    failedProgs = show $ testStatusFail ts
    totalProgs = show $ testStatusTotal ts
    totalRuns = show $ testStatusRuns ts
    passedRuns = show $ testStatusRunPass ts
    failedRuns = show $ testStatusRunFail ts
    remainProgs = show . length $ testStatusRemain ts
    remainProgs' = remainProgs ++ "/" ++ totalProgs
    remainRuns = show $ testStatusRunsRemain ts
    remainRuns' = remainRuns ++ "/" ++ totalRuns

tableLines :: Int
tableLines = 8

spaceTable :: IO ()
spaceTable = putStr $ replicate tableLines '\n'

reportTable :: TestStatus -> IO ()
reportTable ts = do
  moveCursorToTableTop
  putStatusTable ts
  clearLine
  w <- maybe 80 Terminal.width <$> Terminal.size
  T.putStrLn $ atMostChars (w - T.length labelstr) running
  where
    running = labelstr <> (T.unwords . reverse . map (T.pack . testCaseProgram) . testStatusRun) ts
    labelstr = "Now testing: "

reportLine :: MVar SystemTime -> TestStatus -> IO ()
reportLine time_mvar ts =
  modifyMVar_ time_mvar $ \time -> do
    time_now <- getSystemTime
    if systemSeconds time_now - systemSeconds time >= period
      then do
        T.putStrLn $
          showText (testStatusFail ts)
            <> " failed, "
            <> showText (testStatusPass ts)
            <> " passed, "
            <> showText num_remain
            <> " to go."
        pure time_now
      else pure time
  where
    num_remain = length $ testStatusRemain ts
    period = 60

moveCursorToTableTop :: IO ()
moveCursorToTableTop = cursorUpLine tableLines

runTests :: TestConfig -> [FilePath] -> IO ()
runTests config paths = do
  -- We force line buffering to ensure that we produce running output.
  -- Otherwise, CI tools and the like may believe we are hung and kill
  -- us.
  hSetBuffering stdout LineBuffering

  let mode = configTestMode config
  all_tests <-
    map (makeTestCase config mode)
      <$> testSpecsFromPathsOrDie paths
  -- putStrLn $ "Test cases are: " ++ show (map testCaseProgram all_tests)
  testmvar <- newEmptyMVar
  reportmvar <- newEmptyMVar
  concurrency <- maybe getNumCapabilities pure $ configConcurrency config
  replicateM_ concurrency $ forkIO $ runTest testmvar reportmvar

  let (excluded, included) = partition (excludedTest config) all_tests
  _ <- forkIO $ mapM_ (putMVar testmvar . excludeCases config) included

  time_mvar <- newMVar $ MkSystemTime 0 0

  let fancy = not (configLineOutput config) && fancyTerminal

      report
        | fancy = reportTable
        | otherwise = reportLine time_mvar
      clear
        | fancy = clearFromCursorToScreenEnd
        | otherwise = pure ()

      numTestCases tc =
        case testAction $ testCaseTest tc of
          CompileTimeFailure _ -> 1
          RunCases ios sts wts ->
            length (concatMap iosTestRuns ios)
              + length sts
              + length wts
      getResults ts
        | null (testStatusRemain ts) = report ts >> pure ts
        | otherwise = do
            report ts
            msg <- takeMVar reportmvar
            case msg of
              TestStarted test ->
                getResults $ ts {testStatusRun = test : testStatusRun ts}
              TestDone test res -> do
                let ts' =
                      ts
                        { testStatusRemain = test `delete` testStatusRemain ts,
                          testStatusRun = test `delete` testStatusRun ts,
                          testStatusRunsRemain =
                            testStatusRunsRemain ts - numTestCases test
                        }
                case res of
                  Success -> do
                    let ts'' =
                          ts'
                            { testStatusRunPass =
                                testStatusRunPass ts' + numTestCases test
                            }
                    getResults $ ts'' {testStatusPass = testStatusPass ts + 1}
                  Failure s -> do
                    when fancy moveCursorToTableTop
                    clear
                    putDoc $
                      annotate (bold <> bgColor Red) (pretty (testCaseProgram test) <> ":")
                        <> hardline
                        <> vsep (map pretty s)
                        <> hardline
                    when fancy spaceTable
                    getResults $
                      ts'
                        { testStatusFail = testStatusFail ts' + 1,
                          testStatusRunPass =
                            testStatusRunPass ts'
                              + max 0 (numTestCases test - length s),
                          testStatusRunFail =
                            testStatusRunFail ts'
                              + min (numTestCases test) (length s)
                        }
  when fancy spaceTable

  ts <-
    getResults
      TestStatus
        { testStatusRemain = included,
          testStatusRun = [],
          testStatusTotal = length included,
          testStatusFail = 0,
          testStatusPass = 0,
          testStatusRuns = sum $ map numTestCases included,
          testStatusRunsRemain = sum $ map numTestCases included,
          testStatusRunPass = 0,
          testStatusRunFail = 0
        }

  -- Removes "Now testing" output.
  if fancy
    then cursorUpLine 1 >> clearLine
    else
      putStrLn $
        show (testStatusPass ts)
          <> "/"
          <> show (testStatusTotal ts)
          <> " passed."

  unless (null excluded) . putStrLn $
    show (length excluded) ++ " program(s) excluded."

  exitWith $ case testStatusFail ts of
    0 -> ExitSuccess
    _ -> ExitFailure 1

---
--- Configuration and command line parsing
---

data PropSpec = PropSpec
  { psProp :: T.Text,
    psGen :: Maybe T.Text,
    psShrink :: Maybe T.Text,
    psSize :: Maybe Int64,
    psSeed :: Maybe Int32,
    psPPrint :: Maybe T.Text
  }
  deriving (Show, Eq)

data PBTConfig = PBTConfig
  { configNumTests :: Int32,
    configMaxSize :: Int64,
    configBaseSeed :: Int32
  }
  deriving (Show, Eq)

data TestConfig = TestConfig
  { configTestMode :: TestMode,
    configPrograms :: ProgConfig,
    configExclude :: [T.Text],
    configLineOutput :: Bool,
    configConcurrency :: Maybe Int,
    configEntryPoint :: Maybe String,
    configPBTConfig :: PBTConfig
  }

defaultConfig :: TestConfig
defaultConfig =
  TestConfig
    { configTestMode = Compiled,
      configExclude = ["disable", "notest"],
      configPrograms =
        ProgConfig
          { configBackend = "c",
            configFuthark = Nothing,
            configRunner = "",
            configExtraOptions = [],
            configExtraCompilerOptions = [],
            configTuning = Just "tuning",
            configCacheExt = Nothing
          },
      configLineOutput = False,
      configConcurrency = Nothing,
      configEntryPoint = Nothing,
      configPBTConfig =
        PBTConfig
          { configNumTests = 100,
            configMaxSize = 50,
            configBaseSeed = 123456
          }
    }

data ProgConfig = ProgConfig
  { configBackend :: String,
    configFuthark :: Maybe FilePath,
    configRunner :: FilePath,
    configExtraCompilerOptions :: [String],
    configTuning :: Maybe String,
    configCacheExt :: Maybe String,
    -- | Extra options passed to the programs being run.
    configExtraOptions :: [String]
  }
  deriving (Show)

changeProgConfig :: (ProgConfig -> ProgConfig) -> TestConfig -> TestConfig
changeProgConfig f config = config {configPrograms = f $ configPrograms config}

changePBTConfig :: (PBTConfig -> PBTConfig) -> TestConfig -> TestConfig
changePBTConfig f config = config {configPBTConfig = f $ configPBTConfig config}

setBackend :: FilePath -> ProgConfig -> ProgConfig
setBackend backend config =
  config {configBackend = backend}

setFuthark :: FilePath -> ProgConfig -> ProgConfig
setFuthark futhark config =
  config {configFuthark = Just futhark}

setRunner :: FilePath -> ProgConfig -> ProgConfig
setRunner runner config =
  config {configRunner = runner}

addCompilerOption :: String -> ProgConfig -> ProgConfig
addCompilerOption option config =
  config {configExtraCompilerOptions = configExtraCompilerOptions config ++ [option]}

addOption :: String -> ProgConfig -> ProgConfig
addOption option config =
  config {configExtraOptions = configExtraOptions config ++ [option]}

commandLineOptions :: [FunOptDescr TestConfig]
commandLineOptions =
  [ Option
      "t"
      ["typecheck"]
      (NoArg $ Right $ \config -> config {configTestMode = TypeCheck})
      "Only perform type-checking",
    Option
      "i"
      ["interpreted"]
      (NoArg $ Right $ \config -> config {configTestMode = Interpreted})
      "Only interpret",
    Option
      "c"
      ["compiled"]
      (NoArg $ Right $ \config -> config {configTestMode = Compiled})
      "Only run compiled code (the default)",
    Option
      "C"
      ["compile"]
      (NoArg $ Right $ \config -> config {configTestMode = Compile})
      "Only compile, do not run.",
    Option
      "s"
      ["structure"]
      (NoArg $ Right $ \config -> config {configTestMode = Structure})
      "Perform structure tests.",
    Option
      "I"
      ["internalise"]
      (NoArg $ Right $ \config -> config {configTestMode = Internalise})
      "Only run the compiler frontend.",
    Option
      []
      ["no-terminal", "notty"]
      (NoArg $ Right $ \config -> config {configLineOutput = True})
      "Provide simpler line-based output.",
    Option
      []
      ["backend"]
      (ReqArg (Right . changeProgConfig . setBackend) "BACKEND")
      "Backend used for compilation (defaults to 'c').",
    Option
      []
      ["futhark"]
      (ReqArg (Right . changeProgConfig . setFuthark) "PROGRAM")
      "Program to run for subcommands (defaults to same binary as 'futhark test').",
    Option
      []
      ["runner"]
      (ReqArg (Right . changeProgConfig . setRunner) "PROGRAM")
      "The program used to run the Futhark-generated programs (defaults to nothing).",
    Option
      []
      ["exclude"]
      ( ReqArg
          ( \tag ->
              Right $ \config ->
                config {configExclude = T.pack tag : configExclude config}
          )
          "TAG"
      )
      "Exclude test programs that define this tag.",
    Option
      "p"
      ["pass-option"]
      (ReqArg (Right . changeProgConfig . addOption) "OPT")
      "Pass this option to programs being run.",
    Option
      []
      ["pass-compiler-option"]
      (ReqArg (Right . changeProgConfig . addCompilerOption) "OPT")
      "Pass this option to the compiler (or typechecker if in -t mode).",
    Option
      []
      ["tuning"]
      ( ReqArg
          (\s -> Right $ changeProgConfig $ \config -> config {configTuning = Just s})
          "EXTENSION"
      )
      "Look for tuning files with this extension (defaults to .tuning).",
    Option
      []
      ["no-tuning"]
      (NoArg $ Right $ changeProgConfig $ \config -> config {configTuning = Nothing})
      "Do not load tuning files.",
    Option
      []
      ["cache-extension"]
      ( ReqArg
          (\s -> Right $ changeProgConfig $ \config -> config {configCacheExt = Just s})
          "EXTENSION"
      )
      "Use cache files with this extension (none by default).",
    Option
      []
      ["concurrency"]
      ( ReqArg
          ( \n ->
              case reads n of
                [(n', "")]
                  | n' > 0 ->
                      Right $ \config -> config {configConcurrency = Just n'}
                _ ->
                  Left . optionsError $ "'" ++ n ++ "' is not a positive integer."
          )
          "NUM"
      )
      "Number of tests to run concurrently.",
    Option
      "e"
      ["entry-point"]
      ( ReqArg
          ( \s -> Right $ \config ->
              config {configEntryPoint = Just s}
          )
          "NAME"
      )
      "Only run entry points with this name.",
    Option
      "n"
      ["num-tests"]
      ( ReqArg
          ( \n ->
              case reads n of
                [(n', "")]
                  | n' >= 0 ->
                      Right $ changePBTConfig $ \pbt -> pbt {configNumTests = n'}
                _ ->
                  Left . optionsError $ "'" ++ n ++ "' is not a non-negative integer."
          )
          "NUM"
      )
      "Number of tests to run per property (default: 100).",
    Option
      "m"
      ["max-size"]
      ( ReqArg
          ( \n ->
              case reads n of
                [(n', "")]
                  | n' >= 0 ->
                      Right $ changePBTConfig $ \pbt -> pbt {configMaxSize = n'}
                _ ->
                  Left . optionsError $ "'" ++ n ++ "' is not a non-negative integer."
          )
          "NUM"
      )
      "Maximum size parameter to use for generators (default: 50).",
    Option
      "base-seed"
      ["base-seed"]
      ( ReqArg
          ( \n ->
              case (reads n :: [(Integer, String)]) of
                [(n', "")]
                  | n' >= 0 ->
                      Right $ changePBTConfig $ \pbt -> pbt {configBaseSeed = fromIntegral n'}
                _ ->
                  Left . optionsError $ "'" ++ n ++ "' is not a non-negative integer."
          )
          "NUM"
      )
      ( "Base seed to use for generators (default: "
          ++ show (configBaseSeed $ configPBTConfig defaultConfig)
          ++ "). The seed for test #i will be base-seed + i."
      )
  ]

excludeBackend :: TestConfig -> TestConfig
excludeBackend config =
  config
    { configExclude =
        "no_" <> T.pack (configBackend (configPrograms config))
          : configExclude config
    }

-- | Run @futhark test@.
main :: String -> [String] -> IO ()
main = mainWithOptions defaultConfig commandLineOptions "options... programs..." $ \progs config ->
  case progs of
    [] -> Nothing
    _ -> Just $ runTests (excludeBackend config) progs

runPBT :: PBTConfig -> Server -> [PropSpec] -> IO [TestResult]
runPBT config srv specs = do
  withSystemTempFile "pbt-scratch" $ \scratchBin _scratchHandle -> do
    epsE <- cmdEntryPoints srv
    entrypoints <- case epsE of
      Left err -> error $ "cmdEntryPoints failed: " <> show err
      Right xs -> pure xs

    forM_ specs $ \s -> do
      let prop = psProp s
      unless (prop `elem` entrypoints) $
        error $
          "Property is not a server entry point: " <> T.unpack prop
      validatePropTypes srv prop
      gen <- case psGen s of
        Just g | g `elem` entrypoints -> pure g
        Just g -> error $ "Generator is not a server entry point: " <> T.unpack g
        Nothing -> error $ "No generator specified for " <> T.unpack prop
      validateGenTypes srv prop gen
      case psShrink s of
        Just sh | sh `elem` entrypoints -> validateShrinkTypes srv prop sh
        Just "auto" -> pure ()
        Just sh -> error $ "Shrinker is not a server entry point: " <> T.unpack sh
        Nothing -> pure ()

    -- 3. Execute tests and concatenate result lists
    results <- forM specs $ \s -> runOne s config scratchBin srv
    pure $ results

runOne :: PropSpec -> PBTConfig -> FilePath -> Server -> IO TestResult
runOne s config scratchBin srv = do
  let propName = psProp s
      genName = fromMaybe (error "missing generator") (psGen s)
      sizeBase = fromMaybe (configMaxSize config) (psSize s)
      seedBase = fromMaybe (configBaseSeed config) (psSeed s)
      serverSize = "runPBT_size"
      serverSeed = "runPBT_seed"
      serverIn = "runPBT_input"
      serverOk = "runPBT_ok"

  let loop i
        | i >= configNumTests config = pure $ Success
        | otherwise = do
            let seed = seedBase + i

            -- Setup and run Property
            sendGenInputs srv serverSize serverSeed genName sizeBase seed
            genOuts <- allocOuts srv genName
            withFreedVars srv genOuts $ do
              _ <- callFreeIns srv genName genOuts [serverSize, serverSeed]
              _ <-
                freeOnException srv [serverIn] $
                  useInputTypeToPack scratchBin srv serverIn propName genOuts
              pure ()

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
                  Just "auto" -> autoShrinkLoop scratchBin srv propName genName serverIn sizeBase seed genOuts
                  Just shrinkName -> shrinkLoop scratchBin srv propName serverIn shrinkName
                  Nothing -> pure ()

                -- Collect Counterexample Log
                inputTypesE <- getInputTypes srv propName
                counterLog <- case inputTypesE of
                  Right (ty0 : _) -> do
                    prettyOut <- case psPPrint s of
                      Just futPPrint -> do
                        prettyOuts <- allocOuts srv futPPrint
                        withFreedVars srv prettyOuts $ do
                          _ <- callFreeIns srv futPPrint prettyOuts [serverIn]
                          valE <- FSV.getValue srv (head prettyOuts)
                          case valE of
                            Left err -> error $ "getValue failed: " <> show err
                            Right (U8Value _ bytes) -> pure [chr (fromIntegral b) | b <- SV.toList bytes]
                            Right _ -> error "pretty printer returned non-u8 value"
                      Nothing -> prettyVar srv serverIn ty0

                    pure $ "Minimal counterexample: " <> T.pack prettyOut
                  _ -> pure "Could not retrieve input types for counterexample log."

                pure $ Failure [failmsg <> counterLog]
  loop 0

validatePropTypes :: Server -> EntryName -> IO ()
validatePropTypes srv propName = do
  -- Exactly one input
  insE <- getInputTypes srv propName
  ins <- either (error . show) pure insE
  case ins of
    [_] -> pure ()
    [] -> error $ "Property " <> T.unpack propName <> " has no inputs? Expected 1."
    tys -> error $ "Property " <> T.unpack propName <> " has " <> show (length tys) <> " inputs; expected 1: " <> show tys

  -- Exactly one output, and it must be bool
  outsE <- getOutputTypes srv propName
  outs <- either (error . show) pure outsE
  case outs of
    ["bool"] -> pure ()
    [] -> error $ "Property " <> T.unpack propName <> " has no outputs? Expected 1 bool."
    [ty] -> error $ "Property " <> T.unpack propName <> " output must be bool, got: " <> T.unpack ty
    tys -> error $ "Property " <> T.unpack propName <> " has " <> show (length tys) <> " outputs; expected 1 bool: " <> show tys

getInputTypes :: Server -> EntryName -> IO (Either CmdFailure [TypeName])
getInputTypes srv entry = do
  r <- cmdInputs srv entry
  pure $ fmap (map inputType) r

getOutputTypes :: Server -> EntryName -> IO (Either CmdFailure [TypeName])
getOutputTypes s entry = do
  outs <- cmdOutputs s entry
  pure $ fmap (map outputType) outs

autoShrinkLoop :: FilePath -> Server -> EntryName -> EntryName -> VarName -> Int64 -> Int32 -> [VarName] -> IO ()
autoShrinkLoop scratchBin srv propName genName vIn size seed genOuts = do
  let vCand = "qc_try"
      vOk = "qc_ok"
      serverSize = "qc_size"
      serverSeed = "qc_seed"

  -- Property must have exactly one input
  propInTys <- either (error . show) pure =<< getInputTypes srv propName
  propTy <- case propInTys of
    [ty] -> pure ty
    [] -> error $ "Property " <> T.unpack propName <> " has no inputs?"
    tys -> error $ "Property " <> T.unpack propName <> " has >1 input: " <> show tys

  let loop :: Int64 -> IO ()
      loop i
        | i <= 1 =
            pure ()
        | otherwise = do
            let newSize = i - 1

            sendGenInputs srv serverSize serverSeed genName newSize seed

            res <- withFreedVars srv genOuts $
              withFreedVar srv vCand $ do
                _ <- callFreeIns srv genName genOuts [serverSize, serverSeed]
                _ <-
                  freeOnException srv [vCand] $
                    packType scratchBin srv vCand propTy genOuts

                ok <- withCallKeepIns srv propName [vOk] [vCand] $ \_ ->
                  getVal srv vOk

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

shrinkLoop :: FilePath -> Server -> EntryName -> VarName -> EntryName -> IO ()
shrinkLoop scratchBin srv propName vIn shrinkName = do
  let vTry = "qc_try"
      vOk = "qc_ok"
      vTactic = "qc_tactic"
      vInRetyped = vIn <> "_shrinktyped"

  propTy <- getSingleInputType srv propName
  shrinkInTys <- either (error . show) pure =<< getInputTypes srv shrinkName
  -- (shrinkXTy, _) <- case shrinkInTys of
  --   [tyX, "i32"] -> pure (tyX, "i32")
  --   tys -> error $ "Shrinker " <> T.unpack shrinkName <> " expected (x, i32), got: " <> show tys
  (shrinkXTy, tacticTy) <- case shrinkInTys of
    [tyX, tyTac] -> pure (tyX, tyTac)
    [] -> error $ "Shrinker " <> T.unpack shrinkName <> " has no inputs?"
    [_] -> error $ "Shrinker " <> T.unpack shrinkName <> " has 1 input; expected 2 (x,tactic)."
    tys -> error $ "Shrinker " <> T.unpack shrinkName <> " has " <> show (length tys) <> " inputs; expected 2."

  when (tacticTy /= "i32") $
    error $
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
          statusTys <- either (error . show) pure statusTyE
          let statusTy = last statusTys
          when (statusTy /= "i8") $
            error $
              "Shrinker "
                <> T.unpack shrinkName
                <> " last output must be i8 status, got: "
                <> T.unpack statusTy

          status <- (getVal srv statusVar :: IO Int8)

          -- Build vTry (candidate y) and make sure it is freed at the end of this step.
          withFreedVar srv vTry $ do
            _ <- freeOnException srv [vTry] $ packType scratchBin srv vTry propTy yParts

            -- Evaluate property on y (keep inputs alive; vTry freed by withFreedVar scope anyway)
            ok <- withCallKeepIns srv propName [vOk] [vTry] $ \_ -> getVal srv vOk

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
                  _ -> pure StopShrinking

  let loop (tactic :: Int32) = do
        r <- oneStep tactic
        case r of
          AcceptedSame -> loop 0
          AcceptedInc -> pure ()
          PropPassed -> loop (tactic + 1)
          StopShrinking -> pure ()

  loop (0)

validateGenTypes :: Server -> EntryName -> EntryName -> IO ()
validateGenTypes srv propName genName = do
  propTy <- getSingleInputType srv propName

  genOutsE <- getOutputTypes srv genName
  genOuts <- either (error . show) pure genOutsE
  ok <- outsMatchType srv propTy genOuts
  unless ok $
    error $
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
  shrinkIns <- either (error . show) pure shrinkInsE
  case shrinkIns of
    [xTy, tacTy] -> do
      unless (xTy == propTy) $
        error $
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
        error $
          "Shrinker tactic type mismatch.\n"
            <> "  shrinker "
            <> T.unpack shrinkName
            <> " takes tactic: "
            <> T.unpack tacTy
            <> "\n"
            <> "Expected tactic to be i32."
    _ ->
      error $
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
  shrinkOuts <- either (error . show) pure shrinkOutsE
  when (null shrinkOuts) $
    error $
      "Shrinker " <> T.unpack shrinkName <> " has no outputs? Expected (testType, bool)."

  let doneTy = last shrinkOuts
      yOuts = init shrinkOuts

  unless (doneTy == "i8") $
    error $
      "Shrinker output mismatch.\n"
        <> "  shrinker "
        <> T.unpack shrinkName
        <> " last output type: "
        <> T.unpack doneTy
        <> "\n"
        <> "Expected last output to be bool (done flag)."

  ok <- outsMatchType srv propTy yOuts
  unless ok $
    error $
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
  ins <- either (\err -> error ("cmdInputs failed for " <> T.unpack genName <> ": " <> show err)) pure insE

  case map inputType ins of
    [sizeTy, seedTy]
      | sizeTy == "i64" && seedTy == "i32" -> do
          putVal srv sizeName size
          putVal srv seedName seed
      | otherwise ->
          error ("Expected both size and seed to be i64 and i32, got: " <> T.unpack sizeTy <> ", " <> T.unpack seedTy)
    tys ->
      error ("Expected generator to have exactly two inputs, got types: " <> show tys)

--- | Allocate output variables for a generator entry point.
-- needed because eg. tuples return multiple outputs, and we need to know how many to allocate. We name them like genName_out0, genName_out1, etc.
-- Calls cmdOutputs to find how many outputs the entrypoint has and allocates that many variables in the server, returning their names.
allocOuts :: Server -> VarName -> IO [VarName]
allocOuts srv entry = do
  outsE <- cmdOutputs srv entry
  outs <- either (\e -> error ("cmdOutputs failed for " <> T.unpack entry <> ": " <> show e)) pure outsE
  pure [entry <> "_out" <> T.pack (show i) | (i, _) <- zip [(0 :: Int) ..] outs]

isCompositeLike :: Server -> VarName -> TypeName -> IO Bool
isCompositeLike srv var ty = do
  -- 1. Check the 'kind' of the current type
  typeNameE <- cmdKind srv ty
  typeName <- either (\e -> error ("kind command failed for " <> T.unpack var <> ": " <> show e)) pure typeNameE

  -- T.putStrLn $ "Checking type: " <> T.unpack ty <> " (Kind: " <> T.unpack currentKind <> ")"

  case typeName of
    Array -> do
      elemTyE <- cmdElemtype srv ty
      case elemTyE of
        Right et -> do
          -- T.putStrLn $ "  -> Found nested element type: " <> T.unpack et
          isCompositeLike srv var et
        Left e -> error ("elemtype command failed for " <> T.unpack ty <> ": " <> show e)
    Record -> pure True
    _ -> pure False

putVal :: (PutValue1 a) => Server -> T.Text -> a -> IO ()
putVal s name x = do
  let v = putValue1 x
  mFail <- FSV.putValue s name v
  case mFail of
    Nothing -> pure ()
    Just err -> error $ "putValue failed for " <> T.unpack name <> ": " <> show err

freeVars :: Server -> [T.Text] -> IO ()
freeVars s vs = do
  mFail <- cmdFree s vs
  case mFail of
    Nothing -> pure ()
    Just err
      | any (T.isPrefixOf "Unknown variable:") (failureMsg err) -> pure ()
      | otherwise ->
          error $ "cmdFree failed for " <> show vs <> ": " <> show err

callFreeIns :: (HasCallStack) => Server -> EntryName -> [VarName] -> [VarName] -> IO [VarName]
callFreeIns s entry outs ins = do
  freeVars s outs
  r <- cmdCall s entry outs ins
  freeVars s ins
  case r of
    Left err -> error $ "cmdCall failed for " <> T.unpack entry <> ": " <> show err
    Right okLines -> pure okLines

callKeepIns :: (HasCallStack) => Server -> EntryName -> [VarName] -> [VarName] -> IO [VarName]
callKeepIns s entry outs ins = do
  freeVars s outs
  r <- cmdCall s entry outs ins
  case r of
    Left err -> error $ "cmdCall failed for " <> T.unpack entry <> ": " <> show err
    Right okLines -> pure okLines

freeOnException :: Server -> [VarName] -> IO a -> IO a
freeOnException srv vs action =
  action `onException` freeVars srv vs

withCallFreeIns :: (HasCallStack) => Server -> EntryName -> [VarName] -> [VarName] -> ([VarName] -> IO a) -> IO a
withCallFreeIns srv entry outs ins k = do
  _ <- callFreeIns srv entry outs ins
  k outs `finally` freeVars srv outs

withCallKeepIns :: (HasCallStack) => Server -> EntryName -> [VarName] -> [VarName] -> ([VarName] -> IO a) -> IO a
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
        Just err -> error ("cmdStore failed: " <> show err)
        Nothing -> pure ()

      -- make destination reusable
      freeVars srv [outVar]

      -- restore as the requested type/name
      m2 <- cmdRestore srv scratchBin [(outVar, typ)]
      case m2 of
        Just err -> error ("cmdRestore failed: " <> show err)
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
  insP <- either (error . show) pure =<< cmdInputs srv propName
  inputType <- case insP of
    [inp] -> pure (inputType inp) -- this is the important fix
    [] -> error $ "Expected property " <> T.unpack propName <> " to have exactly one input, got none."
    tys -> error $ "Expected property " <> T.unpack propName <> " to have exactly one input, got: " <> show (map inputType tys)

  packType scratchBin srv var inputType componentVars

getVal :: (GetValue a) => Server -> VarName -> IO a
getVal s name = do
  v <- getDataVal s name
  case getValue v of
    Just b -> pure b
    -- expected type of a
    Nothing -> error $ "Expected " <> T.unpack name <> " to decode, got: " <> T.unpack (valueText v)

getDataVal :: Server -> VarName -> IO Value
getDataVal s name = do
  r <- FSV.getValue s name
  case r of
    Left msg -> error $ "getValue failed for " <> T.unpack name <> ": " <> T.unpack msg
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
            Just err -> error $ "cmdProject failed for field " <> T.unpack fname <> ": " <> show err
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
  tys <- either (error . show) pure tysE
  case tys of
    [ty] -> pure ty
    [] -> error $ "Entrypoint " <> T.unpack ep <> " has no inputs (expected 1)."
    _ -> error $ "Entrypoint " <> T.unpack ep <> " has >1 input (expected 1): " <> show tys

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

requestedPropertyNames :: [InputOutputs] -> [T.Text]
requestedPropertyNames ios =
  nubText [iosEntryPoint io | io <- ios, isPropertyInputOutput io]

declaredPropertyNames :: [PropSpec] -> [T.Text]
declaredPropertyNames specs =
  nubText $ map psProp specs

nubText :: [T.Text] -> [T.Text]
nubText = go []
  where
    go _ [] = []
    go seen (x:xs)
      | x `elem` seen = go seen xs
      | otherwise = x : go (x : seen) xs

propertyDiagnostics :: [T.Text] -> [PropSpec] -> [TestResult]
propertyDiagnostics requested specs =
  missingRequested ++ missingDeclarations
  where
    declared = declaredPropertyNames specs

    requestedWithoutAttr =
      [ name | name <- requested, name `notElem` declared ]

    declaredWithoutRequest =
      [ name | name <- declared, name `notElem` requested ]

    missingRequested =
      [ Failure
          [ "Unknown property in test specification: "
              <> name
              <> "\nThere is a '-- property: "
              <> name
              <> "' block, but no matching #[prop(...)] attribute on that entry point."
          ]
      | name <- requestedWithoutAttr
      ]

    missingDeclarations =
      [ Failure
          [ "Undeclared property test block for attribute-backed property: "
              <> name
              <> "\nThere is a #[prop(...)] attribute on entry point '"
              <> name
              <> "', but no matching '-- property: "
              <> name
              <> "' block in the test specification."
          ]
      | name <- declaredWithoutRequest
      ]