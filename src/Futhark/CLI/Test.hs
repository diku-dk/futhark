{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @futhark test@
module Futhark.CLI.Test (main) where

import Control.Applicative.Lift (Errors, Lift (..), failure, runErrors)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Except hiding (throwError)
import qualified Control.Monad.Except as E
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import Data.List (delete, partition)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Futhark.Analysis.Metrics.Type
import Futhark.Server
import Futhark.Test
import Futhark.Util (atMostChars, fancyTerminal)
import Futhark.Util.Console
import Futhark.Util.Options
import Futhark.Util.Table
import System.Console.ANSI
import qualified System.Console.Terminal.Size as Terminal
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process.ByteString (readProcessWithExitCode)
import Text.Regex.TDFA

--- Test execution

type TestM = ExceptT [T.Text] IO

-- Taken from transformers-0.5.5.0.
eitherToErrors :: Either e a -> Errors e a
eitherToErrors = either failure Pure

throwError :: MonadError [e] m => e -> m a
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

context1 :: Monad m => T.Text -> ExceptT T.Text m a -> ExceptT T.Text m a
context1 s = withExceptT $ \e -> s <> ":\n" <> e

accErrors :: [TestM a] -> TestM [a]
accErrors tests = do
  eithers <- lift $ mapM runExceptT tests
  let errors = traverse eitherToErrors eithers
  ExceptT $ return $ runErrors errors

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

  context prog_ctx $
    pureTestResults $ liftIO $ withServer (futharkServerCfg to_run to_run_args) f

data TestCase = TestCase
  { _testCaseMode :: TestMode,
    testCaseProgram :: FilePath,
    testCaseTest :: ProgramTest,
    _testCasePrograms :: ProgConfig
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
    SeqMemPipeline ->
      check ["--seq-mem"]
    GpuMemPipeline ->
      check ["--gpu-mem"]
    NoPipeline ->
      check []
  where
    check opt = do
      futhark <- liftIO $ maybe getExecutablePath return $ configFuthark programs
      let opts = ["dev"] ++ opt ++ ["--metrics", program]
      (code, output, err) <- liftIO $ readProcessWithExitCode futhark opts ""
      let output' = T.decodeUtf8 output
      case code of
        ExitSuccess
          | [(m, [])] <- reads $ T.unpack output' -> return m
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
    maybePipeline GpuPipeline = "(kernels) "
    maybePipeline SeqMemPipeline = "(seq-mem) "
    maybePipeline GpuMemPipeline = "(gpu-mem) "
    maybePipeline NoPipeline = ""

    ok (AstMetrics metrics) (name, expected_occurences) =
      case M.lookup name metrics of
        Nothing
          | expected_occurences > 0 ->
            throwError $
              name <> maybePipeline pipeline <> " should have occurred " <> T.pack (show expected_occurences)
                <> " times, but did not occur at all in optimised program."
        Just actual_occurences
          | expected_occurences /= actual_occurences ->
            throwError $
              name <> maybePipeline pipeline <> " should have occurred " <> T.pack (show expected_occurences)
                <> " times, but occurred "
                <> T.pack (show actual_occurences)
                <> " times."
        _ -> return ()

testWarnings :: [WarningTest] -> SBS.ByteString -> TestM ()
testWarnings warnings futerr = accErrors_ $ map testWarning warnings
  where
    testWarning (ExpectedWarning regex_s regex)
      | not (match regex $ T.unpack $ T.decodeUtf8 futerr) =
        throwError $
          "Expected warning:\n  " <> regex_s
            <> "\nGot warnings:\n  "
            <> T.decodeUtf8 futerr
      | otherwise = return ()

runInterpretedEntry :: FutharkExe -> FilePath -> InputOutputs -> TestM ()
runInterpretedEntry (FutharkExe futhark) program (InputOutputs entry run_cases) =
  let dir = takeDirectory program
      runInterpretedCase run@(TestRun _ inputValues _ index _) =
        unless (any (`elem` runTags run) ["compiled", "script"]) $
          context ("Entry point: " <> entry <> "; dataset: " <> T.pack (runDescription run)) $ do
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
runTestCase (TestCase mode program testcase progs) = do
  futhark <- liftIO $ maybe getExecutablePath return $ configFuthark progs
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
      context checkctx $ do
        (code, _, err) <-
          liftIO $ readProcessWithExitCode futhark ["check", program] ""
        case code of
          ExitSuccess -> throwError "Expected failure\n"
          ExitFailure 127 -> throwError $ progNotFound $ T.pack futhark
          ExitFailure 1 -> throwError $ T.decodeUtf8 err
          ExitFailure _ -> liftExcept $ checkError expected_error $ T.decodeUtf8 err
    RunCases {} | mode == TypeCheck -> do
      let options = ["check", program] ++ configExtraCompilerOptions progs
      context checkctx $ do
        (code, _, err) <- liftIO $ readProcessWithExitCode futhark options ""

        case code of
          ExitSuccess -> return ()
          ExitFailure 127 -> throwError $ progNotFound $ T.pack futhark
          ExitFailure _ -> throwError $ T.decodeUtf8 err
    RunCases ios structures warnings -> do
      -- Compile up-front and reuse same executable for several entry points.
      let backend = configBackend progs
          extra_compiler_options = configExtraCompilerOptions progs

      unless (mode == Compile) $
        context "Generating reference outputs" $
          -- We probably get the concurrency at the test program level,
          -- so force just one data set at a time here.
          ensureReferenceOutput (Just 1) (FutharkExe futhark) "c" program ios

      unless (mode == Interpreted) $
        context ("Compiling with --backend=" <> T.pack backend) $ do
          compileTestProgram extra_compiler_options (FutharkExe futhark) backend program warnings
          mapM_ (testMetrics progs program) structures
          unless (mode == Compile) $ do
            (tuning_opts, _) <-
              liftIO $ determineTuning (configTuning progs) program
            let extra_options = tuning_opts ++ configExtraOptions progs
                runner = configRunner progs
            context "Running compiled program" $
              withProgramServer program runner extra_options $ \server -> do
                let run = runCompiledEntry (FutharkExe futhark) server program
                concat <$> mapM run ios

      unless (mode == Compile || mode == Compiled) $
        context "Interpreting" $
          accErrors_ $ map (runInterpretedEntry (FutharkExe futhark) program) ios

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
      let outs = ["out" <> T.pack (show i) | i <- [0 .. length output_types' - 1]]
          ins = ["in" <> T.pack (show i) | i <- [0 .. length input_types' - 1]]
          onRes = either (Failure . pure) (const Success)
      mapM (fmap onRes . runCompiledCase input_types' outs ins) run_cases
  where
    dir = takeDirectory program

    runCompiledCase input_types outs ins run = runExceptT $ do
      let TestRun _ input_spec _ index _ = run
          case_ctx =
            "Entry point: " <> entry <> "; dataset: "
              <> T.pack (runDescription run)

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

checkError :: MonadError T.Text m => ExpectedError -> T.Text -> m ()
checkError (ThisError regex_s regex) err
  | not (match regex $ T.unpack err) =
    E.throwError $
      "Expected error:\n  " <> regex_s
        <> "\nGot error:\n  "
        <> err
checkError _ _ =
  return ()

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
    Right vs -> return $ SuccessResult vs
runResult _ (ExitFailure _) _ stderr_s =
  return $ ErrorResult $ T.decodeUtf8 stderr_s

compileTestProgram :: [String] -> FutharkExe -> String -> FilePath -> [WarningTest] -> TestM ()
compileTestProgram extra_options futhark backend program warnings = do
  (_, futerr) <- compileProgram ("--server" : extra_options) futhark backend program
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
  return ()
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
  E.throwError $ "Program succeeded, but expected failure:\n  " <> T.pack (show f)

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
    save e = return $ Failure [T.pack $ show e]

doTest :: TestCase -> IO TestResult
doTest = catching . runTestM . runTestCase

makeTestCase :: TestConfig -> TestMode -> (FilePath, ProgramTest) -> TestCase
makeTestCase config mode (file, spec) =
  TestCase mode file spec $ configPrograms config

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

-- | Exclude those test cases that have tags we do not wish to run.
excludeCases :: TestConfig -> TestCase -> TestCase
excludeCases config tcase =
  tcase {testCaseTest = onTest $ testCaseTest tcase}
  where
    onTest (ProgramTest desc tags action) =
      ProgramTest desc tags $ onAction action
    onAction (RunCases ios stest wtest) =
      RunCases (map onIOs ios) stest wtest
    onAction action = action
    onIOs (InputOutputs entry runs) =
      InputOutputs entry $ filter (not . any excluded . runTags) runs
    excluded = (`elem` configExclude config) . T.pack

statusTable :: TestStatus -> String
statusTable ts = buildTable rows 1
  where
    rows =
      [ [mkEntry "", passed, failed, mkEntry "remaining"],
        map mkEntry ["programs", passedProgs, failedProgs, remainProgs'],
        map mkEntry ["runs", passedRuns, failedRuns, remainRuns']
      ]
    passed = ("passed", [SetColor Foreground Vivid Green])
    failed = ("failed", [SetColor Foreground Vivid Red])
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
tableLines = 1 + (length . lines $ blankTable)
  where
    blankTable = statusTable $ TestStatus [] [] 0 0 0 0 0 0 0

spaceTable :: IO ()
spaceTable = putStr $ replicate tableLines '\n'

reportTable :: TestStatus -> IO ()
reportTable ts = do
  moveCursorToTableTop
  putStrLn $ statusTable ts
  clearLine
  w <- maybe 80 Terminal.width <$> Terminal.size
  putStrLn $ atMostChars (w - length labelstr) running
  where
    running = labelstr ++ (unwords . reverse . map testCaseProgram . testStatusRun) ts
    labelstr = "Now testing: "

moveCursorToTableTop :: IO ()
moveCursorToTableTop = cursorUpLine tableLines

reportText :: TestStatus -> IO ()
reportText ts =
  putStr $
    "(" ++ show (testStatusFail ts) ++ " failed, "
      ++ show (testStatusPass ts)
      ++ " passed, "
      ++ show num_remain
      ++ " to go).\n"
  where
    num_remain = length $ testStatusRemain ts

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
  testmvar <- newEmptyMVar
  reportmvar <- newEmptyMVar
  concurrency <- maybe getNumCapabilities pure $ configConcurrency config
  replicateM_ concurrency $ forkIO $ runTest testmvar reportmvar

  let (excluded, included) = partition (excludedTest config) all_tests
  _ <- forkIO $ mapM_ (putMVar testmvar . excludeCases config) included

  let fancy = not (configLineOutput config) && fancyTerminal

      report
        | fancy = reportTable
        | otherwise = reportText
      clear
        | fancy = clearFromCursorToScreenEnd
        | otherwise = putStr "\n"

      numTestCases tc =
        case testAction $ testCaseTest tc of
          CompileTimeFailure _ -> 1
          RunCases ios sts wts ->
            (length . concat) (iosTestRuns <$> ios)
              + length sts
              + length wts

      getResults ts
        | null (testStatusRemain ts) = report ts >> return ts
        | otherwise = do
          report ts
          msg <- takeMVar reportmvar
          case msg of
            TestStarted test -> do
              unless fancy $
                putStr $ "Started testing " <> testCaseProgram test <> " "
              getResults $ ts {testStatusRun = test : testStatusRun ts}
            TestDone test res -> do
              let ts' =
                    ts
                      { testStatusRemain = test `delete` testStatusRemain ts,
                        testStatusRun = test `delete` testStatusRun ts,
                        testStatusRunsRemain =
                          testStatusRunsRemain ts
                            - numTestCases test
                      }
              case res of
                Success -> do
                  let ts'' =
                        ts'
                          { testStatusRunPass =
                              testStatusRunPass ts' + numTestCases test
                          }
                  unless fancy $
                    putStr $ "Finished testing " <> testCaseProgram test <> " "
                  getResults $ ts'' {testStatusPass = testStatusPass ts + 1}
                Failure s -> do
                  when fancy moveCursorToTableTop
                  clear
                  putStr $ inBold (testCaseProgram test <> ":\n") <> T.unpack (T.unlines s)
                  when fancy spaceTable
                  getResults $
                    ts'
                      { testStatusFail = testStatusFail ts' + 1,
                        testStatusRunPass =
                          testStatusRunPass ts'
                            + numTestCases test - length s,
                        testStatusRunFail =
                          testStatusRunFail ts'
                            + length s
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
  when fancy $ cursorUpLine 1 >> clearLine

  let excluded_str
        | null excluded = ""
        | otherwise = " (" ++ show (length excluded) ++ " program(s) excluded).\n"
  putStr excluded_str
  exitWith $ case testStatusFail ts of
    0 -> ExitSuccess
    _ -> ExitFailure 1

---
--- Configuration and command line parsing
---

data TestConfig = TestConfig
  { configTestMode :: TestMode,
    configPrograms :: ProgConfig,
    configExclude :: [T.Text],
    configLineOutput :: Bool,
    configConcurrency :: Maybe Int
  }

defaultConfig :: TestConfig
defaultConfig =
  TestConfig
    { configTestMode = Everything,
      configExclude = ["disable"],
      configPrograms =
        ProgConfig
          { configBackend = "c",
            configFuthark = Nothing,
            configRunner = "",
            configExtraOptions = [],
            configExtraCompilerOptions = [],
            configTuning = Just "tuning"
          },
      configLineOutput = False,
      configConcurrency = Nothing
    }

data ProgConfig = ProgConfig
  { configBackend :: String,
    configFuthark :: Maybe FilePath,
    configRunner :: FilePath,
    configExtraCompilerOptions :: [String],
    configTuning :: Maybe String,
    -- | Extra options passed to the programs being run.
    configExtraOptions :: [String]
  }
  deriving (Show)

changeProgConfig :: (ProgConfig -> ProgConfig) -> TestConfig -> TestConfig
changeProgConfig f config = config {configPrograms = f $ configPrograms config}

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

data TestMode
  = TypeCheck
  | Compile
  | Compiled
  | Interpreted
  | Everything
  deriving (Eq, Show)

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
      "Only run compiled code",
    Option
      "C"
      ["compile"]
      (NoArg $ Right $ \config -> config {configTestMode = Compile})
      "Only compile, do not run.",
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
      ["no-tuning"]
      (NoArg $ Right $ changeProgConfig $ \config -> config {configTuning = Nothing})
      "Do not load tuning files.",
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
      "Number of tests to run concurrently."
  ]

excludeBackend :: TestConfig -> TestConfig
excludeBackend config =
  config
    { configExclude =
        "no_" <> T.pack (configBackend (configPrograms config)) :
        configExclude config
    }

-- | Run @futhark test@.
main :: String -> [String] -> IO ()
main = mainWithOptions defaultConfig commandLineOptions "options... programs..." $ \progs config ->
  case progs of
    [] -> Nothing
    _ -> Just $ runTests (excludeBackend config) progs
