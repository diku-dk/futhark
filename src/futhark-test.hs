{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
-- | This program is a convenience utility for running the Futhark
-- test suite, and its test programs.
module Main (main) where

import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import Control.Concurrent
import Control.Monad
import Control.Exception
import Control.Monad.Except

import Data.List
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Map.Strict as M
import System.Console.GetOpt
import System.Process.ByteString (readProcessWithExitCode)
import System.Exit
import System.IO
import System.FilePath
import Text.Regex.TDFA

import Futhark.Util.Pretty (prettyText)
import Futhark.Analysis.Metrics
import Futhark.Test

import Futhark.Util.Options

--- Test execution

type TestM = ExceptT T.Text IO

runTestM :: TestM () -> IO TestResult
runTestM = fmap (either Failure $ const Success) . runExceptT

io :: IO a -> TestM a
io = liftIO

context :: T.Text -> TestM a -> TestM a
context s = withExceptT ((s<>":\n")<>)

data TestResult = Success
                | Failure T.Text
                deriving (Eq, Show)

data TestCase = TestCase { _testCaseMode :: TestMode
                         , testCaseProgram :: FilePath
                         , testCaseTest :: ProgramTest
                         , _testCasePrograms :: ProgConfig
                         }
                deriving (Show)

instance Eq TestCase where
  x == y = testCaseProgram x == testCaseProgram y

instance Ord TestCase where
  x `compare` y = testCaseProgram x `compare` testCaseProgram y

data RunResult = ErrorResult Int SBS.ByteString
               | SuccessResult [Value]

progNotFound :: T.Text -> T.Text
progNotFound s = s <> ": command not found"

optimisedProgramMetrics :: ProgConfig -> StructurePipeline -> FilePath -> TestM AstMetrics
optimisedProgramMetrics programs pipeline program =
  case pipeline of SOACSPipeline ->
                     check "-s"
                   KernelsPipeline ->
                     check "--kernels"
                   SequentialCpuPipeline ->
                     check "--cpu"
                   GpuPipeline ->
                     check "--gpu"
  where check opt = do
          (code, output, err) <-
            io $ readProcessWithExitCode (configTypeChecker programs) [opt, "--metrics", program] ""
          let output' = T.decodeUtf8 output
          case code of
            ExitSuccess
              | [(m, [])] <- reads $ T.unpack output' -> return m
              | otherwise -> throwError $ "Could not read metrics output:\n" <> output'
            ExitFailure 127 -> throwError $ progNotFound $ T.pack $ configTypeChecker programs
            ExitFailure _ -> throwError $ T.decodeUtf8 err

testMetrics :: ProgConfig -> FilePath -> StructureTest -> TestM ()
testMetrics programs program (StructureTest pipeline (AstMetrics expected)) =
  context "Checking metrics" $ do
    actual <- optimisedProgramMetrics programs pipeline program
    mapM_ (ok actual) $ M.toList expected
  where ok (AstMetrics metrics) (name, expected_occurences) =
          case M.lookup name metrics of
            Nothing
              | expected_occurences > 0 ->
              throwError $ name <> " should have occurred " <> T.pack (show expected_occurences) <>
              " times, but did not occur at all in optimised program."
            Just actual_occurences
              | expected_occurences /= actual_occurences ->
                throwError $ name <> " should have occurred " <> T.pack (show expected_occurences) <>
              " times, but occured " <> T.pack (show actual_occurences) <> " times."
            _ -> return ()

testWarnings :: [WarningTest] -> SBS.ByteString -> TestM ()
testWarnings warnings futerr = mapM_ testWarning warnings
  where testWarning (ExpectedWarning regex_s regex)
          | not (match regex $ T.unpack $ T.decodeUtf8 futerr) =
            throwError $ "Expected warning:\n  " <> regex_s <>
            "\nGot warnings:\n  " <> T.decodeUtf8 futerr
          | otherwise = return ()

runTestCase :: TestCase -> TestM ()
runTestCase (TestCase mode program testcase progs) =
  case testAction testcase of

    CompileTimeFailure expected_error -> do
      let typeChecker = configTypeChecker progs
      context ("Type-checking with " <> T.pack typeChecker) $ do
        (code, _, err) <-
          io $ readProcessWithExitCode typeChecker ["-t", program] ""
        case code of
         ExitSuccess -> throwError "Expected failure\n"
         ExitFailure 127 -> throwError $ progNotFound $ T.pack typeChecker
         ExitFailure 1 -> throwError $ T.decodeUtf8 err
         ExitFailure _ -> checkError expected_error err

    RunCases _ _ warnings | mode == TypeCheck -> do
      let typeChecker = configTypeChecker progs
          options = ["-t", program] ++ configExtraCompilerOptions progs
      context ("Type-checking with " <> T.pack typeChecker) $ do
        (code, _, err) <- io $ readProcessWithExitCode typeChecker options ""
        testWarnings warnings err
        case code of
         ExitSuccess -> return ()
         ExitFailure 127 -> throwError $ progNotFound $ T.pack typeChecker
         ExitFailure _ -> throwError $ T.decodeUtf8 err

    RunCases ios structures warnings -> do
      -- Compile up-front and reuse same executable for several entry points.
      let compiler = configCompiler progs
          extra_options = configExtraCompilerOptions progs
      unless (mode == Interpreted) $
        context ("Compiling with " <> T.pack compiler) $ do
          compileTestProgram extra_options compiler program warnings
          mapM_ (testMetrics progs program) structures
      unless (mode == Compile) $
        mapM_ runInputOutputs ios

  where

    runInputOutputs (InputOutputs entry run_cases) =
      forM_ run_cases $ \run -> context ("Entry point: " <> entry <> "; dataset: " <>
                                         T.pack (runDescription run)) $ do
        let interpreter = configInterpreter progs
        unless (mode == Compiled || "compiled" `elem` runTags run) $
          context ("Interpreting with " <> T.pack interpreter) $
            interpretTestProgram interpreter program entry run

        context "Running compiled program" $
          runCompiledTestProgram (configExtraOptions progs) program entry run

checkError :: ExpectedError -> SBS.ByteString -> TestM ()
checkError (ThisError regex_s regex) err
  | not (match regex $ T.unpack $ T.decodeUtf8 err) =
     throwError $ "Expected error:\n  " <> regex_s <>
     "\nGot error:\n  " <> T.decodeUtf8 err
checkError _ _ =
  return ()

runResult :: FilePath -> ExitCode -> SBS.ByteString -> SBS.ByteString -> TestM RunResult
runResult program ExitSuccess stdout_s _ =
  case valuesFromByteString "stdout" $ LBS.fromStrict stdout_s of
    Left e   -> do
      let actualf = program `addExtension` "actual"
      io $ SBS.writeFile actualf stdout_s
      throwError $ T.pack e <> "\n(See " <> T.pack actualf <> ")"
    Right vs -> return $ SuccessResult vs
runResult _ (ExitFailure code) _ stderr_s =
  return $ ErrorResult code stderr_s

getExpectedResult :: MonadIO m =>
                     FilePath -> ExpectedResult Values
                  -> m (ExpectedResult [Value])
getExpectedResult dir (Succeeds (Just vals)) = Succeeds . Just <$> getValues dir vals
getExpectedResult _   (Succeeds Nothing) = return $ Succeeds Nothing
getExpectedResult _   (RunTimeFailure err) = return $ RunTimeFailure err

interpretTestProgram :: String -> FilePath -> T.Text -> TestRun -> TestM ()
interpretTestProgram futharki program entry (TestRun _ inputValues expectedResult _) = do
  input <- T.unlines . map prettyText <$> getValues dir inputValues
  expectedResult' <- getExpectedResult dir expectedResult
  (code, output, err) <-
    io $ readProcessWithExitCode futharki ["-e", T.unpack entry, program] $
    T.encodeUtf8 input
  case code of
    ExitFailure 127 ->
      throwError $ progNotFound $ T.pack futharki
    _               ->
      compareResult program expectedResult' =<< runResult program code output err
  where dir = takeDirectory program

compileTestProgram :: [String] -> String -> FilePath -> [WarningTest] -> TestM ()
compileTestProgram extra_options futharkc program warnings = do
  (futcode, _, futerr) <- io $ readProcessWithExitCode futharkc options ""
  testWarnings warnings futerr
  case futcode of
    ExitFailure 127 -> throwError $ progNotFound $ T.pack futharkc
    ExitFailure _   -> throwError $ T.decodeUtf8 futerr
    ExitSuccess     -> return ()
  where binOutputf = dropExtension program
        options = [program, "-o", binOutputf] ++ extra_options

runCompiledTestProgram :: [String] -> String -> T.Text -> TestRun -> TestM ()
runCompiledTestProgram extra_options program entry (TestRun _ inputValues expectedResult _) = do
  input <- getValuesBS dir inputValues
  expectedResult' <- getExpectedResult dir expectedResult
  -- Explicitly prefixing the current directory is necessary for
  -- readProcessWithExitCode to find the binary when binOutputf has
  -- no path component.
  let binpath = "." </> binOutputf
      entry_options = ["-e", T.unpack entry]
  context ("Running " <> T.pack (unwords $ binpath : entry_options ++ extra_options)) $ do
    (progCode, output, progerr) <-
      io $ readProcessWithExitCode binpath (entry_options ++ extra_options) $
      LBS.toStrict input
    withExceptT validating $
      compareResult program expectedResult' =<< runResult program progCode output progerr
  where binOutputf = dropExtension program
        dir = takeDirectory program
        validating = ("validating test result:\n"<>)

compareResult :: FilePath -> ExpectedResult [Value] -> RunResult
              -> TestM ()
compareResult _ (Succeeds Nothing) SuccessResult{} =
  return ()
compareResult program (Succeeds (Just expectedResult)) (SuccessResult actualResult) =
  case compareValues actualResult expectedResult of
    Just mismatch -> do
      let actualf = program `addExtension` "actual"
          expectedf = program `addExtension` "expected"
      io $ SBS.writeFile actualf $
        T.encodeUtf8 $ T.unlines $ map prettyText actualResult
      io $ SBS.writeFile expectedf $
        T.encodeUtf8 $ T.unlines $ map prettyText expectedResult
      throwError $ T.pack actualf <> " and " <> T.pack expectedf <>
        " do not match:\n" <> T.pack (show mismatch)
    Nothing ->
      return ()
compareResult _ (RunTimeFailure expectedError) (ErrorResult _ actualError) =
  checkError expectedError actualError
compareResult _ (Succeeds _) (ErrorResult code err) =
  throwError $ "Program failed with error code " <>
  T.pack (show code) <> " and stderr:\n  " <> T.decodeUtf8 err
compareResult _ (RunTimeFailure f) (SuccessResult _) =
  throwError $ "Program succeeded, but expected failure:\n  " <> T.pack (show f)

---
--- Test manager
---

data TestStatus = TestStatus { testStatusRemain :: [TestCase]
                             , testStatusRun :: [TestCase]
                             , testStatusFail :: Int
                             , testStatusPass :: Int
                             , testStatusCases :: Int
                             }

catching :: IO TestResult -> IO TestResult
catching m = m `catch` save
  where save :: SomeException -> IO TestResult
        save e = return $ Failure $ T.pack $ show e

doTest :: TestCase -> IO TestResult
doTest = catching . runTestM . runTestCase

makeTestCase :: TestConfig -> TestMode -> (FilePath, ProgramTest) -> TestCase
makeTestCase config mode (file, spec) =
  TestCase mode file spec $ configPrograms config

data ReportMsg = TestStarted TestCase
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

clearLine :: IO ()
clearLine = putStr "\27[2K"

reportInteractive :: TestStatus -> IO ()
reportInteractive ts = do
  clearLine
  putStr $ atMostChars 160 line ++ "\r"
  hFlush stdout
    where num_running = length $ testStatusRun ts
          num_remain  = length $ testStatusRemain ts
          line = show (testStatusFail ts)  ++ " failed; " ++
                 show (testStatusPass ts)  ++ " passed; " ++
                 show num_remain           ++ " to go; currently running " ++
                 show num_running          ++ " tests: " ++
                 (unwords . reverse . map testCaseProgram . testStatusRun) ts

atMostChars :: Int -> String -> String
atMostChars n s | length s > n = take (n-3) s ++ "..."
                | otherwise    = s

reportText :: TestStatus -> IO ()
reportText ts =
  putStr $ "(" ++ show (testStatusFail ts)  ++ " failed, " ++
                  show (testStatusPass ts)  ++ " passed, " ++
                  show num_remain           ++ " to go).\n"
    where num_remain  = length $ testStatusRemain ts

runTests :: TestConfig -> [FilePath] -> IO ()
runTests config paths = do
  -- We force line buffering to ensure that we produce running output.
  -- Otherwise, CI tools and the like may believe we are hung and kill
  -- us.
  hSetBuffering stdout LineBuffering

  let mode = configTestMode config
  all_tests <- map (makeTestCase config mode) <$> testSpecsFromPaths paths

  testmvar <- newEmptyMVar
  reportmvar <- newEmptyMVar
  concurrency <- getNumCapabilities
  replicateM_ concurrency $ forkIO $ runTest testmvar reportmvar

  let (excluded, included) = partition (excludedTest config) all_tests
  _ <- forkIO $ mapM_ (putMVar testmvar) included
  isTTY <- (&& not (configUnbufferOutput config)) <$> hIsTerminalDevice stdout

  let report = if isTTY then reportInteractive else reportText
      clear  = if isTTY then clearLine else putStr "\n"

      numTestCases tc =
        case testAction $ testCaseTest tc of
          CompileTimeFailure _ -> 1
          RunCases inputOutputs _ _ -> length . concat $ iosTestRuns <$> inputOutputs

      getResults ts
        | null (testStatusRemain ts) = clear >> return ts
        | otherwise = do
          report ts
          msg <- takeMVar reportmvar
          case msg of
            TestStarted test -> do
              unless isTTY $
                putStr $ "Started testing " <> testCaseProgram test <> " "
              getResults $ ts {testStatusRun = test : testStatusRun ts}
            TestDone test res -> do
              let ts' = ts { testStatusRemain = test `delete` testStatusRemain ts
                           , testStatusRun    = test `delete` testStatusRun ts
                           , testStatusCases  = numTestCases test + testStatusCases ts
                           }
              case res of
                Success -> do
                  unless isTTY $
                    putStr $ "Finished testing " <> testCaseProgram test <> " "
                  getResults $ ts' { testStatusPass = testStatusPass ts + 1}
                Failure s -> do
                  clear
                  T.putStrLn (T.pack (testCaseProgram test) <> ":\n" <> s)
                  getResults $ ts' { testStatusFail = testStatusFail ts + 1}

  ts <- getResults TestStatus { testStatusRemain = included
                              , testStatusRun    = []
                              , testStatusFail   = 0
                              , testStatusPass   = 0
                              , testStatusCases  = 0
                              }
  let excluded_str = if null excluded
                     then ""
                     else " (" ++ show (length excluded) ++ " excluded)"
  putStrLn $ show (testStatusFail ts)  ++ " failed, " ++
             show (testStatusPass ts)  ++ " passed, " ++
             show (testStatusCases ts) ++ " cases run" ++ excluded_str ++ "."
  exitWith $ case testStatusFail ts of 0 -> ExitSuccess
                                       _ -> ExitFailure 1

---
--- Configuration and command line parsing
---

data TestConfig = TestConfig
                  { configTestMode :: TestMode
                  , configPrograms :: ProgConfig
                  , configExclude :: [T.Text]
                  , configUnbufferOutput :: Bool
                  }

defaultConfig :: TestConfig
defaultConfig = TestConfig { configTestMode = Everything
                           , configExclude = [ "disable" ]
                           , configPrograms =
                             ProgConfig
                             { configCompiler = "futhark-c"
                             , configInterpreter = "futharki"
                             , configTypeChecker = "futhark"
                             , configExtraOptions = []
                             , configExtraCompilerOptions = []
                             }
                           , configUnbufferOutput = False
                           }

data ProgConfig = ProgConfig
                  { configCompiler :: FilePath
                  , configInterpreter :: FilePath
                  , configTypeChecker :: FilePath
                  , configExtraCompilerOptions :: [String]
                  , configExtraOptions :: [String]
                  -- ^ Extra options passed to the programs being run.
                  }
                  deriving (Show)

changeProgConfig :: (ProgConfig -> ProgConfig) -> TestConfig -> TestConfig
changeProgConfig f config = config { configPrograms = f $ configPrograms config }

setCompiler :: FilePath -> ProgConfig -> ProgConfig
setCompiler compiler config =
  config { configCompiler = compiler }

setInterpreter :: FilePath -> ProgConfig -> ProgConfig
setInterpreter interpreter config =
  config { configInterpreter = interpreter }

setTypeChecker :: FilePath -> ProgConfig -> ProgConfig
setTypeChecker typeChecker config =
  config { configTypeChecker = typeChecker }

addCompilerOption :: String -> ProgConfig -> ProgConfig
addCompilerOption option config =
  config { configExtraCompilerOptions = configExtraCompilerOptions config ++ [option] }

addOption :: String -> ProgConfig -> ProgConfig
addOption option config =
  config { configExtraOptions = configExtraOptions config ++ [option] }

data TestMode = TypeCheck
              | Compile
              | Compiled
              | Interpreted
              | Everything
              deriving (Eq, Show)

commandLineOptions :: [FunOptDescr TestConfig]
commandLineOptions = [
    Option "t" ["typecheck"]
    (NoArg $ Right $ \config -> config { configTestMode = TypeCheck })
    "Only perform type-checking"
  , Option "i" ["interpreted"]
    (NoArg $ Right $ \config -> config { configTestMode = Interpreted })
    "Only interpret"
  , Option "c" ["compiled"]
    (NoArg $ Right $ \config -> config { configTestMode = Compiled })
    "Only run compiled code"
  , Option "C" ["compile"]
    (NoArg $ Right $ \config -> config { configTestMode = Compile })
    "Only compile, do not run."
  , Option [] ["nobuffer"]
    (NoArg $ Right $ \config -> config { configUnbufferOutput = True })
    "Do not buffer output, and write each result on a line by itself."
  , Option [] ["typechecker"]
    (ReqArg (Right . changeProgConfig . setTypeChecker) "PROGRAM")
    "What to run for type-checking (defaults to 'futhark')."
  , Option [] ["compiler"]
    (ReqArg (Right . changeProgConfig . setCompiler) "PROGRAM")
    "What to run for code generation (defaults to 'futhark-c')."
  , Option [] ["interpreter"]
    (ReqArg (Right . changeProgConfig . setInterpreter) "PROGRAM")
    "What to run for interpretation (defaults to 'futharki')."
  , Option [] ["exclude"]
    (ReqArg (\tag ->
               Right $ \config ->
               config { configExclude = T.pack tag : configExclude config })
     "TAG")
    "Exclude test programs that define this tag."
  , Option "p" ["pass-option"]
    (ReqArg (Right . changeProgConfig . addOption) "OPT")
    "Pass this option to programs being run."
  , Option [] ["pass-compiler-option"]
    (ReqArg (Right . changeProgConfig . addCompilerOption) "OPT")
    "Pass this option to the compiler (or typechecker if in -t mode)."
  ]

main :: IO ()
main = mainWithOptions defaultConfig commandLineOptions "options... programs..." $ \progs config ->
  Just $ runTests config progs
