{-# LANGUAGE OverloadedStrings, TupleSections, FlexibleContexts #-}
-- | This program is a convenience utility for running the Futhark
-- test suite, and its test programs.
module Main (main) where


import Control.Applicative
import Control.Concurrent
import Control.Monad hiding (forM_)
import Control.Exception hiding (try)
import Control.Monad.Except hiding (forM_)

import Data.List hiding (foldl')
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Foldable (forM_)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.HashMap.Lazy as HM
import System.Console.GetOpt
import System.Directory
import System.Process.Text (readProcessWithExitCode)
import System.Exit
import System.IO
import System.FilePath
import Text.Regex.TDFA

import Prelude

import Futhark.Util.Pretty (prettyText)
import Futhark.Analysis.Metrics
import Futhark.Pipeline
import Futhark.Compiler
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
                         , _testCaseOptions :: [String]
                         -- ^ Extra options to pass to the program.
                         }
                deriving (Show)

instance Eq TestCase where
  x == y = testCaseProgram x == testCaseProgram y

instance Ord TestCase where
  x `compare` y = testCaseProgram x `compare` testCaseProgram y

data RunResult = ErrorResult Int T.Text
               | SuccessResult [Value]

progNotFound :: T.Text -> T.Text
progNotFound s = s <> ": command not found"

structTestConfig :: FutharkConfig
structTestConfig = newFutharkConfig { futharkWarn = False }

optimisedProgramMetrics :: StructurePipeline -> FilePath -> TestM AstMetrics
optimisedProgramMetrics (SOACSPipeline pipeline) program = do
  res <- io $ runFutharkM (runPipelineOnProgram structTestConfig pipeline program) False
  case res of
    Left err ->
      throwError $ errorDesc err
    Right prog ->
      return $ progMetrics prog
optimisedProgramMetrics (KernelsPipeline pipeline) program = do
  res <- io $ runFutharkM (runPipelineOnProgram structTestConfig pipeline program) False
  case res of
    Left err ->
      throwError $ errorDesc err
    Right prog ->
      return $ progMetrics prog

testMetrics :: FilePath -> StructureTest -> TestM ()
testMetrics program (StructureTest pipeline expected) = context "Checking metrics" $ do
  actual <- optimisedProgramMetrics pipeline program
  mapM_ (ok actual) $ HM.toList expected
  where ok metrics (name, expected_occurences) =
          case HM.lookup name metrics of
            Nothing
              | expected_occurences > 0 ->
              throwError $ name <> " should have occurred " <> T.pack (show expected_occurences) <>
              " times, but did not occur at all in optimised program."
            Just actual_occurences
              | expected_occurences /= actual_occurences ->
                throwError $ name <> " should have occurred " <> T.pack (show expected_occurences) <>
              " times, but occured " <> T.pack (show actual_occurences) <> " times."
            _ -> return ()

runTestCase :: TestCase -> TestM ()
runTestCase (TestCase mode program testcase progs extra_options) = do
  forM_ (testExpectedStructure testcase) $ testMetrics program

  case testAction testcase of

    CompileTimeFailure expected_error -> do
      let typeChecker = configTypeChecker progs
      context ("Type-checking with " <> T.pack typeChecker) $ do
        (code, _, err) <-
          io $ readProcessWithExitCode typeChecker ["-t", program] ""
        case code of
         ExitSuccess -> throwError "Expected failure\n"
         ExitFailure 127 -> throwError $ progNotFound $ T.pack typeChecker
         ExitFailure 1 -> throwError err
         ExitFailure _ -> checkError expected_error err

    RunCases _ | mode == OnlyTypeCheck -> do
      let typeChecker = configTypeChecker progs
      context ("Type-checking with " <> T.pack typeChecker) $ do
        (code, _, err) <-
          io $ readProcessWithExitCode typeChecker ["-t", program] ""
        case code of
         ExitSuccess -> return ()
         ExitFailure 127 -> throwError $ progNotFound $ T.pack typeChecker
         ExitFailure _ -> throwError err

    RunCases ios -> do
      -- Compile up-front and reuse same executable for several entry points.
      let compiler = configCompiler progs
      unless (mode == OnlyInterpret) $
        context ("Compiling with " <> T.pack compiler) $
        compileTestProgram compiler program

      mapM_ runInputOutputs ios

  where
    runInputOutputs (InputOutputs entry run_cases) =
      forM_ run_cases $ \run -> context ("Entry point: " <> entry <> "; dataset: " <>
                                         T.pack (runDescription run)) $ do
        let interpreter = configInterpreter progs
        unless (mode == OnlyCompile) $
          context ("Interpreting with " <> T.pack interpreter) $
            interpretTestProgram interpreter program entry run

        context "Running compiled program" $
          runCompiledTestProgram extra_options program entry run

checkError :: ExpectedError -> T.Text -> TestM ()
checkError (ThisError regex_s regex) err
  | not (match regex $ T.unpack err) =
     throwError $ "Expected error:\n  " <> regex_s <>
     "\nGot error:\n  " <> err
checkError _ _ =
  return ()

runResult :: FilePath -> ExitCode -> T.Text -> T.Text -> TestM RunResult
runResult program ExitSuccess stdout_s _ =
  case valuesFromText "stdout" stdout_s of
    Left e   -> do
      actual <- io $ writeOutFile program "actual" stdout_s
      throwError $ T.pack (show e) <> "\n(See " <> T.pack actual <> ")"
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
    io $ readProcessWithExitCode futharki ["-e", T.unpack entry, program] input
  case code of
    ExitFailure 127 ->
      throwError $ progNotFound $ T.pack futharki
    _               ->
      compareResult program expectedResult' =<< runResult program code output err
  where dir = takeDirectory program

compileTestProgram :: String -> FilePath -> TestM ()
compileTestProgram futharkc program = do
  (futcode, _, futerr) <-
    io $ readProcessWithExitCode futharkc
    [program, "-o", binOutputf] ""
  case futcode of
    ExitFailure 127 -> throwError $ progNotFound $ T.pack futharkc
    ExitFailure _   -> throwError futerr
    ExitSuccess     -> return ()
  where binOutputf = program `replaceExtension` "bin"

runCompiledTestProgram :: [String] -> String -> T.Text -> TestRun -> TestM ()
runCompiledTestProgram extra_options program entry (TestRun _ inputValues expectedResult _) = do
  input <- getValuesText dir inputValues
  expectedResult' <- getExpectedResult dir expectedResult
  -- Explicitly prefixing the current directory is necessary for
  -- readProcessWithExitCode to find the binary when binOutputf has
  -- no path component.
  let binpath = "." </> binOutputf
      entry_options = ["-e", T.unpack entry]
  context ("Running " <> T.pack (unwords $ binpath : entry_options ++ extra_options)) $ do
    (progCode, output, progerr) <-
      io $ readProcessWithExitCode binpath (entry_options ++ extra_options) input
    withExceptT validating $
      compareResult program expectedResult' =<< runResult program progCode output progerr
  where binOutputf = program `replaceExtension` "bin"
        dir = takeDirectory program
        validating = ("validating test result:\n"<>)

compareResult :: FilePath -> ExpectedResult [Value] -> RunResult
              -> TestM ()
compareResult _ (Succeeds Nothing) SuccessResult{} =
  return ()
compareResult program (Succeeds (Just expectedResult)) (SuccessResult actualResult) =
  case compareValues actualResult expectedResult of
    Just mismatch -> do
      actualf <-
        io $ writeOutFile program "actual" $
        T.unlines $ map prettyText actualResult
      expectedf <-
        io $ writeOutFile program "expected" $
        T.unlines $ map prettyText expectedResult
      throwError $ T.pack actualf <> " and " <> T.pack expectedf <>
        " do not match:\n" <> T.pack (show mismatch)
    Nothing ->
      return ()
compareResult _ (RunTimeFailure expectedError) (ErrorResult _ actualError) =
  checkError expectedError actualError
compareResult _ (Succeeds _) (ErrorResult code err) =
  throwError $ "Program failed with error code " <>
  T.pack (show code) <> " and stderr:\n  " <> err
compareResult _ (RunTimeFailure f) (SuccessResult _) =
  throwError $ "Program succeeded, but expected failure:\n  " <> T.pack (show f)

writeOutFile :: FilePath -> String -> T.Text -> IO FilePath
writeOutFile base ext content =
  attempt (0::Int)
  where template = base `replaceExtension` ext
        attempt i = do
          let filename = template ++ "-" ++ show i
          exists <- doesFileExist filename
          if exists
            then attempt $ i+1
            else do T.writeFile filename content
                    return filename

---
--- Test manager
---

catching :: IO TestResult -> IO TestResult
catching m = m `catch` save
  where save :: SomeException -> IO TestResult
        save e = return $ Failure $ T.pack $ show e

doTest :: TestCase -> IO TestResult
doTest = catching . runTestM . runTestCase

makeTestCase :: TestConfig -> TestMode -> (FilePath, ProgramTest) -> TestCase
makeTestCase config mode (file, spec) =
  TestCase mode file spec (configPrograms config) (configExtraOptions config)

runTest :: MVar TestCase -> MVar (TestCase, TestResult) -> IO ()
runTest testmvar resmvar = forever $ do
  test <- takeMVar testmvar
  res <- doTest test
  putMVar resmvar (test, res)

excludedTest :: TestConfig -> TestCase -> Bool
excludedTest config =
  any (`elem` configExclude config) . testTags . testCaseTest

clearLine :: IO ()
clearLine = putStr "\27[2K"

reportInteractive :: String -> Int -> Int -> Int -> IO ()
reportInteractive first failed passed remaining = do
  clearLine
  putStr $
    "\rWaiting for " ++ first ++ " (" ++
    show failed ++ " failed, " ++
    show passed ++ " passed, " ++
    show remaining ++ " to go.)\r"
  hFlush stdout

reportText :: String -> Int -> Int -> Int -> IO ()
reportText first failed passed remaining =
  putStr $ "Waiting for " ++ first ++ " (" ++
         show failed ++ " failed, " ++
         show passed ++ " passed, " ++
         show remaining ++ " to go.)\n"

runTests :: TestConfig -> [FilePath] -> IO ()
runTests config paths = do
  -- We force line buffering to ensure that we produce running output.
  -- Otherwise, CI tools and the like may believe we are hung and kill
  -- us.
  hSetBuffering stdout LineBuffering

  let mode = configTestMode config
  all_tests <- map (makeTestCase config mode) <$> testSpecsFromPaths paths

  testmvar <- newEmptyMVar
  resmvar <- newEmptyMVar
  concurrency <- getNumCapabilities
  replicateM_ concurrency $ forkIO $ runTest testmvar resmvar

  let (excluded, included) = partition (excludedTest config) all_tests
  _ <- forkIO $ mapM_ (putMVar testmvar) included
  isTTY <- (|| configUnbufferOutput config) <$> hIsTerminalDevice stdout

  let report = if isTTY then reportInteractive else reportText
      clear  = if isTTY then clearLine else putStr "\n"
      getResults remaining failed passed =
        case S.toList remaining of
          []      -> clear >> return (failed, passed)
          first:_ -> do
            report (testCaseProgram first) failed passed $ S.size remaining
            (test, res) <- takeMVar resmvar
            let next = getResults $ test `S.delete` remaining
            case res of
              Success -> next failed (passed+1)
              Failure s -> do clear
                              T.putStrLn (T.pack (testCaseProgram test) <> ":\n" <> s)
                              next (failed+1) passed

  (failed, passed) <- getResults (S.fromList included) 0 0
  let excluded_str = if null excluded
                     then ""
                     else " (" ++ show (length excluded) ++ " excluded)"
  putStrLn $ show failed ++ " failed, " ++ show passed ++ " passed" ++ excluded_str ++ "."
  exitWith $ case failed of 0 -> ExitSuccess
                            _ -> ExitFailure 1

---
--- Configuration and command line parsing
---

data TestConfig = TestConfig
                  { configTestMode :: TestMode
                  , configPrograms :: ProgConfig
                  , configExclude :: [T.Text]
                  , configExtraOptions :: [String]
                  -- ^ Extra options passed to the programs being run.
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
                             }
                           , configExtraOptions = []
                           , configUnbufferOutput = False
                           }

data ProgConfig = ProgConfig
                  { configCompiler :: FilePath
                  , configInterpreter :: FilePath
                  , configTypeChecker :: FilePath
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

data TestMode = OnlyTypeCheck
              | OnlyCompile
              | OnlyInterpret
              | Everything
              deriving (Eq, Show)

commandLineOptions :: [FunOptDescr TestConfig]
commandLineOptions = [
    Option "t" ["only-typecheck"]
    (NoArg $ Right $ \config -> config { configTestMode = OnlyTypeCheck })
    "Only perform type-checking"
  , Option "i" ["only-interpret"]
    (NoArg $ Right $ \config -> config { configTestMode = OnlyInterpret })
    "Only interpret"
  , Option "c" ["only-compile"]
    (NoArg $ Right $ \config -> config { configTestMode = OnlyCompile })
    "Only run compiled code"
  , Option [] ["travis"]
    (NoArg $ Right $ \config -> config { configExclude = T.pack "notravis" :
                                                         configExclude config
                                       , configUnbufferOutput = True
                                       })
    "Only run compiled code not marked notravis"

  , Option [] ["typechecker"]
    (ReqArg (Right . changeProgConfig . setTypeChecker)
     "PROGRAM")
    "What to run for type-checking (defaults to 'futhark')."
  , Option [] ["compiler"]
    (ReqArg (Right . changeProgConfig . setCompiler)
     "PROGRAM")
    "What to run for code generation (defaults to 'futhark-c')."
  , Option [] ["interpreter"]
    (ReqArg (Right . changeProgConfig . setInterpreter)
     "PROGRAM")
    "What to run for interpretation (defaults to 'futharki')."
  , Option [] ["exclude"]
    (ReqArg (\tag ->
               Right $ \config ->
               config { configExclude = T.pack tag : configExclude config })
     "TAG")
    "Exclude test programs that define this tag."
  , Option "p" ["pass-option"]
    (ReqArg (\opt ->
               Right $ \config ->
               config { configExtraOptions = opt : configExtraOptions config })
     "OPT")
    "Pass this option to programs being run."
  ]

main :: IO ()
main = mainWithOptions defaultConfig commandLineOptions $ \progs config ->
  Just $ runTests config progs
