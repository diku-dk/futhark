{-# LANGUAGE OverloadedStrings, TupleSections, FlexibleContexts #-}
-- | This program is a convenience utility for running the Futhark
-- test suite, and its test programs.
module Main ( ProgramTest (..)
            , TestRun (..)
            , TestCase (..)
            , main) where


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
import qualified Data.HashMap.Lazy as HM
import System.Console.GetOpt
import System.Directory
import System.Process
import System.Exit
import System.IO
import System.FilePath
import Text.Regex.TDFA

import Prelude

import Futhark.Util.Pretty (pretty)
import Futhark.Representation.AST.Syntax.Core hiding (Prim)
import Futhark.Analysis.Metrics
import Futhark.Pipeline
import Futhark.Compiler
import Futhark.Util.Log
import Futhark.Test

import Futhark.Util.Options

--- Test execution

type TestM = ExceptT String IO

runTestM :: TestM () -> IO TestResult
runTestM = fmap (either Failure $ const Success) . runExceptT

io :: IO a -> TestM a
io = liftIO

context :: String -> TestM a -> TestM a
context s = withExceptT ((s ++ ":\n") ++)

data TestResult = Success
                | Failure String
                deriving (Eq, Show)

data TestCase = TestCase { testCaseProgram :: FilePath
                         , testCaseTest :: ProgramTest
                         , testCasePrograms :: ProgConfig
                         }
                deriving (Show)

instance Eq TestCase where
  x == y = testCaseProgram x == testCaseProgram y

instance Ord TestCase where
  x `compare` y = testCaseProgram x `compare` testCaseProgram y

data RunResult = ErrorResult Int String
               | SuccessResult [Value]

progNotFound :: String -> String
progNotFound s = s ++ ": command not found"

optimisedProgramMetrics :: StructurePipeline -> FilePath -> TestM AstMetrics
optimisedProgramMetrics (SOACSPipeline pipeline) program = do
  res <- io $ runFutharkM $ runPipelineOnProgram newFutharkConfig pipeline program
  case res of
    (Left err, msgs) ->
      throwError $ T.unpack $ T.unlines [toText msgs, errorDesc err]
    (Right prog, _) ->
      return $ progMetrics prog
optimisedProgramMetrics (KernelsPipeline pipeline) program = do
  res <- io $ runFutharkM $ runPipelineOnProgram newFutharkConfig pipeline program
  case res of
    (Left err, msgs) ->
      throwError $ T.unpack $ T.unlines [toText msgs, errorDesc err]
    (Right prog, _) ->
      return $ progMetrics prog

testMetrics :: FilePath -> StructureTest -> TestM ()
testMetrics program (StructureTest pipeline expected) = context "Checking metrics" $ do
  actual <- optimisedProgramMetrics pipeline program
  mapM_ (ok actual) $ HM.toList expected
  where ok metrics (name, expected_occurences) =
          case HM.lookup name metrics of
            Nothing
              | expected_occurences > 0 ->
              throwError $ T.unpack name ++ " should have occurred " ++ show expected_occurences ++
              " times, but did not occur at all in optimised program."
            Just actual_occurences
              | expected_occurences /= actual_occurences ->
                throwError $ T.unpack name ++ " should have occurred " ++ show expected_occurences ++
              " times, but occured " ++ show actual_occurences ++ " times."
            _ -> return ()

runTestCase :: TestCase -> TestM ()
runTestCase (TestCase program testcase progs) = do
  forM_ (testExpectedStructure testcase) $ testMetrics program

  case testAction testcase of

    CompileTimeFailure expected_error ->
      forM_ (configTypeCheckers progs) $ \typeChecker ->
        context ("Type-checking with " ++ typeChecker) $ do
          (code, _, err) <-
            io $ readProcessWithExitCode typeChecker [program] ""
          case code of
           ExitSuccess -> throwError "Expected failure\n"
           ExitFailure 127 -> throwError $ progNotFound typeChecker
           ExitFailure 1 -> throwError err
           ExitFailure _ -> checkError expected_error err

    RunCases [] ->
      forM_ (configCompilers progs) $ \compiler ->
      context ("Compiling with " ++ compiler) $
      justCompileTestProgram compiler program

    RunCases run_cases ->
      forM_ run_cases $ \run -> do
        unless (runMode run == CompiledOnly) $
          forM_ (configInterpreters progs) $ \interpreter ->
            context ("Interpreting with " ++ interpreter) $
              interpretTestProgram interpreter program run

        unless (runMode run == InterpretedOnly) $
          forM_ (configCompilers progs) $ \compiler ->
            context ("Compiling with " ++ compiler) $
              compileTestProgram compiler program run

checkError :: ExpectedError -> String -> TestM ()
checkError (ThisError regex_s regex) err
  | not (match regex err) =
     throwError $ "Expected error:\n  " ++ T.unpack regex_s ++
     "\nGot error:\n  " ++ err
checkError _ _ =
  return ()

runResult :: FilePath -> ExitCode -> String -> String -> TestM RunResult
runResult program ExitSuccess stdout_s _ =
  case valuesFromString "stdout" stdout_s of
    Left e   -> do
      actual <- io $ writeOutFile program "actual" stdout_s
      throwError $ show e <> "\n(See " <> actual <> ")"
    Right vs -> return $ SuccessResult vs
runResult _ (ExitFailure code) _ stderr_s =
  return $ ErrorResult code stderr_s

getExpectedResult :: (Functor m, MonadIO m) =>
                     FilePath -> ExpectedResult Values
                  -> m (ExpectedResult [Value])
getExpectedResult dir (Succeeds (Just vals)) = Succeeds <$> Just <$> getValues dir vals
getExpectedResult _   (Succeeds Nothing) = return $ Succeeds Nothing
getExpectedResult _   (RunTimeFailure err) = return $ RunTimeFailure err

interpretTestProgram :: String -> FilePath -> TestRun -> TestM ()
interpretTestProgram futharki program (TestRun _ inputValues expectedResult) = do
  input <- intercalate "\n" <$> map pretty <$> getValues dir inputValues
  expectedResult' <- getExpectedResult dir expectedResult
  (code, output, err) <- io $ readProcessWithExitCode futharki [program] input
  case code of
    ExitFailure 127 ->
      throwError $ progNotFound futharki
    _               ->
      compareResult program expectedResult' =<< runResult program code output err
  where dir = takeDirectory program

compileTestProgram :: String -> FilePath -> TestRun -> TestM ()
compileTestProgram futharkc program (TestRun _ inputValues expectedResult) = do
  input <- getValuesString dir inputValues
  expectedResult' <- getExpectedResult dir expectedResult
  (futcode, _, futerr) <-
    io $ readProcessWithExitCode futharkc
    [program, "-o", binOutputf] ""
  case futcode of
    ExitFailure 127 -> throwError $ progNotFound futharkc
    ExitFailure _   -> throwError futerr
    ExitSuccess     -> return ()
  -- Explicitly prefixing the current directory is necessary for
  -- readProcessWithExitCode to find the binary when binOutputf has
  -- no path component.
  (progCode, output, progerr) <-
    io $ readProcessWithExitCode ("." </> binOutputf) [] input
  withExceptT validating $
    compareResult program expectedResult' =<< runResult program progCode output progerr
  where binOutputf = program `replaceExtension` "bin"
        dir = takeDirectory program
        validating = ("validating test result:\n"++)

justCompileTestProgram :: String -> FilePath -> TestM ()
justCompileTestProgram futharkc program =
  withExceptT compiling $ do
    (futcode, _, futerr) <-
      io $ readProcessWithExitCode futharkc
      [program, "-o", binOutputf] ""
    case futcode of
      ExitFailure 127 -> throwError $ progNotFound futharkc
      ExitFailure _   -> throwError futerr
      ExitSuccess     -> return ()
  where binOutputf = program `replaceExtension` "bin"

        compiling = ("compiling:\n"++)

compareResult :: FilePath -> ExpectedResult [Value] -> RunResult
              -> TestM ()
compareResult _ (Succeeds Nothing) SuccessResult{} =
  return ()
compareResult program (Succeeds (Just expectedResult)) (SuccessResult actualResult) =
  case compareValues actualResult expectedResult of
    Just mismatch -> do
      actualf <-
        io $ writeOutFile program "actual" $
        unlines $ map pretty actualResult
      expectedf <-
        io $ writeOutFile program "expected" $
        unlines $ map pretty expectedResult
      throwError $ actualf ++ " and " ++ expectedf ++ " do not match:\n" ++ show mismatch
    Nothing ->
      return ()
compareResult _ (RunTimeFailure expectedError) (ErrorResult _ actualError) =
  checkError expectedError actualError
compareResult _ (Succeeds _) (ErrorResult code err) =
  throwError $ "Program failed with error code " ++ show code ++ " and stderr:\n  " ++ err
compareResult _ (RunTimeFailure f) (SuccessResult _) =
  throwError $ "Program succeeded, but expected failure:\n  " ++ show f

writeOutFile :: FilePath -> String -> String -> IO FilePath
writeOutFile base ext content =
  attempt (0::Int)
  where template = base `replaceExtension` ext
        attempt i = do
          let filename = template ++ "-" ++ show i
          exists <- doesFileExist filename
          if exists
            then attempt $ i+1
            else do writeFile filename content
                    return filename

---
--- Test manager
---

catching :: IO TestResult -> IO TestResult
catching m = m `catch` save
  where save :: SomeException -> IO TestResult
        save e = return $ Failure $ show e

doTest :: TestCase -> IO TestResult
doTest = catching . runTestM . runTestCase

makeTestCase :: ProgConfig -> TestMode -> FilePath -> IO TestCase
makeTestCase progs mode file = do
  spec <- applyMode mode <$> testSpecFromFile file
  return $ TestCase file spec progs

applyMode :: TestMode -> ProgramTest -> ProgramTest
applyMode mode test =
  test { testAction = applyModeToAction mode $ testAction test }

applyModeToAction :: TestMode -> TestAction -> TestAction
applyModeToAction _ a@CompileTimeFailure{} =
  a
applyModeToAction OnlyTypeCheck (RunCases _) =
  RunCases []
applyModeToAction mode (RunCases cases) =
  RunCases $ mapMaybe (applyModeToCase mode) cases

applyModeToCase :: TestMode -> TestRun -> Maybe TestRun
applyModeToCase OnlyInterpret run =
  Just run { runMode = InterpretedOnly }
applyModeToCase OnlyCompile run =
  Just run { runMode = CompiledOnly }
applyModeToCase OnTravis run | runMode run == NoTravis =
  Nothing
applyModeToCase _ run =
  Just run

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
runTests config files = do
  let mode = configTestMode config
  testmvar <- newEmptyMVar
  resmvar <- newEmptyMVar
  concurrency <- getNumCapabilities
  replicateM_ concurrency $ forkIO $ runTest testmvar resmvar
  all_tests <- mapM (makeTestCase (configPrograms config) mode) files
  let (excluded, included) = partition (excludedTest config) all_tests
  _ <- forkIO $ mapM_ (putMVar testmvar) included
  isTTY <- (&& mode /= OnTravis) <$> hIsTerminalDevice stdout

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
                              putStrLn (testCaseProgram test ++ ":\n" ++ s)
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
                  }

defaultConfig :: TestConfig
defaultConfig = TestConfig { configTestMode = Everything
                           , configExclude = [ "disable" ]
                           , configPrograms =
                             ProgConfig
                             { configCompiler = Left "futhark-c"
                             , configInterpreter = Left "futharki"
                             , configTypeChecker = Left "futhark"
                             }
                           }

data ProgConfig = ProgConfig
                  { configCompiler :: Either FilePath [FilePath]
                  , configInterpreter :: Either FilePath [FilePath]
                  , configTypeChecker :: Either FilePath [FilePath]
                  }
                  deriving (Show)

changeProgConfig :: (ProgConfig -> ProgConfig) -> TestConfig -> TestConfig
changeProgConfig f config = config { configPrograms = f $ configPrograms config }

configCompilers :: ProgConfig -> [FilePath]
configCompilers = either pure id . configCompiler

configInterpreters :: ProgConfig -> [FilePath]
configInterpreters = either pure id . configInterpreter

configTypeCheckers :: ProgConfig -> [FilePath]
configTypeCheckers = either pure id . configTypeChecker

addCompiler :: FilePath -> ProgConfig -> ProgConfig
addCompiler compiler config = case configCompiler config of
  Left _ -> config { configCompiler = Right [compiler] }
  Right existing -> config { configCompiler = Right $ compiler : existing }

addInterpreter :: FilePath -> ProgConfig -> ProgConfig
addInterpreter interpreter config = case configInterpreter config of
  Left _ -> config { configInterpreter = Right [interpreter] }
  Right existing -> config { configInterpreter = Right $ interpreter : existing }

addTypeChecker :: FilePath -> ProgConfig -> ProgConfig
addTypeChecker typeChecker config = case configTypeChecker config of
  Left _ -> config { configTypeChecker = Right [typeChecker] }
  Right existing -> config { configTypeChecker = Right $ typeChecker : existing }

data TestMode = OnlyTypeCheck
              | OnlyCompile
              | OnlyInterpret
              | OnTravis
              | Everything
              deriving (Eq)

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
    (NoArg $ Right $ \config -> config { configTestMode = OnTravis
                                       , configExclude = T.pack "notravis" :
                                                         configExclude config })
    "Only run compiled code not marked notravis"

  , Option [] ["typechecker"]
    (ReqArg (Right . changeProgConfig . addTypeChecker)
     "PROGRAM")
    "What to run for type-checking (defaults to 'futhark')."
  , Option [] ["compiler"]
    (ReqArg (Right . changeProgConfig . addCompiler)
     "PROGRAM")
    "What to run for code generation (defaults to 'futhark-c')."
  , Option [] ["interpreter"]
    (ReqArg (Right . changeProgConfig . addInterpreter)
     "PROGRAM")
    "What to run for interpretation (defaults to 'futharki')."
  , Option [] ["exclude"]
    (ReqArg (\tag ->
               Right $ \config ->
               config { configExclude = T.pack tag : configExclude config })
     "TAG")
    "Exclude test programs that define this tag."
  ]

main :: IO ()
main = mainWithOptions defaultConfig commandLineOptions $ \progs config ->
  Just $ runTests config progs
