{-# LANGUAGE OverloadedStrings, TupleSections, FlexibleContexts #-}
-- | This program is a convenience utility for running the Futhark
-- test suite, and its test programs.
module Main ( ProgramTest (..)
            , TestRun (..)
            , TestCase (..)
            , main) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Exception hiding (try)
import Control.Monad.Except
import Data.Char
import Data.List
import Data.Monoid
import Data.Ord
import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.Environment
import System.Process
import System.Exit
import System.IO
import System.FilePath

import Text.Parsec hiding ((<|>), many)
import Text.Parsec.Text
import Text.Parsec.Error
import Text.Regex.TDFA

import Prelude

import Futhark.Representation.AST.Pretty (pretty)
import Futhark.Representation.AST.Syntax.Core
import Futhark.Internalise.TypesValues (internaliseValue)
import qualified Language.Futhark.Parser as F

-- | Number of tests to run concurrently.
concurrency :: Int
concurrency = 8

---
--- Test specification parser
---

-- | Description of a test to be carried out on a Futhark program.
-- The Futhark program is stored separately.
data ProgramTest =
  ProgramTest { testDescription :: T.Text
              , testAction      :: TestAction
              }
  deriving (Show)

data TestAction
  = CompileTimeFailure ExpectedError
  | RunCases [TestRun]
  deriving (Show)

data ExpectedError = AnyError
                   | ThisError T.Text Regex

instance Show ExpectedError where
  show AnyError = "AnyError"
  show (ThisError r _) = "ThisError " ++ show r

data RunMode
  = CompiledOnly
  | InterpretedOnly
  | InterpretedAndCompiled
  deriving (Eq, Show)

data TestRun = TestRun
               { runMode :: RunMode
               , runInput :: Values
               , runExpectedResult :: ExpectedResult Values
               }
             deriving (Show)

data Values = Values [Value]
            | InFile FilePath
            deriving (Show)

data ExpectedResult values
  = Succeeds values
  | RunTimeFailure ExpectedError
  deriving (Show)

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

lexstr :: String -> Parser ()
lexstr = void . lexeme . string

braces :: Parser a -> Parser a
braces p = lexstr "{" *> p <* lexstr "}"

parseDescription :: Parser T.Text
parseDescription = lexeme $ T.pack <$> (anyChar `manyTill` descriptionSeparator)

descriptionSeparator :: Parser ()
descriptionSeparator = try (string "--" >> void newline) <|> eof

parseAction :: Parser TestAction
parseAction = CompileTimeFailure <$> (lexstr "error:" *> parseExpectedError) <|>
              RunCases <$> parseRunCases

parseRunMode :: Parser RunMode
parseRunMode = (lexstr "compiled" *> pure CompiledOnly) <|>
               pure InterpretedAndCompiled

parseRunCases :: Parser [TestRun]
parseRunCases = many $ TestRun <$> parseRunMode <*> parseInput <*> parseExpectedResult

parseExpectedResult :: Parser (ExpectedResult Values)
parseExpectedResult = (Succeeds <$> (lexstr "output" *> parseValues)) <|>
                 (RunTimeFailure <$> (lexstr "error:" *> parseExpectedError))

parseExpectedError :: Parser ExpectedError
parseExpectedError = lexeme $ do
  s <- restOfLine
  if T.all isSpace s
    then return AnyError
         -- blankCompOpt creates a regular expression that treats
         -- newlines like ordinary characters, which is what we want.
    else ThisError s <$> makeRegexOptsM blankCompOpt defaultExecOpt (T.unpack s)

parseInput :: Parser Values
parseInput = lexstr "input" *> parseValues

parseValues :: Parser Values
parseValues = do s <- parseBlock
                 case parseValuesFromString $ T.unpack s of
                   Left err -> fail $ show err
                   Right vs -> return $ Values vs
              <|> lexstr "@" *> lexeme (InFile <$> T.unpack <$> restOfLine)

parseValuesFromString :: String -> Either F.ParseError [Value]
parseValuesFromString s =
  liftM concat $ mapM internalise =<< F.parseValues "input" s
  where internalise v =
          maybe (Left $ F.ParseError $ "Invalid input value: " ++ pretty v) Right $
          internaliseValue v

parseBlock :: Parser T.Text
parseBlock = lexeme $ braces (T.pack <$> parseBlockBody 0)

parseBlockBody :: Int -> Parser String
parseBlockBody n = do
  c <- lookAhead anyChar
  case (c,n) of
    ('}', 0) -> return mempty
    ('}', _) -> (:) <$> anyChar <*> parseBlockBody (n-1)
    ('{', _) -> (:) <$> anyChar <*> parseBlockBody (n+1)
    _        -> (:) <$> anyChar <*> parseBlockBody n

restOfLine :: Parser T.Text
restOfLine = T.pack <$> (anyChar `manyTill` (void newline <|> eof))

testSpec :: Parser ProgramTest
testSpec =
  ProgramTest <$> parseDescription <*> parseAction

readTestSpec :: SourceName -> T.Text -> Either ParseError ProgramTest
readTestSpec = parse $ testSpec <* eof

commentPrefix :: T.Text
commentPrefix = "//"

fixPosition :: ParseError -> ParseError
fixPosition err =
  let newpos = incSourceColumn (errorPos err) $ T.length commentPrefix
  in setErrorPos newpos err

testSpecFromFile :: FilePath -> IO ProgramTest
testSpecFromFile path = do
  s <- T.unlines <$>
       map (T.drop 2) <$>
       takeWhile (commentPrefix `T.isPrefixOf`) <$>
       T.lines <$>
       T.readFile path
  case readTestSpec path s of
    Left err -> error $ show $ fixPosition err
    Right v  -> return v

---
--- Test execution
---

type TestM = ExceptT String IO

runTestM :: TestM () -> IO TestResult
runTestM = liftM (either Failure $ const Success) . runExceptT

io :: IO a -> TestM a
io = liftIO

data TestResult = Success
                | Failure String
                deriving (Eq, Show)

data TestCase = TestCase { testCaseProgram :: FilePath
                         , testCaseTest    :: ProgramTest
                         }
                deriving (Show)

instance Eq TestCase where
  x == y = testCaseProgram x == testCaseProgram y

instance Ord TestCase where
  x `compare` y = testCaseProgram x `compare` testCaseProgram y

data RunResult = ErrorResult Int String
               | SuccessResult [Value]

futharkNotFound :: String
futharkNotFound = "futhark binary not found"

futharkiNotFound :: String
futharkiNotFound = "futharki binary not found"

futharkcNotFound :: String
futharkcNotFound = "futhark-c binary not found"

runTestCase :: TestCase -> TestM ()
runTestCase (TestCase program testcase) =
  case testAction testcase of

    CompileTimeFailure expected_error -> do
      (code, _, err) <- io $ readProcessWithExitCode "futhark" [program] ""
      case code of
        ExitSuccess -> throwError "Expected failure\n"
        ExitFailure 127 -> throwError futharkNotFound
        ExitFailure 1 -> throwError err
        ExitFailure _ -> checkError expected_error err

    RunCases run_cases ->
      mapM_ (executeTestProgram program) run_cases

checkError :: ExpectedError -> String -> TestM ()
checkError (ThisError regex_s regex) err
  | not (match regex err) =
     throwError $ "Expected error:\n  " ++ T.unpack regex_s ++
     "\nGot error:\n  " ++ err
checkError _ _ =
  return ()

executeTestProgram :: FilePath -> TestRun -> TestM ()
executeTestProgram program run = do
  unless (runMode run == CompiledOnly) $
    interpretTestProgram program run
  unless (runMode run == InterpretedOnly) $
    compileTestProgram program run

runResult :: ExitCode -> String -> String -> TestM RunResult
runResult ExitSuccess stdout_s _ =
  case parseValuesFromString stdout_s of
    Left e   -> throwError $ show e
    Right vs -> return $ SuccessResult vs
runResult (ExitFailure code) _ stderr_s =
  return $ ErrorResult code stderr_s

getValues :: MonadIO m => FilePath -> Values -> m [Value]
getValues _ (Values vs) =
  return vs
getValues dir (InFile file) = do
  s <- liftIO $ readFile $ dir </> file
  case parseValuesFromString s of
    Left e   -> fail $ show e
    Right vs -> return vs

getExpectedResult :: MonadIO m =>
                     FilePath -> ExpectedResult Values -> m (ExpectedResult [Value])
getExpectedResult dir (Succeeds vals)      = liftM Succeeds $ getValues dir vals
getExpectedResult _   (RunTimeFailure err) = return $ RunTimeFailure err

interpretTestProgram :: FilePath -> TestRun -> TestM ()
interpretTestProgram program (TestRun _ inputValues expectedResult) =
  withExceptT interpreting $ do
    input <- intercalate "\n" <$> map pretty <$> getValues dir inputValues
    expectedResult' <- getExpectedResult dir expectedResult
    (code, output, err) <- io $ readProcessWithExitCode "futharki" [program] input
    case code of
      ExitFailure 127 ->
        throwError futharkiNotFound
      _               ->
        compareResult program expectedResult' =<< runResult code output err
  where interpreting = ("interpreting:\n"++)
        dir = takeDirectory program

compileTestProgram :: FilePath -> TestRun -> TestM ()
compileTestProgram program (TestRun _ inputValues expectedResult) =
  withExceptT compiling $ do
    input <- intercalate "\n" <$> map pretty <$> getValues dir inputValues
    expectedResult' <- getExpectedResult dir expectedResult
    (futcode, _, futerr) <-
      io $ readProcessWithExitCode "futhark-c"
      [program, "-o", binOutputf] ""
    case futcode of
      ExitFailure 127 -> throwError futharkcNotFound
      ExitFailure _   -> throwError futerr
      ExitSuccess     -> return ()
    (progCode, output, progerr) <-
      io $ readProcessWithExitCode binOutputf [] input
    compareResult program expectedResult' =<< runResult progCode output progerr
  where binOutputf = program `replaceExtension` "bin"
        dir = takeDirectory program

        compiling = ("compiling:\n"++)

compareResult :: FilePath -> ExpectedResult [Value] -> RunResult -> TestM ()
compareResult program (Succeeds expectedResult) (SuccessResult actualResult) =
  unless (compareValues actualResult expectedResult) $ do
    actualf <-
      io $ writeOutFile program "actual" $
      unlines $ map pretty actualResult
    expectedf <-
      io $ writeOutFile program "expected" $
      unlines $ map pretty expectedResult
    throwError $ actualf ++ " and " ++ expectedf ++ " do not match."
compareResult _ (RunTimeFailure expectedError) (ErrorResult _ actualError) =
  checkError expectedError actualError
compareResult _ (Succeeds _) (ErrorResult _ err) =
  throwError $ "Program failed with error:\n  " ++ err
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

compareValues :: [Value] -> [Value] -> Bool
compareValues vs1 vs2
  | length vs1 /= length vs2 = False
  | otherwise = and $ zipWith compareValue vs1 vs2

compareValue :: Value -> Value -> Bool
compareValue (BasicVal bv1) (BasicVal bv2) =
  compareBasicValue bv1 bv2
compareValue (ArrayVal vs1 _ _) (ArrayVal vs2 _ _) =
  A.bounds vs1 == A.bounds vs2 &&
  and (zipWith compareBasicValue (A.elems vs1) (A.elems vs2))
compareValue _ _ =
  False

compareBasicValue :: BasicValue -> BasicValue -> Bool
compareBasicValue (RealVal x) (RealVal y) = abs (x - y) < epsilon
  where epsilon = 0.0001
compareBasicValue x y = x == y

catching :: IO TestResult -> IO TestResult
catching m = m `catch` save
  where save :: SomeException -> IO TestResult
        save e = return $ Failure $ show e

doTest :: TestCase -> IO TestResult
doTest = catching . runTestM . runTestCase

makeTestCase :: TestMode -> FilePath -> IO TestCase
makeTestCase mode file = do
  spec <- applyMode mode <$> testSpecFromFile file
  return $ TestCase file spec

applyMode :: TestMode -> ProgramTest -> ProgramTest
applyMode mode test =
  test { testAction = applyModeToAction mode $ testAction test }

applyModeToAction :: TestMode -> TestAction -> TestAction
applyModeToAction _ a@(CompileTimeFailure {}) =
  a
applyModeToAction OnlyTypeCheck (RunCases _) =
  RunCases []
applyModeToAction mode (RunCases cases) =
  RunCases $ map (applyModeToCase mode) cases

applyModeToCase :: TestMode -> TestRun -> TestRun
applyModeToCase OnlyInterpret run =
  run { runMode = InterpretedOnly }
applyModeToCase OnlyCompile run =
  run { runMode = CompiledOnly }
applyModeToCase _ run =
  run

runTest :: MVar TestCase -> MVar (TestCase, TestResult) -> IO ()
runTest testmvar resmvar = forever $ do
  test <- takeMVar testmvar
  res <- doTest test
  putMVar resmvar (test, res)

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

runTests :: TestMode -> [FilePath] -> IO ()
runTests mode files = do
  testmvar <- newEmptyMVar
  resmvar <- newEmptyMVar
  replicateM_ concurrency $ forkIO $ runTest testmvar resmvar
  tests <- mapM (makeTestCase mode) files
  _ <- forkIO $ mapM_ (putMVar testmvar) tests
  isTTY <- hIsTerminalDevice stdout

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

  (failed, passed) <- getResults (S.fromList tests) 0 0
  putStrLn $ show failed ++ " failed, " ++ show passed ++ " passed."
  exitWith $ case failed of 0 -> ExitSuccess
                            _ -> ExitFailure 1

runProgram :: FilePath -> IO ()
runProgram file = do
  spec <- testSpecFromFile file
  case testAction spec of
    RunCases [run] -> do
      (code, output, err) <-
        readProcessWithExitCode "futharki" [file] =<<
        unlines <$> map pretty <$> getValues dir (runInput run)
      hPutStr stderr err
      putStr output
      exitWith code
    _ ->
      error "No input data"
  where dir = takeDirectory file

data TestMode = OnlyTypeCheck
              | OnlyCompile
              | OnlyInterpret
              | Everything

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--run", program] -> runProgram program
    "-t" : args' -> runTests OnlyTypeCheck args'
    "-c" : args' -> runTests OnlyCompile args'
    "-i" : args' -> runTests OnlyInterpret args'
    _            -> runTests Everything args
