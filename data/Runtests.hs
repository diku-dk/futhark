#!/usr/bin/env runhaskell

module Main(main) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Except
import qualified Data.Array as A

import qualified Data.Set as S
import Data.List

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

-- We need some functions from Futhark itself to preprocess input
-- values.
import Futhark.Internalise.TypesValues (internaliseValue)
import Language.Futhark.Parser (parseValues)
import Futhark.Representation.AST.Pretty (pretty)
import Futhark.Representation.AST.Syntax.Core

futharkFlags :: String
futharkFlags = "-s"

-- | Number of tests to run concurrently.
concurrency :: Int
concurrency = 8

clearLine :: IO ()
clearLine = putStr "\27[2K"

type TestM = ExceptT String IO

runTestM :: TestM () -> IO TestResult
runTestM = liftM (either Failure $ const Success) . runExceptT

io :: IO a -> TestM a
io = liftIO

readValuesFromFile :: FilePath -> TestM [Value]
readValuesFromFile filename = do
  s <- liftIO $ readFile filename
  case parseValues filename s of
    Left e -> throwError $ "When reading data file " ++ filename ++ ": " ++ show e
    Right vs -> return $ concatMap internaliseValue vs

data TestResult = Success
                | Failure String

data Test = TypeFailure FilePath
          | Optimise FilePath
          | Run FilePath FilePath FilePath
          | Compile FilePath FilePath FilePath
            deriving (Eq, Ord)

testDescription :: Test -> String
testDescription (TypeFailure f) = "type failure of " ++ f
testDescription (Optimise f) = "optimisation of " ++ f
testDescription (Run f _ _) = "interpretation of " ++ f
testDescription (Compile f _ _) = "compilation of " ++ f

futharkNotFound :: String
futharkNotFound = "futhark binary not found"

failureTest :: FilePath -> TestM ()
failureTest f = do
  (code, _, err) <- io $ readProcessWithExitCode "futhark" [futharkFlags, f] ""
  case code of
    ExitSuccess -> throwError "Expected failure\n"
    ExitFailure 127 -> throwError futharkNotFound
    ExitFailure 1 -> throwError err
    ExitFailure _ -> return ()

typeCheckTest :: FilePath -> TestM ()
typeCheckTest f = do
  (code, _, err) <- io $ readProcessWithExitCode "futhark" [futharkFlags, f] ""
  case code of
    ExitSuccess -> return ()
    ExitFailure 127 -> throwError futharkNotFound
    ExitFailure _ -> throwError err

executeTest :: FilePath -> FilePath -> FilePath -> TestM ()
executeTest f inputf outputf = do
  input <- (intercalate "\n" . map pretty) <$> readValuesFromFile inputf
  (code, output, err) <- io $ readProcessWithExitCode "futhark" [futharkFlags, "-i", f] input
  io $ writeFile expectedOutputf output
  case code of
    ExitSuccess     -> compareResult outputf expectedOutputf
    ExitFailure 127 -> throwError futharkNotFound
    ExitFailure _   -> throwError err
  where expectedOutputf = outputf `replaceExtension` "interpreterout"

compileTest :: FilePath -> FilePath -> FilePath -> TestM ()
compileTest f inputf outputf = do
  input <- (intercalate "\n" . map pretty) <$> readValuesFromFile inputf
  (futcode, l0prog, l0err) <-
    io $ readProcessWithExitCode "futhark"
    [futharkFlags, "-fs", "--in-place-lowering", "-ae", "--compile-sequential", f] ""
  io $ writeFile cOutputf l0prog
  case futcode of
    ExitFailure 127 -> throwError futharkNotFound
    ExitFailure _   -> throwError l0err
    ExitSuccess     -> return ()
  (gccCode, _, gccerr) <-
    io $ readProcessWithExitCode "gcc"
    [cOutputf, "-o", binOutputf, "-lm", "-O3"] ""
  case gccCode of
    ExitFailure _ -> throwError gccerr
    ExitSuccess   -> return ()
  (progCode, output, progerr) <-
    io $ readProcessWithExitCode binOutputf [] input
  io $ writeFile expectedOutputf output
  case progCode of
    ExitFailure _ -> throwError progerr
    ExitSuccess -> compareResult outputf expectedOutputf
  where cOutputf = outputf `replaceExtension` "c"
        binOutputf = outputf `replaceExtension` "bin"
        expectedOutputf = outputf `replaceExtension` "compilerout"

compareResult :: FilePath -> FilePath -> TestM ()
compareResult outputf expectedOutputf = do
  output <- readValuesFromFile outputf
  expectedOutput <- readValuesFromFile expectedOutputf
  unless (compareValues output expectedOutput) $
    throwError $ outputf ++ " and " ++ expectedOutputf ++ " do not match."

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

doTest :: Test -> IO TestResult
doTest = catching . runTestM . doTest'
  where doTest' (TypeFailure f) = failureTest f
        doTest' (Optimise f) = typeCheckTest f
        doTest' (Run f inputf outputf) = executeTest f inputf outputf
        doTest' (Compile f inputf outputf) = compileTest f inputf outputf

runTest :: MVar Test -> MVar (Test, TestResult) -> IO ()
runTest testmvar resmvar = forever $ do
  test <- takeMVar testmvar
  res <- doTest test
  putMVar resmvar (test, res)

makeTests :: TestMode -> FilePath -> IO [Test]
makeTests mode f = do
  let infile  = f `replaceExtension` "in"
      outfile = f `replaceExtension` "out"
  inexists <- doesFileExist infile
  outexists <- doesFileExist outfile
  return $ case (inexists, outexists, mode) of
             (True, True, OnlyCompile) -> [Compile f infile outfile]
             (True, True, OnlyInterpret) -> [Run f infile outfile]
             (True, True, Everything)  -> [Run f infile outfile,
                                           Compile f infile outfile]
             (True, _, _)              -> [Optimise f]
             _                         -> [TypeFailure f]

reportInteractive :: String -> Int -> Int -> Int -> IO ()
reportInteractive first failed passed remaining = do
  clearLine
  putStr $ "\rWaiting for " ++ first ++ " (" ++
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
  tests <- concat <$> mapM (makeTests mode) files
  _ <- forkIO $ mapM_ (putMVar testmvar) tests
  isTTY <- hIsTerminalDevice stdout
  let report = if isTTY then reportInteractive else reportText
      clear  = if isTTY then clearLine else putStr "\n"
      getResults remaining failed passed =
        case S.toList remaining of
          []      -> clear >> return (failed, passed)
          first:_ -> do
            report (testDescription first) failed passed $ S.size remaining
            (test, res) <- takeMVar resmvar
            let next = getResults $ test `S.delete` remaining
            case res of
              Success -> next failed (passed+1)
              Failure s -> do clear
                              putStrLn (testDescription test ++ ":\n" ++ s)
                              next (failed+1) passed

  (failed, passed) <- getResults (S.fromList tests) 0 0
  putStrLn $ show failed ++ " failed, " ++ show passed ++ " passed."
  exitWith $ case failed of 0 -> ExitSuccess
                            _ -> ExitFailure 1

data TestMode = OnlyTypeCheck
              | OnlyCompile
              | OnlyInterpret
              | Everything

main :: IO ()
main = do
  args <- getArgs
  case args of
    "-t" : args' -> runTests OnlyTypeCheck args'
    "-c" : args' -> runTests OnlyCompile args'
    "-i" : args' -> runTests OnlyInterpret args'
    _            -> runTests Everything args
