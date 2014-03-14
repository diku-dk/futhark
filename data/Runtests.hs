#!/usr/bin/env runhaskell

module Main(main) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad

import qualified Data.Set as S
import Data.Char (isSpace)

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

l0flags :: String
l0flags = "-sfoe"

-- | Number of tests to run concurrently.
concurrency :: Int
concurrency = 8

clearLine :: IO ()
clearLine = putStr "\27[2K"

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

l0cNotFound :: String
l0cNotFound = "l0c binary not found"

failureTest :: FilePath -> IO TestResult
failureTest f = do
  (code, _, err) <- readProcessWithExitCode "l0c" [l0flags, f] ""
  case code of
    ExitSuccess -> return $ Failure "Expected failure\n"
    ExitFailure 127 -> return $ Failure l0cNotFound
    ExitFailure 1 -> return $ Failure err
    ExitFailure _ -> return Success

typeCheckTest :: FilePath -> IO TestResult
typeCheckTest f = do
  (code, _, err) <- readProcessWithExitCode "l0c" [l0flags, f] ""
  case code of
    ExitSuccess -> return Success
    ExitFailure 127 -> return $ Failure l0cNotFound
    ExitFailure _ -> return $ Failure err

executeTest :: FilePath -> FilePath -> FilePath -> IO TestResult
executeTest f inputf outputf = do
  input <- readFile inputf
  (code, output, err) <- readProcessWithExitCode "l0c" [l0flags, "-i", f] input
  expectedOutput <- readFile outputf
  case code of
    ExitSuccess
      | output `compareOutput` expectedOutput -> return Success
      | otherwise -> do
        writeFile expectedOutputf output
        return $ Failure $ outputf ++ " and " ++ expectedOutputf ++ " do not match."
    ExitFailure 127 -> return $ Failure l0cNotFound
    ExitFailure _   -> return $ Failure err
  where expectedOutputf = outputf `replaceExtension` "interpreterout"

compileTest :: FilePath -> FilePath -> FilePath -> IO TestResult
compileTest f inputf outputf = do
  input <- readFile inputf
  expectedOutput <- readFile outputf
  (l0code, l0prog, l0err) <- readProcessWithExitCode "l0c" [l0flags, "--compile-sequential", f] ""
  writeFile cOutputf l0prog
  case l0code of
    ExitFailure 127 -> return $ Failure l0cNotFound
    ExitFailure _   -> return $ Failure l0err
    ExitSuccess     -> do
      (gccCode, _, gccerr) <- readProcessWithExitCode "gcc" [cOutputf, "-o", binOutputf, "-lm"] ""
      case gccCode of
        ExitFailure _ -> return $ Failure gccerr
        ExitSuccess   -> do
          (progCode, output, progerr) <- readProcessWithExitCode binOutputf [] input
          writeFile cOutputf output
          case progCode of
            ExitFailure _ -> return $ Failure progerr
            ExitSuccess
              | output `compareOutput` expectedOutput -> return Success
              | otherwise -> do
                  writeFile expectedOutputf output
                  return $ Failure $ outputf ++ " and " ++ expectedOutputf ++ " do not match."
  where cOutputf = outputf `replaceExtension` "c"
        binOutputf = outputf `replaceExtension` "bin"
        expectedOutputf = outputf `replaceExtension` "compilerout"

compareOutput :: String -> String -> Bool
compareOutput x y = filter (not . isSpace) x == filter (not . isSpace) y

catching :: IO TestResult -> IO TestResult
catching m = m `catch` save
  where save :: SomeException -> IO TestResult
        save e = return $ Failure $ show e

doTest :: Test -> IO TestResult
doTest (TypeFailure f) = catching $ failureTest f
doTest (Optimise f) = catching $ typeCheckTest f
doTest (Run f inputf outputf) = catching $ executeTest f inputf outputf
doTest (Compile f inputf outputf) = catching $ compileTest f inputf outputf

runTest :: MVar Test -> MVar (Test, TestResult) -> IO ()
runTest testmvar resmvar = forever $ do
  test <- takeMVar testmvar
  res <- doTest test
  putMVar resmvar (test, res)

makeTests :: Bool -> FilePath -> IO [Test]
makeTests run f = do
  let infile  = f `replaceExtension` "in"
      outfile = f `replaceExtension` "out"
  inexists <- doesFileExist infile
  outexists <- doesFileExist outfile
  return $ case (inexists, outexists) of
             (True, True) | run -> [Run f infile outfile,
                                    Compile f infile outfile]
             (True, _)          -> [Optimise f]
             _                  -> [TypeFailure f]

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

runTests :: Bool -> [FilePath] -> IO ()
runTests run files = do
  testmvar <- newEmptyMVar
  resmvar <- newEmptyMVar
  replicateM_ concurrency $ forkIO $ runTest testmvar resmvar
  tests <- concat <$> mapM (makeTests run) files
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

main :: IO ()
main = do
  args <- getArgs
  case args of
    "-t" : args' -> runTests False args'
    _            -> runTests True args
