#!/usr/bin/env runhaskell

module Main(main) where

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
l0flags = "-sfr"

-- | Number of concurrent l0c instances to run.
concurrency :: Int
concurrency = 8

clearLine :: IO ()
clearLine = putStr "\27[2K"

data TestResult = Success
                | Failure String

data Test = TypeFailure FilePath
          | TypeCheck FilePath
          | Run FilePath FilePath FilePath

failureTest :: FilePath -> IO TestResult
failureTest f = do
  (code, _, err) <- readProcessWithExitCode "l0c" [l0flags, f] ""
  case code of
    ExitSuccess -> return $ Failure "Expected failure\n"
    ExitFailure 1 -> return $ Failure err
    ExitFailure _ -> return Success

compileTest :: FilePath -> IO TestResult
compileTest f = do
  (code, _, err) <- readProcessWithExitCode "l0c" [l0flags, f] ""
  case code of
    ExitSuccess -> return Success
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
    ExitFailure _ -> return $ Failure err
  where expectedOutputf = outputf `replaceExtension` "testout"

compareOutput :: String -> String -> Bool
compareOutput x y = filter (not . isSpace) x == filter (not . isSpace) y

catching :: IO TestResult -> IO TestResult
catching m = m `catch` save
  where save :: SomeException -> IO TestResult
        save e = return $ Failure $ show e

doTest :: Bool -> Test -> IO TestResult
doTest _ (TypeFailure f) = catching $ failureTest f
doTest _ (TypeCheck f) = catching $ compileTest f
doTest True (Run f inputf outputf) = catching $ executeTest f inputf outputf
doTest False (Run f _ _) = catching $ compileTest f

runTest :: Bool -> MVar (FilePath, Test) -> MVar (FilePath, TestResult) -> IO ()
runTest run testmvar resmvar = forever $ do
  (file, test) <- takeMVar testmvar
  res <- doTest run test
  putMVar resmvar (file, res)

makeTest :: FilePath -> IO Test
makeTest f = do
  let infile = f `replaceExtension` "in"
      outfile = f `replaceExtension` "out"
  inexists <- doesFileExist infile
  outexists <- doesFileExist outfile
  return $ case (inexists, outexists) of
             (True, True)  -> Run f infile outfile
             (True, False)    -> TypeCheck f
             _                   -> TypeFailure f

runTests :: Bool -> [FilePath] -> IO ()
runTests run files = do
  testmvar <- newEmptyMVar
  resmvar <- newEmptyMVar
  replicateM_ concurrency $ forkIO $ runTest run testmvar resmvar
  _ <- forkIO $ forM_ files $ \file -> do
         test <- makeTest file
         putMVar testmvar (file, test)
  let getResults :: S.Set FilePath -> Int -> Int -> IO (Int,Int)
      getResults remaining failed passed =
        case S.toList remaining of
          []      -> clearLine >> return (failed, passed)
          first:_ -> do
            clearLine
            putStr $ "\rWaiting for " ++ first ++ " (" ++
                   show failed ++ " failed, " ++
                   show passed ++ " passed, " ++
                   show (S.size remaining) ++ " to go.)\r"
            hFlush stdout
            (file, res) <- takeMVar resmvar
            let next = getResults $ file `S.delete` remaining
            case res of
              Success -> next failed (passed+1)
              Failure s -> do clearLine
                              putStrLn (file ++ ":\n" ++ s)
                              next (failed+1) passed

  (failed, passed) <- getResults (S.fromList files) 0 0
  putStrLn $ show failed ++ " failed, " ++ show passed ++ " passed."
  exitWith $ case failed of 0 -> ExitSuccess
                            _ -> ExitFailure 1

main :: IO ()
main = do
  args <- getArgs
  case args of
    "-t" : args' -> runTests False args'
    _            -> runTests True args
