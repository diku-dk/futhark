-- | L0 Compiler Driver
module Main (main) where

import Control.Applicative
import System.Environment (getArgs)

import L0.AbSyn
import L0.Parser (parseL0)
import L0.TypeChecker
import L0.Renamer
import L0.Interpreter
import L0.EnablingOpts.EnablingOptDriver
import L0.FirstOrderTransform

import Debug.Trace

-- import L0.CCodeGen

main :: IO ()
main = do args <- getArgs
          case args of
            ["-p",   file] -> prettyprint file
            ["-t",   file] -> typecheck (const $ return ()) file
            ["-tp",  file] -> typecheck (putStrLn . prettyPrint) file
            ["-r",   file] -> typecheck rename file
            ["-i",   file] -> interpret file
            ["-tr",  file] -> typecheck (fotransform) file
            ["-cos", file] -> testCosmin file
--            ["-c", file] -> compile file
            _ -> error "Usage: <-p|-t|-tp|-r|-i|-c> <file>"

prettyprint :: FilePath -> IO ()
prettyprint file = putStrLn =<< prettyPrint <$> parse file

typecheck :: (Prog Type -> IO ()) -> FilePath -> IO ()
typecheck next file = do
  prog <- parse file
  case checkProg prog of
    Left e -> error $ show e
    Right prog'  ->
      case checkProg prog' of
        Left e  -> error $ "Error during second type checking phase. This implies a bug in the type checker.\n" ++ show e
        Right _ -> next prog'

fotransform :: Prog Type -> IO ()
fotransform prog = let prog' = transformProg prog
                   in case checkProg $ transformProg prog of
                        Right prog'' -> putStrLn $ prettyPrint prog''
                        Left e -> do putStrLn $ prettyPrint prog'
                                     error $ "Type error after transformation:\n" ++ show e

rename :: Prog Type -> IO ()
rename prog = do let prog' = renameProg prog
                 putStrLn $ prettyPrint prog'
                 case checkProg prog' of
                   Left e -> error $ "Type error after renaming:\n" ++ show e
                   _      -> return ()

interpret :: FilePath -> IO ()
interpret file = do
  prog <- parse file
  case checkProg prog of
    Left err    -> error $ "Typechecking error:\n" ++ show err
    Right prog' -> do
      res <- runProgIO prog''
      case res of Left err -> error $ "Interpreter error:\n" ++ show err
                  Right v  -> return ()


testCosmin :: FilePath -> IO ()
testCosmin file = do
  prog <- parse file
  case checkProg prog of
    Left err    -> error $ "Typechecking error:\n" ++ show err
    Right prog' -> do
      let prog'' = renameProg prog'
      case enablingOpts prog'' of
        Left  err -> error $ "Enabling Optimization Error:\n" ++ show err
        Right prog2 -> do
          _   <- trace ("Opt Program: "++prettyPrint prog2++"\nResult:") (putStrLn "")
          res <- runProgIO prog2
          case res of Left err -> error $ "Interpreter error:\n" ++ show err
                      Right v  -> putStrLn $ ppValue v -- ++ (prettyPrint prog')

{-
compile :: FilePath -> IO ()
compile file = do
  prog <- parse file
  case checkProg prog of
    Left err    -> error $ "Typechecking error:\n" ++ show err
    Right prog' -> putStr $ compileProg $ renameProg prog'
-}

parse :: FilePath -> IO (Prog (Maybe Type))
parse file = either fail return . parseL0 file =<< readFile file
