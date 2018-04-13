{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Control.Exception
import Data.Char
import Data.Loc
import Data.Version
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Except
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import NeatInterpolation (text)
import System.Exit
import System.Console.GetOpt
import qualified System.Console.Haskeline as Haskeline

import Language.Futhark
import Language.Futhark.Parser
import Language.Futhark.TypeChecker
import Futhark.MonadFreshNames
import Futhark.Pipeline
import Futhark.Interpreter
import Futhark.Version
import Futhark.Passes
import Futhark.Compiler
import Futhark.Internalise
import Futhark.Util.Options
import Language.Futhark.Futlib.Prelude

banner :: String
banner = unlines [
  "|// |\\    |   |\\  |\\   /",
  "|/  | \\   |\\  |\\  |/  /",
  "|   |  \\  |/  |   |\\  \\",
  "|   |   \\ |   |   | \\  \\"
  ]

main :: IO ()
main = reportingIOErrors $
       mainWithOptions interpreterConfig options run
  where run [prog] config = Just $ interpret config prog
        run []     _      = Just repl
        run _      _      = Nothing

repl :: IO ()
repl = do
  putStr banner
  putStrLn $ "Version " ++ showVersion version
  putStrLn "(C) HIPERFIT research centre"
  putStrLn "Department of Computer Science, University of Copenhagen (DIKU)"
  putStrLn ""
  putStrLn "Run :help for a list of commands."
  putStrLn ""
  s <- newInterpreterState
  Haskeline.runInputT Haskeline.defaultSettings
    (evalStateT (forever readEvalPrint) s)

interpret :: InterpreterConfig -> FilePath -> IO ()
interpret config =
  runCompilerOnProgram newFutharkConfig preludeBasis standardPipeline $
  interpretAction' $ interpreterEntryPoint config

newtype InterpreterConfig = InterpreterConfig { interpreterEntryPoint :: Name }

interpreterConfig :: InterpreterConfig
interpreterConfig = InterpreterConfig defaultEntryPoint

options :: [FunOptDescr InterpreterConfig]
options = [ Option "e" ["entry-point"]
          (ReqArg (\entry -> Right $ \config ->
                      config { interpreterEntryPoint = nameFromString entry })
           "NAME")
            "The entry point to execute."
          ]

data InterpreterState =
  InterpreterState { interpImports :: Imports
                   , interpNameSource :: VNameSource
                   }

newInterpreterState :: IO InterpreterState
newInterpreterState = do
  res <- runExceptT $ readLibrary False preludeBasis mempty []
  case res of
    Right (_, imports, src) ->
      return InterpreterState { interpImports = imports
                              , interpNameSource = src
                              }
    Left err -> do
      putStrLn "Error when initialising interpreter state:"
      print err
      exitFailure

type FutharkiM = StateT InterpreterState (Haskeline.InputT IO)

readEvalPrint :: FutharkiM ()
readEvalPrint = do
  line <- inputLine "> "
  case T.uncons line of
    Just (':', command) -> do
      let (cmdname, rest) = T.break isSpace command
          arg = T.dropWhileEnd isSpace $ T.dropWhile isSpace rest
      case lookup cmdname commands of
        Nothing -> liftIO $ T.putStrLn $ "Unknown command '" <> cmdname <> "'"
        Just (cmdf, _) -> cmdf arg
    _ -> do
      imports <- gets interpImports
      src <- gets interpNameSource
      -- Read an expression.
      maybe_e <- parseExpIncrM (inputLine "  ") "input" line
      case maybe_e of
        Left err -> liftIO $ print err
        Right e -> do
          -- Generate a 0-ary function with empty name with the
          -- expression as its body, append it to the stored program,
          -- then run it.
          let mkOpen f = OpenDec (ModImport f NoInfo noLoc) [] NoInfo noLoc
              opens = map (mkOpen . fst) imports
              mainfun = ValBind { valBindEntryPoint = True
                                , valBindName = nameFromString ""
                                , valBindRetType = NoInfo
                                , valBindRetDecl = Nothing
                                , valBindTypeParams = []
                                , valBindParams = []
                                , valBindBody = e
                                , valBindLocation = noLoc
                                , valBindDoc = Nothing
                                }
              prog' = Prog Nothing $ opens ++ [ValDec mainfun]
          runProgram imports src prog'
  where inputLine prompt = do
          inp <- lift $ Haskeline.getInputLine prompt
          case inp of
            Just s -> return $ T.pack s
            Nothing -> liftIO $ do putStrLn "Leaving futharki."
                                   exitSuccess

runProgram :: Imports -> VNameSource -> UncheckedProg -> FutharkiM ()
runProgram imports src prog = liftIO $
  case checkProg False imports src "" prog of
    Left err -> print err
    Right (imp, _, src') ->
      case evalState (internaliseProg $ imports ++ [("", imp)]) src' of
        Left err -> print err
        Right prog'' ->
          case runFun (nameFromString "") [] prog'' of
            Left err -> print err
            Right vs -> mapM_ (putStrLn . pretty) vs

type Command = T.Text -> FutharkiM ()

commands :: [(T.Text, (Command, T.Text))]
commands = [("load", (loadCommand, [text|
Load a Futhark source file.  Usage:

  > :load foo.fut

If the loading succeeds, any subsequentialy entered expressions entered
subsequently will have access to the definition (such as function definitions)
in the source file.

Only one source file can be loaded at a time.  Using the :load command a
second time will replace the previously loaded file.

|])),
            ("help", (helpCommand, [text|
Print a list of commands and a description of their behaviour.
|])),
            ("quit", (quitCommand, [text|
Quit futharki.
|]))]
  where loadCommand :: Command
        loadCommand file = do
          liftIO $ T.putStrLn $ "Reading " <> file
          res <- liftIO $ runExceptT (readProgram False preludeBasis mempty (T.unpack file))
                 `Haskeline.catch` \(err::IOException) ->
                 return (Left (ExternalError (T.pack $ show err)))
          case res of
            Left err -> liftIO $ dumpError newFutharkConfig err
            Right (_, imports, src) ->
              modify $ \env -> env { interpImports = imports
                                   , interpNameSource = src
                                   }

        helpCommand :: Command
        helpCommand _ = liftIO $ forM_ commands $ \(cmd, (_, desc)) -> do
            T.putStrLn $ ":" <> cmd
            T.putStrLn $ T.replicate (1+T.length cmd) "-"
            T.putStr desc
            T.putStrLn ""
            T.putStrLn ""

        quitCommand :: Command
        quitCommand _ = liftIO exitSuccess
