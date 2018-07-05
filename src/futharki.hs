{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Control.Exception
import Data.Char
import Data.List
import Data.Loc
import Data.Maybe
import Data.Version
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Except
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import NeatInterpolation (text)
import System.FilePath
import System.Exit
import System.Console.GetOpt
import qualified System.Console.Haskeline as Haskeline

import Language.Futhark
import Language.Futhark.Parser
import Language.Futhark.TypeChecker
import Language.Futhark.Semantic
import Futhark.MonadFreshNames
import Futhark.Interpreter
import Futhark.Version
import Futhark.Passes
import Futhark.Compiler
import Futhark.Pipeline
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
  putStrLn $ "Version " ++ showVersion version ++ "."
  putStrLn "Copyright (C) DIKU, University of Copenhagen, released under the ISC license."
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
                   , interpDecs :: [UncheckedDec]
                   , interpNameSource :: VNameSource
                   , interpCount :: Int
                   }

newInterpreterState :: IO InterpreterState
newInterpreterState = do
  res <- runExceptT $ readLibrary preludeBasis []
  case res of
    Right (_, imports, src) ->
      return InterpreterState { interpImports = imports
                              , interpDecs = map mkOpen $ basisRoots preludeBasis
                              , interpNameSource = src
                              , interpCount = 0
                              }
    Left err -> do
      putStrLn "Error when initialising interpreter state:"
      print err
      exitFailure

getPrompt :: FutharkiM String
getPrompt = do
  i <- gets interpCount
  return $ "[" ++ show i ++ "]"

mkOpen :: FilePath -> UncheckedDec
mkOpen f = OpenDec (ModImport f NoInfo noLoc) [] NoInfo noLoc

type FutharkiM = StateT InterpreterState (Haskeline.InputT IO)

readEvalPrint :: FutharkiM ()
readEvalPrint = do
  i <- gets interpCount
  prompt <- getPrompt
  modify $ \s -> s { interpCount = i + 1 }
  line <- inputLine $ "[" ++ show i ++ "]> "
  case T.uncons line of
    Just (':', command) -> do
      let (cmdname, rest) = T.break isSpace command
          arg = T.dropWhileEnd isSpace $ T.dropWhile isSpace rest
      case filter ((cmdname `T.isPrefixOf`) . fst) commands of
        [] -> liftIO $ T.putStrLn $ "Unknown command '" <> cmdname <> "'"
        [(_, (cmdf, _))] -> cmdf arg
        matches -> liftIO $ T.putStrLn $ "Ambiguous command; could be one of " <>
                   mconcat (intersperse ", " (map fst matches))
    _ -> do
      -- Read a declaration or expression.
      maybe_dec_or_e <- parseDecOrExpIncrM (inputLine "  ") prompt line

      case maybe_dec_or_e of
        Left err -> liftIO $ print err
        Right (Left d) -> onDec d
        Right (Right e) -> onExp e
  where inputLine prompt = do
          inp <- lift $ Haskeline.getInputLine prompt
          case inp of
            Just s -> return $ T.pack s
            Nothing -> liftIO $ do putStrLn "Leaving futharki."
                                   exitSuccess

onDec :: UncheckedDec -> FutharkiM ()
onDec d = do
  prompt <- getPrompt
  decs <- gets interpDecs
  -- See first if it type checks.
  let decs' = decs ++ [d]
      prog = Prog Nothing decs'
      include = mkInitialImport prompt
  -- We have to read in any new imports done by the declaration.
  basis <- curBasis
  res <- runExceptT $ readImports basis $
         map (uncurry $ mkImportFrom include) $
         progImports $ Prog Nothing [d]
  case res of
    Left err -> liftIO $ print err
    Right (_, imports, src) ->
      case checkProg imports src include prog of
        Left err -> liftIO $ print err
        Right (fm, _, src') -> do
          liftIO $ mapM_ putStrLn $ mapMaybe reportDec $ progDecs $ fileProg fm
          modify $ \s ->
            s { interpDecs = decs ++ [mkOpen prompt]
              , interpNameSource = src'
              , interpImports = imports ++ [(includeToString include, fm)] }

  where curBasis = do
          imports <- gets interpImports
          src <- gets interpNameSource
          return $ Basis { basisImports = imports
                         , basisNameSource = src
                         , basisRoots = basisRoots preludeBasis }

        reportDec (ValDec vb) =
          Just $ baseString (valBindName vb) <> " : " <>
          pretty (unInfo $ valBindRetType vb)
        reportDec (TypeDec tb) =
          Just $ pretty tb
        reportDec _ = Nothing

onExp :: UncheckedExp -> FutharkiM ()
onExp e = do
  imports <- gets interpImports
  decs <- gets interpDecs
  src <- gets interpNameSource
  -- Generate a 0-ary function with empty name with the expression as
  -- its body, append it to the stored program, then run it.
  let prog' = mkProg decs e
  void $ runProgram imports src prog'

mkProg :: [UncheckedDec] -> UncheckedExp -> UncheckedProg
mkProg decs e =
  let mainfun = ValBind { valBindEntryPoint = True
                        , valBindName = nameFromString ""
                        , valBindRetType = NoInfo
                        , valBindRetDecl = Nothing
                        , valBindTypeParams = []
                        , valBindParams = []
                        , valBindBody = e
                        , valBindLocation = noLoc
                        , valBindDoc = Nothing
                        }
  in Prog Nothing $ decs ++ [ValDec mainfun]

runProgram :: Imports -> VNameSource -> UncheckedProg -> FutharkiM Bool
runProgram imports src prog = liftIO $
  case checkProg imports src (mkInitialImport "") prog of
    Left err -> print err >> return False
    Right (imp, _, src') ->
      case evalState (internaliseProg True $ imports ++ [("", imp)]) src' of
        Left err -> print err >> return False
        Right prog'' ->
          case runFun (nameFromString "") [] prog'' of
            Left err -> print err >> return False
            Right vs -> mapM_ (putStrLn . pretty) vs >> return True

type Command = T.Text -> FutharkiM ()

commands :: [(T.Text, (Command, T.Text))]
commands = [("load", (loadCommand, [text|
Load a Futhark source file.  Usage:

  > :load foo.fut

If the loading succeeds, any subsequentialy entered expressions entered
subsequently will have access to the definition (such as function definitions)
in the source file.

Only one source file can be loaded at a time.  Using the :load command a
second time will replace the previously loaded file.  It will also replace
any declarations entered at the REPL.

|])),
            ("type", (typeCommand, [text|
Show the type of an expression.
|])),
            ("help", (helpCommand, [text|
Print a list of commands and a description of their behaviour.
|])),
            ("quit", (quitCommand, [text|
Quit futharki.
|]))]

loadCommand :: Command
loadCommand file = do
  liftIO $ T.putStrLn $ "Reading " <> file
  res <- liftIO $ runExceptT (readProgram preludeBasis (T.unpack file))
         `Haskeline.catch` \(err::IOException) ->
         return (Left (ExternalError (T.pack $ show err)))
  case res of
    Left err -> liftIO $ dumpError newFutharkConfig err
    Right (_, imports, src) ->
      modify $ \env -> env { interpImports = imports
                           , interpDecs = map mkOpen $ basisRoots preludeBasis ++
                                          [dropExtension $ T.unpack file]
                           , interpNameSource = src
                           }

typeCommand :: Command
typeCommand e = do
  prompt <- getPrompt
  case parseExp prompt e of
    Left err -> liftIO $ print err
    Right e' -> do
      imports <- gets interpImports
      src <- gets interpNameSource
      decs <- gets interpDecs
      case checkExp imports src decs e' of
        Left err -> liftIO $ print err
        Right e'' -> liftIO $ putStrLn $ pretty e' <> " : " <> pretty (typeOf e'')

helpCommand :: Command
helpCommand _ = liftIO $ forM_ commands $ \(cmd, (_, desc)) -> do
    T.putStrLn $ ":" <> cmd
    T.putStrLn $ T.replicate (1+T.length cmd) "-"
    T.putStr desc
    T.putStrLn ""
    T.putStrLn ""

quitCommand :: Command
quitCommand _ = liftIO exitSuccess
