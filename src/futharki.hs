{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Control.Exception
import Data.Array
import Data.Char
import Data.List
import Data.Loc
import Data.Version
import qualified Data.Map as M
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
import System.IO
import qualified System.Console.Haskeline as Haskeline

import Language.Futhark
import Language.Futhark.Parser
import qualified Language.Futhark.TypeChecker as T
import qualified Language.Futhark.Semantic as T
import Futhark.MonadFreshNames
import Futhark.Version
import Futhark.Compiler
import Futhark.Pipeline
import Futhark.Util.Options
import Futhark.Util (toPOSIX)

import qualified Language.Futhark.Interpreter as I

banner :: String
banner = unlines [
  "|// |\\    |   |\\  |\\   /",
  "|/  | \\   |\\  |\\  |/  /",
  "|   |  \\  |/  |   |\\  \\",
  "|   |   \\ |   |   | \\  \\"
  ]

main :: IO ()
main = reportingIOErrors $
       mainWithOptions interpreterConfig options "options... program" run
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
interpret config fp = do
  pr <- loadProgram fp
  env <- case pr of Nothing -> exitFailure
                    Just env -> return env

  let entry = interpreterEntryPoint config
      (tenv, ienv) = interpEnv env
  vr <- parseValues "stdin" <$> T.getContents

  inps <-
    case vr of
      Left err -> do
        hPutStrLn stderr $ "Error when reading input: " ++ show err
        exitFailure
      Right vs
        | Just vs' <- mapM convertValue vs ->
            return vs'
        | otherwise -> do
            hPutStrLn stderr "Error when reading input: irregular array."
            exitFailure

  (fname, ret) <-
    case M.lookup (T.Term, entry) $ T.envNameMap tenv of
      Just fname
        | Just (T.BoundV _ t) <- M.lookup (qualLeaf fname) $ T.envVtable tenv ->
            return (fname, snd $ unfoldFunType t)
      _ -> do hPutStrLn stderr $ "Invalid entry point: " ++ pretty entry
              exitFailure

  let putValue v t
        | I.isEmptyArray v =
            putStrLn $ "empty(" ++ pretty (stripArray 1 t) ++ ")"
        | otherwise = putStrLn $ pretty v

  case I.interpretFunction ienv (qualLeaf fname) inps of
    Left err -> do hPrint stderr err
                   exitFailure
    Right res ->
      case (I.fromTuple res, isTupleRecord ret) of
        (Just vs, Just ts) -> zipWithM_ putValue vs ts
        _ -> putValue res ret

convertValue :: Value -> Maybe I.Value
convertValue (PrimValue p) = Just $ I.ValuePrim p
convertValue (ArrayValue arr _) = I.mkArray =<< mapM convertValue (elems arr)

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
                   , interpCount :: Int
                   , interpEnv :: (T.Env, I.Ctx)
                   }

newInterpreterState :: IO InterpreterState
newInterpreterState = do
  -- Load the builtins through the type checker.
  (_, imports, src) <- badOnLeft =<< runExceptT (readLibrary [])
  -- Then into the interpreter.
  ienv <- badOnLeft $ foldM I.interpretImport I.initialCtx $ map (fmap fileProg) imports

  -- Then make the prelude available in the type checker.
  (tenv, d) <- badOnLeft $ T.checkDec imports src T.initialEnv $ mkOpen "/futlib/prelude"
  -- Then in the interpreter.
  ienv' <- badOnLeft $ I.interpretDec ienv d

  return InterpreterState { interpImports = imports
                          , interpNameSource = src
                          , interpCount = 0
                          , interpEnv = (tenv, ienv')
                          }
  where badOnLeft (Right x) = return x
        badOnLeft (Left err) = do
          putStrLn "Error when initialising interpreter state:"
          print err
          exitFailure

loadProgram :: FilePath -> IO (Maybe InterpreterState)
loadProgram file = fmap (either (const Nothing) Just) $ runExceptT $ do
  (_, imports, src) <-
    badOnLeft =<< liftIO (runExceptT (readProgram file)
                          `Haskeline.catch` \(err::IOException) ->
                             return (Left (ExternalError (T.pack $ show err))))

  ienv1 <- badOnLeft' $ foldM I.interpretImport I.initialCtx $ map (fmap fileProg) imports
  (tenv1, d1) <- badOnLeft' $ T.checkDec imports src T.initialEnv $
                 mkOpen "/futlib/prelude"
  (tenv2, d2) <- badOnLeft' $ T.checkDec imports src tenv1 $
                 mkOpen $ toPOSIX $ dropExtension file
  ienv2 <- badOnLeft' $ I.interpretDec ienv1 d1
  ienv3 <- badOnLeft' $ I.interpretDec ienv2 d2

  return InterpreterState { interpImports = imports
                          , interpNameSource = src
                          , interpCount = 0
                          , interpEnv = (tenv2, ienv3)
                          }
  where badOnLeft (Right x) = return x
        badOnLeft (Left err) = do liftIO $ dumpError newFutharkConfig err
                                  throwError ()
        badOnLeft' :: (Show err, MonadIO m) => Either err x -> ExceptT () m x
        badOnLeft' = badOnLeft . either (Left . ExternalError . T.pack . show) Right

getPrompt :: FutharkiM String
getPrompt = do
  i <- gets interpCount
  return $ "[" ++ show i ++ "]"

mkOpen :: FilePath -> UncheckedDec
mkOpen f = OpenDec (ModImport f NoInfo noLoc) NoInfo noLoc

type FutharkiM = StateT InterpreterState (Haskeline.InputT IO)

readEvalPrint :: FutharkiM ()
readEvalPrint = do
  i <- gets interpCount
  prompt <- getPrompt
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
  modify $ \s -> s { interpCount = i + 1 }
  where inputLine prompt = do
          inp <- lift $ Haskeline.getInputLine prompt
          case inp of
            Just s -> return $ T.pack s
            Nothing -> liftIO $ do putStrLn "Leaving futharki."
                                   exitSuccess

getIt :: FutharkiM (Imports, VNameSource, T.Env, I.Ctx)
getIt = do
  imports <- gets interpImports
  src <- gets interpNameSource
  (tenv, ienv) <- gets interpEnv
  return (imports, src, tenv, ienv)

onDec :: UncheckedDec -> FutharkiM ()
onDec d = do
  (imports, src, tenv, ienv) <- getIt
  case T.checkDec imports src tenv d of
    Left e -> liftIO $ print e
    Right (tenv', d') ->
      case I.interpretDec ienv d' of
        Left e -> liftIO $ print e
        Right ienv' -> modify $ \s -> s { interpEnv = (tenv', ienv') }

onExp :: UncheckedExp -> FutharkiM ()
onExp e = do
  (imports, src, tenv, ienv) <- getIt
  case showErr . I.interpretExp ienv =<<
       showErr (T.checkExp imports src tenv e) of
    Left err -> liftIO $ putStrLn err
    Right v -> liftIO $ putStrLn $ pretty v
    where showErr :: Show a => Either a b -> Either String b
          showErr = either (Left . show) Right

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
loadCommand file = void $ runExceptT $ do
  liftIO $ T.putStrLn $ "Reading " <> file
  r <- liftIO $ loadProgram $ T.unpack file
  case r of
    Just prog_env ->
      modify $ \env -> env { interpImports = interpImports prog_env
                           , interpNameSource = interpNameSource prog_env
                           , interpEnv = interpEnv prog_env
                           }
    Nothing -> return ()

typeCommand :: Command
typeCommand e = do
  prompt <- getPrompt
  case parseExp prompt e of
    Left err -> liftIO $ print err
    Right e' -> do
      imports <- gets interpImports
      src <- gets interpNameSource
      (tenv, _) <- gets interpEnv
      case T.checkExp imports src tenv e' of
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
