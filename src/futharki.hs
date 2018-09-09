{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main (main) where

import Control.Monad.Free.Church
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
  s <- newFutharkiState
  let toploop = do
        void $ evalStateT (runExceptT $ runFutharkiM $ forever readEvalPrint) s
        quit <- confirmQuit
        if quit
          then liftIO $ putStrLn "Leaving futharki."
          else toploop
  Haskeline.runInputT Haskeline.defaultSettings toploop

confirmQuit :: Haskeline.InputT IO Bool
confirmQuit = do
  c <- Haskeline.getInputChar "Quit futharki? (y/n) "
  case c of
    Just 'y' -> return True
    Just 'n' -> return False
    _        -> confirmQuit

interpret :: InterpreterConfig -> FilePath -> IO ()
interpret config fp = do
  pr <- loadProgram fp
  env <- case pr of Nothing -> exitFailure
                    Just env -> return env

  let entry = interpreterEntryPoint config
      (tenv, ienv) = futharkiEnv env
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

  r <- runInterpreter' $ I.interpretFunction ienv (qualLeaf fname) inps
  case r of
    Left err -> do hPrint stderr err
                   exitFailure
    Right res ->
      case (I.fromTuple res, isTupleRecord ret) of
        (Just vs, Just ts) -> zipWithM_ putValue vs ts
        _ -> putValue res ret

putValue :: I.Value -> StructType -> IO ()
putValue v t
  | I.isEmptyArray v =
      putStrLn $ "empty(" ++ pretty (stripArray 1 t) ++ ")"
  | otherwise = putStrLn $ pretty v

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

data FutharkiState =
  FutharkiState { futharkiImports :: Imports
                , futharkiNameSource :: VNameSource
                , futharkiCount :: (Int, [Int])
                , futharkiEnv :: (T.Env, I.Ctx)
                }

newFutharkiState :: IO FutharkiState
newFutharkiState = do
  -- Load the builtins through the type checker.
  (_, imports, src) <- badOnLeft =<< runExceptT (readLibrary [])
  -- Then into the interpreter.
  ienv <- foldM (\ctx -> badOnLeft <=< runInterpreter' . I.interpretImport ctx)
          I.initialCtx $ map (fmap fileProg) imports

  -- Then make the prelude available in the type checker.
  (tenv, d) <- badOnLeft $ T.checkDec imports src T.initialEnv $ mkOpen "/futlib/prelude"
  -- Then in the interpreter.
  ienv' <- badOnLeft =<< runInterpreter' (I.interpretDec ienv d)

  return FutharkiState { futharkiImports = imports
                       , futharkiNameSource = src
                       , futharkiCount = (0, [])
                       , futharkiEnv = (tenv, ienv')
                       }
  where badOnLeft (Right x) = return x
        badOnLeft (Left err) = do
          putStrLn "Error when initialising interpreter state:"
          print err
          exitFailure

loadProgram :: FilePath -> IO (Maybe FutharkiState)
loadProgram file = fmap (either (const Nothing) Just) $ runExceptT $ do
  (_, imports, src) <-
    badOnLeft =<< liftIO (runExceptT (readProgram file)
                          `Haskeline.catch` \(err::IOException) ->
                             return (Left (ExternalError (T.pack $ show err))))

  ienv1 <- foldM (\ctx -> badOnLeft' <=< runInterpreter' . I.interpretImport ctx) I.initialCtx $
           map (fmap fileProg) imports
  (tenv1, d1) <- badOnLeft' $ T.checkDec imports src T.initialEnv $
                 mkOpen "/futlib/prelude"
  (tenv2, d2) <- badOnLeft' $ T.checkDec imports src tenv1 $
                 mkOpen $ toPOSIX $ dropExtension file
  ienv2 <- badOnLeft' =<< runInterpreter' (I.interpretDec ienv1 d1)
  ienv3 <- badOnLeft' =<< runInterpreter' (I.interpretDec ienv2 d2)

  return FutharkiState { futharkiImports = imports
                       , futharkiNameSource = src
                       , futharkiCount = (0, [])
                       , futharkiEnv = (tenv2, ienv3)
                       }
  where badOnLeft (Right x) = return x
        badOnLeft (Left err) = do liftIO $ dumpError newFutharkConfig err
                                  throwError ()
        badOnLeft' :: (Show err, MonadIO m) => Either err x -> ExceptT () m x
        badOnLeft' = badOnLeft . either (Left . ExternalError . T.pack . show) Right

getPrompt :: FutharkiM String
getPrompt = do
  (i, is) <- gets futharkiCount
  return $ show (i:is) ++ "> "

mkOpen :: FilePath -> UncheckedDec
mkOpen f = OpenDec (ModImport f NoInfo noLoc) NoInfo noLoc

-- The ExceptT part is just so we have an easy way of ending the L
-- part of the REPL.
newtype FutharkiM a =
  FutharkiM { runFutharkiM :: ExceptT () (StateT FutharkiState (Haskeline.InputT IO)) a }
  deriving (Functor, Applicative, Monad,
            MonadState FutharkiState, MonadIO, MonadError ())

readEvalPrint :: FutharkiM ()
readEvalPrint = do
  prompt <- getPrompt
  line <- inputLine prompt
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
  (i, is) <- gets futharkiCount
  modify $ \s -> s { futharkiCount = (i + 1, is) }
  where inputLine prompt = do
          inp <- FutharkiM $ lift $ lift $ Haskeline.getInputLine prompt
          case inp of
            Just s -> return $ T.pack s
            Nothing -> throwError ()

getIt :: FutharkiM (Imports, VNameSource, T.Env, I.Ctx)
getIt = do
  imports <- gets futharkiImports
  src <- gets futharkiNameSource
  (tenv, ienv) <- gets futharkiEnv
  return (imports, src, tenv, ienv)

onDec :: UncheckedDec -> FutharkiM ()
onDec d = do
  (imports, src, tenv, ienv) <- getIt
  case T.checkDec imports src tenv d of
    Left e -> liftIO $ print e
    Right (tenv', d') -> do
      r <- runInterpreter $ I.interpretDec ienv d'
      case r of
        Left err -> liftIO $ print err
        Right ienv' -> modify $ \s -> s { futharkiEnv = (tenv', ienv') }

onExp :: UncheckedExp -> FutharkiM ()
onExp e = do
  (imports, src, tenv, ienv) <- getIt
  case showErr (T.checkExp imports src tenv e) of
    Left err -> liftIO $ putStrLn err
    Right e' -> do
      r <- runInterpreter $ I.interpretExp ienv e'
      case r of
        Left err -> liftIO $ print err
        Right v -> liftIO $ putStrLn $ pretty v
    where showErr :: Show a => Either a b -> Either String b
          showErr = either (Left . show) Right

runInterpreter :: F I.ExtOp a -> FutharkiM (Either I.InterpreterError a)
runInterpreter m = runF m (return . Right) intOp
  where
    intOp (I.ExtOpError err) =
      return $ Left err
    intOp (I.ExtOpTrace w v c) = do
      liftIO $ putStrLn $ "Trace at " ++ locStr w ++ ": " ++ v
      c
    intOp (I.ExtOpBreak w ctx tenv c) = do
      liftIO $ putStrLn $ "Breaking at " ++ intercalate " -> " (map locStr w) ++ "."
      liftIO $ putStrLn "<Ctrl-d> to continue."
      s <- get
      -- Note the cleverness to preserve the Haskeline session (for
      -- line history and such).
      s' <- FutharkiM $ lift $ lift $
            execStateT (runExceptT $ runFutharkiM $ forever readEvalPrint)
            s { futharkiEnv = (tenv <> fst (futharkiEnv s), ctx)
              , futharkiCount = (0, uncurry (:) (futharkiCount s)) }
      liftIO $ putStrLn "Continuing..."
      put s { futharkiCount = futharkiCount s' }
      c

runInterpreter' :: MonadIO m => F I.ExtOp a -> m (Either I.InterpreterError a)
runInterpreter' m = runF m (return . Right) intOp
  where intOp (I.ExtOpError err) = return $ Left err
        intOp (I.ExtOpTrace w v c) = do
          liftIO $ putStrLn $ "Trace at " ++ locStr w ++ ": " ++ v
          c
        intOp (I.ExtOpBreak _ _ _ c) = c

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
      modify $ \env -> env { futharkiImports = futharkiImports prog_env
                           , futharkiNameSource = futharkiNameSource prog_env
                           , futharkiEnv = futharkiEnv prog_env
                           }
    Nothing -> return ()

typeCommand :: Command
typeCommand e = do
  prompt <- getPrompt
  case parseExp prompt e of
    Left err -> liftIO $ print err
    Right e' -> do
      imports <- gets futharkiImports
      src <- gets futharkiNameSource
      (tenv, _) <- gets futharkiEnv
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
