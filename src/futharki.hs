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
import Data.Maybe
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
import System.Directory
import System.FilePath
import System.Exit
import System.Console.GetOpt
import System.IO
import qualified System.Console.Haskeline as Haskeline

import Language.Futhark
import Language.Futhark.Parser hiding (EOF)
import qualified Language.Futhark.TypeChecker as T
import qualified Language.Futhark.Semantic as T
import Futhark.MonadFreshNames
import Futhark.Version
import Futhark.Compiler
import Futhark.Pipeline
import Futhark.Util.Options
import Futhark.Util (toPOSIX, maybeHead)

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

data StopReason = EOF | Stop | Exit | Load FilePath

repl :: IO ()
repl = do
  putStr banner
  putStrLn $ "Version " ++ showVersion version ++ "."
  putStrLn "Copyright (C) DIKU, University of Copenhagen, released under the ISC license."
  putStrLn ""
  putStrLn "Run :help for a list of commands."
  putStrLn ""

  let toploop s = do
        (stop, s') <- runStateT (runExceptT $ runFutharkiM $ forever readEvalPrint) s
        case stop of
          Left Stop -> finish s'
          Left EOF -> finish s'
          Left Exit -> finish s'
          Left (Load file) -> do
            liftIO $ T.putStrLn $ "Loading " <> T.pack file
            maybe_new_state <-
              liftIO $ newFutharkiState (futharkiCount s) $ Just file
            case maybe_new_state of
              Right new_state -> toploop new_state
              Left err -> do liftIO $ putStrLn err
                             toploop s'
          Right _ -> return ()

      finish s = do
        quit <- confirmQuit
        if quit then return () else toploop s

  maybe_init_state <- liftIO $ newFutharkiState 0 Nothing
  case maybe_init_state of
    Left err -> error $ "Failed to initialise intepreter state: " ++ err
    Right init_state -> Haskeline.runInputT Haskeline.defaultSettings $ toploop init_state

  putStrLn "Leaving futharki."

confirmQuit :: Haskeline.InputT IO Bool
confirmQuit = do
  c <- Haskeline.getInputChar "Quit futharki? (y/n) "
  case c of
    Nothing -> return True -- EOF
    Just 'y' -> return True
    Just 'n' -> return False
    _        -> confirmQuit

interpret :: InterpreterConfig -> FilePath -> IO ()
interpret config fp = do
  pr <- newFutharkiState 0 $ Just fp
  env <- case pr of Left err -> do hPutStrLn stderr err
                                   exitFailure
                    Right env -> return env

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
            return (fname, toStructural $ snd $ unfoldFunType t)
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

putValue :: I.Value -> TypeBase () () -> IO ()
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
                , futharkiCount :: Int
                , futharkiEnv :: (T.Env, I.Ctx)
                , futharkiBreaking :: Maybe Loc
                  -- ^ Are we currently stopped at a breakpoint?
                , futharkiSkipBreaks :: [Loc]
                -- ^ Skip breakpoints at these locations.
                , futharkiLoaded :: Maybe FilePath
                -- ^ The currently loaded file.
                }

newFutharkiState :: Int -> Maybe FilePath -> IO (Either String FutharkiState)
newFutharkiState count maybe_file = runExceptT $ do
  (imports, src, tenv, ienv) <- case maybe_file of

    Nothing -> do
      -- Load the builtins through the type checker.
      (_, imports, src) <- badOnLeft =<< runExceptT (readLibrary [])
      -- Then into the interpreter.
      ienv <- foldM (\ctx -> badOnLeft <=< runInterpreter' . I.interpretImport ctx)
              I.initialCtx $ map (fmap fileProg) imports

      -- Then make the prelude available in the type checker.
      (tenv, d, src') <- badOnLeft $ T.checkDec imports src T.initialEnv
                         (T.mkInitialImport ".") $ mkOpen "/futlib/prelude"
      -- Then in the interpreter.
      ienv' <- badOnLeft =<< runInterpreter' (I.interpretDec ienv d)
      return (imports, src', tenv, ienv')

    Just file -> do
      (ws, imports, src) <-
        badOnLeft =<< liftIO (runExceptT (readProgram file)
                              `Haskeline.catch` \(err::IOException) ->
                                 return (Left (ExternalError (T.pack $ show err))))
      liftIO $ hPrint stderr ws

      let imp = T.mkInitialImport "."
      ienv1 <- foldM (\ctx -> badOnLeft <=< runInterpreter' . I.interpretImport ctx) I.initialCtx $
               map (fmap fileProg) imports
      (tenv1, d1, src') <- badOnLeft $ T.checkDec imports src T.initialEnv imp $
                           mkOpen "/futlib/prelude"
      (tenv2, d2, src'') <- badOnLeft $ T.checkDec imports src' tenv1 imp $
                            mkOpen $ toPOSIX $ dropExtension file
      ienv2 <- badOnLeft =<< runInterpreter' (I.interpretDec ienv1 d1)
      ienv3 <- badOnLeft =<< runInterpreter' (I.interpretDec ienv2 d2)
      return (imports, src'', tenv2, ienv3)

  return FutharkiState { futharkiImports = imports
                       , futharkiNameSource = src
                       , futharkiCount = count
                       , futharkiEnv = (tenv, ienv)
                       , futharkiBreaking = Nothing
                       , futharkiSkipBreaks = mempty
                       , futharkiLoaded = maybe_file
                       }
  where badOnLeft :: Show err => Either err a -> ExceptT String IO a
        badOnLeft (Right x) = return x
        badOnLeft (Left err) = throwError $ show err

getPrompt :: FutharkiM String
getPrompt = do
  i <- gets futharkiCount
  return $ "[" ++ show i ++ "]> "

mkOpen :: FilePath -> UncheckedDec
mkOpen f = OpenDec (ModImport f NoInfo noLoc) noLoc

-- The ExceptT part is more of a continuation, really.
newtype FutharkiM a =
  FutharkiM { runFutharkiM :: ExceptT StopReason (StateT FutharkiState (Haskeline.InputT IO)) a }
  deriving (Functor, Applicative, Monad,
            MonadState FutharkiState, MonadIO, MonadError StopReason)

readEvalPrint :: FutharkiM ()
readEvalPrint = do
  prompt <- getPrompt
  line <- inputLine prompt
  breaking <- gets futharkiBreaking
  case T.uncons line of
    Nothing
      | isJust breaking -> throwError Stop
      | otherwise -> return ()

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
  modify $ \s -> s { futharkiCount = futharkiCount s + 1 }
  where inputLine prompt = do
          inp <- FutharkiM $ lift $ lift $ Haskeline.getInputLine prompt
          case inp of
            Just s -> return $ T.pack s
            Nothing -> throwError EOF

getIt :: FutharkiM (Imports, VNameSource, T.Env, I.Ctx)
getIt = do
  imports <- gets futharkiImports
  src <- gets futharkiNameSource
  (tenv, ienv) <- gets futharkiEnv
  return (imports, src, tenv, ienv)

onDec :: UncheckedDec -> FutharkiM ()
onDec d = do
  (imports, src, tenv, ienv) <- getIt
  cur_import <- T.mkInitialImport . fromMaybe "." <$> gets futharkiLoaded

  -- Most of the complexity here concerns the dealing with the fact
  -- that 'import "foo"' is a declaration.  We have to involve a lot
  -- of machinery to load this external code before executing the
  -- declaration itself.
  let basis = Basis imports src ["/futlib/prelude"]
      mkImport = uncurry $ T.mkImportFrom cur_import
  imp_r <- runExceptT $ readImports basis (map mkImport $ decImports d)

  case imp_r of
    Left e -> liftIO $ print e
    Right (_, imports',  src') ->
      case T.checkDec imports' src' tenv cur_import d of
        Left e -> liftIO $ print e
        Right (tenv', d', src'') -> do
          let new_imports = filter ((`notElem` map fst imports) . fst) imports'
          int_r <- runInterpreter $ do
            let onImport ienv' (s, imp) =
                  I.interpretImport ienv' (s, T.fileProg imp)
            ienv' <- foldM onImport ienv new_imports
            I.interpretDec ienv' d'
          case int_r of
            Left err -> liftIO $ print err
            Right ienv' -> modify $ \s -> s { futharkiEnv = (tenv', ienv')
                                            , futharkiImports = imports'
                                            , futharkiNameSource = src''
                                            }

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
      s <- get

      -- Are we supposed to skip this breakpoint?
      let loc = maybe noLoc locOf $ maybeHead w

      -- We do not want recursive breakpoints.  It could work fine
      -- technically, but is probably too confusing to be useful.
      unless (isJust (futharkiBreaking s) || loc `elem` futharkiSkipBreaks s) $ do
        liftIO $ putStrLn $ "Breaking at " ++ intercalate " -> " (map locStr w) ++ "."
        liftIO $ putStrLn "<Enter> to continue."

        -- Note the cleverness to preserve the Haskeline session (for
        -- line history and such).
        (stop, s') <-
          FutharkiM $ lift $ lift $
          runStateT (runExceptT $ runFutharkiM $ forever readEvalPrint)
          s { futharkiEnv = (tenv, ctx)
            , futharkiCount = futharkiCount s + 1
            , futharkiBreaking = Just loc }

        case stop of
          Left (Load file) -> throwError $ Load file
          _ -> do liftIO $ putStrLn "Continuing..."
                  put s { futharkiCount = futharkiCount s'
                        , futharkiSkipBreaks = futharkiSkipBreaks s' <> futharkiSkipBreaks s }

      c

runInterpreter' :: MonadIO m => F I.ExtOp a -> m (Either I.InterpreterError a)
runInterpreter' m = runF m (return . Right) intOp
  where intOp (I.ExtOpError err) = return $ Left err
        intOp (I.ExtOpTrace w v c) = do
          liftIO $ putStrLn $ "Trace at " ++ locStr w ++ ": " ++ v
          c
        intOp (I.ExtOpBreak _ _ _ c) = c

type Command = T.Text -> FutharkiM ()

loadCommand :: Command
loadCommand file = do
  loaded <- gets futharkiLoaded
  case (T.null file, loaded) of
    (True, Just loaded') -> throwError $ Load loaded'
    (True, Nothing) -> liftIO $ T.putStrLn "No file specified and no file previously loaded."
    (False, _) -> throwError $ Load $ T.unpack file

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

unbreakCommand :: Command
unbreakCommand _ = do
  breaking <- gets futharkiBreaking
  case breaking of
    Nothing -> liftIO $ putStrLn "Not currently stopped at a breakpoint."
    Just loc -> do modify $ \s -> s { futharkiSkipBreaks = loc : futharkiSkipBreaks s }
                   throwError Stop

pwdCommand :: Command
pwdCommand _ = liftIO $ putStrLn =<< getCurrentDirectory

cdCommand :: Command
cdCommand dir
 | T.null dir = liftIO $ putStrLn "Usage: ':cd <dir>'."
 | otherwise =
    liftIO $ setCurrentDirectory (T.unpack dir)
    `Haskeline.catch` \(err::IOException) -> print err

helpCommand :: Command
helpCommand _ = liftIO $ forM_ commands $ \(cmd, (_, desc)) -> do
    T.putStrLn $ ":" <> cmd
    T.putStrLn $ T.replicate (1+T.length cmd) "-"
    T.putStr desc
    T.putStrLn ""
    T.putStrLn ""

quitCommand :: Command
quitCommand _ = throwError Exit

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
            ("unbreak", (unbreakCommand, [text|
Skip all future occurences of the current breakpoint.
|])),
            ("pwd", (pwdCommand, [text|
Print the current working directory.
|])),
            ("cd", (cdCommand, [text|
Change the current working directory.
|])),
            ("help", (helpCommand, [text|
Print a list of commands and a description of their behaviour.
|])),
            ("quit", (quitCommand, [text|
Quit futharki.
|]))]
