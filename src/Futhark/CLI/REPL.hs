{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Futhark.CLI.REPL (main) where

import Control.Monad.Free.Church
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
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.IO as T
import NeatInterpolation (text)
import System.Directory
import System.FilePath
import System.Console.GetOpt
import System.IO
import Text.Read (readMaybe)
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
import Futhark.Util (toPOSIX)

import qualified Language.Futhark.Interpreter as I

banner :: String
banner = unlines [
  "|// |\\    |   |\\  |\\   /",
  "|/  | \\   |\\  |\\  |/  /",
  "|   |  \\  |/  |   |\\  \\",
  "|   |   \\ |   |   | \\  \\"
  ]

main :: String -> [String] -> IO ()
main = mainWithOptions interpreterConfig options "options..." run
  where run []     _      = Just repl
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
    Left err -> error $ "Failed to initialise interpreter state: " ++ err
    Right init_state -> Haskeline.runInputT Haskeline.defaultSettings $ toploop init_state

  putStrLn "Leaving 'futhark repl'."

confirmQuit :: Haskeline.InputT IO Bool
confirmQuit = do
  c <- Haskeline.getInputChar "Quit REPL? (y/n) "
  case c of
    Nothing -> return True -- EOF
    Just 'y' -> return True
    Just 'n' -> return False
    _        -> confirmQuit

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

-- | Representation of breaking at a breakpoint, to allow for
-- navigating through the stack frames and such.
data Breaking = Breaking { breakingStack :: NE.NonEmpty I.StackFrame
                         , breakingAt :: Int
                           -- ^ Index of the current breakpoint (with
                           -- 0 being the outermost).
                         }

data FutharkiState =
  FutharkiState { futharkiImports :: Imports
                , futharkiNameSource :: VNameSource
                , futharkiCount :: Int
                , futharkiEnv :: (T.Env, I.Ctx)
                , futharkiBreaking :: Maybe Breaking
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
    Right (_, e') -> do
      r <- runInterpreter $ I.interpretExp ienv e'
      case r of
        Left err -> liftIO $ print err
        Right v -> liftIO $ putStrLn $ pretty v
    where showErr :: Show a => Either a b -> Either String b
          showErr = either (Left . show) Right

prettyBreaking :: Breaking -> String
prettyBreaking b =
  prettyStacktrace (breakingAt b) $ map locStr $ NE.toList $ breakingStack b

runInterpreter :: F I.ExtOp a -> FutharkiM (Either I.InterpreterError a)
runInterpreter m = runF m (return . Right) intOp
  where
    intOp (I.ExtOpError err) =
      return $ Left err
    intOp (I.ExtOpTrace w v c) = do
      liftIO $ putStrLn $ "Trace at " ++ locStr (srclocOf w) ++ ": " ++ v
      c
    intOp (I.ExtOpBreak callstack c) = do
      s <- get

      let top = NE.head callstack
          ctx = I.stackFrameCtx top
          tenv = I.typeCheckerEnv $ I.ctxEnv ctx
          breaking = Breaking callstack 0

      -- Are we supposed to skip this breakpoint?  Also, We do not
      -- want recursive breakpoints.  It could work fine technically,
      -- but is probably too confusing to be useful.
      unless (isJust (futharkiBreaking s) || locOf top `elem` futharkiSkipBreaks s) $ do
        liftIO $ putStrLn $ "Breakpoint at " ++ locStr top
        liftIO $ putStrLn $ prettyBreaking breaking
        liftIO $ putStrLn "<Enter> to continue."

        -- Note the cleverness to preserve the Haskeline session (for
        -- line history and such).
        (stop, s') <-
          FutharkiM $ lift $ lift $
          runStateT (runExceptT $ runFutharkiM $ forever readEvalPrint)
          s { futharkiEnv = (tenv, ctx)
            , futharkiCount = futharkiCount s + 1
            , futharkiBreaking = Just breaking
            }

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
        intOp (I.ExtOpBreak _ c) = c

type Command = T.Text -> FutharkiM ()

loadCommand :: Command
loadCommand file = do
  loaded <- gets futharkiLoaded
  case (T.null file, loaded) of
    (True, Just loaded') -> throwError $ Load loaded'
    (True, Nothing) -> liftIO $ T.putStrLn "No file specified and no file previously loaded."
    (False, _) -> throwError $ Load $ T.unpack file

genTypeCommand :: (Show err1, Show err2) =>
                  (String -> T.Text -> Either err1 a)
               -> (Imports -> VNameSource -> T.Env -> a -> Either err2 b)
               -> (b -> String)
               -> Command
genTypeCommand f g h e = do
  prompt <- getPrompt
  case f prompt e of
    Left err -> liftIO $ print err
    Right e' -> do
      imports <- gets futharkiImports
      src <- gets futharkiNameSource
      (tenv, _) <- gets futharkiEnv
      case g imports src tenv e' of
        Left err -> liftIO $ print err
        Right x -> liftIO $ putStrLn $ h x

typeCommand :: Command
typeCommand = genTypeCommand parseExp T.checkExp $ \(ps, e) ->
  pretty e <> concatMap ((" "<>) . pretty) ps <>
  " : " <> pretty (typeOf e)

mtypeCommand :: Command
mtypeCommand = genTypeCommand parseModExp T.checkModExp $ pretty . fst

unbreakCommand :: Command
unbreakCommand _ = do
  top <- fmap (NE.head . breakingStack) <$> gets futharkiBreaking
  case top of
    Nothing -> liftIO $ putStrLn "Not currently stopped at a breakpoint."
    Just top' -> do modify $ \s -> s { futharkiSkipBreaks = locOf top' : futharkiSkipBreaks s }
                    throwError Stop

frameCommand :: Command
frameCommand which = do
  maybe_stack <- fmap breakingStack <$> gets futharkiBreaking
  case (maybe_stack, readMaybe $ T.unpack which) of
    (Just stack, Just i)
      | frame:_ <- NE.drop i stack -> do
          let breaking = Breaking stack i
              ctx = I.stackFrameCtx frame
              tenv = I.typeCheckerEnv $ I.ctxEnv ctx
          modify $ \s -> s { futharkiEnv = (tenv, ctx)
                           , futharkiBreaking = Just breaking
                           }
          liftIO $ putStrLn $ prettyBreaking breaking
    (Just _, _) ->
      liftIO $ putStrLn $ "Invalid stack index: " ++ T.unpack which
    (Nothing, _) ->
      liftIO $ putStrLn "Not stopped at a breakpoint."

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

If the loading succeeds, any expressions entered subsequently can use the
declarations in the source file.

Only one source file can be loaded at a time.  Using the :load command a
second time will replace the previously loaded file.  It will also replace
any declarations entered at the REPL.

|])),
            ("type", (typeCommand, [text|
Show the type of an expression, which must fit on a single line.
|])),
            ("mtype", (mtypeCommand, [text|
Show the type of a module expression, which must fit on a single line.
|])),
            ("unbreak", (unbreakCommand, [text|
Skip all future occurences of the current breakpoint.
|])),
            ("frame", (frameCommand, [text|
While at a break point, jump to another stack frame, whose variables can then
be inspected.  Resuming from the breakpoint will jump back to the innermost
stack frame.
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
Exit REPL.
|]))]
