-- | Facilities for evaluating Futhark code.
--
-- This most directly provides the building blocks for @futhark eval@, but this
-- module also contains server interaction machinery used by @futhark repl@ and
-- other commands that make use of the Futhark interpreter.
module Futhark.Eval
  ( EvalConfig (..),
    InterpreterState,
    runExpr,
    runParsedExpr,
    evalConfig,
    evalServerOptions,
    runFFI,
    forceValue,
    interpretImports,
    initialiseInterpreter,
    newInterpreterState,
    newInterpreterStateWith,
    Evaluation (..),
    EvalRecordRef,
    runEvalRecordRef,
  )
where

import Control.Exception (IOException, catch)
import Control.Monad (foldM, unless, void, when, (<=<))
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.Free.Church (F, runF)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Bifunctor (first)
import Data.Either (isLeft)
import Data.IORef (IORef, modifyIORef')
import Data.Map qualified as M
import Data.Maybe (isJust, maybeToList)
import Data.Sequence (Seq, (|>))
import Data.Text qualified as T
import Futhark.Compiler (prettyWarnings, readProgramFilesExceptKnown)
import Futhark.Compiler.Program (FileModule (..), Imports, VFS, fileScope)
import Futhark.Error (badOnLeft, externalErrorS, prettyCompilerError)
import Futhark.FreshNames (VNameSource)
import Futhark.Server qualified as S
import Futhark.Test (FutharkExe (..), compileProgram, futharkServerCfg)
import Futhark.Util (showText)
import Futhark.Util.Options (ArgDescr (..), FunOptDescr, OptDescr (..))
import Futhark.Util.Pretty (commasep, hPutDoc, hPutDocLn, hardline, putDocLn)
import Language.Futhark.Core (locText)
import Language.Futhark.Interpreter qualified as I
import Language.Futhark.Interpreter.FFI.Push qualified as FFI
import Language.Futhark.Interpreter.FFI.ServerM qualified as FFI
import Language.Futhark.Parser (parseExp)
import Language.Futhark.Parser.Monad (SyntaxError (SyntaxError))
import Language.Futhark.Pretty (toName)
import Language.Futhark.Prop (UncheckedExp, typeOf)
import Language.Futhark.Semantic qualified as T
import Language.Futhark.Syntax (DecBase (ValDec), ProgBase (progDecs), ValBindBase (..), nameToText, typeParamName)
import Language.Futhark.TypeChecker qualified as T
import Prettyprinter (Doc, align, pretty, unAnnotate, vcat, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.FilePath (dropExtension, (</>))
import System.IO (stderr)

-- | The class of monads that can perform expression evaluation.
class (Monad m) => Evaluation m where
  abort :: Doc AnsiStyle -> m b
  trace :: Doc AnsiStyle -> m ()

instance (Evaluation m) => Evaluation (ExceptT (Doc AnsiStyle) m) where
  trace :: Doc AnsiStyle -> ExceptT e m ()
  trace = lift . trace

  abort = throwError

instance Evaluation IO where
  abort reason = do
    hPutDocLn stderr reason
    exitWith $ ExitFailure 1

  trace = putDocLn

newtype EvalRecordRef a
  = EvalRecordRef
      (ExceptT (Doc AnsiStyle) (ReaderT (IORef (Seq (Doc AnsiStyle))) IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance Evaluation EvalRecordRef where
  abort :: Doc AnsiStyle -> EvalRecordRef b
  abort = EvalRecordRef . throwError

  trace :: Doc AnsiStyle -> EvalRecordRef ()
  trace message = EvalRecordRef $ do
    messagesRef <- lift ask
    liftIO $ modifyIORef' messagesRef (|> message)

runEvalRecordRef ::
  IORef (Seq (Doc AnsiStyle)) ->
  EvalRecordRef a ->
  IO (Either (Doc AnsiStyle) a)
runEvalRecordRef msgRef (EvalRecordRef action) =
  flip runReaderT msgRef $ runExceptT action

newtype InterpreterState = InterpreterState (VNameSource, T.Env, I.Ctx, Maybe FFI.Server)

-- | Run an expression in the given interpreter state. The expression is parsed,
-- type checked, and then run. Returns a prettyprinted result. Must be run in a
-- monad that supports aborting and traces.
runExpr ::
  (Evaluation m, MonadIO m) =>
  InterpreterState ->
  T.Text ->
  m (Doc AnsiStyle)
runExpr state str =
  case parseExp "" str of
    Left (SyntaxError _ serr) -> abort $ pretty serr
    Right uexp -> runParsedExpr state uexp

-- | As 'runExpr', but for an expression that has already been parsed -
-- perhaps from somewhere other than a Futhark file.
runParsedExpr ::
  (Evaluation m, MonadIO m) =>
  InterpreterState ->
  UncheckedExp ->
  m (Doc AnsiStyle)
runParsedExpr (InterpreterState (src, env, ctx, s)) uexp = do
  fexp <- case T.checkExp [] src env uexp of
    (_, Left terr) -> do
      abort $ T.prettyTypeError terr
    (_, Right ([], e)) -> pure e
    (_, Right (tparams, e)) ->
      abort $
        vcat
          [ "Inferred type of expression: " <> align (pretty (typeOf e)),
            "The following types are ambiguous: "
              <> commasep (map (pretty . nameToText . toName . typeParamName) tparams)
          ]
  pval <- runInterpreterNoBreak s $ I.interpretExp ctx fexp
  case pval of
    Left err -> abort $ I.prettyInterpreterError err
    Right val -> do
      forced <- liftIO $ forceValue s val
      case forced of
        Left err -> abort $ I.prettyInterpreterError err
        Right val' -> pure $ I.prettyValue val' <> hardline

data EvalConfig = EvalConfig
  { evalPrintWarnings :: Bool,
    evalFile :: Maybe String,
    -- | If @Just@, compile the file using this backend.
    evalBackend :: Maybe String,
    evalSkipCompilation :: Bool,
    evalExtraOptions :: [String],
    evalCompilerOptions :: [String],
    evalFuthark :: Maybe FilePath
  }

evalConfig :: EvalConfig
evalConfig =
  EvalConfig
    { evalPrintWarnings = True,
      evalFile = Nothing,
      evalBackend = Nothing,
      evalSkipCompilation = False,
      evalExtraOptions = [],
      evalCompilerOptions = [],
      evalFuthark = Nothing
    }

-- | Command line options for configuring the use of an external server.
evalServerOptions :: [FunOptDescr EvalConfig]
evalServerOptions =
  [ Option
      "p"
      ["pass-option"]
      ( ReqArg
          ( \opt ->
              Right $ \config ->
                config {evalExtraOptions = opt : evalExtraOptions config}
          )
          "OPT"
      )
      "Pass this option to programs being run.",
    Option
      []
      ["pass-compiler-option"]
      ( ReqArg
          ( \opt ->
              Right $ \config ->
                config {evalCompilerOptions = opt : evalCompilerOptions config}
          )
          "OPT"
      )
      "Pass this option to the compiler.",
    Option
      []
      ["skip-compilation"]
      (NoArg $ Right $ \config -> config {evalSkipCompilation = True})
      "Use already compiled server-mode program.",
    Option
      []
      ["backend"]
      ( ReqArg
          (\backend -> Right $ \config -> config {evalBackend = Just backend})
          "BACKEND"
      )
      "The compiler backend used (defaults to interpreted)."
  ]

-- | Compile the given file and start a server for it. Returns 'Left' on error.
prepareServer :: EvalConfig -> FilePath -> String -> IO (Either T.Text FFI.Server)
prepareServer cfg file backend = runExceptT $ do
  futhark <- liftIO $ maybe getExecutablePath pure $ evalFuthark cfg

  unless (evalSkipCompilation cfg) $
    void $
      compileProgram ("--server" : evalCompilerOptions cfg) (FutharkExe futhark) backend file

  let server_cfg = futharkServerCfg ("." </> dropExtension file) $ evalExtraOptions cfg
  started <-
    liftIO $
      (Right <$> FFI.startServer server_cfg)
        `catch` (\(S.ServerException err) -> pure $ Left err)
        `catch` (\(err :: IOException) -> pure $ Left $ showText err)
  either throwError pure started

-- | Perform an action on the server. A failure is reported as a 'Left'. Calls
-- 'error' if no server is provided.
runFFI ::
  Maybe FFI.Server ->
  FFI.ServerM I.Value ->
  IO (Either I.InterpreterError I.Value)
runFFI Nothing _ = error "External call, but no server."
runFFI (Just server) m =
  first (I.InterpreterError . T.pack) <$> FFI.runServerM server m

-- | Fetch in full a value that may reside on a server, so that it can
-- be printed or otherwise inspected.
forceValue ::
  Maybe FFI.Server ->
  I.Value ->
  IO (Either I.InterpreterError I.Value)
forceValue Nothing v = pure $ Right v
forceValue server v = runFFI server $ FFI.getLazy v

externalise :: FileModule -> FileModule
externalise fm = fm {fileProg = (fileProg fm) {progDecs = map onDec $ progDecs $ fileProg fm}}
  where
    onDec (ValDec vb)
      | isJust $ valBindEntryPoint vb =
          ValDec $ vb {valBindAttrs = "$external" : valBindAttrs vb}
    onDec dec = dec

-- | Mark the entry points of the last import - which is the file the
-- user actually asked us to load - as external, meaning that calls to
-- them are dispatched to a server instead of being interpreted.
externaliseLast :: Imports -> Imports
externaliseLast [] = []
externaliseLast [(k, fm)] = [(k, externalise fm)]
externaliseLast (x : xs) = x : externaliseLast xs

-- | Type check and interpret the given program (if any), producing a state
-- in which expressions can be evaluated with 'runExpr'.
newInterpreterState ::
  (MonadIO m, Evaluation m) =>
  EvalConfig ->
  VFS ->
  m (Either (Doc AnsiStyle) InterpreterState)
newInterpreterState cfg vfs = newInterpreterStateOn cfg vfs Nothing

-- | As 'newInterpreterState', but dispatch entry point calls to a server that
-- is already running, instead of starting one. The server is never shut down by
-- this function; that remains the responsibility of whoever started it.
newInterpreterStateWith ::
  (MonadIO m, Evaluation m) =>
  EvalConfig ->
  VFS ->
  FFI.Server ->
  m (Either (Doc AnsiStyle) InterpreterState)
newInterpreterStateWith cfg vfs = newInterpreterStateOn cfg vfs . Just

newInterpreterStateOn ::
  (MonadIO m, Evaluation m) =>
  EvalConfig ->
  VFS ->
  Maybe FFI.Server ->
  m (Either (Doc AnsiStyle) InterpreterState)
newInterpreterStateOn cfg vfs server = runExceptT $ do
  let maybe_file = evalFile cfg
  (ws, imports, src) <-
    badOnLeft prettyCompilerError
      =<< liftIO
        ( runExceptT (readProgramFilesExceptKnown [] vfs $ maybeToList maybe_file)
            `catch` \(err :: IOException) ->
              pure (externalErrorS (show err))
        )
  when (evalPrintWarnings cfg) $
    liftIO . hPutDoc stderr $
      prettyWarnings ws

  (s, tenv, ienv) <- ExceptT $ initialiseInterpreter cfg maybe_file server imports

  pure $ InterpreterState (src, tenv, ienv, s)

-- | Interpret the given imports in order with the provided interpreter runner,
-- and produce the type and interpreter environment of the last one.
interpretImports ::
  (Monad m) =>
  (F I.ExtOp I.Ctx -> m (Either I.InterpreterError I.Ctx)) ->
  Imports ->
  ExceptT (Doc AnsiStyle) m (T.Env, I.Ctx)
interpretImports runner imports = do
  let foldFile ctx =
        badOnLeft I.prettyInterpreterError
          <=< lift . runner . I.interpretImport ctx
  ictx <- foldM foldFile I.initialCtx $ map (fmap fileProg) imports
  let (iname, fm) = last imports
  pure (fileScope fm, ictx {I.ctxEnv = I.ctxImports ictx M.! iname})

-- | Set up an interpreter context for the given program. If a server is
-- provided, or a backend has been requested (in which case a server is
-- started), then the entry points of the loaded file are marked external, such
-- that calls to them are dispatched to the server instead of being interpreted.
-- On failure, a server started here is shut down again, so a returned server is
-- always one that the caller now owns (and must eventually stop). A server
-- passed in by the caller is never shut down here.
initialiseInterpreter ::
  (Evaluation m, MonadIO m) =>
  EvalConfig ->
  Maybe FilePath ->
  Maybe FFI.Server ->
  Imports ->
  m (Either (Doc AnsiStyle) (Maybe FFI.Server, T.Env, I.Ctx))
initialiseInterpreter cfg maybe_file server imports =
  case (server, maybe_file, evalBackend cfg) of
    (Just s, _, _) ->
      evalWith (Just s) $ externaliseLast imports
    (Nothing, Just file, Just backend) -> do
      started <- liftIO $ prepareServer cfg file backend
      case started of
        Left err -> pure $ Left $ pretty err
        Right s -> do
          r <- evalWith (Just s) $ externaliseLast imports
          -- Do not leave a server running if we never got off the ground.
          when (isLeft r) $ void $ liftIO $ FFI.stopServer s
          pure r
    _ -> evalWith Nothing imports
  where
    evalWith s =
      runExceptT
        . fmap (\(tenv, ienv) -> (s, tenv, ienv))
        . interpretImports (runInterpreterNoBreak s)

-- | Run an interpreter action, dispatching external calls to the given
-- server (if any). Breakpoints are ignored, as there is no way to enter a
-- debugging prompt.
runInterpreterNoBreak ::
  (Evaluation m, MonadIO m) =>
  Maybe FFI.Server ->
  F I.ExtOp a ->
  m (Either I.InterpreterError a)
runInterpreterNoBreak s m = runF m (pure . Right) intOp
  where
    intOp (I.ExtOpError err) = pure $ Left err
    intOp (I.ExtOpTrace w v c) = do
      trace $ pretty w <> ":" <+> align (unAnnotate v)
      c
    intOp (I.ExtOpBreak _ I.BreakNaN _ c) = c
    intOp (I.ExtOpBreak w _ _ c) = do
      trace $ pretty (locText w) <> ": ignoring breakpoint in top-level constant."
      c
    intOp (I.ExtOpFFI sm c) = either (pure . Left) c =<< liftIO (runFFI s sm)
