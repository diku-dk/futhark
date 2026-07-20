module Futhark.Eval
  ( EvalConfig (..),
    runExpr,
    evalConfig,
    newFutharkiState,
    Evaluation (..),
    EvalRecordRef (),
    runEvalRecordRef,
  )
where

import Control.Arrow (Arrow (second))
import Control.Exception (IOException, catch)
import Control.Monad (foldM, unless, void, when, (<=<), zipWithM)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Free.Church (F, runF)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.IORef (IORef, modifyIORef')
import Data.Map qualified as M
import Data.Maybe (isJust, maybeToList)
import Data.Sequence (Seq, (|>))
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Futhark.Compiler (prettyWarnings, readProgramFilesExceptKnown)
import Futhark.Compiler.Program (VFS, fileProg, fileScope)
import Futhark.Error (externalErrorS, prettyCompilerError)
import Futhark.FreshNames (VNameSource)
import Futhark.Server qualified as S
import Futhark.Test (FutharkExe (..), compileProgram)
import Futhark.Util.Pretty (commasep, hPutDoc, hPutDocLn, putDocLn, hardline)
import Language.Futhark.Interpreter qualified as I
import Language.Futhark.Interpreter.FFI.Push qualified as FFI
import Language.Futhark.Interpreter.FFI.ServerM qualified as FFI
import Language.Futhark.Parser (parseExp)
import Language.Futhark.Parser.Monad (SyntaxError (SyntaxError))
import Language.Futhark.Pretty (toName)
import Language.Futhark.Prop (typeOf)
import Language.Futhark.Semantic qualified as T
import Language.Futhark.Syntax (DecBase (ValDec), ProgBase (progDecs), ValBindBase (..), nameToText, typeParamName)
import Language.Futhark.TypeChecker qualified as T
import Prettyprinter (Doc, align, pretty, unAnnotate, vcat, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (ExitFailure), exitFailure, exitWith)
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
runExpr (InterpreterState (src, env, ctx, s)) str = do
  uexp <- case parseExp "" str of
    Left (SyntaxError _ serr) -> abort $ pretty serr
    Right e -> pure e
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
    Right val -> pure $ I.prettyValue val <> hardline

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

prepareServer :: EvalConfig -> FilePath -> String -> IO FFI.Server
prepareServer cfg file backend = do
  futhark <- maybe getExecutablePath pure $ evalFuthark cfg

  unless (evalSkipCompilation cfg) $ do
    let compile_options = "--server" : evalCompilerOptions cfg

    let onError err = do
          T.hPutStrLn stderr err
          exitFailure

    void $
      either onError pure <=< runExceptT $
        compileProgram compile_options (FutharkExe futhark) backend file

  let prog = "." </> dropExtension file
  FFI.startServer $ S.newServerCfg prog []

newFutharkiState ::
  (MonadIO m, Evaluation m) =>
  EvalConfig ->
  VFS ->
  m (Either (Doc AnsiStyle) InterpreterState)
newFutharkiState cfg vfs = runExceptT $ do
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

  let modifyLast _ [] = []
      modifyLast f [x] = [f x]
      modifyLast f (x : xs) = x : modifyLast f xs

  (imports', s) <- case (maybe_file, evalBackend cfg) of
    (Just file, Just backend) -> liftIO $ do
      let mdec (ValDec vb)
            | isJust $ valBindEntryPoint vb =
                ValDec $ vb {valBindAttrs = "$external" : valBindAttrs vb}
          mdec dec = dec
          (_, m) = last imports
          m' = m {fileProg = (fileProg m) {progDecs = map mdec $ progDecs $ fileProg m}}
      (modifyLast (second $ const m') imports,) . Just
        <$> prepareServer cfg file backend
    _ -> pure (imports, Nothing)

  ictx <-
    let foldFile ctx =
          badOnLeft I.prettyInterpreterError
            <=< runInterpreterNoBreak s
              . I.interpretImport ctx
     in foldM foldFile I.initialCtx $
          map (fmap fileProg) imports'

  let (tenv, ienv) =
        let (iname, fm) = last imports'
         in ( fileScope fm,
              ictx {I.ctxEnv = I.ctxImports ictx M.! iname}
            )

  pure $ InterpreterState (src, tenv, ienv, s)
  where
    badOnLeft :: (Monad m) => (err -> err') -> Either err a -> ExceptT err' m a
    badOnLeft _ (Right x) = pure x
    badOnLeft p (Left err) = throwError $ p err

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
    intOp (I.ExtOpBreak _ _ _ c) = c
    intOp (I.ExtOpCall n ps shp c) =
      liftIO (either error id <$> case s of
        Nothing -> error "External call, but no server."
        Just s' -> FFI.runServerM s' $ do
          FFI.gc
          pts <- FFI.inputs n
          zipWithM FFI.put pts ps >>= FFI.call n >>= FFI.get shp) >>= c
