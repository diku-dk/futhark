module Futhark.Eval
  ( InterpreterConfig (..),
    runExpr,
    interpreterConfig,
    newFutharkiState,
    Evaluation (..),
    EvalRecordRef (),
    runEvalRecordRef,
  )
where

import System.FilePath (dropExtension, (</>))
import Control.Arrow (Arrow(second))
import Control.Exception (IOException, catch)
import Control.Monad (foldM, when, (<=<))
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Free.Church (F, runF)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Bifunctor (first)
import Data.IORef (IORef, modifyIORef', readIORef, writeIORef, newIORef)
import Data.Map qualified as M
import Data.Maybe (maybeToList)
import Data.Sequence (Seq, (|>))
import Data.Text qualified as T
import Futhark.Compiler (prettyWarnings, readProgramFilesExceptKnown)
import Futhark.Compiler.Program (VFS, fileProg, fileScope)
import Futhark.Error (externalErrorS, prettyCompilerError)
import Futhark.FreshNames (VNameSource)
import Futhark.Util.Pretty (commasep, hPutDoc, hPutDocLn, hardline, putDocLn)
import Language.Futhark.Interpreter qualified as I
import Language.Futhark.Interpreter.FFI qualified as S
import Language.Futhark.Interpreter.FFI.Server (FutharkServer)
import Language.Futhark.Interpreter.FFI.Server qualified as S
import Language.Futhark.Interpreter.FFI.Server.Packer qualified as SP
import Language.Futhark.Interpreter.FFI.Values (Location)
import Language.Futhark.Parser (parseExp)
import Language.Futhark.Parser.Monad (SyntaxError (SyntaxError))
import Language.Futhark.Pretty (toName)
import Language.Futhark.Prop (typeOf)
import Language.Futhark.Semantic qualified as T
import Language.Futhark.Syntax (nameToText, typeParamName, VName (VName), ProgBase (progDecs), DecBase (ValDec), ValBindBase(..))
import Language.Futhark.TypeChecker qualified as T
import Prettyprinter (Doc, align, pretty, unAnnotate, vcat, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)
import System.Exit (ExitCode (ExitFailure), exitWith)
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

newtype InterpreterState = InterpreterState (VNameSource, T.Env, I.Ctx, Maybe FutharkServer)

-- TODO: Should NOT be IORef. This is temporary, for testing
call :: IORef (Maybe FutharkServer) -> VName -> [I.Value] -> IO I.Value
call s (VName n _) p = do
  let p' = map S.fromInterpreterValue p
  (Just s') <- readIORef s
  (r, s'') <- first S.toInterpreterValue <$> S.runFutharkServerM (SP.call (nameToText n) p') s'
  writeIORef s $ Just s''
  pure r

realize' l = SP.realize' l

-- TODO: Should NOT be IORef. This is temporary, for testing
realize :: IORef (Maybe FutharkServer) -> Location -> IO I.Value
realize s l = do
  (Just s') <- readIORef s
  (r, s'') <- first S.toInterpreterValue <$> S.runFutharkServerM (realize' l) s'
  writeIORef s $ Just s''
  pure r

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
  is <- liftIO $ newIORef s
  pval <- runInterpreterNoBreak call realize is $ I.interpretExp ctx fexp
  case pval of
    Left err -> do
      abort $ I.prettyInterpreterError err
    Right val -> pure $ I.prettyValue val <> hardline

data InterpreterConfig = InterpreterConfig
  { interpreterPrintWarnings :: Bool,
    interpreterFile :: Maybe String
  }

interpreterConfig :: InterpreterConfig
interpreterConfig = InterpreterConfig True Nothing

newFutharkiState ::
  (MonadIO m, Evaluation m) =>
  InterpreterConfig ->
  Maybe FilePath ->
  VFS ->
  m (Either (Doc AnsiStyle) InterpreterState)
newFutharkiState cfg maybe_file vfs = runExceptT $ do
  (ws, imports, src) <-
    badOnLeft prettyCompilerError
      =<< liftIO
        ( runExceptT (readProgramFilesExceptKnown [] vfs $ maybeToList maybe_file)
            `catch` \(err :: IOException) ->
              pure (externalErrorS (show err))
        )
  when (interpreterPrintWarnings cfg) $
    liftIO $
      hPutDoc stderr $
        prettyWarnings ws

  let modifyLast _ []     = []
      modifyLast f [x]    = [f x]
      modifyLast f (x:xs) = x : modifyLast f xs

  (imports', s) <- case maybe_file of
        Just file -> liftIO $
          let mdec (ValDec vb) =
                ValDec $ vb { valBindAttrs = "$external" : valBindAttrs vb }
              mdec o = o
              (_, m) = last imports
              m' = m { fileProg = (fileProg m) { progDecs = progDecs $ fileProg m} }
              prog = "." </> dropExtension file
          in (modifyLast (second $ const m') imports,) . Just <$> S.startServer prog
        Nothing -> pure (imports, Nothing)

  is <- liftIO $ newIORef s
  ictx <-
    let foldFile ctx =
          badOnLeft I.prettyInterpreterError
            <=< runInterpreterNoBreak call realize is
              . I.interpretImport ctx
     in foldM foldFile I.initialCtx $
          map (fmap fileProg) imports'
  s' <- liftIO $ readIORef is

  let (tenv, ienv) =
        let (iname, fm) = last imports'
         in ( fileScope fm,
              ictx {I.ctxEnv = I.ctxImports ictx M.! iname}
            )

  pure $ InterpreterState (src, tenv, ienv, s')
  where
    badOnLeft :: (Monad m) => (err -> err') -> Either err a -> ExceptT err' m a
    badOnLeft _ (Right x) = pure x
    badOnLeft p (Left err) = throwError $ p err

runInterpreterNoBreak ::
  (Evaluation m, MonadIO m) =>
  (IORef (Maybe FutharkServer) -> VName -> [I.Value] -> IO I.Value) ->
  (IORef (Maybe FutharkServer) -> Location -> IO I.Value) ->
  IORef (Maybe FutharkServer) ->
  F I.ExtOp a ->
  m (Either I.InterpreterError a)
runInterpreterNoBreak call' realize' s m = runF m (pure . Right) intOp
  where
    intOp (I.ExtOpError err) = pure $ Left err
    intOp (I.ExtOpTrace w v c) = do
      trace $ pretty w <> ":" <+> align (unAnnotate v)
      c
    intOp (I.ExtOpBreak _ _ _ c) = c
    intOp (I.ExtOpCall n p c) = do
      r <- liftIO $ call' s n p
      c r
    intOp (I.ExtOpRealize l c) = do
      r <- liftIO $ realize' s l
      c r
