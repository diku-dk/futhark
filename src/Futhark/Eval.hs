module Futhark.Eval (InterpreterConfig (..), runExpr, interpreterConfig, newFutharkiState, EvalIO (..)) where

import Control.Exception (IOException, catch)
import Control.Monad (foldM, when, (<=<))
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Free.Church (F, runF)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Map qualified as M
import Data.Maybe (maybeToList)
import Data.Text qualified as T
import Futhark.Compiler (prettyWarnings, readProgramFilesExceptKnown)
import Futhark.Compiler.Program (VFS, fileProg, fileScope)
import Futhark.Error (externalErrorS, prettyCompilerError)
import Futhark.FreshNames (VNameSource)
import Futhark.Util.Pretty (commasep, hPutDoc, hPutDocLn, hardline, putDocLn)
import Language.Futhark.Interpreter qualified as I
import Language.Futhark.Parser (parseExp)
import Language.Futhark.Parser.Monad (SyntaxError (SyntaxError))
import Language.Futhark.Pretty (toName)
import Language.Futhark.Prop (typeOf)
import Language.Futhark.Semantic qualified as T
import Language.Futhark.Syntax (nameToText, typeParamName)
import Language.Futhark.TypeChecker qualified as T
import Prettyprinter (Doc, align, pretty, unAnnotate, vcat, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (stderr)

class AbortEvaluation m where
  abort :: Doc AnsiStyle -> m b

class TraceEvaluation m where
  trace :: Doc AnsiStyle -> m ()

newtype EvalIO a = EvalIO {runEvalIO :: IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance (MonadTrans m, AbortEvaluation m', Monad m') => AbortEvaluation (m m') where
  abort :: Doc AnsiStyle -> m m' b
  abort = lift . abort

instance AbortEvaluation EvalIO where
  abort :: Doc AnsiStyle -> EvalIO b
  abort reason = EvalIO $ do
    hPutDocLn stderr reason
    exitWith $ ExitFailure 1

instance (MonadTrans m, TraceEvaluation m', Monad m') => TraceEvaluation (m m') where
  trace :: Doc AnsiStyle -> m m' ()
  trace = lift . trace

instance TraceEvaluation EvalIO where
  trace :: Doc AnsiStyle -> EvalIO ()
  trace = EvalIO . putDocLn

newtype InterpreterState = InterpreterState (VNameSource, T.Env, I.Ctx)

-- Use parseExp, checkExp, then interpretExp.
runExpr ::
  (Monad m, AbortEvaluation m, TraceEvaluation m) =>
  InterpreterState ->
  String ->
  m (Doc AnsiStyle)
runExpr (InterpreterState (src, env, ctx)) str = do
  uexp <- case parseExp "" (T.pack str) of
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
  pval <- runInterpreterNoBreak $ I.interpretExp ctx fexp
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
  (MonadIO m, TraceEvaluation m) =>
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

  ictx <-
    let foldFile ctx =
          badOnLeft I.prettyInterpreterError
            <=< runInterpreterNoBreak
              . I.interpretImport ctx
     in foldM foldFile I.initialCtx $
          map (fmap fileProg) imports

  let (tenv, ienv) =
        let (iname, fm) = last imports
         in ( fileScope fm,
              ictx {I.ctxEnv = I.ctxImports ictx M.! iname}
            )

  pure $ InterpreterState (src, tenv, ienv)
  where
    badOnLeft :: (Monad m) => (err -> err') -> Either err a -> ExceptT err' m a
    badOnLeft _ (Right x) = pure x
    badOnLeft p (Left err) = throwError $ p err

runInterpreterNoBreak :: (TraceEvaluation m, Monad m) => F I.ExtOp a -> m (Either I.InterpreterError a)
runInterpreterNoBreak m = runF m (pure . Right) intOp
  where
    intOp (I.ExtOpError err) = pure $ Left err
    intOp (I.ExtOpTrace w v c) = do
      trace $ pretty w <> ":" <+> align (unAnnotate v)
      c
    intOp (I.ExtOpBreak _ _ _ c) = c
