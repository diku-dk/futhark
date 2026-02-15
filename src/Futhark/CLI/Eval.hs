-- | @futhark eval@
module Futhark.CLI.Eval (main) where

import Control.Exception (IOException, catch)
import Control.Monad
  ( foldM,
    forM_,
    when,
    (<=<),
  )
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Free.Church (F (runF))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Map qualified as M
import Data.Maybe (maybeToList)
import Data.Text qualified as T
import Futhark.Compiler
  ( FileModule (fileProg, fileScope),
    VFS,
    prettyWarnings,
    readProgramFilesExceptKnown,
  )
import Futhark.Error (externalErrorS, prettyCompilerError)
import Futhark.FreshNames (VNameSource)
import Futhark.Util.Options
  ( ArgDescr (NoArg, ReqArg),
    FunOptDescr,
    OptDescr (Option),
    mainWithOptions,
  )
import Futhark.Util.Pretty
  ( AnsiStyle,
    Doc,
    Pretty (pretty),
    align,
    commasep,
    hPutDoc,
    hPutDocLn,
    hardline,
    putDocLn,
    unAnnotate,
    vcat,
    (<+>),
  )
import Language.Futhark.Interpreter qualified as I
import Language.Futhark.Parser
  ( SyntaxError (SyntaxError),
    parseExp,
  )
import Language.Futhark.Pretty (IsName (toName))
import Language.Futhark.Prop (typeOf)
import Language.Futhark.Semantic qualified as T
import Language.Futhark.Syntax (nameToText, typeParamName)
import Language.Futhark.TypeChecker qualified as T
import Language.Futhark.TypeChecker.Monad qualified as I
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (stderr)

-- | Run @futhark eval@.
main :: String -> [String] -> IO ()
main = mainWithOptions interpreterConfig options "options... <exprs...>" run
  where
    run [] _ = Nothing
    run exprs config = Just $ runExprs exprs config

class MonadAbort m where
  abort :: Doc AnsiStyle -> m b

class MonadTrace m where
  trace :: Doc AnsiStyle -> m ()

newtype EvalIO a = EvalIO {runEvalIO :: IO a}
  deriving (Functor, Applicative, Monad)

instance (MonadTrans m, MonadAbort m', Monad m') => MonadAbort (m m') where
  abort :: Doc AnsiStyle -> m m' b
  abort = lift . abort

instance MonadAbort EvalIO where
  abort :: Doc AnsiStyle -> EvalIO b
  abort reason = EvalIO $ do
    hPutDocLn stderr reason
    exitWith $ ExitFailure 1

instance (MonadTrans m, MonadTrace m', Monad m') => MonadTrace (m m') where
  trace :: Doc AnsiStyle -> m m' ()
  trace = lift . trace

instance MonadTrace EvalIO where
  trace :: Doc AnsiStyle -> EvalIO ()
  trace = EvalIO . putDocLn

instance MonadIO EvalIO where
  liftIO :: IO a -> EvalIO a
  liftIO = EvalIO

runExprs :: [String] -> InterpreterConfig -> IO ()
runExprs exprs cfg = do
  let InterpreterConfig _ file = cfg
  maybe_new_state <- runEvalIO $ newFutharkiState cfg file M.empty
  (src, env, ctx) <- case maybe_new_state of
    Left reason -> do
      hPutDocLn stderr reason
      exitWith $ ExitFailure 2
    Right s -> pure s
  forM_ exprs $ \expr -> putDocLn =<< runEvalIO (runExpr src env ctx expr)

-- Use parseExp, checkExp, then interpretExp.
runExpr ::
  (Monad m, MonadAbort m, MonadTrace m) =>
  VNameSource ->
  T.Env ->
  I.Ctx ->
  String ->
  m (Doc AnsiStyle)
runExpr src env ctx str = do
  uexp <- case parseExp "" (T.pack str) of
    Left (SyntaxError _ serr) -> abort $ pretty serr
    Right e -> pure e
  fexp <- case T.checkExp [] src env uexp of
    (_, Left terr) -> do
      abort $ I.prettyTypeError terr
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

options :: [FunOptDescr InterpreterConfig]
options =
  [ Option
      "f"
      ["file"]
      ( ReqArg
          ( \entry -> Right $ \config ->
              config {interpreterFile = Just entry}
          )
          "NAME"
      )
      "The file to load before evaluating expressions.",
    Option
      "w"
      ["no-warnings"]
      (NoArg $ Right $ \config -> config {interpreterPrintWarnings = False})
      "Do not print warnings."
  ]

newFutharkiState ::
  InterpreterConfig ->
  Maybe FilePath ->
  VFS ->
  EvalIO (Either (Doc AnsiStyle) (VNameSource, T.Env, I.Ctx))
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
            <=< runInterpreterNoBreak . I.interpretImport ctx
     in foldM foldFile I.initialCtx $
          map (fmap fileProg) imports

  let (tenv, ienv) =
        let (iname, fm) = last imports
         in ( fileScope fm,
              ictx {I.ctxEnv = I.ctxImports ictx M.! iname}
            )

  pure (src, tenv, ienv)
  where
    badOnLeft :: (Monad m) => (err -> err') -> Either err a -> ExceptT err' m a
    badOnLeft _ (Right x) = pure x
    badOnLeft p (Left err) = throwError $ p err

runInterpreterNoBreak :: (MonadTrace m, Monad m) => F I.ExtOp a -> m (Either I.InterpreterError a)
runInterpreterNoBreak m = runF m (pure . Right) intOp
  where
    intOp (I.ExtOpError err) = pure $ Left err
    intOp (I.ExtOpTrace w v c) = do
      trace $ pretty w <> ":" <+> align (unAnnotate v)
      c
    intOp (I.ExtOpBreak _ _ _ c) = c
