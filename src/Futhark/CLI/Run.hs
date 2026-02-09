-- | @futhark run@
module Futhark.CLI.Run (main) where

import Control.Exception
import Control.Monad
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Free.Church
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy qualified as BS
import Data.Map qualified as M
import Data.Maybe
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Futhark.Compiler
import Futhark.Data.Reader (readValues)
import Futhark.Pipeline
import Futhark.Server
  ( InputType (..),
    OutputType (..),
    Server,
    cmdEither,
    cmdEntryPoints,
    cmdInputs,
    cmdOutputs,
    newServerCfg,
    startServer,
  )
import Futhark.Util.Options
import Futhark.Util.Pretty (AnsiStyle, Doc, align, hPutDoc, hPutDocLn, pretty, unAnnotate, (<+>))
import Language.Futhark
import Language.Futhark.Interpreter qualified as I
import Language.Futhark.Semantic qualified as T
import System.Exit
import System.FilePath
import System.IO
import Prelude

-- | Run @futhark run@.
main :: String -> [String] -> IO ()
main = mainWithOptions interpreterConfig options "options... <program.fut>" run
  where
    run [prog] config = Just $ interpret config prog
    run _ _ = Nothing

describeEntryPoints :: Server -> IO ()
describeEntryPoints server = (either (error . T.unpack) pure <=< runExceptT) $ do
  liftIO $ putStrLn "Program contains these entry points:"
  entry_points <- cmdEither $ cmdEntryPoints server
  forM_ entry_points $ \ep -> do
    liftIO $ T.putStr $ ep <> " : "
    inputs <- cmdEither $ cmdInputs server ep
    outputs <- cmdEither $ cmdOutputs server ep
    let onInput (InputType True t) = "*" <> t
        onInput (InputType False t) = t
        onOutput (OutputType True t) = "*" <> t
        onOutput (OutputType False t) = t
    liftIO $
      T.putStrLn $
        T.intercalate " -> " (map onInput inputs)
          <> " -> "
          <> "("
          <> T.intercalate "," (map onOutput outputs)
          <> ")"
    pure ()

interpret :: InterpreterConfig -> FilePath -> IO ()
interpret config fp = do
  pr <- newFutharkiState config fp
  (server, tenv, ienv) <- case pr of
    Left err -> do
      hPutDocLn stderr err
      exitFailure
    Right env -> pure env

  let entry = interpreterEntryPoint config
  vr <- readValues <$> BS.getContents

  inps <-
    case vr of
      Nothing -> do
        T.hPutStrLn stderr "Incorrectly formatted input data."
        exitFailure
      Just vs ->
        pure vs

  (fname, ret) <-
    case M.lookup (T.Term, entry) $ T.envNameMap tenv of
      Just fname
        | Just (T.BoundV _ t) <- M.lookup (qualLeaf fname) $ T.envVtable tenv ->
            pure (fname, toStructural $ snd $ unfoldFunType t)
      _ -> do
        T.hPutStrLn stderr $ "Invalid entry point: " <> prettyText entry
        exitFailure

  mapM_ describeEntryPoints server

  case I.interpretFunction ienv (qualLeaf fname) inps of
    Left err -> do
      T.hPutStrLn stderr err
      exitFailure
    Right run -> do
      run' <- runInterpreter' server run
      case run' of
        Left err -> do
          hPrint stderr err
          exitFailure
        Right res ->
          case (I.fromTuple res, isTupleRecord ret) of
            (Just vs, Just ts) -> zipWithM_ putValue vs ts
            _ -> putValue res ret

putValue :: I.Value -> TypeBase () () -> IO ()
putValue v t
  | I.isEmptyArray v = T.putStrLn $ I.prettyEmptyArray t v
  | otherwise = T.putStrLn $ I.valueText v

data InterpreterConfig = InterpreterConfig
  { interpreterEntryPoint :: Name,
    interpreterPrintWarnings :: Bool,
    interpreterProgram :: Maybe FilePath
  }

interpreterConfig :: InterpreterConfig
interpreterConfig = InterpreterConfig defaultEntryPoint True Nothing

options :: [FunOptDescr InterpreterConfig]
options =
  [ Option
      "e"
      ["entry-point"]
      ( ReqArg
          ( \entry -> Right $ \config ->
              config {interpreterEntryPoint = nameFromString entry}
          )
          "NAME"
      )
      "The entry point to execute.",
    Option
      "w"
      ["no-warnings"]
      (NoArg $ Right $ \config -> config {interpreterPrintWarnings = False})
      "Do not print warnings.",
    Option
      ""
      ["program"]
      ( ReqArg
          (\fname -> Right $ \config -> config {interpreterProgram = Just fname})
          "PROGRAM"
      )
      "Load this compiled program."
  ]

newFutharkiState ::
  InterpreterConfig ->
  FilePath ->
  IO (Either (Doc AnsiStyle) (Maybe Server, T.Env, I.Ctx))
newFutharkiState cfg file = runExceptT $ do
  server <- case interpreterProgram cfg of
    Nothing -> pure Nothing
    Just fname -> Just <$> liftIO (startServer (newServerCfg fname []))

  (ws, imports, _src) <-
    badOnLeft prettyCompilerError
      =<< liftIO
        ( runExceptT (readProgramFile [] file)
            `catch` \(err :: IOException) ->
              pure (externalErrorS (show err))
        )
  when (interpreterPrintWarnings cfg) $
    liftIO $
      hPutDoc stderr $
        prettyWarnings ws

  let loadImport ctx =
        badOnLeft I.prettyInterpreterError
          <=< runInterpreter' server . I.interpretImport ctx

  ictx <- foldM loadImport I.initialCtx $ map (fmap fileProg) imports
  let (tenv, ienv) =
        let (iname, fm) = last imports
         in ( fileScope fm,
              ictx {I.ctxEnv = I.ctxImports ictx M.! iname}
            )

  pure (server, tenv, ienv)
  where
    badOnLeft :: (err -> err') -> Either err a -> ExceptT err' IO a
    badOnLeft _ (Right x) = pure x
    badOnLeft p (Left err) = throwError $ p err

runInterpreter' :: (MonadIO m) => Maybe Server -> F I.ExtOp a -> m (Either I.InterpreterError a)
runInterpreter' _server m = runF m (pure . Right) intOp
  where
    intOp (I.ExtOpError err) = pure $ Left err
    intOp (I.ExtOpTrace w v c) = do
      liftIO $ hPutDocLn stderr $ pretty w <> ":" <+> align (unAnnotate v)
      c
    intOp (I.ExtOpBreak _ _ _ c) = c
