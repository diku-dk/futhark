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
import Data.Text.IO qualified as T
import Futhark.Compiler
import Futhark.Data.Reader (readValues)
import Futhark.Pipeline
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

interpret :: InterpreterConfig -> FilePath -> IO ()
interpret config fp = do
  pr <- newFutharkiState config fp
  (tenv, ienv) <- case pr of
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

  case I.interpretFunction ienv (qualLeaf fname) inps of
    Left err -> do
      T.hPutStrLn stderr err
      exitFailure
    Right run -> do
      run' <- runInterpreter' run
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
    interpreterPrintWarnings :: Bool
  }

interpreterConfig :: InterpreterConfig
interpreterConfig = InterpreterConfig defaultEntryPoint True

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
      "Do not print warnings."
  ]

newFutharkiState ::
  InterpreterConfig ->
  FilePath ->
  IO (Either (Doc AnsiStyle) (T.Env, I.Ctx))
newFutharkiState cfg file = runExceptT $ do
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
          <=< runInterpreter' . I.interpretImport ctx

  ictx <- foldM loadImport I.initialCtx $ map (fmap fileProg) imports
  let (tenv, ienv) =
        let (iname, fm) = last imports
         in ( fileScope fm,
              ictx {I.ctxEnv = I.ctxImports ictx M.! iname}
            )

  pure (tenv, ienv)
  where
    badOnLeft :: (err -> err') -> Either err a -> ExceptT err' IO a
    badOnLeft _ (Right x) = pure x
    badOnLeft p (Left err) = throwError $ p err

runInterpreter' :: (MonadIO m) => F I.ExtOp a -> m (Either I.InterpreterError a)
runInterpreter' m = runF m (pure . Right) intOp
  where
    intOp (I.ExtOpError err) = pure $ Left err
    intOp (I.ExtOpTrace w v c) = do
      liftIO $ hPutDocLn stderr $ pretty w <> ":" <+> align (unAnnotate v)
      c
    intOp (I.ExtOpBreak _ _ _ c) = c
