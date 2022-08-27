{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | @futhark run@
module Futhark.CLI.Run (main) where

import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.Free.Church
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Futhark.Compiler
import Futhark.Pipeline
import Futhark.Util (toPOSIX)
import Futhark.Util.Options
import Futhark.Util.Pretty (AnsiStyle, Doc, hPutDoc)
import Language.Futhark
import qualified Language.Futhark.Interpreter as I
import Language.Futhark.Parser
import qualified Language.Futhark.Semantic as T
import qualified Language.Futhark.TypeChecker as T
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
      hPutDoc stderr err
      exitFailure
    Right env -> pure env

  let entry = interpreterEntryPoint config
  vr <- parseValues "stdin" <$> T.getContents

  inps <-
    case vr of
      Left (SyntaxError loc err) -> do
        T.hPutStrLn stderr $ "Input syntax error at " <> T.pack (locStr loc) <> ":\n" <> err
        exitFailure
      Right vs ->
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
  | otherwise = putStrLn $ prettyString v

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
  (ws, imports, src) <-
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

  let imp = T.mkInitialImport "."
  ienv1 <-
    foldM (\ctx -> badOnLeft I.prettyInterpreterError <=< runInterpreter' . I.interpretImport ctx) I.initialCtx $
      map (fmap fileProg) imports
  (tenv1, d1, src') <-
    badOnLeft T.prettyTypeError . snd $
      T.checkDec imports src T.initialEnv imp $
        mkOpen "/prelude/prelude"
  (tenv2, d2, _) <-
    badOnLeft T.prettyTypeError . snd $
      T.checkDec imports src' tenv1 imp $
        mkOpen $
          toPOSIX $
            dropExtension file
  ienv2 <- badOnLeft I.prettyInterpreterError =<< runInterpreter' (I.interpretDec ienv1 d1)
  ienv3 <- badOnLeft I.prettyInterpreterError =<< runInterpreter' (I.interpretDec ienv2 d2)
  pure (tenv2, ienv3)
  where
    badOnLeft :: (err -> err') -> Either err a -> ExceptT err' IO a
    badOnLeft _ (Right x) = pure x
    badOnLeft p (Left err) = throwError $ p err

mkOpen :: FilePath -> UncheckedDec
mkOpen f = OpenDec (ModImport f NoInfo mempty) mempty

runInterpreter' :: MonadIO m => F I.ExtOp a -> m (Either I.InterpreterError a)
runInterpreter' m = runF m (pure . Right) intOp
  where
    intOp (I.ExtOpError err) = pure $ Left err
    intOp (I.ExtOpTrace w v c) = do
      liftIO $ hPutStrLn stderr $ w ++ ": " ++ v
      c
    intOp (I.ExtOpBreak _ _ _ c) = c
