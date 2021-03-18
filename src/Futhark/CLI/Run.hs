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
import qualified Data.Text.IO as T
import Futhark.Compiler
import Futhark.Pipeline
import Futhark.Util (toPOSIX)
import Futhark.Util.Options
import Language.Futhark
import qualified Language.Futhark.Interpreter as I
import Language.Futhark.Parser hiding (EOF)
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
      hPutStrLn stderr err
      exitFailure
    Right env -> return env

  let entry = interpreterEntryPoint config
  vr <- parseValues "stdin" <$> T.getContents

  inps <-
    case vr of
      Left err -> do
        hPutStrLn stderr $ "Error when reading input: " ++ show err
        exitFailure
      Right vs ->
        return vs

  (fname, ret) <-
    case M.lookup (T.Term, entry) $ T.envNameMap tenv of
      Just fname
        | Just (T.BoundV _ t) <- M.lookup (qualLeaf fname) $ T.envVtable tenv ->
          return (fname, toStructural $ snd $ unfoldFunType t)
      _ -> do
        hPutStrLn stderr $ "Invalid entry point: " ++ pretty entry
        exitFailure

  case I.interpretFunction ienv (qualLeaf fname) inps of
    Left err -> do
      hPutStrLn stderr err
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
  | I.isEmptyArray v = putStrLn $ I.prettyEmptyArray t v
  | otherwise = putStrLn $ pretty v

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
  IO (Either String (T.Env, I.Ctx))
newFutharkiState cfg file = runExceptT $ do
  (ws, imports, src) <-
    badOnLeft show
      =<< liftIO
        ( runExceptT (readProgram [] file)
            `catch` \(err :: IOException) ->
              return (externalErrorS (show err))
        )
  when (interpreterPrintWarnings cfg) $
    liftIO $ hPutStr stderr $ pretty ws

  let imp = T.mkInitialImport "."
  ienv1 <-
    foldM (\ctx -> badOnLeft show <=< runInterpreter' . I.interpretImport ctx) I.initialCtx $
      map (fmap fileProg) imports
  (tenv1, d1, src') <-
    badOnLeft pretty $
      snd $
        T.checkDec imports src T.initialEnv imp $
          mkOpen "/prelude/prelude"
  (tenv2, d2, _) <-
    badOnLeft pretty $
      snd $
        T.checkDec imports src' tenv1 imp $
          mkOpen $ toPOSIX $ dropExtension file
  ienv2 <- badOnLeft show =<< runInterpreter' (I.interpretDec ienv1 d1)
  ienv3 <- badOnLeft show =<< runInterpreter' (I.interpretDec ienv2 d2)
  return (tenv2, ienv3)
  where
    badOnLeft :: (err -> String) -> Either err a -> ExceptT String IO a
    badOnLeft _ (Right x) = return x
    badOnLeft p (Left err) = throwError $ p err

mkOpen :: FilePath -> UncheckedDec
mkOpen f = OpenDec (ModImport f NoInfo mempty) mempty

runInterpreter' :: MonadIO m => F I.ExtOp a -> m (Either I.InterpreterError a)
runInterpreter' m = runF m (return . Right) intOp
  where
    intOp (I.ExtOpError err) = return $ Left err
    intOp (I.ExtOpTrace w v c) = do
      liftIO $ putStrLn $ "Trace at " ++ locStr w ++ ": " ++ v
      c
    intOp (I.ExtOpBreak _ _ c) = c
