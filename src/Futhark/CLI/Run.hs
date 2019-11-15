{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Futhark.CLI.Run (main) where

import Control.Monad.Free.Church
import Control.Exception
import Data.List
import Data.Loc
import Data.Maybe
import qualified Data.Map as M
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Except
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath
import System.Exit
import System.Console.GetOpt
import System.IO
import qualified System.Console.Haskeline as Haskeline

import Prelude

import Language.Futhark
import Language.Futhark.Parser hiding (EOF)
import qualified Language.Futhark.TypeChecker as T
import qualified Language.Futhark.Semantic as T
import Futhark.Compiler
import Futhark.Pipeline
import Futhark.Util.Options
import Futhark.Util (toPOSIX)

import qualified Language.Futhark.Interpreter as I

main :: String -> [String] -> IO ()
main = mainWithOptions interpreterConfig options "options... program" run
  where run [prog] config = Just $ interpret config prog
        run _      _      = Nothing

interpret :: InterpreterConfig -> FilePath -> IO ()
interpret config fp = do
  pr <- newFutharkiState config fp
  (tenv, ienv) <- case pr of Left err -> do hPutStrLn stderr err
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
      _ -> do hPutStrLn stderr $ "Invalid entry point: " ++ pretty entry
              exitFailure

  case I.interpretFunction ienv (qualLeaf fname) inps of
    Left err -> do hPutStrLn stderr err
                   exitFailure
    Right run -> do
      run' <- runInterpreter' run
      case run' of
        Left err -> do hPrint stderr err
                       exitFailure
        Right res ->
          case (I.fromTuple res, isTupleRecord ret) of
            (Just vs, Just ts) -> zipWithM_ putValue vs ts
            _ -> putValue res ret

putValue :: I.Value -> TypeBase () () -> IO ()
putValue v t
  | I.isEmptyArray v = putStrLn $ I.prettyEmptyArray t v
  | otherwise = putStrLn $ pretty v

data InterpreterConfig =
  InterpreterConfig { interpreterEntryPoint :: Name
                    , interpreterPrintWarnings :: Bool
                    }

interpreterConfig :: InterpreterConfig
interpreterConfig = InterpreterConfig defaultEntryPoint True

options :: [FunOptDescr InterpreterConfig]
options = [ Option "e" ["entry-point"]
            (ReqArg (\entry -> Right $ \config ->
                        config { interpreterEntryPoint = nameFromString entry })
             "NAME")
            "The entry point to execute."
          , Option "w" ["no-warnings"]
            (NoArg $ Right $ \config -> config { interpreterPrintWarnings = False })
            "Do not print warnings."
          ]

newFutharkiState :: InterpreterConfig -> FilePath
                 -> IO (Either String (T.Env, I.Ctx))
newFutharkiState cfg file = runExceptT $ do
  (ws, imports, src) <-
    badOnLeft =<< liftIO (runExceptT (readProgram file)
                          `Haskeline.catch` \(err::IOException) ->
                             return (Left (ExternalError (T.pack $ show err))))
  when (interpreterPrintWarnings cfg) $
    liftIO $ hPrint stderr ws

  let imp = T.mkInitialImport "."
  ienv1 <- foldM (\ctx -> badOnLeft <=< runInterpreter' . I.interpretImport ctx) I.initialCtx $
           map (fmap fileProg) imports
  (tenv1, d1, src') <- badOnLeft $ T.checkDec imports src T.initialEnv imp $
                       mkOpen "/futlib/prelude"
  (tenv2, d2, _) <- badOnLeft $ T.checkDec imports src' tenv1 imp $
                    mkOpen $ toPOSIX $ dropExtension file
  ienv2 <- badOnLeft =<< runInterpreter' (I.interpretDec ienv1 d1)
  ienv3 <- badOnLeft =<< runInterpreter' (I.interpretDec ienv2 d2)
  return (tenv2, ienv3)
  where badOnLeft :: Show err => Either err a -> ExceptT String IO a
        badOnLeft (Right x) = return x
        badOnLeft (Left err) = throwError $ show err

mkOpen :: FilePath -> UncheckedDec
mkOpen f = OpenDec (ModImport f NoInfo noLoc) noLoc

runInterpreter' :: MonadIO m => F I.ExtOp a -> m (Either I.InterpreterError a)
runInterpreter' m = runF m (return . Right) intOp
  where intOp (I.ExtOpError err) = return $ Left err
        intOp (I.ExtOpTrace w v c) = do
          liftIO $ putStrLn $ "Trace at " ++ locStr w ++ ": " ++ v
          c
        intOp (I.ExtOpBreak _ c) = c
