{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Haskell code for interacting with a Futhark server program (a
-- program compiled with @--server@).
module Futhark.Server
  ( Server,
    withServer,
    CmdFailure (..),
    VarName,
    TypeName,
    EntryName,
    cmdRestore,
    cmdStore,
    cmdCall,
    cmdFree,
    cmdRename,
    cmdInputs,
    cmdOutputs,
    cmdClear,
    cmdReport,
    cmdMaybe,
    cmdEither,
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Futhark.Util (isEnvVarAtLeast)
import System.Directory (removeFile)
import System.Exit
import System.IO hiding (stdin, stdout)
import System.IO.Temp
import qualified System.Process as P

-- | A handle to a running server.
data Server = Server
  { serverStdin :: Handle,
    serverStdout :: Handle,
    serverErrLog :: FilePath,
    serverProc :: P.ProcessHandle,
    serverDebug :: Bool
  }

startServer :: FilePath -> [FilePath] -> IO Server
startServer prog options = do
  tmpdir <- getCanonicalTemporaryDirectory
  (err_log_f, err_log_h) <- openTempFile tmpdir "futhark-server-stderr.log"
  (Just stdin, Just stdout, Nothing, phandle) <-
    P.createProcess
      ( (P.proc prog options)
          { P.std_err = P.UseHandle err_log_h,
            P.std_in = P.CreatePipe,
            P.std_out = P.CreatePipe
          }
      )

  code <- P.getProcessExitCode phandle
  case code of
    Just (ExitFailure e) ->
      error $ "Cannot start " ++ prog ++ ": error " ++ show e
    _ -> do
      let server =
            Server
              { serverStdin = stdin,
                serverStdout = stdout,
                serverProc = phandle,
                serverDebug = isEnvVarAtLeast "FUTHARK_COMPILER_DEBUGGING" 1,
                serverErrLog = err_log_f
              }
      void (responseLines server) `catch` onStartupError server
      pure server
  where
    onStartupError :: Server -> IOError -> IO a
    onStartupError s _ = do
      code <- P.waitForProcess $ serverProc s
      stderr_s <- readFile $ serverErrLog s
      removeFile $ serverErrLog s
      error $
        "Command failed with " ++ show code ++ ":\n"
          ++ unwords (prog : options)
          ++ "\nStderr:\n"
          ++ stderr_s

stopServer :: Server -> IO ()
stopServer s = do
  hClose $ serverStdin s
  void $ P.waitForProcess $ serverProc s
  removeFile $ serverErrLog s

-- | Start a server, execute an action, then shut down the server.
withServer :: FilePath -> [FilePath] -> (Server -> IO a) -> IO a
withServer prog options = bracket (startServer prog options) stopServer

-- Read lines of response until the next %%% OK (which is what
-- indicates that the server is ready for new instructions).
responseLines :: Server -> IO [Text]
responseLines s = do
  l <- T.hGetLine $ serverStdout s
  when (serverDebug s) $
    T.hPutStrLn stderr $ "<<< " <> l
  case l of
    "%%% OK" -> pure []
    _ -> (l :) <$> responseLines s

-- | The command failed, and this is why.  The first 'Text' is any
-- output before the failure indincator, and the second Text is the
-- output after the indicator.
data CmdFailure = CmdFailure {failureLog :: [Text], failureMsg :: [Text]}
  deriving (Eq, Ord, Show)

-- Figure out whether the response is a failure, and if so, return the
-- failure message.
checkForFailure :: [Text] -> Either CmdFailure [Text]
checkForFailure [] = Right []
checkForFailure ("%%% FAILURE" : ls) = Left $ CmdFailure mempty ls
checkForFailure (l : ls) =
  case checkForFailure ls of
    Left (CmdFailure xs ys) -> Left $ CmdFailure (l : xs) ys
    Right ls' -> Right $ l : ls'

-- Words with spaces in them must be quoted.
quoteWord :: Text -> Text
quoteWord t
  | Just _ <- T.find (== ' ') t =
    "\"" <> t <> "\""
  | otherwise = t

sendCommand :: Server -> [Text] -> IO (Either CmdFailure [Text])
sendCommand s command = do
  let command' = T.unwords $ map quoteWord command

  when (serverDebug s) $
    T.hPutStrLn stderr $ ">>> " <> command'

  T.hPutStrLn (serverStdin s) command'
  hFlush $ serverStdin s
  checkForFailure <$> responseLines s `catch` onError
  where
    onError :: IOError -> IO a
    onError e = do
      code <- P.getProcessExitCode $ serverProc s
      let code_msg =
            case code of
              Just (ExitFailure x) ->
                "\nServer process exited unexpectedly with exit code: " ++ show x
              _ -> mempty
      stderr_s <- readFile $ serverErrLog s
      error $
        "After sending command " ++ show command ++ " to server process:"
          ++ show e
          ++ code_msg
          ++ "\nServer stderr:\n"
          ++ stderr_s

-- | The name of a server-side variable.
type VarName = Text

-- | The name of a server-side type.
type TypeName = Text

-- | The name of an entry point.
type EntryName = Text

helpCmd :: Server -> [Text] -> IO (Maybe CmdFailure)
helpCmd s cmd =
  either Just (const Nothing) <$> sendCommand s cmd

cmdRestore :: Server -> FilePath -> [(VarName, TypeName)] -> IO (Maybe CmdFailure)
cmdRestore s fname vars = helpCmd s $ "restore" : T.pack fname : concatMap f vars
  where
    f (v, t) = [v, t]

cmdStore :: Server -> FilePath -> [VarName] -> IO (Maybe CmdFailure)
cmdStore s fname vars = helpCmd s $ "store" : T.pack fname : vars

cmdCall :: Server -> EntryName -> [VarName] -> [VarName] -> IO (Either CmdFailure [T.Text])
cmdCall s entry outs ins =
  sendCommand s $ "call" : entry : outs ++ ins

cmdFree :: Server -> [VarName] -> IO (Maybe CmdFailure)
cmdFree s vs = helpCmd s $ "free" : vs

cmdRename :: Server -> VarName -> VarName -> IO (Maybe CmdFailure)
cmdRename s oldname newname = helpCmd s ["rename", oldname, newname]

cmdInputs :: Server -> EntryName -> IO (Either CmdFailure [TypeName])
cmdInputs s entry =
  sendCommand s ["inputs", entry]

cmdOutputs :: Server -> EntryName -> IO (Either CmdFailure [TypeName])
cmdOutputs s entry =
  sendCommand s ["outputs", entry]

cmdClear :: Server -> IO (Maybe CmdFailure)
cmdClear s = helpCmd s ["clear"]

cmdReport :: Server -> IO (Either CmdFailure [T.Text])
cmdReport s = sendCommand s ["report"]

-- | Turn a 'Maybe'-producing command into a monadic action.
cmdMaybe :: (MonadError T.Text m, MonadIO m) => IO (Maybe CmdFailure) -> m ()
cmdMaybe = maybe (pure ()) (throwError . T.unlines . failureMsg) <=< liftIO

-- | Turn an 'Either'-producing command into a monadic action.
cmdEither :: (MonadError T.Text m, MonadIO m) => IO (Either CmdFailure a) -> m a
cmdEither = either (throwError . T.unlines . failureMsg) pure <=< liftIO
