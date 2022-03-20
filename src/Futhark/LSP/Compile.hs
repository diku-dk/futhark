module Futhark.LSP.Compile (tryTakeStateFromMVar, tryReCompile) where

import Control.Concurrent.MVar (MVar, putMVar, takeMVar)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Futhark.Compiler.Program (LoadedProg, noLoadedProg, reloadProg)
import Futhark.LSP.Diagnostic (errorToDiagnostics, sendDiagnostics, warningsToDiagnostics)
import Futhark.LSP.Utils (State (..), debug, emptyState)
import Language.Futhark.Warnings (listWarnings)
import Language.LSP.Server (LspT)
import Language.LSP.Types (TextDocumentVersion, filePathToUri, toNormalizedUri)

-- try to take state from MVar, if it's empty (Nothing), try to compile.
tryTakeStateFromMVar :: MVar State -> Maybe FilePath -> LspT () IO State
tryTakeStateFromMVar stateMVar filePath = do
  oldState <- liftIO $ takeMVar stateMVar -- TODO: change to modifyMVar for exception handling
  case stateProgram oldState of
    Nothing -> do
      newState <- tryCompile filePath (State $ Just noLoadedProg) (Just 0) -- first compile, version 0
      liftIO $ putMVar stateMVar newState
      pure newState
    Just _ -> do
      liftIO $ putMVar stateMVar oldState
      pure oldState

-- try to (re)-compile, replace old state if successful.
tryReCompile :: MVar State -> Maybe FilePath -> TextDocumentVersion -> LspT () IO ()
tryReCompile stateMVar filePath version = do
  debug "(Re)-compiling ..."
  oldState <- liftIO $ takeMVar stateMVar -- TODO: change to modifyMVar_ for exception handling
  newState <- tryCompile filePath oldState version
  case stateProgram newState of
    Nothing -> do
      debug "Failed to (re)-compile, using old state or Nothing"
      liftIO $ putMVar stateMVar oldState
    Just _ -> do
      debug "(Re)-compile successful"
      liftIO $ putMVar stateMVar newState

-- try to compile file, publish diagnostics on warnings or error, return newly compiled state.
-- single point where the compilation is done, and shouldn't be exported.
tryCompile :: Maybe FilePath -> State -> TextDocumentVersion -> LspT () IO State
tryCompile Nothing _ _ = pure emptyState
tryCompile (Just path) state version = do
  let oldLoadedProg = getLoadedProg state
  res <- liftIO $ reloadProg oldLoadedProg [path]
  case res of
    Right (warnings, newLoadedProg) -> do
      let diags = warningsToDiagnostics $ listWarnings warnings
      sendDiagnostics (toNormalizedUri $ filePathToUri path) diags version
      pure $ State (Just newLoadedProg)
    Left progErr -> do
      debug "Compilation failed, publishing diagnostics"
      let diags = errorToDiagnostics progErr
      sendDiagnostics (toNormalizedUri $ filePathToUri path) diags version
      pure emptyState

getLoadedProg :: State -> LoadedProg
getLoadedProg (State (Just loadedProg)) = loadedProg
getLoadedProg (State Nothing) = noLoadedProg
