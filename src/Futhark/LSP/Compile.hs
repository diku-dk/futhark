module Futhark.LSP.Compile (tryTakeStateFromMVar, tryReCompile) where

import Control.Concurrent.MVar (MVar, putMVar, takeMVar)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Futhark.Compiler.Program (LoadedProg, noLoadedProg, reloadProg)
import Futhark.LSP.Diagnostic (errorToDiagnostics, sendDiagnostics, warningsToDiagnostics)
import Futhark.LSP.State (State (..), emptyState)
import Futhark.Util (debug)
import Language.Futhark.Warnings (listWarnings)
import Language.LSP.Server (LspT)
import Language.LSP.Types (TextDocumentVersion, filePathToUri, toNormalizedUri)

-- | Try to take state from MVar, if it's empty, try to compile.
tryTakeStateFromMVar :: MVar State -> Maybe FilePath -> LspT () IO State
tryTakeStateFromMVar state_mvar file_path = do
  old_state <- liftIO $ takeMVar state_mvar
  case stateProgram old_state of
    Nothing -> do
      new_state <- tryCompile file_path (State $ Just noLoadedProg) (Just 0) -- first compile, version 0
      liftIO $ putMVar state_mvar new_state
      pure new_state
    Just _ -> do
      liftIO $ putMVar state_mvar old_state
      pure old_state

-- | Try to (re)-compile, replace old state if successful.
tryReCompile :: MVar State -> Maybe FilePath -> TextDocumentVersion -> LspT () IO ()
tryReCompile state_mvar file_path version = do
  debug "(Re)-compiling ..."
  old_state <- liftIO $ takeMVar state_mvar
  new_state <- tryCompile file_path old_state version
  case stateProgram new_state of
    Nothing -> do
      debug "Failed to (re)-compile, using old state or Nothing"
      liftIO $ putMVar state_mvar old_state
    Just _ -> do
      debug "(Re)-compile successful"
      liftIO $ putMVar state_mvar new_state

-- | Try to compile, publish diagnostics on warnings and errors, return newly compiled state.
--  Single point where the compilation is done, and shouldn't be exported.
tryCompile :: Maybe FilePath -> State -> TextDocumentVersion -> LspT () IO State
tryCompile Nothing _ _ = pure emptyState
tryCompile (Just path) state version = do
  let old_loaded_prog = getLoadedProg state
  res <- liftIO $ reloadProg old_loaded_prog [path]
  case res of
    Right (warnings, new_loaded_prog) -> do
      let diags = warningsToDiagnostics $ listWarnings warnings
      sendDiagnostics (toNormalizedUri $ filePathToUri path) diags version
      pure $ State (Just new_loaded_prog)
    Left prog_error -> do
      debug "Compilation failed, publishing diagnostics"
      let diags = errorToDiagnostics prog_error
      sendDiagnostics (toNormalizedUri $ filePathToUri path) diags version
      pure emptyState

getLoadedProg :: State -> LoadedProg
getLoadedProg (State (Just loaded_prog)) = loaded_prog
getLoadedProg (State Nothing) = noLoadedProg
