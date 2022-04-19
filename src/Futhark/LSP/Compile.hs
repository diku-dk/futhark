-- | Building blocks for "recompiling" (actually just type-checking)
-- the Futhark program managed by the language server.  The challenge
-- here is that if the program becomes type-invalid, we want to keep
-- the old state around.
module Futhark.LSP.Compile (tryTakeStateFromMVar, tryReCompile) where

import Control.Concurrent.MVar (MVar, putMVar, takeMVar)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Map as M
import qualified Data.Text as T
import Futhark.Compiler.Program (LoadedProg, lpFilePaths, lpWarnings, noLoadedProg, reloadProg)
import Futhark.LSP.Diagnostic (diagnosticSource, maxDiagnostic, publishErrorDiagnostics, publishWarningDiagnostics)
import Futhark.LSP.State (State (..), emptyState)
import Futhark.Util (debug)
import Language.Futhark.Warnings (listWarnings)
import Language.LSP.Server (LspT, flushDiagnosticsBySource, getVirtualFiles)
import Language.LSP.Types (fromNormalizedFilePath, uriToNormalizedFilePath)
import Language.LSP.VFS (VFS (vfsMap), virtualFileText)

-- | Try to take state from MVar, if it's empty, try to compile.
tryTakeStateFromMVar :: MVar State -> Maybe FilePath -> LspT () IO State
tryTakeStateFromMVar state_mvar file_path = do
  old_state <- liftIO $ takeMVar state_mvar
  case stateProgram old_state of
    Nothing -> do
      new_state <- tryCompile file_path (State $ Just noLoadedProg)
      liftIO $ putMVar state_mvar new_state
      pure new_state
    Just prog -> do
      -- If this is in the context of some file that is not part of
      -- the program, try to reload the program from that file.
      let files = lpFilePaths prog
      state <- case file_path of
        Just file_path'
          | file_path' `notElem` files -> do
              debug $ "File not part of program: " <> show file_path'
              debug $ "Program contains: " <> show files
              tryCompile file_path (State $ Just noLoadedProg)
        _ -> pure old_state
      liftIO $ putMVar state_mvar state
      pure state

-- | Try to (re)-compile, replace old state if successful.
tryReCompile :: MVar State -> Maybe FilePath -> LspT () IO ()
tryReCompile state_mvar file_path = do
  debug "(Re)-compiling ..."
  old_state <- liftIO $ takeMVar state_mvar
  new_state <- tryCompile file_path old_state
  case stateProgram new_state of
    Nothing -> do
      debug "Failed to (re)-compile, using old state or Nothing"
      liftIO $ putMVar state_mvar old_state
    Just _ -> do
      debug "(Re)-compile successful"
      liftIO $ putMVar state_mvar new_state

-- | Try to compile, publish diagnostics on warnings and errors, return newly compiled state.
--  Single point where the compilation is done, and shouldn't be exported.
tryCompile :: Maybe FilePath -> State -> LspT () IO State
tryCompile Nothing _ = pure emptyState
tryCompile (Just path) state = do
  debug $ "Reloading program from " <> show path
  let old_loaded_prog = getLoadedProg state
  vfs <- getVirtualFiles
  res <- liftIO $ reloadProg old_loaded_prog [path] (transformVFS vfs)
  flushDiagnosticsBySource maxDiagnostic diagnosticSource
  case res of
    Right new_loaded_prog -> do
      publishWarningDiagnostics $ listWarnings $ lpWarnings new_loaded_prog
      pure $ State (Just new_loaded_prog)
    Left prog_error -> do
      debug "Compilation failed, publishing diagnostics"
      publishErrorDiagnostics prog_error
      pure emptyState

-- | Transform VFS to a map of file paths to file contents.
-- This is used to pass the file contents to the compiler.
transformVFS :: VFS -> M.Map FilePath T.Text
transformVFS vfs =
  M.foldrWithKey
    ( \uri virtual_file acc ->
        case uriToNormalizedFilePath uri of
          Nothing -> acc
          Just file_path ->
            M.insert (fromNormalizedFilePath file_path) (virtualFileText virtual_file) acc
    )
    M.empty
    (vfsMap vfs)

getLoadedProg :: State -> LoadedProg
getLoadedProg (State (Just loaded_prog)) = loaded_prog
getLoadedProg (State Nothing) = noLoadedProg
