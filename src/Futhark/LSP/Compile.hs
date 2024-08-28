-- | Building blocks for "recompiling" (actually just type-checking)
-- the Futhark program managed by the language server.  The challenge
-- here is that if the program becomes type-invalid, we want to keep
-- the old state around.
module Futhark.LSP.Compile (tryTakeStateFromIORef, tryReCompile) where

import Colog.Core (logStringStderr, (<&))
import Control.Lens.Getter (view)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Futhark.Compiler.Program (LoadedProg, lpFilePaths, lpWarnings, noLoadedProg, reloadProg)
import Futhark.LSP.Diagnostic (diagnosticSource, maxDiagnostic, publishErrorDiagnostics, publishWarningDiagnostics)
import Futhark.LSP.State (State (..), emptyState, updateStaleContent, updateStaleMapping)
import Futhark.LSP.Tool (computeMapping)
import Language.Futhark.Warnings (listWarnings)
import Language.LSP.Protocol.Types
  ( filePathToUri,
    fromNormalizedFilePath,
    toNormalizedUri,
    uriToNormalizedFilePath,
  )
import Language.LSP.Server (LspT, flushDiagnosticsBySource, getVirtualFile, getVirtualFiles)
import Language.LSP.VFS (VFS, vfsMap, virtualFileText)

-- | Try to take state from IORef, if it's empty, try to compile.
tryTakeStateFromIORef :: IORef State -> Maybe FilePath -> LspT () IO State
tryTakeStateFromIORef state_mvar file_path = do
  old_state <- liftIO $ readIORef state_mvar
  case stateProgram old_state of
    Nothing -> do
      new_state <- tryCompile old_state file_path noLoadedProg
      liftIO $ writeIORef state_mvar new_state
      pure new_state
    Just prog -> do
      -- If this is in the context of some file that is not part of
      -- the program, try to reload the program from that file.
      let files = lpFilePaths prog
      state <- case file_path of
        Just file_path'
          | file_path' `notElem` files -> do
              logStringStderr <& ("File not part of program: " <> show file_path')
              logStringStderr <& ("Program contains: " <> show files)
              tryCompile old_state file_path noLoadedProg
        _ -> pure old_state
      liftIO $ writeIORef state_mvar state
      pure state

-- | Try to (re)-compile, replace old state if successful.
tryReCompile :: IORef State -> Maybe FilePath -> LspT () IO ()
tryReCompile state_mvar file_path = do
  logStringStderr <& "(Re)-compiling ..."
  old_state <- liftIO $ readIORef state_mvar
  let loaded_prog = getLoadedProg old_state
  new_state <- tryCompile old_state file_path loaded_prog
  case stateProgram new_state of
    Nothing -> do
      logStringStderr <& "Failed to (re)-compile, using old state or Nothing"
      logStringStderr <& "Computing PositionMapping for: " <> show file_path
      mapping <- computeMapping old_state file_path
      liftIO $ writeIORef state_mvar $ updateStaleMapping file_path mapping old_state
    Just _ -> do
      logStringStderr <& "(Re)-compile successful"
      liftIO $ writeIORef state_mvar new_state

-- | Try to compile, publish diagnostics on warnings and errors, return newly compiled state.
--  Single point where the compilation is done, and shouldn't be exported.
tryCompile :: State -> Maybe FilePath -> LoadedProg -> LspT () IO State
tryCompile _ Nothing _ = pure emptyState
tryCompile state (Just path) old_loaded_prog = do
  logStringStderr <& "Reloading program from " <> show path
  vfs <- getVirtualFiles
  res <- liftIO $ reloadProg old_loaded_prog [path] (transformVFS vfs) -- NOTE: vfs only keeps track of current opened files
  flushDiagnosticsBySource maxDiagnostic diagnosticSource
  case res of
    Right new_loaded_prog -> do
      publishWarningDiagnostics $ listWarnings $ lpWarnings new_loaded_prog
      maybe_virtual_file <- getVirtualFile $ toNormalizedUri $ filePathToUri path
      case maybe_virtual_file of
        Nothing -> pure $ State (Just new_loaded_prog) (staleData state) -- should never happen
        Just virtual_file ->
          pure $ updateStaleContent path virtual_file new_loaded_prog state
    -- Preserve files that have been opened should be enoguth.
    -- But still might need an update on re-compile logic, don't discard all state afterwards,
    -- try to compile from root file, if there is a depencency relatetion, improve performance and provide more dignostic.
    Left prog_error -> do
      logStringStderr <& "Compilation failed, publishing diagnostics"
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
    (view vfsMap vfs)

getLoadedProg :: State -> LoadedProg
getLoadedProg state = fromMaybe noLoadedProg (stateProgram state)
