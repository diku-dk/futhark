-- | Building blocks for "recompiling" (actually just type-checking)
-- the Futhark program managed by the language server.  The challenge
-- here is that if the program becomes type-invalid, we want to keep
-- the old state around.
module Futhark.LSP.Compile (tryTakeStateFromIORef, tryReCompile) where

import Colog.Core (Severity (Debug, Warning), (<&))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.Maybe (fromMaybe, isJust)
import Futhark.Compiler.Program (LoadedProg, lpFilePaths, lpWarnings, noLoadedProg, reloadProg)
import Futhark.LSP.Diagnostic (diagnosticSource, maxDiagnostic, publishErrorDiagnostics, publishWarningDiagnostics)
import Futhark.LSP.State (State (..), updateStaleContent, updateStaleMapping)
import Futhark.LSP.Tool (computeMapping, logWithSeverity, transformVFS)
import Futhark.Util (showText)
import Language.Futhark.Warnings (listWarnings)
import Language.LSP.Protocol.Types
  ( filePathToUri,
    toNormalizedUri,
  )
import Language.LSP.Server (LspT, flushDiagnosticsBySource, getVirtualFile, getVirtualFiles)
import Language.LSP.VFS (virtualFileVersion)

-- | Get the current state.  If the given file is not part of the
-- current program, first try to compile the program rooted at that
-- file, unless that already failed for this version of the file.
tryTakeStateFromIORef :: IORef State -> Maybe FilePath -> LspT () IO State
tryTakeStateFromIORef state_mvar Nothing = liftIO $ readIORef state_mvar
tryTakeStateFromIORef state_mvar (Just file_path) = do
  old_state <- liftIO $ readIORef state_mvar
  case stateProgram old_state of
    Just prog
      | file_path `elem` lpFilePaths prog -> pure old_state
      | otherwise -> do
          logWithSeverity Warning <& ("File not part of program: " <> showText file_path)
          logWithSeverity Warning <& ("Program contains: " <> showText (lpFilePaths prog))
          compileUnlessFailed old_state
    Nothing -> compileUnlessFailed old_state
  where
    compileUnlessFailed old_state = do
      attempt <- compileAttempt file_path
      if isJust attempt && attempt == stateFailed old_state
        then pure old_state
        else do
          new_state <- either id id <$> tryCompile old_state file_path noLoadedProg
          liftIO $ writeIORef state_mvar new_state
          pure new_state

-- | Try to (re)-compile, replace old state if successful.
tryReCompile :: IORef State -> Maybe FilePath -> LspT () IO ()
tryReCompile _ Nothing = pure ()
tryReCompile state_mvar (Just file_path) = do
  logWithSeverity Debug <& "(Re)-compiling ..."
  old_state <- liftIO $ readIORef state_mvar
  result <- tryCompile old_state file_path $ getLoadedProg old_state
  case result of
    Left failed_state -> do
      logWithSeverity Debug <& "Failed to (re)-compile, using old state or Nothing"
      logWithSeverity Debug <& "Computing PositionMapping for: " <> showText file_path
      mapping <- computeMapping failed_state (Just file_path)
      liftIO $ writeIORef state_mvar $ updateStaleMapping (Just file_path) mapping failed_state
    Right new_state -> do
      logWithSeverity Debug <& "(Re)-compile successful"
      liftIO $ writeIORef state_mvar new_state

-- | The file and its current version, as recorded in 'stateFailed'.
compileAttempt :: FilePath -> LspT () IO (Maybe (FilePath, Int32))
compileAttempt file_path =
  fmap ((file_path,) . virtualFileVersion)
    <$> getVirtualFile (toNormalizedUri $ filePathToUri file_path)

-- | Try to compile and publish diagnostics on warnings and errors.
-- Returns the new state on success, and on failure the old state
-- with the failed attempt recorded.  Single point where the
-- compilation is done, and shouldn't be exported.
tryCompile :: State -> FilePath -> LoadedProg -> LspT () IO (Either State State)
tryCompile state path old_loaded_prog = do
  logWithSeverity Debug <& "Reloading program from " <> showText path
  vfs <- getVirtualFiles
  res <- liftIO $ reloadProg old_loaded_prog [path] (transformVFS vfs) -- NOTE: vfs only keeps track of current opened files
  flushDiagnosticsBySource maxDiagnostic diagnosticSource
  case res of
    Right new_loaded_prog -> do
      logWithSeverity Debug <& "Successfully compiled, publishing warnings"
      publishWarningDiagnostics $ listWarnings $ lpWarnings new_loaded_prog
      maybe_virtual_file <- getVirtualFile $ toNormalizedUri $ filePathToUri path
      let new_state = case maybe_virtual_file of
            Nothing -> state {stateProgram = Just new_loaded_prog} -- should never happen
            Just virtual_file -> updateStaleContent path virtual_file new_loaded_prog state
      pure $ Right new_state {stateFailed = Nothing}
    Left prog_error -> do
      logWithSeverity Debug <& "Compilation failed, publishing diagnostics"
      publishErrorDiagnostics prog_error
      attempt <- compileAttempt path
      pure $ Left state {stateFailed = attempt}

getLoadedProg :: State -> LoadedProg
getLoadedProg state = fromMaybe noLoadedProg (stateProgram state)
