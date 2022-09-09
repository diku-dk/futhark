-- | The language server state definition.
module Futhark.LSP.State
  ( State (..),
    emptyState,
    getStaleContent,
    getStaleMapping,
    updateStaleContent,
    updateStaleMapping,
  )
where

import Data.Map qualified as M
import Futhark.Compiler.Program (LoadedProg)
import Futhark.LSP.PositionMapping (PositionMapping, StaleFile (..))
import Language.LSP.VFS (VirtualFile)

-- | The state of the language server.
data State = State
  { -- | The loaded program.
    stateProgram :: Maybe LoadedProg,
    -- | The stale data, stored to provide PositionMapping when requested.
    -- All files that have been opened have an entry.
    staleData :: M.Map FilePath StaleFile
  }

-- | Initial state.
emptyState :: State
emptyState = State Nothing M.empty

-- | Get the contents of a stale (last successfully complied) file's contents.
getStaleContent :: State -> FilePath -> Maybe VirtualFile
getStaleContent state file_path = (Just . staleContent) =<< M.lookup file_path (staleData state)

-- | Get the PositionMapping for a file.
getStaleMapping :: State -> FilePath -> Maybe PositionMapping
getStaleMapping state file_path = staleMapping =<< M.lookup file_path (staleData state)

-- | Update the state with another pair of file_path and contents.
-- Could do a clean up becausae there is no need to store files that are not in lpFilePaths prog.
updateStaleContent :: FilePath -> VirtualFile -> LoadedProg -> State -> State
updateStaleContent file_path file_content loadedProg state =
  -- NOTE: insert will replace the old value if the key already exists.
  -- updateStaleContent is only called after a successful type-check,
  -- so the PositionsMapping should be Nothing here, it's calculated after failed type-check.
  State (Just loadedProg) (M.insert file_path (StaleFile file_content Nothing) (staleData state))

-- | Update the state with another pair of file_path and PositionMapping.
updateStaleMapping :: Maybe FilePath -> Maybe PositionMapping -> State -> State
updateStaleMapping (Just file_path) mapping state = do
  case M.lookup file_path (staleData state) of
    Nothing -> state -- Only happends when the file have never been successfully type-checked before.
    Just (StaleFile file_content _mapping) ->
      State (stateProgram state) (M.insert file_path (StaleFile file_content mapping) (staleData state))
updateStaleMapping _ _ state = state
