-- | The language server state definition.
module Futhark.LSP.State
  ( State (State, stateProgram),
    emptyState,
    getStaleContent,
    updateStaleContent,
  )
where

import qualified Data.Map as M
import Futhark.Compiler.Program (LoadedProg)
import Language.LSP.VFS (VirtualFile)

-- | The state of the language server.
data State = State
  { -- | The loaded program.
    stateProgram :: Maybe LoadedProg,
    -- | The last succussful type-checked file contents.
    -- Using VirtualFile type for convenience, we just need {version, content}
    staleProgram :: M.Map FilePath VirtualFile
  }

-- | Initial state.
emptyState :: State
emptyState = State Nothing M.empty

-- | Get the contents of a stale (last succuessfully complied) file's contents.
getStaleContent :: State -> Maybe FilePath -> Maybe VirtualFile
getStaleContent state (Just file_path) = M.lookup file_path (staleProgram state)
getStaleContent _ _ = Nothing

-- | Update the state with another pair of file_path and contents.
-- Could do a clean up becausae there is no need to store files that are not in lpFilePaths prog.
updateStaleContent :: FilePath -> VirtualFile -> State -> State
updateStaleContent file_path virtual_file state =
  State (stateProgram state) (M.insert file_path virtual_file (staleProgram state))
