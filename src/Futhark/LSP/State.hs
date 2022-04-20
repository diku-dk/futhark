-- | The language server state definition.
module Futhark.LSP.State
  ( State (..),
    emptyState,
    getStaleContent,
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
    staleProgram :: M.Map FilePath VirtualFile
  }

-- | Initial state.
emptyState :: State
emptyState = State Nothing M.empty

-- | Get the contents of a stale (last succuessfully complied) file's contents.
getStaleContent :: State -> Maybe FilePath -> Maybe VirtualFile
getStaleContent state (Just file_path) = M.lookup file_path (staleProgram state)
getStaleContent _ _ = Nothing
