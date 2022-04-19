-- | The language server state definition.
module Futhark.LSP.State
  ( State (..),
    emptyState,
    getStaleContent,
  )
where

import qualified Data.Map as M
import qualified Data.Text as T
import Futhark.Compiler.Program (LoadedProg)

-- | The state of the language server.
data State = State
  { -- | The loaded program.
    stateProgram :: Maybe LoadedProg,
    -- | The last succussful type-checked file contents.
    staleProgram :: M.Map FilePath T.Text
  }

-- | Initial state.
emptyState :: State
emptyState = State Nothing M.empty

-- | Get the contents of a stale (last succuessfully complied) file's contents.
getStaleContent :: State -> Maybe FilePath -> Maybe T.Text
getStaleContent state (Just file_path) = M.lookup file_path (staleProgram state)
getStaleContent _ _ = Nothing
