-- | The language server state definition.
module Futhark.LSP.State
  ( State (..),
    emptyState,
  )
where

import Futhark.Compiler.Program (LoadedProg)

-- | The state of the language server.
newtype State = State
  { -- | The loaded program.
    stateProgram :: Maybe LoadedProg
  }

-- | Initial state.
emptyState :: State
emptyState = State Nothing
