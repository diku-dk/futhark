module Futhark.LSP.State
  ( State (..),
    emptyState,
  )
where

import Futhark.Compiler.Program (LoadedProg)

newtype State = State
  { stateProgram :: Maybe LoadedProg
  }

emptyState :: State
emptyState = State Nothing
