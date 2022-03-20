module Futhark.LSP.Utils
  ( debug,
    State (..),
    emptyState,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Futhark.Compiler.Program (LoadedProg)
import System.Log.Logger (debugM)

debug :: Control.Monad.IO.Class.MonadIO m => String -> m ()
debug msg = liftIO $ debugM "futhark" msg

newtype State = State
  { stateProgram :: Maybe LoadedProg
  }

emptyState :: State
emptyState = State Nothing
