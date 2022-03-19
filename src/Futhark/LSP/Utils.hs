module Futhark.LSP.Utils
  ( debug,
    State (..),
    emptyState,
    emptySemanticTokens,
  )
where

import Control.Monad.IO.Class (liftIO)
import Futhark.Compiler.Program (LoadedProg)
import Language.LSP.Types (List (List), SemanticTokens (..))
import System.Log.Logger (debugM)

debug msg = liftIO $ debugM "futhark" msg

newtype State = State
  { stateProgram :: Maybe LoadedProg
  }

emptyState :: State
emptyState = State Nothing

emptySemanticTokens :: SemanticTokens
emptySemanticTokens = SemanticTokens Nothing (List [0, 0, 3, 1, 0])
