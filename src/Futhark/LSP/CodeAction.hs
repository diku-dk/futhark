{-# LANGUAGE ExplicitNamespaces #-}

module Futhark.LSP.CodeAction (getCodeActions) where

import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Futhark.LSP.State (State)
import Futhark.LSP.Tool (bindingsInRange)
import Language.LSP.Protocol.Types (CodeAction, Command, Range, type (|?))

getCodeActions :: Range -> State -> FilePath -> [Command |? CodeAction]
getCodeActions range state filepath =
  bindingsInRange range state filepath
    & fromMaybe []
    & _
