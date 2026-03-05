module Futhark.LSP.CommandType (CommandType (..)) where

data CommandType
  = CodeLens
  deriving (Show, Read, Enum, Bounded)
