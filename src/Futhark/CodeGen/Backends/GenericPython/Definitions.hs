{-# LANGUAGE TemplateHaskell #-}
module Futhark.CodeGen.Backends.GenericPython.Definitions
  ( pyFunctions
  , pyUtility
  , pyReader
  ) where

import Data.FileEmbed

pyFunctions :: String
pyFunctions = $(embedStringFile "rts/python/memory.py")

pyUtility :: String
pyUtility = $(embedStringFile "rts/python/scalar.py")

pyReader :: String
pyReader = $(embedStringFile "rts/python/reader.py")
