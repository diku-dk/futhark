{-# LANGUAGE TemplateHaskell #-}
module Futhark.CodeGen.Backends.GenericPython.Definitions
  ( pyFunctions
  , pyUtility
  , pyTestMain
  ) where

import Data.FileEmbed

pyFunctions :: String
pyFunctions = $(embedStringFile "rts/python/memory.py")

pyUtility :: String
pyUtility = $(embedStringFile "rts/python/scalar.py")

pyTestMain :: String
pyTestMain = $(embedStringFile "rts/python/reader.py")
