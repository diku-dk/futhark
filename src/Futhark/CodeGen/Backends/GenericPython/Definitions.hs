{-# LANGUAGE TemplateHaskell #-}
module Futhark.CodeGen.Backends.GenericPython.Definitions
  ( pyFunctions
  , pyUtility
  , pyValues
  , pyPanic
  , pyTuning
  ) where

import Data.FileEmbed

pyFunctions :: String
pyFunctions = $(embedStringFile "rts/python/memory.py")

pyUtility :: String
pyUtility = $(embedStringFile "rts/python/scalar.py")

pyValues :: String
pyValues = $(embedStringFile "rts/python/values.py")

pyPanic :: String
pyPanic = $(embedStringFile "rts/python/panic.py")

pyTuning :: String
pyTuning = $(embedStringFile "rts/python/tuning.py")
