{-# LANGUAGE TemplateHaskell #-}

-- | Code snippets used by the Python backends.
module Futhark.CodeGen.RTS.Python
  ( memoryPy,
    openclPy,
    panicPy,
    scalarPy,
    serverPy,
    tuningPy,
    valuesPy,
  )
where

import Data.FileEmbed
import Data.Text qualified as T

-- | @rts/python/memory.py@
memoryPy :: T.Text
memoryPy = $(embedStringFile "rts/python/memory.py")

-- | @rts/python/opencl.py@
openclPy :: T.Text
openclPy = $(embedStringFile "rts/python/opencl.py")

-- | @rts/python/panic.py@
panicPy :: T.Text
panicPy = $(embedStringFile "rts/python/panic.py")

-- | @rts/python/scalar.py@
scalarPy :: T.Text
scalarPy = $(embedStringFile "rts/python/scalar.py")

-- | @rts/python/server.py@
serverPy :: T.Text
serverPy = $(embedStringFile "rts/python/server.py")

-- | @rts/python/tuning.py@
tuningPy :: T.Text
tuningPy = $(embedStringFile "rts/python/tuning.py")

-- | @rts/python/values.py@
valuesPy :: T.Text
valuesPy = $(embedStringFile "rts/python/values.py")
