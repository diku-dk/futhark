{-# LANGUAGE TemplateHaskell #-}

-- | Code snippets used by the CUDA backend.
module Futhark.CodeGen.RTS.CUDA (preludeCU, preludeTensorCores, intTypesCU) where

import Data.FileEmbed
import Data.Text qualified as T

-- | @rts/cuda/prelude.cu@
preludeCU :: T.Text
preludeCU = $(embedStringFile "rts/cuda/prelude.cu")
{-# NOINLINE preludeCU #-}

-- TODO: Maybe its own file

-- | @rts/cuda/preludeTensorCores.cu@
preludeTensorCores :: T.Text
preludeTensorCores = $(embedStringFile "rts/cuda/preludeTensorCores.cu")
{-# NOINLINE preludeTensorCores #-}

intTypesCU :: T.Text
intTypesCU = $(embedStringFile "rts/cuda/int_types.cu")
{-# NOINLINE intTypesCU #-}
