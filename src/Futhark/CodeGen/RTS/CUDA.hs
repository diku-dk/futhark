{-# LANGUAGE TemplateHaskell #-}

-- | Code snippets used by the CUDA backend.
module Futhark.CodeGen.RTS.CUDA (preludeCU, preludeTC, intTypesCU) where

import Data.FileEmbed
import Data.Text qualified as T

-- | @rts/cuda/prelude.cu@
preludeCU :: T.Text
preludeCU = $(embedStringFile "rts/cuda/prelude.cu")
{-# NOINLINE preludeCU #-}

-- TODO: Maybe its own file

-- | @rts/cuda/preludeTC.cu@
preludeTC :: T.Text
preludeTC = $(embedStringFile "rts/cuda/prelude_tc.cu")
{-# NOINLINE preludeTC #-}

intTypesCU :: T.Text
intTypesCU = $(embedStringFile "rts/cuda/int_types.cu")
{-# NOINLINE intTypesCU #-}
