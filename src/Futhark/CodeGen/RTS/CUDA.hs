{-# LANGUAGE TemplateHaskell #-}

-- | Code snippets used by the CUDA backend.
module Futhark.CodeGen.RTS.CUDA (preludeCU) where

import Data.FileEmbed
import Data.Text qualified as T

-- | @rts/cuda/prelude.cu@
preludeCU :: T.Text
preludeCU = $(embedStringFile "rts/cuda/prelude.cu")
{-# NOINLINE preludeCU #-}
