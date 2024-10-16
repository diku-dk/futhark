{-# LANGUAGE TemplateHaskell #-}

-- | Code snippets used by the CUDA backend.
module Futhark.CodeGen.RTS.CUDA (preludeCU, preludeMMM) where

import Data.FileEmbed
import Data.Text qualified as T

-- | @rts/cuda/prelude.cu@
preludeCU :: T.Text
preludeCU = $(embedStringFile "rts/cuda/prelude.cu")
{-# NOINLINE preludeCU #-}

-- | @rts/cuda/preludeMMM.cu@
preludeMMM :: T.Text
preludeMMM = $(embedStringFile "rts/cuda/preludeMMM.cu")
{-# NOINLINE preludeMMM #-}
