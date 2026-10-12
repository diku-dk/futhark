{-# LANGUAGE TemplateHaskell #-}

-- | Code snippets used by the Metal backend.
module Futhark.CodeGen.RTS.Metal (preludeMetal) where

import Data.FileEmbed
import Data.Text qualified as T

-- | @rts/metal/prelude.metal@
preludeMetal :: T.Text
preludeMetal = $(embedStringFile "rts/metal/prelude.metal")
{-# NOINLINE preludeMetal #-}
