{-# LANGUAGE TemplateHaskell #-}

-- | Code snippets used by the OpenCL and CUDA backends.
module Futhark.CodeGen.RTS.OpenCL (transposeCL) where

import Data.FileEmbed
import Data.Text qualified as T

-- | @rts/opencl/transpose.cl@
transposeCL :: T.Text
transposeCL = $(embedStringFile "rts/opencl/transpose.cl")
{-# NOINLINE transposeCL #-}
