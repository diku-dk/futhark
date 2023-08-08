{-# LANGUAGE TemplateHaskell #-}

-- | Code snippets used by the OpenCL and CUDA backends.
module Futhark.CodeGen.RTS.OpenCL
  ( transposeCL,
    preludeCL,
    copyCL,
  )
where

import Data.FileEmbed
import Data.Text qualified as T

-- | @rts/opencl/transpose.cl@
transposeCL :: T.Text
transposeCL = $(embedStringFile "rts/opencl/transpose.cl")
{-# NOINLINE transposeCL #-}

-- | @rts/opencl/prelude.cl@
preludeCL :: T.Text
preludeCL = $(embedStringFile "rts/opencl/prelude.cl")
{-# NOINLINE preludeCL #-}

-- | @rts/opencl/copy.cl@
copyCL :: T.Text
copyCL = $(embedStringFile "rts/opencl/copy.cl")
{-# NOINLINE copyCL #-}
