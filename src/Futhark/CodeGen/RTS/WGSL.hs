{-# LANGUAGE TemplateHaskell #-}

-- | Code snippets used by the WebGPU backend as part of WGSL shaders.
module Futhark.CodeGen.RTS.WGSL
  ( scalar,
    scalar8,
    scalar16,
    scalar32,
    scalar64,
    atomics,
    wgsl_prelude,
    lmad_copy,
    map_transpose,
    map_transpose_low_height,
    map_transpose_low_width,
    map_transpose_small,
    map_transpose_large,
  )
where

import Data.FileEmbed
import Data.Text qualified as T

-- | @rts/wgsl/scalar.wgsl@
scalar :: T.Text
scalar = $(embedStringFile "rts/wgsl/scalar.wgsl")
{-# NOINLINE scalar #-}

-- | @rts/wgsl/scalar8.wgsl@
scalar8 :: T.Text
scalar8 = $(embedStringFile "rts/wgsl/scalar8.wgsl")
{-# NOINLINE scalar8 #-}

-- | @rts/wgsl/scalar16.wgsl@
scalar16 :: T.Text
scalar16 = $(embedStringFile "rts/wgsl/scalar16.wgsl")
{-# NOINLINE scalar16 #-}

-- | @rts/wgsl/scalar32.wgsl@
scalar32 :: T.Text
scalar32 = $(embedStringFile "rts/wgsl/scalar32.wgsl")
{-# NOINLINE scalar32 #-}

-- | @rts/wgsl/scalar64.wgsl@
scalar64 :: T.Text
scalar64 = $(embedStringFile "rts/wgsl/scalar64.wgsl")
{-# NOINLINE scalar64 #-}

-- | @rts/wgsl/atomics.wgsl@
atomics :: T.Text
atomics = $(embedStringFile "rts/wgsl/atomics.wgsl")
{-# NOINLINE atomics #-}

wgsl_prelude :: T.Text
wgsl_prelude =
  -- Put scalar32 in front of the other integer types since they are all
  -- internally represented using i32.
  mconcat
    [ "enable f16;\n",
      scalar,
      scalar32,
      scalar8,
      scalar16,
      scalar64,
      atomics
    ]

-- | @rts/wgsl/lmad_copy.wgsl@
lmad_copy :: T.Text
lmad_copy = $(embedStringFile "rts/wgsl/lmad_copy.wgsl")
{-# NOINLINE lmad_copy #-}

-- | @rts/wgsl/map_transpose.wgsl@
map_transpose :: T.Text
map_transpose = $(embedStringFile "rts/wgsl/map_transpose.wgsl")
{-# NOINLINE map_transpose #-}

-- | @rts/wgsl/map_transpose_low_height.wgsl@
map_transpose_low_height :: T.Text
map_transpose_low_height = $(embedStringFile "rts/wgsl/map_transpose_low_height.wgsl")
{-# NOINLINE map_transpose_low_height #-}

-- | @rts/wgsl/map_transpose_low_width.wgsl@
map_transpose_low_width :: T.Text
map_transpose_low_width = $(embedStringFile "rts/wgsl/map_transpose_low_width.wgsl")
{-# NOINLINE map_transpose_low_width #-}

-- | @rts/wgsl/map_transpose_small.wgsl@
map_transpose_small :: T.Text
map_transpose_small = $(embedStringFile "rts/wgsl/map_transpose_small.wgsl")
{-# NOINLINE map_transpose_small #-}

-- | @rts/wgsl/map_transpose_large.wgsl@
map_transpose_large :: T.Text
map_transpose_large = $(embedStringFile "rts/wgsl/map_transpose_large.wgsl")
{-# NOINLINE map_transpose_large #-}
