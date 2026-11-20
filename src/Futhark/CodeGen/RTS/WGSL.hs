{-# LANGUAGE TemplateHaskell #-}

-- | Code snippets used by the WebGPU backend as part of WGSL shaders.
module Futhark.CodeGen.RTS.WGSL
  ( atomics,
    wgslPrelude,
    lmadCopy,
    mapTranspose,
    mapTransposeLowHeight,
    mapTransposeLowWidth,
    mapTransposeSmall,
    mapTransposeLarge,
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
atomicsGlobal :: T.Text
atomicsGlobal = $(embedStringFile "rts/wgsl/atomics.wgsl")
{-# NOINLINE atomicsGlobal #-}

-- | The atomic operations of @rts/wgsl/atomics.wgsl@, for both storage buffers
-- ('_global') and workgroup memory ('_shared'). WGSL functions cannot be
-- polymorphic in the address space of a pointer, so the latter are copies of
-- the former with a different pointer type.
atomics :: T.Text
atomics =
  atomicsGlobal
    <> T.replace
      "ptr<storage, atomic<i32>, read_write>"
      "ptr<workgroup, atomic<i32>>"
      (T.replace "_global(" "_shared(" atomicsGlobal)

wgslPrelude :: T.Text
wgslPrelude =
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
lmadCopy :: T.Text
lmadCopy = $(embedStringFile "rts/wgsl/lmad_copy.wgsl")
{-# NOINLINE lmadCopy #-}

-- | @rts/wgsl/map_transpose.wgsl@
mapTranspose :: T.Text
mapTranspose = $(embedStringFile "rts/wgsl/map_transpose.wgsl")
{-# NOINLINE mapTranspose #-}

-- | @rts/wgsl/map_transpose_low_height.wgsl@
mapTransposeLowHeight :: T.Text
mapTransposeLowHeight = $(embedStringFile "rts/wgsl/map_transpose_low_height.wgsl")
{-# NOINLINE mapTransposeLowHeight #-}

-- | @rts/wgsl/map_transpose_low_width.wgsl@
mapTransposeLowWidth :: T.Text
mapTransposeLowWidth = $(embedStringFile "rts/wgsl/map_transpose_low_width.wgsl")
{-# NOINLINE mapTransposeLowWidth #-}

-- | @rts/wgsl/map_transpose_small.wgsl@
mapTransposeSmall :: T.Text
mapTransposeSmall = $(embedStringFile "rts/wgsl/map_transpose_small.wgsl")
{-# NOINLINE mapTransposeSmall #-}

-- | @rts/wgsl/map_transpose_large.wgsl@
mapTransposeLarge :: T.Text
mapTransposeLarge = $(embedStringFile "rts/wgsl/map_transpose_large.wgsl")
{-# NOINLINE mapTransposeLarge #-}
