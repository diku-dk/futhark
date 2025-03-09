{-# LANGUAGE TemplateHaskell #-}

-- | Code snippets used by the WebGPU backend as part of WGSL shaders.
module Futhark.CodeGen.RTS.WGSL
  ( scalar,
    scalar8,
    scalar16,
    scalar32,
    scalar64,
    builtin_kernels,
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

-- | @rts/wgsl/builtin_kernels.wgsl@
builtin_kernels :: T.Text
builtin_kernels = T.concat
  [ header
  , T.concat
    [ genTransposeKernel "1b" "i8" True
    , genTransposeKernel "2b" "i16" True
    , genTransposeKernel "4b" "i32" False
    , genTransposeKernel "8b" "i64" False
    ]
  , footer
  ]
  where
    content = $(embedStringFile "rts/wgsl/builtin_kernels.wgsl")
    (header, middle) = T.breakOn "// Begin of builtin kernel group" content
    (kernel, footer) = T.breakOn "// End of builtin kernel group" middle
    genTransposeKernel name elemType atomic =
      let baseKernel = if atomic
                       then T.replace "<ELEM_TYPE>" "<atomic<ELEM_TYPE>>" kernel
                       else kernel
      in T.replace "NAME" name $ T.replace "ELEM_TYPE" elemType baseKernel
{-# NOINLINE builtin_kernels #-}
