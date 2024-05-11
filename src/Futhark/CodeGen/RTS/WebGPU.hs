{-# LANGUAGE TemplateHaskell #-}

-- | Code snippets used by the WebGPU backend.
module Futhark.CodeGen.RTS.WebGPU
  ( serverWsJs,
    utilJs,
    valuesJs,
    wrappersJs
  )
where

import Data.FileEmbed
import Data.Text qualified as T

-- | @rts/webgpu/server_ws.js@
serverWsJs :: T.Text
serverWsJs = $(embedStringFile "rts/webgpu/server_ws.js")
{-# NOINLINE serverWsJs #-}

-- | @rts/webgpu/util.js@
utilJs :: T.Text
utilJs = $(embedStringFile "rts/webgpu/util.js")
{-# NOINLINE utilJs #-}

-- | @rts/webgpu/values.js@
valuesJs :: T.Text
valuesJs = $(embedStringFile "rts/webgpu/values.js")
{-# NOINLINE valuesJs #-}

-- | @rts/webgpu/wrappers.js@
wrappersJs :: T.Text
wrappersJs = $(embedStringFile "rts/webgpu/wrappers.js")
{-# NOINLINE wrappersJs #-}
