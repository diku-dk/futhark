{-# LANGUAGE TemplateHaskell #-}

-- | Code snippets used by the WebGPU backend.
module Futhark.CodeGen.RTS.WebGPU
  ( serverWsJs,
    utilJs,
    wrappersJs,
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

-- | @rts/webgpu/wrappers.js@
wrappersJs :: T.Text
wrappersJs = $(embedStringFile "rts/webgpu/wrappers.js")
{-# NOINLINE wrappersJs #-}
