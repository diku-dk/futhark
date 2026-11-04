{-# LANGUAGE TemplateHaskell #-}

-- | Code snippets used by the WebGPU backend.
module Futhark.CodeGen.RTS.WebGPU
  ( serverWsJs,
  )
where

import Data.FileEmbed
import Data.Text qualified as T

-- | @rts/webgpu/server_ws.js@
serverWsJs :: T.Text
serverWsJs = $(embedStringFile "rts/webgpu/server_ws.js")
{-# NOINLINE serverWsJs #-}
