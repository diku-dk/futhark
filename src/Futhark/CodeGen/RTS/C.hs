{-# LANGUAGE TemplateHaskell #-}

-- | Code snippets used by the C backends.
module Futhark.CodeGen.RTS.C
  ( atomicsH,
    contextH,
    contextPrototypesH,
    copyH,
    freeListH,
    eventListH,
    gpuH,
    gpuPrototypesH,
    halfH,
    lockH,
    scalarF16H,
    scalarH,
    schedulerH,
    serverH,
    timingH,
    tuningH,
    utilH,
    valuesH,
    errorsH,
    cacheH,
    uniformH,
    ispcUtilH,
    backendsOpenclH,
    backendsCudaH,
    backendsHipH,
    backendsCH,
    backendsMulticoreH,
    backendsWebGPUH,
  )
where

import Data.FileEmbed
import Data.Text qualified as T

-- We mark everything here NOINLINE so that the dependent modules
-- don't have to be recompiled just because we change the RTS files.

-- | @rts/c/atomics.h@
atomicsH :: T.Text
atomicsH =
  -- The order matters, as e.g. atomics16.h is implemented in terms of 32-bit
  -- atomics.
  mconcat
    [ $(embedStringFile "rts/c/atomics64.h"),
      $(embedStringFile "rts/c/atomics32.h"),
      $(embedStringFile "rts/c/atomics16.h"),
      $(embedStringFile "rts/c/atomics8.h")
    ]
{-# NOINLINE atomicsH #-}

-- | @rts/c/uniform.h@
uniformH :: T.Text
uniformH = $(embedStringFile "rts/c/uniform.h")
{-# NOINLINE uniformH #-}

-- | @rts/c/free_list.h@
freeListH :: T.Text
freeListH = $(embedStringFile "rts/c/free_list.h")
{-# NOINLINE freeListH #-}

-- | @rts/c/event_list.h@
eventListH :: T.Text
eventListH = $(embedStringFile "rts/c/event_list.h")
{-# NOINLINE eventListH #-}

-- | @rts/c/gpu.h@
gpuH :: T.Text
gpuH = $(embedStringFile "rts/c/gpu.h")
{-# NOINLINE gpuH #-}

-- | @rts/c/gpu_prototypes.h@
gpuPrototypesH :: T.Text
gpuPrototypesH = $(embedStringFile "rts/c/gpu_prototypes.h")
{-# NOINLINE gpuPrototypesH #-}

-- | @rts/c/half.h@
halfH :: T.Text
halfH = $(embedStringFile "rts/c/half.h")
{-# NOINLINE halfH #-}

-- | @rts/c/lock.h@
lockH :: T.Text
lockH = $(embedStringFile "rts/c/lock.h")
{-# NOINLINE lockH #-}

-- | @rts/c/scalar_f16.h@
scalarF16H :: T.Text
scalarF16H = $(embedStringFile "rts/c/scalar_f16.h")
{-# NOINLINE scalarF16H #-}

-- | @rts/c/scalar.h@
scalarH :: T.Text
scalarH = $(embedStringFile "rts/c/scalar.h")
{-# NOINLINE scalarH #-}

-- | @rts/c/scheduler.h@
schedulerH :: T.Text
schedulerH = $(embedStringFile "rts/c/scheduler.h")
{-# NOINLINE schedulerH #-}

-- | @rts/c/server.h@
serverH :: T.Text
serverH = $(embedStringFile "rts/c/server.h")
{-# NOINLINE serverH #-}

-- | @rts/c/timing.h@
timingH :: T.Text
timingH = $(embedStringFile "rts/c/timing.h")
{-# NOINLINE timingH #-}

-- | @rts/c/tuning.h@
tuningH :: T.Text
tuningH = $(embedStringFile "rts/c/tuning.h")
{-# NOINLINE tuningH #-}

-- | @rts/c/util.h@
utilH :: T.Text
utilH = $(embedStringFile "rts/c/util.h")
{-# NOINLINE utilH #-}

-- | @rts/c/values.h@
valuesH :: T.Text
valuesH = $(embedStringFile "rts/c/values.h")
{-# NOINLINE valuesH #-}

-- | @rts/c/errors.h@
errorsH :: T.Text
errorsH = $(embedStringFile "rts/c/errors.h")
{-# NOINLINE errorsH #-}

-- | @rts/c/ispc_util.h@
ispcUtilH :: T.Text
ispcUtilH = $(embedStringFile "rts/c/ispc_util.h")
{-# NOINLINE ispcUtilH #-}

-- | @rts/c/cache.h@
cacheH :: T.Text
cacheH = $(embedStringFile "rts/c/cache.h")
{-# NOINLINE cacheH #-}

-- | @rts/c/context.h@
contextH :: T.Text
contextH = $(embedStringFile "rts/c/context.h")
{-# NOINLINE contextH #-}

-- | @rts/c/context_prototypes.h@
contextPrototypesH :: T.Text
contextPrototypesH = $(embedStringFile "rts/c/context_prototypes.h")
{-# NOINLINE contextPrototypesH #-}

-- | @rts/c/backends/opencl.h@
backendsOpenclH :: T.Text
backendsOpenclH = $(embedStringFile "rts/c/backends/opencl.h")
{-# NOINLINE backendsOpenclH #-}

-- | @rts/c/backends/cuda.h@
backendsCudaH :: T.Text
backendsCudaH = $(embedStringFile "rts/c/backends/cuda.h")
{-# NOINLINE backendsCudaH #-}

-- | @rts/c/backends/hip.h@
backendsHipH :: T.Text
backendsHipH = $(embedStringFile "rts/c/backends/hip.h")
{-# NOINLINE backendsHipH #-}

-- | @rts/c/backends/c.h@
backendsCH :: T.Text
backendsCH = $(embedStringFile "rts/c/backends/c.h")
{-# NOINLINE backendsCH #-}

-- | @rts/c/backends/multicore.h@
backendsMulticoreH :: T.Text
backendsMulticoreH = $(embedStringFile "rts/c/backends/multicore.h")
{-# NOINLINE backendsMulticoreH #-}

-- | @rts/c/backends/webgpu.h@
backendsWebGPUH :: T.Text
backendsWebGPUH = $(embedStringFile "rts/c/backends/webgpu.h")
{-# NOINLINE backendsWebGPUH #-}

-- | @rts/c/copy.h@
copyH :: T.Text
copyH = $(embedStringFile "rts/c/copy.h")
{-# NOINLINE copyH #-}
