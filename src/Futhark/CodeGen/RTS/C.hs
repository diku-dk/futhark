{-# LANGUAGE TemplateHaskell #-}

-- | Code snippets used by the C backends.
module Futhark.CodeGen.RTS.C
  ( atomicsH,
    contextH,
    contextPrototypesH,
    cudaH,
    freeListH,
    halfH,
    lockH,
    openclH,
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
  )
where

import Data.FileEmbed
import Data.Text qualified as T

-- We mark everything here NOINLINE so that the dependent modules
-- don't have to be recompiled just because we change the RTS files.

-- | @rts/c/atomics.h@
atomicsH :: T.Text
atomicsH = $(embedStringFile "rts/c/atomics.h")
{-# NOINLINE atomicsH #-}

-- | @rts/c/uniform.h@
uniformH :: T.Text
uniformH = $(embedStringFile "rts/c/uniform.h")
{-# NOINLINE uniformH #-}

-- | @rts/c/cuda.h@
cudaH :: T.Text
cudaH = $(embedStringFile "rts/c/cuda.h")
{-# NOINLINE cudaH #-}

-- | @rts/c/free_list.h@
freeListH :: T.Text
freeListH = $(embedStringFile "rts/c/free_list.h")
{-# NOINLINE freeListH #-}

-- | @rts/c/half.h@
halfH :: T.Text
halfH = $(embedStringFile "rts/c/half.h")
{-# NOINLINE halfH #-}

-- | @rts/c/lock.h@
lockH :: T.Text
lockH = $(embedStringFile "rts/c/lock.h")
{-# NOINLINE lockH #-}

-- | @rts/c/opencl.h@
openclH :: T.Text
openclH = $(embedStringFile "rts/c/opencl.h")
{-# NOINLINE openclH #-}

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
