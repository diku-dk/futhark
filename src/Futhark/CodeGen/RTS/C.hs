{-# LANGUAGE TemplateHaskell #-}

-- | Code snippets used by the C backends.
module Futhark.CodeGen.RTS.C
  ( atomicsH,
    chaselevH,
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
  )
where

import Data.FileEmbed
import qualified Data.Text as T

-- | @rts/c/atomics.h@
atomicsH :: T.Text
atomicsH = $(embedStringFile "rts/c/atomics.h")

-- | @rts/c/chaselev.h@
chaselevH :: T.Text
chaselevH = $(embedStringFile "rts/c/chaselev.h")

-- | @rts/c/cuda.h@
cudaH :: T.Text
cudaH = $(embedStringFile "rts/c/cuda.h")

-- | @rts/c/free_list.h@
freeListH :: T.Text
freeListH = $(embedStringFile "rts/c/free_list.h")

-- | @rts/c/half.h@
halfH :: T.Text
halfH = $(embedStringFile "rts/c/half.h")

-- | @rts/c/lock.h@
lockH :: T.Text
lockH = $(embedStringFile "rts/c/lock.h")

-- | @rts/c/opencl.h@
openclH :: T.Text
openclH = $(embedStringFile "rts/c/opencl.h")

-- | @rts/c/scalar_f16.h@
scalarF16H :: T.Text
scalarF16H = $(embedStringFile "rts/c/scalar_f16.h")

-- | @rts/c/scalar.h@
scalarH :: T.Text
scalarH = $(embedStringFile "rts/c/scalar.h")

-- | @rts/c/scheduler.h@
schedulerH :: T.Text
schedulerH = $(embedStringFile "rts/c/scheduler.h")

-- | @rts/c/server.h@
serverH :: T.Text
serverH = $(embedStringFile "rts/c/server.h")

-- | @rts/c/timing.h@
timingH :: T.Text
timingH = $(embedStringFile "rts/c/timing.h")

-- | @rts/c/tuning.h@
tuningH :: T.Text
tuningH = $(embedStringFile "rts/c/tuning.h")

-- | @rts/c/util.h@
utilH :: T.Text
utilH = $(embedStringFile "rts/c/util.h")

-- | @rts/c/values.h@
valuesH :: T.Text
valuesH = $(embedStringFile "rts/c/values.h")
