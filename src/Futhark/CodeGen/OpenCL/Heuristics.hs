module Futhark.CodeGen.OpenCL.Heuristics
       ( SizeHeuristic (..)
       , DeviceType (..)
       , WhichSize (..)
       , HeuristicValue (..)
       , sizeHeuristicsTable
       )
       where

-- Some OpenCL platforms have a SIMD/warp/wavefront-based execution
-- model that execute groups of threads in lockstep, permitting us to
-- perform cross-thread synchronisation within each such group without
-- the use of barriers.  Unfortunately, there seems to be no reliable
-- way to query these sizes at runtime.  Instead, we use this table to
-- figure out which size we should use for a specific platform and
-- device.  If nothing matches here, the wave size should be set to
-- one.
--
-- We also use this to select reasonable default group sizes and group
-- counts.

-- | The type of OpenCL device that this heuristic applies to.
data DeviceType = DeviceCPU | DeviceGPU

-- | The value supplies by a heuristic can be a constant, or inferred
-- from some device information.
data HeuristicValue = HeuristicConst Int
                    | HeuristicDeviceInfo String

-- | A size that can be assigned a default.
data WhichSize = LockstepWidth | NumGroups | GroupSize | TileSize | Threshold

-- | A heuristic for setting the default value for something.
data SizeHeuristic =
    SizeHeuristic { platformName :: String
                  , deviceType :: DeviceType
                  , heuristicSize :: WhichSize
                  , heuristicValue :: HeuristicValue
                  }

-- | All of our heuristics.
sizeHeuristicsTable :: [SizeHeuristic]
sizeHeuristicsTable =
  [ SizeHeuristic "NVIDIA CUDA" DeviceGPU LockstepWidth $ HeuristicConst 32
  , SizeHeuristic "AMD Accelerated Parallel Processing" DeviceGPU LockstepWidth $ HeuristicConst 32
  , SizeHeuristic "" DeviceGPU LockstepWidth $ HeuristicConst 1
  , SizeHeuristic "" DeviceGPU NumGroups $ HeuristicConst 256
  , SizeHeuristic "" DeviceGPU GroupSize $ HeuristicConst 256
  , SizeHeuristic "" DeviceGPU TileSize $ HeuristicConst 32
  , SizeHeuristic "" DeviceGPU Threshold $ HeuristicConst $ 32*1024

  , SizeHeuristic "" DeviceCPU LockstepWidth $ HeuristicConst 1
  , SizeHeuristic "" DeviceCPU NumGroups $ HeuristicDeviceInfo "MAX_COMPUTE_UNITS"
  , SizeHeuristic "" DeviceCPU GroupSize $ HeuristicConst 32
  , SizeHeuristic "" DeviceCPU TileSize $ HeuristicConst 4
  , SizeHeuristic "" DeviceCPU Threshold $ HeuristicDeviceInfo "MAX_COMPUTE_UNITS"
  ]
