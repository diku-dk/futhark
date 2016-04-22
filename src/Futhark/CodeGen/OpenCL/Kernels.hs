{-# LANGUAGE QuasiQuotes #-}
module Futhark.CodeGen.OpenCL.Kernels
       ( LockstepWidthHeuristic (..)
       , DeviceType (..)
       , lockstepWidthHeuristicsTable

       , mapTranspose
       )
       where

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.OpenCL as C

-- Some OpenCL platforms have a SIMD/warp/wavefront-based execution
-- model that execute groups of threads in lockstep, permitting us to
-- perform cross-thread synchronisation within each such group without
-- the use of barriers.  Unfortunately, there seems to be no reliable
-- way to query these sizes at runtime.  Instead, we use this table to
-- figure out which size we should use for a specific platform and
-- device.  If nothing matches here, the wave size should be set to
-- one.

-- | The type of OpenCL device that this heuristic applies to.
data DeviceType = DeviceCPU | DeviceGPU

-- | A rule for picking the lockstep width.  If the platform and device
-- type matches, then the lockstep width should be set to the given
-- integer.
data LockstepWidthHeuristic =
  LockstepWidthHeuristic { platformName :: String
                         , deviceType :: DeviceType
                         , lockstepWidth :: Int
                         }

-- | All of our heuristics.
lockstepWidthHeuristicsTable :: [LockstepWidthHeuristic]
lockstepWidthHeuristicsTable =
  [ LockstepWidthHeuristic "NVIDIA CUDA" DeviceGPU 32
  , LockstepWidthHeuristic "AMD Accelerated Parallel Processing" DeviceGPU 64
  ]

mapTranspose :: C.ToIdent a => a -> C.Type -> C.Func
mapTranspose kernel_name elem_type =
  [C.cfun|
  // This kernel is optimized to ensure all global reads and writes are coalesced,
  // and to avoid bank conflicts in shared memory.  The shared memory array is sized
  // to (BLOCK_DIM+1)*BLOCK_DIM.  This pads each row of the 2D block in shared memory
  // so that bank conflicts do not occur when threads address the array column-wise.
  //
  // Note that total_size may not equal width*height if we are dealing with
  // a truncated array - this happens sometimes for coalescing optimisations.
  __kernel void $id:kernel_name(__global $ty:elem_type *odata,
                                uint odata_offset,
                                __global $ty:elem_type *idata,
                                uint idata_offset,
                                uint width,
                                uint height,
                                uint total_size,
                                __local $ty:elem_type* block) {
    uint x_index;
    uint y_index;
    uint our_array_offset;

    // Adjust the input and output arrays with the basic offset.
    odata += odata_offset/sizeof($ty:elem_type);
    idata += idata_offset/sizeof($ty:elem_type);

    // Adjust the input and output arrays for the third dimension.
    our_array_offset = get_global_id(2) * width * height;
    odata += our_array_offset;
    idata += our_array_offset;

    // read the matrix tile into shared memory
    x_index = get_global_id(0);
    y_index = get_global_id(1);

    uint index_in = y_index * width + x_index;

    if(x_index < width && y_index < height && index_in < total_size)
    {
        block[get_local_id(1)*(FUT_BLOCK_DIM+1)+get_local_id(0)] = idata[index_in];
    }

    barrier(CLK_LOCAL_MEM_FENCE);

    // Write the transposed matrix tile to global memory.
    x_index = get_group_id(1) * FUT_BLOCK_DIM + get_local_id(0);
    y_index = get_group_id(0) * FUT_BLOCK_DIM + get_local_id(1);

    uint index_out = y_index * height + x_index;

    if(x_index < height && y_index < width && index_out < total_size)
    {
        odata[index_out] = block[get_local_id(0)*(FUT_BLOCK_DIM+1)+get_local_id(1)];
    }
  }|]
