{-# LANGUAGE QuasiQuotes #-}
module Futhark.CodeGen.OpenCL.Kernels
       ( SizeHeuristic (..)
       , DeviceType (..)
       , WhichSize (..)
       , HeuristicValue (..)
       , sizeHeuristicsTable

       , mapTranspose
       , TransposeType(..)
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
data WhichSize = LockstepWidth | NumGroups | GroupSize | TileSize

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
  , SizeHeuristic "AMD Accelerated Parallel Processing" DeviceGPU LockstepWidth $ HeuristicConst 64
  , SizeHeuristic "" DeviceGPU LockstepWidth $ HeuristicConst 1
  , SizeHeuristic "" DeviceGPU NumGroups $ HeuristicConst 128
  , SizeHeuristic "" DeviceGPU GroupSize $ HeuristicConst 256
  , SizeHeuristic "" DeviceGPU TileSize $ HeuristicConst 32

  , SizeHeuristic "" DeviceCPU LockstepWidth $ HeuristicConst 1
  , SizeHeuristic "" DeviceCPU NumGroups $ HeuristicDeviceInfo "MAX_COMPUTE_UNITS"
  , SizeHeuristic "" DeviceCPU GroupSize $ HeuristicConst 32
  , SizeHeuristic "" DeviceCPU TileSize $ HeuristicConst 4
  ]

-- | Which form of transposition to generate code for.
data TransposeType = TransposeNormal
                   | TransposeLowWidth
                   | TransposeLowHeight
                   | TransposeSmall -- ^ For small arrays that do not
                                    -- benefit from coalescing.
                   deriving (Eq, Ord, Show)

-- | @mapTranspose name elem_type transpose_type@ Generate a transpose kernel
-- with requested @name@ for elements of type @elem_type@. There are special
-- support to handle input arrays with low width or low height, which can be
-- indicated by @transpose_type@.
--
-- Normally when transposing a @[2][n]@ array we would use a @FUT_BLOCK_DIM x
-- FUT_BLOCK_DIM@ group to process a @[2][FUT_BLOCK_DIM]@ slice of the input
-- array. This would mean that many of the threads in a group would be inactive.
-- We try to remedy this by using a special kernel that will process a larger
-- part of the input, by using more complex indexing. In our example, we could
-- use all threads in a group if we are processing @(2/FUT_BLOCK_DIM)@ as large
-- a slice of each rows per group. The variable 'mulx' contains this factor for
-- the kernel to handle input arrays with low height.
--
-- See issue #308 on GitHub for more details.
mapTranspose :: C.ToIdent a => a -> C.Type -> TransposeType -> C.Func
mapTranspose kernel_name elem_type transpose_type =
  case transpose_type of
    TransposeNormal ->
      mkTranspose [C.citems|
        // This kernel is optimized to ensure all global reads and writes are
        // coalesced, and to avoid bank conflicts in shared memory.  Each
        // thread group transposes a 2D tile of FUT_BLOCK_DIM*2 by
        // FUT_BLOCK_DIM*2 elements. The size of a thread group is
        // FUT_BLOCK_DIM/2 by FUT_BLOCK_DIM*2, meaning that each thread will
        // process 4 elements in a 2D tile.  The shared memory array containing
        // the 2D tile consists of FUT_BLOCK_DIM*2 by FUT_BLOCK_DIM*2+1
        // elements. Padding each row with an additional element prevents bank
        // conflicts from occuring when the tile is accessed column-wise.
        //
        // Note that input_size and output_size may not equal width*height if
        // we are dealing with a truncated array - this happens sometimes for
        // coalescing optimisations.
        const uint tile_dim = 2 * FUT_BLOCK_DIM;
        const uint elems_per_thread = 4;
        uint x_index = get_global_id(0);
        uint y_index = get_group_id(1) * tile_dim + get_local_id(1);
        if (x_index < width) {
          for (int i = 0; i < tile_dim; i += tile_dim/elems_per_thread)
          {
            uint index_in = (y_index + i) * width + x_index;
            if (y_index + i < height && index_in < input_size)
            {
              block[(get_local_id(1) + i)*(tile_dim+1)+get_local_id(0)] = idata[idata_offset + index_in];
            }
          }
        }
        barrier(CLK_LOCAL_MEM_FENCE);

        x_index = get_group_id(1) * tile_dim + get_local_id(0);
        y_index = get_group_id(0) * tile_dim + get_local_id(1);
        if (x_index < height)
        {
          for (int i = 0; i < tile_dim; i += tile_dim/elems_per_thread)
          {
            uint index_out = (y_index + i) * height + x_index;
            if (y_index + i < width && index_out < output_size)
            {
              odata[odata_offset + index_out] = block[get_local_id(0)*(tile_dim+1)+get_local_id(1)+i];
            }
          }
        }
      |]
    TransposeLowWidth ->
      mkTranspose $ lowDimBody
      [C.cexp|get_group_id(0) * FUT_BLOCK_DIM + (get_local_id(0) / muly)|]
      [C.cexp|get_group_id(1) * FUT_BLOCK_DIM * muly
           + get_local_id(1)
           + (get_local_id(0) % muly) * FUT_BLOCK_DIM
          |]
      [C.cexp|get_group_id(1) * FUT_BLOCK_DIM * muly
           + get_local_id(0)
           + (get_local_id(1) % muly) * FUT_BLOCK_DIM|]
      [C.cexp|get_group_id(0) * FUT_BLOCK_DIM + (get_local_id(1) / muly)|]
    TransposeLowHeight ->
      mkTranspose $ lowDimBody
      [C.cexp|get_group_id(0) * FUT_BLOCK_DIM * mulx
           + get_local_id(0)
           + (get_local_id(1) % mulx) * FUT_BLOCK_DIM
          |]
      [C.cexp|get_group_id(1) * FUT_BLOCK_DIM + (get_local_id(1) / mulx)|]
      [C.cexp|get_group_id(1) * FUT_BLOCK_DIM + (get_local_id(0) / mulx)|]
      [C.cexp|get_group_id(0) * FUT_BLOCK_DIM * mulx
           + get_local_id(1)
           + (get_local_id(0) % mulx) * FUT_BLOCK_DIM
           |]
    TransposeSmall ->
      smallKernel
  where
    params = [C.cparams|__global $ty:elem_type *odata,
                        uint basic_odata_offset,
                        __global $ty:elem_type *idata,
                        uint basic_idata_offset,
                        uint width,
                        uint height,
                        uint input_size,
                        uint output_size,
                        uint mulx, uint muly, uint num_arrays,
                        __local $ty:elem_type* block
                        |]

    mkTranspose body =
      [C.cfun|
       __kernel void $id:kernel_name($params:params) {
         uint our_array_offset = get_group_id(2) * width * height;
         uint odata_offset =
           basic_odata_offset/sizeof($ty:elem_type) + our_array_offset;
         uint idata_offset =
           basic_idata_offset/sizeof($ty:elem_type) + our_array_offset;
         $items:body
       }|]

    lowDimBody x_in_index y_in_index x_out_index y_out_index =
      [C.citems|
         uint x_index = $exp:x_in_index;
         uint y_index = $exp:y_in_index;
         uint index_in = y_index * width + x_index;
         if(x_index < width && y_index < height && index_in < input_size)
         {
           block[get_local_id(1)*(FUT_BLOCK_DIM+1)+get_local_id(0)] =
             idata[idata_offset + index_in];
         }
         barrier(CLK_LOCAL_MEM_FENCE);

         x_index = $exp:x_out_index;
         y_index = $exp:y_out_index;
         uint index_out = y_index * height + x_index;
         if(x_index < height && y_index < width && index_out < output_size)
         {
           odata[odata_offset + index_out] =
             block[get_local_id(0)*(FUT_BLOCK_DIM+1)+get_local_id(1)];
         }
      |]

    smallKernel =
      [C.cfun|
         __kernel void $id:kernel_name($params:params) {
           uint our_array_offset = get_global_id(0) / (height*width) * (height*width);
           uint x_index = get_global_id(0) % (height*width) / height;
           uint y_index = get_global_id(0) % height;

           uint odata_offset =
             basic_odata_offset/sizeof($ty:elem_type) + our_array_offset;
           uint idata_offset =
             basic_idata_offset/sizeof($ty:elem_type) + our_array_offset;

           // Read and write the element.
           uint index_in = y_index * width + x_index;
           uint index_out = x_index * height + y_index;
           if (get_global_id(0) < input_size) {
               odata[odata_offset + index_out] = idata[idata_offset + index_in];
           }
         }|]
