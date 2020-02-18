module Futhark.CodeGen.ImpGen.Kernels.Transpose
  ( TransposeType(..)
  , TransposeArgs
  , mapTranspose
  , mapTransposeKernel
  )
  where

import Prelude hiding (quot, rem)

import Futhark.CodeGen.ImpCode.Kernels
import Futhark.Representation.AST.Attributes.Types
import Futhark.Representation.AST.Attributes.Names (freeIn, namesToList)
import Futhark.Util.IntegralExp (IntegralExp, quot, rem, quotRoundingUp)

-- | Which form of transposition to generate code for.
data TransposeType = TransposeNormal
                   | TransposeLowWidth
                   | TransposeLowHeight
                   | TransposeSmall -- ^ For small arrays that do not
                                    -- benefit from coalescing.
                   deriving (Eq, Ord, Show)

type TransposeArgs = (VName, Exp,
                      VName, Exp,
                      Exp, Exp, Exp, Exp,
                      Exp, Exp, Exp,
                      VName)

elemsPerThread :: IntegralExp a => a
elemsPerThread = 4

-- | Generate a transpose kernel.  There is special support to handle
-- input arrays with low width, low height, or both.
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
--
-- These kernels are optimized to ensure all global reads and writes
-- are coalesced, and to avoid bank conflicts in shared memory.  Each
-- thread group transposes a 2D tile of block_dim*2 by block_dim*2
-- elements. The size of a thread group is block_dim/2 by
-- block_dim*2, meaning that each thread will process 4 elements in a
-- 2D tile.  The shared memory array containing the 2D tile consists
-- of block_dim*2 by block_dim*2+1 elements. Padding each row with
-- an additional element prevents bank conflicts from occuring when
-- the tile is accessed column-wise.
--
-- Note that input_size and output_size may not equal width*height if
-- we are dealing with a truncated array - this happens sometimes for
-- coalescing optimisations.
mapTranspose :: Exp -> TransposeArgs -> PrimType -> TransposeType -> KernelCode
mapTranspose block_dim args t kind =
  case kind of
    TransposeSmall ->
      mconcat
      [ get_ids

      , dec our_array_offset $ v32 get_global_id_0 `quot` (height*width) * (height*width)

      , dec x_index $ (v32 get_global_id_0 `rem` (height*width)) `quot` height
      , dec y_index $ v32 get_global_id_0 `rem` height

      , dec odata_offset $
        (basic_odata_offset `quot` primByteSize t) + v32 our_array_offset
      , dec idata_offset $
        (basic_idata_offset `quot` primByteSize t) + v32 our_array_offset

      , dec index_in $ v32 y_index * width + v32 x_index
      , dec index_out $ v32 x_index * height + v32 y_index

      , If (v32 get_global_id_0 .<. input_size)
        (Write odata (elements $ v32 odata_offset + v32 index_out) t (Space "global") Nonvolatile $
         index idata (elements $ v32 idata_offset + v32 index_in) t (Space "global") Nonvolatile)
        mempty
      ]

    TransposeLowWidth ->
      mkTranspose $ lowDimBody
      (v32 get_group_id_0 * block_dim + (v32 get_local_id_0 `quot` muly))
      (v32 get_group_id_1 * block_dim * muly + v32 get_local_id_1 +
       (v32 get_local_id_0 `rem` muly) * block_dim)
      (v32 get_group_id_1* block_dim * muly + v32 get_local_id_0 +
       (v32 get_local_id_1 `rem` muly) * block_dim)
      (v32 get_group_id_0 * block_dim + (v32 get_local_id_1 `quot` muly))

    TransposeLowHeight ->
      mkTranspose $ lowDimBody
      (v32 get_group_id_0 * block_dim * mulx + v32 get_local_id_0 +
       (v32 get_local_id_1 `rem` mulx) * block_dim)
      (v32 get_group_id_1 * block_dim + (v32 get_local_id_1 `quot` mulx))
      (v32 get_group_id_1 * block_dim + (v32 get_local_id_0 `quot` mulx))
      (v32 get_group_id_0 * block_dim * mulx + v32 get_local_id_1 +
       (v32 get_local_id_0 `rem` mulx) * block_dim)

    TransposeNormal ->
      mkTranspose $ mconcat
      [ dec x_index $ v32 get_global_id_0
      , dec y_index $ v32 get_group_id_1 * tile_dim + v32 get_local_id_1
      , when (v32 x_index .<. width) $
        For j Int32 elemsPerThread $
        let i = v32 j * (tile_dim `quot` elemsPerThread)
        in mconcat [ dec index_in $ (v32 y_index + i) * width + v32 x_index
                   , when (v32 y_index + i .<. height .&&.
                           v32 index_in .<. input_size) $
                     Write block (elements $ (v32 get_local_id_1 + i) * (tile_dim+1)
                                             + v32 get_local_id_0)
                     t (Space "local") Nonvolatile $
                     index idata (elements $ v32 idata_offset + v32 index_in)
                     t (Space "global") Nonvolatile]
      , Op LocalBarrier
      , SetScalar x_index $ v32 get_group_id_1 * tile_dim + v32 get_local_id_0
      , SetScalar y_index $ v32 get_group_id_0 * tile_dim + v32 get_local_id_1
      , when (v32 x_index .<. height) $
        For j Int32 elemsPerThread $
        let i = v32 j * (tile_dim `quot` elemsPerThread)
        in mconcat [ dec index_out $ (v32 y_index + i) * height + v32 x_index
                   , when (v32 y_index + i .<. width .&&.
                           v32 index_out .<. output_size) $
                     Write odata (elements $ v32 odata_offset + v32 index_out)
                     t (Space "global") Nonvolatile $
                     index block (elements $ v32 get_local_id_0 * (tile_dim+1)
                                             + v32 get_local_id_1+i)
                     t (Space "local") Nonvolatile
                   ]
      ]

  where dec v e = DeclareScalar v Nonvolatile int32 <> SetScalar v e
        v32 = flip var int32
        tile_dim = 2 * block_dim

        when a b = If a b mempty

        (odata, basic_odata_offset, idata, basic_idata_offset,
         width, height, input_size, output_size,
         mulx, muly, _num_arrays, block) = args

        -- Be extremely careful when editing this list to ensure that
        -- the names match up.  Also, be careful that the tags on
        -- these names do not conflict with the tags of the
        -- surrounding code.  We accomplish the latter by using very
        -- low tags (normal variables start at least in the low
        -- hundreds).
        [   our_array_offset , x_index , y_index
          , odata_offset, idata_offset, index_in, index_out
          , get_global_id_0
          , get_local_id_0, get_local_id_1
          , get_group_id_0, get_group_id_1, get_group_id_2
          , j] =
          zipWith (flip VName) [30..] $ map nameFromString
          [ "our_array_offset" , "x_index" , "y_index"
          , "odata_offset", "idata_offset", "index_in", "index_out"
          , "get_global_id_0"
          , "get_local_id_0", "get_local_id_1"
          , "get_group_id_0", "get_group_id_1", "get_group_id_2"
          , "j"]

        get_ids =
          mconcat [ DeclareScalar get_global_id_0 Nonvolatile int32
                  , Op $ GetGlobalId get_global_id_0 0
                  , DeclareScalar get_local_id_0 Nonvolatile int32
                  , Op $ GetLocalId get_local_id_0 0
                  , DeclareScalar get_local_id_1 Nonvolatile int32
                  , Op $ GetLocalId get_local_id_1 1
                  , DeclareScalar get_group_id_0 Nonvolatile int32
                  , Op $ GetGroupId get_group_id_0 0
                  , DeclareScalar get_group_id_1 Nonvolatile int32
                  , Op $ GetGroupId get_group_id_1 1
                  , DeclareScalar get_group_id_2 Nonvolatile int32
                  , Op $ GetGroupId get_group_id_2 2
                  ]

        mkTranspose body =
          mconcat
          [ get_ids
          , dec our_array_offset $ v32 get_group_id_2 * width * height
          , dec odata_offset $
            (basic_odata_offset `quot` primByteSize t) + v32 our_array_offset
          , dec idata_offset $
            (basic_idata_offset `quot` primByteSize t) + v32 our_array_offset
          , body
          ]

        lowDimBody x_in_index y_in_index x_out_index y_out_index =
          mconcat
          [ dec x_index x_in_index
          , dec y_index y_in_index
          , dec index_in $ v32 y_index * width + v32 x_index
          , when (v32 x_index .<. width .&&. v32 y_index .<. height .&&. v32 index_in .<. input_size) $
            Write block (elements $ v32 get_local_id_1 * (block_dim+1) + v32 get_local_id_0)
            t (Space "local") Nonvolatile $
            index idata (elements $ v32 idata_offset + v32 index_in)
            t (Space "global") Nonvolatile
          , Op LocalBarrier
          , SetScalar x_index x_out_index
          , SetScalar y_index y_out_index
          , dec index_out $ v32 y_index * height + v32 x_index
          , when (v32 x_index .<. height .&&. v32 y_index .<. width .&&. v32 index_out .<. output_size) $
            Write odata (elements $ v32 odata_offset + v32 index_out)
            t (Space "global") Nonvolatile $
            index block (elements $ v32 get_local_id_0 * (block_dim+1) + v32 get_local_id_1)
            t (Space "local") Nonvolatile
          ]

mapTransposeKernel :: String -> Integer -> TransposeArgs -> PrimType -> TransposeType
                   -> Kernel
mapTransposeKernel desc block_dim_int args t kind =
  Kernel
  { kernelBody = DeclareMem block (Space "local") <>
                 Op (LocalAlloc block block_size) <>
                 mapTranspose block_dim args t kind
  , kernelUses = uses
  , kernelNumGroups = num_groups
  , kernelGroupSize = group_size
  , kernelName = nameFromString name
  , kernelFailureTolerant = True
  }
  where pad2DBytes k = k * (k + 1) * primByteSize t
        block_size =
          case kind of TransposeSmall -> 1 -- Not used, but AMD's
                                           -- OpenCL does not like
                                           -- zero-size local memory.
                       TransposeNormal -> fromInteger $ pad2DBytes $ 2*block_dim_int
                       TransposeLowWidth -> fromInteger $ pad2DBytes block_dim_int
                       TransposeLowHeight -> fromInteger $ pad2DBytes block_dim_int
        block_dim = fromInteger block_dim_int

        (odata, basic_odata_offset, idata, basic_idata_offset,
         width, height, input_size, output_size,
         mulx, muly, num_arrays,
         block) = args

        (num_groups, group_size) =
          case kind of
            TransposeSmall ->
              ([(num_arrays * width * height) `quotRoundingUp` (block_dim * block_dim)],
               [block_dim * block_dim])
            TransposeLowWidth ->
              lowDimKernelAndGroupSize block_dim num_arrays width $ height `quotRoundingUp` muly
            TransposeLowHeight ->
              lowDimKernelAndGroupSize block_dim num_arrays (width `quotRoundingUp` mulx) height
            TransposeNormal ->
              let actual_dim = block_dim*2
              in ( [ width `quotRoundingUp` actual_dim
                   , height `quotRoundingUp` actual_dim
                   , num_arrays]
                 , [actual_dim, actual_dim `quot` elemsPerThread, 1])

        uses = map (`ScalarUse` int32)
               (namesToList $ mconcat $ map freeIn
                [basic_odata_offset, basic_idata_offset, num_arrays,
                 width, height, input_size, output_size, mulx, muly]) ++
               map MemoryUse [odata, idata]

        name =
          case kind of TransposeSmall -> desc ++ "_small"
                       TransposeLowHeight -> desc ++ "_low_height"
                       TransposeLowWidth -> desc ++ "_low_width"
                       TransposeNormal -> desc

lowDimKernelAndGroupSize :: Exp -> Exp -> Exp -> Exp -> ([Exp], [Exp])
lowDimKernelAndGroupSize block_dim num_arrays x_elems y_elems =
  ([x_elems `quotRoundingUp` block_dim,
    y_elems `quotRoundingUp` block_dim,
    num_arrays],
   [block_dim, block_dim, 1])
