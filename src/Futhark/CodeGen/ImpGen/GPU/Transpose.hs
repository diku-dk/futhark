-- | Carefully optimised implementations of GPU transpositions.
-- Written in ImpCode so we can compile it to both CUDA and OpenCL.
module Futhark.CodeGen.ImpGen.GPU.Transpose
  ( TransposeType (..),
    TransposeArgs,
    mapTransposeKernel,
  )
where

import Futhark.CodeGen.ImpCode.GPU
import Futhark.IR.Prop.Types
import Futhark.Util.IntegralExp (divUp, quot, rem)
import Prelude hiding (quot, rem)

-- | Which form of transposition to generate code for.
data TransposeType
  = TransposeNormal
  | TransposeLowWidth
  | TransposeLowHeight
  | -- | For small arrays that do not
    -- benefit from coalescing.
    TransposeSmall
  deriving (Eq, Ord, Show)

-- | The types of the arguments accepted by a transposition function.
type TransposeArgs =
  ( VName,
    TExp Int32,
    VName,
    TExp Int32,
    TExp Int32,
    TExp Int32,
    TExp Int32,
    TExp Int32,
    TExp Int32,
    VName
  )

elemsPerThread :: TExp Int32
elemsPerThread = 4

mapTranspose :: TExp Int32 -> TransposeArgs -> PrimType -> TransposeType -> KernelCode
mapTranspose block_dim args t kind =
  case kind of
    TransposeSmall ->
      mconcat
        [ get_ids,
          dec our_array_offset $ le32 get_global_id_0 `quot` (height * width) * (height * width),
          dec x_index $ (le32 get_global_id_0 `rem` (height * width)) `quot` height,
          dec y_index $ le32 get_global_id_0 `rem` height,
          DeclareScalar val Nonvolatile t,
          dec odata_offset $
            (basic_odata_offset `quot` primByteSize t) + le32 our_array_offset,
          dec idata_offset $
            (basic_idata_offset `quot` primByteSize t) + le32 our_array_offset,
          dec index_in $ le32 y_index * width + le32 x_index,
          dec index_out $ le32 x_index * height + le32 y_index,
          when
            (le32 get_global_id_0 .<. width * height * num_arrays)
            ( mconcat
                [ Read val idata (elements $ sExt64 $ le32 idata_offset + le32 index_in) t (Space "global") Nonvolatile,
                  Write odata (elements $ sExt64 $ le32 odata_offset + le32 index_out) t (Space "global") Nonvolatile (var val t)
                ]
            )
        ]
    TransposeLowWidth ->
      mkTranspose $
        lowDimBody
          (le32 get_group_id_0 * block_dim + (le32 get_local_id_0 `quot` muly))
          ( le32 get_group_id_1 * block_dim * muly + le32 get_local_id_1
              + (le32 get_local_id_0 `rem` muly) * block_dim
          )
          ( le32 get_group_id_1 * block_dim * muly + le32 get_local_id_0
              + (le32 get_local_id_1 `rem` muly) * block_dim
          )
          (le32 get_group_id_0 * block_dim + (le32 get_local_id_1 `quot` muly))
    TransposeLowHeight ->
      mkTranspose $
        lowDimBody
          ( le32 get_group_id_0 * block_dim * mulx + le32 get_local_id_0
              + (le32 get_local_id_1 `rem` mulx) * block_dim
          )
          (le32 get_group_id_1 * block_dim + (le32 get_local_id_1 `quot` mulx))
          (le32 get_group_id_1 * block_dim + (le32 get_local_id_0 `quot` mulx))
          ( le32 get_group_id_0 * block_dim * mulx + le32 get_local_id_1
              + (le32 get_local_id_0 `rem` mulx) * block_dim
          )
    TransposeNormal ->
      mkTranspose $
        mconcat
          [ dec x_index $ le32 get_global_id_0,
            dec y_index $ le32 get_group_id_1 * tile_dim + le32 get_local_id_1,
            DeclareScalar val Nonvolatile t,
            when (le32 x_index .<. width) $
              For j (untyped elemsPerThread) $
                let i = le32 j * (tile_dim `quot` elemsPerThread)
                 in mconcat
                      [ dec index_in $ (le32 y_index + i) * width + le32 x_index,
                        when (le32 y_index + i .<. height) $
                          mconcat
                            [ Read
                                val
                                idata
                                (elements $ sExt64 $ le32 idata_offset + le32 index_in)
                                t
                                (Space "global")
                                Nonvolatile,
                              Write
                                block
                                ( elements $
                                    sExt64 $
                                      (le32 get_local_id_1 + i) * (tile_dim + 1)
                                        + le32 get_local_id_0
                                )
                                t
                                (Space "local")
                                Nonvolatile
                                (var val t)
                            ]
                      ],
            Op $ Barrier FenceLocal,
            SetScalar x_index $ untyped $ le32 get_group_id_1 * tile_dim + le32 get_local_id_0,
            SetScalar y_index $ untyped $ le32 get_group_id_0 * tile_dim + le32 get_local_id_1,
            when (le32 x_index .<. height) $
              For j (untyped elemsPerThread) $
                let i = le32 j * (tile_dim `quot` elemsPerThread)
                 in mconcat
                      [ dec index_out $ (le32 y_index + i) * height + le32 x_index,
                        when (le32 y_index + i .<. width) $
                          mconcat
                            [ Read
                                val
                                block
                                ( elements . sExt64 $
                                    le32 get_local_id_0 * (tile_dim + 1) + le32 get_local_id_1 + i
                                )
                                t
                                (Space "local")
                                Nonvolatile,
                              Write
                                odata
                                (elements $ sExt64 $ le32 odata_offset + le32 index_out)
                                t
                                (Space "global")
                                Nonvolatile
                                (var val t)
                            ]
                      ]
          ]
  where
    dec v (TPrimExp e) =
      DeclareScalar v Nonvolatile (primExpType e) <> SetScalar v e
    tile_dim = 2 * block_dim

    when a b = If a b mempty

    ( odata,
      basic_odata_offset,
      idata,
      basic_idata_offset,
      width,
      height,
      mulx,
      muly,
      num_arrays,
      block
      ) = args

    -- Be extremely careful when editing this list to ensure that
    -- the names match up.  Also, be careful that the tags on
    -- these names do not conflict with the tags of the
    -- surrounding code.  We accomplish the latter by using very
    -- low tags (normal variables start at least in the low
    -- hundreds).
    [ our_array_offset,
      x_index,
      y_index,
      odata_offset,
      idata_offset,
      index_in,
      index_out,
      get_global_id_0,
      get_local_id_0,
      get_local_id_1,
      get_group_id_0,
      get_group_id_1,
      get_group_id_2,
      j,
      val
      ] =
        zipWith (flip VName) [30 ..] $
          map
            nameFromString
            [ "our_array_offset",
              "x_index",
              "y_index",
              "odata_offset",
              "idata_offset",
              "index_in",
              "index_out",
              "get_global_id_0",
              "get_local_id_0",
              "get_local_id_1",
              "get_group_id_0",
              "get_group_id_1",
              "get_group_id_2",
              "j",
              "val"
            ]

    get_ids =
      mconcat
        [ DeclareScalar get_global_id_0 Nonvolatile int32,
          Op $ GetGlobalId get_global_id_0 0,
          DeclareScalar get_local_id_0 Nonvolatile int32,
          Op $ GetLocalId get_local_id_0 0,
          DeclareScalar get_local_id_1 Nonvolatile int32,
          Op $ GetLocalId get_local_id_1 1,
          DeclareScalar get_group_id_0 Nonvolatile int32,
          Op $ GetGroupId get_group_id_0 0,
          DeclareScalar get_group_id_1 Nonvolatile int32,
          Op $ GetGroupId get_group_id_1 1,
          DeclareScalar get_group_id_2 Nonvolatile int32,
          Op $ GetGroupId get_group_id_2 2
        ]

    mkTranspose body =
      mconcat
        [ get_ids,
          dec our_array_offset $ le32 get_group_id_2 * width * height,
          dec odata_offset $
            (basic_odata_offset `quot` primByteSize t) + le32 our_array_offset,
          dec idata_offset $
            (basic_idata_offset `quot` primByteSize t) + le32 our_array_offset,
          body
        ]

    lowDimBody x_in_index y_in_index x_out_index y_out_index =
      mconcat
        [ dec x_index x_in_index,
          dec y_index y_in_index,
          DeclareScalar val Nonvolatile t,
          dec index_in $ le32 y_index * width + le32 x_index,
          when (le32 x_index .<. width .&&. le32 y_index .<. height) $
            mconcat
              [ Read
                  val
                  idata
                  (elements $ sExt64 $ le32 idata_offset + le32 index_in)
                  t
                  (Space "global")
                  Nonvolatile,
                Write
                  block
                  (elements $ sExt64 $ le32 get_local_id_1 * (block_dim + 1) + le32 get_local_id_0)
                  t
                  (Space "local")
                  Nonvolatile
                  (var val t)
              ],
          Op $ Barrier FenceLocal,
          SetScalar x_index $ untyped x_out_index,
          SetScalar y_index $ untyped y_out_index,
          dec index_out $ le32 y_index * height + le32 x_index,
          when (le32 x_index .<. height .&&. le32 y_index .<. width) $
            mconcat
              [ Read
                  val
                  block
                  (elements $ sExt64 $ le32 get_local_id_0 * (block_dim + 1) + le32 get_local_id_1)
                  t
                  (Space "local")
                  Nonvolatile,
                Write
                  odata
                  (elements $ sExt64 (le32 odata_offset + le32 index_out))
                  t
                  (Space "global")
                  Nonvolatile
                  (var val t)
              ]
        ]

-- | Generate a transpose kernel.  There is special support to handle
-- input arrays with low width, low height, or both.
--
-- Normally when transposing a @[2][n]@ array we would use a @FUT_BLOCK_DIM x
-- FUT_BLOCK_DIM@ group to process a @[2][FUT_BLOCK_DIM]@ slice of the input
-- array. This would mean that many of the threads in a group would be inactive.
-- We try to remedy this by using a special kernel that will process a larger
-- part of the input, by using more complex indexing. In our example, we could
-- use all threads in a group if we are processing @(2/FUT_BLOCK_DIM)@ as large
-- a slice of each rows per group. The variable @mulx@ contains this factor for
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
mapTransposeKernel ::
  String ->
  Integer ->
  TransposeArgs ->
  PrimType ->
  TransposeType ->
  Kernel
mapTransposeKernel desc block_dim_int args t kind =
  Kernel
    { kernelBody =
        DeclareMem block (Space "local")
          <> Op (LocalAlloc block block_size)
          <> mapTranspose block_dim args t kind,
      kernelUses = uses,
      kernelNumGroups = map untyped num_groups,
      kernelGroupSize = map untyped group_size,
      kernelName = nameFromString name,
      kernelFailureTolerant = True,
      kernelCheckLocalMemory = False
    }
  where
    pad2DBytes k = k * (k + 1) * primByteSize t
    block_size =
      bytes $
        case kind of
          TransposeSmall -> 1 :: TExp Int64
          -- Not used, but AMD's OpenCL
          -- does not like zero-size
          -- local memory.
          TransposeNormal -> fromInteger $ pad2DBytes $ 2 * block_dim_int
          TransposeLowWidth -> fromInteger $ pad2DBytes block_dim_int
          TransposeLowHeight -> fromInteger $ pad2DBytes block_dim_int
    block_dim = fromInteger block_dim_int :: TExp Int32

    ( odata,
      basic_odata_offset,
      idata,
      basic_idata_offset,
      width,
      height,
      mulx,
      muly,
      num_arrays,
      block
      ) = args

    (num_groups, group_size) =
      case kind of
        TransposeSmall ->
          ( [(num_arrays * width * height) `divUp` (block_dim * block_dim)],
            [block_dim * block_dim]
          )
        TransposeLowWidth ->
          lowDimKernelAndGroupSize block_dim num_arrays width $ height `divUp` muly
        TransposeLowHeight ->
          lowDimKernelAndGroupSize block_dim num_arrays (width `divUp` mulx) height
        TransposeNormal ->
          let actual_dim = block_dim * 2
           in ( [ width `divUp` actual_dim,
                  height `divUp` actual_dim,
                  num_arrays
                ],
                [actual_dim, actual_dim `quot` elemsPerThread, 1]
              )

    uses =
      map
        (`ScalarUse` int32)
        ( namesToList $
            mconcat $
              map
                freeIn
                [ basic_odata_offset,
                  basic_idata_offset,
                  num_arrays,
                  width,
                  height,
                  mulx,
                  muly
                ]
        )
        ++ map MemoryUse [odata, idata]

    name =
      case kind of
        TransposeSmall -> desc ++ "_small"
        TransposeLowHeight -> desc ++ "_low_height"
        TransposeLowWidth -> desc ++ "_low_width"
        TransposeNormal -> desc

lowDimKernelAndGroupSize ::
  TExp Int32 ->
  TExp Int32 ->
  TExp Int32 ->
  TExp Int32 ->
  ([TExp Int32], [TExp Int32])
lowDimKernelAndGroupSize block_dim num_arrays x_elems y_elems =
  ( [ x_elems `divUp` block_dim,
      y_elems `divUp` block_dim,
      num_arrays
    ],
    [block_dim, block_dim, 1]
  )
