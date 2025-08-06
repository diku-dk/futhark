-- Code and comments based on
-- https://github.com/kkushagra/rodinia/blob/master/openmp/nw
--
-- ==
-- entry: nw_flat
-- compiled random input { 16i64 10i32 [2362369]i32 [2362369]i32 } auto output
-- compiled random input { 32i64 10i32 [2362369]i32 [2362369]i32 } auto output
-- compiled random input { 64i64 10i32 [2362369]i32 [2362369]i32 } auto output
-- compiled input { 3i64
--                  10i32
--                  [4i32, 2i32, 4i32, 9i32, 2i32, 1i32, 7i32, 1i32, 9i32, 8i32,
--                   7i32, 6i32, 7i32, 5i32, 0i32, 9i32, 0i32, 0i32, 2i32, 5i32,
--                   6i32, 9i32, 3i32, 3i32, 7i32, 6i32, 6i32, 5i32, 4i32, 7i32,
--                   1i32, 9i32, 5i32, 4i32, 4i32, 5i32, 9i32, 6i32, 7i32, 2i32,
--                   2i32, 9i32, 6i32, 6i32, 8i32, 4i32, 4i32, 8i32, 0i32, 4i32,
--                   5i32, 5i32, 5i32, 1i32, 3i32, 1i32, 1i32, 7i32, 2i32, 8i32,
--                   5i32, 3i32, 9i32, 4i32, 2i32, 8i32, 1i32, 1i32, 0i32, 5i32,
--                   8i32, 7i32, 0i32, 7i32, 5i32, 9i32, 1i32, 5i32, 5i32, 1i32,
--                   6i32, 1i32, 8i32, 9i32, 3i32, 4i32, 6i32, 0i32, 2i32, 5i32,
--                   4i32, 8i32, 7i32, 7i32, 2i32, 0i32, 5i32, 0i32, 1i32, 3i32]
--                  [4i32, 2i32, 4i32, 9i32, 2i32, 1i32, 7i32, 1i32, 9i32, 8i32,
--                   7i32, 6i32, 7i32, 5i32, 0i32, 9i32, 0i32, 0i32, 2i32, 5i32,
--                   6i32, 9i32, 3i32, 3i32, 7i32, 6i32, 6i32, 5i32, 4i32, 7i32,
--                   1i32, 9i32, 5i32, 4i32, 4i32, 5i32, 9i32, 6i32, 7i32, 2i32,
--                   2i32, 9i32, 6i32, 6i32, 8i32, 4i32, 4i32, 8i32, 0i32, 4i32,
--                   5i32, 5i32, 5i32, 1i32, 3i32, 1i32, 1i32, 7i32, 2i32, 8i32,
--                   5i32, 3i32, 9i32, 4i32, 2i32, 8i32, 1i32, 1i32, 0i32, 5i32,
--                   8i32, 7i32, 0i32, 7i32, 5i32, 9i32, 1i32, 5i32, 5i32, 1i32,
--                   6i32, 1i32, 8i32, 9i32, 3i32, 4i32, 6i32, 0i32, 2i32, 5i32,
--                   4i32, 8i32, 7i32, 7i32, 2i32, 0i32, 5i32, 0i32, 1i32, 3i32] }
-- output { [ 4i32,  2i32,  4i32,  9i32,  2i32,  1i32,  7i32,  1i32,  9i32,  8i32,
--            7i32, 10i32,  9i32,  9i32,  9i32, 11i32,  1i32,  7i32,  3i32, 14i32,
--            6i32, 16i32, 13i32, 12i32, 16i32, 15i32, 17i32,  7i32, 11i32, 10i32,
--            1i32, 15i32, 21i32, 17i32, 16i32, 21i32, 24i32, 23i32, 14i32, 13i32,
--            2i32, 10i32, 21i32, 27i32, 25i32, 20i32, 25i32, 32i32, 23i32, 18i32,
--            5i32,  7i32, 15i32, 22i32, 30i32, 26i32, 21i32, 32i32, 34i32, 31i32,
--            5i32,  8i32, 16i32, 19i32, 24i32, 38i32, 28i32, 22i32, 32i32, 39i32,
--            8i32, 12i32,  8i32, 23i32, 24i32, 33i32, 39i32, 33i32, 27i32, 33i32,
--            6i32,  9i32, 20i32, 17i32, 26i32, 28i32, 39i32, 39i32, 35i32, 32i32,
--            4i32, 14i32, 16i32, 27i32, 19i32, 26i32, 33i32, 39i32, 40i32, 38i32] }
-- structure gpu-mem { Alloc 6 }
-- structure seq-mem { Alloc 8 }

import "intrinsics"

def mkVal [bp1] [b] (y: i32) (x: i32) (pen: i32) (block: [bp1][bp1]i32) (ref: [b][b]i32) : i32 =
  #[unsafe]
  i32.max (block[y, x - 1] - pen) (block[y - 1, x] - pen)
  |> i32.max (block[y - 1, x - 1] + ref[y - 1, x - 1])

def process_block [b] [bp1]
                  (penalty: i32)
                  (block: [bp1][bp1]i32)
                  (ref: [b][b]i32) : *[b][b]i32 =
  -- let bp1 = assert (bp1 = b + 1) bp1

  -- Process the first half (anti-diagonally) of the block
  let block =
    loop block = copy block
    for m < b do
      let inds =
        tabulate b (\tx ->
                      if tx > m
                      then (-1, -1)
                      else let ind_x = i32.i64 (tx + 1)
                           let ind_y = i32.i64 (m - tx + 1)
                           in (i64.i32 ind_y, i64.i32 ind_x))
      let vals =
        -- tabulate over the m'th anti-diagonal before the middle
        tabulate b
                 (\tx ->
                    if tx > m
                    then 0
                    else let ind_x = i32.i64 (tx + 1)
                         let ind_y = i32.i64 (m - tx + 1)
                         let v = mkVal ind_y ind_x penalty block ref
                         in v)
      in scatter_2d block inds vals
  -- Process the second half (anti-diagonally) of the block
  let block =
    loop block for m < b - 1 do
      let m = b - 2 - m
      let inds =
        tabulate b (\tx ->
                      (if tx > m
                       then (-1, -1)
                       else let ind_x = i32.i64 (tx + b - m)
                            let ind_y = i32.i64 (b - tx)
                            in ((i64.i32 ind_y, i64.i32 ind_x))))
      let vals =
        -- tabulate over the m'th anti-diagonal after the middle
        tabulate b (\tx ->
                      (if tx > m
                       then (0)
                       else let ind_x = i32.i64 (tx + b - m)
                            let ind_y = i32.i64 (b - tx)
                            let v = mkVal ind_y ind_x penalty block ref
                            in v))
      in scatter_2d block inds vals
  in block[1:, 1:] :> *[b][b]i32

entry nw_flat [n]
              (block_size: i64)
              (penalty: i32)
              (input: *[n]i32)
              (refs: [n]i32) : *[n]i32 =
  let row_length = i64.f64 <| f64.sqrt <| f64.i64 n
  let num_blocks =
    -- assert ((row_length - 1) % b == 0) <|
    (row_length - 1) / block_size
  let bp1 = block_size + 1
  let input =
    loop input for i < num_blocks do
      let ip1 = i + 1
      let v =
        #[incremental_flattening(only_intra)]
        map2 (process_block penalty)
             (flat_index_3d input
                            (i * block_size)
                            ip1
                            (row_length * block_size - block_size)
                            bp1
                            row_length
                            bp1
                            1i64)
             (flat_index_3d refs
                            (row_length + 1 + i * block_size)
                            ip1
                            (row_length * block_size - block_size)
                            block_size
                            row_length
                            block_size
                            1i64)
      in flat_update_3d input
                        (row_length + 1 + i * block_size)
                        (row_length * block_size - block_size)
                        (row_length)
                        1
                        v
  let input =
    loop input for i < num_blocks - 1 do
      let v =
        #[incremental_flattening(only_intra)]
        map2 (process_block penalty)
             (flat_index_3d input
                            (((i + 1) * block_size + 1) * row_length - block_size - 1)
                            (num_blocks - i - 1)
                            (row_length * block_size - block_size)
                            bp1
                            row_length
                            bp1
                            1i64)
             (flat_index_3d refs
                            (((i + 1) * block_size + 2) * row_length - block_size)
                            (num_blocks - i - 1)
                            (row_length * block_size - block_size)
                            block_size
                            row_length
                            block_size
                            1i64)
      in flat_update_3d input
                        (((i + 1) * block_size + 2) * row_length - block_size)
                        (row_length * block_size - block_size)
                        (row_length)
                        1
                        v
  in input
