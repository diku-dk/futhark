-- Code and comments based on
-- https://github.com/kkushagra/rodinia/blob/master/openmp/nw
--
-- ==
-- compiled random input { 10i32 [2362369]i32 [24][24][64][64]i32 }

let mkVal [bp1][b] (y:i32) (x:i32) (pen:i32) (block:[bp1][bp1]i32) (ref:[b][b]i32) : i32 =
  #[unsafe]
  i32.max (block[y, x - 1] - pen) (block[y - 1, x] - pen)
  |> i32.max (block[y - 1, x - 1] + ref[y - 1, x - 1])


let process_block [b][bp1]
                  (penalty: i32)
                  (block: [bp1][bp1]i32)
                  (ref: [b][b]i32): *[b][b]i32 =
  -- let bp1 = assert (bp1 = b + 1) bp1

  -- Process the first half (anti-diagonally) of the block
  let block =
    loop block = copy block for m < b do
       let inds =
            tabulate b (\tx ->  (
                    if tx > m then (-1, -1)
                    else let ind_x = i32.i64 (tx + 1)
                         let ind_y = i32.i64 (m - tx + 1)
                         in  (i64.i32 ind_y, i64.i32 ind_x)))
        let vals =
            -- tabulate over the m'th anti-diagonal before the middle
            tabulate b (\tx ->  (
                    if tx > m then 0
                    else let ind_x = i32.i64 (tx + 1)
                         let ind_y = i32.i64 (m - tx + 1)
                         let v = mkVal ind_y ind_x penalty block ref
                         in  v))
        in scatter_2d block inds vals

  -- Process the second half (anti-diagonally) of the block
  let block = loop block for m < b-1 do
        let m = b - 2 - m
        let inds = tabulate b (\tx ->  (
                    if tx > m then (-1, -1)
                    else let ind_x = i32.i64 (tx + b - m)
                         let ind_y = i32.i64 (b - tx)
                         in  ((i64.i32 ind_y, i64.i32 ind_x)) )
                )
        let vals =
            -- tabulate over the m'th anti-diagonal after the middle
            tabulate b (\tx ->  (
                    if tx > m then (0)
                    else let ind_x = i32.i64 (tx + b - m)
                         let ind_y = i32.i64 (b - tx)
                         let v = mkVal ind_y ind_x penalty block ref
                         in  v ))
        in scatter_2d block inds vals

  in block[1:, 1:] :> [b][b]i32

let main [n]
         (block_size: i64)
         (penalty: i32)
         (input: *[n]i32)
         (refs: [n]i32)
         -- : *[n]i32
 =
  let row_length = i64.f64 <| f64.sqrt <| f64.i64 n
  let num_blocks = -- assert ((row_length - 1) % b == 0) <|
                   (row_length - 1) / block_size
  let bp1 = block_size + 1

  let i = 0
  let ip1 = i + 1
  let input =
    -- loop input for i < num_blocks do
    -- let input[row_length + 1 + i * block_size;
    --           1 + i : row_length * block_size - block_size,
    --           block_size : row_length,
    --           block_size : 1] =
      #[incremental_flattening(only_intra)]
      map2 (process_block penalty)
           (input[i * block_size;
                 ip1 : row_length * block_size - block_size,
                 block_size + 1 : row_length,
                 block_size + 1 : 1i64] :> [ip1][bp1][bp1]i32)
           (refs[row_length + 1 + i * block_size;
                ip1 : row_length * block_size - block_size,
                block_size : row_length,
                block_size : 1i64] :> [ip1][block_size][block_size]i32)

    -- in input

  in input
