-- Parallel blocked LU-decomposition.
--
-- ==
-- entry: lud
-- random input { 32i64 [1024]f32 }
-- compiled random input { 32i64 [16384]f32 }
-- compiled random input { 32i64 [4194304]f32 }

import "intrinsics"

def dotprod [n] (a: [n]f32) (b: [n]f32): f32 =
  map2 (*) a b
       |> reduce (+) 0

def lud_diagonal [b] (a: [b][b]f32): *[b][b]f32 =
  map1 (\mat ->
          let mat = copy mat
          in loop (mat: *[b][b]f32) for i < b-1 do
             let col = map (\j -> if j > i then
                                    #[unsafe] (mat[j,i] - (dotprod mat[j,:i] mat[:i,i])) / mat[i,i]
                                  else
                                    mat[j,i])
                           (iota b)
            let mat[:,i] = col

            let row = map (\j -> if j > i then
                                   mat[i+1, j] - (dotprod mat[:i+1, j] mat[i+1, :i+1])
                                 else
                                   mat[i+1, j])
                          (iota b)
            let mat[i+1] = row

            in mat
       ) (unflatten (a :> [opaque 1*b][b]f32))
       |> head

def lud_perimeter_upper [m][b] (diag: [b][b]f32) (a0s: [m][b][b]f32): *[m][b][b]f32 =
    let a1s = map (\ (x: [b][b]f32): [b][b]f32  -> transpose(x)) a0s in
    let a2s =
        map  (\a1: [b][b]f32  ->
              map  (\row0: [b]f32  ->   -- Upper
                    loop row = copy row0 for i < b do
                    let sum = (loop sum=0.0f32 for k < i do sum + diag[i,k] * row[k])
                    let row[i] = row[i] - sum
                    in  row
                   ) a1
             ) a1s
    in map (\x: [b][b]f32 -> transpose(x)) a2s

def lud_perimeter_lower [b][m] (diag: [b][b]f32) (mat: [m][b][b]f32): *[m][b][b]f32 =
  map (\blk: [b][b]f32  ->
        map  (\ (row0: [b]f32): *[b]f32  ->   -- Lower
                loop row = copy row0 for j < b do
                        let sum = loop sum=0.0f32 for k < j do
                            sum + diag[k,j] * row[k]
                        let row[j] = (row[j] - sum) / diag[j,j]
                        in  row
            ) blk
      ) mat

def lud_internal [m][b] (top_per: [m][b][b]f32) (lft_per: [m][b][b]f32) (mat_slice: [m][m][b][b]f32): *[m][m][b][b]f32 =
  let top_slice = map transpose top_per in
  map (\(mat_arr: [m][b][b]f32, lft: [b][b]f32): [m][b][b]f32  ->
        map (\ (mat_blk: [b][b]f32, top: [b][b]f32): [b][b]f32  ->
                map  (\ (mat_row: [b]f32, lft_row: [b]f32): [b]f32  ->
                        map  (\(mat_el, top_row)  ->
                                let prods = map2 (*) lft_row top_row
                                let sum   = f32.sum prods
                                in mat_el - sum
                             ) (zip (mat_row) top)
                    ) (zip (mat_blk) lft )
           ) (zip (mat_arr) (top_slice) )
     ) (zip (mat_slice) (lft_per) )

entry lud [n] (block_size: i64) (mat: *[n]f32): [n]f32 =
  let row_length = i64.f64 <| f64.sqrt <| f64.i64 n
  let row_length = assert (row_length ** 2 == n) row_length
  let block_size = i64.min block_size row_length
  let num_blocks = assert (row_length % block_size == 0) (row_length / block_size)

  let mat = loop mat for step < num_blocks - 1 do
        -- 1. compute the current diagonal block
        let diag =
          lud_diagonal
          (flat_index_2d mat (row_length * block_size * step + block_size * step)
                         block_size row_length
                         block_size 1)
        let mat = flat_update_2d mat (row_length * block_size * step + block_size * step)
                                 row_length 1 diag

        -- 2. compute the top perimeter
        let top_per_irreg =
           lud_perimeter_upper diag
                               (flat_index_3d mat (row_length * block_size * step + block_size * (step + 1))
                                              (num_blocks - step - 1) block_size
                                              block_size row_length
                                              block_size 1)
        let mat = flat_update_3d mat (row_length * block_size * step + block_size * (step + 1))
                                 block_size row_length 1 top_per_irreg

        -- 3. compute the left perimeter
        let lft_per_irreg =
          lud_perimeter_lower diag
                              (flat_index_3d mat (row_length * block_size * (step + 1) + block_size * step)
                                             (num_blocks - step - 1) (row_length * block_size)
                                             block_size row_length
                                             block_size 1)
        let mat = flat_update_3d mat (row_length * block_size * (step + 1) + block_size * step)
                                 (row_length * block_size) row_length 1 lft_per_irreg

        -- 4. compute the internal blocks
        let internal =
          lud_internal top_per_irreg lft_per_irreg
                       (flat_index_4d mat (row_length * block_size * (step + 1) + block_size * (step + 1))
                                      (num_blocks - step - 1) (row_length * block_size)
                                      (num_blocks - step - 1) block_size
                                      block_size row_length
                                      block_size 1)
        let mat = flat_update_4d mat (row_length * block_size * (step + 1) + block_size * (step + 1))
                                 (row_length * block_size) block_size row_length 1 internal

        in mat

  let last_step = num_blocks - 1
  let last_offset = last_step * block_size * row_length + last_step * block_size
  let v = lud_diagonal (flat_index_2d mat last_offset block_size row_length block_size 1)
  let mat = flat_update_2d mat last_offset row_length 1 v
  in mat

entry lud_2d [m] (mat: *[m][m]f32): [m][m]f32 =
  let mat = flatten mat
  let mat = lud 32 mat
  in unflatten mat
