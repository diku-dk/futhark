-- Parallel blocked LU-decomposition.
--
-- ==
-- structure gpu-mem { Alloc 30 }
-- structure seq-mem { Alloc 20 }

def block_size: i64 = 32


def dotprod [n] (a: [n]f32) (b: [n]f32): f32 =
  map2 (*) a b
  |> reduce (+) 0

def lud_diagonal [b] (a: [b][b]f32): *[b][b]f32 =
  #[incremental_flattening(only_intra)]
  map (\mat ->
         let mat = copy mat
         in #[unsafe]
            loop (mat: *[b][b]f32) for i < b-1 do
            let col = map (\j -> if j > i then
                                   (mat[j,i] - (dotprod mat[j,:i] mat[:i,i])) / mat[i,i]
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
        map (\a1 ->
               map (\row0 -> -- Upper
                      #[unsafe]
                      loop row = copy row0 for i < b do
                      let sum = loop sum=0.0f32 for k < i do
                                  sum + diag[i,k] * row[k]
                      let row[i] = row[i] - sum
                      in  row
                   ) a1
            ) a1s
    in map transpose a2s

def lud_perimeter_lower [b][m] (diag: [b][b]f32) (mat: [m][b][b]f32): *[m][b][b]f32 =
  map (\blk ->
         map (\row0 -> -- Lower
                #[unsafe]
                loop row = copy row0 for j < b do
                let sum = loop sum=0.0f32 for k < j do
                            sum + diag[k,j] * row[k]
                let row[j] = (row[j] - sum) / diag[j,j]
                in  row
             ) blk
      ) mat

def lud_internal [m][b] (top_per: [m][b][b]f32) (lft_per: [m][b][b]f32) (mat_slice: [m][m][b][b]f32): *[m][m][b][b]f32 =
  let top_slice = map transpose top_per in
  #[incremental_flattening(only_inner)]
  map2 (\mat_arr lft ->
        #[incremental_flattening(only_inner)]
        map2 (\mat_blk top ->
                #[incremental_flattening(only_inner)]
                map2 (\mat_row lft_row ->
                        #[sequential_inner]
                        map2 (\mat_el top_row ->
                                let prods = map2 (*) lft_row top_row
                                let sum = f32.sum prods
                                in mat_el - sum
                             ) mat_row top
                    ) mat_blk lft
           ) mat_arr top_slice
     ) mat_slice lft_per

def main [num_blocks] (matb: *[num_blocks][num_blocks][32][32]f32): *[num_blocks][num_blocks][32][32]f32 =
    #[unsafe]
    let matb = loop matb for step < num_blocks - 1 do
        -- 1. compute the current diagonal block
        let diag = lud_diagonal matb[step,step] in

        -- 2. compute the top  perimeter
        let row_slice = matb[step,step+1:num_blocks]
        let top_per_irreg = lud_perimeter_upper diag row_slice

        -- 3. compute the left perimeter and update matrix
        let col_slice = matb[step+1:num_blocks,step]
        let lft_per_irreg = lud_perimeter_lower diag col_slice |> opaque

        -- 4. compute the internal blocks
        let inner_slice = matb[step+1:num_blocks,step+1:num_blocks]
        let internal = lud_internal top_per_irreg lft_per_irreg inner_slice

        -- 5. update matrix in place
        let matb[step, step] = diag
        let matb[step, step+1:num_blocks] = top_per_irreg
        let matb[step+1:num_blocks, step] = lft_per_irreg
        let matb[step+1:num_blocks, step+1:num_blocks] = internal
        in matb

    let last_step = num_blocks - 1 in
    let matb[last_step,last_step] =
      lud_diagonal matb[last_step, last_step]

    in matb
