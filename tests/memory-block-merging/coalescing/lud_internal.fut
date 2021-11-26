-- ==
-- structure gpu-mem { Alloc 2 }

let lud_internal [m][b] (top_per: [m][b][b]f32) (lft_per: [m][b][b]f32) (mat_slice: [m][m][b][b]f32): *[m][m][b][b]f32 =
  let top_slice = map transpose top_per in
  #[incremental_flattening(only_intra)]
  map2 (\mat_arr lft ->
          map2 (\mat_blk top ->
                  map2 (\mat_row lft_row ->
                          map2 (\mat_el top_row ->
                                  let prods = map2 (*) lft_row top_row
                                  let sum = f32.sum prods
                                  in mat_el - sum
                               ) mat_row top
                       ) mat_blk lft
               ) mat_arr top_slice
       ) mat_slice lft_per

let main [num_blocks][b] (matb: *[num_blocks][num_blocks][b][b]f32) =
  let n = b * num_blocks

  let step = 0

  let slice_size = step + 1 - num_blocks
  let diag = iota (b * b) |> map f32.i64 |> unflatten b b |> opaque
  let top_per_irreg = matb[step,step+1:num_blocks]

  -- 3. compute the left perimeter and update matrix
  let col_slice = matb[step+1:num_blocks,step]
  -- let lft_per_irreg = lud_perimeter_lower diag col_slice

  -- 4. compute the internal blocks
  let inner_slice = matb[step+1:num_blocks,step+1:num_blocks]
  -- let internal = lud_internal top_per_irreg (opaque lft_per_irreg) inner_slice
  let internal = lud_internal top_per_irreg col_slice inner_slice

  -- let matb[step+1:, step] = lft_per_irreg --
  let matb[step+1:, step] = opaque (copy col_slice)
  let matb[step+1:, step+1:] = internal


  in matb
