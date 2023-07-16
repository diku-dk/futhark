-- ==
-- compiled random input { [16][16][8][8]f32 }
-- auto output
-- structure gpu-mem { Alloc 3 }

let lud_internal [b][m] (top_per: [m][b][b]f32) (lft_per: [m][b][b]f32) (mat_slice: [m][m][b][b]f32): *[m][m][b][b]f32 =
  let top_slice = map transpose top_per in
  #[incremental_flattening(no_outer)]
  map2 (\mat_arr lft ->
          #[incremental_flattening(no_outer)]
          map2 (\mat_blk top ->
                  #[incremental_flattening(only_intra)]
                  map2 (\mat_row lft_row ->
                          map2 (\mat_el top_row ->
                                  let prods = map2 (*) lft_row top_row
                                  let sum = f32.sum prods
                                  in mat_el - sum
                               ) mat_row top
                       ) mat_blk lft
               ) mat_arr top_slice
       ) mat_slice lft_per

let main [b][num_blocks] (matb: *[num_blocks][num_blocks][b][b]f32) =
  let top_per_irreg = matb[0,1:num_blocks]

  let col_slice = matb[1:num_blocks,0]

  let inner_slice = matb[1:num_blocks,1:num_blocks]
  let internal = lud_internal top_per_irreg col_slice inner_slice

  let matb[1:, 1:] = internal

  in matb
