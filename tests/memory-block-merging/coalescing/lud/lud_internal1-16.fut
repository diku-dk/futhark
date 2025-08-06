-- ==
-- structure gpu-mem { Alloc 2 }
-- structure seq-mem { Alloc 3 }

def lud_internal [m] (top_per: [m][16][16]f32) (lft_per: [m][16][16]f32) (mat_slice: [m][m][16][16]f32) : *[m][m][16][16]f32 =
  let top_slice = map transpose top_per
  in #[incremental_flattening(only_intra)]
     map2 (\mat_arr lft ->
             map2 (\mat_blk top ->
                     map2 (\mat_row lft_row ->
                             map2 (\mat_el top_row ->
                                     let prods = map2 (*) lft_row top_row
                                     let sum = f32.sum prods
                                     in mat_el - sum)
                                  mat_row
                                  top)
                          mat_blk
                          lft)
                  mat_arr
                  top_slice)
          mat_slice
          lft_per

def main [num_blocks] (matb: *[num_blocks][num_blocks][16][16]f32) =
  let top_per_irreg = matb[0, 1:num_blocks]
  let col_slice = matb[1:num_blocks, 0]
  let inner_slice = matb[1:num_blocks, 1:num_blocks]
  let internal = lud_internal top_per_irreg col_slice inner_slice
  let matb[1:, 1:] = internal
  in matb
