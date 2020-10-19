-- ==
--
-- compiled random input {[256][16][16]f32 [256][16][16]f32 [256][256][16][16]f32} auto output
--
-- compiled random input {[128][32][32]f32 [128][32][32]f32 [128][128][32][32]f32} auto output


let main [m][b] (top_per: [m][b][b]f32)
                (lft_per: [m][b][b]f32)
                (mat_slice: [m][m][b][b]f32 )
              : *[m][m][b][b]f32 =
  -- let top_slice = map transpose top_per in
  map (\(mat_arr: [m][b][b]f32, lft: [b][b]f32): [m][b][b]f32  ->
        map (\ (mat_blk: [b][b]f32, top: [b][b]f32): [b][b]f32  ->
                map  (\ (mat_row: [b]f32, lft_row: [b]f32): [b]f32  ->
                        map2 (\mat_el top_row ->
                                let prods = map2 (*) lft_row top_row
                                let sum   = f32.sum prods
                                in mat_el - sum
                             ) mat_row (transpose top)
                    ) (zip (mat_blk) lft )
           ) (zip (mat_arr) (top_per) )
     ) (zip (mat_slice) (lft_per) )
