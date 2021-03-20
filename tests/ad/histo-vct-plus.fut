-- Simple histogram with vector addition
-- ==
-- compiled input { [ 0i64, 1i64, 0i64, -1i64, 0i64, -1i64]
--					[ [ 1f32,  2f32]
--					, [ 3f32,  4f32]
--					, [ 5f32,  6f32]
--					, [ 7f32,  8f32]
--					, [ 9f32, 10f32]
--					, [11f32, 12f32]
--                  ]
-- 					[ [1f32, 2f32], [3f32, 4f32] ]
--					[ [9f32, 8f32], [7f32, 6f32] ]
--				  }
-- output 		  { [ [9f32, 8f32]
--					, [7f32, 6f32]
--					, [9f32, 8f32]
--					, [0f32, 0f32]
--					, [9f32, 8f32]
--					, [0f32, 0f32]
--					]
--					[ [9f32, 8f32], [7f32, 6f32] ]
--				  }

let histo_vct_plus [n][d][w] (is: [n]i64) (xs: [n][d]f32, hist: [w][d]f32) : [w][d]f32 =
  reduce_by_index (copy hist) (map2 (+)) (replicate d 0f32) is xs

entry main [n][d][w] (is: [n]i64) (xss: [n][d]f32) (hist: [w][d]f32) (hist_bar: [w][d]f32) =
  vjp (histo_vct_plus is) (xss, hist) hist_bar
