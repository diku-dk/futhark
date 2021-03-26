-- Simple reduce with vectorized addition
-- ==
-- compiled input { [ [1.0f32, 2.0f32, 3.0f32, 4.0f32]
--					, [1.0f32, 2.0f32, 3.0f32, 4.0f32]
--					, [1.0f32, 2.0f32, 3.0f32, 4.0f32]
--					, [1.0f32, 2.0f32, 3.0f32, 4.0f32]
--					]
--					[ [5.0f32, 5.0f32, 5.0f32, 5.0f32]
--					, [4.0f32, 4.0f32, 4.0f32, 4.0f32]
--					, [3.0f32, 3.0f32, 3.0f32, 3.0f32]
--					, [2.0f32, 2.0f32, 2.0f32, 2.0f32]
--					]
--				  }
-- output { [ [14.0f32, 14.0f32, 14.0f32, 14.0f32]
--			, [ 9.0f32,  9.0f32,  9.0f32,  9.0f32]
--			, [ 5.0f32,  5.0f32,  5.0f32,  5.0f32]
--			, [ 2.0f32,  2.0f32,  2.0f32,  2.0f32]
--			]
--		  }

let scan_mult [n][d] (xs: [n][d]f32) : [n][d]f32 =
  scan (map2 (+)) (replicate d 0) xs

entry main [n][d] (xs: [n][d]f32) (ys_bar: [n][d]f32) =
  vjp scan_mult xs ys_bar
