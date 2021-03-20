-- Simple reduce with vector addition
-- ==
-- compiled input { [ [1f32, 2f32,  3f32,  4f32]
--					, [5f32, 6f32,  7f32,  8f32]
--					, [9f32, 10f32, 11f32, 12f32]
--                  ]
-- 					[13f32, 14f32, 15f32, 16f32]
--				  }
-- output 		  { [ [13f32, 14f32, 15f32, 16f32]
--					, [13f32, 14f32, 15f32, 16f32]
--					, [13f32, 14f32, 15f32, 16f32]
--                  ]
--				  }

let red_vct_plus [n][m] (xs: [n][m]f32) : [m]f32 =
  reduce (map2 (+)) (replicate m 0f32) xs

entry main [n][m] (xss: [n][m]f32) (rs_bar: [m]f32) =
  vjp red_vct_plus (xss) rs_bar


-- [[1f32,2f32,3f32,4f32],[5f32,6f32,7f32,8f32],[9f32,10f32,11f32,12f32]] [13f32,14f32,15f32,16f32]