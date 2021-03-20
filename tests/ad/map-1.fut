-- Simple map with free scalar variable
-- ==
-- compiled input { [1.0f32, 2.0f32, 3.0f32, 4.0f32]
--					3.0f32
--					[5.0f32, 6.0f32, 7.0f32, 8.0f32]
--				  }
-- output { [25.0f32, 42.0f32, 63.0f32, 88.0f32]
--			70.0f32
--		  }

let map_mult [n] (xs: [n]f32, a:f32) : [n]f32 =
  map (\x -> x*a + x*x) xs

entry main [n] (xs: [n]f32) (a: f32) (rs_: [n]f32) =
  vjp map_mult (xs,a) rs_
