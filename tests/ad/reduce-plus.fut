-- Simple reduce with multiplication
-- ==
-- compiled input { [1.0f32, 2.0f32, 3.0f32, 4.0f32] 2.0f32 } output { [2.0f32, 2.0f32, 2.0f32, 2.0f32] 10.0f32 }

let red_plus [n] (xs: [n]f32, c: f32) : f32 =
  reduce (+) 0 xs * c

entry main [n] (xs: [n]f32) (c: f32) =
  vjp red_plus (xs,c) 1