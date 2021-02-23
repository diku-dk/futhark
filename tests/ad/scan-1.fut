-- Simple reduce with multiplication
-- ==
-- compiled input { [1.0f32, 2.0f32, 3.0f32, 4.0f32] [5.0f32, 4.0f32, 3.0f32, 2.0f32] } output { [79.0f32, 37.0f32, 22.0f32, 12.0f32] }

let scan_mult [n] (xs: [n]f32) : [n]f32 =
  scan (*) 1 xs

entry main [n] (xs: [n]f32) (ys_bar: [n]f32) =
  vjp scan_mult xs ys_bar
