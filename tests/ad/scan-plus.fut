-- Simple reduce with multiplication
-- ==
-- compiled input { [1.0f32, 2.0f32, 3.0f32, 4.0f32] [5.0f32, 4.0f32, 3.0f32, 2.0f32] } output { [14.0f32, 9.0f32, 5.0f32, 2.0f32] }

let scan_plus [n] (xs: [n]f32) : [n]f32 =
  scan (+) 0 xs

entry main [n] (xs: [n]f32) (ys_bar: [n]f32) =
  vjp scan_plus xs ys_bar
