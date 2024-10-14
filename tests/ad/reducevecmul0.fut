-- ==
-- entry: rev
-- input { [[0.0f32, 2.0f32, 0.0f32, 4.0f32], [4.0f32, 2.0f32, 0.0f32, 0.0f32]] } output { [[4.000000f32, 0.000000f32, 0.000000f32, 0.000000f32], [0.000000f32, 0.000000f32, 0.000000f32, 0.000000f32]] }

let red_mult [m][n] (xs: [m][n]f32) : [n]f32 =
  reduce (map2 (*)) (replicate n 1) xs

entry rev [m][n] (xs: [m][n]f32)  =
  vjp red_mult xs (replicate n 0 with [0] = 1)

let main = rev