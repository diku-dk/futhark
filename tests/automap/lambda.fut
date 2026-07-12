-- ==
-- entry: main
-- random input { [10]f32 [10]f32  }

entry main [n](xs: [n]f32)  (ys: [n]f32): [n]f32 =
  map2 (*) xs ys
