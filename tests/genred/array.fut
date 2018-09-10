-- Test gen_reduce on array of arrays
-- ==

let main [m][n] (xs : *[n][m]i32) (image : *[n]i32) : *[n][m]i32 =
  gen_reduce xs (\x y -> map2 (+) x y) (replicate m 0) image (replicate n (iota m))
