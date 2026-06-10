-- ==
-- input { 3i64 [0i64, 1i64, 2i64, 3i64, 4i64, 5i64, 6i64, 7i64, 8i64, 9i64] }
-- auto output

entry main (n: i64) (xs: [10]i64) : [n][10]i64 =
  map (\i ->
         let v = replicate 2 (replicate (i + 1) (i * 10))
         in intrinsics.flat_update_2d (copy xs) 1 2 2 v)
      (iota n)
