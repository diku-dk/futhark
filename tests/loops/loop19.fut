-- A local higher-order function that loops.
-- ==
-- input { [1.0, 2.0, 3.0] 4i64 }
-- output { [5.0, 6.0, 7.0] }

entry main [n] (xs: [n]f64) (k: i64) : [n]f64 =
  let pow f k x = loop x for _i < k do f x
  let f = map (+ 1.0)
  in pow f k xs
