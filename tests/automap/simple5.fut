-- ==
-- input { [1,2,3] 4 }
-- output { [5, 6, 7] }

entry main [n] (xs: [n]i32) (y : i32) : [n]i32 =
  (\x y -> x + y) xs y
