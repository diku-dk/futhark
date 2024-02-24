-- ==
-- entry: main
-- input { [1,2] 10 }
-- output { [11, 12] }

entry main [n] (xs: [n]i32) (y : i32) : [n]i32 =
  xs + y
