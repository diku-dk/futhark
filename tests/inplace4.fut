-- In-place update of a slice.
-- ==
-- input { [0,1,2,3,4] [42,42] }
-- output { [42,42,2,3,4] }
-- input { [42,42] [0,1,2,3,4] }
-- error: inplace4.fut:9

let main [n][m] (xs: *[n]i32) (ys: [m]i32) : [n]i32 =
  xs with [0:m] = ys
