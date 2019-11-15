-- ==
-- input { [1,2,3] }
-- output { 3 }

type^ f = (k: i32) -> [k]i32 -> i32

let f : f = \n (xs: [n]i32) -> length xs

let main [K] (input: [K]i32) =
  f K input
