-- ==
-- error: `k`

type^ f = (k: i32) -> [k]i32 -> i32

let f : f = \_ xs -> length xs

let main [K] (input: [K]i32) =
  f K input
