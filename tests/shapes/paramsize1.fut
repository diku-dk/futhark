-- ==
-- error: "k"

type^ f = (k: i64) -> [k]i32 -> i64

let f : f = \_ xs -> length xs

let main [K] (input: [K]i32) =
  f K input
