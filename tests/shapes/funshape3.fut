-- ==
-- error: Causality check

let f [n] (_: [n]i32) (_: [n]i32 -> i32, _: [n]i32) : i32 =
  n

let main x = f (iota (x+2)) (\_ -> 0, iota (x+2))
