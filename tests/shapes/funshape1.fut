-- ==
-- error: Causality check

let f [n] (_: [n]i32 -> i32) : [n]i32 -> i32 =
  let m = n + 1
  in \_ -> m

let main xs = f (\_ -> 0) <| filter (>0) xs
