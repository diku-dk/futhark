-- ==
-- error: Causality check

def f [n] (_: [n]i32 -> i32) : [n]i32 -> i64 =
  let m = n + 1
  in \_ -> m

def main xs = f (\_ -> 0) <| filter (> 0) xs
