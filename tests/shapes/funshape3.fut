-- ==
-- error: Causality check

def f [n] (_: [n]i64) (_: [n]i64 -> i32, _: [n]i64) =
  n

def main x = f (iota (x+2)) (\_ -> 0, iota (x+2))
