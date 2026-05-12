-- ==
-- input  { [1, 1, 1, 1, 1] }
-- output { [0i64, 1i64, 2i64, 3i64, 4i64] empty([0]i64)  }
def main [n] (cost: *[n]i32) =
  if opaque (true)
  then partition (\_ -> (opaque true)) (iota n)
  else ([], [])
