-- Reshape/replicate simplification test.
-- ==
-- structure { Reshape 1 }

def main [n] (b: [n]i32, m: i64) =
  let x = n * m
  let c = b :> [x]i32
  let d = replicate 10 c
  in unflatten_3d 2 5 (n*m) d
