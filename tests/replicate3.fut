-- Reshape/replicate simplification test.
-- ==
-- structure { Reshape 1 }

def main [n] (b: [n]i32, m: i64) =
  let x = n * m
  let c = b :> [x]i32
  let d = replicate (2*5*(n*m)) c
  in unflatten_3d d
