-- ==
-- input { 5000i64 [1i64] }
-- error:

def main [n] (k: i64) (dst: *[n]i64) =
  let src = iota k
  in dst with [1:4] = src
