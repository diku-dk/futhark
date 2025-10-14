-- ==
-- structure gpu-mem { Alloc 0 }
-- structure seq-mem { Alloc 0 }

def main [n] (xsss: *[n][n][n]i64) (ass0: [n][n]i64) (as0: [n]i64) (i: i64) : [n][n][n]i64 =
  let ass = map (map (* 5i64)) ass0 |> opaque
  let as = map (+ 3i64) as0
  let ass[i] = as
  let xsss[i] = ass
  in xsss
