-- ==
-- structure gpu-mem { Alloc 3 }
-- structure seq-mem { Alloc 3 }

def main [n] (ind: i64) (ass: [n][n]i64) (as: [n]i64) (inds: [n]i64) =
  let yss = replicate n (replicate n 33)
  let bs = iota n
  let bs =
    loop bs
    for i < n do
      map (* i) bs
  let yss[ind] = bs
  in yss
