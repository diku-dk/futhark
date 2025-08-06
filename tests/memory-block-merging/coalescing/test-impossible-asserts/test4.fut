-- ==
-- structure gpu-mem { Alloc 1 }
-- structure seq-mem { Alloc 1 }

def main [n] (zss: *[n][n]f64) (x: f64) (y: f64) (i: i64) (j: i64) : *[n][n]f64 =
  let ys = replicate n y
  let xs = replicate n x
  let zss[i] = xs
  let zss[j] = ys
  in zss
