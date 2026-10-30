-- ==
-- structure gpu-mem { Alloc 1 }
-- structure seq-mem { Alloc 1 }

def main [n] (zss: *[n][n]f32) (x: f32) (y: f32) (i: i64) (j: i64) : *[n][n]f32 =
  let ys = replicate n y
  let xs = replicate n x
  let zss[i] = xs
  let zss[j] = ys
  in zss
