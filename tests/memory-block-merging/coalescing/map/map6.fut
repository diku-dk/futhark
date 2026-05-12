-- ==
-- structure gpu-mem { Alloc 1 }
-- structure seq-mem { Alloc 2 }

def main [n] (xs: [n]i64) =
  let ys = map (+ 1) xs
  let zs = map (* 2) xs
  in concat ys zs
