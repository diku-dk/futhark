-- Interchange map-invariant branches to exploit all parallelism.
-- ==
-- structure gpu { Kernel/If 0 }

def main (b: bool) (xs: []i32) (ys: []i32) =
  map (\x -> if b then map (+ x) ys else ys) xs
