-- Interchange map-invariant branches to exploit all parallelism.
-- ==
-- structure distributed { Kernel/If 0 }

let main (b: bool) (xs: []i32) (ys: []i32) =
  map (\x -> if b then map (+x) ys else ys) xs
