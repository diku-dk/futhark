-- ==
-- input { [[1,2,3,4]] }
-- auto output
-- structure gpu-mem { Alloc 3 }

def main [n][m] (xs: [n][m]i32) =
  #[incremental_flattening(only_intra)]
  map (\row ->
         let ys = scan (+) 0 row
         let zs = map (+1) ys
         in reverse zs) xs
