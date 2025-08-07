-- ==
-- input { [[1,2,3,4]] }
-- output { [[11, 7, 4, 2]] }
-- structure gpu-mem { Alloc 2 }

def main [n] [m] (xs: [n][m]i32) =
  #[incremental_flattening(only_intra)]
  map (\row ->
         let ys = scan (+) 0 row
         let zs = map (+ 1) ys
         in reverse zs)
      xs
