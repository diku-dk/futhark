-- ==
-- input { [[ 0, 1, 2, 3], [ 4, 5, 6, 7]] }
-- output { [[10, 11, 12, 13], [30, 31, 32, 33]] }
-- random input { [100][10]i32 }
-- auto output
-- structure gpu-mem { Alloc 2 }

def main (xss: [][]i32) =
  #[incremental_flattening(only_intra)]
  map (\xs ->
         let as = map (+ 1) xs |> opaque
         let a = reduce (+) 0 as
         let bs = map (+ a) xs
         in bs)
      xss
