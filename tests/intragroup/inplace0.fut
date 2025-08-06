-- In-place updates at the group level are tricky, because we have to
-- be sure only one thread writes.
-- ==
-- random input { [1][256]f32 } auto output
-- structure gpu { SegMap/SegScan 2 SegMap/Update 1 }

def main (xss: [][]f32) =
  #[incremental_flattening(only_intra)]
  map (\xs ->
         let ys = scan (+) 0 xs
         let ys[0] = ys[0] + 1
         in scan (+) 0 ys)
      xss
