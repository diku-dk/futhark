-- ==
-- compiled random input { [1023]bool [1023][256]i32 } auto output
-- structure gpu {
--   /SegMap/If/True/SegMap 1
--   /SegMap/If/False/SegScan 1
-- }

def main (bs: []bool) (xss: [][]i32) =
  #[incremental_flattening(only_intra)]
  map2 (\b xs ->
          if b
          then map (+ 2) xs
          else map (+ 3) (scan (+) 0 xs))
       bs
       xss
