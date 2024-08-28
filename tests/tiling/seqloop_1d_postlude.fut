-- A prelude value is used both within the tiled loop and the
-- postlude.
-- ==
-- random input { 3 [10][20]i32 [10]i32 } auto output
-- structure gpu { SegMap/Loop/Loop/SegMap 2 }

def main (n: i32) (xs: [][]i32) (ys: []i32) =
  map (\y ->
         let y' = loop y for i < n do
                    #[sequential] i32.sum (map (+y) (#[unsafe] xs[i]))
         in y + y')
      ys
