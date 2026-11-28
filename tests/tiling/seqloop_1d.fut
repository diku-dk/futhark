-- Two dimensions in the kernel, but we are only tiling along the
-- innermost one.
-- ==
-- input { [1,2,3] [[1,2,3],[4,5,6],[7,8,9]] [1,2,3] } auto output
-- script input { mk_input 600i64 300i64 } auto output
-- structure gpu { SegMap/Loop/Loop/SegMap 2 }

-- With m=600 and the default block size of 256, the tiled reduction
-- has two whole tiles plus a partial one.
local entry mk_input (m: i64) (k: i64) : ([]i32, [3][m]i32, [k]i32) =
  let f s i j = i32.i64 ((i * 7 + j * 13 + s) % 19) - 9
  in ([1, 3, 2], tabulate_2d 3 m (f 0), tabulate k (f 5 0))

def main (ns: []i32) (xs: [][]i32) (ys: []i32) =
  map (\n ->
         map (\y ->
                loop y for i < n do
                  #[sequential] i32.sum (map (+ y) (#[unsafe] xs[i])))
             ys)
      ns
