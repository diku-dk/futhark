-- 2D tiling where the loop to tile is inside a sequential loop, that
-- is variant to an outermost dimension.
-- ==
-- input { [1,2,3] [[1,2,3],[4,5,6]] [[1,2,3],[4,5,6]] } auto output
-- script input { mk_input 33i64 5i64 80i64 } auto output
-- structure gpu { SegMap/Loop/Loop/SegMap 2 }

-- With k=80 and the default tile size of 32, the tiled reduction has
-- two whole tiles plus a partial one.
local entry mk_input (a: i64) (b: i64) (k: i64) : ([]i32, [a][k]i32, [b][k]i32) =
  let f s i j = i32.i64 ((i * 7 + j * 13 + s) % 19) - 9
  in ([1, 3, 2], tabulate_2d a k (f 0), tabulate_2d b k (f 5))

def main [k] (ns: []i32) (xss: [][k]i32) (yss: [][k]i32) =
  map (\n ->
         map (\xs' ->
                map (\ys' ->
                       loop z = 0
                       for _p < n do
                         #[sequential] i32.sum (map (+ z) (map2 (*) xs' ys')))
                    yss)
             xss)
      ns
