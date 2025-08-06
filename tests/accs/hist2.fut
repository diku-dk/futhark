-- Writing an array with a vector operator.
-- ==
-- input { [[2],[3],[4]] }
-- output { [[3i32], [3i32], [5i32]] }

import "intrinsics"

def f (acc: *acc ([][]i32)) (i, x) =
  let acc = write acc (i * 2) x
  in acc

def main [n] (xs: *[][n]i32) =
  reduce_by_index_stream xs
                         (map2 (+))
                         (replicate n 0)
                         f
                         (zip (iota 10) (replicate 10 (replicate n 1)))
