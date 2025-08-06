-- ==
-- input { 4i64 5i64 2i64 }
-- output { [[0, 0, 1, 1, 0], [0, 0, 1, 1, 1], [0, 0, 1, 1, 2], [0, 0, 1, 1, 3]] }

import "intrinsics"

def f (acc: *acc ([]i32)) i =
  let acc = write acc (i * 2) (i32.i64 i)
  let acc = write acc (i * 2 + 1) (i32.i64 i)
  in acc

def main n m k =
  tabulate n (\i ->
                let xs = replicate m (i32.i64 i)
                in scatter_stream xs f (iota k))
