-- ==
-- input { [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19] }
-- output { [0, 2, 5, 8, 11, 14, 17, 20, 23, 26, 19, 11, 12, 13, 14, 15, 16, 17, 18, 19] }

import "intrinsics"

def f (acc: *acc ([]i32)) i =
  let acc = write acc i (i32.i64 i)
  let acc = write acc (i + 1) (i32.i64 i)
  in acc

def main (xs: *[]i32) =
  reduce_by_index_stream xs (+) 0 f (iota 10)
