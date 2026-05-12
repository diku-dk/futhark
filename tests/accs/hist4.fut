-- Tuple data.
-- ==
-- input {
-- [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19]
-- [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19]
-- }
-- output {
-- [0, 1, 3, 4, 6, 7, 9, 10, 12, 13, 15, 16, 18, 19, 21, 22, 24, 25, 27, 28]
-- [1, 2, 4, 5, 7, 8, 10, 11, 13, 14, 16, 17, 19, 20, 22, 23, 25, 26, 28, 29]
-- }

import "intrinsics"

def f (acc: *acc ([](i32, i32))) i =
  let acc = write acc (i * 2) (i32.i64 i, i32.i64 (i + 1))
  let acc = write acc (i * 2 + 1) (i32.i64 i, i32.i64 (i + 1))
  in acc

def main (xs: *[]i32) (ys: *[]i32) =
  let op x y = (x.0 + y.0, x.1 + y.1)
  in reduce_by_index_stream (zip xs ys) op (0, 0) f (iota 10) |> unzip
