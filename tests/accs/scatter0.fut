-- ==
-- input { [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19] }
-- output { [0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9] }

import "intrinsics"

def f (acc: *acc ([]i32)) i =
  let acc = write acc (i*2) (i32.i64 i)
  let acc = write acc (i*2+1) (i32.i64 i)
  in acc

def main (xs: *[]i32) =
  scatter_stream xs f (iota 10)
