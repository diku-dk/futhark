-- ==
-- input { [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19] }
-- output { [0, 1, 3, 4, 6, 7, 9, 10, 12, 13, 15, 16, 18, 19, 21, 22, 24, 25, 27, 28] }

import "intrinsics"

let f (acc: *acc ([]i32)) i =
  let acc = write acc (i*2) (i32.i64 i)
  let acc = write acc (i*2+1) (i32.i64 i)
  in acc

let main (xs: *[]i32) =
  reduce_by_index_stream xs (+) 0 f (iota 10)
