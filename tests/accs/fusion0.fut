-- ==
-- input { [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19] }
-- output { [0, 2, 4, 7, 8, 12, 13, 17, 16, 24, 11, 16, 15, 19, 16, 27, 16, 25, 26, 28] }
-- structure { /WithAcc/SplitAcc/Screma/Screma 0 }

import "intrinsics"

def f (acc: *acc ([]i32)) i =
  let js = scan (+) 0 (map (+ i) (iota 10))
  in loop acc for j in js do
       write acc j (i32.i64 i)

def main (xs: *[]i32) =
  reduce_by_index_stream xs (+) 0 f (iota 10)
