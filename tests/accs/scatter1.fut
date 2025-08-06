-- ==
-- input { [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19] }
-- output { [0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 18, 19] }

import "intrinsics"

def f (acc: *acc ([]i32)) i =
  loop acc for j < i do
    write acc (j + i) 1

def main (xs: *[]i32) =
  scatter_stream xs f (iota 10)
