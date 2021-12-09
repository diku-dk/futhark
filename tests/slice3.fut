-- Slicing produces a size that we can obtain.
-- ==
-- input { [1,2,3] 0i64 1i64 } output { 1i64 }

def main (xs: []i32) (i: i64) (j: i64) =
  length xs[i:j]
