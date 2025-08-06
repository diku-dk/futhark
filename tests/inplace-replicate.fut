-- ==
-- input { [1,2,3,4] 2i64 42 } output { [1i32, 2i32, 42i32, 4i32] }
-- structure { Replicate 0 Assert 1 }

def main (xs: *[]i32) (i: i64) (v: i32) =
  xs with [i:i + 1] = replicate 1 v
