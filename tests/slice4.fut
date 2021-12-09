-- Zero strides are detected.
-- ==
-- input { [1,2,3,4,5] 0i64 1i64 0i64 }
-- error: out of bounds

def main (xs: []i32) a b c =
  xs[a:b:c]
