-- ==
-- input { [[1,2,3], [4,5,6], [7,8,9]] 1i64 }
-- output { [[1,2,3], [11,13,15], [7,8,9]] }
-- structure gpu-mem { Alloc 1 }
-- structure seq-mem { Alloc 0 }

def main [n] (xss: *[n][n]i32) (i: i64) =
  let xs = map2 (+) xss[i] xss[i + 1]
  let xss' = xss with [i] = xs
  in xss'
