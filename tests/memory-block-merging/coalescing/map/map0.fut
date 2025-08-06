-- ==
-- input { [[1,2,3], [4,5,6], [7,8,9]] 1i64 }
-- output { [[1,2,3], [5,6,7], [7,8,9]] }
-- structure gpu-mem { Alloc 0 }
-- structure seq-mem { Alloc 0 }

def main [n] (xss: *[n][n]i32) (i: i64) =
  let xs = map (+ 1) xss[i]
  let xss' = xss with [i] = xs
  in xss'
