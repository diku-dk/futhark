-- ==
-- input { [[1i64,2i64,3i64],[4i64,5i64,6i64],[7i64,8i64,9i64]] 1i64 }
-- output { [[1i64,1i64,1i64], [5i64,6i64,7i64], [8i64,9i64,10i64]] }
-- structure gpu-mem { Alloc 2 }
-- structure seq-mem { Alloc 2 }

def main [n] (xss: [n][n]i64) (i: i64) =
  -- The basis array
  let xss = map (map (+ 1)) xss
  -- There's also an allocation here
  let xs = replicate n i
  -- This loop and the result inside should be short-circuited
  let xss =
    loop xss for j < i do
      xss with [j] = xs
  let xss = copy xss
  in xss
