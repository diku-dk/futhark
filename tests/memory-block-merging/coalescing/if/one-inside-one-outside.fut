-- An if expression where one branch array is defined outside the 'if', and one
-- is defined inside the 'if'.  This should be okay as long as the usual safety
-- conditions are kept, since 'ys0' and 'ys1' can use the same memory block
-- without 'ys0' being overwritten (it seems). However, we cannot handle this yet.
-- ==
-- input { [[1i64, 4i64], [9i64, 16i64]]
--         false
--         1i64
--       }
-- output { [[1i64, 4i64], [1i64, 2i64]]
--        }
-- structure gpu-mem { Alloc 2 }

def main [n] (xs: *[n][n]i64) (cond: bool) (i: i64) : [n][n]i64 =
  let ys0 = iota n
  let ys =
    if cond
    then ys0
    else let ys1 = map (+ 1) (iota n)
         in ys1
  let xs[i] = ys
  in xs
