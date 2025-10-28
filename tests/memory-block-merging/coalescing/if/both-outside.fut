-- An if expression where both branch arrays are defined outside the 'if'.  This
-- should *not* be okay, since 'ys0' and 'ys1' sharing the same memory means
-- that one of them gets overwritten.
-- ==
-- input { [[1i64, 4i64], [9i64, 16i64]]
--         false
--         1i64
--       }
-- output { [[1i64, 4i64], [1i64, 2i64]]
--        }
-- structure seq-mem { Alloc 2 }
-- structure gpu-mem { Alloc 2 }

def main [n] (xs: *[n][n]i64) (cond: bool) (i: i64) : [n][n]i64 =
  let ys0 = iota n
  let ys1 = map (+ 1) (iota n)
  let ys =
    if cond
    then ys0
    else ys1
  let xs[i] = ys
  in xs
