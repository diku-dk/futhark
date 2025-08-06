-- An if expression where both branch arrays are defined inside the 'if'.  This
-- should be okay as long as the usual safety conditions are kept, since 'ys0'
-- and 'ys1' exist independently of each other.
-- ==
-- input { [[1i64, 4i64], [9i64, 16i64]]
--         false
--         1i64
--       }
-- output { [[1i64, 4i64], [1i64, 2i64]]
--        }
-- structure seq-mem { Alloc 0 }
-- structure gpu-mem { Alloc 0 }

def main [n] (xs: *[n][n]i64) (cond: bool) (i: i64) : [n][n]i64 =
  -- Both branches will use the memory of ys, which will use the memory of
  -- xs[i].
  let ys =
    if cond
    then let ys0 = iota n
         in ys0
    else let ys1 = map (+ 1) (iota n)
         in ys1
  -- xs is not allocated in this body, so we end up with zero allocations.
  let xs[i] = ys
  in xs
