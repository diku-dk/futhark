-- An if expression where both branch arrays are defined inside the 'if'.  This
-- should be okay as long as the usual safety conditions are kept, since 'ys0'
-- and 'ys1' exist independently of each other.
-- ==
-- input { [[1, 4], [9, 16]]
--         false
--         1
--       }
-- output { [[1, 4], [1, 2]]
--        }
-- structure cpu { Alloc 0 }
-- structure gpu { Alloc 0 }

let main [n] (xs: *[n][n]i32, cond: bool, i: i32): [n][n]i32 =
  -- Both branches will use the memory of ys, which will use the memory of
  -- xs[i].
  let ys = if cond
           then let ys0 = iota n
                in ys0
           else let ys1 = map (+ 1) (iota n)
                in ys1

  -- xs is not allocated in this body, so we end up with zero allocations.
  let xs[i] = ys
  in xs
