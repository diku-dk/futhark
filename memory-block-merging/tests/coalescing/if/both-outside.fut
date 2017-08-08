-- An if expression where both branch arrays are defined outside the 'if'.  This
-- should *not* be okay, since 'ys0' and 'ys1' sharing the same memory means
-- that one of them gets overwritten.
-- ==
-- input { [[1, 4], [9, 16]]
--         false
--         1
--       }
-- output { [[1, 4], [1, 2]]
--        }
-- structure cpu { Alloc 2 }
-- structure gpu { Alloc 2 }

let main (xs: *[#n][#n]i32, cond: bool, i: i32): [n][n]i32 =
  let ys0 = iota n
  let ys1 = map (+ 1) (iota n)
  let ys = if cond
           then ys0
           else ys1
  let xs[i] = ys
  in xs
