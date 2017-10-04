-- Same as if-neg-3.fut, but with an extra nested if.  This should not produce
-- any coalescings either.
-- ==
-- input { true true
--         [9, 9] [9, 9]
--         [[0, 0], [0, 0]]
--       }
-- output { [[1, 2], [0, 0]]
--          10 10
--        }
-- structure cpu { Alloc 3 }
-- structure gpu { Alloc 3 }

let main [n] (cond0: bool, cond1: bool,
              y0: [n]i32, z0: [n]i32,
              x: *[n][n]i32): (*[n][n]i32, i32, i32) =
  let y = map (+ 1) y0
  let z = map (+ 1) z0
  let (a, b, c) =
    if cond0
    then if cond1
         then let y1 = map (+ 1) (iota n)
              in (y1, y[0], z[0])
         else (y, 0, 0)
    else (z, 0, 0)
  let x[0] = a
  in (x, b, c)
