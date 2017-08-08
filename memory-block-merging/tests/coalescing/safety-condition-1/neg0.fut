-- Negative test.  We cannot fulfill safety condition 1, since 'ys' is used
-- after the coalescing-enabling line (in 'zs').
-- ==
-- input { 3
--         [0, 1, 2, 3]
--       }
-- output { [[0, 1, 2, 3],
--           [4, 5, 6, 7],
--           [8, 9,10,11],
--           [1, 2, 3, 4]]
--          [2, 3, 4, 5]
--        }
-- structure cpu { Alloc 3 }
-- structure gpu { Alloc 3 }

let main (i: i32, ys0: [#n]i32): ([n][n]i32, [n]i32) =
  let ys = map (+ 1) ys0
  let xs = reshape (n, n) (iota (n * n))
  let xs[i] = ys
  let zs = map (+ 1) ys
  in (xs, zs)
