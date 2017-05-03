-- Negative test.  We cannot fulfill safety condition 1, since 'ys' is used
-- after the coalescing-enabling line.
-- ==
-- input { 3
--         4
--       }
-- output { [[0, 1, 2, 3],
--           [4, 5, 6, 7],
--           [8, 9,10,11],
--           [1, 2, 3, 4]]
--          [1, 2, 3, 4]
--        }

let main (i: i32, n: i32): ([n][n]i32, [n]i32) =
  let ys = map (+ 1) (iota n)
  let xs = reshape (n, n) (iota (n * n))
  let xs[i] = ys
  in (xs, ys)
