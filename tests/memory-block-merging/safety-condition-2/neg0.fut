-- Negative test.  We cannot fulfill safety condition 2, since 'ys' is allocated
-- before the function body is run, so 'xs', which is created in the body, can
-- never be allocated before 'ys'.
-- ==
-- input { 0
--         [10, 20, 30]
--       }
-- output { [[10, 20, 30],
--           [3, 4, 5],
--           [6, 7, 8]]
--        }

-- structure cpu { Alloc 1 }

let main (i: i32, ys: [#n]i32): [n][n]i32 =
  let xs = reshape (n, n) (iota (n * n))
  let xs[i] = ys
  in xs
