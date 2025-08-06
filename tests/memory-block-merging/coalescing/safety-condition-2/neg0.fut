-- Negative test.  We cannot fulfill safety condition 2, since 'ys' is allocated
-- before the function body is run, so 'xs', which is created in the body, can
-- never be allocated before 'ys'.
-- ==
-- input { 0i64
--         [10i64, 20i64, 30i64]
--       }
-- output { [[10i64, 20i64, 30i64],
--           [3i64, 4i64, 5i64],
--           [6i64, 7i64, 8i64]]
--        }
-- structure seq-mem { Alloc 1 }
-- structure gpu-mem { Alloc 1 }

def main [n] (i: i64) (ys: [n]i64) : [n][n]i64 =
  let xs = tabulate_2d n n (\i j -> i * n + j)
  let xs[i] = ys
  in xs
