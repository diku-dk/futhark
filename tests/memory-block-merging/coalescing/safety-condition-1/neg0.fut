-- Negative test.  We cannot fulfill safety condition 1, since 'ys' is used
-- after the coalescing-enabling line (in 'zs').
--
-- However, the new fusion engine fuses the two maps (ys and zs), meaning that
-- it actually is short-circuited on the seq-mem backend.
-- ==
-- input { 3i64
--         [0i64, 1i64, 2i64, 3i64]
--       }
-- output { [[0i64, 1i64, 2i64, 3i64],
--           [4i64, 5i64, 6i64, 7i64],
--           [8i64, 9i64, 10i64, 11i64],
--           [1i64, 2i64, 3i64, 4i64]]
--          [2i64, 3i64, 4i64, 5i64]
--        }
-- structure seq-mem { Alloc 2 }
-- structure gpu-mem { Alloc 2 }

def main [n] (i: i64) (ys0: [n]i64) : ([n][n]i64, [n]i64) =
  let xs = tabulate_2d n n (\i j -> i * n + j) |> opaque
  let ys = map (+ 1) ys0
  let xs[i] = ys
  let zs = map (+ 1) ys
  -- This could also be a short-circuit point in SeqMem

  in (xs, zs)
