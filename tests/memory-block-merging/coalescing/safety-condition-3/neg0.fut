-- Negative test.  'xs' is used while 'ys' is live, so we cannot merge their
-- memory blocks, since 'zs' would then map over the contents of 'ys' instead of
-- the original contents of 'xs[i]'.
-- ==
-- input { [[2, 2],
--          [2, 2]]
--         [3, 4]
--         1i64
--       }
-- output { [[2, 2],
--           [4, 5]]
--          6
--        }
-- structure seq-mem { Alloc 2 }
-- structure gpu-mem { Alloc 3 }

def main [n] (xs: *[n][n]i32) (ys0: [n]i32) (i: i64) : ([n][n]i32, i32) =
  let ys = map (+ 1) ys0
  let zs = map (+ ys[0]) xs[i]
  -- Cannot be hoisted to exist before 'ys', which
  -- would have solved the problem.
  let xs[i] = ys
  in (xs, zs[i])
